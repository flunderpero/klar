/*
# Monomorphization (And Dead Code Elimination)

This pass walks from the AST starting at the  `main` function and collects all references to
functions and creates specialized versions if they are generic.

The result is a set of `FunctionSpecializations` that represents all reachable and specialized
functions in the AST.

Because only reachable functions are collected, this pass also performs dead code elimination.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type SpecializedTypeInfo struct {
	base         *typed.TypeInfo
	overlayTypes map[ast.NodeId]typed.Type
}

func newTypeInfo(typeInfo *typed.TypeInfo) *SpecializedTypeInfo {
	return &SpecializedTypeInfo{
		base:         typeInfo,
		overlayTypes: make(map[ast.NodeId]typed.Type),
	}
}

func (self *SpecializedTypeInfo) set(node ast.Node, ty typed.Type) {
	self.overlayTypes[node.Id()] = ty
}

func (self *SpecializedTypeInfo) MustLookup(node ast.Node) typed.Type {
	if ty, ok := self.overlayTypes[node.Id()]; ok {
		return ty
	}
	return self.base.MustLookup(node)
}

func (self *SpecializedTypeInfo) MustLookupSymbol(id typed.IsId) *typed.Symbol {
	return self.base.MustLookupSymbol(id)
}

type FunctionSpecialization struct {
	FuncDef     *ast.FunctionDefinition
	Specialized *typed.FunctionType
	Base        *typed.FunctionType
	TypeInfo    *SpecializedTypeInfo
	IsMain      bool
}

func (self FunctionSpecialization) String() string {
	name := fmt.Sprintf("%s (%s from %s)", self.FuncDef.Decl.Name, self.Specialized.Id(), self.Base.Id())
	return fmt.Sprintf("FunctionSpecialization\n%s\n%s", base.IndentString(name, 1), base.Indent(self.Specialized, 1))
}

type workItem struct {
	funcInfo    *funcInfo
	specialized *typed.FunctionType
	typeInfo    *SpecializedTypeInfo
}

type mono struct {
	ast.DefaultVisitor
	funcInfos        map[typed.TypeId]*funcInfo
	queue            []*workItem
	current          *workItem
	globalTypeInfo   *typed.TypeInfo
	funcSpecs        []*FunctionSpecialization
	genericsResolver *typed.GenericsResolver
}

func (self *mono) VisitNode(expr ast.Node, w ast.Walker) error {
	if err := w.WalkNode(expr); err != nil {
		return err
	}
	ty := self.current.typeInfo.MustLookup(expr)
	ty = self.resolve(ty)
	if specialized, ok := self.lookupOrCreateSpecializedFunction(ty); ok {
		self.current.typeInfo.set(expr, specialized)
	} else {
		self.current.typeInfo.set(expr, ty)
	}
	return nil
}

func (self *mono) newWorkItem(info *funcInfo, specialized *typed.FunctionType) *workItem {
	return &workItem{
		funcInfo:    info,
		specialized: specialized,
		typeInfo:    newTypeInfo(self.globalTypeInfo),
	}
}

func (self *mono) resolve(ty typed.Type) typed.Type {
	typeParams := self.current.specialized.TypeParams()
	typeArgs := self.current.specialized.TypeArgs()
	switch ty := ty.(type) {
	case *typed.FunctionType:
		// There are no more methods, everything is a plain function.
		ty.Receiver = nil
	case *typed.StructType:
		// Structs are just data holders at this point, because we converted
		// all methods to plain functions.
		ty.Methods = []typed.TypeAndName[*typed.FunctionType]{}
	case *typed.DeclaredType:
		return ty
	}
	return self.genericsResolver.ResolveTypeArgs(ty, typeParams, typeArgs)
}

func (self *mono) lookupOrCreateSpecializedFunction(ty typed.Type) (*typed.FunctionType, bool) {
	funcType, ok := ty.(*typed.FunctionType)
	if !ok {
		return nil, false
	}
	if typed.IsBuiltInFunction(funcType) {
		return nil, false
	}
	baseType, ok := funcType.GenericBase()
	if !ok {
		baseType = funcType
	}
	funcInfo, ok := self.funcInfos[baseType.Id()]
	if !ok {
		return nil, false
	}
	for _, queued := range self.queue {
		if queued.funcInfo == funcInfo {
			if queued.specialized.Id() == funcType.Id() {
				return queued.specialized, true
			}
		}
	}
	for _, spec := range self.funcSpecs {
		if spec.Base.Id() == baseType.Id() {
			if spec.Specialized.Id() == funcType.Id() {
				return spec.Specialized, true
			}
		}
	}
	if !typed.IsFullyResolved(funcType) {
		panic(fmt.Sprintf("function type is not fully resolved: %s", ty))
	}
	item := self.newWorkItem(funcInfo, funcType)
	self.queue = append(self.queue, item)
	return item.specialized, true
}

func (self *mono) run() {
	for len(self.queue) > 0 {
		item := self.queue[0]
		self.current = item
		self.queue = self.queue[1:]
		walker := &ast.DefaultWalker{Visitor: self}
		base.Must(walker.WalkFunctionDefinition(item.funcInfo.funcDef))
		funcSpec := &FunctionSpecialization{
			FuncDef:     item.funcInfo.funcDef,
			Specialized: item.specialized,
			Base:        item.funcInfo.funcType,
			TypeInfo:    item.typeInfo,
			IsMain:      item.funcInfo.funcType == self.globalTypeInfo.Main,
		}
		self.funcSpecs = append(self.funcSpecs, funcSpec)
	}
}

func Monomorphize(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	funcInfos map[typed.TypeId]*funcInfo,
	genericsResolver *typed.GenericsResolver,
) []*FunctionSpecialization {
	main, ok := funcInfos[typeInfo.Main.Id()]
	if !ok {
		panic("Main function not found")
	}
	m := mono{
		funcInfos:        funcInfos,
		queue:            []*workItem{},
		globalTypeInfo:   typeInfo,
		funcSpecs:        []*FunctionSpecialization{},
		genericsResolver: genericsResolver,
	}
	m.lookupOrCreateSpecializedFunction(main.funcType)
	m.run()
	return m.funcSpecs
}
