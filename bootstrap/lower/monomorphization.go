/*
# Monomorphization (And Dead Code Elimination)

This pass walks the AST starting at the  `main` function and collects all references to
functions and creates specialized versions if they are generic.

The result is a set of `FunctionSpecializations` that represents all reachable and specialized
functions in the AST.

Because only reachable functions are collected, this pass also performs dead code elimination
on the function level.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type funcInfo struct {
	funcDef  *ast.FunctionDefinition
	funcType *typed.FunctionType
}

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

func (self *SpecializedTypeInfo) BuiltIns() *typed.BuiltIns {
	return &self.base.BuiltIns
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
	genericsResolver *typed.TypeResolver
}

func (self *mono) VisitNode(node ast.Node, w ast.Walker) error {
	if err := w.WalkNode(node); err != nil {
		return err
	}
	ty := self.resolve(node)
	if specialized, ok := self.lookupOrCreateSpecializedFunction(ty); ok {
		self.current.typeInfo.set(node, specialized)
	} else {
		self.current.typeInfo.set(node, ty)
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

func (self *mono) resolve(node ast.Node) typed.Type {
	ty := self.current.typeInfo.MustLookup(node)
	typeParams := self.current.specialized.TypeParams()
	typeArgs := self.current.specialized.TypeArgs()
	switch ty := ty.(type) {
	case *typed.FunctionType:
		if typeParam, ok := ty.Receiver.(*typed.TypeParam); ok {
			// We know that a `TraitType` can only come from a type parameter.
			// So we first look up the name of the function and then we find
			// the struct that is set as the type-arg for the type-param with the
			// trait-bound.
			// And finally, we substitute the TraitType's function we got here with
			// the struct's function.
			traitType := typeParam.TraitBound
			tyBase := ty
			if genericBase, ok := ty.GenericBase(); ok {
				tyBase = genericBase.(*typed.FunctionType)
			}
			var traitMethod *typed.Method
			for _, method := range traitType.Methods {
				if method.Type.Id() == tyBase.Id() {
					traitMethod = method
					break
				}
			}
			if traitMethod == nil {
				panic(fmt.Sprintf("method not found in trait: %s", ty))
			}
			traitBoundTypeParam := self.globalTypeInfo.MustLookupTraitBoundTypeParam(node)
			var targetType typed.ImplementableType = nil
			for i, typeParam := range typeParams {
				if traitBoundTypeParam.Id() == typeParam.Id() {
					targetType = typeArgs[i].(typed.ImplementableType)
					break
				}
			}
			if targetType == nil {
				panic(fmt.Sprintf("type not found for trait bound type param: %s", traitBoundTypeParam))
			}
			ty, ok = targetType.FindMethod(traitMethod.Name)
			if !ok {
				panic(fmt.Sprintf("method %q not found in struct: %s", traitMethod.Name, targetType))
			}
			return ty
		}
	case *typed.DeclaredType:
		return ty
	}
	return self.genericsResolver.ResolveType(ty, typeParams, typeArgs)
}

func (self *mono) lookupOrCreateSpecializedFunction(ty typed.Type) (*typed.FunctionType, bool) {
	funcType, ok := ty.(*typed.FunctionType)
	if !ok {
		return nil, false
	}
	if self.globalTypeInfo.BuiltIns.IsBuiltInFunction(funcType) {
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
			if typed.MatchTypeArgs(queued.specialized, funcType.TypeArgs()) {
				return queued.specialized, true
			}
		}
	}
	for _, spec := range self.funcSpecs {
		if spec.Base.Id() == baseType.Id() {
			if typed.MatchTypeArgs(spec.Specialized, funcType.TypeArgs()) {
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

type collectFuncInfos struct {
	ast.DefaultVisitor
	funcInfos map[typed.TypeId]*funcInfo
	typeInfo  *typed.TypeInfo
}

func (self *collectFuncInfos) VisitFunctionDefinition(def *ast.FunctionDefinition, w ast.Walker) error {
	funcType := self.typeInfo.MustLookup(def.Decl).(*typed.DeclaredType).Type.(*typed.FunctionType)
	funcInfo := &funcInfo{funcDef: def, funcType: funcType}
	self.funcInfos[funcType.Id()] = funcInfo
	if err := w.WalkFunctionDefinition(def); err != nil {
		return err
	}
	return nil
}

func Monomorphization(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	genericsResolver *typed.TypeResolver,
) []*FunctionSpecialization {
	collectVisitor := &collectFuncInfos{
		DefaultVisitor: ast.DefaultVisitor{},
		funcInfos:      make(map[typed.TypeId]*funcInfo), typeInfo: typeInfo}
	collectWalker := ast.DefaultWalker{Visitor: collectVisitor}
	if err := collectWalker.WalkNode(module); err != nil {
		panic(err)
	}
	funcInfos := collectVisitor.funcInfos
	main, ok := funcInfos[typeInfo.Main.Id()]
	if !ok {
		panic("`main` function not found")
	}
	panicFunc, ok := funcInfos[typeInfo.Panic.Id()]
	if !ok {
		panic("`panic` function not found")
	}
	m := mono{
		funcInfos:        funcInfos,
		queue:            []*workItem{},
		globalTypeInfo:   typeInfo,
		funcSpecs:        []*FunctionSpecialization{},
		genericsResolver: genericsResolver,
	}
	m.lookupOrCreateSpecializedFunction(panicFunc.funcType)
	m.lookupOrCreateSpecializedFunction(main.funcType)
	m.run()
	return m.funcSpecs
}
