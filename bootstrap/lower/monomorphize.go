/*
# Monomorphization (And Dead Code Elimination)

This pass walks from the `main` function and collects all references to functions and creates
specialized versions if they are generic.

The result is a set of `FunctionSpecializations` that represents all reachable (and specialized)
functions in the AST.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type SpecializedTypeInfo struct {
	base           *typed.TypeInfo
	overlayTypes   map[ast.NodeId]typed.Type
	overlaySymbols map[typed.IsId]*typed.Symbol
	typeParams     []typed.TypeParam
	typeArgs       []typed.Type
}

func newTypeInfo(typeInfo *typed.TypeInfo, typeParams []typed.TypeParam, typeArgs []typed.Type) *SpecializedTypeInfo {
	return &SpecializedTypeInfo{
		base:         typeInfo,
		overlayTypes: make(map[ast.NodeId]typed.Type),
		typeParams:   typeParams,
		typeArgs:     typeArgs,
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
	if s, ok := self.LookupSymbol(id); ok {
		return s
	}
	panic(fmt.Sprintf("symbol not found: %s", id))
}

func (self *SpecializedTypeInfo) LookupSymbol(id typed.IsId) (*typed.Symbol, bool) {
	if symbol, ok := self.overlaySymbols[id]; ok {
		return symbol, true
	}
	return self.base.LookupSymbol(id)
}

func (self *SpecializedTypeInfo) lookupAndResolve(node ast.Node) typed.Type {
	ty := self.MustLookup(node)
	return typed.ResolveTypeArgs(ty, self.typeParams, self.typeArgs)
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

func mustBeFullyResolved(ty typed.Type) {
	if genericType, ok := ty.(typed.GenericType); ok {
		if !typed.IsFullyResolved(genericType) {
			panic(fmt.Sprintf("generic type is not fully resolved: %s", ty))
		}
	}
}

type workItem struct {
	funcInfo    *funcInfo
	specialized *typed.FunctionType
	typeInfo    *SpecializedTypeInfo
}

type mono struct {
	ast.DefaultVisitor
	funcInfos           map[typed.TypeId]*funcInfo
	queue               []*workItem
	typeInfo            *typed.TypeInfo
	specializedTypeInfo *SpecializedTypeInfo
	funcSpecs           []*FunctionSpecialization
	nextTypeId          int
}

func (self *mono) VisitIdentExpression(expr *ast.IdentExpression) error {
	// Note: During `prepare` we converted all `ast.MemberExpression` that reference
	//       a static method to `ast.IdentExpressions`.
	//       So there is no need to visit member expressions.
	ty := self.specializedTypeInfo.lookupAndResolve(expr)
	if specialized, ok := self.lookupOrCreateSpecialized(ty); ok {
		// This is a specialized function type (with its own `TypeId`).
		// We have to set it in the global typeInfo.
		self.typeInfo.Set(expr, specialized)
	} else {
		// The node's type might have been altered by `lookupAndResolve`,
		// i.e. type parameters may have been resolved. The result is a
		// specialized type that is local to the current function.
		self.specializedTypeInfo.set(expr, ty)
	}
	return nil
}

func (self *mono) newTypeId() typed.TypeId {
	self.nextTypeId++
	return typed.TypeId(self.nextTypeId)
}

func (self *mono) newWorkItem(info *funcInfo, specialized *typed.FunctionType) *workItem {
	return &workItem{
		funcInfo:    info,
		typeInfo:    newTypeInfo(self.typeInfo, specialized.TypeParams(), specialized.TypeArgs()),
		specialized: specialized,
	}
}

func matchTypeArgs(ty1 *typed.FunctionType, ty2 *typed.FunctionType) bool {
	for i, typeArg := range ty1.TypeArgs() {
		if typeArg.Id() != ty2.TypeArgs()[i].Id() {
			return false
		}
	}
	return true
}

func (self *mono) lookupOrCreateSpecialized(ty typed.Type) (*typed.FunctionType, bool) {
	funcType, ok := ty.(*typed.FunctionType)
	if !ok {
		return nil, false
	}
	if typed.IsBuiltInFunction(funcType) {
		return nil, false
	}
	funcInfo, ok := self.funcInfos[ty.Id()]
	if !ok {
		return nil, false
	}
	specialized := funcType.CloneWithNewId(self.newTypeId())
	mustBeFullyResolved(specialized)
	item := self.newWorkItem(funcInfo, specialized)
	for _, queued := range self.queue {
		if queued.funcInfo == item.funcInfo {
			if matchTypeArgs(queued.specialized, item.specialized) {
				return queued.specialized, true
			}
		}
	}
	for _, spec := range self.funcSpecs {
		if spec.Base.Id() == funcType.Id() {
			if matchTypeArgs(spec.Specialized, specialized) {
				return spec.Specialized, true
			}
		}
	}
	self.queue = append(self.queue, item)
	return item.specialized, true
}

func (self *mono) run() {
	for len(self.queue) > 0 {
		item := self.queue[0]
		self.specializedTypeInfo = item.typeInfo
		self.queue = self.queue[1:]
		walker := &ast.DefaultWalker{Visitor: self}
		base.Must(walker.WalkFunctionDefinition(item.funcInfo.funcDef))
		baseSymbol := self.typeInfo.MustLookupSymbol(item.funcInfo.funcType.Id())
		symbolName := baseSymbol.Name
		for _, typeArg := range item.specialized.TypeArgs() {
			symbolName += "$"
			typeArgName := self.typeInfo.MustLookupSymbol(typeArg.Id()).FQN()
			symbolName += typeArgName
		}
		self.typeInfo.DeclareSymbol(item.specialized.Id(), &typed.Symbol{
			Name:  symbolName,
			Scope: baseSymbol.Scope,
		})
		funcSpec := &FunctionSpecialization{
			FuncDef:     item.funcInfo.funcDef,
			Specialized: item.specialized,
			Base:        item.funcInfo.funcType,
			TypeInfo:    item.typeInfo,
			IsMain:      item.funcInfo.funcType == self.typeInfo.Main,
		}
		self.funcSpecs = append(self.funcSpecs, funcSpec)
	}
}

func Monomorphize(module *ast.Module, typeInfo *typed.TypeInfo, funcInfos map[typed.TypeId]*funcInfo, nextTypeId int) []*FunctionSpecialization {
	main, ok := funcInfos[typeInfo.Main.Id()]
	if !ok {
		panic("Main function not found")
	}
	m := mono{
		funcInfos:           funcInfos,
		queue:               []*workItem{},
		typeInfo:            typeInfo,
		specializedTypeInfo: newTypeInfo(typeInfo, []typed.TypeParam{}, []typed.Type{}),
		funcSpecs:           []*FunctionSpecialization{},
		nextTypeId:          nextTypeId,
	}
	m.lookupOrCreateSpecialized(main.funcType)
	m.run()
	return m.funcSpecs
}
