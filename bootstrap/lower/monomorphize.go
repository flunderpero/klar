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
	if s, ok := self.LookupSymbol(id); ok {
		return s
	}
	panic(fmt.Sprintf("symbol not found: %s", id))
}

func (self *SpecializedTypeInfo) LookupSymbol(id typed.IsId) (*typed.Symbol, bool) {
	return self.base.LookupSymbol(id)
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

type StructSpecialization struct {
	Specialized *typed.StructType
	Base        *typed.StructType
}

func (self StructSpecialization) String() string {
	name := fmt.Sprintf("%s from %s", self.Specialized.Id(), self.Base.Id())
	return fmt.Sprintf("StructSpecialization\n%s\n%s", base.IndentString(name, 1), base.Indent(self.Specialized, 1))
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
	funcInfos      map[typed.TypeId]*funcInfo
	queue          []*workItem
	current        *workItem
	globalTypeInfo *typed.TypeInfo
	funcSpecs      []*FunctionSpecialization
	structSpecs    []*StructSpecialization
	nextTypeId     int
}

func (self *mono) VisitNode(expr ast.Node, w ast.Walker) error {
	if err := w.WalkNode(expr); err != nil {
		return err
	}
	// Note: During `prepare` we converted all `ast.MemberExpression` that reference
	//       a static method to `ast.IdentExpressions`.
	//       So there is no need to visit member expressions.
	ty := self.current.typeInfo.MustLookup(expr)
	ty = self.resolve(ty)
	if specialized, ok := self.lookupOrCreateSpecializedFunction(ty); ok {
		self.current.typeInfo.set(expr, specialized)
	} else if specialized, ok := self.lookupOrCreateSpecializedStruct(ty); ok {
		self.current.typeInfo.set(expr, specialized)
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
		if typed.HasTypeParams(ty) {
			// We need to also resolve all type parameters of the function as well.
			typeParams = append(typeParams, ty.TypeParams()...)
			typeArgs = append(typeArgs, ty.TypeArgs()...)
		}
		return typed.ResolveTypeArgs(ty, typeParams, typeArgs)
	case *typed.StructType:
		// Structs are just data holders at this point, because we converted
		// all methods to plain functions.
		ty.Methods = []typed.TypeAndName[*typed.FunctionType]{}
		return typed.ResolveTypeArgs(ty, typeParams, typeArgs)
	default:
		return typed.ResolveTypeArgs(ty, typeParams, typeArgs)
	}
}

func matchSpecializedTypes(ty1 typed.GenericType, ty2 typed.GenericType) bool {
	for i, typeArg := range ty1.TypeArgs() {
		if typeArg.Id() != ty2.TypeArgs()[i].Id() {
			return false
		}
	}
	return true
}

func (self *mono) lookupOrCreateSpecializedFunction(ty typed.Type) (*typed.FunctionType, bool) {
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
	for _, queued := range self.queue {
		if queued.funcInfo == funcInfo {
			if matchSpecializedTypes(queued.specialized, funcType) {
				return queued.specialized, true
			}
		}
	}
	for _, spec := range self.funcSpecs {
		if spec.Base.Id() == funcType.Id() {
			if matchSpecializedTypes(spec.Specialized, funcType) {
				return spec.Specialized, true
			}
		}
	}
	specialized := funcType.CloneWithNewId(self.newTypeId())
	mustBeFullyResolved(specialized)
	// Fix specialized struct types in the function's signature.
	for i, param := range specialized.Params {
		if specializedStruct, ok := self.lookupOrCreateSpecializedStruct(param.Type); ok {
			specialized.Params[i] = typed.TypeAndName[typed.Type]{Type: specializedStruct, Name: param.Name}
		}
	}
	if specializedStruct, ok := self.lookupOrCreateSpecializedFunction(specialized.Result); ok {
		specialized.Result = specializedStruct
	}
	item := self.newWorkItem(funcInfo, specialized)
	self.queue = append(self.queue, item)
	self.declareSymbolForSpecializedType(specialized, funcType)
	return item.specialized, true
}

func (self *mono) lookupOrCreateSpecializedStruct(ty typed.Type) (*typed.StructType, bool) {
	structType, ok := ty.(*typed.StructType)
	if !ok {
		return nil, false
	}
	for _, spec := range self.structSpecs {
		if spec.Base.Id() == structType.Id() {
			if matchSpecializedTypes(spec.Specialized, structType) {
				return spec.Specialized, true
			}
		}
	}
	specialized := structType.CloneWithNewId(self.newTypeId())
	mustBeFullyResolved(specialized)
	self.structSpecs = append(self.structSpecs, &StructSpecialization{Base: structType, Specialized: specialized})
	self.declareSymbolForSpecializedType(specialized, structType)
	return specialized, true
}

func (self *mono) declareSymbolForSpecializedType(specialized typed.GenericType, base typed.GenericType) {
	baseSymbol := self.globalTypeInfo.MustLookupSymbol(base.Id())
	symbolName := baseSymbol.Name
	for _, typeArg := range specialized.TypeArgs() {
		symbolName += "$"
		typeArgName := self.globalTypeInfo.MustLookupSymbol(typeArg.Id()).FQN()
		symbolName += typeArgName
	}
	self.globalTypeInfo.DeclareSymbol(specialized.Id(), &typed.Symbol{
		Name:  symbolName,
		Scope: baseSymbol.Scope,
	})
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

func Monomorphize(module *ast.Module, typeInfo *typed.TypeInfo, funcInfos map[typed.TypeId]*funcInfo, nextTypeId int) []*FunctionSpecialization {
	main, ok := funcInfos[typeInfo.Main.Id()]
	if !ok {
		panic("Main function not found")
	}
	m := mono{
		funcInfos:      funcInfos,
		queue:          []*workItem{},
		globalTypeInfo: typeInfo,
		funcSpecs:      []*FunctionSpecialization{},
		nextTypeId:     nextTypeId,
	}
	m.lookupOrCreateSpecializedFunction(main.funcType)
	m.run()
	return m.funcSpecs
}
