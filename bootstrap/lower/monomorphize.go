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

type FunctionSpecialization struct {
	FuncDef     *ast.FunctionDefinition
	Specialized *typed.FunctionType
	Base        *typed.FunctionType
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
}

func (self *workItem) resolve(ty typed.Type) typed.Type {
	switch ty := ty.(type) {
	case *typed.FunctionType:
		// There are no more methods, everything is a plain function.
		ty.Receiver = nil
		if typed.HasTypeParams(ty) {
			// We need to also resolve all type parameters of the function.
			typeParams := append(self.specialized.TypeParams(), ty.TypeParams()...)
			typeArgs := append(self.specialized.TypeArgs(), ty.TypeArgs()...)
			return typed.ResolveTypeArgs(ty, typeParams, typeArgs)
		}
	case *typed.StructType:
		// Structs are just data holders at this point, because we converted
		// all methods to plain functions.
		ty.Methods = []typed.TypeAndName[*typed.FunctionType]{}
	}
	return typed.ResolveTypeArgs(ty, self.specialized.TypeParams(), self.specialized.TypeArgs())
}

type mono struct {
	ast.DefaultVisitor
	funcInfos  map[typed.TypeId]*funcInfo
	queue      []*workItem
	current    *workItem
	typeInfo   *typed.TypeInfo
	funcSpecs  []*FunctionSpecialization
	nextTypeId int
}

func (self *mono) VisitIdentExpression(expr *ast.IdentExpression) error {
	// Note: During `prepare` we converted all `ast.MemberExpression` that reference
	//       a static method to `ast.IdentExpressions`.
	//       So there is no need to visit member expressions.
	ty := self.typeInfo.MustLookup(expr)
	ty = self.current.resolve(ty)
	if specialized, ok := self.lookupOrCreateSpecialized(ty); ok {
		// This is a specialized function type (with its own `TypeId`).
		// We have to set it in the global typeInfo.
		self.typeInfo.Set(expr, specialized)
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
	}
}

func matchSpecializedTypes(ty1 *typed.FunctionType, ty2 *typed.FunctionType) bool {
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
			if matchSpecializedTypes(queued.specialized, item.specialized) {
				return queued.specialized, true
			}
		}
	}
	for _, spec := range self.funcSpecs {
		if spec.Base.Id() == funcType.Id() {
			if matchSpecializedTypes(spec.Specialized, specialized) {
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
		self.current = item
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
		funcInfos:  funcInfos,
		queue:      []*workItem{},
		typeInfo:   typeInfo,
		funcSpecs:  []*FunctionSpecialization{},
		nextTypeId: nextTypeId,
	}
	m.lookupOrCreateSpecialized(main.funcType)
	m.run()
	return m.funcSpecs
}
