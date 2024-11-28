package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type funcInfo struct {
	funcDef  *ast.FunctionDefinition
	funcType *typed.FunctionType
}

type prepare struct {
	DefaultTransformer
	funcInfos        map[typed.TypeId]*funcInfo
	funcInfo         *funcInfo
	typeInfo         *typed.TypeInfo
	genericsResolver *typed.GenericsResolver
	nodeCreator      *ast.NodeCreator
}

func (self *prepare) convertToTypeIdIdentExpression(expr ast.Expression, ty typed.Type) *ast.IdentExpression {
	symbol := self.typeInfo.MustLookupSymbol(ty.Id())
	res := self.nodeCreator.NewIdentExpression(ast.Ident(symbol.FQN()), expr.Span())
	self.typeInfo.Set(res, ty)
	return res
}

func (self *prepare) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkMemberExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.MemberExpression)
	if !ok {
		return newExpr, false
	}
	functionType, ok := self.typeInfo.MustLookup(expr).(*typed.FunctionType)
	if !ok || !functionType.IsStaticMethod() {
		return expr, true
	}
	res := self.convertToTypeIdIdentExpression(expr, functionType)
	return res, true
}

func (self *prepare) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.CallExpression)
	if !ok {
		return newExpr, true
	}
	calleeType := self.typeInfo.MustLookup(expr.Callee).(typed.CallableType)
	params := calleeType.CallParams()
	// Re-order the call arguments to be in the order of the function parameters.
	callArgs := make([]ast.CallArg, len(expr.Args))
	positionalArgIndex := 0
	for _, callArg := range expr.Args {
		if callArg.Name == "" {
			callArgs[positionalArgIndex] = callArg
			positionalArgIndex += 1
			continue
		}
		for i, param := range params {
			if param.Name == callArg.Name {
				callArgs[i] = callArg
				break
			}
		}
	}
	expr.Args = callArgs
	switch calleeType := calleeType.(type) {
	case *typed.FunctionType:
		if !calleeType.IsMethod() || calleeType.IsStaticMethod() {
			return expr, true
		}
		receiver := expr.Callee.(*ast.MemberExpression).Target
		receiverType := self.typeInfo.MustLookup(receiver).(typed.GenericType)
		if typed.HasTypeParams(receiverType) {
			// Propagate the type parameters and arguments from the receiver type to the callee.
			calleeType = self.genericsResolver.CloneAndMergeReceiverGenerics(calleeType)
		}
		receiverCallArg := ast.CallArg{Name: "self", Value: receiver, Span: expr.Span()}
		expr.Args = append([]ast.CallArg{receiverCallArg}, expr.Args...)
		expr.Callee = self.convertToTypeIdIdentExpression(expr.Callee, calleeType)
		return expr, true
	case *typed.StructType:
		expr.Callee = self.convertToTypeIdIdentExpression(expr.Callee, calleeType)
		return expr, true
	default:
		panic(fmt.Sprintf("unexpected callable type: %T", calleeType))
	}
}

func (self *prepare) VisitFunctionDefinition(def *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool) {
	funcType := self.typeInfo.MustLookup(def.Decl).(*typed.DeclaredType).Type.(*typed.FunctionType)
	funcInfo := &funcInfo{funcDef: def, funcType: funcType}
	self.funcInfos[funcType.Id()] = funcInfo
	self.funcInfo = funcInfo
	def, ok := w.WalkFunctionDefinition(def)
	if !ok {
		return nil, false
	}
	self.funcInfo = nil
	return def, true
}

func (self *prepare) VisitTraitDeclaration(decl *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	w.WalkTraitDeclaration(decl)
	return nil, false
}

func (self *prepare) VisitImplDefinition(def *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool) {
	w.WalkImplDefinition(def)
	return nil, false
}

func Prepare(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	genericsResolver *typed.GenericsResolver,
	nodeCreator *ast.NodeCreator,
	typeCreator *typed.TypeCreator,
) (*ast.Module, map[typed.TypeId]*funcInfo) {
	prepare := &prepare{
		funcInfos:        map[typed.TypeId]*funcInfo{},
		typeInfo:         typeInfo,
		genericsResolver: genericsResolver,
		nodeCreator:      nodeCreator,
	}
	walker := DefaultTransformWalker{Transformer: prepare}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	return module, prepare.funcInfos
}
