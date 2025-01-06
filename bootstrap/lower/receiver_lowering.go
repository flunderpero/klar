/*
# Receiver Lowering

This pass will convert all calls like `object.method()` to `object_method(object)`.
In addition to that the call arguments will be brought into the order of the parameters.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type receiverLowering struct {
	DefaultTransformer
	typeInfo                        *typed.TypeInfo
	genericsResolver                *typed.GenericsResolver
	nodeCreator                     *ast.NodeCreator
	mergedReceiverGenericsFunctions []*typed.FunctionType
}

func (self *receiverLowering) convertToTypeIdIdentExpression(expr ast.Expression, ty typed.Type) *ast.IdentExpression {
	symbol := self.typeInfo.MustLookupSymbol(ty.Id())
	res := self.nodeCreator.NewIdentExpression(ast.Ident(symbol.FQN()), expr.Span())
	self.typeInfo.Set(res, ty)
	return res
}

func (self *receiverLowering) mergeReceiverGenericsIntoFunction(funcType *typed.FunctionType, receiverType typed.GenericType) *typed.FunctionType {
	funcBase := funcType
	if base, ok := funcType.GenericBase(); ok {
		funcBase = base.(*typed.FunctionType)
	}
	typeArgs := receiverType.TypeArgs()
	for _, merged := range self.mergedReceiverGenericsFunctions {
		mergedBase := merged
		if base, ok := merged.GenericBase(); ok {
			mergedBase = base.(*typed.FunctionType)
		}
		if mergedBase.Id() != funcBase.Id() {
			continue
		}
		if len(merged.TypeArgs()) == len(typeArgs) && typed.MatchTypeArgs(merged, typeArgs) {
			return merged
		}
	}
	res := self.genericsResolver.CloneAndMergeReceiverGenerics(funcType, receiverType)
	self.mergedReceiverGenericsFunctions = append(self.mergedReceiverGenericsFunctions, res)
	return res
}

func (self *receiverLowering) mergeMethodMemberExpression(expr *ast.MemberExpression, funcType *typed.FunctionType) *ast.IdentExpression {
	receiver := expr.Target
	receiverType := self.typeInfo.MustLookup(receiver)
	if typeParam, ok := receiverType.(*typed.TypeParam); ok {
		receiverType = typeParam.TraitBound
	}
	if genericType, ok := receiverType.(typed.GenericType); ok && typed.HasTypeParams(genericType) {
		funcType = self.mergeReceiverGenericsIntoFunction(funcType, genericType)
	}
	res := self.convertToTypeIdIdentExpression(expr, funcType)
	if typeParam, ok := self.typeInfo.LookupTraitBoundTypeParam(expr); ok {
		self.typeInfo.SetTraitBoundTypeParam(res, typeParam)
	}
	return res
}

func (self *receiverLowering) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkMemberExpression(expr)
	visitMustNotChange(expr, visited, ok)
	switch ty := self.typeInfo.MustLookup(expr).(type) {
	case *typed.FunctionType:
		if !ty.IsStaticMethod() {
			return expr, true
		}
		return self.mergeMethodMemberExpression(expr, ty), true
	}
	return expr, true
}

func (self *receiverLowering) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkCallExpression(expr)
	visitMustNotChange(expr, visited, ok)
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
		receiverCallArg := ast.CallArg{Name: "self", Value: receiver, Span: expr.Span()}
		expr.Args = append([]ast.CallArg{receiverCallArg}, expr.Args...)
		expr.Callee = self.mergeMethodMemberExpression(expr.Callee.(*ast.MemberExpression), calleeType)
		return expr, true
	case *typed.StructType, *typed.TupleType, *typed.NamedUnionVariantConstructor:
		return expr, true
	default:
		panic(fmt.Sprintf("unexpected callable type: %T", calleeType))
	}
}

func ReceiverLowering(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	genericsResolver *typed.GenericsResolver,
	nodeCreator *ast.NodeCreator,
) *ast.Module {
	transformer := &receiverLowering{typeInfo: typeInfo, genericsResolver: genericsResolver, nodeCreator: nodeCreator}
	walker := DefaultTransformWalker{Transformer: transformer}
	module, ok := walker.WalkModule(module)
	if !ok {
		panic("module has been deleted")
	}
	return module
}
