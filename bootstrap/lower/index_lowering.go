/*
# Index Access Lowering

Convert indexed access (`x = arr[1]` and `arr[1] = x`) to calls to `InternalArray` functions.
*/
package lower

import (
	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type indexLowering struct {
	DefaultTransformer
	typeInfo    *typed.TypeInfo
	typeCreator *typed.TypeCreator
	nodeCreator *ast.NodeCreator
}

func (self *indexLowering) VisitIndexExpression(expr *ast.IndexExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkIndexExpression(expr)
	visitMustNotChange(expr, visited, ok)
	arrayType := self.typeInfo.MustLookup(expr.Target).(*typed.StructType)
	arrayElementType := self.typeInfo.BuiltIns.GetArrayElementType(arrayType)
	getFunc, ok := arrayType.FindMethod("internal_get")
	if !ok {
		panic("InternalArray.internal_get not found")
	}
	callArgs := []ast.CallArg{{Value: expr.Index, Span: expr.Index.Span()}}
	callExpr := self.nodeCreator.NewCallExpression(
		self.nodeCreator.NewMemberExpression(expr.Target, "internal_get", expr.Span()), callArgs, expr.Span())
	self.typeInfo.Set(callExpr.Callee, getFunc)
	self.typeInfo.Set(callExpr, arrayElementType)
	return callExpr, true
}

func (self *indexLowering) VisitAssignmentStatement(stmt *ast.AssignmentStatement, w TransformWalker) (ast.Node, bool) {
	index, ok := stmt.IsIndexAssigment()
	if !ok {
		return stmt, true
	}
	arrayType := self.typeInfo.MustLookup(index.Target).(*typed.StructType)
	arrayElementType := self.typeInfo.BuiltIns.GetArrayElementType(arrayType)
	setFunc, ok := arrayType.FindMethod("internal_set")
	if !ok {
		panic("InternalArray.internal_set not found")
	}
	callArgs := []ast.CallArg{{Value: index.Index, Span: index.Index.Span()}, {Value: stmt.Value, Span: stmt.Value.Span()}}
	callExpr := self.nodeCreator.NewCallExpression(
		self.nodeCreator.NewMemberExpression(index.Target, "internal_set", stmt.Span()), callArgs, stmt.Span())
	self.typeInfo.Set(callExpr.Callee, setFunc)
	self.typeInfo.Set(callExpr, arrayElementType)
	stmt.Value = callExpr
	return callExpr, true
}

func IndexLowering(
	module *ast.Module, typeInfo *typed.TypeInfo, typeCreator *typed.TypeCreator, nodeCreator *ast.NodeCreator) *ast.Module {
	transformer := &indexLowering{
		typeInfo:    typeInfo,
		typeCreator: typeCreator,
		nodeCreator: nodeCreator,
	}
	walker := DefaultTransformWalker{Transformer: transformer}
	module, ok := walker.WalkModule(module)
	if !ok {
		panic("module has been removed")
	}
	return module
}
