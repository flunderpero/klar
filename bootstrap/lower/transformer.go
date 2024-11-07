/*
This is a transforming AST walker / visitor that allows implementations to modify
the AST.

Since this transformer is used after all correctness checks (in the `typed` module)
all `Visit` and `Walk` functions do not return an `error`. If there is an error it is because
the transformation logic is flawed and then it becomes a `panic`.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
)

type Transformer interface {
	VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool)
	VisitModule(module *ast.Module, w TransformWalker) (*ast.Module, bool)
	VisitStructTypeDeclaration(ty *ast.StructTypeDeclaration) (*ast.StructTypeDeclaration, bool)
	VisitImplDefinition(impl *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool)
	VisitFunctionDeclaration(decl *ast.FunctionDeclaration) (*ast.FunctionDeclaration, bool)
	VisitFunctionDefinition(fn *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool)
	VisitVariableDefinition(variable *ast.VariableDefinition, w TransformWalker) (*ast.VariableDefinition, bool)
	VisitExpression(expr ast.Expression, w TransformWalker) (ast.Expression, bool)
	VisitBlockExpression(expr *ast.BlockExpression, w TransformWalker) (*ast.BlockExpression, bool)
	VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (*ast.CallExpression, bool)
	VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (*ast.MemberExpression, bool)
	VisitStructInitExpression(expr *ast.StructInitExpression, w TransformWalker) (*ast.StructInitExpression, bool)
	VisitIfExpression(expr *ast.IfExpression, w TransformWalker) (*ast.IfExpression, bool)
	VisitBinaryExpression(expr *ast.BinaryExpression, w TransformWalker) (*ast.BinaryExpression, bool)
	VisitIdentExpression(expr *ast.IdentExpression) (*ast.IdentExpression, bool)
	VisitStringLiteralExpression(expr *ast.StringLiteralExpression) (*ast.StringLiteralExpression, bool)
	VisitIntLiteralExpression(expr *ast.IntLiteralExpression) (*ast.IntLiteralExpression, bool)
	VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) (*ast.BoolLiteralExpression, bool)
	VisitAssignmentStatement(stmt *ast.AssignmentStatement, w TransformWalker) (*ast.AssignmentStatement, bool)
	VisitLoopStatement(stmt *ast.LoopStatement, w TransformWalker) (*ast.LoopStatement, bool)
	VisitBreakStatement(stmt *ast.BreakStatement) (*ast.BreakStatement, bool)
	VisitContinueStatement(stmt *ast.ContinueStatement) (*ast.ContinueStatement, bool)
}

type TransformWalker interface {
	WalkNode(node ast.Node) (ast.Node, bool)
	WalkModule(module *ast.Module) (*ast.Module, bool)
	WalkFunctionDefinition(fn *ast.FunctionDefinition) (*ast.FunctionDefinition, bool)
	WalkImplDefinition(impl *ast.ImplDefinition) (*ast.ImplDefinition, bool)
	WalkVariableDefinition(variable *ast.VariableDefinition) (*ast.VariableDefinition, bool)
	WalkExpression(expr ast.Expression) (ast.Expression, bool)
	WalkBlockExpression(expr *ast.BlockExpression) (*ast.BlockExpression, bool)
	WalkCallExpression(expr *ast.CallExpression) (*ast.CallExpression, bool)
	WalkMemberExpression(expr *ast.MemberExpression) (*ast.MemberExpression, bool)
	WalkStructInitExpression(expr *ast.StructInitExpression) (*ast.StructInitExpression, bool)
	WalkIfExpression(expr *ast.IfExpression) (*ast.IfExpression, bool)
	WalkBinaryExpression(expr *ast.BinaryExpression) (*ast.BinaryExpression, bool)
	WalkAssignmentStatement(stmt *ast.AssignmentStatement) (*ast.AssignmentStatement, bool)
	WalkLoopStatement(stmt *ast.LoopStatement) (*ast.LoopStatement, bool)
}

type DefaultTransformer struct{}

func (_ *DefaultTransformer) VisitIdentExpression(expr *ast.IdentExpression) (*ast.IdentExpression, bool) {
	return expr, true
}

func (_ *DefaultTransformer) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) (*ast.StringLiteralExpression, bool) {
	return expr, true
}

func (_ *DefaultTransformer) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) (*ast.IntLiteralExpression, bool) {
	return expr, true
}

func (_ *DefaultTransformer) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) (*ast.BoolLiteralExpression, bool) {
	return expr, true
}

func (_ *DefaultTransformer) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (*ast.CallExpression, bool) {
	return w.WalkCallExpression(expr)
}

func (_ *DefaultTransformer) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (*ast.MemberExpression, bool) {
	return w.WalkMemberExpression(expr)
}

func (_ *DefaultTransformer) VisitStructInitExpression(expr *ast.StructInitExpression, w TransformWalker) (*ast.StructInitExpression, bool) {
	return w.WalkStructInitExpression(expr)
}

func (_ *DefaultTransformer) VisitIfExpression(expr *ast.IfExpression, w TransformWalker) (*ast.IfExpression, bool) {
	return w.WalkIfExpression(expr)
}

func (_ *DefaultTransformer) VisitBinaryExpression(expr *ast.BinaryExpression, w TransformWalker) (*ast.BinaryExpression, bool) {
	return w.WalkBinaryExpression(expr)
}

func (_ *DefaultTransformer) VisitBlockExpression(expr *ast.BlockExpression, w TransformWalker) (*ast.BlockExpression, bool) {
	return w.WalkBlockExpression(expr)
}

func (_ *DefaultTransformer) VisitExpression(expr ast.Expression, w TransformWalker) (ast.Expression, bool) {
	return w.WalkExpression(expr)
}

func (_ *DefaultTransformer) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) (*ast.FunctionDeclaration, bool) {
	return decl, true
}

func (_ *DefaultTransformer) VisitFunctionDefinition(fn *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool) {
	return w.WalkFunctionDefinition(fn)
}

func (_ *DefaultTransformer) VisitImplDefinition(impl *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool) {
	return w.WalkImplDefinition(impl)
}

func (_ *DefaultTransformer) VisitVariableDefinition(variable *ast.VariableDefinition, w TransformWalker) (*ast.VariableDefinition, bool) {
	return w.WalkVariableDefinition(variable)
}

func (_ *DefaultTransformer) VisitAssignmentStatement(stmt *ast.AssignmentStatement, w TransformWalker) (*ast.AssignmentStatement, bool) {
	return w.WalkAssignmentStatement(stmt)
}

func (_ *DefaultTransformer) VisitLoopStatement(stmt *ast.LoopStatement, w TransformWalker) (*ast.LoopStatement, bool) {
	return w.WalkLoopStatement(stmt)
}

func (_ *DefaultTransformer) VisitBreakStatement(stmt *ast.BreakStatement) (*ast.BreakStatement, bool) {
	return stmt, true
}

func (_ *DefaultTransformer) VisitContinueStatement(stmt *ast.ContinueStatement) (*ast.ContinueStatement, bool) {
	return stmt, true
}

func (_ *DefaultTransformer) VisitStructTypeDeclaration(tc *ast.StructTypeDeclaration) (*ast.StructTypeDeclaration, bool) {
	return tc, true
}

func (_ *DefaultTransformer) VisitModule(module *ast.Module, w TransformWalker) (*ast.Module, bool) {
	return w.WalkModule(module)
}

func (_ *DefaultTransformer) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	return w.WalkNode(node)
}

type DefaultTransformWalker struct {
	Transformer Transformer
}

func (w *DefaultTransformWalker) WalkExpression(expr ast.Expression) (ast.Expression, bool) {
	switch expr := expr.(type) {
	case *ast.IdentExpression:
		return w.Transformer.VisitIdentExpression(expr)
	case *ast.StringLiteralExpression:
		return w.Transformer.VisitStringLiteralExpression(expr)
	case *ast.IntLiteralExpression:
		return w.Transformer.VisitIntLiteralExpression(expr)
	case *ast.BoolLiteralExpression:
		return w.Transformer.VisitBoolLiteralExpression(expr)
	case *ast.BinaryExpression:
		return w.Transformer.VisitBinaryExpression(expr, w)
	case *ast.CallExpression:
		return w.Transformer.VisitCallExpression(expr, w)
	case *ast.MemberExpression:
		return w.Transformer.VisitMemberExpression(expr, w)
	case *ast.IfExpression:
		return w.Transformer.VisitIfExpression(expr, w)
	case *ast.BlockExpression:
		return w.Transformer.VisitBlockExpression(expr, w)
	case *ast.StructInitExpression:
		return w.Transformer.VisitStructInitExpression(expr, w)
	default:
		panic(fmt.Sprintf("VisitExpression not implemented for expression type: %T", expr))
	}
}

func (w *DefaultTransformWalker) WalkBinaryExpression(expr *ast.BinaryExpression) (*ast.BinaryExpression, bool) {
	lhs, lhsOk := w.Transformer.VisitExpression(expr.Lhs, w)
	rhs, rhsOk := w.Transformer.VisitExpression(expr.Rhs, w)
	if !lhsOk && !rhsOk {
		return nil, false
	}
	if lhsOk != rhsOk {
		panic("either both or none of lhs and rhs can be deleted")
	}
	expr.Lhs = lhs
	expr.Rhs = rhs
	return expr, true
}

func (w *DefaultTransformWalker) WalkCallExpression(expr *ast.CallExpression) (*ast.CallExpression, bool) {
	callee, ok := w.Transformer.VisitExpression(expr.Callee, w)
	if !ok {
		return nil, false
	}
	expr.Callee = callee
	args := []ast.Expression{}
	for _, arg := range expr.Args {
		transformed, ok := w.Transformer.VisitExpression(arg, w)
		if ok {
			args = append(args, transformed)
		}
	}
	expr.Args = args
	return expr, true
}

func (w *DefaultTransformWalker) WalkStructInitExpression(expr *ast.StructInitExpression) (*ast.StructInitExpression, bool) {
	fields := []ast.StructInitField{}
	for _, field := range expr.Fields {
		value, ok := w.Transformer.VisitExpression(field.Value, w)
		if ok {
			field.Value = value
			fields = append(fields, field)
		}
	}
	expr.Fields = fields
	return expr, true
}

func (w *DefaultTransformWalker) WalkIfExpression(expr *ast.IfExpression) (*ast.IfExpression, bool) {
	condition, conditionOk := w.Transformer.VisitExpression(expr.Condition, w)
	trueBody, trueBodyOk := w.Transformer.VisitBlockExpression(expr.TrueBody, w)
	if expr.FalseBody != nil {
		falseBody, falseBodyOk := w.Transformer.VisitBlockExpression(expr.FalseBody, w)
		if falseBodyOk != conditionOk || falseBodyOk != trueBodyOk {
			panic("either all or none of condition, trueBody and falseBody must be deleted")
		}
		expr.FalseBody = falseBody
	}
	if conditionOk != trueBodyOk {
		panic("either all or none of condition and trueBody must be deleted")
	}
	expr.Condition = condition
	expr.TrueBody = trueBody
	return expr, true
}

func (w *DefaultTransformWalker) WalkBlockExpression(expr *ast.BlockExpression) (*ast.BlockExpression, bool) {
	nodes := []ast.Node{}
	for _, node := range expr.Nodes {
		transformed, ok := w.Transformer.VisitNode(node, w)
		if ok {
			nodes = append(nodes, transformed)
		}
	}
	expr.Nodes = nodes
	return expr, true
}

func (w *DefaultTransformWalker) WalkMemberExpression(expr *ast.MemberExpression) (*ast.MemberExpression, bool) {
	target, ok := w.Transformer.VisitExpression(expr.Target, w)
	if !ok {
		return nil, false
	}
	expr.Target = target
	return expr, true
}

func (w *DefaultTransformWalker) WalkModule(module *ast.Module) (*ast.Module, bool) {
	nodes := []ast.Node{}
	for _, node := range module.Nodes {
		transformed, ok := w.Transformer.VisitNode(node, w)
		if ok {
			nodes = append(nodes, transformed)
		}
	}
	module.Nodes = nodes
	return module, true
}

func (w *DefaultTransformWalker) WalkImplDefinition(impl *ast.ImplDefinition) (*ast.ImplDefinition, bool) {
	methods := []*ast.FunctionDefinition{}
	for _, method := range impl.Methods {
		transformed, ok := w.Transformer.VisitFunctionDefinition(method, w)
		if ok {
			methods = append(methods, transformed)
		}
	}
	impl.Methods = methods
	return impl, true
}

func (w *DefaultTransformWalker) WalkFunctionDefinition(fn *ast.FunctionDefinition) (*ast.FunctionDefinition, bool) {
	body, ok := w.Transformer.VisitBlockExpression(fn.Body, w)
	if !ok {
		return nil, false
	}
	fn.Body = body
	return fn, true
}

func (w *DefaultTransformWalker) WalkVariableDefinition(variable *ast.VariableDefinition) (*ast.VariableDefinition, bool) {
	value, ok := w.Transformer.VisitExpression(variable.Value, w)
	if !ok {
		return nil, false
	}
	variable.Value = value
	return variable, true
}

func (w *DefaultTransformWalker) WalkAssignmentStatement(stmt *ast.AssignmentStatement) (*ast.AssignmentStatement, bool) {
	variable, variableOk := w.Transformer.VisitIdentExpression(stmt.Variable)
	rhs, rhsOk := w.Transformer.VisitExpression(stmt.Rhs, w)
	if variableOk != rhsOk {
		panic("either both or none of variable and rhs can be deleted")
	}
	if !variableOk {
		return nil, false
	}
	stmt.Variable = variable
	stmt.Rhs = rhs
	return stmt, true
}

func (w *DefaultTransformWalker) WalkLoopStatement(stmt *ast.LoopStatement) (*ast.LoopStatement, bool) {
	body, ok := w.Transformer.VisitBlockExpression(stmt.Body, w)
	if !ok {
		return nil, false
	}
	stmt.Body = body
	return stmt, true
}

func (w *DefaultTransformWalker) WalkNode(node ast.Node) (ast.Node, bool) {
	switch node := node.(type) {
	case *ast.Module:
		return w.Transformer.VisitModule(node, w)
	case *ast.StructTypeDeclaration:
		return w.Transformer.VisitStructTypeDeclaration(node)
	case *ast.ImplDefinition:
		return w.Transformer.VisitImplDefinition(node, w)
	case *ast.FunctionDefinition:
		return w.Transformer.VisitFunctionDefinition(node, w)
	case *ast.VariableDefinition:
		return w.Transformer.VisitVariableDefinition(node, w)
	case *ast.AssignmentStatement:
		return w.Transformer.VisitAssignmentStatement(node, w)
	case *ast.LoopStatement:
		return w.Transformer.VisitLoopStatement(node, w)
	case *ast.BreakStatement:
		return w.Transformer.VisitBreakStatement(node)
	case *ast.ContinueStatement:
		return w.Transformer.VisitContinueStatement(node)
	default:
		return w.Transformer.VisitExpression(node, w)
	}
}
