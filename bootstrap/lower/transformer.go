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
	VisitNode(node ast.Node, w TransformWalker) ast.Node
	VisitModule(module *ast.Module, w TransformWalker) *ast.Module
	VisitStructTypeDeclaration(ty *ast.StructTypeDeclaration) *ast.StructTypeDeclaration
	VisitImplDefinition(impl *ast.ImplDefinition, w TransformWalker) *ast.ImplDefinition
	VisitFunctionDefinition(fn *ast.FunctionDefinition, w TransformWalker) *ast.FunctionDefinition
	VisitVariableDefinition(variable *ast.VariableDefinition, w TransformWalker) *ast.VariableDefinition
	VisitExpression(expr ast.Expression, w TransformWalker) ast.Expression
	VisitBlockExpression(expr *ast.BlockExpression, w TransformWalker) *ast.BlockExpression
	VisitCallExpression(expr *ast.CallExpression, w TransformWalker) *ast.CallExpression
	VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) *ast.MemberExpression
	VisitStructInitExpression(expr *ast.StructInitExpression, w TransformWalker) *ast.StructInitExpression
	VisitIfExpression(expr *ast.IfExpression, w TransformWalker) *ast.IfExpression
	VisitBinaryExpression(expr *ast.BinaryExpression, w TransformWalker) *ast.BinaryExpression
	VisitIdentExpression(expr *ast.IdentExpression) *ast.IdentExpression
	VisitStringLiteralExpression(expr *ast.StringLiteralExpression) *ast.StringLiteralExpression
	VisitIntLiteralExpression(expr *ast.IntLiteralExpression) *ast.IntLiteralExpression
	VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) *ast.BoolLiteralExpression
	VisitAssignmentStatement(stmt *ast.AssignmentStatement, w TransformWalker) *ast.AssignmentStatement
	VisitLoopStatement(stmt *ast.LoopStatement, w TransformWalker) *ast.LoopStatement
	VisitBreakStatement(stmt *ast.BreakStatement) *ast.BreakStatement
	VisitContinueStatement(stmt *ast.ContinueStatement) *ast.ContinueStatement
}

type TransformWalker interface {
	WalkNode(node ast.Node) ast.Node
	WalkModule(module *ast.Module) *ast.Module
	WalkFunctionDefinition(fn *ast.FunctionDefinition) *ast.FunctionDefinition
	WalkImplDefinition(impl *ast.ImplDefinition) *ast.ImplDefinition
	WalkVariableDefinition(variable *ast.VariableDefinition) *ast.VariableDefinition
	WalkExpression(expr ast.Expression) ast.Expression
	WalkBlockExpression(expr *ast.BlockExpression) *ast.BlockExpression
	WalkCallExpression(expr *ast.CallExpression) *ast.CallExpression
	WalkMemberExpression(expr *ast.MemberExpression) *ast.MemberExpression
	WalkStructInitExpression(expr *ast.StructInitExpression) *ast.StructInitExpression
	WalkIfExpression(expr *ast.IfExpression) *ast.IfExpression
	WalkBinaryExpression(expr *ast.BinaryExpression) *ast.BinaryExpression
	WalkAssignmentStatement(stmt *ast.AssignmentStatement) *ast.AssignmentStatement
	WalkLoopStatement(stmt *ast.LoopStatement) *ast.LoopStatement
}

type DefaultTransformer struct{}

func (_ *DefaultTransformer) VisitIdentExpression(expr *ast.IdentExpression) *ast.IdentExpression {
	return expr
}

func (_ *DefaultTransformer) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) *ast.StringLiteralExpression {
	return expr
}

func (_ *DefaultTransformer) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) *ast.IntLiteralExpression {
	return expr
}

func (_ *DefaultTransformer) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) *ast.BoolLiteralExpression {
	return expr
}

func (_ *DefaultTransformer) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) *ast.CallExpression {
	return w.WalkCallExpression(expr)
}

func (_ *DefaultTransformer) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) *ast.MemberExpression {
	return w.WalkMemberExpression(expr)
}

func (_ *DefaultTransformer) VisitStructInitExpression(expr *ast.StructInitExpression, w TransformWalker) *ast.StructInitExpression {
	return w.WalkStructInitExpression(expr)
}

func (_ *DefaultTransformer) VisitIfExpression(expr *ast.IfExpression, w TransformWalker) *ast.IfExpression {
	return w.WalkIfExpression(expr)
}

func (_ *DefaultTransformer) VisitBinaryExpression(expr *ast.BinaryExpression, w TransformWalker) *ast.BinaryExpression {
	return w.WalkBinaryExpression(expr)
}

func (_ *DefaultTransformer) VisitBlockExpression(expr *ast.BlockExpression, w TransformWalker) *ast.BlockExpression {
	return w.WalkBlockExpression(expr)
}

func (_ *DefaultTransformer) VisitExpression(expr ast.Expression, w TransformWalker) ast.Expression {
	return w.WalkExpression(expr)
}

func (_ *DefaultTransformer) VisitFunctionDefinition(fn *ast.FunctionDefinition, w TransformWalker) *ast.FunctionDefinition {
	return w.WalkFunctionDefinition(fn)
}

func (_ *DefaultTransformer) VisitImplDefinition(impl *ast.ImplDefinition, w TransformWalker) *ast.ImplDefinition {
	return w.WalkImplDefinition(impl)
}

func (_ *DefaultTransformer) VisitVariableDefinition(variable *ast.VariableDefinition, w TransformWalker) *ast.VariableDefinition {
	return w.WalkVariableDefinition(variable)
}

func (_ *DefaultTransformer) VisitAssignmentStatement(stmt *ast.AssignmentStatement, w TransformWalker) *ast.AssignmentStatement {
	return w.WalkAssignmentStatement(stmt)
}

func (_ *DefaultTransformer) VisitLoopStatement(stmt *ast.LoopStatement, w TransformWalker) *ast.LoopStatement {
	return w.WalkLoopStatement(stmt)
}

func (_ *DefaultTransformer) VisitBreakStatement(stmt *ast.BreakStatement) *ast.BreakStatement {
	return stmt
}

func (_ *DefaultTransformer) VisitContinueStatement(stmt *ast.ContinueStatement) *ast.ContinueStatement {
	return stmt
}

func (_ *DefaultTransformer) VisitStructTypeDeclaration(tc *ast.StructTypeDeclaration) *ast.StructTypeDeclaration {
	return tc
}

func (_ *DefaultTransformer) VisitModule(module *ast.Module, w TransformWalker) *ast.Module {
	return w.WalkModule(module)
}

func (_ *DefaultTransformer) VisitNode(node ast.Node, w TransformWalker) ast.Node {
	return w.WalkNode(node)
}

type DefaultTransformWalker struct {
	Transformer Transformer
}

func (w *DefaultTransformWalker) WalkExpression(expr ast.Expression) ast.Expression {
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

func (w *DefaultTransformWalker) WalkBinaryExpression(expr *ast.BinaryExpression) *ast.BinaryExpression {
	lhs := w.Transformer.VisitExpression(expr.Lhs, w)
	rhs := w.Transformer.VisitExpression(expr.Rhs, w)
	if lhs == nil && rhs == nil {
		return nil
	}
	if lhs == nil || rhs == nil {
		panic("lhs and rhs must be both nil or both non-nil")
	}
	expr.Lhs = lhs
	expr.Rhs = rhs
	return expr
}

func (w *DefaultTransformWalker) WalkCallExpression(expr *ast.CallExpression) *ast.CallExpression {
	expr.Callee = w.Transformer.VisitExpression(expr.Callee, w)
	args := []ast.Expression{}
	for _, arg := range expr.Args {
		transformed := w.Transformer.VisitExpression(arg, w)
		if transformed != nil {
			args = append(args, transformed)
		}
	}
	expr.Args = args
	return expr
}

func (w *DefaultTransformWalker) WalkStructInitExpression(expr *ast.StructInitExpression) *ast.StructInitExpression {
	for _, field := range expr.Fields {
		field.Value = w.Transformer.VisitExpression(field.Value, w)
	}
	return expr
}

func (w *DefaultTransformWalker) WalkIfExpression(expr *ast.IfExpression) *ast.IfExpression {
	expr.Condition = w.Transformer.VisitExpression(expr.Condition, w)
	if expr.FalseBody != nil {
		expr.FalseBody = w.Transformer.VisitBlockExpression(expr.FalseBody, w)
	}
	expr.TrueBody = w.Transformer.VisitBlockExpression(expr.TrueBody, w)
	return expr
}

func (w *DefaultTransformWalker) WalkBlockExpression(expr *ast.BlockExpression) *ast.BlockExpression {
	nodes := []ast.Node{}
	for _, node := range expr.Nodes {
		transformed := w.Transformer.VisitNode(node, w)
		if transformed != nil {
			nodes = append(nodes, node)
		}
	}
	expr.Nodes = nodes
	return expr
}

func (w *DefaultTransformWalker) WalkMemberExpression(expr *ast.MemberExpression) *ast.MemberExpression {
	expr.Target = w.Transformer.VisitExpression(expr.Target, w)
	return expr
}

func (w *DefaultTransformWalker) WalkModule(module *ast.Module) *ast.Module {
	nodes := []ast.Node{}
	for _, node := range module.Nodes {
		transformed := w.Transformer.VisitNode(node, w)
		if transformed != nil {
			nodes = append(nodes, node)
		}
	}
	module.Nodes = nodes
	return module
}

func (w *DefaultTransformWalker) WalkImplDefinition(impl *ast.ImplDefinition) *ast.ImplDefinition {
	methods := []*ast.FunctionDefinition{}
	for _, method := range impl.Methods {
		transformed := w.Transformer.VisitFunctionDefinition(method, w)
		if transformed != nil {
			methods = append(methods, transformed)
		}
	}
	impl.Methods = methods
	return impl
}

func (w *DefaultTransformWalker) WalkFunctionDefinition(fn *ast.FunctionDefinition) *ast.FunctionDefinition {
	fn.Body = w.Transformer.VisitBlockExpression(fn.Body, w)
	return fn
}

func (w *DefaultTransformWalker) WalkVariableDefinition(variable *ast.VariableDefinition) *ast.VariableDefinition {
	variable.Value = w.Transformer.VisitExpression(variable.Value, w)
	return variable
}

func (w *DefaultTransformWalker) WalkAssignmentStatement(stmt *ast.AssignmentStatement) *ast.AssignmentStatement {
	stmt.Variable = w.Transformer.VisitIdentExpression(stmt.Variable)
	stmt.Rhs = w.Transformer.VisitExpression(stmt.Rhs, w)
	return stmt
}

func (w *DefaultTransformWalker) WalkLoopStatement(stmt *ast.LoopStatement) *ast.LoopStatement {
	stmt.Body = w.Transformer.VisitBlockExpression(stmt.Body, w)
	return stmt
}

func (w *DefaultTransformWalker) WalkNode(node ast.Node) ast.Node {
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
