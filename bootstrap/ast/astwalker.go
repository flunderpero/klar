package ast

import "fmt"

type ASTVisitor interface {
	VisitNode(node Node, w ASTWalker) error
	VisitModule(module *Module, w ASTWalker) error
	VisitFunctionDefinition(fn *FunctionDefinition, w ASTWalker) error
	VisitVariableDefinition(fn *VariableDefinition, w ASTWalker) error
	VisitExpression(expr Expression, w ASTWalker) error
	VisitBlockExpression(expr *BlockExpression, w ASTWalker) error
	VisitCallExpression(expr *CallExpression, w ASTWalker) error
	VisitIfExpression(expr *IfExpression, w ASTWalker) error
	VisitBinaryExpression(expr *BinaryExpression, w ASTWalker) error
	VisitIdentExpression(expr *IdentExpression) error
	VisitStringLiteralExpression(expr *StringLiteralExpression) error
	VisitIntLiteralExpression(expr *IntLiteralExpression) error
	VisitBoolLiteralExpression(expr *BoolLiteralExpression) error
	VisitAssignmentStatement(stmt *AssignmentStatement, w ASTWalker) error
	VisitLoopStatement(stmt *LoopStatement, w ASTWalker) error
	VisitBreakStatement(stmt *BreakStatement) error
	VisitContinueStatement(stmt *ContinueStatement) error
}

type ASTWalker interface {
	WalkNode(node Node) error
	WalkModule(module *Module) error
	WalkFunctionDefinition(fn *FunctionDefinition) error
	WalkVariableDefinition(fn *VariableDefinition) error
	WalkExpression(expr Expression) error
	WalkBlockExpression(expr *BlockExpression) error
	WalkCallExpression(expr *CallExpression) error
	WalkIfExpression(expr *IfExpression) error
	WalkBinaryExpression(expr *BinaryExpression) error
	WalkAssignmentStatement(stmt *AssignmentStatement) error
	WalkLoopStatement(stmt *LoopStatement) error
}

type DefaultASTVisitor struct{}

func (_ *DefaultASTVisitor) VisitIdentExpression(expr *IdentExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitIntLiteralExpression(expr *IntLiteralExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitBoolLiteralExpression(expr *BoolLiteralExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitCallExpression(expr *CallExpression, w ASTWalker) error {
	return w.WalkCallExpression(expr)
}

func (_ *DefaultASTVisitor) VisitIfExpression(expr *IfExpression, w ASTWalker) error {
	return w.WalkIfExpression(expr)
}

func (_ *DefaultASTVisitor) VisitBinaryExpression(expr *BinaryExpression, w ASTWalker) error {
	return w.WalkBinaryExpression(expr)
}

func (_ *DefaultASTVisitor) VisitBlockExpression(expr *BlockExpression, w ASTWalker) error {
	return w.WalkBlockExpression(expr)
}

func (_ *DefaultASTVisitor) VisitExpression(expr Expression, w ASTWalker) error {
	return w.WalkExpression(expr)
}

func (_ *DefaultASTVisitor) VisitFunctionDefinition(fn *FunctionDefinition, w ASTWalker) error {
	return w.WalkFunctionDefinition(fn)
}

func (_ *DefaultASTVisitor) VisitVariableDefinition(fn *VariableDefinition, w ASTWalker) error {
	return w.WalkVariableDefinition(fn)
}

func (_ *DefaultASTVisitor) VisitAssignmentStatement(stmt *AssignmentStatement, w ASTWalker) error {
	return w.WalkAssignmentStatement(stmt)
}

func (_ *DefaultASTVisitor) VisitLoopStatement(stmt *LoopStatement, w ASTWalker) error {
	return w.WalkLoopStatement(stmt)
}

func (_ *DefaultASTVisitor) VisitBreakStatement(stmt *BreakStatement) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitContinueStatement(stmt *ContinueStatement) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitModule(module *Module, w ASTWalker) error {
	return w.WalkModule(module)
}

func (_ *DefaultASTVisitor) VisitNode(node Node, w ASTWalker) error {
	return w.WalkNode(node)
}

type DefaultASTWalker struct {
	Visitor ASTVisitor
}

func (w *DefaultASTWalker) WalkExpression(expr Expression) error {
	var err error
	switch expr := expr.(type) {
	case *IdentExpression:
		err = w.Visitor.VisitIdentExpression(expr)
	case *StringLiteralExpression:
		err = w.Visitor.VisitStringLiteralExpression(expr)
	case *IntLiteralExpression:
		err = w.Visitor.VisitIntLiteralExpression(expr)
	case *BoolLiteralExpression:
		err = w.Visitor.VisitBoolLiteralExpression(expr)
	case *BinaryExpression:
		err = w.Visitor.VisitBinaryExpression(expr, w)
	case *CallExpression:
		err = w.Visitor.VisitCallExpression(expr, w)
	case *IfExpression:
		err = w.Visitor.VisitIfExpression(expr, w)
	case *BlockExpression:
		err = w.Visitor.VisitBlockExpression(expr, w)
	default:
		return fmt.Errorf("VisitExpression not implemented for expression type: %T", expr)
	}
	return err
}

func (w *DefaultASTWalker) WalkBinaryExpression(expr *BinaryExpression) error {
	if err := w.Visitor.VisitNode(expr.Lhs, w); err != nil {
		return err
	}
	return w.Visitor.VisitNode(expr.Rhs, w)
}

func (w *DefaultASTWalker) WalkCallExpression(expr *CallExpression) error {
	if err := w.Visitor.VisitNode(expr.Callee, w); err != nil {
		return err
	}
	for _, arg := range expr.Args {
		if err := w.Visitor.VisitNode(arg, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultASTWalker) WalkIfExpression(expr *IfExpression) error {
	if err := w.Visitor.VisitNode(expr.Condition, w); err != nil {
		return err
	}
	if expr.FalseBody != nil {
		if err := w.Visitor.VisitNode(expr.FalseBody, w); err != nil {
			return err
		}
	}
	return w.Visitor.VisitNode(expr.TrueBody, w)
}

func (w *DefaultASTWalker) WalkBlockExpression(expr *BlockExpression) error {
	for _, node := range expr.Nodes {
		if err := w.Visitor.VisitNode(node, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultASTWalker) WalkModule(module *Module) error {
	for _, node := range module.Nodes {
		if err := w.Visitor.VisitNode(node, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultASTWalker) WalkFunctionDefinition(fn *FunctionDefinition) error {
	return w.WalkBlockExpression(fn.Body)
}

func (w *DefaultASTWalker) WalkVariableDefinition(fn *VariableDefinition) error {
	return w.WalkNode(fn.Value)
}

func (w *DefaultASTWalker) WalkAssignmentStatement(stmt *AssignmentStatement) error {
	if err := w.WalkNode(stmt.Lhs); err != nil {
		return err
	}
	return w.WalkNode(stmt.Rhs)
}

func (w *DefaultASTWalker) WalkLoopStatement(stmt *LoopStatement) error {
	return w.WalkBlockExpression(stmt.Body)
}

func (w *DefaultASTWalker) WalkNode(node Node) error {
	var err error
	switch node := node.(type) {
	case *Module:
		err = w.Visitor.VisitModule(node, w)
	case *FunctionDefinition:
		err = w.Visitor.VisitFunctionDefinition(node, w)
	case *VariableDefinition:
		err = w.Visitor.VisitVariableDefinition(node, w)
	case *AssignmentStatement:
		err = w.Visitor.VisitAssignmentStatement(node, w)
	case *LoopStatement:
		err = w.Visitor.VisitLoopStatement(node, w)
	case *BreakStatement:
		err = w.Visitor.VisitBreakStatement(node)
	case *ContinueStatement:
		err = w.Visitor.VisitContinueStatement(node)
	default:
		err = w.Visitor.VisitExpression(node, w)
	}
	return err
}
