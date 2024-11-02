package ast

import "fmt"

type ASTVisitor interface {
	VisitNode(node Node, w ASTWalker) error
	VisitModule(module *Module, w ASTWalker) error
	VisitFunctionDefinition(fn *FunctionDefinition, w ASTWalker) error
	VisitExpression(expr Expression, w ASTWalker) error
	VisitBlockExpression(expr *BlockExpression, w ASTWalker) error
	VisitCallExpression(expr *CallExpression, w ASTWalker) error
	VisitIfExpression(expr *IfExpression, w ASTWalker) error
	VisitAddExpression(expr *AddExpression, w ASTWalker) error
	VisitIdentExpression(expr *IdentExpression) error
	VisitStringLiteralExpression(expr *StringLiteralExpression) error
	VisitIntLiteralExpression(expr *IntLiteralExpression) error
	VisitBoolLiteralExpression(expr *BoolLiteralExpression) error
}

type ASTWalker interface {
	WalkNode(node Node) error
	WalkModule(module *Module) error
	WalkFunctionDefinition(fn *FunctionDefinition) error
	WalkExpression(expr Expression) error
	WalkBlockExpression(expr *BlockExpression) error
	WalkCallExpression(expr *CallExpression) error
	WalkIfExpression(expr *IfExpression) error
	WalkAddExpression(expr *AddExpression) error
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

func (_ *DefaultASTVisitor) VisitAddExpression(expr *AddExpression, w ASTWalker) error {
	return w.WalkAddExpression(expr)
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
	case *AddExpression:
		err = w.Visitor.VisitAddExpression(expr, w)
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

func (w *DefaultASTWalker) WalkAddExpression(expr *AddExpression) error {
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

func (w *DefaultASTWalker) WalkNode(node Node) error {
	var err error
	switch node := node.(type) {
	case *Module:
		err = w.Visitor.VisitModule(node, w)
	case *FunctionDefinition:
		err = w.Visitor.VisitFunctionDefinition(node, w)
	default:
		err = w.Visitor.VisitExpression(node, w)
	}
	return err
}
