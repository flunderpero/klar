package main

import "fmt"

type ASTVisitor interface {
	VisitNode(node Node, w ASTWalker) error
	VisitExpression(expr Expression, w ASTWalker) error
	VisitBlockExpression(expr *BlockExpression, w ASTWalker) error
	VisitCallExpression(expr *CallExpression, w ASTWalker) error
	VisitIdentExpression(expr *IdentExpression) error
	VisitStringLiteralExpression(expr *StringLiteralExpression) error
}

type ASTWalker interface {
	WalkNode(node Node) error
	WalkExpression(expr Expression) error
	WalkBlockExpression(expr *BlockExpression) error
	WalkCallExpression(expr *CallExpression) error
}

type DefaultASTVisitor struct{}

func (_ *DefaultASTVisitor) VisitIdentExpression(expr *IdentExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	return nil
}

func (_ *DefaultASTVisitor) VisitCallExpression(expr *CallExpression, w ASTWalker) error {
	return w.WalkCallExpression(expr)
}

func (_ *DefaultASTVisitor) VisitBlockExpression(expr *BlockExpression, w ASTWalker) error {
	return w.WalkBlockExpression(expr)
}

func (_ *DefaultASTVisitor) VisitExpression(expr Expression, w ASTWalker) error {
	return w.WalkExpression(expr)
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
	case *CallExpression:
		err = w.Visitor.VisitCallExpression(expr, w)
	case *BlockExpression:
		err = w.Visitor.VisitBlockExpression(expr, w)
	default:
		return fmt.Errorf("VisitExpression not implemented for expression type: %T", expr)
	}
	return err
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

func (w *DefaultASTWalker) WalkBlockExpression(expr *BlockExpression) error {
	for _, node := range expr.Nodes {
		if err := w.Visitor.VisitNode(node, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultASTWalker) WalkNode(node Node) error {
	var err error
	switch node := node.(type) {
	default:
		err = w.Visitor.VisitExpression(node, w)
	}
	return err
}
