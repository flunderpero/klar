package main

import "fmt"

type ASTVisitor interface {
	VisitIdentExpression(expr *IdentExpression) error
	VisitStringLiteralExpression(expr *StringLiteralExpression) error
	VisitCallExpression(expr *CallExpression) error
	VisitExpression(expr Expression) error
	VisitNode(node Node) error
}

type EmptyASTVisitor struct{}

func (_ *EmptyASTVisitor) VisitIdentExpression(expr *IdentExpression) error {
	return nil
}

func (_ *EmptyASTVisitor) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	return nil
}

func (_ *EmptyASTVisitor) VisitCallExpression(expr *CallExpression) error {
	return nil
}

func (_ *EmptyASTVisitor) VisitExpression(expr Expression) error {
	return nil
}

func (_ *EmptyASTVisitor) VisitNode(node Node) error {
	return nil
}

type DepthFirstASTWalker struct {
	Visitor ASTVisitor
}

func (w *DepthFirstASTWalker) WalkExpression(expr Expression) error {
	var err error
	switch expr := expr.(type) {
	case *IdentExpression:
		err = w.Visitor.VisitIdentExpression(expr)
	case *StringLiteralExpression:
		err = w.Visitor.VisitStringLiteralExpression(expr)
	case *CallExpression:
		if err = w.WalkCallExpression(expr); err != nil {
			return err
		}
		err = w.Visitor.VisitCallExpression(expr)
	default:
		return fmt.Errorf("VisitExpression not implemented for expression type: %T", expr)
	}
	if err != nil {
		return err
	}
	return w.Visitor.VisitExpression(expr)
}

func (w *DepthFirstASTWalker) WalkCallExpression(expr *CallExpression) error {
	if err := w.WalkExpression(expr.Callee); err != nil {
		return err
	}
	for _, arg := range expr.Args {
		if err := w.WalkExpression(arg); err != nil {
			return err
		}
	}
	return nil
}

func (w *DepthFirstASTWalker) WalkNode(node Node) error {
	var err error
	switch node := node.(type) {
	default:
		err = w.WalkExpression(node)
	}
	if err != nil {
		return err
	}
	return w.Visitor.VisitNode(node)
}
