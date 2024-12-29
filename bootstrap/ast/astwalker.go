package ast

import (
	"github.com/pkg/errors"
)

type Visitor interface {
	VisitNode(node Node, w Walker) error
	VisitModule(module *Module, w Walker) error
	VisitStructTypeDeclaration(ty *StructTypeDeclaration) error
	VisitUnionTypeDeclaration(ty *UnionTypeDeclaration) error
	VisitFunctionDeclaration(decl *FunctionDeclaration) error
	VisitFunctionDefinition(fn *FunctionDefinition, w Walker) error
	VisitVariableDefinition(variable *VariableDefinition, w Walker) error
	VisitExpression(expr Expression, w Walker) error
	VisitBlockExpression(expr *BlockExpression, w Walker) error
	VisitCallExpression(expr *CallExpression, w Walker) error
	VisitMemberExpression(expr *MemberExpression, w Walker) error
	VisitIfExpression(expr *IfExpression, w Walker) error
	VisitMatchExpression(expr *MatchExpression, w Walker) error
	VisitBinaryExpression(expr *BinaryExpression, w Walker) error
	VisitUnaryExpression(expr *UnaryExpression, w Walker) error
	VisitTraitDeclaration(impl *TraitDeclaration, w Walker) error
	VisitImplDefinition(impl *ImplDefinition, w Walker) error
	VisitIdentExpression(expr *IdentExpression) error
	VisitStringLiteralExpression(expr *StringLiteralExpression) error
	VisitCharLiteralExpression(expr *CharLiteralExpression) error
	VisitIntLiteralExpression(expr *IntLiteralExpression) error
	VisitBoolLiteralExpression(expr *BoolLiteralExpression) error
	VisitTupleLiteralExpression(expr *TupleLiteralExpression, w Walker) error
	VisitAssignmentStatement(stmt *AssignmentStatement, w Walker) error
	VisitLoopStatement(stmt *LoopStatement, w Walker) error
	VisitBreakStatement(stmt *BreakStatement) error
	VisitContinueStatement(stmt *ContinueStatement) error
}

type Walker interface {
	WalkNode(node Node) error
	WalkModule(module *Module) error
	WalkFunctionDefinition(fn *FunctionDefinition) error
	WalkTraitDeclaration(impl *TraitDeclaration) error
	WalkImplDefinition(impl *ImplDefinition) error
	WalkVariableDefinition(variable *VariableDefinition) error
	WalkExpression(expr Expression) error
	WalkBlockExpression(expr *BlockExpression) error
	WalkCallExpression(expr *CallExpression) error
	WalkTupleLiteralExpression(expr *TupleLiteralExpression) error
	WalkMemberExpression(expr *MemberExpression) error
	WalkIfExpression(expr *IfExpression) error
	WalkMatchExpression(expr *MatchExpression) error
	WalkBinaryExpression(expr *BinaryExpression) error
	WalkUnaryExpression(expr *UnaryExpression) error
	WalkAssignmentStatement(stmt *AssignmentStatement) error
	WalkLoopStatement(stmt *LoopStatement) error
}

type DefaultVisitor struct{}

func (_ *DefaultVisitor) VisitIdentExpression(expr *IdentExpression) error {
	return nil
}

func (_ *DefaultVisitor) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	return nil
}

func (_ *DefaultVisitor) VisitCharLiteralExpression(expr *CharLiteralExpression) error {
	return nil
}

func (_ *DefaultVisitor) VisitIntLiteralExpression(expr *IntLiteralExpression) error {
	return nil
}

func (_ *DefaultVisitor) VisitBoolLiteralExpression(expr *BoolLiteralExpression) error {
	return nil
}

func (_ *DefaultVisitor) VisitTupleLiteralExpression(expr *TupleLiteralExpression, w Walker) error {
	return w.WalkTupleLiteralExpression(expr)
}

func (_ *DefaultVisitor) VisitCallExpression(expr *CallExpression, w Walker) error {
	return w.WalkCallExpression(expr)
}

func (_ *DefaultVisitor) VisitMemberExpression(expr *MemberExpression, w Walker) error {
	return w.WalkMemberExpression(expr)
}

func (_ *DefaultVisitor) VisitIfExpression(expr *IfExpression, w Walker) error {
	return w.WalkIfExpression(expr)
}

func (_ *DefaultVisitor) VisitMatchExpression(expr *MatchExpression, w Walker) error {
	return w.WalkMatchExpression(expr)
}

func (_ *DefaultVisitor) VisitBinaryExpression(expr *BinaryExpression, w Walker) error {
	return w.WalkBinaryExpression(expr)
}

func (_ *DefaultVisitor) VisitUnaryExpression(expr *UnaryExpression, w Walker) error {
	return w.WalkUnaryExpression(expr)
}

func (_ *DefaultVisitor) VisitBlockExpression(expr *BlockExpression, w Walker) error {
	return w.WalkBlockExpression(expr)
}

func (_ *DefaultVisitor) VisitExpression(expr Expression, w Walker) error {
	return w.WalkExpression(expr)
}

func (_ *DefaultVisitor) VisitFunctionDeclaration(decl *FunctionDeclaration) error {
	return nil
}

func (_ *DefaultVisitor) VisitFunctionDefinition(fn *FunctionDefinition, w Walker) error {
	return w.WalkFunctionDefinition(fn)
}

func (_ *DefaultVisitor) VisitTraitDeclaration(impl *TraitDeclaration, w Walker) error {
	return w.WalkTraitDeclaration(impl)
}

func (_ *DefaultVisitor) VisitImplDefinition(impl *ImplDefinition, w Walker) error {
	return w.WalkImplDefinition(impl)
}

func (_ *DefaultVisitor) VisitVariableDefinition(variable *VariableDefinition, w Walker) error {
	return w.WalkVariableDefinition(variable)
}

func (_ *DefaultVisitor) VisitAssignmentStatement(stmt *AssignmentStatement, w Walker) error {
	return w.WalkAssignmentStatement(stmt)
}

func (_ *DefaultVisitor) VisitLoopStatement(stmt *LoopStatement, w Walker) error {
	return w.WalkLoopStatement(stmt)
}

func (_ *DefaultVisitor) VisitBreakStatement(stmt *BreakStatement) error {
	return nil
}

func (_ *DefaultVisitor) VisitContinueStatement(stmt *ContinueStatement) error {
	return nil
}

func (_ *DefaultVisitor) VisitStructTypeDeclaration(tc *StructTypeDeclaration) error {
	return nil
}

func (_ *DefaultVisitor) VisitUnionTypeDeclaration(tc *UnionTypeDeclaration) error {
	return nil
}

func (_ *DefaultVisitor) VisitModule(module *Module, w Walker) error {
	return w.WalkModule(module)
}

func (_ *DefaultVisitor) VisitNode(node Node, w Walker) error {
	return w.WalkNode(node)
}

type DefaultWalker struct {
	Visitor Visitor
}

func (w *DefaultWalker) WalkExpression(expr Expression) error {
	var err error
	switch expr := expr.(type) {
	case *IdentExpression:
		err = w.Visitor.VisitIdentExpression(expr)
	case *StringLiteralExpression:
		err = w.Visitor.VisitStringLiteralExpression(expr)
	case *CharLiteralExpression:
		err = w.Visitor.VisitCharLiteralExpression(expr)
	case *IntLiteralExpression:
		err = w.Visitor.VisitIntLiteralExpression(expr)
	case *BoolLiteralExpression:
		err = w.Visitor.VisitBoolLiteralExpression(expr)
	case *BinaryExpression:
		err = w.Visitor.VisitBinaryExpression(expr, w)
	case *UnaryExpression:
		err = w.Visitor.VisitUnaryExpression(expr, w)
	case *CallExpression:
		err = w.Visitor.VisitCallExpression(expr, w)
	case *MemberExpression:
		err = w.Visitor.VisitMemberExpression(expr, w)
	case *TupleLiteralExpression:
		err = w.Visitor.VisitTupleLiteralExpression(expr, w)
	case *IfExpression:
		err = w.Visitor.VisitIfExpression(expr, w)
	case *MatchExpression:
		err = w.Visitor.VisitMatchExpression(expr, w)
	case *BlockExpression:
		err = w.Visitor.VisitBlockExpression(expr, w)
	default:
		return errors.Errorf("VisitExpression not implemented for expression type: %T", expr)
	}
	return err
}

func (w *DefaultWalker) WalkBinaryExpression(expr *BinaryExpression) error {
	if err := w.Visitor.VisitNode(expr.Lhs, w); err != nil {
		return err
	}
	return w.Visitor.VisitNode(expr.Rhs, w)
}

func (w *DefaultWalker) WalkUnaryExpression(expr *UnaryExpression) error {
	return w.Visitor.VisitNode(expr.Value, w)
}

func (w *DefaultWalker) WalkCallExpression(expr *CallExpression) error {
	if err := w.Visitor.VisitNode(expr.Callee, w); err != nil {
		return err
	}
	for _, arg := range expr.Args {
		if err := w.Visitor.VisitNode(arg.Value, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkIfExpression(expr *IfExpression) error {
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

func (w *DefaultWalker) WalkMatchExpression(expr *MatchExpression) error {
	if err := w.Visitor.VisitNode(expr.Expression, w); err != nil {
		return err
	}
	for _, arm := range expr.Arms {
		if err := w.Visitor.VisitNode(arm.Body, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkBlockExpression(expr *BlockExpression) error {
	for _, node := range expr.Nodes {
		if err := w.Visitor.VisitNode(node, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkMemberExpression(expr *MemberExpression) error {
	return w.Visitor.VisitNode(expr.Target, w)
}

func (w *DefaultWalker) WalkTupleLiteralExpression(expr *TupleLiteralExpression) error {
	for _, value := range expr.Values {
		if err := w.Visitor.VisitNode(value, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkModule(module *Module) error {
	for _, node := range module.Nodes {
		if err := w.Visitor.VisitNode(node, w); err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkTraitDeclaration(impl *TraitDeclaration) error {
	for _, decl := range impl.MethodDecls {
		err := w.Visitor.VisitFunctionDeclaration(decl)
		if err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkImplDefinition(impl *ImplDefinition) error {
	for _, method := range impl.Methods {
		err := w.Visitor.VisitFunctionDefinition(method, w)
		if err != nil {
			return err
		}
	}
	return nil
}

func (w *DefaultWalker) WalkFunctionDefinition(fn *FunctionDefinition) error {
	if err := w.Visitor.VisitFunctionDeclaration(fn.Decl); err != nil {
		return nil
	}
	return w.Visitor.VisitBlockExpression(fn.Body, w)
}

func (w *DefaultWalker) WalkVariableDefinition(variable *VariableDefinition) error {
	return w.Visitor.VisitNode(variable.Value, w)
}

func (w *DefaultWalker) WalkAssignmentStatement(stmt *AssignmentStatement) error {
	if err := w.Visitor.VisitNode(stmt.Variable, w); err != nil {
		return err
	}
	return w.Visitor.VisitNode(stmt.Rhs, w)
}

func (w *DefaultWalker) WalkLoopStatement(stmt *LoopStatement) error {
	return w.Visitor.VisitBlockExpression(stmt.Body, w)
}

func (w *DefaultWalker) WalkNode(node Node) error {
	var err error
	switch node := node.(type) {
	case *Module:
		err = w.Visitor.VisitModule(node, w)
	case *StructTypeDeclaration:
		err = w.Visitor.VisitStructTypeDeclaration(node)
	case *UnionTypeDeclaration:
		err = w.Visitor.VisitUnionTypeDeclaration(node)
	case *TraitDeclaration:
		err = w.Visitor.VisitTraitDeclaration(node, w)
	case *ImplDefinition:
		err = w.Visitor.VisitImplDefinition(node, w)
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
