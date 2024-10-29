package main

import "fmt"

type Type interface {
	String() string
}

type StringType struct{}

func (ty *StringType) String() string {
	return "StringType()"
}

type BoolType struct{}

func (ty *BoolType) String() string {
	return "BoolType()"
}

type UnitType struct{}

func (ty *UnitType) String() string {
	return "UnitType()"
}

type FunctionType struct {
	Name       string
	ArgTypes   []Type
	ReturnType Type
}

func (ty *FunctionType) String() string {
	return fmt.Sprintf("FunctionType(%s, %v, %s)", ty.Name, ty.ArgTypes, ty.ReturnType)
}

type TypeEnvironment struct {
	types map[string]Type
}

func (te *TypeEnvironment) lookup(name string) (Type, bool) {
	ty, found := te.types[name]
	return ty, found
}

func (te *TypeEnvironment) declare(name string, ty Type) error {
	if _, found := te.lookup(name); found {
		return fmt.Errorf("type %s already declared", name)
	}
	te.types[name] = ty
	return nil
}

type TypeChecker struct {
	DefaultASTVisitor
	typeByNodeId map[NodeId]Type
	typeEnv      TypeEnvironment
}

func (tc *TypeChecker) mustLookup(node Node) Type {
	ty, found := tc.typeByNodeId[node.Id()]
	if !found {
		panic(fmt.Sprintf("Type not found for node #%d: %s", node.Id(), node))
	}
	return ty
}

func (tc *TypeChecker) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	tc.typeByNodeId[expr.id] = &StringType{}
	return nil
}

func (tc *TypeChecker) VisitBoolLiteralExpression(expr *BoolLiteralExpression) error {
	tc.typeByNodeId[expr.id] = &BoolType{}
	return nil
}

func (tc *TypeChecker) VisitIdentExpression(expr *IdentExpression) error {
	ty, found := tc.typeEnv.lookup(expr.Name)
	if !found {
		return fmt.Errorf("type not found for identifier %s", expr.Name)
	}
	tc.typeByNodeId[expr.id] = ty
	return nil
}

func (tc *TypeChecker) VisitCallExpression(expr *CallExpression, w ASTWalker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return fmt.Errorf("failed to walk call expression: %w", err)
	}
	calleeType := tc.mustLookup(expr.Callee)
	funcType, ok := calleeType.(*FunctionType)
	if !ok {
		return fmt.Errorf("callee %s is not a function type", calleeType)
	}
	if len(funcType.ArgTypes) != len(expr.Args) {
		return fmt.Errorf("expected %d arguments, got %d for function %s", len(funcType.ArgTypes), len(expr.Args), funcType)
	}
	for i, arg := range expr.Args {
		argType := tc.mustLookup(arg)
		if argType != funcType.ArgTypes[i] {
			return fmt.Errorf("expected argument %d to be of type %s, got %s", i, funcType.ArgTypes[i], argType)
		}
	}
	tc.typeByNodeId[expr.id] = funcType.ReturnType
	return nil
}

func (tc *TypeChecker) VisitBlockExpression(expr *BlockExpression, w ASTWalker) error {
	if err := w.WalkBlockExpression(expr); err != nil {
		return fmt.Errorf("failed to walk block expression: %w", err)
	}
	blockType := tc.mustLookup(expr.Nodes[len(expr.Nodes)-1])
	tc.typeByNodeId[expr.id] = blockType
	return nil
}

func (tc *TypeChecker) VisitIfExpression(expr *IfExpression, w ASTWalker) error {
	if err := w.WalkIfExpression(expr); err != nil {
		return fmt.Errorf("failed to walk if expression: %w", err)
	}
	condType := tc.mustLookup(expr.Condition)
	_, ok := tc.mustLookup(expr.Condition).(*BoolType)
	if !ok {
		return fmt.Errorf("the condition of an if expression must be a boolean type, got: %s", condType)
	}
	trueBodyType := tc.mustLookup(expr.TrueBody)
	tc.typeByNodeId[expr.id] = trueBodyType
	return nil
}

func (tc *TypeChecker) VisitModule(module *Module, w ASTWalker) error {
	tc.typeByNodeId[module.id] = &UnitType{}
	return w.WalkModule(module)
}

func (tc *TypeChecker) TypeCheck(node Node, w ASTWalker) (Type, error) {
	if err := w.WalkNode(node); err != nil {
		return nil, err
	}
	nodeType := tc.mustLookup(node)
	return nodeType, nil

}

func TypeCheck(node Node) (Type, map[NodeId]Type, error) {
	defaultTypeEnv := TypeEnvironment{types: make(map[string]Type)}
	// Declare builtin types.
	if err := defaultTypeEnv.declare("String", &StringType{}); err != nil {
		panic(fmt.Errorf("Failed to declare String type: %w", err))
	}
	if err := defaultTypeEnv.declare("print", &FunctionType{
		Name:       "print",
		ArgTypes:   []Type{&StringType{}},
		ReturnType: &UnitType{},
	}); err != nil {
		panic(fmt.Errorf("Failed to declare print function: %w", err))
	}
	tc := &TypeChecker{
		DefaultASTVisitor: DefaultASTVisitor{},
		typeByNodeId:      make(map[NodeId]Type),
		typeEnv:           defaultTypeEnv,
	}
	walker := &DefaultASTWalker{Visitor: tc}
	res, err := tc.TypeCheck(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, tc.typeByNodeId, nil
}
