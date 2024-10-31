package main

import (
	"fmt"
)

type Type interface {
	String() string
}

type StrType struct{}

func (ty *StrType) String() string {
	return "StrType()"
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
	args := ""
	for _, arg := range ty.ArgTypes {
		if args != "" {
			args += ", "
		}
		args += arg.String()
	}
	return fmt.Sprintf("FunctionType(%s, %s, %s)", ty.Name, args, ty.ReturnType)
}

type TypeEnvironment struct {
	types  map[string]Type
	parent *TypeEnvironment
}

func (te *TypeEnvironment) lookup(name string) (Type, bool) {
	ty, found := te.types[name]
	if !found && te.parent != nil {
		return te.parent.lookup(name)
	}
	return ty, found
}

func (te *TypeEnvironment) declare(name string, ty Type) error {
	if _, found := te.types[name]; found {
		return fmt.Errorf("type %s already declared", name)
	}
	te.types[name] = ty
	return nil
}

type TypeChecker struct {
	DefaultASTVisitor
	typeByNodeId map[NodeId]Type
	typeEnv      *TypeEnvironment
}

func (tc *TypeChecker) enterScope() {
	tc.typeEnv = &TypeEnvironment{types: make(map[string]Type), parent: tc.typeEnv}
}

func (tc *TypeChecker) exitScope() {
	tc.typeEnv = tc.typeEnv.parent
}

func (tc *TypeChecker) mustLookup(node Node) Type {
	ty, found := tc.typeByNodeId[node.Id()]
	if !found {
		panic(fmt.Sprintf("Type not found for node #%d: %s", node.Id(), node))
	}
	return ty
}

func (tc *TypeChecker) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	tc.typeByNodeId[expr.id] = &StrType{}
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

func (tc *TypeChecker) VisitFunctionDefinition(fn *FunctionDefinition, w ASTWalker) error {
	argTypes := []Type{}
	for _, arg := range fn.Args {
		argType, found := tc.typeEnv.lookup(arg.Type)
		if !found {
			return fmt.Errorf("type %s not found for argument %s", arg.Type, arg.Name)
		}
		tc.typeByNodeId[arg.Id()] = argType
		argTypes = append(argTypes, argType)
	}
	funcType := &FunctionType{
		Name:       fn.Name,
		ArgTypes:   argTypes,
		ReturnType: &UnitType{},
	}
	tc.typeByNodeId[fn.id] = funcType
	if err := tc.typeEnv.declare(fn.Name, funcType); err != nil {
		return fmt.Errorf("failed to declare function %s: %w", fn.Name, err)
	}
	tc.enterScope()
	defer tc.exitScope()
	for i, arg := range fn.Args {
		argType := argTypes[i]
		if err := tc.typeEnv.declare(arg.Name, argType); err != nil {
			return fmt.Errorf("failed to declare argument %s: %w", arg.Name, err)
		}
	}
	if err := w.WalkFunctionDefinition(fn); err != nil {
		return fmt.Errorf("failed to walk function definition: %w", err)
	}
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
	defaultTypeEnv := &TypeEnvironment{types: make(map[string]Type)}
	// Declare builtin types.
	if err := defaultTypeEnv.declare("Str", &StrType{}); err != nil {
		panic(fmt.Errorf("Failed to declare Str type: %w", err))
	}
	if err := defaultTypeEnv.declare("print", &FunctionType{
		Name:       "print",
		ArgTypes:   []Type{&StrType{}},
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
