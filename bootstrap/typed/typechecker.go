package typed

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
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

type typeEnvironment struct {
	types  map[string]Type
	parent *typeEnvironment
}

func (te *typeEnvironment) lookup(name string) (Type, bool) {
	ty, found := te.types[name]
	if !found && te.parent != nil {
		return te.parent.lookup(name)
	}
	return ty, found
}

func (te *typeEnvironment) declare(name string, ty Type) error {
	if _, found := te.types[name]; found {
		return fmt.Errorf("type %s already declared", name)
	}
	te.types[name] = ty
	return nil
}

type typeChecker struct {
	ast.DefaultASTVisitor
	typeByNodeId map[ast.NodeId]Type
	typeEnv      *typeEnvironment
}

func (tc *typeChecker) enterScope() {
	tc.typeEnv = &typeEnvironment{types: make(map[string]Type), parent: tc.typeEnv}
}

func (tc *typeChecker) exitScope() {
	tc.typeEnv = tc.typeEnv.parent
}

func (tc *typeChecker) mustLookup(node ast.Node) Type {
	ty, found := tc.typeByNodeId[node.Id()]
	if !found {
		panic(fmt.Sprintf("Type not found for node #%d: %s", node.Id(), node))
	}
	return ty
}

func (tc *typeChecker) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	tc.typeByNodeId[expr.Id()] = &StrType{}
	return nil
}

func (tc *typeChecker) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	tc.typeByNodeId[expr.Id()] = &BoolType{}
	return nil
}

func (tc *typeChecker) VisitIdentExpression(expr *ast.IdentExpression) error {
	ty, found := tc.typeEnv.lookup(expr.Name)
	if !found {
		return fmt.Errorf("type not found for identifier %s", expr.Name)
	}
	tc.typeByNodeId[expr.Id()] = ty
	return nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.ASTWalker) error {
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
	tc.typeByNodeId[expr.Id()] = funcType.ReturnType
	return nil
}

func (tc *typeChecker) VisitBlockExpression(expr *ast.BlockExpression, w ast.ASTWalker) error {
	if err := w.WalkBlockExpression(expr); err != nil {
		return fmt.Errorf("failed to walk block expression: %w", err)
	}
	blockType := tc.mustLookup(expr.Nodes[len(expr.Nodes)-1])
	tc.typeByNodeId[expr.Id()] = blockType
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.ASTWalker) error {
	if err := w.WalkIfExpression(expr); err != nil {
		return fmt.Errorf("failed to walk if expression: %w", err)
	}
	condType := tc.mustLookup(expr.Condition)
	_, ok := tc.mustLookup(expr.Condition).(*BoolType)
	if !ok {
		return fmt.Errorf("the condition of an if expression must be a boolean type, got: %s", condType)
	}
	trueBodyType := tc.mustLookup(expr.TrueBody)
	tc.typeByNodeId[expr.Id()] = trueBodyType
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.ASTWalker) error {
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
	tc.typeByNodeId[fn.Id()] = funcType
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

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.ASTWalker) error {
	tc.typeByNodeId[module.Id()] = &UnitType{}
	return w.WalkModule(module)
}

func (tc *typeChecker) check(node ast.Node, w ast.ASTWalker) (Type, error) {
	if err := w.WalkNode(node); err != nil {
		return nil, err
	}
	nodeType := tc.mustLookup(node)
	return nodeType, nil

}

func TypeCheck(node ast.Node) (Type, map[ast.NodeId]Type, error) {
	defaultTypeEnv := &typeEnvironment{types: make(map[string]Type)}
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
	tc := &typeChecker{
		DefaultASTVisitor: ast.DefaultASTVisitor{},
		typeByNodeId:      make(map[ast.NodeId]Type),
		typeEnv:           defaultTypeEnv,
	}
	walker := &ast.DefaultASTWalker{Visitor: tc}
	res, err := tc.check(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, tc.typeByNodeId, nil
}
