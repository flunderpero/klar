package typed

import (
	"fmt"
	"slices"

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

type Int64Type struct{}

func (ty *Int64Type) String() string {
	return "Int64Type()"
}

type UnitType struct{}

func (ty *UnitType) String() string {
	return "UnitType()"
}

type StructField struct {
	Name ast.Ident
	Type Type
}

func (f *StructField) String() string {
	return fmt.Sprintf("%s = %s", f.Name, f.Type)
}

type StructType struct {
	Name        ast.TypeIdent
	Fields      []StructField
	Declaration *ast.StructTypeDeclaration
}

func (ty *StructType) String() string {
	fields := ""
	for _, field := range ty.Fields {
		if fields != "" {
			fields += ", "
		}
		fields += field.String()
	}
	return fmt.Sprintf("StructType(%s, %s)", ty.Name, fields)
}

func (ty *StructType) FindFieldIndex(name ast.Ident) (int, error) {
	fieldIndex := slices.IndexFunc(ty.Fields, func(field StructField) bool { return field.Name == name })
	if fieldIndex < 0 {
		return -1, fmt.Errorf("field %q not found in struct type %q", name, ty)
	}
	return fieldIndex, nil
}

func (ty *StructType) FindField(name ast.Ident) (*StructField, error) {
	fieldIndex, err := ty.FindFieldIndex(name)
	if err != nil {
		return nil, err
	}
	return &ty.Fields[fieldIndex], nil
}

type FunctionType struct {
	Name       ast.Ident
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
	types     map[string]Type
	variables map[string]*ast.VariableDefinition
	parent    *typeEnvironment
}

func newTypeEnvironment(parent *typeEnvironment) *typeEnvironment {
	return &typeEnvironment{
		types:     make(map[string]Type),
		variables: make(map[string]*ast.VariableDefinition),
		parent:    parent,
	}
}

func (te *typeEnvironment) lookup(name string) (Type, bool) {
	ty, found := te.types[name]
	if !found && te.parent != nil {
		return te.parent.lookup(name)
	}
	return ty, found
}

func (te *typeEnvironment) lookupVariable(name ast.Ident) (Type, *ast.VariableDefinition, bool) {
	ty, found := te.lookup(string(name))
	if !found {
		return nil, nil, false
	}
	def, found := te.variables[string(name)]
	if !found && te.parent != nil {
		return te.parent.lookupVariable(name)
	}
	return ty, def, found
}

func (te *typeEnvironment) declare(name string, ty Type) error {
	if _, found := te.types[name]; found {
		return fmt.Errorf("type %s already declared", name)
	}
	te.types[name] = ty
	return nil
}

func (te *typeEnvironment) declareVariable(name string, ty Type, def *ast.VariableDefinition) error {
	if err := te.declare(name, ty); err != nil {
		return err
	}
	te.variables[name] = def
	return nil
}

type TypeByNode struct {
	types map[ast.NodeId]Type
}

func (m *TypeByNode) Lookup(node ast.Node) (Type, error) {
	ty := m.types[node.Id()]
	if ty == nil {
		return nil, fmt.Errorf("type not found for node #%d: %s", node.Id(), node)
	}
	return ty, nil
}

func (m *TypeByNode) LookupType(node ast.Node, ty Type) (Type, error) {
	got, err := m.Lookup(node)
	if err != nil {
		return nil, err
	}
	if got != ty {
		return nil, fmt.Errorf("expected type %s, got %s", ty, got)
	}
	return got, nil
}

func (m *TypeByNode) MustLookup(node ast.Node) Type {
	ty, err := m.Lookup(node)
	if err != nil {
		panic(err)
	}
	return ty
}

func (m *TypeByNode) set(node ast.Node, ty Type) {
	m.types[node.Id()] = ty
}

type typeChecker struct {
	ast.DefaultVisitor
	typeByNode TypeByNode
	typeEnv    *typeEnvironment
	loopDepth  int
}

func (tc *typeChecker) enterScope() {
	tc.typeEnv = newTypeEnvironment(tc.typeEnv)
}

func (tc *typeChecker) exitScope() {
	tc.typeEnv = tc.typeEnv.parent
}

func (tc *typeChecker) enterLoop() {
	tc.loopDepth += 1
}

func (tc *typeChecker) exitLoop() {
	tc.loopDepth -= 1
}

func (tc *typeChecker) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	tc.typeByNode.set(expr, &StrType{})
	return nil
}

func (tc *typeChecker) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	tc.typeByNode.set(expr, &Int64Type{})
	return nil
}

func (tc *typeChecker) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	tc.typeByNode.set(expr, &BoolType{})
	return nil
}

func (tc *typeChecker) VisitIdentExpression(expr *ast.IdentExpression) error {
	ty, found := tc.typeEnv.lookup(string(expr.Ident))
	if !found {
		return fmt.Errorf("type not found for identifier %s", expr.Ident)
	}
	tc.typeByNode.set(expr, ty)
	return nil
}

func (tc *typeChecker) VisitBinaryExpression(expr *ast.BinaryExpression, w ast.Walker) error {
	if err := w.WalkBinaryExpression(expr); err != nil {
		return err
	}
	lhs := tc.typeByNode.MustLookup(expr.Lhs)
	rhs := tc.typeByNode.MustLookup(expr.Rhs)
	switch expr.Op {
	case ast.OpAdd:
		if _, ok := lhs.(*Int64Type); !ok {
			return fmt.Errorf("lhs of add expression must be of type Int64Type, got %s", lhs)
		}
		if _, ok := rhs.(*Int64Type); !ok {
			return fmt.Errorf("rhs of add expression must be of type Int64Type, got %s", rhs)
		}
		tc.typeByNode.set(expr, &Int64Type{})
	case ast.OpEquality:
		// For now, we only support equality of numbers.
		if _, ok := lhs.(*Int64Type); !ok {
			return fmt.Errorf("lhs of equality expression must be of type Int64Type, got %s", lhs)
		}
		if _, ok := rhs.(*Int64Type); !ok {
			return fmt.Errorf("rhs of equality expression must be of type Int64Type, got %s", rhs)
		}
		tc.typeByNode.set(expr, &BoolType{})
	default:
		return fmt.Errorf("unsupported binary operator: %s", expr.Op)
	}
	return nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return fmt.Errorf("failed to walk call expression: %w", err)
	}
	calleeType := tc.typeByNode.MustLookup(expr.Callee)
	funcType, ok := calleeType.(*FunctionType)
	if !ok {
		return fmt.Errorf("callee %s is not a function type", calleeType)
	}
	if len(funcType.ArgTypes) != len(expr.Args) {
		return fmt.Errorf("expected %d arguments, got %d for function %s", len(funcType.ArgTypes), len(expr.Args), funcType)
	}
	for i, arg := range expr.Args {
		argType := tc.typeByNode.MustLookup(arg)
		if argType != funcType.ArgTypes[i] {
			return fmt.Errorf("expected argument %d to be of type %q, got %q", i, funcType.ArgTypes[i], argType)
		}
	}
	tc.typeByNode.set(expr, funcType.ReturnType)
	return nil
}

func (tc *typeChecker) VisitMemberExpression(expr *ast.MemberExpression, w ast.Walker) error {
	if err := w.WalkMemberExpression(expr); err != nil {
		return fmt.Errorf("failed to walk member expression: %w", err)
	}
	structType_ := tc.typeByNode.MustLookup(expr.Target)
	structType, isType := structType_.(*StructType)
	if !isType {
		return fmt.Errorf("type %q is not a struct type", structType_)
	}
	structField, err := structType.FindField(expr.Field)
	if err != nil {
		return err
	}
	tc.typeByNode.set(expr, structField.Type)
	return nil
}

func (tc *typeChecker) VisitBlockExpression(expr *ast.BlockExpression, w ast.Walker) error {
	if err := w.WalkBlockExpression(expr); err != nil {
		return fmt.Errorf("failed to walk block expression: %w", err)
	}
	blockType := tc.typeByNode.MustLookup(expr.Nodes[len(expr.Nodes)-1])
	tc.typeByNode.set(expr, blockType)
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	tc.enterScope()
	defer tc.exitScope()
	if err := w.WalkIfExpression(expr); err != nil {
		return fmt.Errorf("failed to walk if expression: %w", err)
	}
	condType := tc.typeByNode.MustLookup(expr.Condition)
	_, ok := tc.typeByNode.MustLookup(expr.Condition).(*BoolType)
	if !ok {
		return fmt.Errorf("the condition of an if expression must be a boolean type, got: %s", condType)
	}
	// Only an if expression with an else branch can have a type other than unit.
	// And currently we don't have else branches.
	tc.typeByNode.set(expr, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitStructInitExpression(expr *ast.StructInitExpression, w ast.Walker) error {
	if err := w.WalkStructInitExpression(expr); err != nil {
		return err
	}
	structType_, found := tc.typeEnv.lookup(string(expr.TypeIdent))
	if !found {
		return fmt.Errorf("type %q not found for struct init expression", expr.TypeIdent)
	}
	structType, isType := structType_.(*StructType)
	if !isType {
		return fmt.Errorf("type %q is not a struct type", expr.TypeIdent)
	}
	for _, initField := range expr.Fields {
		structField, err := structType.FindField(initField.Name)
		if err != nil {
			return err
		}
		fieldType := tc.typeByNode.MustLookup(initField.Value)
		if structField.Type != fieldType {
			return fmt.Errorf("expected field %q to be of type %q, got %q", initField.Name, structField.Type, fieldType)
		}
	}
	if len(expr.Fields) != len(structType.Fields) {
		return fmt.Errorf("expected %d fields, got %d", len(structType.Fields), len(expr.Fields))
	}
	tc.typeByNode.set(expr, structType)
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.Walker) error {
	argTypes := []Type{}
	for _, arg := range fn.Args {
		argType, found := tc.typeEnv.lookup(string(arg.Type))
		if !found {
			return fmt.Errorf("type %s not found for argument %s", arg.Type, arg.Name)
		}
		argTypes = append(argTypes, argType)
	}
	var returnType Type = &UnitType{}
	if fn.ReturnType != "" {
		ty, found := tc.typeEnv.lookup(string(fn.ReturnType))
		if !found {
			return fmt.Errorf("type %s not found for return type of function %s", fn.ReturnType, fn.Name)
		}
		returnType = ty
	}
	funcType := &FunctionType{
		Name:       fn.Name,
		ArgTypes:   argTypes,
		ReturnType: returnType,
	}
	tc.typeByNode.set(fn, funcType)
	if err := tc.typeEnv.declare(string(fn.Name), funcType); err != nil {
		return fmt.Errorf("failed to declare function %s: %w", fn.Name, err)
	}
	tc.enterScope()
	defer tc.exitScope()
	for i, arg := range fn.Args {
		argType := argTypes[i]
		if err := tc.typeEnv.declare(string(arg.Name), argType); err != nil {
			return fmt.Errorf("failed to declare argument %s: %w", arg.Name, err)
		}
	}
	if err := w.WalkFunctionDefinition(fn); err != nil {
		return fmt.Errorf("failed to walk function definition: %w", err)
	}
	return nil
}

func (tc *typeChecker) VisitVariableDefinition(v *ast.VariableDefinition, w ast.Walker) error {
	if err := w.WalkNode(v.Value); err != nil {
		return err
	}
	valueType := tc.typeByNode.MustLookup(v.Value)
	if _, ok := valueType.(*UnitType); ok {
		return fmt.Errorf("variable %s must have a non-unit type", v.Name)
	}
	if err := tc.typeEnv.declareVariable(string(v.Name), valueType, v); err != nil {
		return err
	}
	tc.typeByNode.set(v, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitAssignmentStatement(s *ast.AssignmentStatement, w ast.Walker) error {
	if err := w.WalkAssignmentStatement(s); err != nil {
		return err
	}
	rhsType := tc.typeByNode.MustLookup(s.Rhs)
	varType, varDefinition, ok := tc.typeEnv.lookupVariable(s.Variable.Ident)
	if !ok {
		return fmt.Errorf("unknown variable %q", s.Variable.Ident)
	}
	if !varDefinition.Mutable {
		return fmt.Errorf("variable %q is not mutable", s.Variable.Ident)
	}
	if s.IsAssignToMember() {
		structType, ok := varType.(*StructType)
		if !ok {
			return fmt.Errorf("variable %q is not a struct type", s.Variable.Ident)
		}
		field, err := structType.FindField(*s.Field)
		if err != nil {
			return err
		}
		varType = field.Type
	}
	if varType != rhsType {
		return fmt.Errorf("lhs and rhs of assignment statement must have the same type, got %s and %s", varType, rhsType)
	}
	tc.typeByNode.set(s, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitLoopStatement(s *ast.LoopStatement, w ast.Walker) error {
	tc.typeByNode.set(s, &UnitType{})
	tc.enterLoop()
	defer tc.exitLoop()
	return w.WalkLoopStatement(s)
}

func (tc *typeChecker) VisitContinueStatement(s *ast.ContinueStatement) error {
	if tc.loopDepth == 0 {
		return fmt.Errorf("continue statement outside of a loop")
	}
	tc.typeByNode.set(s, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return fmt.Errorf("break statement outside of a loop")
	}
	tc.typeByNode.set(s, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitStructTypeDeclaration(d *ast.StructTypeDeclaration) error {
	fields := []StructField{}
	for _, field := range d.Fields {
		fieldType, found := tc.typeEnv.lookup(string(field.Type))
		if !found {
			return fmt.Errorf("type %q not found for field %q", field.Type, field.Name)
		}
		fields = append(fields, StructField{Name: field.Name, Type: fieldType})
	}
	structType := &StructType{Name: d.Name, Fields: fields, Declaration: d}
	if err := tc.typeEnv.declare(string(d.Name), structType); err != nil {
		return err
	}
	tc.typeByNode.set(d, &UnitType{})
	return nil
}

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.Walker) error {
	tc.typeByNode.set(module, &UnitType{})
	return w.WalkModule(module)
}

func (tc *typeChecker) check(node ast.Node, w ast.Walker) (Type, error) {
	if err := w.WalkNode(node); err != nil {
		return nil, err
	}
	nodeType := tc.typeByNode.MustLookup(node)
	return nodeType, nil
}

func TypeCheck(node ast.Node) (Type, *TypeByNode, error) {
	defaultTypeEnv := newTypeEnvironment(nil)
	// Declare builtin types.
	if err := defaultTypeEnv.declare("Str", &StrType{}); err != nil {
		panic(fmt.Errorf("Failed to declare Str type: %w", err))
	}
	if err := defaultTypeEnv.declare("Int", &Int64Type{}); err != nil {
		panic(fmt.Errorf("Failed to declare Int type: %w", err))
	}
	if err := defaultTypeEnv.declare("()", &UnitType{}); err != nil {
		panic(fmt.Errorf("Failed to declare UnitType type: %w", err))
	}
	if err := defaultTypeEnv.declare("print", &FunctionType{
		Name:       "print",
		ArgTypes:   []Type{&StrType{}},
		ReturnType: &UnitType{},
	}); err != nil {
		panic(fmt.Errorf("Failed to declare print function: %w", err))
	}
	if err := defaultTypeEnv.declare("print_int", &FunctionType{
		Name:       "print_int",
		ArgTypes:   []Type{&Int64Type{}},
		ReturnType: &UnitType{},
	}); err != nil {
		panic(fmt.Errorf("Failed to declare print_int function: %w", err))
	}
	tc := &typeChecker{
		DefaultVisitor: ast.DefaultVisitor{},
		typeByNode:     TypeByNode{types: make(map[ast.NodeId]Type)},
		typeEnv:        defaultTypeEnv,
	}
	walker := &ast.DefaultWalker{Visitor: tc}
	res, err := tc.check(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, &tc.typeByNode, nil
}
