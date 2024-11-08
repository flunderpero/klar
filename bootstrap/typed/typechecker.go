package typed

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ast"
)

type Type interface {
	String() string
}

var StrType = &strType{}
var BoolType = &boolType{}
var Int64Type = &int64Type{}
var UnitType = &unitType{}

type NamedType interface {
	String() string
	TypeName() string
}

type CallableType interface {
	String() string
	TypeName() string
	CallArgTypes() []*FunctionArg
	CallReturnType() Type
}

type TypeWithTraits interface {
	String() string
	Traits() *[]*TraitType
}

type strType struct {
	traits []*TraitType
}

func (ty *strType) String() string {
	return "StrType()"
}

func (ty *strType) Traits() *[]*TraitType {
	return &ty.traits
}

type boolType struct {
	traits []*TraitType
}

func (ty *boolType) Traits() *[]*TraitType {
	return &ty.traits
}

func (ty *boolType) String() string {
	return "BoolType()"
}

type int64Type struct {
	traits []*TraitType
}

func (ty *int64Type) Traits() *[]*TraitType {
	return &ty.traits
}

func (ty *int64Type) String() string {
	return "Int64Type()"
}

type unitType struct{}

func (ty *unitType) String() string {
	return "UnitType()"
}

type StructField struct {
	Name ast.Ident
	Type Type
}

func (f *StructField) String() string {
	return fmt.Sprintf("%s = %s", f.Name, f.Type)
}

type DeclaredType struct {
	Type Type
}

func (ty *DeclaredType) TypeName() string {
	return ty.Type.String()
}

func (ty *DeclaredType) String() string {
	return fmt.Sprintf("DeclaredType(%s)", ty.Type)
}

type StructType struct {
	Name    ast.TypeIdent
	Fields  []StructField
	Methods []*MethodType
	traits  []*TraitType
}

func (ty *StructType) Traits() *[]*TraitType {
	return &ty.traits
}

func (ty *StructType) TypeName() string {
	return string(ty.Name)
}

func (ty *StructType) String() string {
	fields := ""
	for _, field := range ty.Fields {
		if fields != "" {
			fields += ", "
		}
		fields += field.String()
	}
	methods := ""
	for _, method := range ty.Methods {
		methods += "\n    "
		methods += method.String()
	}
	return fmt.Sprintf("StructType(\n    %s\n    %s%s\n)", ty.Name, fields, methods)
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

func (ty *StructType) FindMethod(name ast.Ident) (*MethodType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method, nil
		}
	}
	return nil, fmt.Errorf("method %q not found in struct type %q", name, ty)
}

func (ty *StructType) FindMember(name ast.Ident) (Type, error) {
	field, err := ty.FindField(name)
	if err == nil {
		return field.Type, nil
	}
	method, err := ty.FindMethod(name)
	if err == nil {
		return method, nil
	}
	return nil, fmt.Errorf("member %q not found in struct type %q", name, ty)
}

type TraitType struct {
	Name    ast.TypeIdent
	Methods []*MethodType
}

func (ty *TraitType) String() string {
	methods := ""
	for _, method := range ty.Methods {
		methods += "\n    "
		methods += method.String()
	}
	return fmt.Sprintf("TraitType(\n    %s%s\n)", ty.Name, methods)
}

func (ty *TraitType) TypeName() string {
	return string(ty.Name)
}

func (ty *TraitType) FindMethod(name ast.Ident) (*MethodType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method, nil
		}
	}
	return nil, fmt.Errorf("method %q not found in trait type %q", name, ty)
}

type ImplType struct {
	ReceiverType Type
}

func (ty *ImplType) String() string {
	return fmt.Sprintf("ImplType(%s)", ty.ReceiverType)
}

type FunctionArg struct {
	Name ast.Ident
	Type Type
}

type MethodType struct {
	Name         ast.Ident
	Args         []*FunctionArg
	ReturnType   Type
	ReceiverType NamedType
}

func (ty *MethodType) TypeName() string {
	return string(ty.Name)
}

func (ty *MethodType) CallArgTypes() []*FunctionArg {
	return ty.Args
}

func (ty *MethodType) CallReturnType() Type {
	return ty.ReturnType
}

func (ty *MethodType) CheckSameSignatureIgnoringReceiverTypes(other *MethodType) error {
	match := func(thisType Type, otherType Type) bool {
		if otherType == other.ReceiverType {
			return thisType == ty.ReceiverType
		}
		return thisType == otherType
	}
	if ty.Name != other.Name {
		return fmt.Errorf("method names do not match: %s != %s", ty.Name, other.Name)
	}
	if len(ty.Args) != len(other.Args) {
		return fmt.Errorf("argument count does not match: %d != %d", len(ty.Args), len(other.Args))
	}
	if !match(ty.ReturnType, other.ReturnType) {
		return fmt.Errorf("return types do not match: %s != %s", ty.ReturnType, other.ReturnType)
	}
	for i, arg := range ty.Args {
		otherArg := other.Args[i]
		if !match(arg.Type, otherArg.Type) {
			return fmt.Errorf("argument types do not match: %s != %s", arg.Type, otherArg.Type)
		}
	}
	return nil
}

func (ty *MethodType) IsStatic() bool {
	return len(ty.Args) == 0 || ty.Args[0].Name != "self"
}

func (ty *MethodType) String() string {
	args := ""
	for _, arg := range ty.Args {
		if args != "" {
			args += ", "
		}
		var argType string
		if arg.Type == ty.ReceiverType.(Type) {
			argType = "Self"
		} else {
			argType = arg.Type.String()
		}
		args += fmt.Sprintf("%s %s", arg.Name, argType)
	}
	var ret string
	if ty.ReturnType == ty.ReceiverType.(Type) {
		ret = "Self"
	} else {
		ret = ty.ReturnType.String()
	}
	return fmt.Sprintf("FunctionType(%s, %s, %s)", ty.Name, args, ret)
}

func (ty *MethodType) ArgTypesWithoutSelf() []*FunctionArg {
	if len(ty.Args) > 0 && ty.Args[0].Name == "self" {
		return ty.Args[1:]
	}
	return ty.Args
}

func newMethodTypeFromFunctionType(functionType *FunctionType, receiverType NamedType) *MethodType {
	return &MethodType{
		Name:         functionType.Name,
		Args:         functionType.Args,
		ReturnType:   functionType.ReturnType,
		ReceiverType: receiverType,
	}
}

type FunctionType struct {
	Name       ast.Ident
	Args       []*FunctionArg
	ReturnType Type
}

func (ty *FunctionType) String() string {
	args := ""
	for _, arg := range ty.Args {
		if args != "" {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", arg.Name, arg.Type)
	}
	return fmt.Sprintf("FunctionType(%s, %s, %s)", ty.Name, args, ty.ReturnType)
}

func (ty *FunctionType) TypeName() string {
	return string(ty.Name)
}

func (ty *FunctionType) CallArgTypes() []*FunctionArg {
	return ty.Args
}

func (ty *FunctionType) CallReturnType() Type {
	return ty.ReturnType
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

func (te *typeEnvironment) isVariable(name string) bool {
	_, found := te.variables[name]
	return found
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

type TypeInfo struct {
	types map[ast.NodeId]Type
	// The type an `ReferenceExpression` points to if it does not refer to a variable.
	typeBindings map[ast.ReferenceExpression]NamedType
	Main         *FunctionType
}

func (m *TypeInfo) LookupTypeBinding(expr ast.ReferenceExpression) (NamedType, bool) {
	declaration, found := m.typeBindings[expr]
	return declaration, found
}

func (m *TypeInfo) Lookup(node ast.Node) (Type, error) {
	ty := m.types[node.Id()]
	if ty == nil {
		return nil, fmt.Errorf("type not found for node #%d: %s", node.Id(), node)
	}
	return ty, nil
}

func (m *TypeInfo) LookupType(node ast.Node, ty Type) (Type, error) {
	got, err := m.Lookup(node)
	if err != nil {
		return nil, err
	}
	if got != ty {
		return nil, fmt.Errorf("expected type %s, got %s", ty, got)
	}
	return got, nil
}

func (m *TypeInfo) MustLookup(node ast.Node) Type {
	ty, err := m.Lookup(node)
	if err != nil {
		panic(err)
	}
	return ty
}

func (m *TypeInfo) MustLookupDeclaredType(node ast.Node) *DeclaredType {
	ty := m.MustLookup(node)
	typeDecl, ok := m.MustLookup(node).(*DeclaredType)
	if !ok {
		panic(fmt.Errorf("expected type declaration, got %s", ty))
	}
	return typeDecl
}

func (m *TypeInfo) Set(node ast.Node, ty Type) {
	m.types[node.Id()] = ty
}

type typeChecker struct {
	ast.DefaultVisitor
	typeInfo  *TypeInfo
	typeEnv   *typeEnvironment
	loopDepth int
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
	tc.typeInfo.Set(expr, StrType)
	return nil
}

func (tc *typeChecker) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	tc.typeInfo.Set(expr, Int64Type)
	return nil
}

func (tc *typeChecker) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	tc.typeInfo.Set(expr, BoolType)
	return nil
}

func (tc *typeChecker) VisitReferenceExpression(expr ast.ReferenceExpression) error {
	var refStr string
	switch expr := expr.(type) {
	case *ast.TypeIdentExpression:
		refStr = string(expr.Ident)
	case *ast.IdentExpression:
		refStr = string(expr.Ident)
	default:
		panic(fmt.Sprintf("unexpected reference expression type: %T", expr))
	}
	ty, found := tc.typeEnv.lookup(refStr)
	if !found {
		return fmt.Errorf("type not found for identifier %s", refStr)
	}
	tc.typeInfo.Set(expr, ty)
	if !tc.typeEnv.isVariable(refStr) {
		if namedType, ok := ty.(NamedType); ok {
			tc.typeInfo.typeBindings[expr] = namedType
		}
	}
	return nil
}

func (tc *typeChecker) VisitBinaryExpression(expr *ast.BinaryExpression, w ast.Walker) error {
	if err := w.WalkBinaryExpression(expr); err != nil {
		return err
	}
	lhs := tc.typeInfo.MustLookup(expr.Lhs)
	rhs := tc.typeInfo.MustLookup(expr.Rhs)
	switch expr.Op {
	case ast.OpAdd:
		if lhs != Int64Type {
			return fmt.Errorf("lhs of add expression must be of type Int64Type, got %s", lhs)
		}
		if rhs != Int64Type {
			return fmt.Errorf("rhs of add expression must be of type Int64Type, got %s", rhs)
		}
		tc.typeInfo.Set(expr, Int64Type)
	case ast.OpEquality:
		// For now, we only support equality of numbers.
		if lhs != Int64Type {
			return fmt.Errorf("lhs of equality expression must be of type Int64Type, got %s", lhs)
		}
		if rhs != Int64Type {
			return fmt.Errorf("rhs of equality expression must be of type Int64Type, got %s", rhs)
		}
		tc.typeInfo.Set(expr, BoolType)
	default:
		return fmt.Errorf("unsupported binary operator: %s", expr.Op)
	}
	return nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return fmt.Errorf("failed to walk call expression: %w", err)
	}
	calleeType, ok := tc.typeInfo.MustLookup(expr.Callee).(CallableType)
	if !ok {
		return fmt.Errorf("callee %q is not a callable type", calleeType)
	}
	var args []*FunctionArg
	if method, ok := calleeType.(*MethodType); ok {
		args = method.ArgTypesWithoutSelf()
	} else {
		args = calleeType.CallArgTypes()
	}
	if len(args) != len(expr.Args) {
		return fmt.Errorf("expected %d arguments, got %d for %s", len(args), len(expr.Args), calleeType)
	}
	for i, arg := range expr.Args {
		argType := tc.typeInfo.MustLookup(arg)
		if argType != args[i].Type {
			return fmt.Errorf("expected argument %d to be of type %q, got %q", i, args[i].Type, argType)
		}
	}
	tc.typeInfo.Set(expr, calleeType.CallReturnType())
	return nil
}

func (tc *typeChecker) VisitMemberExpression(expr *ast.MemberExpression, w ast.Walker) error {
	if err := w.WalkMemberExpression(expr); err != nil {
		return fmt.Errorf("failed to walk member expression: %w", err)
	}
	structType_ := tc.typeInfo.MustLookup(expr.Target)
	structType, isType := structType_.(*StructType)
	if !isType {
		return fmt.Errorf("type %q is not a struct type", structType_)
	}
	member, err := structType.FindMember(expr.Field)
	if err != nil {
		return err
	}
	tc.typeInfo.Set(expr, member)
	return nil
}

func (tc *typeChecker) VisitBlockExpression(expr *ast.BlockExpression, w ast.Walker) error {
	if err := w.WalkBlockExpression(expr); err != nil {
		return fmt.Errorf("failed to walk block expression: %w", err)
	}
	blockType := tc.typeInfo.MustLookup(expr.Nodes[len(expr.Nodes)-1])
	tc.typeInfo.Set(expr, blockType)
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	tc.enterScope()
	defer tc.exitScope()
	if err := w.WalkIfExpression(expr); err != nil {
		return fmt.Errorf("failed to walk if expression: %w", err)
	}
	condType := tc.typeInfo.MustLookup(expr.Condition)
	if tc.typeInfo.MustLookup(expr.Condition) != BoolType {
		return fmt.Errorf("the condition of an if expression must be a boolean type, got: %s", condType)
	}
	// Only an if expression with an else branch can have a type other than unit.
	// And currently we don't have else branches.
	tc.typeInfo.Set(expr, UnitType)
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
		fieldType := tc.typeInfo.MustLookup(initField.Value)
		if structField.Type != fieldType {
			return fmt.Errorf("struct init: expected field %q to be of type %q, got %q", initField.Name, structField.Type, fieldType)
		}
	}
	if len(expr.Fields) != len(structType.Fields) {
		return fmt.Errorf("expected %d fields, got %d", len(structType.Fields), len(expr.Fields))
	}
	tc.typeInfo.Set(expr, structType)
	return nil
}

func (tc *typeChecker) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	args := []*FunctionArg{}
	for _, arg := range decl.Args {
		argType, found := tc.typeEnv.lookup(string(arg.Type))
		if !found {
			return fmt.Errorf("type %s not found for argument %s", arg.Type, arg.Name)
		}
		args = append(args, &FunctionArg{Name: arg.Name, Type: argType})
	}
	var returnType Type = UnitType
	if decl.ReturnType != "" {
		ty, found := tc.typeEnv.lookup(string(decl.ReturnType))
		if !found {
			return fmt.Errorf("type %s not found for return type of function %s", decl.ReturnType, decl.Name)
		}
		returnType = ty
	}
	funcType := &FunctionType{
		Name:       decl.Name,
		Args:       args,
		ReturnType: returnType,
	}
	if funcType.Name == "main" {
		if len(funcType.Args) > 0 {
			return fmt.Errorf("main function must not have arguments")
		}
		if _, ok := funcType.ReturnType.(*unitType); !ok {
			return fmt.Errorf("main function must return () (no return value)")
		}
		tc.typeInfo.Main = funcType
	}
	tc.typeInfo.Set(decl, &DeclaredType{Type: funcType})
	if err := tc.typeEnv.declare(string(decl.Name), funcType); err != nil {
		return fmt.Errorf("failed to declare function %s: %w", decl.Name, err)
	}
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.Walker) error {
	if err := tc.VisitFunctionDeclaration(fn.Decl); err != nil {
		return err
	}
	funcDeclType := tc.typeInfo.MustLookup(fn.Decl).(*DeclaredType)
	funcType := funcDeclType.Type.(*FunctionType)
	tc.typeInfo.Set(fn, funcDeclType)
	tc.enterScope()
	defer tc.exitScope()
	for i, arg := range fn.Decl.Args {
		argType := funcType.Args[i].Type
		if err := tc.typeEnv.declare(string(arg.Name), argType); err != nil {
			return fmt.Errorf("failed to declare argument %s: %w", arg.Name, err)
		}
	}
	if err := w.WalkFunctionDefinition(fn); err != nil {
		return fmt.Errorf("failed to walk function definition: %w", err)
	}
	return nil
}

func (tc *typeChecker) VisitTraitDeclaration(trait *ast.TraitDeclaration, w ast.Walker) error {
	// We need to forward declare the trait type so that we can set the `Self` type correctly.
	traitType := &TraitType{Name: trait.Name}
	if err := tc.typeEnv.declare(string(trait.Name), traitType); err != nil {
		return err
	}
	tc.enterScope()
	defer tc.exitScope()
	if err := tc.typeEnv.declare("Self", traitType); err != nil {
		return err
	}
	if err := w.WalkTraitDeclaration(trait); err != nil {
		return err
	}
	for _, methodDecl := range trait.MethodDecls {
		functionType := tc.typeInfo.MustLookupDeclaredType(methodDecl).Type.(*FunctionType)
		methodType := newMethodTypeFromFunctionType(functionType, traitType)
		traitType.Methods = append(traitType.Methods, methodType)
	}
	tc.typeInfo.Set(trait, traitType)
	return nil
}

func (tc *typeChecker) VisitImplDefinition(impl *ast.ImplDefinition, w ast.Walker) error {
	structType_, found := tc.typeEnv.lookup(string(impl.Target))
	if !found {
		return fmt.Errorf("type %q not found for impl definition", impl.Target)
	}
	structType, ok := structType_.(*StructType)
	if !ok {
		return fmt.Errorf("type %q is not a struct type", structType_)
	}
	tc.enterScope()
	defer tc.exitScope()
	if err := tc.typeEnv.declare("Self", structType); err != nil {
		return err
	}
	if err := w.WalkImplDefinition(impl); err != nil {
		return err
	}
	var traitType *TraitType = nil
	unimplementedTraitMethods := map[ast.Ident]*MethodType{}
	if impl.ImplementsTrait() {
		traitType_, found := tc.typeEnv.lookup(string(impl.Trait))
		if !found {
			return fmt.Errorf("trait %q not found for impl definition", impl.Trait)
		}
		traitType, ok = traitType_.(*TraitType)
		if !ok {
			return fmt.Errorf("type %q is not a trait type", traitType_)
		}
		for _, method := range traitType.Methods {
			unimplementedTraitMethods[method.Name] = method
		}
	}
	for _, method := range impl.Methods {
		decl := method.Decl
		if _, err := structType.FindField(decl.Name); err == nil {
			return fmt.Errorf("method name %q already used in struct type %q", decl.Name, structType.Name)
		}
		if _, err := structType.FindMethod(decl.Name); err == nil {
			return fmt.Errorf("method name %q already used in struct type %q", decl.Name, structType.Name)
		}
		typeDecl := tc.typeInfo.MustLookupDeclaredType(method)
		functionType, ok := typeDecl.Type.(*FunctionType)
		if !ok {
			return fmt.Errorf("type is not a function type: %s", typeDecl)
		}
		methodType := newMethodTypeFromFunctionType(functionType, structType)
		if traitType != nil {
			traitMethodType, err := traitType.FindMethod(decl.Name)
			if err != nil {
				return fmt.Errorf("method %q not found in trait %q", decl.Name, traitType.Name)
			}
			if err := traitMethodType.CheckSameSignatureIgnoringReceiverTypes(methodType); err != nil {
				return fmt.Errorf(
					"method %q in impl %q has different signature than in trait %q: %w",
					decl.Name,
					structType.Name,
					traitType.Name,
					err,
				)
			}
			delete(unimplementedTraitMethods, decl.Name)
		}
		structType.Methods = append(structType.Methods, methodType)
		tc.typeInfo.Set(method, &DeclaredType{Type: methodType})
	}
	if traitType != nil && len(unimplementedTraitMethods) > 0 {
		missingTraitMethods := []string{}
		for _, method := range unimplementedTraitMethods {
			missingTraitMethods = append(missingTraitMethods, method.String())
		}
		return fmt.Errorf(
			"impl %q does not implement all methods of trait %q: %s",
			structType.Name,
			traitType.Name,
			strings.Join(missingTraitMethods, ", "),
		)
	}
	tc.typeInfo.Set(impl, &ImplType{ReceiverType: structType})
	return nil
}

func (tc *typeChecker) VisitVariableDefinition(v *ast.VariableDefinition, w ast.Walker) error {
	if err := w.WalkNode(v.Value); err != nil {
		return err
	}
	valueType := tc.typeInfo.MustLookup(v.Value)
	if valueType == UnitType {
		return fmt.Errorf("variable %s must have a non-unit type", v.Name)
	}
	if err := tc.typeEnv.declareVariable(string(v.Name), valueType, v); err != nil {
		return err
	}
	tc.typeInfo.Set(v, UnitType)
	return nil
}

func (tc *typeChecker) VisitAssignmentStatement(s *ast.AssignmentStatement, w ast.Walker) error {
	if err := w.WalkAssignmentStatement(s); err != nil {
		return err
	}
	rhsType := tc.typeInfo.MustLookup(s.Rhs)
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
	tc.typeInfo.Set(s, UnitType)
	return nil
}

func (tc *typeChecker) VisitLoopStatement(s *ast.LoopStatement, w ast.Walker) error {
	tc.typeInfo.Set(s, UnitType)
	tc.enterLoop()
	defer tc.exitLoop()
	return w.WalkLoopStatement(s)
}

func (tc *typeChecker) VisitContinueStatement(s *ast.ContinueStatement) error {
	if tc.loopDepth == 0 {
		return fmt.Errorf("continue statement outside of a loop")
	}
	tc.typeInfo.Set(s, UnitType)
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return fmt.Errorf("break statement outside of a loop")
	}
	tc.typeInfo.Set(s, UnitType)
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
	structType := &StructType{Name: d.Name, Fields: fields}
	if err := tc.typeEnv.declare(string(d.Name), structType); err != nil {
		return err
	}
	tc.typeInfo.Set(d, &DeclaredType{Type: structType})
	return nil
}

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.Walker) error {
	tc.typeInfo.Set(module, UnitType)
	return w.WalkModule(module)
}

func (tc *typeChecker) check(node ast.Node, w ast.Walker) (Type, error) {
	if err := w.WalkNode(node); err != nil {
		return nil, err
	}
	nodeType := tc.typeInfo.MustLookup(node)
	if tc.typeInfo.Main == nil {
		return nil, fmt.Errorf("main function not found")
	}
	return nodeType, nil
}

func TypeCheck(node ast.Node) (Type, *TypeInfo, error) {
	defaultTypeEnv := newTypeEnvironment(nil)
	// Declare builtin types.
	if err := defaultTypeEnv.declare("Str", StrType); err != nil {
		panic(fmt.Errorf("Failed to declare Str type: %w", err))
	}
	if err := defaultTypeEnv.declare("Int", Int64Type); err != nil {
		panic(fmt.Errorf("Failed to declare Int type: %w", err))
	}
	if err := defaultTypeEnv.declare("()", UnitType); err != nil {
		panic(fmt.Errorf("Failed to declare UnitType type: %w", err))
	}
	if err := defaultTypeEnv.declare("print", &FunctionType{
		Name:       "print",
		Args:       []*FunctionArg{&FunctionArg{Name: "s", Type: StrType}},
		ReturnType: UnitType,
	}); err != nil {
		panic(fmt.Errorf("Failed to declare print function: %w", err))
	}
	if err := defaultTypeEnv.declare("print_int", &FunctionType{
		Name:       "print_int",
		Args:       []*FunctionArg{&FunctionArg{Name: "i", Type: Int64Type}},
		ReturnType: UnitType,
	}); err != nil {
		panic(fmt.Errorf("Failed to declare print_int function: %w", err))
	}
	tc := &typeChecker{
		DefaultVisitor: ast.DefaultVisitor{},
		typeInfo: &TypeInfo{
			types:        make(map[ast.NodeId]Type),
			typeBindings: make(map[ast.ReferenceExpression]NamedType),
		},
		typeEnv: defaultTypeEnv,
	}
	walker := &ast.DefaultWalker{Visitor: tc}
	res, err := tc.check(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, tc.typeInfo, nil
}
