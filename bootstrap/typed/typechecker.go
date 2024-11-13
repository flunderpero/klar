package typed

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/pkg/errors"
)

type Type interface {
	String() string
}

var builtInSpan = token.Span{File: new(string), Src: &[]byte{}, Start: 0, End: 0}
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

func (ty strType) String() string {
	return "StrType"
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

func (ty boolType) String() string {
	return "BoolType"
}

type int64Type struct {
	traits []*TraitType
}

func (ty *int64Type) Traits() *[]*TraitType {
	return &ty.traits
}

func (ty int64Type) String() string {
	return "Int64Type"
}

type unitType struct{}

func (ty unitType) String() string {
	return "UnitType"
}

type StructField struct {
	Name ast.Ident
	Type Type
}

func (f StructField) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Type, 1))
}

type DeclaredType struct {
	Type Type
}

func (ty *DeclaredType) TypeName() string {
	return ty.Type.String()
}

func (ty DeclaredType) String() string {
	return fmt.Sprintf("DeclaredType\n%s", base.Indent(ty.Type, 1))
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

func (ty StructType) String() string {
	return fmt.Sprintf(
		"StructType\n%s%s%s", base.Indent(ty.Name, 1), base.IndentSlice(ty.Fields, 1), base.IndentSlice(ty.Methods, 1))
}

func (ty *StructType) FindFieldIndex(name ast.Ident, span token.Span) (int, error) {
	fieldIndex := slices.IndexFunc(ty.Fields, func(field StructField) bool { return field.Name == name })
	if fieldIndex < 0 {
		return -1, errors.Errorf("%s: field %q not found in struct type %q", span, name, ty)
	}
	return fieldIndex, nil
}

func (ty *StructType) FindField(name ast.Ident, span token.Span) (*StructField, error) {
	fieldIndex, err := ty.FindFieldIndex(name, span)
	if err != nil {
		return nil, err
	}
	return &ty.Fields[fieldIndex], nil
}

func (ty *StructType) FindMethod(name ast.Ident, span token.Span) (*MethodType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method, nil
		}
	}
	return nil, errors.Errorf("%s: method %q not found in struct type %q", span, name, ty)
}

func (ty *StructType) FindMember(name ast.Ident, span token.Span) (Type, error) {
	field, err := ty.FindField(name, span)
	if err == nil {
		return field.Type, nil
	}
	method, err := ty.FindMethod(name, span)
	if err == nil {
		return method, nil
	}
	return nil, errors.Errorf("%s: member %q not found in struct type %q", span, name, ty.Name)
}

type TraitType struct {
	Name    ast.TypeIdent
	Methods []*MethodType
}

func (ty TraitType) String() string {
	return fmt.Sprintf("TraitType\n%s%s)", base.Indent(ty.Name, 1), base.IndentSlice(ty.Methods, 1))
}

func (ty *TraitType) TypeName() string {
	return string(ty.Name)
}

func (ty *TraitType) FindMethod(name ast.Ident, span token.Span) (*MethodType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method, nil
		}
	}
	return nil, errors.Errorf("%s: method %q not found in trait type %q", span, name, ty)
}

type ImplType struct {
	ReceiverType Type
}

func (ty ImplType) String() string {
	return fmt.Sprintf("ImplType\n%s", base.Indent(ty.ReceiverType, 1))
}

type FunctionArg struct {
	Name ast.Ident
	Type Type
}

func (arg FunctionArg) String() string {
	return fmt.Sprintf("%s\n%s", arg.Name, base.Indent(arg.Type, 1))
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

func (ty *MethodType) CheckSameSignatureIgnoringReceiverTypes(other *MethodType, span token.Span) error {
	match := func(thisType Type, otherType Type) bool {
		if otherType == other.ReceiverType {
			return thisType == ty.ReceiverType
		}
		return thisType == otherType
	}
	if ty.Name != other.Name {
		return errors.Errorf("%s: method names do not match: %s != %s", span, ty.Name, other.Name)
	}
	if len(ty.Args) != len(other.Args) {
		return errors.Errorf("%s: argument count does not match: %d != %d", span, len(ty.Args), len(other.Args))
	}
	if !match(ty.ReturnType, other.ReturnType) {
		return errors.Errorf("%s: return types do not match: %s != %s", span, ty.ReturnType, other.ReturnType)
	}
	for i, arg := range ty.Args {
		otherArg := other.Args[i]
		if !match(arg.Type, otherArg.Type) {
			return errors.Errorf("%s: argument types do not match: %s != %s", span, arg.Type, otherArg.Type)
		}
	}
	return nil
}

func (ty *MethodType) IsStatic() bool {
	return len(ty.Args) == 0 || ty.Args[0].Name != "self"
}

func (ty MethodType) String() string {
	typeToString := func(t Type) string {
		if t == ty.ReceiverType.(Type) {
			return "Self"
		}
		return t.String()
	}
	argToString := func(arg *FunctionArg) string {
		return fmt.Sprintf("%s\n%s", arg.Name, base.IndentString(typeToString(arg.Type), 1))
	}
	args := base.Map(ty.Args, argToString)
	return fmt.Sprintf(
		"MethodType\n%s%s\n%s",
		base.Indent(ty.Name, 1),
		base.IndentStringSlice(args, 1),
		base.IndentString(typeToString(ty.ReturnType), 1))
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

func (ty FunctionType) String() string {
	return fmt.Sprintf(
		"FunctionType\n%s%s\n%s", base.Indent(ty.Name, 1), base.IndentSlice(ty.Args, 1), base.Indent(ty.ReturnType, 1))
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

func (te *typeEnvironment) declare(name string, ty Type, span token.Span) error {
	if _, found := te.types[name]; found {
		return errors.Errorf("%s: type %s already declared", span, name)
	}
	te.types[name] = ty
	return nil
}

func (te *typeEnvironment) declareVariable(name string, ty Type, def *ast.VariableDefinition) error {
	if err := te.declare(name, ty, def.Span()); err != nil {
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
		return nil, errors.Errorf("%s: type not found for node #%d: %s", node.Span(), node.Id(), node)
	}
	return ty, nil
}

func (m *TypeInfo) LookupType(node ast.Node, ty Type) (Type, error) {
	got, err := m.Lookup(node)
	if err != nil {
		return nil, err
	}
	if got != ty {
		return nil, errors.Errorf("%s: expected type %s, got %s", node.Span(), ty, got)
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
		panic(errors.Errorf("%s: expected type declaration, got %s", node.Span(), ty))
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
	case *ast.TypeExpression:
		switch ty := expr.Type.(type) {
		case *ast.SimpleType:
			refStr = string(ty.Name)
		default:
			return errors.Errorf("%s: unexpected type expression type: %T", expr.Span(), expr)
		}
	case *ast.IdentExpression:
		refStr = string(expr.Ident)
	default:
		panic(fmt.Sprintf("unexpected reference expression type: %T", expr))
	}
	ty, found := tc.typeEnv.lookup(refStr)
	if !found {
		return errors.Errorf("%s: type not found for identifier %s", expr.Span(), refStr)
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
			return errors.Errorf("%s: lhs of add expression must be of type Int64Type, got %s", expr.Span(), lhs)
		}
		if rhs != Int64Type {
			return errors.Errorf("%s: rhs of add expression must be of type Int64Type, got %s", expr.Span(), rhs)
		}
		tc.typeInfo.Set(expr, Int64Type)
	case ast.OpEquality:
		// For now, we only support equality of numbers.
		if lhs != Int64Type {
			return errors.Errorf("%s: lhs of equality expression must be of type Int64Type, got %s", expr.Span(), lhs)
		}
		if rhs != Int64Type {
			return errors.Errorf("%s: rhs of equality expression must be of type Int64Type, got %s", expr.Span(), rhs)
		}
		tc.typeInfo.Set(expr, BoolType)
	default:
		return errors.Errorf("%s: unsupported binary operator: %s", expr.Span(), expr.Op)
	}
	return nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	calleeType, ok := tc.typeInfo.MustLookup(expr.Callee).(CallableType)
	if !ok {
		return errors.Errorf("%s: callee %q is not a callable type", expr.Span(), calleeType)
	}
	var args []*FunctionArg
	if method, ok := calleeType.(*MethodType); ok {
		args = method.ArgTypesWithoutSelf()
	} else {
		args = calleeType.CallArgTypes()
	}
	if len(args) != len(expr.Args) {
		return errors.Errorf("%s: expected %d arguments, got %d for %s", expr.Span(), len(args), len(expr.Args), calleeType)
	}
	for i, arg := range expr.Args {
		argType := tc.typeInfo.MustLookup(arg)
		if argType != args[i].Type {
			return errors.Errorf("%s: expected argument %d to be of type %q, got %q", arg.Span(), i, args[i].Type, argType)
		}
	}
	tc.typeInfo.Set(expr, calleeType.CallReturnType())
	return nil
}

func (tc *typeChecker) VisitMemberExpression(expr *ast.MemberExpression, w ast.Walker) error {
	if err := w.WalkMemberExpression(expr); err != nil {
		return err
	}
	structType_ := tc.typeInfo.MustLookup(expr.Target)
	structType, isType := structType_.(*StructType)
	if !isType {
		return errors.Errorf("%s: type %q is not a struct type", expr.Span(), structType_)
	}
	member, err := structType.FindMember(expr.Field, expr.Span())
	if err != nil {
		return err
	}
	tc.typeInfo.Set(expr, member)
	return nil
}

func (tc *typeChecker) VisitBlockExpression(expr *ast.BlockExpression, w ast.Walker) error {
	if err := w.WalkBlockExpression(expr); err != nil {
		return err
	}
	blockType := tc.typeInfo.MustLookup(expr.Nodes[len(expr.Nodes)-1])
	tc.typeInfo.Set(expr, blockType)
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	tc.enterScope()
	defer tc.exitScope()
	if err := w.WalkIfExpression(expr); err != nil {
		return err
	}
	condType := tc.typeInfo.MustLookup(expr.Condition)
	if tc.typeInfo.MustLookup(expr.Condition) != BoolType {
		return errors.Errorf("%s: the condition of an if expression must be a boolean type, got: %s", expr.Condition.Span(), condType)
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
		return errors.Errorf("%s: type %q not found for struct init expression", expr.Span(), expr.TypeIdent)
	}
	structType, isType := structType_.(*StructType)
	if !isType {
		return errors.Errorf("%s: type %q is not a struct type", expr.Span(), expr.TypeIdent)
	}
	for _, initField := range expr.Fields {
		structField, err := structType.FindField(initField.Name, initField.Span)
		if err != nil {
			return err
		}
		fieldType := tc.typeInfo.MustLookup(initField.Value)
		if structField.Type != fieldType {
			return errors.Errorf(
				"%s: struct init: expected field %q to be of type %q, got %q",
				initField.Span,
				initField.Name,
				structField.Type,
				fieldType,
			)
		}
	}
	if len(expr.Fields) != len(structType.Fields) {
		return errors.Errorf("%s: expected %d fields, got %d", expr.Span(), len(structType.Fields), len(expr.Fields))
	}
	tc.typeInfo.Set(expr, structType)
	return nil
}

func (tc *typeChecker) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	args := []*FunctionArg{}
	for _, arg := range decl.Args {
		argType, found := tc.typeEnv.lookup(arg.Type.TypeName())
		if !found {
			return errors.Errorf("%s: type %s not found for argument %s", arg.Span, arg.Type, arg.Name)
		}
		args = append(args, &FunctionArg{Name: arg.Name, Type: argType})
	}
	returnType, found := tc.typeEnv.lookup(decl.ReturnType.TypeName())
	if !found {
		return errors.Errorf("%s: type %s not found for return type of function %s", decl.Span(), decl.ReturnType, decl.Name)
	}
	funcType := &FunctionType{
		Name:       decl.Name,
		Args:       args,
		ReturnType: returnType,
	}
	if funcType.Name == "main" {
		if len(funcType.Args) > 0 {
			return errors.Errorf("%s: main function must not have arguments", decl.Span())
		}
		if _, ok := funcType.ReturnType.(*unitType); !ok {
			return errors.Errorf("%s: main function must return () (no return value)", decl.Span())
		}
		tc.typeInfo.Main = funcType
	}
	tc.typeInfo.Set(decl, &DeclaredType{Type: funcType})
	if err := tc.typeEnv.declare(string(decl.Name), funcType, decl.Span()); err != nil {
		return err
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
		if err := tc.typeEnv.declare(string(arg.Name), argType, arg.Span); err != nil {
			return err
		}
	}
	if err := w.WalkFunctionDefinition(fn); err != nil {
		return err
	}
	return nil
}

func (tc *typeChecker) VisitTraitDeclaration(trait *ast.TraitDeclaration, w ast.Walker) error {
	// We need to forward declare the trait type so that we can set the `Self` type correctly.
	traitType := &TraitType{Name: trait.Name}
	if err := tc.typeEnv.declare(string(trait.Name), traitType, trait.Span()); err != nil {
		return err
	}
	tc.enterScope()
	defer tc.exitScope()
	if err := tc.typeEnv.declare("Self", traitType, trait.Span()); err != nil {
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
		return errors.Errorf("%s: type %q not found for impl definition", impl.Span(), impl.Target)
	}
	structType, ok := structType_.(*StructType)
	if !ok {
		return errors.Errorf("%s: type %q is not a struct type", impl.Span(), structType_)
	}
	tc.enterScope()
	defer tc.exitScope()
	if err := tc.typeEnv.declare("Self", structType, impl.Span()); err != nil {
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
			return errors.Errorf("%s: trait %q not found for impl definition", impl.Span(), impl.Trait)
		}
		traitType, ok = traitType_.(*TraitType)
		if !ok {
			return errors.Errorf("%s: type %q is not a trait type", impl.Span(), traitType_)
		}
		for _, method := range traitType.Methods {
			unimplementedTraitMethods[method.Name] = method
		}
	}
	for _, method := range impl.Methods {
		decl := method.Decl
		if _, err := structType.FindField(decl.Name, decl.Span()); err == nil {
			return errors.Errorf("%s: method name %q already used in struct type %q", decl.Span(), decl.Name, structType.Name)
		}
		if _, err := structType.FindMethod(decl.Name, decl.Span()); err == nil {
			return errors.Errorf("%s: method name %q already used in struct type %q", decl.Span(), decl.Name, structType.Name)
		}
		typeDecl := tc.typeInfo.MustLookupDeclaredType(method)
		functionType, ok := typeDecl.Type.(*FunctionType)
		if !ok {
			return errors.Errorf("%s: type is not a function type: %s", method.Span(), typeDecl)
		}
		methodType := newMethodTypeFromFunctionType(functionType, structType)
		if traitType != nil {
			traitMethodType, err := traitType.FindMethod(decl.Name, decl.Span())
			if err != nil {
				return errors.Errorf("%s: method %q not found in trait %q", decl.Span(), decl.Name, traitType.Name)
			}
			if err := traitMethodType.CheckSameSignatureIgnoringReceiverTypes(methodType, decl.Span()); err != nil {
				return errors.Wrapf(
					err,
					"%s: method %q in impl %q has different signature than in trait: %s",
					decl.Span(),
					decl.Name,
					structType.Name,
					traitType,
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
		return errors.Errorf(
			"%s: impl %q does not implement all methods of trait %q: %s",
			impl.Span(),
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
		return errors.Errorf("%s: variable %s must have a non-unit type", v.Span(), v.Name)
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
		return errors.Errorf("%s: unknown variable %q", s.Span(), s.Variable.Ident)
	}
	if !varDefinition.Mutable {
		return errors.Errorf("%s: variable %q is not mutable", s.Span(), s.Variable.Ident)
	}
	if s.IsAssignToMember() {
		structType, ok := varType.(*StructType)
		if !ok {
			return errors.Errorf("%s: variable %q is not a struct type", s.Span(), s.Variable.Ident)
		}
		field, err := structType.FindField(*s.Field, s.Span())
		if err != nil {
			return err
		}
		varType = field.Type
	}
	if varType != rhsType {
		return errors.Errorf("%s: lhs and rhs of assignment statement must have the same type, got %s and %s", s.Span(), varType, rhsType)
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
		return errors.Errorf("%s: continue statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, UnitType)
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: break statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, UnitType)
	return nil
}

func (tc *typeChecker) VisitStructTypeDeclaration(d *ast.StructTypeDeclaration) error {
	fields := []StructField{}
	for _, field := range d.Fields {
		fieldType, found := tc.typeEnv.lookup(field.Type.TypeName())
		if !found {
			return errors.Errorf("%s: type %q not found for field %q", field.Span, field.Type, field.Name)
		}
		fields = append(fields, StructField{Name: field.Name, Type: fieldType})
	}
	structType := &StructType{Name: d.Name, Fields: fields}
	if err := tc.typeEnv.declare(string(d.Name), structType, d.Span()); err != nil {
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
		return nil, errors.Errorf("main function not found")
	}
	return nodeType, nil
}

func TypeCheck(node ast.Node) (Type, *TypeInfo, error) {
	defaultTypeEnv := newTypeEnvironment(nil)
	// Declare builtin types.
	if err := defaultTypeEnv.declare("Str", StrType, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare Str type"))
	}
	if err := defaultTypeEnv.declare("Int", Int64Type, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare Int type"))
	}
	if err := defaultTypeEnv.declare("()", UnitType, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare UnitType type"))
	}
	if err := defaultTypeEnv.declare("print", &FunctionType{
		Name:       "print",
		Args:       []*FunctionArg{&FunctionArg{Name: "s", Type: StrType}},
		ReturnType: UnitType,
	}, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare print function"))
	}
	if err := defaultTypeEnv.declare("print_int", &FunctionType{
		Name:       "print_int",
		Args:       []*FunctionArg{&FunctionArg{Name: "i", Type: Int64Type}},
		ReturnType: UnitType,
	}, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare print_int function"))
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
