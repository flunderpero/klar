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

type TypeId int

func (id TypeId) String() string {
	return fmt.Sprintf("type%d", id)
}

func (id TypeId) IdMarker() {}

type isId interface {
	String() string
	IdMarker()
}

type Type interface {
	String() string
	Id() TypeId
	IsAssignableFrom(other Type) bool
}

var BuiltInPrintFunction = &FunctionType{
	BaseType:   BaseType{TypeId(100)},
	ArgTypes:   []Type{StrType},
	ReturnType: NoneType,
}
var BuiltInPrintIntFunction = &FunctionType{
	BaseType:   BaseType{TypeId(101)},
	ArgTypes:   []Type{Int64Type},
	ReturnType: NoneType,
}
var BuiltInUnsafeMallocFunction = &FunctionType{
	BaseType:   BaseType{TypeId(102)},
	ArgTypes:   []Type{Int64Type},
	ReturnType: Int64Type,
}

func IsBuiltInFunction(callable CallableType) bool {
	id := callable.Id()
	return id == BuiltInPrintFunction.Id() || id == BuiltInPrintIntFunction.Id() || id == BuiltInUnsafeMallocFunction.Id()
}

var builtInSpan = token.Span{File: new(string), Src: &[]byte{}, Start: 0, End: 0}
var StrType = &strType{BaseType: BaseType{1}}
var BoolType = &boolType{BaseType: BaseType{2}}
var Int64Type = &int64Type{BaseType: BaseType{3}}
var NoneType = &noneType{BaseType: BaseType{4}}

type CallableType interface {
	GenericType
	CallArgTypes() []Type
	CallReturnType() Type
}

type TypeWithTraits interface {
	Type
	Traits() []*TraitType
}

type BaseType struct {
	id TypeId
}

func NewBaseType(id TypeId) BaseType {
	return BaseType{id}
}

func (ty BaseType) Id() TypeId {
	return ty.id
}

func (ty BaseType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

type strType struct {
	BaseType
	traits []*TraitType
}

func (ty strType) String() string {
	return "StrType"
}

func (ty *strType) Traits() []*TraitType {
	return ty.traits
}

type boolType struct {
	BaseType
	traits []*TraitType
}

func (ty *boolType) Traits() []*TraitType {
	return ty.traits
}

func (ty boolType) String() string {
	return "BoolType"
}

type int64Type struct {
	BaseType
	traits []*TraitType
}

func (ty *int64Type) Traits() []*TraitType {
	return ty.traits
}

func (ty int64Type) String() string {
	return "Int64Type"
}

type noneType struct {
	BaseType
}

func (ty noneType) String() string {
	return "NoneType"
}

type TypeAndName[T Type] struct {
	Name ast.Ident
	Type T
}

func (f TypeAndName[T]) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Type, 1))
}

type GenericType interface {
	Type
	TypeParams() []TypeParam
}

type TypeParam struct {
	BaseType
	GenericType GenericType
	Name        ast.Ident
	Index       int
}

func (ty TypeParam) String() string {
	return fmt.Sprintf("TypeParam %s %s[%d]", ty.Name, ty.GenericType.Id(), ty.Index)
}

func (t TypeParam) Equal(other TypeParam) bool {
	return t.GenericType.Id() == other.GenericType.Id() && t.Index == other.Index
}

func (t TypeParam) IsAssignableFrom(other Type) bool {
	if t.Id() == other.Id() {
		return true
	}
	if other, ok := other.(TypeParam); ok {
		return t.Equal(other)
	}
	return false
}

func typeParamsString(params []TypeParam) string {
	s := ""
	if len(params) > 0 {
		s = fmt.Sprintf("\n(TypeParams)%s", base.IndentSlice(params, 1))
	}
	return s
}

type DeclaredType struct {
	Type Type
}

func (ty DeclaredType) String() string {
	return fmt.Sprintf("DeclaredType\n%s", base.Indent(ty.Type, 1))
}

func (ty DeclaredType) Id() TypeId {
	return ty.Type.Id()
}

func (ty DeclaredType) IsAssignableFrom(other_ Type) bool {
	other := other_
	if declaredType, isDeclaredType := other_.(*DeclaredType); isDeclaredType {
		other = declaredType.Type
	}
	return ty.Type.IsAssignableFrom(other)
}

type StructType struct {
	BaseType
	Fields  []TypeAndName[Type]
	Methods []TypeAndName[MethodType]
	traits  []*TraitType
}

func (ty *StructType) Traits() []*TraitType {
	return ty.traits
}

func (ty StructType) String() string {
	return fmt.Sprintf("StructType\n%s%s", base.IndentSlice(ty.Fields, 1), base.IndentSlice(ty.Methods, 1))
}

func (ty StructType) FindFieldIndex(name ast.Ident, span token.Span) (int, bool) {
	fieldIndex := slices.IndexFunc(ty.Fields, func(field TypeAndName[Type]) bool { return field.Name == name })
	if fieldIndex < 0 {
		return -1, false
	}
	return fieldIndex, true
}

func (ty StructType) FindField(name ast.Ident, span token.Span) (*TypeAndName[Type], bool) {
	fieldIndex, found := ty.FindFieldIndex(name, span)
	if !found {
		return nil, false
	}
	return &ty.Fields[fieldIndex], true
}

func (ty StructType) FindMethod(name ast.Ident, span token.Span) (*MethodType, bool) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return &method.Type, true
		}
	}
	return nil, false
}

func (ty StructType) FindMember(name ast.Ident, span token.Span) (Type, bool) {
	field, found := ty.FindField(name, span)
	if found {
		return field.Type, true
	}
	method, found := ty.FindMethod(name, span)
	if found {
		return method, true
	}
	return nil, false
}

type TraitType struct {
	BaseType
	Methods []TypeAndName[MethodType]
}

func (ty TraitType) String() string {
	return fmt.Sprintf("TraitType%s", base.IndentSlice(ty.Methods, 1))
}

func (ty *TraitType) FindMethod(name ast.Ident, span token.Span) (*MethodType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return &method.Type, nil
		}
	}
	return nil, errors.Errorf("%s: method %q not found in trait type %q", span, name, ty)
}

type ImplType struct {
	BaseType
	ReceiverType Type
}

func (ty ImplType) String() string {
	return fmt.Sprintf("ImplType\n%s", base.Indent(ty.ReceiverType, 1))
}

type MethodType struct {
	BaseType
	typeParams   []TypeParam
	ArgTypes     []Type
	ReturnType   Type
	ReceiverType Type
	IsStatic     bool
}

func (ty MethodType) CallArgTypes() []Type {
	return ty.ArgTypes
}

func (ty MethodType) CallReturnType() Type {
	return ty.ReturnType
}

func (ty MethodType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty MethodType) CheckSameSignatureIgnoringReceiverTypes(other *MethodType, span token.Span) error {
	match := func(thisType Type, otherType Type) bool {
		if otherType == other.ReceiverType {
			return thisType == ty.ReceiverType
		}
		return thisType == otherType
	}
	if len(ty.ArgTypes) != len(other.ArgTypes) {
		return errors.Errorf("%s: argument count does not match: %d != %d", span, len(ty.ArgTypes), len(other.ArgTypes))
	}
	if !match(ty.ReturnType, other.ReturnType) {
		return errors.Errorf("%s: return types do not match: %s != %s", span, ty.ReturnType, other.ReturnType)
	}
	for i, argType := range ty.ArgTypes {
		otherArgType := other.ArgTypes[i]
		if !match(argType, otherArgType) {
			return errors.Errorf("%s: argument types do not match: %s != %s", span, argType, otherArgType)
		}
	}
	return nil
}

func (ty MethodType) String() string {
	typeToString := func(t Type) string {
		if t == ty.ReceiverType {
			return "Self"
		}
		return t.String()
	}
	args := base.Map(ty.ArgTypes, typeToString)
	return fmt.Sprintf(
		"MethodType%s%s\n%s",
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentStringSlice(args, 1),
		base.IndentString(typeToString(ty.ReturnType), 1))
}

func (ty MethodType) ArgTypesWithoutSelf() []Type {
	if ty.IsStatic || len(ty.ArgTypes) == 0 {
		return ty.ArgTypes
	}
	return ty.ArgTypes[1:]
}

func (ty MethodType) IsAssignableFrom(other Type) bool {
	if other.Id() == ty.Id() {
		return true
	}
	if other, ok := other.(MethodType); ok {
		if len(ty.ArgTypes) != len(other.ArgTypes) {
			return false
		}
		for i, argType := range ty.ArgTypes {
			if !argType.IsAssignableFrom(other.ArgTypes[i]) {
				return false
			}
		}
		return ty.ReturnType.IsAssignableFrom(other.ReturnType)
	}
	return false
}

type FunctionType struct {
	BaseType
	typeParams []TypeParam
	ArgTypes   []Type
	ReturnType Type
}

func NewFunctionType(id TypeId, typeParams []TypeParam, argTypes []Type, returnType Type) *FunctionType {
	return &FunctionType{BaseType: BaseType{id}, typeParams: typeParams, ArgTypes: argTypes, ReturnType: returnType}
}

func (ty FunctionType) String() string {
	return fmt.Sprintf(
		"FunctionType%s%s\n%s",
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentSlice(ty.ArgTypes, 1), base.Indent(ty.ReturnType, 1))
}

func (ty FunctionType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty FunctionType) CallArgTypes() []Type {
	return ty.ArgTypes
}

func (ty FunctionType) IsAssignableFrom(other Type) bool {
	if other.Id() == ty.Id() {
		return true
	}
	if other, ok := other.(CallableType); ok {
		otherArgTypes := other.CallArgTypes()
		if len(ty.ArgTypes) != len(otherArgTypes) {
			return false
		}
		for i, argType := range ty.ArgTypes {
			if !argType.IsAssignableFrom(otherArgTypes[i]) {
				return false
			}
		}
		return ty.ReturnType.IsAssignableFrom(other.CallReturnType())
	}
	return false
}

func (ty FunctionType) CallReturnType() Type {
	return ty.ReturnType
}

type Call struct {
	Callee     CallableType
	ArgTypes   []Type
	ReturnType Type
	TypeArgs   []Type
}

type genericScope struct {
	typeParams map[string]*TypeParam
	parent     *genericScope
}

func newGenericScope(parent *genericScope) *genericScope {
	return &genericScope{
		typeParams: make(map[string]*TypeParam),
		parent:     parent,
	}
}

func (self *genericScope) lookupTypeParam(name string) (*TypeParam, bool) {
	ty, found := self.typeParams[name]
	if !found && self.parent != nil {
		return self.parent.lookupTypeParam(name)
	}
	return ty, found
}

func (self *genericScope) declareTypeParam(name string, param *TypeParam, span token.Span) error {
	if _, found := self.typeParams[name]; found {
		return errors.Errorf("%s: type parameter %q already declared", span, name)
	}
	self.typeParams[name] = param
	return nil
}

type variableInfo struct {
	type_         Type
	isFunctionArg bool
	mutable       bool
	span          token.Span
}

type typeScope struct {
	types     map[string]Type
	variables map[string]variableInfo
	parent    *typeScope
}

func newTypeScope(parent *typeScope) *typeScope {
	return &typeScope{
		types:     make(map[string]Type),
		variables: make(map[string]variableInfo),
		parent:    parent,
	}
}

func (te *typeScope) lookupType(name string) (Type, bool) {
	ty, found := te.types[name]
	if !found && te.parent != nil {
		return te.parent.lookupType(name)
	}
	return ty, found
}

func (te *typeScope) lookupVariable(name ast.Ident) (Type, *variableInfo, bool) {
	ty, found := te.lookupType(string(name))
	if !found {
		return nil, nil, false
	}
	info, found := te.variables[string(name)]
	if !found && te.parent != nil {
		return te.parent.lookupVariable(name)
	}
	return ty, &info, found
}

func (te *typeScope) declareType(name string, ty Type, span token.Span) error {
	if _, found := te.types[name]; found {
		return errors.Errorf("%s: type %q already declared", span, name)
	}
	te.types[name] = ty
	return nil
}

func (te *typeScope) declareVariable(name string, info variableInfo) error {
	if err := te.declareType(name, info.type_, info.span); err != nil {
		return err
	}
	te.variables[name] = info
	return nil
}

type Symbol struct {
	Name  string
	Scope *SymbolScope
}

type SymbolScope struct {
	Parent   *SymbolScope
	Children []*SymbolScope
	Node     ast.Node
	Symbols  map[string]*Symbol
}

func newSymbolScope(node ast.Node, parent *SymbolScope) *SymbolScope {
	scope := &SymbolScope{
		Parent:  parent,
		Node:    node,
		Symbols: make(map[string]*Symbol),
	}
	if parent != nil {
		parent.Children = append(parent.Children, scope)
	}
	return scope
}

type TypeInfo struct {
	types   map[ast.NodeId]Type
	symbols map[string]*Symbol
	// The type an `ReferenceExpression` points to if it does not refer to a variable.
	typeBindings map[ast.ReferenceExpression]Type
	// Record the concrete type of a function at call time.
	calls map[*ast.CallExpression]Call
	Main  *FunctionType
}

func (m *TypeInfo) LookupTypeBinding(expr ast.ReferenceExpression) (Type, bool) {
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

func (m *TypeInfo) MustLookupSymbol(id isId) *Symbol {
	keyString := id.String()
	symbol, found := m.symbols[keyString]
	if !found {
		panic(fmt.Sprintf("symbol not found for key %q", keyString))
	}
	return symbol
}

func (m *TypeInfo) DeclareSymbol(id isId, symbol *Symbol) {
	m.symbols[id.String()] = symbol
}

func (m *TypeInfo) recordCall(expr *ast.CallExpression, call Call) {
	m.calls[expr] = call
}

func (m *TypeInfo) MustLookupCall(expr *ast.CallExpression) *Call {
	call, found := m.calls[expr]
	if !found {
		panic(fmt.Sprintf("call not found for expression %q", expr))
	}
	return &call
}

type checkingMode int

const (
	defaultMode           checkingMode = 0
	insideTraitOrImplMode checkingMode = 1
)

type typeChecker struct {
	ast.DefaultVisitor
	typeInfo      *TypeInfo
	typeScope     *typeScope
	symbolScope   *SymbolScope
	genericScope  *genericScope
	loopDepth     int
	nextTypeId    int
	checkingMode  checkingMode
	functionTypes map[string]FunctionType
}

func (tc *typeChecker) newType() BaseType {
	tc.nextTypeId += 1
	return BaseType{TypeId(tc.nextTypeId)}
}

func (tc *typeChecker) enterScope(node ast.Node) {
	tc.typeScope = newTypeScope(tc.typeScope)
	scope := newSymbolScope(node, tc.symbolScope)
	tc.symbolScope = scope
}

func (tc *typeChecker) exitScope() {
	tc.typeScope = tc.typeScope.parent
	tc.symbolScope = tc.symbolScope.Parent
}

func (tc *typeChecker) enterGenericScope() {
	tc.genericScope = newGenericScope(tc.genericScope)
}

func (tc *typeChecker) exitGenericScope() {
	tc.genericScope = tc.genericScope.parent
}

func (tc *typeChecker) enterLoop() {
	tc.loopDepth += 1
}

func (tc *typeChecker) exitLoop() {
	tc.loopDepth -= 1
}

func (tc *typeChecker) enterCheckingMode(mode checkingMode) {
	if tc.checkingMode != defaultMode {
		panic(fmt.Sprintf("unexpected checking mode %d while trying to enter mode %d", tc.checkingMode, mode))
	}
	tc.checkingMode = mode
}

func (tc *typeChecker) exitCheckingMode() {
	if tc.checkingMode == defaultMode {
		panic("unexpected checking mode 0 while trying to exit mode")
	}
	tc.checkingMode = defaultMode
}

func (tc *typeChecker) declareSymbol(key isId, name string) {
	keyString := key.String()
	symbol := &Symbol{Name: name, Scope: tc.symbolScope}
	if _, found := tc.symbolScope.Symbols[keyString]; found {
		panic(fmt.Sprintf("symbol already declared: %q", symbol.Name))
	}
	tc.symbolScope.Symbols[keyString] = symbol
	tc.typeInfo.DeclareSymbol(key, symbol)
}

func (tc *typeChecker) lookupTypeOfNode(node ast.Type) (Type, error) {
	switch node := node.(type) {
	case *ast.FunctionType:
		if res, found := tc.functionTypes[node.TypeName()]; found {
			return &res, nil
		}
		argTypes := make([]Type, len(node.ArgTypes))
		for i, arg := range node.ArgTypes {
			argType, err := tc.lookupTypeOfNode(arg)
			if err != nil {
				return nil, err
			}
			argTypes[i] = argType
		}
		returnType, err := tc.lookupTypeOfNode(node.ReturnType)
		if err != nil {
			return nil, err
		}
		res := FunctionType{BaseType: tc.newType(), ArgTypes: argTypes, ReturnType: returnType}
		tc.functionTypes[node.TypeName()] = res
		return &res, nil
	case *ast.TypeParam:
		res, found := tc.genericScope.lookupTypeParam(node.TypeName())
		if !found {
			return nil, errors.Errorf("undefined type parameter: %s", node.TypeName())
		}
		return res, nil
	default:
		if res, found := tc.typeScope.lookupType(node.TypeName()); found {
			return res, nil
		}
		if res, found := tc.genericScope.lookupTypeParam(node.TypeName()); found {
			return res, nil
		}
		return nil, errors.Errorf("undefined type: %s", node.TypeName())
	}
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
	var ty Type
	switch expr := expr.(type) {
	case *ast.TypeExpression:
		switch expr := expr.Type.(type) {
		case *ast.SimpleType:
			simpleTy, err := tc.lookupTypeOfNode(expr)
			if err != nil {
				return err
			}
			ty = simpleTy
		default:
			return errors.Errorf("%s: unexpected type expression type: %T", expr.Span(), expr)
		}
	case *ast.IdentExpression:
		identTy, found := tc.typeScope.lookupType(expr.Ident.String())
		if !found {
			return errors.Errorf("%s: type not found for identifier %s", expr.Span(), expr.Ident)
		}
		ty = identTy
		if _, _, ok := tc.typeScope.lookupVariable(ast.Ident(expr.Ident.String())); !ok {
			tc.typeInfo.typeBindings[expr] = ty
		}
	default:
		panic(fmt.Sprintf("unexpected reference expression type: %T", expr))
	}
	tc.typeInfo.Set(expr, ty)
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

func (tc *typeChecker) resolveCall(
	calleeType CallableType, astArgs []ast.Expression, astTypeArgs []ast.Type, span token.Span) (*Call, error) {
	var calleeArgTypes []Type
	if methodType, ok := calleeType.(*MethodType); ok {
		calleeArgTypes = methodType.ArgTypesWithoutSelf()
	} else {
		calleeArgTypes = calleeType.CallArgTypes()
	}
	if len(calleeArgTypes) != len(astArgs) {
		return nil, errors.Errorf(
			"%s: expected %d arguments, got %d for %s", span, len(calleeArgTypes), len(astArgs), calleeType)
	}
	argTypes := make([]Type, len(astArgs))
	for i, arg := range astArgs {
		argType := tc.typeInfo.MustLookup(arg)
		argTypes[i] = argType
	}
	typeParams := calleeType.TypeParams()
	typeArgs := make([]Type, len(calleeType.TypeParams()))
	if len(astTypeArgs) > 0 {
		if len(astTypeArgs) != len(typeParams) {
			return nil, errors.Errorf(
				"%s: expected %d type arguments, got %d for %q", span, len(typeParams), len(astTypeArgs), calleeType)
		}
		for i, astTypeArg := range astTypeArgs {
			typeArg, err := tc.lookupTypeOfNode(astTypeArg)
			if err != nil {
				return nil, err
			}
			typeArgs[i] = typeArg
		}
	} else if len(typeParams) > 0 {
		// Let's try to infer the type arguments.
		for i, calleeArgType := range calleeArgTypes {
			if typeParam, ok := calleeArgType.(*TypeParam); ok {
				argType := argTypes[i]
				previousTypeArg := typeArgs[i]
				if previousTypeArg != nil && previousTypeArg != argType {
					return nil, errors.Errorf(
						"%s: type argument %q already inferred as %s, got %s", span, typeParam.Name, previousTypeArg, argType)
				}
				typeArgs[i] = argType
			}
		}
	}
	resolveTypeParam := func(ty Type) Type {
		if typeParam, ok := ty.(*TypeParam); ok {
			for i, param := range typeParams {
				if param.Equal(*typeParam) {
					return typeArgs[i]
				}
			}
		}
		return ty
	}
	resolvedCalleeArgTypes := make([]Type, len(calleeArgTypes))
	for i, ty := range calleeArgTypes {
		resolvedCalleeArgTypes[i] = resolveTypeParam(ty)
	}
	returnType := resolveTypeParam(calleeType.CallReturnType())
	// Finally, verify argument and return types.
	for i, argType := range argTypes {
		calleeArgType := resolvedCalleeArgTypes[i]
		if !calleeArgType.IsAssignableFrom(argType) {
			return nil, errors.Errorf(
				"%s: expected argument %d to be of type %s, got %s", span, i, calleeArgType, argType)
		}
	}
	call := Call{
		Callee:     calleeType,
		ArgTypes:   argTypes,
		ReturnType: returnType,
		TypeArgs:   typeArgs,
	}
	return &call, nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	calleeType, ok := tc.typeInfo.MustLookup(expr.Callee).(CallableType)
	if !ok {
		return errors.Errorf("%s: callee %q is not a callable type", expr.Span(), calleeType)
	}
	call, err := tc.resolveCall(calleeType, expr.Args, expr.TypeArgs, expr.Span())
	if err != nil {
		return err
	}
	tc.typeInfo.Set(expr, call.ReturnType)
	// Record the call so we can later on easily determine all the types.
	tc.typeInfo.recordCall(expr, *call)
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
	member, found := structType.FindMember(expr.Field, expr.Span())
	if !found {
		structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
		return errors.Errorf("%s: member %q not found in struct type %q", expr.Span(), expr.Field, structSymbol.Name)
	}
	tc.typeInfo.Set(expr, member)
	return nil
}

func (tc *typeChecker) VisitBlockExpression(expr *ast.BlockExpression, w ast.Walker) error {
	tc.enterScope(expr)
	defer tc.exitScope()
	if err := w.WalkBlockExpression(expr); err != nil {
		return err
	}
	var blockType Type = NoneType
	if len(expr.Nodes) > 0 {
		blockType = tc.typeInfo.MustLookup(expr.Nodes[len(expr.Nodes)-1])
	}
	tc.typeInfo.Set(expr, blockType)
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	if err := w.WalkIfExpression(expr); err != nil {
		return err
	}
	condType := tc.typeInfo.MustLookup(expr.Condition)
	if tc.typeInfo.MustLookup(expr.Condition) != BoolType {
		return errors.Errorf("%s: the condition of an if expression must be a boolean type, got: %s", expr.Condition.Span(), condType)
	}
	// Only an if expression with an else branch can have a type other than None.
	// And currently we don't have else branches.
	tc.typeInfo.Set(expr, NoneType)
	return nil
}

func (tc *typeChecker) VisitStructInitExpression(expr *ast.StructInitExpression, w ast.Walker) error {
	if err := w.WalkStructInitExpression(expr); err != nil {
		return err
	}
	structType_, found := tc.typeScope.lookupType(string(expr.Ident))
	if !found {
		return errors.Errorf("%s: type %q not found for struct init expression", expr.Span(), expr.Ident)
	}
	structType, isType := structType_.(*StructType)
	if !isType {
		return errors.Errorf("%s: type %q is not a struct type", expr.Span(), expr.Ident)
	}
	for _, initField := range expr.Fields {
		structField, found := structType.FindField(initField.Name, initField.Span)
		if !found {
			structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf(
				"%s: field %q not found in struct type %q", initField.Span, initField.Name, structSymbol.Name)
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

func (tc *typeChecker) resolveTypeParams(genericType GenericType, astParams []ast.TypeParam) ([]TypeParam, error) {
	typeParams := make([]TypeParam, len(astParams))
	for i, astParam := range astParams {
		typeParam := TypeParam{BaseType: tc.newType(), GenericType: genericType, Name: astParam.Name, Index: i}
		typeParams[i] = typeParam
		if err := tc.genericScope.declareTypeParam(typeParam.Name.String(), &typeParam, astParams[i].Span()); err != nil {
			return nil, err
		}
	}
	return typeParams, nil
}

func (tc *typeChecker) resolveFunctionArgsAndReturnType(
	astArgs []ast.FunctionArg, astReturnType ast.Type) (argTypes []Type, returnType Type, err error) {

	argTypes = make([]Type, len(astArgs))
	for i, arg := range astArgs {
		argType, err := tc.lookupTypeOfNode(arg.Type)
		if err != nil {
			return nil, nil, errors.Wrapf(err, "%s: type %s not found for argument %s", arg.Span, arg.Type, arg.Name)
		}
		argTypes[i] = argType
	}
	returnType, err = tc.lookupTypeOfNode(astReturnType)
	if err != nil {
		return nil, nil, errors.Wrapf(
			err, "%s: type %s not found for return type", astReturnType.Span(), astReturnType)
	}
	return argTypes, returnType, nil
}

func (tc *typeChecker) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	if tc.checkingMode == insideTraitOrImplMode {
		tc.enterGenericScope()
		defer tc.exitGenericScope()
		isStatic := len(decl.Args) == 0 || decl.Args[0].Name != "self"
		methodType := &MethodType{BaseType: tc.newType(), IsStatic: isStatic}
		typeParams, err := tc.resolveTypeParams(methodType, decl.TypeParams)
		if err != nil {
			return err
		}
		methodType.typeParams = typeParams
		argTypes, returnType, err := tc.resolveFunctionArgsAndReturnType(decl.Args, decl.ReturnType)
		if err != nil {
			return err
		}
		methodType.ArgTypes = argTypes
		methodType.ReturnType = returnType
		tc.typeInfo.Set(decl, &DeclaredType{Type: methodType})
	} else {
		funcType := &FunctionType{BaseType: tc.newType()}
		typeParams, err := tc.resolveTypeParams(funcType, decl.TypeParams)
		if err != nil {
			return err
		}
		funcType.typeParams = typeParams
		argTypes, returnType, err := tc.resolveFunctionArgsAndReturnType(decl.Args, decl.ReturnType)
		if err != nil {
			return err
		}
		funcType.ArgTypes = argTypes
		funcType.ReturnType = returnType
		if decl.Name == "main" {
			if len(funcType.ArgTypes) > 0 {
				return errors.Errorf("%s: main function must not have arguments", decl.Span())
			}
			if _, ok := funcType.ReturnType.(*noneType); !ok {
				return errors.Errorf("%s: main function must return () (no return value)", decl.Span())
			}
			tc.typeInfo.Main = funcType
		}
		if err := tc.typeScope.declareType(string(decl.Name), funcType, decl.Span()); err != nil {
			return err
		}
		tc.typeInfo.Set(decl, &DeclaredType{Type: funcType})
	}
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.Walker) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	if err := tc.VisitFunctionDeclaration(fn.Decl); err != nil {
		return err
	}
	declaredType := tc.typeInfo.MustLookup(fn.Decl).(*DeclaredType)
	callableType := declaredType.Type.(CallableType)
	tc.declareSymbol(callableType.Id(), fn.Decl.Name.String())
	tc.typeInfo.Set(fn, declaredType)
	tc.enterScope(fn)
	defer tc.exitScope()
	argTypes := callableType.CallArgTypes()
	for i, arg := range fn.Decl.Args {
		argType := argTypes[i]
		varInfo := variableInfo{type_: argType, isFunctionArg: true, mutable: false, span: arg.Span}
		if err := tc.typeScope.declareVariable(string(arg.Name), varInfo); err != nil {
			return err
		}
	}
	if err := tc.VisitBlockExpression(fn.Body, w); err != nil {
		return err
	}
	return nil
}

func (tc *typeChecker) VisitTraitDeclaration(trait *ast.TraitDeclaration, w ast.Walker) error {
	// We need to forward declare the trait type so that we can set the `Self` type correctly.
	traitType := &TraitType{BaseType: tc.newType()}
	if err := tc.typeScope.declareType(string(trait.Name), traitType, trait.Span()); err != nil {
		return err
	}
	tc.enterScope(trait)
	defer tc.exitScope()
	if err := tc.typeScope.declareType("Self", traitType, trait.Span()); err != nil {
		return err
	}
	tc.enterCheckingMode(insideTraitOrImplMode)
	defer tc.exitCheckingMode()
	if err := w.WalkTraitDeclaration(trait); err != nil {
		return err
	}
	for _, methodDecl := range trait.MethodDecls {
		methodType := tc.typeInfo.MustLookupDeclaredType(methodDecl).Type.(*MethodType)
		methodType.ReceiverType = traitType
		methodAndName := TypeAndName[MethodType]{Name: methodDecl.Name, Type: *methodType}
		traitType.Methods = append(traitType.Methods, methodAndName)
	}
	tc.declareSymbol(traitType.Id(), trait.Name.String())
	tc.typeInfo.Set(trait, traitType)
	return nil
}

func (tc *typeChecker) VisitImplDefinition(impl *ast.ImplDefinition, w ast.Walker) error {
	structType_, found := tc.typeScope.lookupType(string(impl.Target))
	if !found {
		return errors.Errorf("%s: type %q not found for impl definition", impl.Span(), impl.Target)
	}
	structType, ok := structType_.(*StructType)
	if !ok {
		return errors.Errorf("%s: type %q is not a struct type", impl.Span(), structType_)
	}
	tc.enterScope(impl)
	defer tc.exitScope()
	if err := tc.typeScope.declareType("Self", structType, impl.Span()); err != nil {
		return err
	}
	tc.enterCheckingMode(insideTraitOrImplMode)
	defer tc.exitCheckingMode()
	if err := w.WalkImplDefinition(impl); err != nil {
		return err
	}
	var traitType *TraitType = nil
	unimplementedTraitMethods := map[ast.Ident]*TypeAndName[MethodType]{}
	if impl.ImplementsTrait() {
		traitType_, found := tc.typeScope.lookupType(string(impl.Trait))
		if !found {
			return errors.Errorf("%s: trait %q not found for impl definition", impl.Span(), impl.Trait)
		}
		traitType, ok = traitType_.(*TraitType)
		if !ok {
			return errors.Errorf("%s: type %q is not a trait type", impl.Span(), traitType_)
		}
		for _, method := range traitType.Methods {
			unimplementedTraitMethods[method.Name] = &method
		}
	}
	for _, method := range impl.Methods {
		decl := method.Decl
		if _, found := structType.FindField(decl.Name, decl.Span()); found {
			structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf(
				"%s: method name %q already used in struct type %q", decl.Span(), decl.Name, structSymbol.Name)
		}
		if _, found := structType.FindMethod(decl.Name, decl.Span()); found {
			structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf(
				"%s: method name %q already used in struct type %q", decl.Span(), decl.Name, structSymbol.Name)
		}
		typeDecl := tc.typeInfo.MustLookupDeclaredType(method)
		methodType, ok := typeDecl.Type.(*MethodType)
		if !ok {
			return errors.Errorf("%s: type is not a method type: %s", method.Span(), typeDecl)
		}
		methodType.ReceiverType = structType
		if traitType != nil {
			traitMethodType, err := traitType.FindMethod(decl.Name, decl.Span())
			if err != nil {
				traitSymbol := tc.typeInfo.MustLookupSymbol(traitType.Id())
				return errors.Errorf("%s: method %q not found in trait %q", decl.Span(), decl.Name, traitSymbol.Name)
			}
			if err := traitMethodType.CheckSameSignatureIgnoringReceiverTypes(methodType, decl.Span()); err != nil {
				structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
				traitSymbol := tc.typeInfo.MustLookupSymbol(traitType.Id())
				return errors.Wrapf(
					err,
					"%s: method %q in impl %q has different signature than in trait: %s",
					decl.Span(),
					decl.Name,
					structSymbol.Name,
					traitSymbol.Name,
				)
			}
			delete(unimplementedTraitMethods, decl.Name)
		}
		structType.Methods = append(structType.Methods, TypeAndName[MethodType]{Name: decl.Name, Type: *methodType})
		tc.typeInfo.Set(method, &DeclaredType{Type: methodType})
	}
	if traitType != nil && len(unimplementedTraitMethods) > 0 {
		missingTraitMethods := []string{}
		for _, method := range unimplementedTraitMethods {
			missingTraitMethods = append(missingTraitMethods, method.String())
		}
		structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
		traitSymbol := tc.typeInfo.MustLookupSymbol(traitType.Id())
		return errors.Errorf(
			"%s: impl %q does not implement all methods of trait %q: %s",
			impl.Span(),
			structSymbol.Name,
			traitSymbol.Name,
			strings.Join(missingTraitMethods, ", "),
		)
	}
	tc.typeInfo.Set(impl, &ImplType{BaseType: tc.newType(), ReceiverType: structType})
	return nil
}

func (tc *typeChecker) VisitVariableDefinition(v *ast.VariableDefinition, w ast.Walker) error {
	if err := w.WalkNode(v.Value); err != nil {
		return err
	}
	valueType := tc.typeInfo.MustLookup(v.Value)
	if valueType == NoneType {
		return errors.Errorf("%s: variable %s must have a type that is not None", v.Span(), v.Name)
	}
	varInfo := variableInfo{type_: valueType, mutable: true, span: v.Span()}
	if err := tc.typeScope.declareVariable(string(v.Name), varInfo); err != nil {
		return err
	}
	tc.typeInfo.Set(v, NoneType)
	return nil
}

func (tc *typeChecker) VisitAssignmentStatement(s *ast.AssignmentStatement, w ast.Walker) error {
	if err := w.WalkAssignmentStatement(s); err != nil {
		return err
	}
	rhsType := tc.typeInfo.MustLookup(s.Rhs)
	varType, varInfo, ok := tc.typeScope.lookupVariable(s.Variable.Ident)
	if !ok {
		return errors.Errorf("%s: unknown variable %q", s.Span(), s.Variable.Ident)
	}
	if !varInfo.mutable {
		return errors.Errorf("%s: variable %q is not mutable", s.Span(), s.Variable.Ident)
	}
	if s.IsAssignToMember() {
		structType, ok := varType.(*StructType)
		if !ok {
			return errors.Errorf("%s: variable %q is not a struct type", s.Span(), s.Variable.Ident)
		}
		field, found := structType.FindField(*s.Field, s.Span())
		if !found {
			structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf("%s: field %q not found in struct type %q", s.Span(), s.Field, structSymbol.Name)
		}
		varType = field.Type
	}
	if !varType.IsAssignableFrom(rhsType) {
		return errors.Errorf(
			"%s: lhs and rhs of assignment statement must have the same type, got %s and %s", s.Span(), varType, rhsType)
	}
	tc.typeInfo.Set(s, NoneType)
	return nil
}

func (tc *typeChecker) VisitLoopStatement(s *ast.LoopStatement, w ast.Walker) error {
	tc.typeInfo.Set(s, NoneType)
	tc.enterLoop()
	defer tc.exitLoop()
	return w.WalkLoopStatement(s)
}

func (tc *typeChecker) VisitContinueStatement(s *ast.ContinueStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: continue statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, NoneType)
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: break statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, NoneType)
	return nil
}

func (tc *typeChecker) VisitStructTypeDeclaration(d *ast.StructTypeDeclaration) error {
	fields := []TypeAndName[Type]{}
	for _, field := range d.Fields {
		fieldType, err := tc.lookupTypeOfNode(field.Type)
		if err != nil {
			return errors.Errorf("%s: type %q not found for field %q", field.Span, field.Type, field.Name)
		}
		fields = append(fields, TypeAndName[Type]{Name: field.Name, Type: fieldType})
	}
	structType := &StructType{BaseType: tc.newType(), Fields: fields}
	if err := tc.typeScope.declareType(string(d.Name), structType, d.Span()); err != nil {
		return err
	}
	tc.declareSymbol(structType.Id(), d.Name.String())
	tc.typeInfo.Set(d, &DeclaredType{Type: structType})
	return nil
}

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.Walker) error {
	tc.typeInfo.Set(module, NoneType)
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
	tc := &typeChecker{
		DefaultVisitor: ast.DefaultVisitor{},
		typeInfo: &TypeInfo{
			types:        make(map[ast.NodeId]Type),
			symbols:      make(map[string]*Symbol),
			typeBindings: make(map[ast.ReferenceExpression]Type),
			calls:        make(map[*ast.CallExpression]Call)},
		typeScope:     newTypeScope(nil),
		symbolScope:   newSymbolScope(node, nil),
		genericScope:  newGenericScope(nil),
		nextTypeId:    1000,
		functionTypes: make(map[string]FunctionType),
	}
	// Declare builtin types and functions.
	if err := tc.typeScope.declareType("Str", StrType, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare StrType"))
	}
	if err := tc.typeScope.declareType("Int", Int64Type, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare IntType"))
	}
	if err := tc.typeScope.declareType("None", NoneType, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare NoneType"))
	}
	if err := tc.typeScope.declareType("print", BuiltInPrintFunction, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare print function"))
	}
	if err := tc.typeScope.declareType("print_int", BuiltInPrintIntFunction, builtInSpan); err != nil {
		panic(errors.Wrap(err, "failed to declare print_int function"))
	}
	walker := &ast.DefaultWalker{Visitor: tc}
	res, err := tc.check(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, tc.typeInfo, nil
}
