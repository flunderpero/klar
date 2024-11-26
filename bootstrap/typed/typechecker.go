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

type IsId interface {
	String() string
	IdMarker()
}

type Type interface {
	String() string
	Id() TypeId
	IsAssignableFrom(other Type) bool
}

var BuiltInPrintFunction = &FunctionType{
	BaseType: BaseType{TypeId(100)},
	Params:   []FunctionParam{{Name: "value", Type: StrType}},
	Result:   NoneType,
}
var BuiltInPrintIntFunction = &FunctionType{
	BaseType: BaseType{TypeId(101)},
	Params:   []FunctionParam{{Name: "value", Type: Int64Type}},
	Result:   NoneType,
}
var BuiltInPrintBoolFunction = &FunctionType{
	BaseType: BaseType{TypeId(102)},
	Params:   []FunctionParam{{Name: "value", Type: BoolType}},
	Result:   NoneType,
}
var BuiltInUnsafeMallocFunction = &FunctionType{
	BaseType: BaseType{TypeId(103)},
	Params:   []FunctionParam{{Name: "size", Type: Int64Type}},
	Result:   Int64Type,
}

func IsBuiltInFunction(functionType *FunctionType) bool {
	id := functionType.Id()
	return id == BuiltInPrintFunction.Id() || id == BuiltInPrintIntFunction.Id() || id == BuiltInUnsafeMallocFunction.Id() || id == BuiltInPrintBoolFunction.Id()
}

var builtInSpan = token.Span{File: new(string), Src: &[]byte{}, Start: 0, End: 0}
var StrType = &strType{BaseType: BaseType{1}}
var BoolType = &boolType{BaseType: BaseType{2}}
var Int64Type = &int64Type{BaseType: BaseType{3}}
var NoneType = &noneType{BaseType: BaseType{4}}

type TypeWithTraits interface {
	Type
	Traits() []*TraitType
}

type CallableType interface {
	Type
	CallParams() []TypeAndName[Type]
	CallResult() Type
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
	TypeArgs() []Type
}

func HasTypeParams(ty GenericType) bool {
	return len(ty.TypeParams()) > 0
}

func IsFullyResolved(ty Type) bool {
	switch ty := ty.(type) {
	case *TypeParam:
		return false
	case GenericType:
		for _, typeArg := range ty.TypeArgs() {
			if !IsFullyResolved(typeArg) {
				return false
			}
		}
	}
	return true
}

type TypeParam struct {
	BaseType
	GenericType GenericType
	Name        ast.Ident
	Index       int
}

func (ty TypeParam) String() string {
	return fmt.Sprintf("TypeParam %s #%s of %s[%d]", ty.Name, ty.Id(), ty.GenericType.Id(), ty.Index)
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

func typeArgsString(args []Type) string {
	s := ""
	if len(args) > 0 {
		s = fmt.Sprintf("\n(TypeArgs)%s", base.IndentSlice(args, 1))
	}
	return s
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
	typeParams []TypeParam
	typeArgs   []Type
	Fields     []TypeAndName[Type]
	Methods    []TypeAndName[*FunctionType]
	traits     []*TraitType
}

func (ty StructType) CloneWithNewId(id TypeId) *StructType {
	return &StructType{
		BaseType:   BaseType{id},
		typeParams: ty.typeParams,
		typeArgs:   ty.typeArgs,
		Fields:     ty.Fields,
		Methods:    ty.Methods,
		traits:     ty.traits,
	}
}

func (ty StructType) Traits() []*TraitType {
	return ty.traits
}

func (ty StructType) String() string {
	typeToString := func(t Type) string {
		if t.Id() == ty.Id() {
			return "Self"
		}
		return t.Id().String()
	}
	fields := make([]string, len(ty.Fields))
	for i, field := range ty.Fields {
		fields[i] = fmt.Sprintf("%s\n%s", field.Name, base.IndentString(typeToString(field.Type), 1))
	}
	return fmt.Sprintf(
		"StructType%s%s\n    (Fields)%s\n    (Methods)%s",
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1),
		base.IndentStringSlice(fields, 2),
		base.IndentSlice(ty.Methods, 2),
	)
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

func (ty StructType) FindMethod(name ast.Ident, span token.Span) (*FunctionType, bool) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method.Type, true
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

func (ty StructType) CallParams() []FunctionParam {
	return ty.Fields
}

func (ty StructType) CallResult() Type {
	return &ty
}

func (ty StructType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty StructType) TypeArgs() []Type {
	return ty.typeArgs
}

type TraitType struct {
	BaseType
	typeParams []TypeParam
	typeArgs   []Type
	Methods    []TypeAndName[*FunctionType]
}

func (ty TraitType) String() string {
	return fmt.Sprintf(
		"TraitType%s%s%s",
		base.IndentSlice(ty.Methods, 1),
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1))
}

func (ty *TraitType) FindMethod(name ast.Ident, span token.Span) (*FunctionType, error) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method.Type, nil
		}
	}
	return nil, errors.Errorf("%s: method %q not found in trait type %q", span, name, ty)
}

func (ty *TraitType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty *TraitType) TypeArgs() []Type {
	return ty.typeArgs
}

type ImplType struct {
	BaseType
	ReceiverType Type
}

func (ty ImplType) String() string {
	return fmt.Sprintf("ImplType\n%s", base.Indent(ty.ReceiverType, 1))
}

type FunctionParam = TypeAndName[Type]

type FunctionType struct {
	BaseType
	typeParams []TypeParam
	typeArgs   []Type
	Receiver   Type
	Params     []FunctionParam
	Result     Type
}

func (ty FunctionType) CloneWithNewId(id TypeId) *FunctionType {
	return &FunctionType{
		BaseType:   BaseType{id},
		typeParams: ty.typeParams,
		typeArgs:   ty.typeArgs,
		Receiver:   ty.Receiver,
		Params:     ty.Params,
		Result:     ty.Result,
	}
}

func (ty FunctionType) CloneWithTypeParamsAndArgs(typeParams []TypeParam, typeArgs []Type) *FunctionType {
	return &FunctionType{
		BaseType:   ty.BaseType,
		typeParams: typeParams,
		typeArgs:   typeArgs,
		Receiver:   ty.Receiver,
		Params:     ty.Params,
		Result:     ty.Result,
	}
}

func (ty FunctionType) String() string {
	typeToString := func(t Type) string {
		if ty.Receiver != nil && t.Id() == ty.Receiver.Id() {
			return "Self"
		}
		return t.Id().String()
	}
	receiverType := ""
	if ty.Receiver != nil {
		receiverType = fmt.Sprintf("\n    (Receiver\n%s", base.Indent(ty.Receiver.Id(), 2))
	}
	params := make([]string, len(ty.Params))
	for i, param := range ty.Params {
		params[i] = fmt.Sprintf("%s\n%s", param.Name, base.IndentString(typeToString(param.Type), 1))
	}
	result := typeToString(ty.Result)
	return fmt.Sprintf(
		"FunctionType%s%s%s\n    (Parameters)%s\n    (Result)\n%s",
		receiverType,
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1),
		base.IndentStringSlice(params, 2),
		base.IndentString(result, 2))
}

func (ty FunctionType) IsMethod() bool {
	return ty.Receiver != nil
}

func (ty FunctionType) IsStaticMethod() bool {
	return ty.IsMethod() && (len(ty.Params) == 0 || ty.Params[0].Type.Id() != ty.Receiver.Id())
}

func (ty FunctionType) CheckSameSignatureIgnoringReceiverTypes(other *FunctionType, span token.Span) error {
	match := func(thisType Type, otherType Type) bool {
		if otherType == other.Receiver {
			return thisType == ty.Receiver
		}
		return thisType == otherType
	}
	if len(ty.Params) != len(other.Params) {
		return errors.Errorf("%s: parameter count does not match: %d != %d", span, len(ty.Params), len(other.Params))
	}
	if !match(ty.Result, other.Result) {
		return errors.Errorf("%s: result types do not match: %s != %s", span, ty.Result, other.Result)
	}
	for i, param := range ty.Params {
		otherParam := other.Params[i]
		if !match(param.Type, otherParam.Type) {
			return errors.Errorf("%s: parameter types do not match: %s != %s", span, param, otherParam)
		}
	}
	return nil
}

func (ty FunctionType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty FunctionType) TypeArgs() []Type {
	return ty.typeArgs
}

func (ty FunctionType) IsAssignableFrom(other Type) bool {
	if other.Id() == ty.Id() {
		return true
	}
	if other, ok := other.(*FunctionType); ok {
		otherParams := other.Params
		if len(ty.Params) != len(otherParams) {
			return false
		}
		for i, param := range ty.Params {
			if !param.Type.IsAssignableFrom(otherParams[i].Type) {
				return false
			}
		}
		return ty.Result.IsAssignableFrom(other.Result)
	}
	return false
}

func (ty FunctionType) CallParams() []FunctionParam {
	return ty.Params
}

func (ty FunctionType) CallResult() Type {
	return ty.Result
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
	type_           Type
	isFunctionParam bool
	mutable         bool
	span            token.Span
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

func (self *Symbol) FQN() string {
	scopeFQN := self.Scope.FQN()
	if scopeFQN != "" {
		return fmt.Sprintf("%s::%s", scopeFQN, self.Name)
	}
	return self.Name
}

type SymbolScope struct {
	Parent   *SymbolScope
	Children []*SymbolScope
	Node     ast.Node
	Symbols  map[string]*Symbol
}

func (self *SymbolScope) FQN() string {
	name := ""
	switch node := self.Node.(type) {
	case *ast.FunctionDefinition:
		name = node.Decl.Name.String()
	case *ast.ImplDefinition:
		name = node.Target.String()
	case *ast.Module:
		name = node.Name.String()
	}
	parentFQN := ""
	if self.Parent != nil {
		parentFQN = self.Parent.FQN()
	}
	if name != "" && parentFQN != "" {
		return fmt.Sprintf("%s::%s", parentFQN, name)
	}
	if name != "" {
		return name
	}
	return parentFQN
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
	// The type an `IdentExpression` points to if it does not refer to a variable.
	typeBindings map[*ast.IdentExpression]Type
	Main         *FunctionType
}

func (m *TypeInfo) LookupTypeBinding(expr *ast.IdentExpression) (Type, bool) {
	declaration, found := m.typeBindings[expr]
	return declaration, found
}

func (m *TypeInfo) Lookup(node ast.Node) (Type, bool) {
	ty, ok := m.types[node.Id()]
	return ty, ok
}

func (m *TypeInfo) MustLookup(node ast.Node) Type {
	ty, ok := m.Lookup(node)
	if !ok {
		panic(errors.Errorf("%s: type not found for node #%d: %s", node.Span(), node.Id(), node))
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

func (m *TypeInfo) LookupSymbol(id IsId) (*Symbol, bool) {
	symbol, ok := m.symbols[id.String()]
	return symbol, ok
}

func (m *TypeInfo) MustLookupSymbol(id IsId) *Symbol {
	ty, ok := m.LookupSymbol(id)
	if !ok {
		panic(fmt.Sprintf("symbol not found for key %q", id.String()))
	}
	return ty
}

func (m *TypeInfo) DeclareSymbol(id IsId, symbol *Symbol) {
	m.symbols[id.String()] = symbol
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

func (tc *typeChecker) declareSymbol(key IsId, name string) {
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
		params := make([]TypeAndName[Type], len(node.Params))
		for i, astParam := range node.Params {
			argType, err := tc.lookupTypeOfNode(astParam)
			if err != nil {
				return nil, err
			}
			// todo: `ast.functionType` does not include a name. Do we want to be able to include a
			//       name?
			params[i] = TypeAndName[Type]{Type: argType, Name: ""}
		}
		result, err := tc.lookupTypeOfNode(node.Result)
		if err != nil {
			return nil, err
		}
		res := FunctionType{BaseType: tc.newType(), Params: params, Result: result}
		tc.functionTypes[node.TypeName()] = res
		return &res, nil
	case *ast.TypeParam:
		res, found := tc.genericScope.lookupTypeParam(node.TypeName())
		if !found {
			return nil, errors.Errorf("undefined type parameter: %s", node.TypeName())
		}
		return res, nil
	case *ast.SimpleType:
		if len(node.TypeArgs) > 0 {
			baseType, found := tc.typeScope.lookupType(node.TypeName())
			if !found {
				return nil, errors.Errorf("undefined type: %s", node.TypeName())
			}
			structType, ok := baseType.(*StructType)
			if !ok {
				return nil, errors.Errorf("expected struct type, got: %T", baseType)
			}
			if len(node.TypeArgs) != len(structType.TypeParams()) {
				return nil, errors.Errorf(
					"expected %d type arguments, got %d for %q",
					len(structType.TypeParams()), len(node.TypeArgs), node.TypeName())
			}
			typeArgs := make([]Type, len(node.TypeArgs))
			for i, typeArg := range node.TypeArgs {
				typeArg, err := tc.lookupTypeOfNode(typeArg)
				if err != nil {
					return nil, err
				}
				typeArgs[i] = typeArg
			}
			return tc.resolveGenericType(structType, node.TypeArgs, node.Span())
		}
	}
	if res, found := tc.typeScope.lookupType(node.TypeName()); found {
		return res, nil
	}
	if res, found := tc.genericScope.lookupTypeParam(node.TypeName()); found {
		return res, nil
	}
	return nil, errors.Errorf("undefined type: %s", node.TypeName())
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

func ResolveTypeArgs(ty Type, typeParams []TypeParam, typeArgs []Type) Type {
	if len(typeParams) != len(typeArgs) {
		panic(fmt.Sprintf("expected %d type arguments, got %d while resolving: %s", len(typeParams), len(typeArgs), ty))
	}
	switch ty := ty.(type) {
	case GenericType:
		genericTypeArgs := make([]Type, len(ty.TypeArgs()))
		for i, typeArg := range ty.TypeArgs() {
			genericTypeArgs[i] = ResolveTypeArgs(typeArg, typeParams, typeArgs)
		}
		switch ty := ty.(type) {
		case *FunctionType:
			params := make([]TypeAndName[Type], len(ty.Params))
			for i, param := range ty.Params {
				if ty.Receiver != nil && ty.Receiver.Id() == param.Type.Id() {
					params[i] = param
					continue
				}
				param := param // Make a copy.
				param.Type = ResolveTypeArgs(param.Type, typeParams, typeArgs)
				params[i] = param
			}
			result := ty.Result
			if ty.Receiver == nil || ty.Receiver.Id() != ty.Result.Id() {
				result = ResolveTypeArgs(ty.Result, typeParams, typeArgs)
			}
			return &FunctionType{
				BaseType:   ty.BaseType,
				typeParams: ty.typeParams,
				typeArgs:   genericTypeArgs,
				Params:     params,
				Result:     result,
				Receiver:   ty.Receiver,
			}
		case *StructType:
			fields := make([]TypeAndName[Type], len(ty.Fields))
			for i, field := range ty.Fields {
				field := field // Make a copy.
				field.Type = ResolveTypeArgs(field.Type, typeParams, typeArgs)
				fields[i] = field
			}
			methods := make([]TypeAndName[*FunctionType], len(ty.Methods))
			for i, method := range ty.Methods {
				method := method // Make a copy.
				resolvedMethodType := ResolveTypeArgs(method.Type, typeParams, typeArgs)
				methodType, ok := resolvedMethodType.(*FunctionType)
				if !ok {
					panic(fmt.Sprintf("expected function type, got: %T", resolvedMethodType))
				}
				method.Type = methodType
				methods[i] = method
			}
			return &StructType{
				BaseType:   ty.BaseType,
				typeParams: ty.typeParams,
				typeArgs:   genericTypeArgs,
				Fields:     fields,
				Methods:    methods,
				traits:     ty.traits,
			}
		case *TraitType:
			methods := make([]TypeAndName[*FunctionType], len(ty.Methods))
			for i, method := range ty.Methods {
				method := method // Make a copy.
				resolvedMethodType := ResolveTypeArgs(method.Type, typeParams, typeArgs)
				methodType, ok := resolvedMethodType.(*FunctionType)
				if !ok {
					panic(fmt.Sprintf("expected function type, got: %T", resolvedMethodType))
				}
				method.Type = methodType
				methods[i] = method
			}
			return &TraitType{
				BaseType:   ty.BaseType,
				typeParams: ty.typeParams,
				typeArgs:   genericTypeArgs,
				Methods:    methods,
			}
		default:
			panic(fmt.Sprintf("unexpected generic type: %T", ty))
		}
	default:
		for i, typeParam := range typeParams {
			if ty.Id() == typeParam.Id() {
				return typeArgs[i]
			}
		}
		return ty
	}
}

func (tc *typeChecker) resolveGenericType(ty GenericType, astTypeArgs []ast.Type, span token.Span) (Type, error) {
	typeParams := ty.TypeParams()
	if len(astTypeArgs) != len(typeParams) {
		return nil, errors.Errorf(
			"%s: expected %d type arguments, got %d for %q", span, len(typeParams), len(astTypeArgs), ty)
	}
	typeArgs := make([]Type, len(astTypeArgs))
	for i, astTypeArg := range astTypeArgs {
		typeArg, err := tc.lookupTypeOfNode(astTypeArg)
		if err != nil {
			return nil, err
		}
		typeArgs[i] = typeArg
	}
	return ResolveTypeArgs(ty, typeParams, typeArgs), nil
}

func (tc *typeChecker) VisitIdentExpression(expr *ast.IdentExpression) error {
	ty, found := tc.typeScope.lookupType(expr.Ident.String())
	if !found {
		return errors.Errorf("%s: type not found for identifier %s", expr.Span(), expr.Ident)
	}
	if genericType, ok := ty.(GenericType); ok && len(expr.TypeArgs) > 0 {
		resolvedType, err := tc.resolveGenericType(genericType, expr.TypeArgs, expr.Span())
		if err != nil {
			return err
		}
		ty = resolvedType
	}
	if _, _, ok := tc.typeScope.lookupVariable(expr.Ident); !ok {
		tc.typeInfo.typeBindings[expr] = ty
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
		// For now, we only support equality of Int (alias for Int64) and Bool.
		if lhs != Int64Type && lhs != BoolType {
			return errors.Errorf("%s: lhs of equality expression must be of type Int64Type, got %s", expr.Span(), lhs)
		}
		if rhs != lhs {
			return errors.Errorf("%s: rhs of equality expression must match lhs, expected %q got %q", expr.Span(), lhs, rhs)
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
	params := calleeType.CallParams()
	result := calleeType.CallResult()
	if funcType, ok := calleeType.(*FunctionType); ok {
		if funcType.IsMethod() && !funcType.IsStaticMethod() {
			params = params[1:]
		}
	}
	if len(params) != len(expr.Args) {
		return errors.Errorf(
			"%s: expected %d arguments, got %d for %s", expr.Span(), len(params), len(expr.Args), calleeType)
	}
	seenParamIndexes := []int{}
	for i, arg := range expr.Args {
		argType := tc.typeInfo.MustLookup(arg.Value)
		var paramIndex = i
		if arg.Name != "" {
			paramIndex = slices.IndexFunc(params, func(p TypeAndName[Type]) bool { return p.Name == arg.Name })
		}
		if paramIndex < 0 {
			return errors.Errorf("%s: parameter %q not found in callee type %s", arg.Span, arg.Name, calleeType)
		}
		if slices.Contains(seenParamIndexes, paramIndex) {
			return errors.Errorf("%s: parameter %q is already assigned", arg.Span, arg.Name)
		}
		seenParamIndexes = append(seenParamIndexes, paramIndex)
		param := params[paramIndex]
		if !param.Type.IsAssignableFrom(argType) {
			return errors.Errorf(
				"%s: expected argument %d to be of type %s, got %s", expr.Span(), paramIndex, param.Type, argType)
		}
	}
	tc.typeInfo.Set(expr, result)
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
	memberType, found := structType.FindMember(expr.Field, expr.Span())
	if !found {
		structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
		return errors.Errorf("%s: member %q not found in struct type %q", expr.Span(), expr.Field, structSymbol.Name)
	}
	if genericType, ok := memberType.(GenericType); ok && len(expr.TypeArgs) > 0 {
		resolvedType, err := tc.resolveGenericType(genericType, expr.TypeArgs, expr.Span())
		if err != nil {
			return err
		}
		memberType = resolvedType
	}
	tc.typeInfo.Set(expr, memberType)
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

func (tc *typeChecker) resolveFunctionParamsAndResult(
	astParams []ast.FunctionParam, astResult ast.Type) (params []TypeAndName[Type], result Type, err error) {

	params = make([]TypeAndName[Type], len(astParams))
	for i, arg := range astParams {
		argType, err := tc.lookupTypeOfNode(arg.Type)
		if err != nil {
			return nil, nil, errors.Wrapf(err, "%s: type %s not found for parameter %s", arg.Span, arg.Type, arg.Name)
		}
		params[i] = TypeAndName[Type]{Type: argType, Name: arg.Name}
	}
	result, err = tc.lookupTypeOfNode(astResult)
	if err != nil {
		return nil, nil, errors.Wrapf(
			err, "%s: type %s not found for return type", astResult.Span(), astResult)
	}
	return params, result, nil
}

func (tc *typeChecker) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	funcType := &FunctionType{BaseType: tc.newType()}
	typeParams, err := tc.resolveTypeParams(funcType, decl.TypeParams)
	if err != nil {
		return err
	}
	funcType.typeParams = typeParams
	// The declared function type will have the type parameters as its type arguments. This way
	// we don't have to distinguish between a type with type arguments set and one without type
	// arguments.
	funcType.typeArgs = make([]Type, len(typeParams))
	for i, typeParam := range typeParams {
		funcType.typeArgs[i] = typeParam
	}
	params, result, err := tc.resolveFunctionParamsAndResult(decl.Params, decl.Result)
	if err != nil {
		return err
	}
	funcType.Params = params
	funcType.Result = result
	if tc.checkingMode != insideTraitOrImplMode {
		if decl.Name == "main" {
			if len(funcType.Params) > 0 {
				return errors.Errorf("%s: main function must not have arguments", decl.Span())
			}
			if _, ok := funcType.Result.(*noneType); !ok {
				return errors.Errorf("%s: main function must return () (no return value)", decl.Span())
			}
			tc.typeInfo.Main = funcType
		}
		if err := tc.typeScope.declareType(string(decl.Name), funcType, decl.Span()); err != nil {
			return err
		}
	}
	tc.typeInfo.Set(decl, &DeclaredType{Type: funcType})
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.Walker) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	if err := tc.VisitFunctionDeclaration(fn.Decl); err != nil {
		return err
	}
	declaredType := tc.typeInfo.MustLookup(fn.Decl).(*DeclaredType)
	functionType := declaredType.Type.(*FunctionType)
	tc.declareSymbol(functionType.Id(), fn.Decl.Name.String())
	tc.typeInfo.Set(fn, declaredType)
	tc.enterScope(fn)
	defer tc.exitScope()
	params := functionType.Params
	for i, astParam := range fn.Decl.Params {
		param := params[i]
		varInfo := variableInfo{type_: param.Type, isFunctionParam: true, mutable: false, span: astParam.Span}
		if err := tc.typeScope.declareVariable(string(param.Name), varInfo); err != nil {
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
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	typeParams, err := tc.resolveTypeParams(traitType, trait.TypeParams)
	if err != nil {
		return err
	}
	traitType.typeParams = typeParams
	if err := tc.typeScope.declareType("Self", traitType, trait.Span()); err != nil {
		return err
	}
	tc.enterCheckingMode(insideTraitOrImplMode)
	defer tc.exitCheckingMode()
	for _, decl := range trait.MethodDecls {
		tc.enterGenericScope()
		err := tc.VisitFunctionDeclaration(decl)
		if err != nil {
			tc.exitGenericScope()
			return err
		}
		tc.exitGenericScope()
	}
	for _, methodDecl := range trait.MethodDecls {
		methodType := tc.typeInfo.MustLookupDeclaredType(methodDecl).Type.(*FunctionType)
		methodType.Receiver = traitType
		methodAndName := TypeAndName[*FunctionType]{Name: methodDecl.Name, Type: methodType}
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
	var traitType *TraitType = nil
	if impl.ImplementsTrait() {
		traitType_, found := tc.typeScope.lookupType(string(impl.Trait))
		if !found {
			return errors.Errorf("%s: trait %q not found for impl definition", impl.Span(), impl.Trait)
		}
		traitType, ok = traitType_.(*TraitType)
		if !ok {
			return errors.Errorf("%s: type %q is not a trait type", impl.Span(), traitType_)
		}
	}
	if HasTypeParams(structType) {
		tc.enterGenericScope()
		defer tc.exitGenericScope()
		for _, typeParam := range structType.typeParams {
			if err := tc.genericScope.declareTypeParam(typeParam.Name.String(), &typeParam, impl.Span()); err != nil {
				return err
			}
		}
	}
	if traitType != nil && HasTypeParams(traitType) {
		traitType_, err := tc.resolveGenericType(traitType, impl.TraitTypeArgs, impl.Span())
		if err != nil {
			return err
		}
		traitType = traitType_.(*TraitType)
	}
	if err := tc.typeScope.declareType("Self", structType, impl.Span()); err != nil {
		return err
	}
	tc.enterCheckingMode(insideTraitOrImplMode)
	defer tc.exitCheckingMode()
	if err := w.WalkImplDefinition(impl); err != nil {
		return err
	}
	unimplementedTraitMethods := map[ast.Ident]*TypeAndName[*FunctionType]{}
	if traitType != nil {
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
		methodType, ok := typeDecl.Type.(*FunctionType)
		if !ok {
			return errors.Errorf("%s: type is not a function type: %s", method.Span(), typeDecl)
		}
		methodType.Receiver = structType
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
		structType.Methods = append(structType.Methods, TypeAndName[*FunctionType]{Name: decl.Name, Type: methodType})
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

func (tc *typeChecker) VisitStructTypeDeclaration(decl *ast.StructTypeDeclaration) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	structType := &StructType{BaseType: tc.newType()}
	typeParams, err := tc.resolveTypeParams(structType, decl.TypeParams)
	if err != nil {
		return err
	}
	structType.typeParams = typeParams
	// The declared struct type will have the type parameters as its type arguments. This way
	// we don't have to distinguish between a type with type arguments set and one without type
	// arguments.
	structType.typeArgs = make([]Type, len(typeParams))
	for i, typeParam := range typeParams {
		structType.typeArgs[i] = typeParam
	}
	fields := []TypeAndName[Type]{}
	for _, field := range decl.Fields {
		fieldType, err := tc.lookupTypeOfNode(field.Type)
		if err != nil {
			return errors.Errorf("%s: type %q not found for field %q", field.Span, field.Type, field.Name)
		}
		fields = append(fields, TypeAndName[Type]{Name: field.Name, Type: fieldType})
	}
	if err := tc.typeScope.declareType(string(decl.Name), structType, decl.Span()); err != nil {
		return err
	}
	structType.Fields = fields
	tc.declareSymbol(structType.Id(), decl.Name.String())
	tc.typeInfo.Set(decl, &DeclaredType{Type: structType})
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
			typeBindings: make(map[*ast.IdentExpression]Type),
		},
		typeScope:     newTypeScope(nil),
		symbolScope:   newSymbolScope(node, nil),
		genericScope:  newGenericScope(nil),
		nextTypeId:    1000,
		functionTypes: make(map[string]FunctionType),
	}
	// Declare builtin types and functions.
	builtInSymbolScope := newSymbolScope(nil, nil)
	declareBuiltIn := func(name string, ty Type) {
		if err := tc.typeScope.declareType(name, ty, builtInSpan); err != nil {
			panic(errors.Wrapf(err, "failed to declare: %s", name))
		}
		tc.typeInfo.DeclareSymbol(ty.Id(), &Symbol{Name: name, Scope: builtInSymbolScope})
	}
	declareBuiltIn("None", NoneType)
	declareBuiltIn("Str", StrType)
	declareBuiltIn("Bool", BoolType)
	declareBuiltIn("Int", Int64Type)
	declareBuiltIn("print", BuiltInPrintFunction)
	declareBuiltIn("print_int", BuiltInPrintIntFunction)
	declareBuiltIn("print_bool", BuiltInPrintBoolFunction)
	declareBuiltIn("_unsafe_malloc", BuiltInUnsafeMallocFunction)
	walker := &ast.DefaultWalker{Visitor: tc}
	res, err := tc.check(node, walker)
	if err != nil {
		return nil, nil, err
	}
	return res, tc.typeInfo, nil
}
