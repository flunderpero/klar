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
	return fmt.Sprintf("#t%d", id)
}

func (id TypeId) IdMarker() {}

type TypeCreator struct {
	nextTypeId int
}

func NewTypeCreator() *TypeCreator {
	return &TypeCreator{nextTypeId: 1000}
}

func (self *TypeCreator) newTypeBase() typeBase {
	self.nextTypeId++
	return typeBase{id: TypeId(self.nextTypeId)}
}

func (self *TypeCreator) newImplementableTypeBase(methods []*Method, traits []*TraitType) implementableTypeBase {
	return implementableTypeBase{typeBase: self.newTypeBase(), methods: methods, traits: traits}
}

func (self *TypeCreator) NewFunctionType(genericBase *FunctionType, selfTypeParam *TypeParam, typeParams []TypeParam, typeArgs []Type, receiver Type, params []ParamOrField, result Type) *FunctionType {
	return &FunctionType{
		typeBase:      self.newTypeBase(),
		genericBase:   genericBase,
		typeParams:    typeParams,
		typeArgs:      typeArgs,
		Receiver:      receiver,
		Params:        params,
		Result:        result,
		SelfTypeParam: selfTypeParam,
	}
}

func (self *TypeCreator) NewTraitType(genericBase *TraitType, typeParams []TypeParam, typeArgs []Type, methods []*Method) *TraitType {
	return &TraitType{
		typeBase:    self.newTypeBase(),
		genericBase: genericBase,
		typeParams:  typeParams,
		typeArgs:    typeArgs,
		Methods:     methods,
	}
}

func (self *TypeCreator) NewStructType(genericBase *StructType, typeParams []TypeParam, typeArgs []Type, fields []ParamOrField, methods []*Method, traits []*TraitType) *StructType {
	return &StructType{
		implementableTypeBase: self.newImplementableTypeBase(methods, traits),
		genericBase:           genericBase,
		typeParams:            typeParams,
		typeArgs:              typeArgs,
		Fields:                fields,
	}
}

func (self *TypeCreator) NewTupleType(values []Type) *TupleType {
	return &TupleType{typeBase: self.newTypeBase(), Values: values}
}

func (self *TypeCreator) NewUnionType(genericBase *UnionType, typeParams []TypeParam, typeArgs []Type, variants []UnionVariant, isAnonymous bool) *UnionType {
	return &UnionType{
		typeBase:    self.newTypeBase(),
		genericBase: genericBase,
		typeParams:  typeParams,
		typeArgs:    typeArgs,
		Variants:    variants,
		IsAnonymous: isAnonymous,
	}
}

type IsId interface {
	String() string
	IdMarker()
}

type Type interface {
	String() string
	Id() TypeId
	IsAssignableFrom(other Type) bool
}

func simpleString(ty Type) string {
	if ty.Id() == 1 {
		return "Str"
	}
	if ty, ok := ty.(TypeParam); ok {
		return ty.String()
	}
	tyStr := strings.Split(fmt.Sprintf("%T", ty), ".")[1]
	if generic, ok := ty.(GenericType); ok {
		if base, ok := generic.GenericBase(); ok {
			tyStr += fmt.Sprintf(" (base %s)", base.Id())
		}
	}
	return fmt.Sprintf("%s %s", tyStr, ty.Id())
}

type TypeWithTraits interface {
	Type
	Traits() []*TraitType
}

type Method = TypeAndName[*FunctionType]

type ImplementableType interface {
	TypeWithTraits
	Methods() []*Method
	FindMethod(name ast.Ident) (*FunctionType, bool)
	addMethod(method *Method) bool
	addTrait(traitType *TraitType) bool
}

type implementableTypeBase struct {
	typeBase
	traits  []*TraitType
	methods []*Method
}

func (self implementableTypeBase) Traits() []*TraitType {
	return self.traits
}

func (self implementableTypeBase) Methods() []*Method {
	return self.methods
}

func (self *implementableTypeBase) addMethod(method *Method) bool {
	if _, found := self.FindMethod(method.Name); found {
		return false
	}
	self.methods = append(self.methods, method)
	return true
}

func (self *implementableTypeBase) addTrait(traitType *TraitType) bool {
	for _, trait := range self.traits {
		if trait.Id() == traitType.Id() {
			return false
		}
	}
	self.traits = append(self.traits, traitType)
	return true
}

func (self implementableTypeBase) FindMethod(name ast.Ident) (*FunctionType, bool) {
	for _, method := range self.methods {
		if method.Name == name {
			return method.Type, true
		}
	}
	return nil, false
}

type CallableType interface {
	Type
	CallParams() []ParamOrField
	CallResult() Type
}

type typeBase struct {
	id TypeId
}

func (ty typeBase) Id() TypeId {
	return ty.id
}

func (ty typeBase) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

type CharType struct {
	implementableTypeBase
}

func (ty CharType) Id() TypeId {
	return 2
}

func (ty CharType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

func (ty CharType) String() string {
	return "CharType"
}

type BoolType struct {
	implementableTypeBase
}

func (ty BoolType) Id() TypeId {
	return 3
}

func (ty BoolType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

func (ty BoolType) String() string {
	return "BoolType"
}

type IntType interface {
	IsSigned() bool
	Bits() int
}

type Int8Type struct {
	implementableTypeBase
}

func (ty Int8Type) IsSigned() bool {
	return true
}

func (ty Int8Type) Bits() int {
	return 8
}

func (ty Int8Type) Id() TypeId {
	return 4
}

func (ty Int8Type) IsAssignableFrom(other Type) bool {
	return other.Id() == Int8Type{}.Id()
}

func (ty Int8Type) String() string {
	return "Int8Type"
}

type Int16Type struct {
	implementableTypeBase
}

func (ty Int16Type) IsSigned() bool {
	return true
}

func (ty Int16Type) Bits() int {
	return 16
}

func (ty Int16Type) Id() TypeId {
	return 5
}

func (ty Int16Type) IsAssignableFrom(other Type) bool {
	return other.Id() == Int16Type{}.Id() || other.Id() == Int8Type{}.Id() || other.Id() == UInt8Type{}.Id()
}

func (ty Int16Type) String() string {
	return "Int16Type"
}

type Int32Type struct {
	implementableTypeBase
}

func (ty Int32Type) IsSigned() bool {
	return true
}

func (ty Int32Type) Bits() int {
	return 32
}

func (ty Int32Type) Id() TypeId {
	return 6
}

func (ty Int32Type) IsAssignableFrom(other Type) bool {
	return other.Id() == Int32Type{}.Id() || other.Id() == Int16Type{}.Id() || other.Id() == Int8Type{}.Id() || other.Id() == UInt8Type{}.Id() || other.Id() == UInt16Type{}.Id()
}

func (ty Int32Type) String() string {
	return "Int32Type"
}

type Int64Type struct {
	implementableTypeBase
}

func (ty Int64Type) IsSigned() bool {
	return true
}

func (ty Int64Type) Bits() int {
	return 64
}

func (ty Int64Type) Id() TypeId {
	return 7
}

func (ty Int64Type) IsAssignableFrom(other Type) bool {
	return other.Id() == Int64Type{}.Id() || other.Id() == Int32Type{}.Id() || other.Id() == Int16Type{}.Id() || other.Id() == Int8Type{}.Id() || other.Id() == UInt8Type{}.Id() || other.Id() == UInt16Type{}.Id() || other.Id() == UInt32Type{}.Id()
}

func (ty Int64Type) String() string {
	return "Int64Type"
}

type UInt8Type struct {
	implementableTypeBase
}

func (ty UInt8Type) IsSigned() bool {
	return false
}

func (ty UInt8Type) Bits() int {
	return 8
}

func (ty UInt8Type) Id() TypeId {
	return 8
}

func (ty UInt8Type) IsAssignableFrom(other Type) bool {
	return other.Id() == UInt8Type{}.Id()
}

func (ty UInt8Type) String() string {
	return "UInt8Type"
}

type UInt16Type struct {
	implementableTypeBase
}

func (ty UInt16Type) IsSigned() bool {
	return false
}

func (ty UInt16Type) Bits() int {
	return 16
}

func (ty UInt16Type) Id() TypeId {
	return 9
}

func (ty UInt16Type) IsAssignableFrom(other Type) bool {
	return other.Id() == UInt16Type{}.Id() || other.Id() == UInt8Type{}.Id()
}

func (ty UInt16Type) String() string {
	return "UInt16Type"
}

type UInt32Type struct {
	implementableTypeBase
}

func (ty UInt32Type) IsSigned() bool {
	return false
}

func (ty UInt32Type) Bits() int {
	return 32
}

func (ty UInt32Type) Id() TypeId {
	return 10
}

func (ty UInt32Type) IsAssignableFrom(other Type) bool {
	return other.Id() == UInt32Type{}.Id() || other.Id() == UInt16Type{}.Id() || other.Id() == UInt8Type{}.Id()
}

func (ty UInt32Type) String() string {
	return "UInt32Type"
}

type UInt64Type struct {
	implementableTypeBase
}

func (ty UInt64Type) IsSigned() bool {
	return false
}

func (ty UInt64Type) Bits() int {
	return 64
}

func (ty UInt64Type) Id() TypeId {
	return 11
}

func (ty UInt64Type) IsAssignableFrom(other Type) bool {
	return other.Id() == UInt64Type{}.Id() || other.Id() == UInt32Type{}.Id() || other.Id() == UInt16Type{}.Id() || other.Id() == UInt8Type{}.Id()
}

func (ty UInt64Type) String() string {
	return "UInt64Type"
}

type NoneType struct {
}

func (ty NoneType) Id() TypeId {
	return 12
}

func (ty NoneType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

func (ty NoneType) String() string {
	return "NoneType"
}

type RawPtr struct{}

func (ty RawPtr) Id() TypeId {
	return 13
}

func (ty RawPtr) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id() || other.Id() == Int64Type{}.Id()
}

func (ty RawPtr) String() string {
	return "RawPtr"
}

type NeverType struct {
}

func (ty NeverType) Id() TypeId {
	return 14
}

func (ty NeverType) IsAssignableFrom(other Type) bool {
	return false
}

func (ty NeverType) String() string {
	return "NeverType"
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
	GenericBase() (GenericType, bool)
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
	typeBase
	GenericType GenericType
	Name        ast.Ident
	Index       int
	TraitBound  *TraitType
}

func (ty TypeParam) String() string {
	traitBound := ""
	if ty.TraitBound != nil {
		traitBound = fmt.Sprintf("\n(TraitBound)\n%s", base.Indent(ty.TraitBound, 1))
	}
	return fmt.Sprintf("TypeParam %s %s of %s[%d]%s", ty.Name, ty.Id(), ty.GenericType.Id(), ty.Index, traitBound)
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

func (t TypeParam) Traits() []*TraitType {
	return []*TraitType{t.TraitBound}
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

type TupleType struct {
	typeBase
	Values []Type
}

func (self TupleType) String() string {
	values := []string{}
	for _, ty := range self.Values {
		values = append(values, simpleString(ty))
	}
	return fmt.Sprintf("TupleType%s", base.IndentStringSlice(values, 1))
}

func (self TupleType) IsAssignableFrom(other Type) bool {
	if self.id == other.Id() {
		return true
	}
	if other, ok := other.(*TupleType); ok {
		if len(self.Values) != len(other.Values) {
			return false
		}
		for i, value := range self.Values {
			if !value.IsAssignableFrom(other.Values[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func (self TupleType) CallParams() []ParamOrField {
	params := make([]ParamOrField, len(self.Values))
	for i, value := range self.Values {
		params[i] = ParamOrField{Name: ast.Ident(fmt.Sprintf("%d", i)), Type: value}
	}
	return params
}

func (self TupleType) CallResult() Type {
	return &self
}

type UnionVariantKind int

const (
	UnionVariantKindNamed UnionVariantKind = 1
	UnionVariantKindType  UnionVariantKind = 2
)

type NamedUnionVariant struct {
	typeBase
	Name        ast.Ident
	Type        *TupleType
	UnionType   *UnionType
	Constructor *NamedUnionVariantConstructor
}

func (self NamedUnionVariant) IsUnitVariant() bool {
	return len(self.Type.Values) == 0
}

func (self NamedUnionVariant) String() string {
	return fmt.Sprintf("NamedUnionVariant %s\n%s", self.Name, base.Indent(self.Type, 1))
}

type NamedUnionVariantConstructor struct {
	typeBase
	Type *NamedUnionVariant
}

func (self NamedUnionVariantConstructor) String() string {
	return fmt.Sprintf("NamedUnionVariantConstructor %s", self.Type.Name)
}

func (self NamedUnionVariantConstructor) CallParams() []ParamOrField {
	return self.Type.Type.CallParams()
}

func (self NamedUnionVariantConstructor) CallResult() Type {
	return self.Type
}

type UnionVariant struct {
	Kind  UnionVariantKind
	Named NamedUnionVariant
	Type  Type
}

func (self UnionVariant) String() string {
	switch self.Kind {
	case UnionVariantKindNamed:
		return self.Named.String()
	case UnionVariantKindType:
		return fmt.Sprintf("TypeVariant %s", simpleString(self.Type))
	default:
		panic(fmt.Sprintf("unexpected union variant kind: %d", self.Kind))
	}
}

func (self UnionVariant) AsType() Type {
	switch self.Kind {
	case UnionVariantKindType:
		return self.Type
	case UnionVariantKindNamed:
		return self.Named
	default:
		panic(fmt.Sprintf("unexpected union variant kind: %d", self.Kind))
	}
}

type UnionType struct {
	typeBase
	genericBase *UnionType
	typeParams  []TypeParam
	typeArgs    []Type
	Variants    []UnionVariant
	IsAnonymous bool
}

func (self UnionType) String() string {
	anon := ""
	if self.IsAnonymous {
		anon = "\n    (anonymous)"
	}
	return fmt.Sprintf("UnionType%s%s%s%s",
		anon,
		base.IndentString(typeParamsString(self.typeParams), 1),
		base.IndentString(typeArgsString(self.typeArgs), 1),
		base.IndentSlice(self.Variants, 1),
	)
}

func (self UnionType) FindNamedVariant(name ast.Ident) (*NamedUnionVariant, bool) {
	for _, variant := range self.Variants {
		if variant.Kind == UnionVariantKindNamed && variant.Named.Name == name {
			return &variant.Named, true
		}
	}
	return nil, false
}

func (self UnionType) TypeParams() []TypeParam {
	return self.typeParams
}

func (self UnionType) TypeArgs() []Type {
	return self.typeArgs
}

func (self UnionType) GenericBase() (GenericType, bool) {
	if self.genericBase == nil {
		return nil, false
	}
	return self.genericBase, true
}

func (self UnionType) IsAssignableFrom(other Type) bool {
	if self.Id() == other.Id() {
		return true
	}
	for _, variant := range self.Variants {
		if variant.AsType().IsAssignableFrom(other) {
			return true
		}
	}
	return false
}

func (self UnionType) Contains(other *UnionType) bool {
	if other.IsAnonymous {
		for _, variant := range other.Variants {
			if !self.IsAssignableFrom(variant.AsType()) {
				return false
			}
		}
		return true
	}
	return self.IsAssignableFrom(other)
}

type StructType struct {
	implementableTypeBase
	genericBase *StructType
	typeParams  []TypeParam
	typeArgs    []Type
	Fields      []ParamOrField
}

func (ty StructType) String() string {
	if ty.Id() == 1 {
		return "Str"
	}
	typeToString := func(t Type) string {
		if t.Id() == ty.Id() {
			return "Self"
		}
		return simpleString(t)
	}
	fields := make([]string, len(ty.Fields))
	for i, field := range ty.Fields {
		fields[i] = fmt.Sprintf("%s %s", field.Name, base.BreakIfMultiline(typeToString(field.Type), 1))
	}
	baseType := ""
	if ty.genericBase != nil {
		baseType = fmt.Sprintf(" (base %s)", ty.genericBase.id)
	}
	return fmt.Sprintf(
		"StructType %s%s%s%s\n    (Fields)%s\n    (Methods)%s",
		ty.id,
		baseType,
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1),
		base.IndentStringSlice(fields, 2),
		base.IndentSlice(ty.Methods(), 2),
	)
}

func (ty StructType) FindFieldIndex(name ast.MemberExpressionField) (int, bool) {
	fieldIndex := slices.IndexFunc(ty.Fields, func(field ParamOrField) bool { return string(field.Name) == string(name) })
	if fieldIndex < 0 {
		return -1, false
	}
	return fieldIndex, true
}

func (ty StructType) FindField(name ast.MemberExpressionField) (*ParamOrField, bool) {
	fieldIndex, found := ty.FindFieldIndex(name)
	if !found {
		return nil, false
	}
	return &ty.Fields[fieldIndex], true
}

func (ty StructType) FindMember(name ast.MemberExpressionField, span token.Span) (Type, bool) {
	field, found := ty.FindField(name)
	if found {
		return field.Type, true
	}
	if name.IsIndex() {
		return nil, false
	}
	method, found := ty.FindMethod(name.AsIdent())
	if found {
		return method, true
	}
	return nil, false
}

func (ty *StructType) addMethod(method *Method) bool {
	if _, found := ty.FindMethod(method.Name); found {
		return false
	}
	if _, found := ty.FindField(ast.MemberExpressionField(method.Name)); found {
		return false
	}
	return ty.implementableTypeBase.addMethod(method)
}

func (ty StructType) CallParams() []ParamOrField {
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

func (ty StructType) GenericBase() (GenericType, bool) {
	if ty.genericBase == nil {
		return nil, false
	}
	return ty.genericBase, true
}

type TraitType struct {
	typeBase
	genericBase *TraitType
	typeParams  []TypeParam
	typeArgs    []Type
	Methods     []*Method
}

func (ty TraitType) String() string {
	return fmt.Sprintf(
		"TraitType%s%s%s",
		base.IndentSlice(ty.Methods, 1),
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1))
}

func (ty *TraitType) FindMethod(name ast.Ident, span token.Span) (*FunctionType, bool) {
	for _, method := range ty.Methods {
		if method.Name == name {
			return method.Type, true
		}
	}
	return nil, false
}

func (ty TraitType) TypeParams() []TypeParam {
	return ty.typeParams
}

func (ty TraitType) TypeArgs() []Type {
	return ty.typeArgs
}

func (ty TraitType) GenericBase() (GenericType, bool) {
	if ty.genericBase == nil {
		return nil, false
	}
	return ty.genericBase, true
}

func (self TraitType) IsAssignableFrom(other Type) bool {
	if other.Id() == self.Id() {
		return true
	}
	typeWithTraits, ok := other.(TypeWithTraits)
	if !ok {
		return false
	}
	for _, trait := range typeWithTraits.Traits() {
		if self.IsAssignableFrom(trait) {
			return true
		}
	}
	return false
}

type ImplType struct {
	typeBase
	ReceiverType Type
}

func (ty ImplType) String() string {
	return fmt.Sprintf("ImplType\n%s", base.Indent(ty.ReceiverType, 1))
}

type ParamOrField struct {
	Type    Type
	Name    ast.Ident
	Mutable bool
}

func (ty ParamOrField) String() string {
	mutable := ""
	if ty.Mutable {
		mutable = "\n    (mutable)"
	}
	return fmt.Sprintf("ParamOrField %s%s\n%s", ty.Name, mutable, base.Indent(ty.Type, 1))
}

type FunctionType struct {
	typeBase
	genericBase   *FunctionType
	typeParams    []TypeParam
	typeArgs      []Type
	Receiver      Type
	Params        []ParamOrField
	Result        Type
	SelfTypeParam *TypeParam // may be nil
}

func (ty FunctionType) String() string {
	typeToString := func(t Type) string {
		if ty.Receiver != nil && t.Id() == ty.Receiver.Id() {
			return fmt.Sprintf("Self %s", t.Id())
		}
		return simpleString(t)
	}
	receiverType := ""
	if ty.Receiver != nil {
		receiverType = fmt.Sprintf("\n    (Receiver %s)", typeToString(ty.Receiver))
	}
	params := make([]string, len(ty.Params))
	for i, param := range ty.Params {
		paramName := param.Name.String()
		if paramName == "" {
			paramName = "<positional>"
		}
		mutable := ""
		if param.Mutable {
			mutable = "mut "
		}
		params[i] = fmt.Sprintf("%s%s %s", mutable, paramName, base.BreakIfMultiline(typeToString(param.Type), 1))
	}
	result := typeToString(ty.Result)
	baseType := ""
	if ty.genericBase != nil {
		baseType = fmt.Sprintf(" (base %s)", ty.genericBase.id)
	}
	return fmt.Sprintf(
		"FunctionType %s%s%s%s%s\n    (Parameters)%s\n    (Result)\n%s",
		ty.id,
		baseType,
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

func checkSameSignatureIgnoringReceiverTypesHelper(this *FunctionType, other *FunctionType, thisType Type, otherType Type) bool {
	if thisType.Id() == otherType.Id() {
		return true
	}
	if otherType.Id() == other.Receiver.Id() {
		return thisType.Id() == this.Receiver.Id()
	}
	if thisTypeParam, ok := thisType.(*TypeParam); ok {
		if otherTypeParam, ok := otherType.(*TypeParam); ok {
			return thisTypeParam.Index == otherTypeParam.Index
		}
	}
	if thisUnionType, ok := thisType.(*UnionType); ok {
		if otherUnionType, ok := otherType.(*UnionType); ok {
			if !thisUnionType.IsAnonymous ||
				!otherUnionType.IsAnonymous ||
				len(thisUnionType.Variants) != len(otherUnionType.Variants) {
				return false
			}
			for i, variant := range thisUnionType.Variants {
				if !checkSameSignatureIgnoringReceiverTypesHelper(
					this, other, variant.AsType(), otherUnionType.Variants[i].AsType()) {
					return false
				}
			}
			return true
		}
	}
	return false
}

func (ty *FunctionType) CheckSameSignatureIgnoringReceiverTypes(other *FunctionType, span token.Span) error {
	if len(ty.Params) != len(other.Params) {
		return errors.Errorf("%s: parameter count does not match: %d != %d", span, len(ty.Params), len(other.Params))
	}
	if !checkSameSignatureIgnoringReceiverTypesHelper(ty, other, ty.Result, other.Result) {
		return errors.Errorf("%s: result types do not match: %s != %s", span, ty.Result, other.Result)
	}
	for i, param := range ty.Params {
		otherParam := other.Params[i]
		if !checkSameSignatureIgnoringReceiverTypesHelper(ty, other, param.Type, otherParam.Type) {
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

func (ty FunctionType) GenericBase() (GenericType, bool) {
	if ty.genericBase == nil {
		return nil, false
	}
	return ty.genericBase, true
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

func (ty FunctionType) CallParams() []ParamOrField {
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

type VariableType struct {
	Type            Type
	IsFunctionParam bool
	IsMutable       bool
	Span            token.Span
}

func (self VariableType) String() string {
	mutable := ""
	if self.IsMutable {
		mutable = "\n    (mutable)"
	}
	functionParam := ""
	if self.IsFunctionParam {
		functionParam = "\n    (function parameter)"
	}
	return fmt.Sprintf("VariableType%s%s\n%s", mutable, functionParam, base.Indent(self.Type, 1))
}

func (self VariableType) Id() TypeId {
	return self.Type.Id()
}

func (self VariableType) IsAssignableFrom(other Type) bool {
	return false
}

type TypeAndSpan struct {
	Type Type
	Span token.Span
}

type typeScope struct {
	types     map[string]TypeAndSpan
	variables map[string]VariableType
	parent    *typeScope
}

func newTypeScope(parent *typeScope) *typeScope {
	return &typeScope{
		types:     make(map[string]TypeAndSpan),
		variables: make(map[string]VariableType),
		parent:    parent,
	}
}

func (te *typeScope) lookupType(name string) (Type, bool) {
	ty, found := te.types[name]
	if !found && te.parent != nil {
		return te.parent.lookupType(name)
	}
	return ty.Type, found
}

func (te *typeScope) lookupVariable(name ast.Ident) (Type, *VariableType, bool) {
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
	if existing, found := te.types[name]; found {
		return errors.Errorf("%s: type %q already declared here: %s", span, name, existing.Span)
	}
	te.types[name] = TypeAndSpan{Type: ty, Span: span}
	return nil
}

func (te *typeScope) declareVariable(name string, ty VariableType) error {
	if err := te.declareType(name, ty.Type, ty.Span); err != nil {
		return err
	}
	te.variables[name] = ty
	return nil
}

type Symbol struct {
	Name  string
	Scope *SymbolScope
}

func (self *Symbol) FQN() string {
	scopeFQN := self.Scope.FQN()
	if scopeFQN != "" {
		if self.Name != "" {
			return fmt.Sprintf("%s::%s", scopeFQN, self.Name)
		}
		return scopeFQN
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
	case *ast.NamedUnionTypeDeclaration:
		name = node.Name.String()
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
	// The type parameter a trait bound belongs to.
	traitBoundsTypeParam map[ast.Node]*TypeParam
	Main                 *FunctionType
	Panic                *FunctionType
	BuiltIns             BuiltIns
}

func (m *TypeInfo) LookupTraitBoundTypeParam(expr ast.Node) (*TypeParam, bool) {
	typeParam, found := m.traitBoundsTypeParam[expr]
	return typeParam, found
}

func (m *TypeInfo) MustLookupTraitBoundTypeParam(expr ast.Node) *TypeParam {
	typeParam, found := m.LookupTraitBoundTypeParam(expr)
	if !found {
		panic(fmt.Sprintf("trait bound type param not found for key %q", expr))
	}
	return typeParam
}

func (m *TypeInfo) SetTraitBoundTypeParam(expr ast.Node, typeParam *TypeParam) {
	m.traitBoundsTypeParam[expr] = typeParam
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
		panic(errors.Errorf("%s: type not found for node %s: %s", node.Span(), node.Id(), node))
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

type inferGenericScope struct {
	parent *inferGenericScope
	types  map[TypeId]Type
}

func newInferGenericScope(parent *inferGenericScope) *inferGenericScope {
	return &inferGenericScope{
		parent: parent,
		types:  make(map[TypeId]Type),
	}
}

func (self *inferGenericScope) lookupTypeParam(param TypeParam) (Type, bool) {
	ty, found := self.types[param.Id()]
	if !found && self.parent != nil {
		return self.parent.lookupTypeParam(param)
	}
	return ty, found
}

func (self *inferGenericScope) declareTypeParam(param *TypeParam, ty Type) bool {
	existing := self.types[param.Id()]
	if existing == nil {
		self.types[param.Id()] = ty
		return true
	}
	return existing.Id() == ty.Id()
}

type checkingMode int

const (
	defaultMode           checkingMode = 0
	insideTraitOrImplMode checkingMode = 1
)

type memoizedScopes struct {
	genericScope *genericScope
	typeScope    *typeScope
	symbolScope  *SymbolScope
}

type typeChecker struct {
	ast.DefaultVisitor
	typeInfo          *TypeInfo
	typeScope         *typeScope
	symbolScope       *SymbolScope
	genericScope      *genericScope
	inferGenericScope *inferGenericScope
	genericsResolver  *GenericsResolver
	loopDepth         int
	checkingMode      checkingMode
	typeCreator       *TypeCreator
	contextualType    Type
	memoizedScopes    map[ast.NodeId]*memoizedScopes
}

func (tc *typeChecker) newTypeBase() typeBase {
	return tc.typeCreator.newTypeBase()
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

func (tc *typeChecker) enterInferGenericScope() {
	tc.inferGenericScope = newInferGenericScope(tc.inferGenericScope)
}

func (tc *typeChecker) exitInferGenericScope() {
	tc.inferGenericScope = tc.inferGenericScope.parent
}

func (tc *typeChecker) memoizeScopes(node ast.Node) {
	if _, ok := tc.memoizedScopes[node.Id()]; ok {
		panic(fmt.Sprintf("scopes already memoized for node %s", node.Id()))
	}
	tc.memoizedScopes[node.Id()] = &memoizedScopes{
		genericScope: tc.genericScope,
		typeScope:    tc.typeScope,
		symbolScope:  tc.symbolScope,
	}
}

func (tc *typeChecker) useMemoizedScopes(node ast.Node) func() {
	memoizedScopes, ok := tc.memoizedScopes[node.Id()]
	if !ok {
		panic(fmt.Sprintf("scopes not memoized for node %s", node.Id()))
	}
	oldGenericScope := tc.genericScope
	oldTypeScope := tc.typeScope
	oldSymbolScope := tc.symbolScope
	tc.genericScope = memoizedScopes.genericScope
	tc.typeScope = memoizedScopes.typeScope
	tc.symbolScope = memoizedScopes.symbolScope
	return func() {
		tc.genericScope = oldGenericScope
		tc.typeScope = oldTypeScope
		tc.symbolScope = oldSymbolScope
	}
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
	declareSymbol(key, name, tc.symbolScope, tc.typeInfo)
}

func declareSymbol(key IsId, name string, symbolScope *SymbolScope, typeInfo *TypeInfo) {
	keyString := key.String()
	symbol := &Symbol{Name: name, Scope: symbolScope}
	if _, found := symbolScope.Symbols[keyString]; found {
		panic(fmt.Sprintf("symbol already declared: %q", symbol.Name))
	}
	symbolScope.Symbols[keyString] = symbol
	typeInfo.DeclareSymbol(key, symbol)
}

func (tc *typeChecker) lookupTypeOfNode(node ast.Type) (Type, error) {
	switch node := node.(type) {
	case *ast.FunctionType:
		params := make([]ParamOrField, len(node.Params))
		for i, astParam := range node.Params {
			argType, err := tc.lookupTypeOfNode(astParam.Type)
			if err != nil {
				return nil, err
			}
			// todo: `ast.functionType` does not include a name. Do we want to be able to include a
			//       name?
			params[i] = ParamOrField{Type: argType, Name: "", Mutable: astParam.Mutable}
		}
		result, err := tc.lookupTypeOfNode(node.Result)
		if err != nil {
			return nil, err
		}
		res := FunctionType{typeBase: tc.newTypeBase(), Params: params, Result: result}
		return &res, nil
	case *ast.TupleType:
		values := make([]Type, len(node.Values))
		for i, astValue := range node.Values {
			valueType, err := tc.lookupTypeOfNode(astValue)
			if err != nil {
				return nil, err
			}
			values[i] = valueType
		}
		res := TupleType{typeBase: tc.newTypeBase(), Values: values}
		return &res, nil
	case *ast.TypeParam:
		res, found := tc.genericScope.lookupTypeParam(node.TypeName())
		if !found {
			return nil, errors.Errorf("undefined type parameter: %s", node.TypeName())
		}
		return res, nil
	case *ast.UnionType:
		unionType := &UnionType{typeBase: tc.newTypeBase(), IsAnonymous: true}
		variants := make([]UnionVariant, len(node.Variants))
		for i, astVariant := range node.Variants {
			var variant UnionVariant
			switch astVariant.Kind {
			case ast.UnionVariantKindType:
				variantType, err := tc.lookupTypeOfNode(astVariant.Type)
				if err != nil {
					return nil, err
				}
				variant = UnionVariant{Kind: UnionVariantKindType, Type: variantType}
			case ast.UnionVariantKindNamed:
				return nil, errors.Errorf(
					"%s: named union variants are not supported in anonymous union types", astVariant.Type.Span())
			default:
				panic(fmt.Sprintf("unexpected variant kind: %d", astVariant.Kind))
			}
			variants[i] = variant
		}
		unionType.Variants = variants
		// Make sure that anonymous union types with the same variant types in the same order
		// map to the same type.
		unionType = tc.genericsResolver.FindOrSetAnonUnionType(unionType)
		return unionType, nil
	case *ast.SimpleType:
		baseType, found := tc.typeScope.lookupType(node.TypeName())
		if found {
			genericType, ok := baseType.(GenericType)
			if ok {
				typeArgs := make([]Type, len(node.TypeArgs))
				for i, typeArg := range node.TypeArgs {
					typeArg, err := tc.lookupTypeOfNode(typeArg)
					if err != nil {
						return nil, err
					}
					typeArgs[i] = typeArg
				}
				return tc.resolveGenericType(genericType, node.TypeArgs, node.Span())
			}
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
	tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Str)
	return nil
}

func (tc *typeChecker) VisitCharLiteralExpression(expr *ast.CharLiteralExpression) error {
	tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Char)
	return nil
}

func (tc *typeChecker) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	if expr.IsUInt64 {
		tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.U64)
		return nil
	}
	v := expr.Int64
	var ty Type
	switch tc.contextualType {
	case tc.typeInfo.BuiltIns.I8:
		if v < -128 || v > 127 {
			return errors.Errorf("%s: value %d out of range for Int8Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.I8
	case tc.typeInfo.BuiltIns.I16:
		if v < -32768 || v > 32767 {
			return errors.Errorf("%s: value %d out of range for Int16Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.I16
	case tc.typeInfo.BuiltIns.I32:
		if v < -2147483648 || v > 2147483647 {
			return errors.Errorf("%s: value %d out of range for Int32Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.I32
	case tc.typeInfo.BuiltIns.U8:
		if v < 0 || v > 255 {
			return errors.Errorf("%s: value %d out of range for UInt8Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.U8
	case tc.typeInfo.BuiltIns.U16:
		if v < 0 || v > 65535 {
			return errors.Errorf("%s: value %d out of range for UInt16Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.U16
	case tc.typeInfo.BuiltIns.U32:
		if v < 0 || v > 4294967295 {
			return errors.Errorf("%s: value %d out of range for UInt32Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.U32
	case tc.typeInfo.BuiltIns.U64:
		if v < 0 {
			return errors.Errorf("%s: value %d out of range for UInt64Type", expr.Span(), v)
		}
		ty = tc.typeInfo.BuiltIns.U64
	default:
		ty = tc.typeInfo.BuiltIns.I64
	}
	tc.typeInfo.Set(expr, ty)
	return nil
}

func (tc *typeChecker) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Bool)
	return nil
}

func (tc *typeChecker) VisitTupleLiteralExpression(expr *ast.TupleLiteralExpression, w ast.Walker) error {
	if err := w.WalkTupleLiteralExpression(expr); err != nil {
		return err
	}
	values := make([]Type, len(expr.Values))
	for i, astValue := range expr.Values {
		valueType := tc.typeInfo.MustLookup(astValue)
		values[i] = valueType
	}
	tupleType := &TupleType{typeBase: tc.newTypeBase(), Values: values}
	// todo: We should generate useful, short symbols. We cannot use `tupleType.String()`
	//       here because it generates a monstrosity of a string.
	symbol := ""
	for _, value := range values {
		if len(symbol) > 0 {
			symbol += "$"
		}
		symbol += value.Id().String()
	}
	tc.declareSymbol(tupleType.Id(), symbol)
	tc.typeInfo.Set(expr, tupleType)
	return nil
}

func (tc *typeChecker) VisitArrayLiteralExpression(expr *ast.ArrayLiteralExpression, w ast.Walker) error {
	if err := w.WalkArrayLiteralExpression(expr); err != nil {
		return err
	}
	elementType := tc.typeInfo.MustLookup(expr.Values[0])
	arrayType := tc.genericsResolver.ResolveTypeArgs(
		tc.typeInfo.BuiltIns.InternalArray, tc.typeInfo.BuiltIns.InternalArray.typeParams, []Type{elementType})
	tc.typeInfo.Set(expr, arrayType)
	return nil
}

func (tc *typeChecker) resolveGenericType(ty GenericType, astTypeArgs []ast.Type, span token.Span) (Type, error) {
	if len(astTypeArgs) == 0 {
		return tc.inferGenericType(ty, span)
	}
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
		typeParam := typeParams[i]
		if typeParam.TraitBound != nil {
			if !typeParam.TraitBound.IsAssignableFrom(typeArg) {
				return nil, errors.Errorf(
					"%s: type argument %q does not satisfy trait bound %q", span, typeArg, typeParam.TraitBound)
			}
		}
		typeArgs[i] = typeArg
	}
	return tc.genericsResolver.ResolveTypeArgs(ty, typeParams, typeArgs), nil
}

func (tc *typeChecker) inferGenericType(ty GenericType, span token.Span) (Type, error) {
	typeParams := ty.TypeParams()
	typeArgs := append([]Type{}, ty.TypeArgs()...)
	foundTypeParam := false
	for i, typeParam := range typeParams {
		ty, found := tc.inferGenericScope.lookupTypeParam(typeParam)
		if !found {
			continue
		}
		foundTypeParam = true
		typeArgs[i] = ty
		if typeParam.TraitBound != nil {
			if !typeParam.TraitBound.IsAssignableFrom(ty) {
				return nil, errors.Errorf(
					"%s: inferred type %q does not satisfy trait bound %q", span, ty, typeParam.TraitBound)
			}
		}
	}
	if foundTypeParam {
		return tc.genericsResolver.ResolveTypeArgs(ty, typeParams, typeArgs), nil
	} else {
		return ty, nil
	}
}

func (tc *typeChecker) VisitIdentExpression(expr *ast.IdentExpression) error {
	ty, found := tc.typeScope.lookupType(expr.Ident.String())
	if !found {
		return errors.Errorf("%s: type not found for identifier %s", expr.Span(), expr.Ident)
	}
	if genericType, ok := ty.(GenericType); ok {
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
	if err := w.WalkNode(expr.Lhs); err != nil {
		return err
	}
	lhs := tc.typeInfo.MustLookup(expr.Lhs)
	tc.contextualType = lhs
	if err := w.WalkNode(expr.Rhs); err != nil {
		return err
	}
	rhs := tc.typeInfo.MustLookup(expr.Rhs)
	switch expr.Op {
	case ast.OpAdd, ast.OpSubtract, ast.OpMultiply, ast.OpDivide, ast.OpModulo, ast.OpBitwiseAnd, ast.OpBitwiseOr, ast.OpBitwiseXor:
		switch lhs.(type) {
		case IntType, *RawPtr:
		default:
			return errors.Errorf(
				"%s: lhs of arithmetic expression must be an integer type, got %s", expr.Span(), lhs)
		}
		if !lhs.IsAssignableFrom(rhs) {
			return errors.Errorf(
				"%s: rhs of arithmetic expression must be assignable to lhs, expected %q got %q", expr.Span(), lhs, rhs)
		}
		tc.typeInfo.Set(expr, lhs)
	case ast.OpBitwiseShiftLeft, ast.OpBitwiseShiftRight:
		if _, ok := lhs.(IntType); !ok {
			return errors.Errorf(
				"%s: lhs of bitwise shift expression must be an integer type, got %s", expr.Span(), lhs)
		}
		if _, ok := rhs.(IntType); !ok {
			return errors.Errorf(
				"%s: rhs of bitwise shift expression must be an integer type, got %s", expr.Span(), rhs)
		}
		tc.typeInfo.Set(expr, lhs)
	case ast.OpEqual, ast.OpNotEqual, ast.OpGreaterThan, ast.OpGreaterThanOrEqual, ast.OpLessThan, ast.OpLessThanOrEqual:
		switch lhs.(type) {
		case IntType, *RawPtr, *CharType:
		case *BoolType:
			if expr.Op != ast.OpEqual && expr.Op != ast.OpNotEqual {
				return errors.Errorf("%s: only == and != are supported for bool types", expr.Span())
			}
		default:
			return errors.Errorf("%s: lhs of comparison expression must be an int, char, or bool type, got %s", expr.Span(), lhs)
		}
		if !lhs.IsAssignableFrom(rhs) {
			return errors.Errorf("%s: rhs of comparison expression must be assignable to lhs, expected %q got %q", expr.Span(), lhs, rhs)
		}
		tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Bool)
	case ast.OpAnd, ast.OpOr:
		if lhs != tc.typeInfo.BuiltIns.Bool {
			return errors.Errorf("%s: lhs of logical expression must be of type BoolType, got %s", expr.Span(), lhs)
		}
		if rhs != tc.typeInfo.BuiltIns.Bool {
			return errors.Errorf("%s: rhs of logical expression must be of type BoolType, got %s", expr.Span(), rhs)
		}
		tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Bool)
	default:
		return errors.Errorf("%s: unsupported binary operator: %s", expr.Span(), expr.Op)
	}
	return nil
}

func (tc *typeChecker) VisitUnaryExpression(expr *ast.UnaryExpression, w ast.Walker) error {
	if err := w.WalkUnaryExpression(expr); err != nil {
		return err
	}
	valueType := tc.typeInfo.MustLookup(expr.Value)
	switch expr.Op {
	case ast.OpNot:
		if valueType != tc.typeInfo.BuiltIns.Bool {
			return errors.Errorf(
				"%s: operand of logical not expression must be of type BoolType, got %s", expr.Span(), valueType)
		}
		tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.Bool)
	case ast.OpBitwiseNot:
		if _, ok := valueType.(IntType); !ok {
			return errors.Errorf(
				"%s: operand of bitwise not expression must be of type IntType, got %s", expr.Span(), valueType)
		}
		tc.typeInfo.Set(expr, valueType)

	default:
		return errors.Errorf("%s: unsupported unary operator: %s", expr.Span(), expr.Op)
	}
	return nil
}

func (tc *typeChecker) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := tc.VisitNode(expr.Callee, w); err != nil {
		return err
	}
	calleeType, ok := tc.typeInfo.MustLookup(expr.Callee).(CallableType)
	if !ok {
		return errors.Errorf("%s: callee %q is not a callable type", expr.Span(), calleeType)
	}
	if namedUnionVariant, ok := calleeType.(*NamedUnionVariantConstructor); ok {
		if namedUnionVariant.Type.IsUnitVariant() {
			return errors.Errorf("%s: unit variant %q cannot be called", expr.Span(), namedUnionVariant.Type.Name)
		}
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
	argTypes := make([]Type, len(expr.Args))
	for i, arg := range expr.Args {
		var paramIndex = i
		if arg.Name != "" {
			paramIndex = slices.IndexFunc(params, func(p ParamOrField) bool { return p.Name == arg.Name })
		}
		if paramIndex < 0 {
			return errors.Errorf("%s: parameter %q not found in callee type %s", arg.Span, arg.Name, calleeType)
		}
		if slices.Contains(seenParamIndexes, paramIndex) {
			return errors.Errorf("%s: parameter %q is already assigned", arg.Span, arg.Name)
		}
		seenParamIndexes = append(seenParamIndexes, paramIndex)
		param := params[paramIndex]
		tc.contextualType = param.Type
		if err := tc.VisitNode(arg.Value, w); err != nil {
			return err
		}
		argType := tc.typeInfo.MustLookup(arg.Value)
		argTypes[paramIndex] = argType
	}
	// Infer generic types if needed.
	if _, ok := calleeType.(GenericType); ok {
		tc.enterInferGenericScope()
		defer tc.exitInferGenericScope()
		hasTypeParamArgs := false
		for i, param := range params {
			if typeParam, ok := param.Type.(*TypeParam); ok {
				hasTypeParamArgs = true
				if !tc.inferGenericScope.declareTypeParam(typeParam, argTypes[i]) {
					return errors.Errorf(
						"%s: type parameter %q already declared with a different type", expr.Span(), typeParam)
				}
			}
		}
		if hasTypeParamArgs {
			if err := tc.VisitNode(expr.Callee, w); err != nil {
				return err
			}
			calleeType = tc.typeInfo.MustLookup(expr.Callee).(CallableType)
			params = calleeType.CallParams()
			if funcType, ok := calleeType.(*FunctionType); ok {
				if funcType.IsMethod() && !funcType.IsStaticMethod() {
					params = params[1:]
				}
			}
			result = calleeType.CallResult()
			tc.typeInfo.Set(expr.Callee, calleeType)
		}
	}
	for i, param := range params {
		argType := argTypes[i]
		if !param.Type.IsAssignableFrom(argType) {
			return errors.Errorf(
				"%s: expected argument %d to be of type %s, got %s", expr.Span(), i, param.Type, argType)
		}
	}
	tc.typeInfo.Set(expr, result)
	return nil
}

func (tc *typeChecker) VisitIndexExpression(expr *ast.IndexExpression, w ast.Walker) error {
	if err := w.WalkIndexExpression(expr); err != nil {
		return err
	}
	indexType, ok := tc.typeInfo.MustLookup(expr.Index).(IntType)
	if !ok {
		return errors.Errorf("%s: index must be of type IntType, got %s", expr.Index.Span(), indexType)
	}
	ty, ok := tc.typeInfo.MustLookup(expr.Target).(*StructType)
	if !ok || !tc.typeInfo.BuiltIns.IsArrayType(ty) {
		return errors.Errorf("%s: target must be the array type, got %s", expr.Target.Span(), ty)
	}
	tc.typeInfo.Set(expr, tc.typeInfo.BuiltIns.GetArrayElementType(ty))
	return nil
}

func (tc *typeChecker) VisitMemberExpression(expr *ast.MemberExpression, w ast.Walker) error {
	if err := w.WalkMemberExpression(expr); err != nil {
		return err
	}
	ty := tc.typeInfo.MustLookup(expr.Target)
	var memberType Type
	if expr.Field.IsIndex() {
		tupleType, ok := ty.(*TupleType)
		if !ok {
			return errors.Errorf("%s: type %q is not a tuple type", expr.Span(), ty)
		}
		fieldIndex := expr.Field.AsIndex()
		if fieldIndex < 0 || fieldIndex >= len(tupleType.Values) {
			return errors.Errorf("%s: index %d out of bounds for tuple type %q", expr.Span(), fieldIndex, ty)
		}
		memberType = tupleType.Values[fieldIndex]
	} else {
		switch ty := ty.(type) {
		case *StructType:
			memberType_, found := ty.FindMember(expr.Field, expr.Span())
			if !found {
				structSymbol := tc.typeInfo.MustLookupSymbol(ty.Id())
				return errors.Errorf("%s: member %q not found in struct type %q", expr.Span(), expr.Field, structSymbol.Name)
			}
			memberType = memberType_
		case ImplementableType:
			method, found := ty.FindMethod(expr.Field.AsIdent())
			if !found {
				symbol := tc.typeInfo.MustLookupSymbol(ty.Id())
				return errors.Errorf("%s: method %q not found in implementable type %q", expr.Span(), expr.Field, symbol.Name)
			}
			memberType = method
		case *UnionType:
			variant, found := ty.FindNamedVariant(expr.Field.AsIdent())
			if !found {
				unionSymbol := tc.typeInfo.MustLookupSymbol(ty.Id())
				return errors.Errorf("%s: variant %q not found in union type %q", expr.Span(), expr.Field, unionSymbol.Name)
			}
			if variant.IsUnitVariant() {
				memberType = variant
			} else {
				memberType = variant.Constructor
			}
		case *TypeParam:
			if ty.TraitBound == nil {
				return errors.Errorf("%s: type parameter %q does not have a trait bound", expr.Span(), ty.Name)
			}
			traitType := ty.TraitBound
			method, found := traitType.FindMethod(expr.Field.AsIdent(), expr.Span())
			if !found {
				return errors.Errorf("%s: method %q not found in trait type %q", expr.Span(), expr.Field, traitType)
			}
			memberType = method
			tc.typeInfo.Set(expr, ty)
			tc.typeInfo.SetTraitBoundTypeParam(expr, ty)
		default:
			panic(fmt.Sprintf("unexpected type %T", ty))
		}
	}
	if genericType, ok := memberType.(GenericType); ok {
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
	var blockType Type = tc.typeInfo.BuiltIns.None
	if len(expr.Nodes) > 0 {
		blockType = tc.typeInfo.MustLookup(expr.Nodes[len(expr.Nodes)-1])
		for i, node := range expr.Nodes {
			if returnStmt, ok := node.(*ast.ReturnStatement); ok {
				if i != len(expr.Nodes)-1 {
					return errors.Errorf("%s: return statement must be the last statement in a block", returnStmt.Span())
				}
				blockType = tc.typeInfo.MustLookup(returnStmt.Value)
			}
		}
	}
	tc.typeInfo.Set(expr, blockType)
	return nil
}

func (tc *typeChecker) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	contextualType := tc.contextualType
	if err := w.WalkIfExpression(expr); err != nil {
		return err
	}
	condType := tc.typeInfo.MustLookup(expr.Condition)
	if _, ok := tc.typeInfo.MustLookup(expr.Condition).(*BoolType); !ok {
		return errors.Errorf("%s: the condition of an if expression must be a boolean type, got: %s", expr.Condition.Span(), condType)
	}
	branchTypes := []Type{tc.typeInfo.MustLookup(expr.TrueBody)}
	if expr.FalseBody == nil {
		branchTypes = append(branchTypes, tc.typeInfo.BuiltIns.None)
	} else {
		branchTypes = append(branchTypes, tc.typeInfo.MustLookup(expr.FalseBody))
	}
	ty, err := tc.combineTypesIfNeeded(branchTypes, contextualType, expr.Span())
	if err != nil {
		return err
	}
	tc.typeInfo.Set(expr, ty)
	return nil
}

func (tc *typeChecker) VisitMatchExpression(expr *ast.MatchExpression, w ast.Walker) error {
	contextualType := tc.contextualType
	if err := tc.VisitNode(expr.Expression, w); err != nil {
		return err
	}
	exprType := tc.typeInfo.MustLookup(expr.Expression)
	for _, arm := range expr.Arms {
		pattern := arm.Pattern
		var patternType Type
		var aliasType Type
		switch pattern := pattern.(type) {
		case *ast.UnionTypePattern:
			if pattern.NamedVariant != "" {
				if pattern.Type != nil {
					ty, err := tc.lookupTypeOfNode(pattern.Type)
					if err != nil {
						return err
					}
					if ty.Id() != exprType.Id() {
						return errors.Errorf("%s: expected union variant of type %s, got %s", pattern.Span(), exprType, ty)
					}
				}
				unionType, ok := exprType.(*UnionType)
				if !ok {
					return errors.Errorf("%s: expected union type, got %s", pattern.Span(), aliasType)
				}
				variantType, ok := unionType.FindNamedVariant(pattern.NamedVariant)
				if !ok {
					return errors.Errorf(
						"%s: variant %q not found in union type %q", pattern.Span(), pattern.NamedVariant, unionType)
				}
				patternType = variantType
			} else {
				patternType_, err := tc.lookupTypeOfNode(pattern.Type)
				if err != nil {
					return err
				}
				patternType = patternType_
				tc.typeInfo.Set(pattern.Type, patternType)
			}
			tc.typeInfo.Set(pattern, patternType)
			if arm.Alias != nil {
				if namedVariantType, ok := patternType.(*NamedUnionVariant); ok {
					aliasType = namedVariantType.Type
				} else {
					aliasType = patternType
				}
			}
		case *ast.IntPattern:
			tc.contextualType = exprType
			if err := tc.VisitIntLiteralExpression(&pattern.Value); err != nil {
				return err
			}
			patternType = tc.typeInfo.MustLookup(&pattern.Value)
			aliasType = patternType
		case *ast.IntRangePattern:
			tc.contextualType = exprType
			if err := tc.VisitIntLiteralExpression(&pattern.From); err != nil {
				return err
			}
			tc.contextualType = exprType
			if err := tc.VisitIntLiteralExpression(&pattern.To); err != nil {
				return err
			}
			patternType = tc.typeInfo.MustLookup(&pattern.From)
			aliasType = patternType
		case *ast.StrPattern:
			if err := tc.VisitStringLiteralExpression(&pattern.Value); err != nil {
				return err
			}
			patternType = tc.typeInfo.MustLookup(&pattern.Value)
			aliasType = patternType
		case *ast.CharPattern:
			if err := tc.VisitCharLiteralExpression(&pattern.Value); err != nil {
				return err
			}
			patternType = tc.typeInfo.MustLookup(&pattern.Value)
			aliasType = patternType
		case *ast.CharRangePattern:
			if err := tc.VisitCharLiteralExpression(&pattern.From); err != nil {
				return err
			}
			if err := tc.VisitCharLiteralExpression(&pattern.To); err != nil {
				return err
			}
			patternType = tc.typeInfo.MustLookup(&pattern.From)
			aliasType = patternType
		case *ast.WildcardPattern:
			patternType = exprType
			aliasType = exprType
		default:
			return errors.Errorf("%s: pattern of type %T not implemented", arm.Span(), pattern)
		}
		if !exprType.IsAssignableFrom(patternType) {
			return errors.Errorf(
				"%s: expected match expression type %s to be assignable to pattern type %s", arm.Span(), exprType, patternType)
		}
		if arm.Alias != nil {
			tc.enterScope(arm.Body)
			defer tc.exitScope()
			if err := tc.typeScope.declareVariable(
				arm.Alias.Ident.String(),
				VariableType{Type: aliasType, Span: arm.Span()}); err != nil {
				return err
			}
		}
		if err := tc.VisitNode(arm.Body, w); err != nil {
			return err
		}
	}
	// Build the resulting type that is either a concrete type if all match arms
	// have the same type or a union type of all different match arm types.
	armTypes := make([]Type, len(expr.Arms))
	for i, arm := range expr.Arms {
		armTypes[i] = tc.typeInfo.MustLookup(arm.Body)
	}
	ty, err := tc.combineTypesIfNeeded(armTypes, contextualType, expr.Span())
	if err != nil {
		return err
	}
	tc.typeInfo.Set(expr, ty)
	return CheckMatch(expr, tc.typeInfo)
}

func (tc *typeChecker) combineTypesIfNeeded(types_ []Type, contextualType Type, span token.Span) (Type, error) {
	// First, expand all anonymous unions. We can't have nested anonymous unions.
	types := []Type{}
	for _, ty := range types_ {
		if unionType, ok := ty.(*UnionType); ok && unionType.IsAnonymous {
			for _, variant := range unionType.Variants {
				if variant.Kind == UnionVariantKindType {
					types = append(types, variant.Type)
				} else {
					panic(fmt.Sprintf("unexpected union variant kind: %d", variant.Kind))
				}
			}
		} else {
			types = append(types, ty)
		}
	}
	// Now remove all `NeverType`.
	for i := 0; i < len(types); i++ {
		if _, ok := types[i].(*NeverType); ok {
			types = append(types[:i], types[i+1:]...)
			i--
		}
	}
	if contextualType != nil {
		for _, ty := range types {
			if !contextualType.IsAssignableFrom(ty) {
				return nil, errors.Errorf(
					"%s: expected type %s to be assignable to contextual type %s", span, ty, contextualType)
			}
		}
		return contextualType, nil
	}
	res := types[0]
	resultIsUnion := false
	for _, ty := range types[1:] {
		if namedUnionVariant, ok := ty.(*NamedUnionVariant); ok {
			ty = namedUnionVariant.UnionType
		}
		if res.Id() != ty.Id() {
			if resultIsUnion {
				unionTy := res.(*UnionType)
				alreadyPartOfUnion := false
				for _, variant := range unionTy.Variants {
					if variant.Type.IsAssignableFrom(ty) {
						alreadyPartOfUnion = true
						break
					}
				}
				if !alreadyPartOfUnion {
					unionTy.Variants = append(unionTy.Variants, UnionVariant{Kind: UnionVariantKindType, Type: ty})
					res = unionTy
				}
			} else {
				res = &UnionType{typeBase: tc.newTypeBase(), IsAnonymous: true, Variants: []UnionVariant{
					{Kind: UnionVariantKindType, Type: res},
					{Kind: UnionVariantKindType, Type: ty},
				}}
				resultIsUnion = true
			}
		}
	}
	if unionType, ok := res.(*UnionType); ok {
		res = tc.genericsResolver.FindOrSetAnonUnionType(unionType)
	}
	return res, nil
}

func (tc *typeChecker) resolveTypeParams(genericType GenericType, astParams []ast.TypeParam) ([]TypeParam, error) {
	typeParams := make([]TypeParam, len(astParams))
	for i, astParam := range astParams {
		var traitBound *TraitType
		if astParam.TraitBound != nil {
			traitBoundType, err := tc.lookupTypeOfNode(astParam.TraitBound)
			if err != nil {
				return nil, err
			}
			traitBound_, ok := traitBoundType.(*TraitType)
			if !ok {
				return nil, errors.Errorf("%s: expected trait type, got %s", astParam.TraitBound.Span(), traitBoundType)
			}
			traitBound = traitBound_
		}
		typeParam := TypeParam{typeBase: tc.newTypeBase(), GenericType: genericType, TraitBound: traitBound, Name: astParam.Name, Index: i}
		typeParams[i] = typeParam
		if err := tc.genericScope.declareTypeParam(typeParam.Name.String(), &typeParam, astParams[i].Span()); err != nil {
			return nil, err
		}
	}
	return typeParams, nil
}

func (tc *typeChecker) resolveFunctionParamsAndResult(
	astParams []ast.FunctionParam, astResult ast.Type) (params []ParamOrField, result Type, err error) {
	params = make([]ParamOrField, len(astParams))
	for i, arg := range astParams {
		argType, err := tc.lookupTypeOfNode(arg.Type)
		if err != nil {
			return nil, nil, errors.Wrapf(err, "%s: type %s not found for parameter %s", arg.Span, arg.Type, arg.Name)
		}
		params[i] = ParamOrField{Type: argType, Name: arg.Name, Mutable: arg.Mutable}
	}
	result, err = tc.lookupTypeOfNode(astResult)
	if err != nil {
		return nil, nil, errors.Wrapf(
			err, "%s: type %s not found for return type", astResult.Span(), astResult)
	}
	return params, result, nil
}

func (tc *typeChecker) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	return nil
}

func (tc *typeChecker) checkFunctionDeclaration(decl *ast.FunctionDeclaration) error {
	funcType := tc.typeInfo.MustLookup(decl).(*DeclaredType).Type.(*FunctionType)
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
			if _, ok := funcType.Result.(*NoneType); !ok {
				return errors.Errorf("%s: main function must return () (no return value)", decl.Span())
			}
			tc.typeInfo.Main = funcType
		}
		if decl.Name == "panic" {
			tc.typeInfo.Panic = funcType
		}
	}
	return nil
}

func (tc *typeChecker) VisitFunctionDefinition(fn *ast.FunctionDefinition, w ast.Walker) error {
	return nil
}

func isValueType(ty Type) bool {
	switch ty.(type) {
	case IntType, *BoolType, *CharType:
		return true
	}
	return false
}

func (tc *typeChecker) checkFunctionDefinitionStage1(fn *ast.FunctionDefinition) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	tc.memoizeScopes(fn)
	if err := tc.checkFunctionDeclaration(fn.Decl); err != nil {
		return err
	}
	return nil
}

func (tc *typeChecker) checkFunctionDefinitionStage2(fn *ast.FunctionDefinition, w ast.Walker) error {
	declaredType := tc.typeInfo.MustLookup(fn).(*DeclaredType)
	functionType := declaredType.Type.(*FunctionType)
	defer tc.useMemoizedScopes(fn)()
	tc.enterScope(fn)
	defer tc.exitScope()
	params := functionType.Params
	for i, astParam := range fn.Decl.Params {
		param := params[i]
		if param.Mutable && isValueType(param.Type) {
			return errors.Errorf("%s: value type %s cannot be mutable", astParam.Type.Span(), param.Type)
		}
		varType := VariableType{Type: param.Type, IsFunctionParam: true, IsMutable: param.Mutable, Span: astParam.Span}
		if err := tc.typeScope.declareVariable(string(param.Name), varType); err != nil {
			return err
		}
	}
	if err := tc.VisitBlockExpression(fn.Body, w); err != nil {
		return err
	}
	return nil
}

func (tc *typeChecker) VisitTraitDeclaration(trait *ast.TraitDeclaration, w ast.Walker) error {
	return nil
}

func (tc *typeChecker) checkTraitDeclarationStage1(forwardTrait *forwardTraitDecl) error {
	trait := forwardTrait.traitDecl
	traitType := tc.typeInfo.MustLookup(trait).(*DeclaredType).Type.(*TraitType)
	tc.enterScope(trait)
	defer tc.exitScope()
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	typeParams, err := tc.resolveTypeParams(traitType, trait.TypeParams)
	if err != nil {
		return err
	}
	traitType.typeParams = typeParams
	selfType := &TypeParam{
		typeBase: tc.newTypeBase(), GenericType: traitType, TraitBound: traitType, Name: "Self", Index: len(typeParams)}
	if err := tc.typeScope.declareType("Self", selfType, trait.Span()); err != nil {
		return err
	}
	tc.enterCheckingMode(insideTraitOrImplMode)
	defer tc.exitCheckingMode()
	decls, err := tc.forwardDeclare(trait.MethodDecls)
	if err != nil {
		return err
	}
	funcDecls := append([]*ast.FunctionDeclaration{}, decls.funcDecls...)
	defs, err := tc.forwardDeclare(trait.MethodDefs)
	forwardTrait.forwardDecls = defs
	if err != nil {
		return err
	}
	funcDefs := map[ast.NodeId]*ast.FunctionDefinition{}
	for _, def := range defs.funcDefs {
		funcDefs[def.Decl.Id()] = def
		funcDecls = append(funcDecls, def.Decl)
	}
	for _, decl := range funcDecls {
		tc.enterGenericScope()
		err := tc.checkFunctionDeclaration(decl)
		if err != nil {
			tc.exitGenericScope()
			return err
		}
		if def, ok := funcDefs[decl.Id()]; ok {
			tc.memoizeScopes(def)
		}
		tc.exitGenericScope()
	}
	tc.memoizeScopes(trait)
	for _, methodDecl := range funcDecls {
		methodType := tc.typeInfo.MustLookupDeclaredType(methodDecl).Type.(*FunctionType)
		methodType.Receiver = selfType
		method := &Method{Name: methodDecl.Name, Type: methodType}
		if _, ok := funcDefs[methodDecl.Id()]; ok {
			methodType.SelfTypeParam = selfType
		}
		traitType.Methods = append(traitType.Methods, method)
	}
	return nil
}

func (tc *typeChecker) checkTraitDeclarationStage2(trait *forwardTraitDecl, w ast.Walker) error {
	defer tc.useMemoizedScopes(trait.traitDecl)()
	return tc.checkForwardDeclsStage2(trait.forwardDecls, w)
}

func (tc *typeChecker) VisitImplDefinition(impl *ast.ImplDefinition, w ast.Walker) error {
	return nil
}

func (tc *typeChecker) checkImplDefinitionStage1(forwardImpl *forwardImplDef, w ast.Walker) error {
	impl := forwardImpl.implDef
	targetType_, found := tc.typeScope.lookupType(string(impl.Target))
	if !found {
		return errors.Errorf("%s: type %q not found for impl definition", impl.Span(), impl.Target)
	}
	targetType, ok := targetType_.(ImplementableType)
	if !ok {
		return errors.Errorf("%s: type %q is not a implementable type", impl.Span(), targetType_)
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
	if genericType, ok := targetType.(GenericType); ok && HasTypeParams(genericType) {
		tc.enterGenericScope()
		defer tc.exitGenericScope()
		for _, typeParam := range genericType.TypeParams() {
			if err := tc.genericScope.declareTypeParam(typeParam.Name.String(), &typeParam, impl.Span()); err != nil {
				return err
			}
		}
	}
	tc.memoizeScopes(impl)
	if traitType != nil && HasTypeParams(traitType) {
		traitType_, err := tc.resolveGenericType(traitType, impl.TraitTypeArgs, impl.Span())
		if err != nil {
			return err
		}
		traitType = traitType_.(*TraitType)
	}
	if err := tc.typeScope.declareType("Self", targetType, impl.Span()); err != nil {
		return err
	}
	forwardDecls, err := tc.forwardDeclare(impl.Methods)
	if err != nil {
		return err
	}
	forwardImpl.forwardDecls = forwardDecls
	if err := tc.checkForwardDeclsStage1(forwardDecls, w); err != nil {
		return err
	}
	unimplementedTraitMethods := map[ast.Ident]*Method{}
	if traitType != nil {
		for _, method := range traitType.Methods {
			unimplementedTraitMethods[method.Name] = method
		}
	}
	for _, methodDef := range impl.Methods {
		decl := methodDef.Decl
		typeDecl := tc.typeInfo.MustLookupDeclaredType(methodDef)
		methodType, ok := typeDecl.Type.(*FunctionType)
		if !ok {
			return errors.Errorf("%s: type is not a function type: %s", methodDef.Span(), typeDecl)
		}
		methodType.Receiver = targetType
		if traitType != nil {
			traitMethodType, ok := traitType.FindMethod(decl.Name, decl.Span())
			if !ok {
				traitSymbol := tc.typeInfo.MustLookupSymbol(traitType.Id())
				return errors.Errorf("%s: method %q not found in trait %q", decl.Span(), decl.Name, traitSymbol.Name)
			}
			if err := traitMethodType.CheckSameSignatureIgnoringReceiverTypes(methodType, decl.Span()); err != nil {
				structSymbol := tc.typeInfo.MustLookupSymbol(targetType.Id())
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
		method := &Method{Name: decl.Name, Type: methodType}
		if !targetType.addMethod(method) {
			targetSymbol := tc.typeInfo.MustLookupSymbol(targetType.Id())
			return errors.Errorf(
				"%s: name %q already exists in type %q", decl.Span(), decl.Name, targetSymbol.Name)
		}
	}
	if traitType != nil {
		for _, method := range unimplementedTraitMethods {
			if method.Type.SelfTypeParam != nil {
				if !targetType.addMethod(method) {
					targetSymbol := tc.typeInfo.MustLookupSymbol(targetType.Id())
					return errors.Errorf(
						"%s: name %q already exists in type %q", impl.Span(), method.Name, targetSymbol.Name)
				}
				delete(unimplementedTraitMethods, method.Name)
			}
		}
		if len(unimplementedTraitMethods) > 0 {
			missingTraitMethods := []string{}
			for _, method := range unimplementedTraitMethods {
				missingTraitMethods = append(missingTraitMethods, method.String())
			}
			structSymbol := tc.typeInfo.MustLookupSymbol(targetType.Id())
			traitSymbol := tc.typeInfo.MustLookupSymbol(traitType.Id())
			return errors.Errorf(
				"%s: impl %q does not implement all methods of trait %q: %s",
				impl.Span(),
				structSymbol.Name,
				traitSymbol.Name,
				strings.Join(missingTraitMethods, ", "),
			)
		}
		if !targetType.addTrait(traitType) {
			targetSymbol := tc.typeInfo.MustLookupSymbol(targetType.Id())
			return errors.Errorf(
				"%s: trait %q already implemented for type %q", impl.Span(), traitType, targetSymbol.Name)
		}
	}
	return nil
}

func (tc *typeChecker) checkImplDefinitionStage2(impl *forwardImplDef, w ast.Walker) error {
	defer tc.useMemoizedScopes(impl.implDef)()
	return tc.checkForwardDeclsStage2(impl.forwardDecls, w)
}

func (tc *typeChecker) VisitVariableDefinition(v *ast.VariableDefinition, w ast.Walker) error {
	var variableType Type
	if v.Type != nil {
		variableType_, err := tc.lookupTypeOfNode(v.Type)
		if err != nil {
			return err
		}
		variableType = variableType_
		tc.contextualType = variableType
	}
	if err := w.WalkNode(v.Value); err != nil {
		return err
	}
	valueType := tc.typeInfo.MustLookup(v.Value)
	if _, ok := valueType.(*NoneType); ok {
		return errors.Errorf("%s: variable %s must have a type that is not None", v.Span(), v.Name)
	}
	if variableType != nil {
		if !variableType.IsAssignableFrom(valueType) {
			return errors.Errorf(
				"%s: variable %q must be assignable to type %s, got %s", v.Span(), v.Name, variableType, valueType)
		}
	} else {
		variableType = valueType
	}
	varType := VariableType{Type: variableType, IsMutable: v.Mutable, Span: v.Span()}
	if err := tc.typeScope.declareVariable(string(v.Name), varType); err != nil {
		return err
	}
	tc.typeInfo.Set(v, &varType)
	return nil
}

func (tc *typeChecker) VisitAssignmentStatement(s *ast.AssignmentStatement, w ast.Walker) error {
	if err := tc.VisitNode(s.Target, w); err != nil {
		return err
	}
	varType, varInfo, ok := tc.typeScope.lookupVariable(s.Variable())
	tc.contextualType = varType
	if err := tc.VisitNode(s.Value, w); err != nil {
		return err
	}
	rhsType := tc.typeInfo.MustLookup(s.Value)
	if !ok {
		return errors.Errorf("%s: unknown variable %q", s.Span(), s.Variable())
	}
	if !varInfo.IsMutable {
		return errors.Errorf("%s: variable %q is not mutable", s.Span(), s.Variable())
	}
	if field, ok := s.IsMemberAssigment(); ok {
		structType, ok := varType.(*StructType)
		if !ok {
			return errors.Errorf("%s: variable %q is not a struct type", s.Span(), s.Variable())
		}
		field, found := structType.FindField(ast.MemberExpressionField(field))
		if !found {
			structSymbol := tc.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf("%s: field %q not found in struct type %q", s.Span(), field, structSymbol.Name)
		}
		if !field.Mutable {
			return errors.Errorf("%s: field %q is not mutable in struct: %s", s.Span(), field.Name, structType)
		}
		varType = field.Type
	} else if index, ok := s.IsIndexAssigment(); ok {
		arrayType, ok := varType.(*StructType)
		if !ok || !tc.typeInfo.BuiltIns.IsArrayType(arrayType) {
			return errors.Errorf("%s: variable %q is not of array type", s.Span(), s.Variable())
		}
		indexType, ok := tc.typeInfo.MustLookup(index.Index).(IntType)
		if !ok {
			return errors.Errorf("%s: index must be of type IntType, got %s", index.Span(), indexType)
		}
		varType = tc.typeInfo.BuiltIns.GetArrayElementType(arrayType)
	} else if s.IsDirectAssigment() {
	} else {
		panic("unexpected assignment type")
	}
	if !varType.IsAssignableFrom(rhsType) {
		return errors.Errorf(
			"%s: lhs and rhs of assignment statement must have the same type, got lhs: %s and rhs: %s", s.Span(), varType, rhsType)
	}
	tc.typeInfo.Set(s, tc.typeInfo.BuiltIns.None)
	return nil
}

func (tc *typeChecker) VisitLoopStatement(s *ast.LoopStatement, w ast.Walker) error {
	tc.typeInfo.Set(s, tc.typeInfo.BuiltIns.None)
	tc.enterLoop()
	defer tc.exitLoop()
	return w.WalkLoopStatement(s)
}

func (tc *typeChecker) VisitContinueStatement(s *ast.ContinueStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: continue statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, tc.typeInfo.BuiltIns.None)
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: break statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, tc.typeInfo.BuiltIns.None)
	return nil
}

func (tc *typeChecker) VisitReturnStatement(s *ast.ReturnStatement, w ast.Walker) error {
	if err := w.WalkReturnStatement(s); err != nil {
		return err
	}
	tc.typeInfo.Set(s, tc.typeInfo.BuiltIns.None)
	return nil
}

func (tc *typeChecker) VisitNamedUnionTypeDeclaration(decl *ast.NamedUnionTypeDeclaration) error {
	return nil
}

func (tc *typeChecker) checkNamedUnionTypeDeclaration(decl *ast.NamedUnionTypeDeclaration) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	unionType := tc.typeInfo.MustLookup(decl).(*DeclaredType).Type.(*UnionType)
	variants := make([]UnionVariant, len(decl.UnionType.Variants))
	for i, astVariant := range decl.UnionType.Variants {
		var variant UnionVariant
		switch astVariant.Kind {
		case ast.UnionVariantKindType:
			variantType, err := tc.lookupTypeOfNode(astVariant.Type)
			if err != nil {
				return err
			}
			variant = UnionVariant{Kind: UnionVariantKindType, Type: variantType}
		case ast.UnionVariantKindNamed:
			variantType_, err := tc.lookupTypeOfNode(astVariant.Named.Type)
			if err != nil {
				return err
			}
			variantType, ok := variantType_.(*TupleType)
			if !ok {
				return errors.Errorf("%s: expected tuple type, got %s", astVariant.Named.Type.Span(), variantType_)
			}
			tc.declareSymbol(variantType.Id(), astVariant.Named.Name.String())
			variant = UnionVariant{
				Kind: UnionVariantKindNamed,
				Named: NamedUnionVariant{
					typeBase:    tc.newTypeBase(),
					Name:        astVariant.Named.Name,
					Type:        variantType,
					UnionType:   unionType,
					Constructor: &NamedUnionVariantConstructor{typeBase: tc.newTypeBase()},
				}}
			variant.Named.Constructor.Type = &variant.Named
		default:
			panic(fmt.Sprintf("unexpected variant kind: %d", astVariant.Kind))
		}
		variants[i] = variant
	}
	unionType.Variants = variants
	typeParams, err := tc.resolveTypeParams(unionType, decl.TypeParams)
	if err != nil {
		return err
	}
	unionType.typeParams = typeParams
	// The declared union type will have the type parameters as its type arguments. This way
	// we don't have to distinguish between a type with type arguments set and one without type
	// arguments.
	unionType.typeArgs = make([]Type, len(typeParams))
	for i, typeParam := range typeParams {
		unionType.typeArgs[i] = typeParam
	}
	return nil
}

func (tc *typeChecker) VisitStructTypeDeclaration(decl *ast.StructTypeDeclaration) error {
	return nil
}

func (tc *typeChecker) checkStructTypeDeclaration(decl *ast.StructTypeDeclaration) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	structType := tc.typeInfo.MustLookup(decl).(*DeclaredType).Type.(*StructType)
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
	fields := []ParamOrField{}
	for _, field := range decl.Fields {
		fieldType, err := tc.lookupTypeOfNode(field.Type)
		if err != nil {
			return errors.Errorf("%s: type %q not found for field %q", field.Span, field.Type, field.Name)
		}
		fields = append(fields, ParamOrField{Name: field.Name, Type: fieldType, Mutable: field.Mutable})
	}
	structType.Fields = fields
	return nil
}

func (tc *typeChecker) VisitNode(node ast.Node, w ast.Walker) error {
	res := w.WalkNode(node)
	// Reset the contextual type.
	tc.contextualType = nil
	return res
}

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.Walker) error {
	tc.typeInfo.Set(module, tc.typeInfo.BuiltIns.None)
	return tc.forwardDeclareAndCheck(module.Nodes, w)
}

func (tc *typeChecker) forwardDeclareAndCheck(nodes any, w ast.Walker) error {
	decls, err := tc.forwardDeclare(nodes)
	if err != nil {
		return err
	}
	if err := tc.checkForwardDeclsStage1(decls, w); err != nil {
		return err
	}
	return tc.checkForwardDeclsStage2(decls, w)
}

func (tc *typeChecker) forwardDeclare(nodes any) (*forwardDecls, error) {
	var n []ast.Node
	switch nodes := nodes.(type) {
	case []ast.Node:
		n = nodes
	case []*ast.FunctionDeclaration:
		n = make([]ast.Node, len(nodes))
		for i, node := range nodes {
			n[i] = node
		}
	case []*ast.FunctionDefinition:
		n = make([]ast.Node, len(nodes))
		for i, node := range nodes {
			n[i] = node
		}
	default:
		panic(fmt.Sprintf("unexpected type %T", nodes))
	}
	return forwardDeclare(n, tc.typeCreator, tc.typeInfo, tc.symbolScope, tc.typeScope)
}

func (tc *typeChecker) checkForwardDeclsStage1(decls *forwardDecls, w ast.Walker) error {
	for _, decl := range decls.funcDecls {
		if err := tc.checkFunctionDeclaration(decl); err != nil {
			return err
		}
	}
	for _, decl := range decls.structDecls {
		if err := tc.checkStructTypeDeclaration(decl); err != nil {
			return err
		}
	}
	for _, def := range decls.funcDefs {
		if err := tc.checkFunctionDefinitionStage1(def); err != nil {
			return err
		}
	}
	for _, decl := range decls.traitDecls {
		if err := tc.checkTraitDeclarationStage1(decl); err != nil {
			return err
		}
	}
	for _, def := range decls.implDefs {
		if err := tc.checkImplDefinitionStage1(def, w); err != nil {
			return err
		}
	}
	// This is a bit of a hack. Because we resolve all types eagerly it can happen
	// that a generic instance has been created (using `GenericsResolver.ResolveTypeArgs()`)
	// but an impl block adds methods to the base type later. These methods are not seen
	// by the instance.
	// todo: We should do better here and lazy resolve (but we try to get away with it).
	tc.genericsResolver.reResolveStructMethods()
	for _, decl := range decls.namedUnionDecls {
		if err := tc.checkNamedUnionTypeDeclaration(decl); err != nil {
			return err
		}
	}
	return nil
}

func (tc *typeChecker) checkForwardDeclsStage2(decls *forwardDecls, w ast.Walker) error {
	for _, def := range decls.funcDefs {
		if err := tc.checkFunctionDefinitionStage2(def, w); err != nil {
			return err
		}
	}
	for _, decl := range decls.traitDecls {
		if err := tc.checkTraitDeclarationStage2(decl, w); err != nil {
			return err
		}
	}
	for _, def := range decls.implDefs {
		if err := tc.checkImplDefinitionStage2(def, w); err != nil {
			return err
		}
	}
	return nil
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

func TypeCheck(node *ast.Module, typeCreator *TypeCreator) (*TypeInfo, *GenericsResolver, error) {
	typeInfo := &TypeInfo{
		types:                make(map[ast.NodeId]Type),
		symbols:              make(map[string]*Symbol),
		typeBindings:         make(map[*ast.IdentExpression]Type),
		traitBoundsTypeParam: make(map[ast.Node]*TypeParam),
	}
	rootTypeScope := newTypeScope(nil)
	rootSymbolScope := newSymbolScope(node, nil)
	declareBuiltIns(rootTypeScope, typeInfo)
	tc := &typeChecker{
		DefaultVisitor:    ast.DefaultVisitor{},
		typeScope:         rootTypeScope,
		typeInfo:          typeInfo,
		symbolScope:       rootSymbolScope,
		typeCreator:       typeCreator,
		genericScope:      newGenericScope(nil),
		inferGenericScope: newInferGenericScope(nil),
		genericsResolver:  newGenericsResolver(typeInfo, typeCreator),
		memoizedScopes:    make(map[ast.NodeId]*memoizedScopes),
	}
	walker := &ast.DefaultWalker{Visitor: tc}
	_, err := tc.check(node, walker)
	if err != nil {
		return nil, tc.genericsResolver, err
	}
	return tc.typeInfo, tc.genericsResolver, nil
}
