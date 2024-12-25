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

func (self *TypeCreator) NewFunctionType(genericBase *FunctionType, typeParams []TypeParam, typeArgs []Type, receiver Type, params []FunctionParam, result Type) *FunctionType {
	return &FunctionType{
		typeBase:    self.newTypeBase(),
		genericBase: genericBase,
		typeParams:  typeParams,
		typeArgs:    typeArgs,
		Receiver:    receiver,
		Params:      params,
		Result:      result,
	}
}

func (self *TypeCreator) NewTraitType(genericBase *TraitType, typeParams []TypeParam, typeArgs []Type, methods []TypeAndName[*FunctionType]) *TraitType {
	return &TraitType{
		typeBase:    self.newTypeBase(),
		genericBase: genericBase,
		typeParams:  typeParams,
		typeArgs:    typeArgs,
		Methods:     methods,
	}
}

func (self *TypeCreator) NewStructType(genericBase *StructType, typeParams []TypeParam, typeArgs []Type, fields []TypeAndName[Type], methods []TypeAndName[*FunctionType], traits []*TraitType) *StructType {
	return &StructType{
		typeBase:    self.newTypeBase(),
		genericBase: genericBase,
		typeParams:  typeParams,
		typeArgs:    typeArgs,
		Fields:      fields,
		Methods:     methods,
		traits:      traits,
	}
}

func (self *TypeCreator) NewTupleType(values []Type) *TupleType {
	return &TupleType{typeBase: self.newTypeBase(), Values: values}
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

var BuiltInPrintFunction = &FunctionType{
	typeBase: typeBase{TypeId(100)},
	Params:   []FunctionParam{{Name: "value", Type: strType}},
	Result:   noneType,
}
var BuiltInPrintCharFunction = &FunctionType{
	typeBase: typeBase{TypeId(101)},
	Params:   []FunctionParam{{Name: "value", Type: charType}},
	Result:   noneType,
}
var BuiltInPrintIntFunction = &FunctionType{
	typeBase: typeBase{TypeId(102)},
	Params:   []FunctionParam{{Name: "value", Type: int64Type}},
	Result:   noneType,
}
var BuiltInPrintUIntFunction = &FunctionType{
	typeBase: typeBase{TypeId(103)},
	Params:   []FunctionParam{{Name: "value", Type: uint64Type}},
	Result:   noneType,
}
var BuiltInPrintBoolFunction = &FunctionType{
	typeBase: typeBase{TypeId(104)},
	Params:   []FunctionParam{{Name: "value", Type: boolType}},
	Result:   noneType,
}
var BuiltInInternalMallocFunction = &FunctionType{
	typeBase: typeBase{TypeId(105)},
	Params:   []FunctionParam{{Name: "size", Type: int64Type}},
	Result:   int64Type,
}
var BuiltInInternalFreeFunction = &FunctionType{
	typeBase: typeBase{TypeId(106)},
	Params:   []FunctionParam{{Name: "ptr", Type: rawPtr}},
	Result:   noneType,
}
var BuiltInSizeOfFunctionTypeParam = &TypeParam{
	typeBase:    typeBase{TypeId(107)},
	GenericType: BuiltInSizeOfFunction,
	Name:        ast.Ident("T"),
	Index:       0,
}
var BuiltInSizeOfFunction = &FunctionType{
	typeBase: typeBase{TypeId(108)},
	Params:   []FunctionParam{},
	Result:   int64Type,
}
var BuiltInInternalWritePtrFunctionTypeParam = &TypeParam{
	typeBase: typeBase{TypeId(109)},
	Name:     ast.Ident("T"),
	Index:    0,
}
var BuiltInInternalWritePtrFunction = &FunctionType{
	typeBase: typeBase{TypeId(110)},
	Params: []FunctionParam{
		{Name: "ptr", Type: rawPtr},
		{Name: "value", Type: BuiltInInternalWritePtrFunctionTypeParam},
	},
	Result: noneType,
}
var BuiltInInternalReadPtrFunctionTypeParam = &TypeParam{
	typeBase:    typeBase{TypeId(111)},
	GenericType: BuiltInInternalReadPtrFunction,
	Name:        ast.Ident("T"),
	Index:       0,
}
var BuiltInInternalReadPtrFunction = &FunctionType{
	typeBase: typeBase{TypeId(112)},
	Params:   []FunctionParam{{Name: "ptr", Type: rawPtr}},
}
var BuiltInInternalExitFunction = &FunctionType{
	typeBase: typeBase{TypeId(113)},
	Params:   []FunctionParam{{Name: "code", Type: int64Type}},
	Result:   neverType,
}

func IsBuiltInFunction(funcType *FunctionType) bool {
	id := funcType.Id()
	res := id >= BuiltInPrintFunction.Id() && id <= BuiltInInternalExitFunction.Id()
	if !res {
		if base, ok := funcType.GenericBase(); ok {
			return IsBuiltInFunction(base.(*FunctionType))
		}
	}
	return res
}

var builtInSpan = token.Span{File: "<builtin>", Src: &[]byte{}, Start: 0, End: 0}
var charType = &CharType{}
var strType = &StrType{}
var boolType = &BoolType{}
var int64Type = &Int64Type{}
var int32Type = &Int32Type{}
var int16Type = &Int16Type{}
var int8Type = &Int8Type{}
var uint64Type = &UInt64Type{}
var uint32Type = &UInt32Type{}
var uint16Type = &UInt16Type{}
var uint8Type = &UInt8Type{}
var noneType = &NoneType{}
var neverType = &NeverType{}
var rawPtr = &RawPtr{}

type TypeWithTraits interface {
	Type
	Traits() []*TraitType
}

type CallableType interface {
	Type
	CallParams() []TypeAndName[Type]
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

type StrType struct {
	traits []*TraitType
}

func (ty StrType) Id() TypeId {
	return 1
}

func (ty StrType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

func (ty StrType) String() string {
	return "StrType"
}

func (ty *StrType) Traits() []*TraitType {
	return ty.traits
}

type CharType struct {
	traits []*TraitType
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

func (ty *CharType) Traits() []*TraitType {
	return ty.traits
}

type BoolType struct {
	traits []*TraitType
}

func (ty BoolType) Id() TypeId {
	return 3
}

func (ty BoolType) IsAssignableFrom(other Type) bool {
	return ty.Id() == other.Id()
}

func (ty *BoolType) Traits() []*TraitType {
	return ty.traits
}

func (ty BoolType) String() string {
	return "BoolType"
}

type IntType interface {
	IsSigned() bool
	Bits() int
}

type Int8Type struct {
	traits []*TraitType
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
	return other.Id() == int8Type.Id()
}

func (ty *Int8Type) Traits() []*TraitType {
	return ty.traits
}

func (ty Int8Type) String() string {
	return "Int8Type"
}

type Int16Type struct {
	traits []*TraitType
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
	return other.Id() == int16Type.Id() || other.Id() == int8Type.Id() || other.Id() == uint8Type.Id()
}

func (ty *Int16Type) Traits() []*TraitType {
	return ty.traits
}

func (ty Int16Type) String() string {
	return "Int16Type"
}

type Int32Type struct {
	traits []*TraitType
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
	return other.Id() == int32Type.Id() || other.Id() == int16Type.Id() || other.Id() == int8Type.Id() || other.Id() == uint8Type.Id() || other.Id() == uint16Type.Id()
}

func (ty *Int32Type) Traits() []*TraitType {
	return ty.traits
}

func (ty Int32Type) String() string {
	return "Int32Type"
}

type Int64Type struct {
	traits []*TraitType
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
	return other.Id() == int64Type.Id() || other.Id() == int32Type.Id() || other.Id() == int16Type.Id() || other.Id() == int8Type.Id() || other.Id() == uint8Type.Id() || other.Id() == uint16Type.Id() || other.Id() == uint32Type.Id()
}

func (ty *Int64Type) Traits() []*TraitType {
	return ty.traits
}

func (ty Int64Type) String() string {
	return "Int64Type"
}

type UInt8Type struct {
	traits []*TraitType
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
	return other.Id() == uint8Type.Id()
}

func (ty *UInt8Type) Traits() []*TraitType {
	return ty.traits
}

func (ty UInt8Type) String() string {
	return "UInt8Type"
}

type UInt16Type struct {
	traits []*TraitType
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
	return other.Id() == uint16Type.Id() || other.Id() == uint8Type.Id()
}

func (ty *UInt16Type) Traits() []*TraitType {
	return ty.traits
}

func (ty UInt16Type) String() string {
	return "UInt16Type"
}

type UInt32Type struct {
	traits []*TraitType
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
	return other.Id() == uint32Type.Id() || other.Id() == uint16Type.Id() || other.Id() == uint8Type.Id()
}

func (ty *UInt32Type) Traits() []*TraitType {
	return ty.traits
}

func (ty UInt32Type) String() string {
	return "UInt32Type"
}

type UInt64Type struct {
	traits []*TraitType
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
	return other.Id() == uint64Type.Id() || other.Id() == uint32Type.Id() || other.Id() == uint16Type.Id() || other.Id() == uint8Type.Id()
}

func (ty *UInt64Type) Traits() []*TraitType {
	return ty.traits
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
	return ty.Id() == other.Id() || other.Id() == int64Type.Id()
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
	return fmt.Sprintf("TypeParam %s #%s of %s[%d]%s", ty.Name, ty.Id(), ty.GenericType.Id(), ty.Index, traitBound)
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
	return fmt.Sprintf("TupleType%s", base.IndentSlice(self.Values, 1))
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

func (self TupleType) CallParams() []FunctionParam {
	params := make([]FunctionParam, len(self.Values))
	for i, value := range self.Values {
		params[i] = FunctionParam{Name: ast.Ident(fmt.Sprintf("%d", i)), Type: value}
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
	Name      ast.Ident
	Type      CallableType
	UnionType *UnionType
}

func (self NamedUnionVariant) String() string {
	return fmt.Sprintf("NamedUnionVariant %s\n%s", self.Name, base.Indent(self.Type, 1))
}

func (self NamedUnionVariant) CallParams() []FunctionParam {
	return self.Type.CallParams()
}

func (self NamedUnionVariant) CallResult() Type {
	return &self
}

type UnionVariant struct {
	Kind  UnionVariantKind
	Named NamedUnionVariant
	Type  Type
}

func (self UnionVariant) String() string {
	switch self.Kind {
	case UnionVariantKindNamed:
		return fmt.Sprintf("NamedVariant\n%s", base.Indent(self.Named, 1))
	case UnionVariantKindType:
		return fmt.Sprintf("TypeVariant\n%s", base.Indent(self.Type, 1))
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
	genericBase *StructType
	typeParams  []TypeParam
	typeArgs    []Type
	Variants    []UnionVariant
}

func (self UnionType) String() string {
	return fmt.Sprintf("UnionType%s%s%s",
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

type StructType struct {
	typeBase
	genericBase *StructType
	typeParams  []TypeParam
	typeArgs    []Type
	Fields      []TypeAndName[Type]
	Methods     []TypeAndName[*FunctionType]
	traits      []*TraitType
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
	baseType := ""
	if ty.genericBase != nil {
		baseType = fmt.Sprintf(" (base #%s)", ty.genericBase.id)
	}
	return fmt.Sprintf(
		"StructType #%s%s%s%s\n    (Fields)%s\n    (Methods)%s",
		ty.id,
		baseType,
		base.IndentString(typeParamsString(ty.typeParams), 1),
		base.IndentString(typeArgsString(ty.typeArgs), 1),
		base.IndentStringSlice(fields, 2),
		base.IndentSlice(ty.Methods, 2),
	)
}

func (ty StructType) FindFieldIndex(name ast.MemberExpressionField, span token.Span) (int, bool) {
	fieldIndex := slices.IndexFunc(ty.Fields, func(field TypeAndName[Type]) bool { return string(field.Name) == string(name) })
	if fieldIndex < 0 {
		return -1, false
	}
	return fieldIndex, true
}

func (ty StructType) FindField(name ast.MemberExpressionField, span token.Span) (*TypeAndName[Type], bool) {
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

func (ty StructType) FindMember(name ast.MemberExpressionField, span token.Span) (Type, bool) {
	field, found := ty.FindField(name, span)
	if found {
		return field.Type, true
	}
	if name.IsIndex() {
		return nil, false
	}
	method, found := ty.FindMethod(name.AsIdent(), span)
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
	Methods     []TypeAndName[*FunctionType]
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

type FunctionParam = TypeAndName[Type]

type FunctionType struct {
	typeBase
	genericBase *FunctionType
	typeParams  []TypeParam
	typeArgs    []Type
	Receiver    Type
	Params      []FunctionParam
	Result      Type
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
		paramName := param.Name.String()
		if paramName == "" {
			paramName = "<positional>"
		}
		params[i] = fmt.Sprintf("%s\n%s", paramName, base.IndentString(typeToString(param.Type), 1))
	}
	result := typeToString(ty.Result)
	baseType := ""
	if ty.genericBase != nil {
		baseType = fmt.Sprintf(" (base #%s)", ty.genericBase.id)
	}
	return fmt.Sprintf(
		"FunctionType #%s%s%s%s%s\n    (Parameters)%s\n    (Result)\n%s",
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

type typeScope struct {
	types     map[string]Type
	variables map[string]VariableType
	parent    *typeScope
}

func newTypeScope(parent *typeScope) *typeScope {
	return &typeScope{
		types:     make(map[string]Type),
		variables: make(map[string]VariableType),
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
	if _, found := te.types[name]; found {
		return errors.Errorf("%s: type %q already declared", span, name)
	}
	te.types[name] = ty
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
	case *ast.UnionTypeDeclaration:
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
	typeInfo         *TypeInfo
	typeScope        *typeScope
	symbolScope      *SymbolScope
	genericScope     *genericScope
	genericsResolver *GenericsResolver
	loopDepth        int
	checkingMode     checkingMode
	typeCreator      *TypeCreator
	contextualType   Type
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
	tc.typeInfo.Set(expr, strType)
	return nil
}

func (tc *typeChecker) VisitCharLiteralExpression(expr *ast.CharLiteralExpression) error {
	tc.typeInfo.Set(expr, charType)
	return nil
}

func (tc *typeChecker) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	if expr.IsUInt64 {
		tc.typeInfo.Set(expr, uint64Type)
		return nil
	}
	v := expr.Int64
	var ty Type
	switch tc.contextualType {
	case int8Type:
		if v < -128 || v > 127 {
			return errors.Errorf("%s: value %d out of range for Int8Type", expr.Span(), v)
		}
		ty = int8Type
	case int16Type:
		if v < -32768 || v > 32767 {
			return errors.Errorf("%s: value %d out of range for Int16Type", expr.Span(), v)
		}
		ty = int16Type
	case int32Type:
		if v < -2147483648 || v > 2147483647 {
			return errors.Errorf("%s: value %d out of range for Int32Type", expr.Span(), v)
		}
		ty = int32Type
	case uint8Type:
		if v < 0 || v > 255 {
			return errors.Errorf("%s: value %d out of range for UInt8Type", expr.Span(), v)
		}
		ty = uint8Type
	case uint16Type:
		if v < 0 || v > 65535 {
			return errors.Errorf("%s: value %d out of range for UInt16Type", expr.Span(), v)
		}
		ty = uint16Type
	case uint32Type:
		if v < 0 || v > 4294967295 {
			return errors.Errorf("%s: value %d out of range for UInt32Type", expr.Span(), v)
		}
		ty = uint32Type
	case uint64Type:
		if v < 0 {
			return errors.Errorf("%s: value %d out of range for UInt64Type", expr.Span(), v)
		}
		ty = uint64Type
	default:
		ty = int64Type
	}
	tc.typeInfo.Set(expr, ty)
	return nil
}

func (tc *typeChecker) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	tc.typeInfo.Set(expr, boolType)
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
	tc.typeInfo.Set(expr, tupleType)
	return nil
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
	case ast.OpAdd, ast.OpMultiply, ast.OpDivide, ast.OpModulo:
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
	case ast.OpEqual, ast.OpNotEqual, ast.OpGreaterThan, ast.OpGreaterThanOrEqual, ast.OpLessThan, ast.OpLessThanOrEqual:
		switch lhs.(type) {
		case IntType, *RawPtr:
		case *BoolType:
			if expr.Op != ast.OpEqual && expr.Op != ast.OpNotEqual {
				return errors.Errorf("%s: only == and != are supported for bool types", expr.Span())
			}
		default:
			return errors.Errorf("%s: lhs of comparison expression must be an int or bool type, got %s", expr.Span(), lhs)
		}
		if !lhs.IsAssignableFrom(rhs) {
			return errors.Errorf("%s: rhs of comparison expression must be assignable to lhs, expected %q got %q", expr.Span(), lhs, rhs)
		}
		tc.typeInfo.Set(expr, boolType)
	case ast.OpAnd, ast.OpOr:
		if lhs != boolType {
			return errors.Errorf("%s: lhs of logical expression must be of type BoolType, got %s", expr.Span(), lhs)
		}
		if rhs != boolType {
			return errors.Errorf("%s: rhs of logical expression must be of type BoolType, got %s", expr.Span(), rhs)
		}
		tc.typeInfo.Set(expr, boolType)
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
		if valueType != boolType {
			return errors.Errorf(
				"%s: operand of logical not expression must be of type BoolType, got %s", expr.Span(), valueType)
		}
		tc.typeInfo.Set(expr, boolType)
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
		tc.contextualType = param.Type
		if err := tc.VisitNode(arg.Value, w); err != nil {
			return err
		}
		argType := tc.typeInfo.MustLookup(arg.Value)
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
		case *UnionType:
			variant, found := ty.FindNamedVariant(expr.Field.AsIdent())
			if !found {
				unionSymbol := tc.typeInfo.MustLookupSymbol(ty.Id())
				return errors.Errorf("%s: variant %q not found in union type %q", expr.Span(), expr.Field, unionSymbol.Name)
			}
			memberType = variant
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
	var blockType Type = noneType
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
	if _, ok := tc.typeInfo.MustLookup(expr.Condition).(*BoolType); !ok {
		return errors.Errorf("%s: the condition of an if expression must be a boolean type, got: %s", expr.Condition.Span(), condType)
	}
	// Only an if expression with an else branch can have a type other than None.
	// And currently we don't have else branches.
	tc.typeInfo.Set(expr, noneType)
	return nil
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
	funcType := &FunctionType{typeBase: tc.newTypeBase()}
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
	tc.declareSymbol(funcType.Id(), decl.Name.String())
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
	tc.typeInfo.Set(fn, declaredType)
	tc.enterScope(fn)
	defer tc.exitScope()
	params := functionType.Params
	for i, astParam := range fn.Decl.Params {
		param := params[i]
		varType := VariableType{Type: param.Type, IsFunctionParam: true, IsMutable: false, Span: astParam.Span}
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
	// We need to forward declare the trait type so that we can set the `Self` type correctly.
	traitType := &TraitType{typeBase: tc.newTypeBase()}
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
		if _, found := structType.FindField(ast.MemberExpressionField(decl.Name), decl.Span()); found {
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
			traitMethodType, ok := traitType.FindMethod(decl.Name, decl.Span())
			if !ok {
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
	if traitType != nil {
		structType.traits = append(structType.traits, traitType)
	}
	tc.typeInfo.Set(impl, &ImplType{typeBase: tc.newTypeBase(), ReceiverType: structType})
	return nil
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
	if err := w.WalkAssignmentStatement(s); err != nil {
		return err
	}
	rhsType := tc.typeInfo.MustLookup(s.Rhs)
	varType, varInfo, ok := tc.typeScope.lookupVariable(s.Variable.Ident)
	if !ok {
		return errors.Errorf("%s: unknown variable %q", s.Span(), s.Variable.Ident)
	}
	if !varInfo.IsMutable {
		return errors.Errorf("%s: variable %q is not mutable", s.Span(), s.Variable.Ident)
	}
	if s.IsAssignToMember() {
		structType, ok := varType.(*StructType)
		if !ok {
			return errors.Errorf("%s: variable %q is not a struct type", s.Span(), s.Variable.Ident)
		}
		field, found := structType.FindField(ast.MemberExpressionField(*s.Field), s.Span())
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
	tc.typeInfo.Set(s, noneType)
	return nil
}

func (tc *typeChecker) VisitLoopStatement(s *ast.LoopStatement, w ast.Walker) error {
	tc.typeInfo.Set(s, noneType)
	tc.enterLoop()
	defer tc.exitLoop()
	return w.WalkLoopStatement(s)
}

func (tc *typeChecker) VisitContinueStatement(s *ast.ContinueStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: continue statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, noneType)
	return nil
}

func (tc *typeChecker) VisitBreakStatement(s *ast.BreakStatement) error {
	if tc.loopDepth == 0 {
		return errors.Errorf("%s: break statement outside of a loop", s.Span())
	}
	tc.typeInfo.Set(s, noneType)
	return nil
}

func (tc *typeChecker) VisitUnionTypeDeclaration(decl *ast.UnionTypeDeclaration) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	unionType := &UnionType{typeBase: tc.newTypeBase()}
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
	if err := tc.typeScope.declareType(string(decl.Name), unionType, decl.Span()); err != nil {
		return err
	}
	tc.declareSymbol(unionType.Id(), decl.Name.String())
	tc.typeInfo.Set(decl, &DeclaredType{Type: unionType})
	tc.enterScope(decl)
	defer tc.exitScope()
	variants := make([]UnionVariant, len(decl.Variants))
	for i, astVariant := range decl.Variants {
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
					typeBase: tc.newTypeBase(), Name: astVariant.Named.Name, Type: variantType, UnionType: unionType}}
		default:
			panic(fmt.Sprintf("unexpected variant kind: %d", astVariant.Kind))
		}
		variants[i] = variant
	}
	unionType.Variants = variants
	return nil
}

func (tc *typeChecker) VisitStructTypeDeclaration(decl *ast.StructTypeDeclaration) error {
	tc.enterGenericScope()
	defer tc.exitGenericScope()
	structType := &StructType{typeBase: tc.newTypeBase()}
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

func (tc *typeChecker) VisitNode(node ast.Node, w ast.Walker) error {
	res := w.WalkNode(node)
	// Reset the contextual type.
	tc.contextualType = nil
	return res
}

func (tc *typeChecker) VisitModule(module *ast.Module, w ast.Walker) error {
	tc.typeInfo.Set(module, noneType)
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

func TypeCheck(node *ast.Module, typeCreator *TypeCreator) (*TypeInfo, *GenericsResolver, error) {
	typeInfo := &TypeInfo{
		types:                make(map[ast.NodeId]Type),
		symbols:              make(map[string]*Symbol),
		typeBindings:         make(map[*ast.IdentExpression]Type),
		traitBoundsTypeParam: make(map[ast.Node]*TypeParam),
	}
	tc := &typeChecker{
		DefaultVisitor:   ast.DefaultVisitor{},
		typeScope:        newTypeScope(nil),
		typeInfo:         typeInfo,
		symbolScope:      newSymbolScope(node, nil),
		typeCreator:      typeCreator,
		genericScope:     newGenericScope(nil),
		genericsResolver: newGenericsResolver(typeInfo, typeCreator),
	}
	// Declare builtin types and functions.
	builtInSymbolScope := newSymbolScope(nil, nil)
	declareBuiltIn := func(name string, ty Type) {
		if err := tc.typeScope.declareType(name, ty, builtInSpan); err != nil {
			panic(errors.Wrapf(err, "failed to declare: %s", name))
		}
		tc.typeInfo.DeclareSymbol(ty.Id(), &Symbol{Name: name, Scope: builtInSymbolScope})
	}
	declareBuiltIn("None", noneType)
	declareBuiltIn("Str", strType)
	declareBuiltIn("Char", charType)
	declareBuiltIn("Bool", boolType)
	declareBuiltIn("Int", int64Type)
	declareBuiltIn("I64", int64Type)
	declareBuiltIn("I32", int32Type)
	declareBuiltIn("I16", int16Type)
	declareBuiltIn("I8", int8Type)
	declareBuiltIn("U64", uint64Type)
	declareBuiltIn("U32", uint32Type)
	declareBuiltIn("U16", uint16Type)
	declareBuiltIn("U8", uint8Type)
	declareBuiltIn("RawPtr", rawPtr)
	declareBuiltIn("Never", neverType)
	declareBuiltIn("print", BuiltInPrintFunction)
	declareBuiltIn("print_char", BuiltInPrintCharFunction)
	declareBuiltIn("print_int", BuiltInPrintIntFunction)
	declareBuiltIn("print_uint", BuiltInPrintUIntFunction)
	declareBuiltIn("print_bool", BuiltInPrintBoolFunction)
	declareBuiltIn("internal_exit", BuiltInInternalExitFunction)
	declareBuiltIn("internal_malloc", BuiltInInternalMallocFunction)
	declareBuiltIn("internal_free", BuiltInInternalFreeFunction)
	BuiltInInternalWritePtrFunctionTypeParam.GenericType = BuiltInInternalWritePtrFunction
	BuiltInInternalWritePtrFunction.typeParams = []TypeParam{*BuiltInInternalWritePtrFunctionTypeParam}
	BuiltInInternalWritePtrFunction.typeArgs = []Type{BuiltInInternalWritePtrFunctionTypeParam}
	declareBuiltIn("internal_write_ptr", BuiltInInternalWritePtrFunction)
	BuiltInInternalReadPtrFunction.typeParams = []TypeParam{*BuiltInInternalReadPtrFunctionTypeParam}
	BuiltInInternalReadPtrFunction.typeArgs = []Type{BuiltInInternalReadPtrFunctionTypeParam}
	BuiltInInternalReadPtrFunction.Result = BuiltInInternalReadPtrFunctionTypeParam
	declareBuiltIn("internal_read_ptr", BuiltInInternalReadPtrFunction)
	BuiltInSizeOfFunction.typeParams = []TypeParam{*BuiltInSizeOfFunctionTypeParam}
	BuiltInSizeOfFunction.typeArgs = []Type{BuiltInSizeOfFunctionTypeParam}
	declareBuiltIn("sizeof", BuiltInSizeOfFunction)
	walker := &ast.DefaultWalker{Visitor: tc}
	_, err := tc.check(node, walker)
	if err != nil {
		return nil, tc.genericsResolver, err
	}
	return tc.typeInfo, tc.genericsResolver, nil
}
