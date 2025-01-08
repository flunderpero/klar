package typed

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/pkg/errors"
)

type BuiltIns struct {
	Str                   *StructType
	Char                  *CharType
	Bool                  *BoolType
	Int                   *Int64Type
	I64                   *Int64Type
	I32                   *Int32Type
	I16                   *Int16Type
	I8                    *Int8Type
	U64                   *UInt64Type
	U32                   *UInt32Type
	U16                   *UInt16Type
	U8                    *UInt8Type
	None                  *NoneType
	Never                 *NeverType
	RawPtr                *RawPtr
	InternalArray         *StructType
	InternalPrint         *FunctionType
	PrintChar             *FunctionType
	PrintInt              *FunctionType
	PrintUInt             *FunctionType
	PrintBool             *FunctionType
	InternalMalloc        *FunctionType
	InternalFree          *FunctionType
	SizeOf                *FunctionType
	InternalWritePtr      *FunctionType
	InternalReadPtr       *FunctionType
	InternalExit          *FunctionType
	InternalCast          *FunctionType
	builtInFunctionIdFrom TypeId
	builtInFunctionIdTo   TypeId
}

func (self *BuiltIns) GetArrayElementType(ty Type) Type {
	if !self.IsArrayType(ty) {
		panic(fmt.Sprintf("expected the array type, got: %s", ty))
	}
	return ty.(*StructType).typeArgs[0]
}

func (self *BuiltIns) IsArrayType(ty Type) bool {
	if structTy, ok := ty.(*StructType); ok {
		return structTy.genericBase == self.InternalArray
	}
	return false
}

func (self *BuiltIns) IsBuiltInFunction(ty *FunctionType) bool {
	return ty.id >= self.builtInFunctionIdFrom && ty.id <= self.builtInFunctionIdTo
}

func (self *BuiltIns) Functions() []*FunctionType {
	return []*FunctionType{
		self.InternalPrint,
		self.PrintChar,
		self.PrintInt,
		self.PrintUInt,
		self.PrintBool,
		self.InternalMalloc,
		self.InternalFree,
		self.SizeOf,
		self.InternalWritePtr,
		self.InternalReadPtr,
		self.InternalExit,
		self.InternalCast,
	}
}

var builtInSpan = token.Span{File: "<builtin>", Src: &[]byte{}, Start: 0, End: 0}

func declareBuiltIn[T Type](typesScope_ *typeScope, typeInfo *TypeInfo, symbolScope *SymbolScope, name string, ty T) T {
	if err := typesScope_.declareType(name, ty, builtInSpan); err != nil {
		panic(errors.Wrapf(err, "failed to declare: %s", name))
	}
	typeInfo.DeclareSymbol(ty.Id(), &Symbol{Name: name, Scope: symbolScope})
	return ty
}

func declareBuiltIns(tc *typeScope, typeInfo *TypeInfo) {
	symbolScope := newSymbolScope(nil, nil)
	builtIns := BuiltIns{}
	builtIns.RawPtr = declareBuiltIn(tc, typeInfo, symbolScope, "RawPtr", &RawPtr{})
	builtIns.Char = declareBuiltIn(tc, typeInfo, symbolScope, "Char", &CharType{})
	builtIns.Bool = declareBuiltIn(tc, typeInfo, symbolScope, "Bool", &BoolType{})
	builtIns.I64 = declareBuiltIn(tc, typeInfo, symbolScope, "I64", &Int64Type{})
	builtIns.I32 = declareBuiltIn(tc, typeInfo, symbolScope, "I32", &Int32Type{})
	builtIns.I16 = declareBuiltIn(tc, typeInfo, symbolScope, "I16", &Int16Type{})
	builtIns.I8 = declareBuiltIn(tc, typeInfo, symbolScope, "I8", &Int8Type{})
	builtIns.U64 = declareBuiltIn(tc, typeInfo, symbolScope, "U64", &UInt64Type{})
	builtIns.U32 = declareBuiltIn(tc, typeInfo, symbolScope, "U32", &UInt32Type{})
	builtIns.U16 = declareBuiltIn(tc, typeInfo, symbolScope, "U16", &UInt16Type{})
	builtIns.U8 = declareBuiltIn(tc, typeInfo, symbolScope, "U8", &UInt8Type{})
	builtIns.Never = declareBuiltIn(tc, typeInfo, symbolScope, "Never", &NeverType{})
	builtIns.None = declareBuiltIn(tc, typeInfo, symbolScope, "None", &NoneType{})
	builtIns.Int = declareBuiltIn(tc, typeInfo, symbolScope, "Int", builtIns.I64)
	builtIns.Str = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"Str",
		&StructType{
			implementableTypeBase: implementableTypeBase{typeBase: typeBase{TypeId(1)}, methods: nil, traits: nil},
			Fields: []TypeAndName[Type]{{
				Name: "len_", Type: builtIns.Int}, {Name: "bytes_", Type: builtIns.RawPtr}}})
	builtIns.InternalArray = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"InternalArray",
		&StructType{
			implementableTypeBase: implementableTypeBase{typeBase: typeBase{TypeId(15)}, methods: nil, traits: nil},
			Fields: []TypeAndName[Type]{
				{Name: "len_", Type: builtIns.Int},
				{Name: "capacity_", Type: builtIns.Int},
				{Name: "data_", Type: builtIns.RawPtr},
			}})
	internalArrayTypeParam := &TypeParam{
		typeBase:    typeBase{TypeId(16)},
		GenericType: builtIns.InternalArray,
		Name:        ast.Ident("T"),
		Index:       0,
	}
	builtIns.InternalArray.typeParams = []TypeParam{*internalArrayTypeParam}
	builtIns.InternalArray.typeArgs = []Type{internalArrayTypeParam}
	builtIns.builtInFunctionIdFrom = 100
	builtIns.builtInFunctionIdTo = builtIns.builtInFunctionIdFrom - 1
	nextFuncId := func() TypeId {
		builtIns.builtInFunctionIdTo += 1
		return builtIns.builtInFunctionIdTo
	}
	builtIns.InternalPrint = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_print",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "value", Type: builtIns.Str}},
			Result:   builtIns.None,
		})
	builtIns.PrintChar = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"print_char",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "value", Type: builtIns.Char}},
			Result:   builtIns.None})
	builtIns.PrintInt = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"print_int",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "value", Type: builtIns.I64}},
			Result:   builtIns.None})
	builtIns.PrintUInt = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"print_uint",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "value", Type: builtIns.U64}},
			Result:   builtIns.None})
	builtIns.PrintBool = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"print_bool",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "value", Type: builtIns.Bool}},
			Result:   builtIns.None})
	builtIns.InternalMalloc = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_malloc",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "size", Type: builtIns.I64}},
			Result:   builtIns.I64})
	builtIns.InternalFree = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_free",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "ptr", Type: builtIns.RawPtr}},
			Result:   builtIns.None})
	builtIns.SizeOf = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"sizeof",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{},
			Result:   builtIns.I64})
	sizeOfTypeParam := &TypeParam{
		typeBase:    typeBase{nextFuncId()},
		GenericType: builtIns.SizeOf,
		Name:        ast.Ident("T"),
		Index:       0,
	}
	builtIns.SizeOf.typeParams = []TypeParam{*sizeOfTypeParam}
	builtIns.SizeOf.typeArgs = []Type{sizeOfTypeParam}
	internalWritePtrTypeParam := &TypeParam{
		typeBase: typeBase{nextFuncId()},
		Name:     ast.Ident("T"),
		Index:    0,
	}
	castFromTypeParam := &TypeParam{
		typeBase: typeBase{nextFuncId()},
		Name:     ast.Ident("T"),
		Index:    0,
	}
	castToTypeParam := &TypeParam{
		typeBase: typeBase{nextFuncId()},
		Name:     ast.Ident("U"),
		Index:    1,
	}
	builtIns.InternalCast = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_cast",
		&FunctionType{
			typeBase:   typeBase{nextFuncId()},
			typeParams: []TypeParam{*castFromTypeParam, *castToTypeParam},
			typeArgs:   []Type{castFromTypeParam, castToTypeParam},
			Params: []FunctionParam{
				{Name: "value", Type: castFromTypeParam},
			},
			Result: castToTypeParam,
		})
	castFromTypeParam.GenericType = builtIns.InternalCast
	castToTypeParam.GenericType = builtIns.InternalCast
	builtIns.InternalWritePtr = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_write_ptr",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params: []FunctionParam{
				{Name: "ptr", Type: builtIns.RawPtr},
				{Name: "value", Type: internalWritePtrTypeParam},
			},
			Result: builtIns.None})
	internalWritePtrTypeParam.GenericType = builtIns.InternalWritePtr
	builtIns.InternalWritePtr.typeParams = []TypeParam{*internalWritePtrTypeParam}
	builtIns.InternalWritePtr.typeArgs = []Type{internalWritePtrTypeParam}
	builtIns.InternalReadPtr = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_read_ptr",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "ptr", Type: builtIns.RawPtr}}})
	internalReadPtrTypeParam := &TypeParam{
		typeBase:    typeBase{nextFuncId()},
		GenericType: builtIns.InternalReadPtr,
		Name:        ast.Ident("T"),
		Index:       0,
	}
	builtIns.InternalReadPtr.typeParams = []TypeParam{*internalReadPtrTypeParam}
	builtIns.InternalReadPtr.typeArgs = []Type{internalReadPtrTypeParam}
	builtIns.InternalReadPtr.Result = internalReadPtrTypeParam
	builtIns.InternalExit = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"internal_exit",
		&FunctionType{
			typeBase: typeBase{nextFuncId()},
			Params:   []FunctionParam{{Name: "code", Type: builtIns.I64}},
			Result:   builtIns.Never})
	typeInfo.BuiltIns = builtIns
}
