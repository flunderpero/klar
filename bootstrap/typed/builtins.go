package typed

import (
	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/pkg/errors"
)

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
	builtIns.builtInFunctionIdFrom = 100
	builtIns.builtInFunctionIdTo = builtIns.builtInFunctionIdFrom - 1
	nextFuncId := func() TypeId {
		builtIns.builtInFunctionIdTo += 1
		return builtIns.builtInFunctionIdTo
	}
	builtIns.Str = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"Str",
		&StructType{
			typeBase: typeBase{TypeId(1)},
			Fields:   []TypeAndName[Type]{{Name: "len_", Type: builtIns.I64}, {Name: "bytes_", Type: builtIns.RawPtr}}})
	builtIns.Print = declareBuiltIn(
		tc,
		typeInfo,
		symbolScope,
		"print",
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
