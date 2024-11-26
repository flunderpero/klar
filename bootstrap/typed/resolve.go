package typed

import "fmt"

type GenericsResolver struct {
	resolved []Type
	// todo: remove this once we finished the refactoring
	//       (`ResolveTypeArgs` returns types with new ids.)
	NewBaseType func() BaseType
}

func newGenericsResolver(newBaseType func() BaseType) *GenericsResolver {
	return &GenericsResolver{NewBaseType: newBaseType}
}

func (self *GenericsResolver) ResolveTypeArgs(ty Type, typeParams []TypeParam, typeArgs []Type) Type {
	if len(typeParams) != len(typeArgs) {
		panic(fmt.Sprintf("expected %d type arguments, got %d while resolving: %s", len(typeParams), len(typeArgs), ty))
	}
	switch ty := ty.(type) {
	case GenericType:
		genericTypeArgs := make([]Type, len(ty.TypeArgs()))
		for i, typeArg := range ty.TypeArgs() {
			genericTypeArgs[i] = self.ResolveTypeArgs(typeArg, typeParams, typeArgs)
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
				param.Type = self.ResolveTypeArgs(param.Type, typeParams, typeArgs)
				params[i] = param
			}
			result := ty.Result
			if ty.Receiver == nil || ty.Receiver.Id() != ty.Result.Id() {
				result = self.ResolveTypeArgs(ty.Result, typeParams, typeArgs)
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
				field.Type = self.ResolveTypeArgs(field.Type, typeParams, typeArgs)
				fields[i] = field
			}
			methods := make([]TypeAndName[*FunctionType], len(ty.Methods))
			for i, method := range ty.Methods {
				method := method // Make a copy.
				resolvedMethodType := self.ResolveTypeArgs(method.Type, typeParams, typeArgs)
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
				resolvedMethodType := self.ResolveTypeArgs(method.Type, typeParams, typeArgs)
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
