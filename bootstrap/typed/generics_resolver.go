package typed

import (
	"fmt"
	"slices"
)

type GenericsResolver struct {
	resolvedFuncTypes   []*FunctionType
	resolvedStructTypes []*StructType
	resolvedTraitTypes  []*TraitType
	typeInfo            *TypeInfo
	typeCreator         *TypeCreator
}

func newGenericsResolver(typeInfo *TypeInfo, typeCreator *TypeCreator) *GenericsResolver {
	return &GenericsResolver{typeInfo: typeInfo, typeCreator: typeCreator}
}

func matchTypeArgs(resolved GenericType, typeArgs []Type) bool {
	resolvedTypeArgs := resolved.TypeArgs()
	if len(resolvedTypeArgs) != len(typeArgs) {
		panic(
			fmt.Sprintf(
				"expected %d type arguments, got %d while resolving: %s", len(resolvedTypeArgs), len(typeArgs), resolved))
	}
	for i, typeArg := range typeArgs {
		if resolvedTypeArgs[i].Id() != typeArg.Id() {
			return false
		}
	}
	return true
}

func (self *GenericsResolver) findResolvedStructType(ty *StructType, typeArgs []Type) (*StructType, bool) {
	tyBase, ok := ty.GenericBase()
	if !ok {
		tyBase = ty
	}
	for _, resolved := range self.resolvedStructTypes {
		base, ok := resolved.GenericBase()
		if !ok {
			panic(fmt.Sprintf("expected to have a generic base type: %s", resolved))
		}
		if base.Id() != tyBase.Id() {
			continue
		}
		if matchTypeArgs(resolved, typeArgs) {
			return resolved, true
		}
	}
	return nil, false
}

func (self *GenericsResolver) findResolvedTraitType(ty *TraitType, typeArgs []Type) (*TraitType, bool) {
	tyBase, ok := ty.GenericBase()
	if !ok {
		tyBase = ty
	}
	for _, resolved := range self.resolvedTraitTypes {
		base, ok := resolved.GenericBase()
		if !ok {
			panic(fmt.Sprintf("expected to have a generic base type: %s", resolved))
		}
		if base.Id() != tyBase.Id() {
			continue
		}
		if matchTypeArgs(resolved, typeArgs) {
			return resolved, true
		}
	}
	return nil, false
}

func (self *GenericsResolver) findResolvedFuncType(ty *FunctionType, typeArgs []Type) (*FunctionType, bool) {
	tyBase, ok := ty.GenericBase()
	if !ok {
		tyBase = ty
	}
	for _, resolved := range self.resolvedFuncTypes {
		base, ok := resolved.GenericBase()
		if !ok {
			panic(fmt.Sprintf("expected to have a generic base type: %s", resolved))
		}
		if base.Id() != tyBase.Id() {
			continue
		}
		if ty.Receiver != nil {
			if resolved.Receiver == nil || resolved.Receiver.Id() != ty.Receiver.Id() {
				continue
			}
		} else if resolved.Receiver != nil {
			continue
		}
		if matchTypeArgs(resolved, typeArgs) {
			return resolved, true
		}
	}
	return nil, false
}

func (self *GenericsResolver) declareSymbolForSpecializedType(resolved GenericType) {
	base, ok := resolved.GenericBase()
	if !ok {
		panic(fmt.Sprintf("expected to have a generic base type: %s", resolved))
	}
	baseSymbol, ok := self.typeInfo.LookupSymbol(base.Id())
	if !ok {
		// This can be a built-in function or a function type parameter.
		return
	}
	symbolName := baseSymbol.Name
	for _, typeArg := range resolved.TypeArgs() {
		symbolName += "$"
		var typeArgName string
		if typeParam, ok := typeArg.(TypeParam); ok {
			typeArgName = typeParam.Name.String()
		} else if typeParam, ok := typeArg.(*TypeParam); ok {
			// fixme: Why do we have *TypeParam and TypeParam here?
			typeArgName = typeParam.Name.String()
		} else {
			typeArgName = self.typeInfo.MustLookupSymbol(typeArg.Id()).FQN()
		}
		symbolName += typeArgName
	}
	symbol := &Symbol{Name: symbolName, Scope: baseSymbol.Scope}
	self.typeInfo.DeclareSymbol(resolved.Id(), symbol)
}

func genericBase[T GenericType](ty T) T {
	base, ok := ty.GenericBase()
	if ok {
		return base.(T)
	}
	return ty
}

func willResolve(ty Type, typeParams []TypeParam, seen map[TypeId]bool) bool {
	if _, ok := seen[ty.Id()]; ok {
		return false
	}
	seen[ty.Id()] = true
	if genericType, ok := ty.(GenericType); ok {
		for _, typeParam := range genericType.TypeParams() {
			if willResolve(typeParam, typeParams, seen) {
				return true
			}
		}
		for _, typeArg := range genericType.TypeArgs() {
			if willResolve(typeArg, typeParams, seen) {
				return true
			}
		}
	}
	switch ty := ty.(type) {
	case *FunctionType:
		for _, param := range ty.Params {
			if willResolve(param.Type, typeParams, seen) {
				return true
			}
		}
		return willResolve(ty.Result, typeParams, seen)
	case *StructType:
		for _, field := range ty.Fields {
			if willResolve(field.Type, typeParams, seen) {
				return true
			}
		}
		for _, method := range ty.Methods {
			if willResolve(method.Type, typeParams, seen) {
				return true
			}
		}
		return false
	case *TraitType:
		for _, method := range ty.Methods {
			if willResolve(method.Type, typeParams, seen) {
				return true
			}
		}
		return false
	default:
		return slices.ContainsFunc(typeParams, func(typeParam TypeParam) bool { return typeParam.id == ty.Id() })
	}
}

func (self *GenericsResolver) ResolveTypeArgs(ty Type, typeParams []TypeParam, typeArgs []Type) Type {
	if len(typeParams) != len(typeArgs) {
		panic(fmt.Sprintf("expected %d type arguments, got %d while resolving: %s", len(typeParams), len(typeArgs), ty))
	}
	switch ty := ty.(type) {
	case GenericType:
		if !willResolve(ty, typeParams, make(map[TypeId]bool)) {
			return ty
		}
		genericTypeArgs := make([]Type, len(ty.TypeArgs()))
		for i, typeArg := range ty.TypeArgs() {
			genericTypeArgs[i] = self.ResolveTypeArgs(typeArg, typeParams, typeArgs)
		}
		switch ty := ty.(type) {
		case *FunctionType:
			if resolved, ok := self.findResolvedFuncType(ty, genericTypeArgs); ok {
				return resolved
			}
			params := make([]TypeAndName[Type], len(ty.Params))
			receiver := ty.Receiver
			if receiver != nil {
				// If you call `ResolveTypeArgs` on the method only (and not on the struct it
				// belongs to), we need to make sure that the receiver is resolved, too.
				// Otherwise we might get a mismatch between the `Receiver` and `Self`.
				//
				// This also works when calling `ResolveTypeArgs` on the struct the method belongs
				// to, because we first add the new struct to the list of resolved structs before
				// resolving its methods. That way, we will find the correct struct when calling
				// `ResolveTypeArgs` on the receiver.
				receiver = self.ResolveTypeArgs(ty.Receiver, typeParams, typeArgs)
			}
			res := self.typeCreator.NewFunctionType(
				genericBase(ty), ty.typeParams, genericTypeArgs, receiver, params, ty.Result)
			self.resolvedFuncTypes = append(self.resolvedFuncTypes, res)
			for i, param := range ty.Params {
				param := param // Make a copy.
				param.Type = self.ResolveTypeArgs(param.Type, typeParams, typeArgs)
				params[i] = param
			}
			res.Result = self.ResolveTypeArgs(ty.Result, typeParams, typeArgs)
			self.declareSymbolForSpecializedType(res)
			return res
		case *StructType:
			if resolved, ok := self.findResolvedStructType(ty, genericTypeArgs); ok {
				return resolved
			}
			res := self.typeCreator.NewStructType(
				genericBase(ty),
				ty.typeParams,
				genericTypeArgs,
				make([]TypeAndName[Type], len(ty.Fields)),
				make([]TypeAndName[*FunctionType], len(ty.Methods)),
				ty.traits,
			)
			self.resolvedStructTypes = append(self.resolvedStructTypes, res)
			for i, field := range ty.Fields {
				field := field // Make a copy.
				field.Type = self.ResolveTypeArgs(field.Type, typeParams, typeArgs)
				res.Fields[i] = field
			}
			for i, method := range ty.Methods {
				method := method // Make a copy.
				method.Type = self.ResolveTypeArgs(method.Type, typeParams, typeArgs).(*FunctionType)
				res.Methods[i] = method
			}
			self.declareSymbolForSpecializedType(res)
			return res
		case *TraitType:
			if resolved, ok := self.findResolvedTraitType(ty, genericTypeArgs); ok {
				return resolved
			}
			res := self.typeCreator.NewTraitType(
				genericBase(ty), ty.typeParams, genericTypeArgs, make([]TypeAndName[*FunctionType], len(ty.Methods)))
			self.resolvedTraitTypes = append(self.resolvedTraitTypes, res)
			for i, method := range ty.Methods {
				method := method // Make a copy.
				method.Type = self.ResolveTypeArgs(method.Type, typeParams, typeArgs).(*FunctionType)
				res.Methods[i] = method
			}
			self.declareSymbolForSpecializedType(res)
			return res
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

func (self *GenericsResolver) CloneAndMergeReceiverGenerics(ty *FunctionType) *FunctionType {
	if ty.Receiver == nil {
		panic(fmt.Sprintf("expected a method (`Receiver != nil`), got: %s", ty))
	}
	genericReceiverType, ok := ty.Receiver.(GenericType)
	if !ok {
		return ty
	}
	if !HasTypeParams(genericReceiverType) {
		return ty
	}
	typeParams := append(genericReceiverType.TypeParams(), ty.TypeParams()...)
	typeArgs := append(genericReceiverType.TypeArgs(), ty.TypeArgs()...)
	baseType := ty
	if ty.genericBase != nil {
		baseType = ty.genericBase
	}
	res := self.typeCreator.NewFunctionType(baseType, typeParams, typeArgs, ty.Receiver, ty.Params, ty.Result)
	self.declareSymbolForSpecializedType(res)
	return res
}
