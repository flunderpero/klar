/*
# Tuple Lowering

This pass will convert all tuple types to struct types and all tuple literals to struct
initialization.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type tupleLowering struct {
	DefaultTransformer
	typeInfo                           *typed.TypeInfo
	typeCreator                        *typed.TypeCreator
	nodeCreator                        *ast.NodeCreator
	tupleStructs                       map[string]*typed.StructType
	replaceTupleTypeWithStructTypeSeen map[typed.TypeId]typed.Type
}

func (self *tupleLowering) convertTupleToStructType(tupleType *typed.TupleType) *typed.StructType {
	key := ""
	for _, value := range tupleType.Values {
		key += value.Id().String() + ","
	}
	structType, ok := self.tupleStructs[key]
	if !ok {
		fields := make([]typed.ParamOrField, len(tupleType.Values))
		for i, value := range tupleType.Values {
			if tupleValue, ok := value.(*typed.TupleType); ok {
				value = self.convertTupleToStructType(tupleValue)
			}
			field := typed.ParamOrField{Name: ast.Ident(fmt.Sprintf("%d", i)), Mutable: false, Type: value}
			fields[i] = field
		}
		structType = self.typeCreator.NewStructType(nil, nil, nil, fields, nil, nil)
		if symbol, ok := self.typeInfo.LookupSymbol(tupleType.Id()); ok {
			self.typeInfo.DeclareSymbol(structType.Id(), symbol)
		}
		self.tupleStructs[key] = structType
	}
	return structType
}

func (self *tupleLowering) VisitTupleLiteralExpression(expr *ast.TupleLiteralExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkTupleLiteralExpression(expr)
	visitMustNotChange(expr, visited, ok)
	tupleType := self.typeInfo.MustLookup(expr).(*typed.TupleType)
	structType := self.convertTupleToStructType(tupleType)
	callArgs := make([]ast.CallArg, len(expr.Values))
	for i, value := range expr.Values {
		self.typeInfo.Set(value, structType.Fields[i].Type)
		callArgs[i] = ast.CallArg{Value: value, Span: value.Span()}
	}
	calleeExpr := self.nodeCreator.NewIdentExpression(ast.Ident("$TupleLowering"), expr.Span())
	self.typeInfo.Set(calleeExpr, structType)
	callExpr := self.nodeCreator.NewCallExpression(calleeExpr, callArgs, expr.Span())
	self.typeInfo.Set(callExpr, structType)
	return callExpr, true
}

func (self *tupleLowering) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	node, ok := w.WalkNode(node)
	visitMustNotRemove(node, ok)
	ty := self.typeInfo.MustLookup(node)
	ty = self.replaceTupleTypeWithStructType(ty)
	self.typeInfo.Set(node, ty)
	return node, true
}

func (self *tupleLowering) replaceTupleTypeWithStructType(ty typed.Type) typed.Type {
	// todo: `typed.DeclaredType` should not have the same `TypeId` as its enclosed type.
	if _, ok := ty.(*typed.DeclaredType); ok {
		return ty
	}
	if res, ok := self.replaceTupleTypeWithStructTypeSeen[ty.Id()]; ok {
		return res
	}
	if genericType, ok := ty.(typed.GenericType); ok {
		typeArgs := genericType.TypeArgs()
		for i, typeArg := range typeArgs {
			if _, ok := typeArg.(typed.TypeParam); ok {
				continue
			}
			typeArgs[i] = self.replaceTupleTypeWithStructType(typeArg)
		}
	}
	switch tyKind := ty.(type) {
	case *typed.TupleType:
		ty = self.convertTupleToStructType(tyKind)
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
	case *typed.FunctionType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for i, param := range tyKind.Params {
			param.Type = self.replaceTupleTypeWithStructType(param.Type)
			tyKind.Params[i] = param
		}
		tyKind.Result = self.replaceTupleTypeWithStructType(tyKind.Result)
		if tyKind.Receiver != nil {
			tyKind.Receiver = self.replaceTupleTypeWithStructType(tyKind.Receiver)
		}
	case *typed.StructType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for i, field := range tyKind.Fields {
			field.Type = self.replaceTupleTypeWithStructType(field.Type)
			tyKind.Fields[i] = field
		}
	case *typed.UnionType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for _, variant := range tyKind.Variants {
			switch variant.Kind {
			case typed.UnionVariantKindType:
				variant.Type = self.replaceTupleTypeWithStructType(variant.Type)
			case typed.UnionVariantKindNamed:
				variant.Named.Type = self.replaceTupleTypeWithStructType(variant.Named.Type).(*typed.TupleType)
			default:
				panic(fmt.Sprintf("unexpected union variant kind: %d", variant.Kind))
			}
		}
	case *typed.NamedUnionVariant:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		tyKind.Type = self.replaceTupleTypeWithStructType(tyKind.Type).(*typed.TupleType)
	case *typed.BoolType,
		*typed.Int64Type,
		*typed.Int32Type,
		*typed.Int16Type,
		*typed.Int8Type,
		*typed.UInt64Type,
		*typed.UInt32Type,
		*typed.UInt16Type,
		*typed.UInt8Type,
		*typed.CharType,
		*typed.NoneType,
		*typed.NeverType,
		*typed.RawPtr,
		*typed.VariableType,
		*typed.TraitType,
		*typed.ImplType,
		*typed.TypeParam:
	default:
		panic(fmt.Sprintf("unexpected type: %T", ty))
	}
	return ty
}

func TupleLowering(
	module *ast.Module, typeInfo *typed.TypeInfo, typeCreator *typed.TypeCreator, nodeCreator *ast.NodeCreator) *ast.Module {
	transformer := &tupleLowering{
		typeInfo:                           typeInfo,
		typeCreator:                        typeCreator,
		nodeCreator:                        nodeCreator,
		tupleStructs:                       map[string]*typed.StructType{},
		replaceTupleTypeWithStructTypeSeen: map[typed.TypeId]typed.Type{},
	}
	walker := DefaultTransformWalker{Transformer: transformer}
	module, ok := walker.WalkModule(module)
	if !ok {
		panic("module has been removed")
	}
	return module
}
