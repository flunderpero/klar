/*
# Union Lowering

This pass will convert all union types to struct types.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type unionLowering struct {
	DefaultTransformer
	typeInfo                           *typed.TypeInfo
	typeCreator                        *typed.TypeCreator
	nodeCreator                        *ast.NodeCreator
	unionStructType                    *typed.StructType
	replaceUnionTypeWithStructTypeSeen map[typed.TypeId]typed.Type
}

func FindUnionVariantTag(unionType *typed.UnionType, variantType typed.Type) int {
	tag := -1
	for i, variant := range unionType.Variants {
		if variantType.Id() == variant.AsType().Id() {
			tag = i
			break
		}
	}
	if tag == -1 {
		panic(fmt.Sprintf("unexpected union variant %s for union type %s", variantType, unionType))
	}
	return tag
}

// Create a `ast.CallExpression` that creates the `self.unionStructType` from the given valueExpr.
func (self *unionLowering) createUnionStructCallExpression(unionType *typed.UnionType, valueExpr ast.Expression) *ast.CallExpression {
	valueType := self.typeInfo.MustLookup(valueExpr)
	tag := FindUnionVariantTag(unionType, valueType)
	callArgs := make([]ast.CallArg, 2)
	tagExpr := self.nodeCreator.NewSignedIntLiteralExpression(int64(tag), valueExpr.Span())
	self.typeInfo.Set(tagExpr, &typed.Int64Type{})
	callArgs[0] = ast.CallArg{Value: tagExpr, Span: valueExpr.Span()}
	callArgs[1] = ast.CallArg{Value: valueExpr, Span: valueExpr.Span()}
	calleeExpr := self.nodeCreator.NewIdentExpression(ast.Ident("$UnionLowering"), valueExpr.Span())
	self.typeInfo.Set(calleeExpr, self.unionStructType)
	callExpr := self.nodeCreator.NewCallExpression(calleeExpr, callArgs, valueExpr.Span())
	self.typeInfo.Set(callExpr, self.unionStructType)
	return callExpr
}

func (self *unionLowering) VisitVariableDefinition(v *ast.VariableDefinition, w TransformWalker) (*ast.VariableDefinition, bool) {
	v, ok := w.WalkVariableDefinition(v)
	if !ok {
		return nil, false
	}
	varType := self.typeInfo.MustLookup(v).(*typed.VariableType)
	ty, ok := varType.Type.(*typed.UnionType)
	if !ok {
		return v, true
	}
	if _, ok := self.typeInfo.MustLookup(v.Value).(*typed.StructType); ok {
		// The value expression has already been converted to a struct type - nothing to do.
		return v, true
	}
	v.Value = self.createUnionStructCallExpression(ty, v.Value)
	return v, true
}

func (self *unionLowering) VisitAssignmentStatement(expr *ast.AssignmentStatement, w TransformWalker) (*ast.AssignmentStatement, bool) {
	expr, ok := w.WalkAssignmentStatement(expr)
	if !ok {
		return nil, false
	}
	ty, ok := self.typeInfo.MustLookup(expr.Variable).(*typed.UnionType)
	if !ok {
		return expr, true
	}
	if _, ok := self.typeInfo.MustLookup(expr.Rhs).(*typed.StructType); ok {
		// The value expression has already been converted to a struct type - nothing to do.
		return expr, true
	}
	expr.Rhs = self.createUnionStructCallExpression(ty, expr.Rhs)
	return expr, true
}

func (self *unionLowering) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkMemberExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.MemberExpression)
	if !ok {
		return newExpr, true
	}
	namedVariantType, ok := self.typeInfo.MustLookup(expr).(*typed.NamedUnionVariant)
	if !ok || !namedVariantType.IsUnitVariant() {
		return expr, true
	}
	valueExpr := self.nodeCreator.NewTupleLiterarExpression([]ast.Expression{}, expr.Span())
	self.typeInfo.Set(valueExpr, namedVariantType)
	callExpr := self.createUnionStructCallExpression(namedVariantType.UnionType, valueExpr)
	// We need to set the type of the `valueExpr` to the namedVariantType's inner type
	// to not mess up following passes like tuple lowering.
	self.typeInfo.Set(valueExpr, namedVariantType.Type)
	return callExpr, true
}

// Convert named union variant initialization calls to calls to initiate the union struct:
//
//	Color.RGB(1, 2, 3)
//
//	will become:
//
//	UnionStruct(tag=1, data=(1, 2, 3)) -- `tag` is the tag of `Color.RGB` in this example.
func (self *unionLowering) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.CallExpression)
	if !ok {
		return newExpr, true
	}
	namedVariantType, ok := self.typeInfo.MustLookup(expr).(*typed.NamedUnionVariant)
	if !ok {
		return expr, true
	}
	expr = self.createUnionStructCallExpression(namedVariantType.UnionType, expr)
	self.typeInfo.Set(expr.Args[1].Value, namedVariantType.Type)
	return expr, true
}

func (self *unionLowering) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	node, ok := w.WalkNode(node)
	if !ok {
		return nil, false
	}
	ty := self.typeInfo.MustLookup(node)
	ty = self.replaceUnionTypeWithStructType(ty)
	self.typeInfo.Set(node, ty)
	return node, true
}

func (self *unionLowering) replaceUnionTypeWithStructType(ty typed.Type) typed.Type {
	// todo: `typed.DeclaredType` should not have the same `TypeId` as its enclosed type.
	if _, ok := ty.(*typed.DeclaredType); ok {
		return ty
	}
	if res, ok := self.replaceUnionTypeWithStructTypeSeen[ty.Id()]; ok {
		return res
	}
	switch tyKind := ty.(type) {
	case *typed.UnionType:
		ty = self.unionStructType
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
		for _, variant := range tyKind.Variants {
			switch variant.Kind {
			case typed.UnionVariantKindType:
				variant.Type = self.replaceUnionTypeWithStructType(variant.Type)
			case typed.UnionVariantKindNamed:
				variant.Named.Type = self.replaceUnionTypeWithStructType(variant.Named.Type).(*typed.TupleType)
			default:
				panic(fmt.Sprintf("unexpected union variant kind: %d", variant.Kind))
			}
		}
	case *typed.TupleType:
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
		for i, value := range tyKind.Values {
			tyKind.Values[i] = self.replaceUnionTypeWithStructType(value)
		}
	case *typed.FunctionType:
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
		for i, param := range tyKind.Params {
			param.Type = self.replaceUnionTypeWithStructType(param.Type)
			tyKind.Params[i] = param
		}
		tyKind.Result = self.replaceUnionTypeWithStructType(tyKind.Result)
		if tyKind.Receiver != nil {
			tyKind.Receiver = self.replaceUnionTypeWithStructType(tyKind.Receiver)
		}
	case *typed.StructType:
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
		for i, field := range tyKind.Fields {
			field.Type = self.replaceUnionTypeWithStructType(field.Type)
			tyKind.Fields[i] = field
		}
		for i, method := range tyKind.Methods {
			method.Type = self.replaceUnionTypeWithStructType(method.Type).(*typed.FunctionType)
			tyKind.Methods[i] = method
		}
	case *typed.NamedUnionVariant, *typed.NamedUnionVariantConstructor:
		ty = self.unionStructType
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
	case *typed.BoolType,
		*typed.Int64Type,
		*typed.Int32Type,
		*typed.Int16Type,
		*typed.Int8Type,
		*typed.UInt64Type,
		*typed.UInt32Type,
		*typed.UInt16Type,
		*typed.UInt8Type,
		*typed.StrType,
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

func UnionLowering(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	typeCreator *typed.TypeCreator,
	nodeCreator *ast.NodeCreator,
	unionStructType *typed.StructType) *ast.Module {
	transformer := &unionLowering{
		typeInfo:                           typeInfo,
		typeCreator:                        typeCreator,
		nodeCreator:                        nodeCreator,
		unionStructType:                    unionStructType,
		replaceUnionTypeWithStructTypeSeen: map[typed.TypeId]typed.Type{},
	}
	walker := DefaultTransformWalker{Transformer: transformer}
	module, ok := walker.WalkModule(module)
	if !ok {
		panic("module has been removed")
	}
	return module
}
