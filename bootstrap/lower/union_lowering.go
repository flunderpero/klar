/*
# Union Lowering

This pass will convert all union types to struct types.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type unionLoweringStage1 struct {
	DefaultTransformer
	typeInfo        *typed.TypeInfo
	nodeCreator     *ast.NodeCreator
	unionStructType *typed.StructType
	nodePath        []ast.Node
}

func (self *unionLoweringStage1) createUnionStructCallExpression(unionType *typed.UnionType, valueExpr ast.Expression) *ast.CallExpression {
	valueType := self.typeInfo.MustLookup(valueExpr)
	tag := FindUnionVariantTag(unionType, valueType)
	callArgs := make([]ast.CallArg, 2)
	tagExpr := self.nodeCreator.NewSignedIntLiteralExpression(int64(tag), valueExpr.Span())
	self.typeInfo.Set(tagExpr, &typed.Int64Type{})
	valueExprType := self.typeInfo.MustLookup(valueExpr)
	if valueExprType.Id() == self.typeInfo.BuiltIns.None.Id() {
		valueExpr = self.nodeCreator.NewSignedIntLiteralExpression(0, valueExpr.Span())
		self.typeInfo.Set(valueExpr, &typed.Int64Type{})
	}
	callArgs[0] = ast.CallArg{Value: tagExpr, Span: valueExpr.Span()}
	callArgs[1] = ast.CallArg{Value: valueExpr, Span: valueExpr.Span()}
	calleeExpr := self.nodeCreator.NewIdentExpression(ast.Ident("$UnionLowering"), valueExpr.Span())
	self.typeInfo.Set(calleeExpr, self.unionStructType)
	callExpr := self.nodeCreator.NewCallExpression(calleeExpr, callArgs, valueExpr.Span())
	self.typeInfo.Set(callExpr, self.unionStructType)
	return callExpr
}

func (self *unionLoweringStage1) createEmptyUnionStructCallExpression(unionType *typed.UnionType, namedUnionVariant *typed.NamedUnionVariant, span token.Span) *ast.CallExpression {
	tag := FindUnionVariantTag(unionType, namedUnionVariant)
	callArgs := make([]ast.CallArg, 2)
	tagExpr := self.nodeCreator.NewSignedIntLiteralExpression(int64(tag), span)
	self.typeInfo.Set(tagExpr, &typed.Int64Type{})
	valueExpr := self.nodeCreator.NewSignedIntLiteralExpression(0, span)
	self.typeInfo.Set(valueExpr, &typed.Int64Type{})
	callArgs[0] = ast.CallArg{Value: tagExpr, Span: span}
	callArgs[1] = ast.CallArg{Value: valueExpr, Span: span}
	calleeExpr := self.nodeCreator.NewIdentExpression(ast.Ident("$UnionLowering"), span)
	self.typeInfo.Set(calleeExpr, self.unionStructType)
	callExpr := self.nodeCreator.NewCallExpression(calleeExpr, callArgs, span)
	self.typeInfo.Set(callExpr, self.unionStructType)
	return callExpr
}

func (self *unionLoweringStage1) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	namedVariantType, isNamedVariant := self.typeInfo.MustLookup(expr).(*typed.NamedUnionVariant)
	visited, ok := w.WalkMemberExpression(expr)
	visitMustNotChange(expr, visited, ok)
	if !isNamedVariant || !namedVariantType.IsUnitVariant() {
		return expr, true
	}
	callExpr := self.createEmptyUnionStructCallExpression(namedVariantType.UnionType, namedVariantType, expr.Span())
	return callExpr, true
}

func (self *unionLoweringStage1) findFinalNodeType(node ast.Node) typed.Type {
	nodeType := self.typeInfo.MustLookup(node)
	nodeUnionType, ok := nodeType.(*typed.UnionType)
	if ok && !nodeUnionType.IsAnonymous {
		return nodeUnionType
	}
	for i := len(self.nodePath) - 2; i >= 0; i-- {
		parent := self.nodePath[i]
		parentType := self.typeInfo.MustLookup(parent)
		switch parent := parent.(type) {
		case *ast.IfExpression:
			previous := self.nodePath[i+1]
			if previous == parent.Condition {
				return nodeType
			}
			if parentUnionType, ok := parentType.(*typed.UnionType); ok && parentUnionType.IsAssignableFrom(nodeType) {
				nodeType = parentUnionType
			} else {
				return nodeType
			}
		case *ast.BlockExpression:
			if node != parent.Nodes[len(parent.Nodes)-1] {
				return nodeType
			}
			if _, ok := node.(*ast.ReturnStatement); ok {
				return nodeType
			}
			if parentUnionType, ok := parentType.(*typed.UnionType); ok && parentUnionType.IsAssignableFrom(nodeType) {
				nodeType = parentUnionType
			}
		case *ast.VariableDefinition:
			parentTy := parentType.(*typed.VariableType).Type
			if parentUnionType, ok := parentTy.(*typed.UnionType); ok && parentUnionType.IsAssignableFrom(nodeType) {
				nodeType = parentUnionType
			}
			return nodeType
		case *ast.AssignmentStatement:
			previous := self.nodePath[i+1]
			if previous == parent.Target {
				return nodeType
			}
			parentTy := self.typeInfo.MustLookup(parent.Target)
			if parentUnionType, ok := parentTy.(*typed.UnionType); ok && parentUnionType.IsAssignableFrom(nodeType) {
				nodeType = parentUnionType
			}
			return nodeType
		case *ast.CallExpression:
			previous := self.nodePath[i+1]
			if previous == parent.Callee {
				return nodeType
			}
			params := self.typeInfo.MustLookup(parent.Callee).(typed.CallableType).CallParams()
			for i, arg := range parent.Args {
				if arg.Value != previous {
					continue
				}
				paramType := params[i].Type
				if paramUnionType, ok := paramType.(*typed.UnionType); ok && paramUnionType.IsAssignableFrom(nodeType) {
					return paramUnionType
				}
			}
			return nodeType
		case *ast.FunctionDefinition:
			funcType := parentType.(*typed.DeclaredType).Type.(*typed.FunctionType)
			if parentUnionType, ok := funcType.Result.(*typed.UnionType); ok && parentUnionType.IsAssignableFrom(nodeType) {
				return parentUnionType
			}
			return nodeType
		case *ast.ReturnStatement:
			var funcType *typed.FunctionType
			for {
				i -= 1
				if i < 0 {
					panic("return statement not in function")
				}
				if funcDef, ok := self.nodePath[i].(*ast.FunctionDefinition); ok {
					funcType = self.typeInfo.MustLookup(funcDef).(*typed.DeclaredType).Type.(*typed.FunctionType)
					break
				}
			}
			if unionType, ok := funcType.Result.(*typed.UnionType); ok && unionType.IsAssignableFrom(nodeType) {
				return unionType
			}
		default:
			return nodeType
		}
	}
	return nodeType

}

func (self *unionLoweringStage1) VisitExpression(expr ast.Expression, w TransformWalker) (ast.Expression, bool) {
	if blockExpr, ok := expr.(*ast.BlockExpression); ok {
		// The block expression is transient, its result is already converted
		// if it is a union type.
		return w.WalkBlockExpression(blockExpr)
	}
	expr, ok := w.WalkExpression(expr)
	visitMustNotRemove(expr, ok)
	if expr, ok := expr.(*ast.CallExpression); ok {
		namedVariantType, ok := self.typeInfo.MustLookup(expr).(*typed.NamedUnionVariant)
		if ok {
			expr = self.createUnionStructCallExpression(namedVariantType.UnionType, expr)
			self.typeInfo.Set(expr.Args[1].Value, namedVariantType.Type)
		}
	}
	ty := self.typeInfo.MustLookup(expr)
	finalTy := self.findFinalNodeType(expr)
	if unionType, ok := finalTy.(*typed.UnionType); ok && ty.Id() != finalTy.Id() {
		expr = self.createUnionStructCallExpression(unionType, expr)
	}
	return expr, true
}

func (self *unionLoweringStage1) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	self.nodePath = append(self.nodePath, node)
	node, ok := w.WalkNode(node)
	visitMustNotRemove(node, ok)
	self.nodePath = self.nodePath[:len(self.nodePath)-1]
	return node, true
}

func (self *unionLoweringStage1) VisitMatchExpression(match *ast.MatchExpression, w TransformWalker) (ast.Expression, bool) {
	panic("match expressions should have been lowered before")
}

type unionLoweringStage2 struct {
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
		panic(fmt.Sprintf("unexpected union variant %s\n for union type %s", variantType, unionType))
	}
	return tag
}

func (self *unionLoweringStage2) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	node, ok := w.WalkNode(node)
	visitMustNotRemove(node, ok)
	ty := self.typeInfo.MustLookup(node)
	ty = self.replaceUnionTypeWithStructType(ty)
	self.typeInfo.Set(node, ty)
	return node, true
}

func (self *unionLoweringStage2) replaceUnionTypeWithStructType(ty typed.Type) typed.Type {
	// todo: `typed.DeclaredType` should not have the same `TypeId` as its enclosed type.
	if _, ok := ty.(*typed.DeclaredType); ok {
		return ty
	}
	if res, ok := self.replaceUnionTypeWithStructTypeSeen[ty.Id()]; ok {
		return res
	}
	if genericType, ok := ty.(typed.GenericType); ok {
		typeArgs := genericType.TypeArgs()
		for i, typeArg := range typeArgs {
			if _, ok := typeArg.(typed.TypeParam); ok {
				continue
			}
			typeArgs[i] = self.replaceUnionTypeWithStructType(typeArg)
		}
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
	stage1Transformer := &unionLoweringStage1{
		typeInfo:        typeInfo,
		nodeCreator:     nodeCreator,
		unionStructType: unionStructType,
		nodePath:        []ast.Node{module},
	}
	stage1Walker := DefaultTransformWalker{Transformer: stage1Transformer}
	module, ok := stage1Walker.WalkModule(module)
	if !ok {
		panic("module has been removed")
	}
	stage2Transformer := &unionLoweringStage2{
		typeInfo:                           typeInfo,
		typeCreator:                        typeCreator,
		nodeCreator:                        nodeCreator,
		unionStructType:                    unionStructType,
		replaceUnionTypeWithStructTypeSeen: map[typed.TypeId]typed.Type{},
	}
	stage2Walker := DefaultTransformWalker{Transformer: stage2Transformer}
	module, ok = stage2Walker.WalkModule(module)
	if !ok {
		panic("module has been removed")
	}
	return module
}
