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

type unionLoweringStage1 struct {
	DefaultTransformer
	typeInfo        *typed.TypeInfo
	nodeCreator     *ast.NodeCreator
	unionStructType *typed.StructType
}

func (self *unionLoweringStage1) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	calleeType := self.typeInfo.MustLookup(expr.Callee).(typed.CallableType)
	for i, param := range calleeType.CallParams() {
		paramType, ok := param.Type.(*typed.UnionType)
		if !ok {
			continue
		}
		argType := self.typeInfo.MustLookup(expr.Args[i].Value)
		if paramType.Id() == argType.Id() {
			continue
		}
		if _, ok := argType.(*typed.NamedUnionVariant); ok {
			continue
		}
		expr.Args[i].Value = createUnionStructCallExpression(
			paramType, expr.Args[i].Value, self.typeInfo, self.nodeCreator, self.unionStructType)
	}
	visited, ok := w.WalkCallExpression(expr)
	visitMustNotChange(expr, visited, ok)
	return expr, true
}

type unionLoweringStage2 struct {
	DefaultTransformer
	typeInfo                           *typed.TypeInfo
	typeCreator                        *typed.TypeCreator
	nodeCreator                        *ast.NodeCreator
	unionStructType                    *typed.StructType
	replaceUnionTypeWithStructTypeSeen map[typed.TypeId]typed.Type
	funcType                           *typed.FunctionType
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
func createUnionStructCallExpression(
	unionType *typed.UnionType,
	valueExpr ast.Expression,
	typeInfo *typed.TypeInfo,
	nodeCreator *ast.NodeCreator,
	unionStructType *typed.StructType,
) *ast.CallExpression {
	valueType := typeInfo.MustLookup(valueExpr)
	tag := FindUnionVariantTag(unionType, valueType)
	callArgs := make([]ast.CallArg, 2)
	tagExpr := nodeCreator.NewSignedIntLiteralExpression(int64(tag), valueExpr.Span())
	typeInfo.Set(tagExpr, &typed.Int64Type{})
	callArgs[0] = ast.CallArg{Value: tagExpr, Span: valueExpr.Span()}
	callArgs[1] = ast.CallArg{Value: valueExpr, Span: valueExpr.Span()}
	calleeExpr := nodeCreator.NewIdentExpression(ast.Ident("$UnionLowering"), valueExpr.Span())
	typeInfo.Set(calleeExpr, unionStructType)
	callExpr := nodeCreator.NewCallExpression(calleeExpr, callArgs, valueExpr.Span())
	typeInfo.Set(callExpr, unionStructType)
	return callExpr
}

func (self *unionLoweringStage2) createUnionStructCallExpression(ty *typed.UnionType, valueExpr ast.Expression) *ast.CallExpression {
	return createUnionStructCallExpression(ty, valueExpr, self.typeInfo, self.nodeCreator, self.unionStructType)
}

func (self *unionLoweringStage2) VisitVariableDefinition(v *ast.VariableDefinition, w TransformWalker) (*ast.VariableDefinition, bool) {
	visited, ok := w.WalkVariableDefinition(v)
	visitMustNotChange(v, visited, ok)
	varType := self.typeInfo.MustLookup(v).(*typed.VariableType)
	ty, ok := varType.Type.(*typed.UnionType)
	if !ok {
		return v, true
	}
	if self.typeInfo.MustLookup(v.Value).Id() == self.unionStructType.Id() {
		// The value expression has already been converted to a struct type - nothing to do.
		return v, true
	}
	v.Value = self.createUnionStructCallExpression(ty, v.Value)
	return v, true
}

func (self *unionLoweringStage2) VisitAssignmentStatement(expr *ast.AssignmentStatement, w TransformWalker) (*ast.AssignmentStatement, bool) {
	visited, ok := w.WalkAssignmentStatement(expr)
	visitMustNotChange(expr, visited, ok)
	ty, ok := self.typeInfo.MustLookup(expr.Target).(*typed.UnionType)
	if !ok {
		return expr, true
	}
	if self.typeInfo.MustLookup(expr.Value).Id() == self.unionStructType.Id() {
		// The value expression has already been converted to a struct type - nothing to do.
		return expr, true
	}
	expr.Value = self.createUnionStructCallExpression(ty, expr.Value)
	return expr, true
}

func (self *unionLoweringStage2) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkMemberExpression(expr)
	visitMustNotChange(expr, visited, ok)
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
func (self *unionLoweringStage2) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	visited, ok := w.WalkCallExpression(expr)
	visitMustNotChange(expr, visited, ok)
	namedVariantType, ok := self.typeInfo.MustLookup(expr).(*typed.NamedUnionVariant)
	if !ok {
		return expr, true
	}
	expr = self.createUnionStructCallExpression(namedVariantType.UnionType, expr)
	self.typeInfo.Set(expr.Args[1].Value, namedVariantType.Type)
	return expr, true
}

func (self *unionLoweringStage2) VisitFunctionDefinition(fn *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool) {
	if self.funcType != nil {
		panic("nested functions should have been hoisted before")
	}
	self.funcType = self.typeInfo.MustLookup(fn).(*typed.DeclaredType).Type.(*typed.FunctionType)
	if unionType, ok := self.funcType.Result.(*typed.UnionType); ok && len(fn.Body.Nodes) > 0 {
		lastNode := fn.Body.Nodes[len(fn.Body.Nodes)-1]
		if _, ok := lastNode.(*ast.ReturnStatement); !ok {
			lastNodeType := self.typeInfo.MustLookup(lastNode)
			if lastNodeType.Id() != unionType.Id() {
				fn.Body.Nodes[len(fn.Body.Nodes)-1] = self.createUnionStructCallExpression(unionType, lastNode)
			}
		}
	}
	visited, ok := w.WalkFunctionDefinition(fn)
	visitMustNotChange(fn, visited, ok)
	self.funcType = nil
	return fn, true
}

func (self *unionLoweringStage2) VisitReturnStatement(stmt *ast.ReturnStatement, w TransformWalker) (*ast.ReturnStatement, bool) {
	if unionType, ok := self.funcType.Result.(*typed.UnionType); ok {
		retType := self.typeInfo.MustLookup(stmt.Value)
		if retType.Id() != unionType.Id() {
			stmt.Value = self.createUnionStructCallExpression(unionType, stmt.Value)
		}
	}
	visited, ok := w.WalkReturnStatement(stmt)
	visitMustNotChange(stmt, visited, ok)
	return stmt, true
}

func (self *unionLoweringStage2) VisitMatchExpression(match *ast.MatchExpression, w TransformWalker) (ast.Expression, bool) {
	panic("match expressions should have been lowered before")
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
	case *typed.ArrayType:
		self.replaceUnionTypeWithStructTypeSeen[ty.Id()] = ty
		tyKind.SetElementType(self.replaceUnionTypeWithStructType(tyKind.ElementType()))
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
