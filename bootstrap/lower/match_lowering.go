/*
# Match Lowering

This pass will convert all match expressions into if/else chains.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type matchLowering struct {
	DefaultTransformer
	typeInfo                           *typed.TypeInfo
	typeCreator                        *typed.TypeCreator
	nodeCreator                        *ast.NodeCreator
	unionStructType                    *typed.StructType
	replaceUnionTypeWithStructTypeSeen map[typed.TypeId]typed.Type
}

func (self *matchLowering) VisitMatchExpression(match *ast.MatchExpression, t TransformWalker) (ast.Expression, bool) {
	// First create a block that will replace the given `match` expression.
	matchType := self.typeInfo.MustLookup(match)
	block := self.nodeCreator.NewBlockExpression([]ast.Node{}, match.Span())
	self.typeInfo.Set(block, matchType)
	var result *ast.IfExpression
	var lastIfExpr *ast.IfExpression
	matchedValueType := self.typeInfo.MustLookup(match.Expression)
	// Assign the result of the expression to a local variable so we don't re-evaluate it all
	// the time. But only do so if it is not already an `ast.IdentExpression`.
	var matchedValueExpr *ast.IdentExpression
	if e, ok := match.Expression.(*ast.IdentExpression); ok {
		matchedValueExpr = e
	} else {
		ident := ast.Ident("$match_lowering_expr")
		matchedValueExpr = self.nodeCreator.NewIdentExpression(ident, match.Expression.Span())
		self.typeInfo.Set(matchedValueExpr, matchedValueType)
		varDef := self.nodeCreator.NewVariableDefinition(ident, nil, false, match.Expression, match.Expression.Span())
		self.typeInfo.Set(varDef, &typed.VariableType{Type: matchedValueType, Span: match.Span()})
		block.Nodes = append(block.Nodes, varDef)
	}
	for _, arm := range match.Arms {
		condition := self.buildPatternCondition(arm, matchedValueType, matchedValueExpr)
		self.typeInfo.Set(condition, &typed.BoolType{})
		if _, isUnionTypePattern := arm.Pattern.(*ast.UnionTypePattern); !isUnionTypePattern && arm.Alias != nil {
			varDef := self.nodeCreator.NewVariableDefinition(arm.Alias.Ident, nil, false, matchedValueExpr, arm.Alias.Span())
			self.typeInfo.Set(varDef, &typed.VariableType{Type: matchedValueType, Span: arm.Alias.Span()})
			arm.Body.Nodes = append([]ast.Node{varDef}, arm.Body.Nodes...)
		}
		ifExpr := self.nodeCreator.NewIfExpression(condition, arm.Body, nil, arm.Span())
		// todo: The type of the if expression should be the union type of its branches
		self.typeInfo.Set(ifExpr, &typed.NoneType{})
		if lastIfExpr != nil {
			lastIfExpr.FalseBody = self.nodeCreator.NewBlockExpression([]ast.Node{ifExpr}, ifExpr.Span())
			// todo: The type of the false body should be calculated properly
			self.typeInfo.Set(lastIfExpr.FalseBody, &typed.NoneType{})
		}
		lastIfExpr = ifExpr
		if result == nil {
			result = ifExpr
		}
	}
	block.Nodes = append(block.Nodes, result)
	return block, true
}

func (self *matchLowering) buildPatternCondition(
	arm *ast.MatchArm, matchedValueType typed.Type, matchedValueExpr ast.Expression) ast.Expression {
	switch pattern := arm.Pattern.(type) {
	case *ast.UnionTypePattern:
		unionType := matchedValueType.(*typed.UnionType)
		var valueType typed.Type
		valueType = self.typeInfo.MustLookup(pattern)
		tag := FindUnionVariantTag(unionType, valueType)
		lhs := self.nodeCreator.NewMemberExpression(matchedValueExpr, "tag", arm.Span())
		self.typeInfo.Set(lhs, &typed.Int64Type{})
		rhs := self.nodeCreator.NewSignedIntLiteralExpression(int64(tag), arm.Span())
		self.typeInfo.Set(rhs, &typed.Int64Type{})
		if arm.Alias != nil {
			// We need to handle the alias directly in here because it needs to be handled
			// differently from all other patterns.
			varType := valueType
			if pattern.NamedVariant != "" {
				varType = valueType.(*typed.NamedUnionVariant).Type
			}
			getData := self.nodeCreator.NewMemberExpression(matchedValueExpr, "data", arm.Alias.Span())
			self.typeInfo.Set(getData, varType)
			varDef := self.nodeCreator.NewVariableDefinition(arm.Alias.Ident, nil, false, getData, arm.Alias.Span())
			self.typeInfo.Set(varDef, &typed.VariableType{Type: varType, Span: arm.Alias.Span()})
			arm.Body.Nodes = append([]ast.Node{varDef}, arm.Body.Nodes...)
		}
		return self.nodeCreator.NewBinaryExpression(lhs, ast.OpEqual, rhs, arm.Span())
	case *ast.WildcardPattern:
		trueExpr := self.nodeCreator.NewBoolLiteralExpression(true, pattern.Span())
		self.typeInfo.Set(trueExpr, &typed.BoolType{})
		return trueExpr
	case *ast.IntPattern:
		return self.nodeCreator.NewBinaryExpression(matchedValueExpr, ast.OpEqual, &pattern.Value, pattern.Span())
	case *ast.CharPattern:
		return self.nodeCreator.NewBinaryExpression(matchedValueExpr, ast.OpEqual, &pattern.Value, pattern.Span())
	case *ast.StrPattern:
		strType := self.typeInfo.BuiltIns.Str
		eqFunc, ok := strType.FindMethod("eq")
		if !ok {
			panic("eq method not found in Str type")
		}
		str := self.nodeCreator.NewIdentExpression(ast.Ident("Str"), pattern.Span())
		self.typeInfo.Set(str, strType)
		strEq := self.nodeCreator.NewMemberExpression(str, "eq", pattern.Span())
		self.typeInfo.Set(strEq, eqFunc)
		return self.nodeCreator.NewCallExpression(strEq, []ast.CallArg{
			{Value: matchedValueExpr, Span: matchedValueExpr.Span()},
			{Value: &pattern.Value, Span: pattern.Value.Span()}}, pattern.Span())
	case *ast.IntRangePattern:
		return self.buildRangePatternCondition(matchedValueExpr, &pattern.From, &pattern.To, pattern.InclusiveTo, pattern.Span())
	case *ast.CharRangePattern:
		return self.buildRangePatternCondition(matchedValueExpr, &pattern.From, &pattern.To, pattern.InclusiveTo, pattern.Span())
	}
	panic(fmt.Sprintf("unhandled pattern type: %T", arm.Pattern))
}

func (self *matchLowering) buildRangePatternCondition(
	matchedValueExpr ast.Expression, from ast.Expression, to ast.Expression, inclusiveTo bool, span token.Span) ast.Expression {
	fromExpr := self.nodeCreator.NewBinaryExpression(
		matchedValueExpr, ast.OpGreaterThanOrEqual, from, from.Span())
	self.typeInfo.Set(fromExpr, &typed.BoolType{})
	toOp := ast.OpLessThan
	if inclusiveTo {
		toOp = ast.OpLessThanOrEqual
	}
	toExpr := self.nodeCreator.NewBinaryExpression(
		matchedValueExpr, toOp, to, to.Span())
	self.typeInfo.Set(toExpr, &typed.BoolType{})
	return self.nodeCreator.NewBinaryExpression(fromExpr, ast.OpAnd, toExpr, span)
}

func MatchLowering(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	typeCreator *typed.TypeCreator,
	nodeCreator *ast.NodeCreator,
	unionStructType *typed.StructType) *ast.Module {
	transformer := &matchLowering{
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
