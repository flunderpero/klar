/*
# Match Lowering

This pass will convert all match expressions into if/else chains.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
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
	exprType := self.typeInfo.MustLookup(match.Expression)
	// First assign the result of the expression to a local variable so we don't re-evaluate it all
	// the time. But only do so if it is not already an `ast.IdentExpression`.
	var exprIdentExpr *ast.IdentExpression
	if e, ok := match.Expression.(*ast.IdentExpression); ok {
		exprIdentExpr = e
	} else {
		exprVarIdent := ast.Ident("__match_lowering_expr")
		exprIdentExpr = self.nodeCreator.NewIdentExpression(exprVarIdent, match.Expression.Span())
		self.typeInfo.Set(exprIdentExpr, exprType)
		exprVarDef := self.nodeCreator.NewVariableDefinition(
			exprVarIdent,
			nil,
			false,
			match.Expression,
			match.Expression.Span(),
		)
		self.typeInfo.Set(exprVarDef, &typed.VariableType{Type: exprType, Span: match.Span()})
		block.Nodes = append(block.Nodes, exprVarDef)
	}
	for _, arm := range match.Arms {
		var condition ast.Expression
		switch pattern := arm.Pattern.(type) {
		case *ast.UnionTypePattern:
			unionType := exprType.(*typed.UnionType)
			var valueType typed.Type
			if pattern.Type != nil {
				valueType = self.typeInfo.MustLookup(pattern.Type)
			}
			if pattern.NamedVariant != "" {
				valueType_, ok := unionType.FindNamedVariant(pattern.NamedVariant)
				if !ok {
					panic(fmt.Sprintf("variant %s not found in union type %s", pattern.NamedVariant, unionType))
				}
				valueType = valueType_
			}
			tag := FindUnionVariantTag(unionType, valueType)
			lhs := self.nodeCreator.NewMemberExpression(exprIdentExpr, "tag", arm.Span())
			self.typeInfo.Set(lhs, &typed.Int64Type{})
			rhs := self.nodeCreator.NewSignedIntLiteralExpression(int64(tag), arm.Span())
			self.typeInfo.Set(rhs, &typed.Int64Type{})
			condition = self.nodeCreator.NewBinaryExpression(lhs, ast.OpEqual, rhs, arm.Span())
			if arm.Alias != nil {
				varType := valueType
				if pattern.NamedVariant != "" {
					varType = valueType.(*typed.NamedUnionVariant).Type
				}
				getData := self.nodeCreator.NewMemberExpression(exprIdentExpr, "data", arm.Alias.Span())
				self.typeInfo.Set(getData, varType)
				varDef := self.nodeCreator.NewVariableDefinition(arm.Alias.Ident, nil, false, getData, arm.Alias.Span())
				self.typeInfo.Set(varDef, &typed.VariableType{Type: varType, Span: arm.Alias.Span()})
				arm.Body.Nodes = append([]ast.Node{varDef}, arm.Body.Nodes...)
			}
		case *ast.IntLiteralPattern:
			condition = self.nodeCreator.NewBinaryExpression(exprIdentExpr, ast.OpEqual, &pattern.Value, arm.Span())
			if arm.Alias != nil {
				varType := self.typeInfo.MustLookup(&pattern.Value)
				varDef := self.nodeCreator.NewVariableDefinition(arm.Alias.Ident, nil, false, &pattern.Value, arm.Alias.Span())
				self.typeInfo.Set(varDef, &typed.VariableType{Type: varType, Span: arm.Alias.Span()})
				arm.Body.Nodes = append([]ast.Node{varDef}, arm.Body.Nodes...)
			}
		default:
			panic(fmt.Sprintf("unhandled pattern type: %T", arm.Pattern))
		}
		self.typeInfo.Set(condition, &typed.BoolType{})
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
