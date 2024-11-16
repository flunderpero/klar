/*
This lowering pass will:

  - rename all types (functions, structs) to the the type id to make them globally
    unique and adapt all `ReferenceExpression` accordingly.

  - change the type of a `ReferenceExpression` that references a function with
    `IdentExpression` to the correct `TypeExpression`.

  - convert `MemberExpression` to `TypeExpression` if it points to a static method.

  - convert all calls to methods to regular function calls with the receiver
    as the first argument if the method is not static.

  - remove all trait declarations.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

// 640kb will be enough forever. :-)
const lowerNodeIdStart = 1_000_000_000

type lower struct {
	DefaultTransformer
	typeInfo       *typed.TypeInfo
	loweredMethods map[typed.TypeId]*typed.FunctionType
	nextNodeId     int
}

func (l *lower) newNodeId() ast.NodeId {
	l.nextNodeId++
	return ast.NodeId(l.nextNodeId)
}

func (l *lower) convertMethodToFunction(method *typed.MethodType) *typed.FunctionType {
	functionType, found := l.loweredMethods[method.Id()]
	if found {
		return functionType
	}
	functionType = &typed.FunctionType{
		BaseType:   typed.NewBaseType(method.Id()),
		ArgTypes:   method.ArgTypes,
		ReturnType: method.ReturnType,
	}
	l.loweredMethods[method.Id()] = functionType
	return functionType
}

func (l *lower) convertToTypeExpression(expr ast.Expression, ty typed.Type) *ast.TypeExpression {
	simpleType := ast.NewSimpleType(ast.TypeIdent(ty.Id().String()), l.newNodeId(), expr.Span())
	return ast.NewTypeExpression(simpleType, expr.Id(), expr.Span())

}

// Convert and `IdentExpression` to a `TypeExpression` if it points to a function type.
func (l *lower) VisitReferenceExpression(expr ast.ReferenceExpression) (ast.Expression, bool) {
	ty, isTypeReference := l.typeInfo.LookupTypeBinding(expr)
	if !isTypeReference {
		return expr, true
	}
	switch exprKind := expr.(type) {
	case *ast.IdentExpression:
		if _, isFunction := ty.(*typed.FunctionType); isFunction {
			// Functions could not be determined as TypeExpressions during parsing because the
			// parser can only identify TypeIdent as a TypeExpression. Now that typechecking
			// has confirmed the type, we can convert the expression from an `IdentExpression`
			// to the proper `TypeExpression`.
			expr = l.convertToTypeExpression(expr, ty)
		}
	case *ast.TypeExpression:
		simpleType := ast.NewSimpleType(ast.TypeIdent(ty.Id().String()), exprKind.Type.Id(), expr.Span())
		exprKind.Type = simpleType
	default:
		panic(fmt.Sprintf("unexpected type reference: %T", expr))
	}
	return expr, true
}

// Convert static methods to regular functions.
func (l *lower) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkMemberExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.MemberExpression)
	if !ok {
		return newExpr, false
	}
	method, isMethod := l.typeInfo.MustLookup(expr).(*typed.MethodType)
	if !isMethod || !method.IsStatic {
		return expr, true
	}
	functionType := l.convertMethodToFunction(method)
	res := l.convertToTypeExpression(expr, functionType)
	l.typeInfo.Set(res, functionType)
	return res, true
}

// Convert method calls to calls to the converted regular function.
func (l *lower) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.CallExpression)
	if !ok {
		return newExpr, true
	}
	method, ok := l.typeInfo.MustLookup(expr.Callee).(*typed.MethodType)
	if !ok {
		return expr, true
	}
	functionType := l.convertMethodToFunction(method)
	obj := expr.Callee.(*ast.MemberExpression).Target
	expr.Args = append([]ast.Expression{obj}, expr.Args...)
	expr.Callee = l.convertToTypeExpression(expr.Callee, functionType)
	l.typeInfo.Set(expr.Callee, functionType)
	return expr, true
}

// Replace the function name with its type id.
func (l *lower) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) (*ast.FunctionDeclaration, bool) {
	ty := l.typeInfo.MustLookup(decl).(*typed.DeclaredType).Type
	decl.Name = ast.Ident(ty.Id().String())
	return decl, true
}

// Replace the struct name with its type id.
func (l *lower) VisitStructTypeDeclaration(decl *ast.StructTypeDeclaration) (*ast.StructTypeDeclaration, bool) {
	ty := l.typeInfo.MustLookup(decl).(*typed.DeclaredType).Type
	decl.Name = ast.TypeIdent(ty.Id().String())
	return decl, true
}

func (l *lower) VisitTraitDeclaration(trait *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	return nil, false
}

func Lower(module *ast.Module, typeInfo *typed.TypeInfo) (*ast.Module, error) {
	l := &lower{
		DefaultTransformer: DefaultTransformer{},
		typeInfo:           typeInfo,
		loweredMethods:     make(map[typed.TypeId]*typed.FunctionType),
		nextNodeId:         lowerNodeIdStart,
	}
	walker := DefaultTransformWalker{Transformer: l}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	return module, nil
}
