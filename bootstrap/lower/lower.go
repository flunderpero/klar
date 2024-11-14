/*
This is a lowering pass that mangles all identifiers and transforms call to methods to
regular function call.
*/
package lower

import (
	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type lower struct {
	DefaultTransformer
	typeInfo             *typed.TypeInfo
	loweredMethods       map[*typed.MethodType]*typed.FunctionType
	referenceExpressions []ast.ReferenceExpression
}

func (l *lower) VisitReferenceExpression(expr ast.ReferenceExpression) (ast.ReferenceExpression, bool) {
	l.referenceExpressions = append(l.referenceExpressions, expr)
	return expr, true
}

// Convert `receiver.method(...)` call to `method(receiver, ...)` call.
func (l *lower) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (*ast.CallExpression, bool) {
	expr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	calleeType := l.typeInfo.MustLookup(expr.Callee)
	if method, isMethod := calleeType.(*typed.MethodType); isMethod {
		if !method.IsStatic {
			obj := expr.Callee.(*ast.MemberExpression).Target
			expr.Args = append([]ast.Expression{obj}, expr.Args...)
		}
		expr.Callee = ast.NewIdentExpression(ast.Ident(expr.Callee.Id().String()), expr.Callee.Id(), expr.Callee.Span())
		functionType, found := l.loweredMethods[method]
		if !found {
			functionType = &typed.FunctionType{
				BaseType:   typed.NewBaseType(method.Id()),
				ArgTypes:   method.ArgTypes,
				ReturnType: method.ReturnType,
			}
			l.loweredMethods[method] = functionType
		}
		l.typeInfo.Set(expr.Callee, functionType)
		return expr, true
	}
	return expr, true
}

func (l *lower) VisitTraitDeclaration(trait *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	return nil, false
}

func Lower(module *ast.Module, typeInfo *typed.TypeInfo) (*ast.Module, error) {
	l := &lower{
		DefaultTransformer: DefaultTransformer{},
		typeInfo:           typeInfo,
		loweredMethods:     make(map[*typed.MethodType]*typed.FunctionType),
	}
	walker := DefaultTransformWalker{Transformer: l}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("Module has been deleted")
	}
	return module, nil
}
