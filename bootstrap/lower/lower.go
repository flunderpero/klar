/*
This is a lowering pass:

  - converts all calls to methods to regular function calls with the receiver
    as the first argument if the method is not static.

  - replaces the `ast.CallExpression.Callee` identifier with the type id of the
    called function.

- removes all trait declarations.
*/
package lower

import (
	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type lower struct {
	DefaultTransformer
	typeInfo       *typed.TypeInfo
	loweredMethods map[*typed.MethodType]*typed.FunctionType
}

func CallableIdent(ty typed.CallableType) ast.Ident {
	return ast.Ident(ty.Id().String())
}

// Convert `receiver.method(...)` call to `method(receiver, ...)` call.
// Replace `expr.Callee` with the callee type id.
func (l *lower) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (*ast.CallExpression, bool) {
	expr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	calleeType := l.typeInfo.MustLookup(expr.Callee).(typed.CallableType)
	if method, isMethod := calleeType.(*typed.MethodType); isMethod {
		if !method.IsStatic {
			obj := expr.Callee.(*ast.MemberExpression).Target
			expr.Args = append([]ast.Expression{obj}, expr.Args...)
		}
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
	}
	// We don't want to call functions by their name anymore but by their type-id.
	expr.Callee = ast.NewIdentExpression(CallableIdent(calleeType), expr.Callee.Id(), expr.Callee.Span())
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
