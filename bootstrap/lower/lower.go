/*
This is a lowering pass that mangles all identifiers and transforms call to methods to
regular function call.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type lower struct {
	DefaultTransformer
	typeInfo         *typed.TypeInfo
	loweredMethods   map[*typed.MethodType]*typed.FunctionType
	mangledNames     map[ast.NodeId]string
	mangledNameScope []string
	identExpressions []*ast.IdentExpression
}

func (l *lower) enterScope(node ast.Node) {
	l.mangledNameScope = append(l.mangledNameScope, l.mangleName(node))
}

func (l *lower) exitScope() {
	l.mangledNameScope = l.mangledNameScope[:len(l.mangledNameScope)-1]
}

func (l *lower) mangleName(node ast.Node) string {
	if name, found := l.mangledNames[node.Id()]; found {
		return name
	}
	scope := l.mangledNameScope[len(l.mangledNameScope)-1]
	var name string
	switch node := node.(type) {
	case *ast.Module:
		name = string(node.Name)
	case *ast.FunctionDeclaration:
		ty := l.typeInfo.MustLookupDeclaredType(node).Type.(typed.NamedType)
		if ty != l.typeInfo.Main {
			name = scope + "_" + string(ty.TypeName())
			node.Name = ast.Ident(name)
			if ft, ok := ty.(*typed.FunctionType); ok {
				ft.Name = ast.Ident(name)
			} else {
				mt := ty.(*typed.MethodType)
				mt.Name = ast.Ident(name)
			}
		}
	case *ast.StructTypeDeclaration:
		ty := l.typeInfo.MustLookupDeclaredType(node).Type.(*typed.StructType)
		name = scope + "_" + string(ty.Name)
		ty.Name = ast.TypeIdent(name)
		node.Name = ty.Name
	default:
		panic(fmt.Sprintf("name mangling not implemented for node: %T", node))
	}
	return name
}

func (l *lower) VisitIdentExpression(expr *ast.IdentExpression) (*ast.IdentExpression, bool) {
	l.identExpressions = append(l.identExpressions, expr)
	return expr, true
}

func (l *lower) VisitStructTypeDeclaration(s *ast.StructTypeDeclaration) (*ast.StructTypeDeclaration, bool) {
	l.mangleName(s)
	return s, true
}

func (l *lower) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) (*ast.FunctionDeclaration, bool) {
	l.mangleName(decl)
	return decl, true
}

// Convert `receiver.method(...)` call to `method(receiver, ...)` call.
func (l *lower) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (*ast.CallExpression, bool) {
	expr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	calleeType := l.typeInfo.MustLookup(expr.Callee)
	if method, isMethod := calleeType.(*typed.MethodType); isMethod {
		if method.IsStatic() {
			panic("Don't know how to handle static methods yet")
		}
		obj := expr.Callee.(*ast.MemberExpression).Target
		expr.Args = append([]ast.Expression{obj}, expr.Args...)
		expr.Callee = ast.NewIdentExpression(method.Name, expr.Callee.Id())
		functionType, found := l.loweredMethods[method]
		if !found {
			functionType = &typed.FunctionType{
				Args:       method.Args,
				Name:       method.Name,
				ReturnType: method.ReturnType,
			}
			l.loweredMethods[method] = functionType
		}
		l.typeInfo.Set(expr.Callee, functionType)
		return expr, true
	}
	return expr, true
}

// Convert all references to the `Self` type with the actual receiver type
// and mangle function names.
func (l *lower) VisitImplDefinition(impl *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool) {
	for _, method := range impl.Methods {
		l.mangleName(method.Decl)
	}
	return impl, true
}

func (l *lower) VisitModule(module *ast.Module, w TransformWalker) (*ast.Module, bool) {
	l.enterScope(module)
	defer l.exitScope()
	return w.WalkModule(module)
}

func (l *lower) mangleIdentExpressions() {
	for _, expr := range l.identExpressions {
		ty, found := l.typeInfo.LookupTypeBinding(expr)
		if !found {
			continue
		}
		expr.Ident = ast.Ident(ty.TypeName())
	}
}

func Lower(module *ast.Module, typeInfo *typed.TypeInfo) (*ast.Module, error) {
	l := &lower{
		DefaultTransformer: DefaultTransformer{},
		typeInfo:           typeInfo,
		loweredMethods:     make(map[*typed.MethodType]*typed.FunctionType),
		mangledNames:       make(map[ast.NodeId]string),
		mangledNameScope:   []string{""},
	}
	walker := DefaultTransformWalker{Transformer: l}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("Module has been deleted")
	}
	l.mangleIdentExpressions()
	return module, nil
}
