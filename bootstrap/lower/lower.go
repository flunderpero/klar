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

  - record all function specializations (for monomorphization)
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/typed"
)

// 640kb will be enough forever. :-)
const lowerNodeIdStart = 1_000_000_000
const lowerTypeIdStart = 1_000_000_000

type FunctionSpecialization struct {
	FunctionDef     *ast.FunctionDefinition
	FunctionDefType *typed.FunctionType
	SpecializedType *typed.FunctionType
	TypeArgs        []typed.Type
}

func (f FunctionSpecialization) String() string {
	name := fmt.Sprintf("%s from %s", f.SpecializedType.Id(), f.FunctionDefType.Id())
	return fmt.Sprintf(
		"FunctionSpecialization\n%s\n%s\n    (TypeArgs)%s",
		base.IndentString(name, 1),
		base.Indent(f.SpecializedType, 1),
		base.IndentSlice(f.TypeArgs, 2))
}

type lower struct {
	DefaultTransformer
	functionDefs            map[typed.TypeId]*ast.FunctionDefinition
	typeInfo                *typed.TypeInfo
	loweredMethods          map[typed.TypeId]*typed.FunctionType
	functionSpecializations []FunctionSpecialization
	nextNodeId              int
	nextTypeId              int
}

// `call` is optional. If not given `declType` must not be generic
func (l *lower) addFunctionSpecialization(declType *typed.FunctionType, call *typed.Call) *typed.FunctionType {
	if typed.IsBuiltInFunction(declType) {
		return declType
	}
	hasTypeParams := len(declType.TypeParams()) > 0
	if hasTypeParams && (call == nil || len(call.TypeArgs) == 0) {
		// We don't record the generic variant (i.e. the one without type args).
		return declType
	}
	for _, f := range l.functionSpecializations {
		if f.FunctionDefType.Id() == declType.Id() {
			typeArgsMatch := true
			for i, arg := range f.TypeArgs {
				if arg.Id() != call.TypeArgs[i].Id() {
					typeArgsMatch = false
					break
				}
			}
			if typeArgsMatch {
				return f.SpecializedType
			}
		}
	}
	var function FunctionSpecialization
	if call == nil || len(call.TypeArgs) == 0 {
		function = FunctionSpecialization{FunctionDefType: declType, SpecializedType: declType}
	} else {
		concreteType := typed.NewFunctionType(l.newTypeId(), declType.TypeParams(), call.ArgTypes, call.ReturnType)
		function = FunctionSpecialization{FunctionDefType: declType, TypeArgs: call.TypeArgs, SpecializedType: concreteType}
	}
	l.functionSpecializations = append(l.functionSpecializations, function)
	return function.SpecializedType
}

func (l *lower) newNodeId() ast.NodeId {
	l.nextNodeId++
	return ast.NodeId(l.nextNodeId)
}

func (l *lower) newTypeId() typed.TypeId {
	l.nextTypeId++
	return typed.TypeId(l.nextTypeId)
}

func (l *lower) convertMethodToFunction(method *typed.MethodType) *typed.FunctionType {
	functionType, found := l.loweredMethods[method.Id()]
	if found {
		return functionType
	}
	functionType = typed.NewFunctionType(method.Id(), method.TypeParams(), method.ArgTypes, method.ReturnType)
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
		if functionType, isFunction := ty.(*typed.FunctionType); isFunction {
			// Functions could not be determined as TypeExpressions during parsing because the
			// parser can only identify TypeIdent as a TypeExpression. Now that typechecking
			// has confirmed the type, we can convert the expression from an `IdentExpression`
			// to the proper `TypeExpression`.
			expr = l.convertToTypeExpression(expr, ty)
			// Since the function has been referenced, we need to generate code for it.
			l.addFunctionSpecialization(functionType, nil)
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
	// Since the function has been referenced, we need to generate code for it.
	l.addFunctionSpecialization(functionType, nil)
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
	calleeType := l.typeInfo.MustLookup(expr.Callee)
	switch calleeType := calleeType.(type) {
	case *typed.FunctionType:
		functionType := l.addFunctionSpecialization(calleeType, l.typeInfo.MustLookupCall(expr))
		l.typeInfo.Set(expr.Callee, functionType)
		return expr, true
	case *typed.MethodType:
		functionType := l.convertMethodToFunction(calleeType)
		receiver := expr.Callee.(*ast.MemberExpression).Target
		receiverType := l.typeInfo.MustLookup(receiver)
		expr.Args = append([]ast.Expression{receiver}, expr.Args...)
		expr.Callee = l.convertToTypeExpression(expr.Callee, functionType)
		call := l.typeInfo.MustLookupCall(expr)
		call.ArgTypes = append([]typed.Type{receiverType}, call.ArgTypes...)
		functionType = l.addFunctionSpecialization(functionType, call)
		l.typeInfo.Set(expr.Callee, functionType)
		return expr, true
	default:
		panic(fmt.Sprintf("expected function or method type, got %T", calleeType))
	}
}

// Replace the function name with its type id.
func (l *lower) VisitFunctionDeclaration(decl *ast.FunctionDeclaration) (*ast.FunctionDeclaration, bool) {
	ty := l.typeInfo.MustLookup(decl).(*typed.DeclaredType).Type
	decl.Name = ast.Ident(ty.Id().String())
	return decl, true
}

// Just record that we've seen this function definition.
func (l *lower) VisitFunctionDefinition(def *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool) {
	def, ok := w.WalkFunctionDefinition(def)
	if !ok {
		return nil, false
	}
	ty := l.typeInfo.MustLookup(def.Decl).(*typed.DeclaredType).Type
	l.functionDefs[ty.Id()] = def
	return def, true
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

func (l *lower) finalizeFunctionSpecializations() []FunctionSpecialization {
	funcSpecs := []FunctionSpecialization{}
	for i, function := range l.functionSpecializations {
		def, ok := l.functionDefs[function.FunctionDefType.Id()]
		if !ok {
			// This function specialization originates from a type binding and not a function definition.
			continue
		}
		function.FunctionDef = def
		l.functionSpecializations[i] = function
		funcSpecs = append(funcSpecs, function)
		symbol := *l.typeInfo.MustLookupSymbol(function.FunctionDefType.Id())
		typeArgs := ""
		for _, arg := range function.TypeArgs {
			typeArgs += arg.Id().String() + ","
		}
		symbol.Name = symbol.Name + typeArgs
		l.typeInfo.DeclareSymbol(function.SpecializedType.Id(), &symbol)
	}
	return funcSpecs
}

type LoweredAST struct {
	Module                  *ast.Module
	FunctionSpecializations []FunctionSpecialization
}

func (l *LoweredAST) String() string {
	return fmt.Sprintf(
		"LoweredAST\n%s%s", base.Indent(l.Module, 1), base.IndentSlice(l.FunctionSpecializations, 1))
}

func Lower(module *ast.Module, typeInfo *typed.TypeInfo) *LoweredAST {
	l := &lower{
		DefaultTransformer: DefaultTransformer{},
		typeInfo:           typeInfo,
		loweredMethods:     make(map[typed.TypeId]*typed.FunctionType),
		functionDefs:       make(map[typed.TypeId]*ast.FunctionDefinition),
		nextNodeId:         lowerNodeIdStart,
		nextTypeId:         lowerTypeIdStart,
	}
	walker := DefaultTransformWalker{Transformer: l}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	if typeInfo.Main != nil {
		// We need to add a specialization for `main` or no code will be generated.
		l.addFunctionSpecialization(typeInfo.Main, nil)
	}
	return &LoweredAST{Module: module, FunctionSpecializations: l.finalizeFunctionSpecializations()}
}
