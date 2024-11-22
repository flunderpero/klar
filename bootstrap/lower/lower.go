/*
# Lowering

This lowering pass will:

  - rename all types (functions, structs) to their type id to make them globally
    unique and adapt all `IdentExpression` accordingly.

  - convert `MemberExpression` to `IdentExpression` if it points to a static method.

  - convert all calls to methods to regular function calls with the receiver
    as the first argument if the method is not static.

  - re-order the arguments in a `CallExpression` to be in the order of the function parameters.

  - remove all trait declarations.

  - record all function specializations (for monomorphization).

## Function Specialization (Monomorphization)

Functions are specialized based on their type arguments, generating distinct
implementations for each unique combination of types. However, since all
non-primitive types in Klar are references, we can optimize this process.
For generic functions without trait constraints, we can use a single implementation
that treats type parameters as opaque pointers, since:

 1. We can only pass around these references without accessing their contents

 2. All references have the same size (pointer size)

This eliminates the need for most specializations, as only functions using primitive
types or trait constraints need unique implementations.
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
	FunctionDef *ast.FunctionDefinition
	Specialized *typed.FunctionType
	Base        *typed.FunctionType
}

func (f FunctionSpecialization) String() string {
	name := fmt.Sprintf("%s from %s", f.Specialized.Id(), f.Base.Id())
	return fmt.Sprintf("FunctionSpecialization\n%s\n%s", base.IndentString(name, 1), base.Indent(f.Specialized, 1))
}

type OpaqueStructType struct {
	typed.BaseType
}

func (t *OpaqueStructType) String() string {
	return "OpaqueStructType"
}

var opaqueStructType = &OpaqueStructType{BaseType: typed.NewBaseType(lowerTypeIdStart)}

type lower struct {
	DefaultTransformer
	functionDefs            map[typed.TypeId]*ast.FunctionDefinition
	typeInfo                *typed.TypeInfo
	functionSpecializations []FunctionSpecialization
	nextNodeId              int
	nextTypeId              int
}

// Convert the given type to an `OpaqueStructType` if it is not a primitive type.
func try_convert_to_opaque(t typed.Type) typed.Type {
	if t == typed.StrType {
		return opaqueStructType
	}
	switch t.(type) {
	case *typed.StructType:
		return opaqueStructType
	}
	return t
}

func (l *lower) addFunctionSpecialization(funcType *typed.FunctionType) *typed.FunctionType {
	if typed.IsBuiltInFunction(funcType) {
		return funcType
	}
	funcTypeArgs := make([]typed.Type, len(funcType.TypeArgs))
	for i, arg := range funcType.TypeArgs {
		funcTypeArgs[i] = try_convert_to_opaque(arg)
	}
	for _, f := range l.functionSpecializations {
		if f.Specialized.Id() == funcType.Id() {
			return funcType
		}
		if f.Base.Id() == funcType.Id() {
			typeArgsMatch := true
			for i, typeArg := range f.Specialized.TypeArgs {
				funcTypeArg := funcTypeArgs[i]
				if typeArg.Id() != funcTypeArg.Id() {
					typeArgsMatch = false
					break
				}
			}
			if typeArgsMatch {
				return f.Specialized
			}
		}
	}
	base := funcType
	if funcType.IsGeneric() {
		funcType = funcType.CloneWith(l.newTypeId(), funcTypeArgs)
	}
	funcSpec := FunctionSpecialization{Specialized: funcType, Base: base}
	l.functionSpecializations = append(l.functionSpecializations, funcSpec)
	return funcType
}

func (l *lower) newTypeId() typed.TypeId {
	l.nextTypeId++
	return typed.TypeId(l.nextTypeId)
}

func (l *lower) convertToTypeIdIdentExpression(expr ast.Expression, ty typed.Type) *ast.IdentExpression {
	res := ast.NewIdentExpression(ast.Ident(ty.Id().String()), expr.Id(), expr.Span())
	l.typeInfo.Set(res, ty)
	return res
}

// Convert and `IdentExpression` to a `TypeExpression` if it points to a function type.
func (l *lower) VisitIdentExpression(expr *ast.IdentExpression) (ast.Expression, bool) {
	ty, isTypeReference := l.typeInfo.LookupTypeBinding(expr)
	if !isTypeReference {
		return expr, true
	}
	if functionType, isFunction := ty.(*typed.FunctionType); isFunction {
		// Since the function has been referenced, we need to generate code for it.
		ty = l.addFunctionSpecialization(functionType)
		expr = l.convertToTypeIdIdentExpression(expr, ty)
	}
	return expr, true
}

// Convert the `MemberExpression` to an `IdentExpression` if it resolves to a static method.
func (l *lower) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkMemberExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.MemberExpression)
	if !ok {
		return newExpr, false
	}
	functionType, ok := l.typeInfo.MustLookup(expr).(*typed.FunctionType)
	if !ok || !functionType.IsStaticMethod() {
		return expr, true
	}
	// Since the function has been referenced, we need to generate code for it.
	functionType = l.addFunctionSpecialization(functionType)
	res := l.convertToTypeIdIdentExpression(expr, functionType)
	return res, true
}

// Convert method calls to function calls with the receiver as first argument.
func (l *lower) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.CallExpression)
	if !ok {
		return newExpr, true
	}
	calleeType := l.typeInfo.MustLookup(expr.Callee).(typed.CallableType)
	params := calleeType.CallParams()
	// Re-order the call arguments to be in the order of the function parameters.
	callArgs := make([]ast.CallArg, len(expr.Args))
	positionalArgIndex := 0
	for _, callArg := range expr.Args {
		if callArg.Name == "" {
			callArgs[positionalArgIndex] = callArg
			positionalArgIndex += 1
			continue
		}
		for i, param := range params {
			if param.Name == callArg.Name {
				callArgs[i] = callArg
				break
			}
		}
	}
	expr.Args = callArgs
	functionType, ok := calleeType.(*typed.FunctionType)
	if !ok {
		return expr, true
	}
	if !functionType.IsMethod() || functionType.IsStaticMethod() {
		functionType = l.addFunctionSpecialization(functionType)
		l.typeInfo.Set(expr.Callee, functionType)
		return expr, true
	}
	receiver := expr.Callee.(*ast.MemberExpression).Target
	receiverCallArg := ast.CallArg{Name: "self", Value: receiver, Span: expr.Span()}
	expr.Args = append([]ast.CallArg{receiverCallArg}, expr.Args...)
	expr.Callee = l.convertToTypeIdIdentExpression(expr.Callee, functionType)
	functionType = l.addFunctionSpecialization(functionType)
	l.typeInfo.Set(expr.Callee, functionType)
	return expr, true
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
	decl.Name = ast.Ident(ty.Id().String())
	return decl, true
}

func (l *lower) VisitTraitDeclaration(trait *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	return nil, false
}

func (l *lower) finalizeFunctionSpecializations() []FunctionSpecialization {
	funcSpecs := []FunctionSpecialization{}
	for i, function := range l.functionSpecializations {
		def, ok := l.functionDefs[function.Base.Id()]
		if !ok {
			// This function specialization originates from a type binding and not a function definition.
			continue
		}
		function.FunctionDef = def
		l.functionSpecializations[i] = function
		funcSpecs = append(funcSpecs, function)
		symbol := *l.typeInfo.MustLookupSymbol(function.Base.Id())
		typeArgs := ""
		for i, typeArg := range function.Specialized.TypeArgs {
			if i > 0 {
				typeArgs += ","
			}
			typeArgs += typeArg.Id().String()
		}
		symbol.Name = fmt.Sprintf("%s<%s>", symbol.Name, typeArgs)
		l.typeInfo.DeclareSymbol(function.Specialized.Id(), &symbol)
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
		functionDefs:       make(map[typed.TypeId]*ast.FunctionDefinition),
		nextNodeId:         lowerNodeIdStart,
		nextTypeId:         lowerTypeIdStart + 1,
	}
	walker := DefaultTransformWalker{Transformer: l}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	if typeInfo.Main != nil {
		// We need to add a specialization for `main` or no code will be generated.
		l.addFunctionSpecialization(typeInfo.Main)
	}
	return &LoweredAST{Module: module, FunctionSpecializations: l.finalizeFunctionSpecializations()}
}
