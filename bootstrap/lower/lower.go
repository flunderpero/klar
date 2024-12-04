/*
# Lowering

Lowering will:

  - convert `MemberExpression` to `IdentExpression` if it points to a static method. (see `prepare`)

  - convert all calls to methods to regular function calls with the receiver
    as the first argument if the method is not static. (see `prepare`)

  - re-order the arguments in a `CallExpression` to be in the order of the function
    parameters. (see `prepare`)

  - convert all tuple literals to anonymous struct initializations so that they can be treated as
    regular structs from here on.

  - remove all trait and impl declarations after collection all function
    definitions. (see `prepare`)

  - monomorphize generic functions and structs.
*/
package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type LoweredAST struct {
	Module    *ast.Module
	FuncSpecs []*FunctionSpecialization
	TypeInfo  *typed.TypeInfo
}

func (l *LoweredAST) String() string {
	return fmt.Sprintf(
		"LoweredAST\n%s%s", base.Indent(l.Module, 1), base.IndentSlice(l.FuncSpecs, 1))
}

func Lower(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	genericsResolver *typed.GenericsResolver,
	nodeCreator *ast.NodeCreator,
	typeCreator *typed.TypeCreator,
) *LoweredAST {
	module, funcInfos := Prepare(module, typeInfo, genericsResolver, nodeCreator, typeCreator)
	funcSpecs := Monomorphize(module, typeInfo, funcInfos, genericsResolver)
	return &LoweredAST{Module: module, FuncSpecs: funcSpecs, TypeInfo: typeInfo}
}
