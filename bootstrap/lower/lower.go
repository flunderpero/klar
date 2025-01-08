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
	unionStructType := typeCreator.NewStructType(
		nil,
		[]typed.TypeParam{},
		[]typed.Type{},
		[]typed.TypeAndName[typed.Type]{
			{Type: &typed.Int64Type{}, Name: "tag"},
			{Type: &typed.RawPtr{}, Name: "data"},
		},
		[]*typed.Method{},
		[]*typed.TraitType{})
	// Note: The order of passes is important.
	module = IndexLowering(module, typeInfo, typeCreator, nodeCreator)
	module = ReceiverLowering(module, typeInfo, genericsResolver, nodeCreator)
	module = MatchLowering(module, typeInfo, typeCreator, nodeCreator, unionStructType)
	module = UnionLowering(module, typeInfo, typeCreator, nodeCreator, unionStructType)
	module = TupleLowering(module, typeInfo, typeCreator, nodeCreator)
	funcSpecs := Monomorphization(module, typeInfo, genericsResolver)
	module = RemoveUnused(module)
	return &LoweredAST{Module: module, FuncSpecs: funcSpecs, TypeInfo: typeInfo}
}
