/*
# Remove Unused

This pass will remove all `ast.TraitDeclaration` and `ast.ImplDefinition` because we don't need them
from this point onwards.
*/
package lower

import "github.com/flunderpero/klar/bootstrap/ast"

type removeUnused struct {
	DefaultTransformer
}

func (self *removeUnused) VisitTraitDeclaration(decl *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	return nil, false
}

func (self *removeUnused) VisitImplDefinition(def *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool) {
	return nil, false
}

func RemoveUnused(module *ast.Module) *ast.Module {
	transformer := &removeUnused{}
	walker := DefaultTransformWalker{Transformer: transformer}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	return module
}
