package typed

import (
	"github.com/flunderpero/klar/bootstrap/ast"
)

type forwardImplDef struct {
	implDef      *ast.ImplDefinition
	forwardDecls *forwardDecls
}

type forwardDecls struct {
	funcDecls       []*ast.FunctionDeclaration
	funcDefs        []*ast.FunctionDefinition
	namedUnionDecls []*ast.NamedUnionTypeDeclaration
	traitDecls      []*ast.TraitDeclaration
	structDecls     []*ast.StructTypeDeclaration
	implDefs        []*forwardImplDef
}

func forwardDeclare(
	nodes []ast.Node, typeCreator *TypeCreator, typeInfo *TypeInfo, symbolScope *SymbolScope, typeScope_ *typeScope) (*forwardDecls, error) {
	res := &forwardDecls{}
	declare := func(name ast.Ident, ty Type, node ast.Node) error {
		if err := typeScope_.declareType(string(name), ty, node.Span()); err != nil {
			return err
		}
		declareSymbol(ty.Id(), string(name), symbolScope, typeInfo)
		return nil
	}
	for _, node := range nodes {
		switch node := node.(type) {
		case *ast.StructTypeDeclaration:
			ty := &StructType{implementableTypeBase: typeCreator.newImplementableTypeBase(nil, nil)}
			if err := declare(node.Name, ty, node); err != nil {
				return nil, err
			}
			typeInfo.Set(node, &DeclaredType{Type: ty})
			res.structDecls = append(res.structDecls, node)
		case *ast.ImplDefinition:
			ty := &ImplType{typeBase: typeCreator.newTypeBase()}
			typeInfo.Set(node, ty)
			res.implDefs = append(res.implDefs, &forwardImplDef{implDef: node})
		case *ast.TraitDeclaration:
			ty := &TraitType{typeBase: typeCreator.newTypeBase()}
			if err := declare(node.Name, ty, node); err != nil {
				return nil, err
			}
			typeInfo.Set(node, &DeclaredType{Type: ty})
			res.traitDecls = append(res.traitDecls, node)
		case *ast.NamedUnionTypeDeclaration:
			ty := &UnionType{typeBase: typeCreator.newTypeBase(), IsAnonymous: false}
			if err := declare(node.Name, ty, node); err != nil {
				return nil, err
			}
			typeInfo.Set(node, &DeclaredType{Type: ty})
			res.namedUnionDecls = append(res.namedUnionDecls, node)
		case *ast.FunctionDeclaration:
			ty := &FunctionType{typeBase: typeCreator.newTypeBase()}
			if err := declare(node.Name, ty, node); err != nil {
				return nil, err
			}
			typeInfo.Set(node, &DeclaredType{Type: ty})
			res.funcDecls = append(res.funcDecls, node)
		case *ast.FunctionDefinition:
			ty := &FunctionType{typeBase: typeCreator.newTypeBase()}
			if err := declare(node.Decl.Name, ty, node.Decl); err != nil {
				return nil, err
			}
			declaredType := &DeclaredType{Type: ty}
			typeInfo.Set(node.Decl, declaredType)
			typeInfo.Set(node, declaredType)
			res.funcDefs = append(res.funcDefs, node)
		}
	}
	return res, nil
}
