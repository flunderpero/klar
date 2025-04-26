from dataclasses import dataclass

from . import ast, typechecker, types


@dataclass
class TypeEnv:
    type_env: typechecker.TypeEnv
    type_res_scope: types.TypeResScope

    def get_node_type(self, node: ast.Node) -> types.Type:
        typ = self.type_env.get_node_type(node)
        return self.resolve(typ)

    def get_unresolved_node_type(self, node: ast.Node) -> types.Type:
        return self.type_env.get_node_type(node)

    def resolve(self, typ: types.Type) -> types.Type:
        return self.type_res_scope.resolve(typ)


@dataclass
class FnSpec:
    typ: types.Fn
    type_env: TypeEnv
    fn_def: ast.FnDef

    def __str__(self) -> str:
        return f"{self.fn_def.decl} :: {self.type_env.resolve(self.typ)}"
