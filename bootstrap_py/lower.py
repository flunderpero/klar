from __future__ import annotations

from dataclasses import dataclass

from . import ast, typechecker, types


@dataclass
class TypeEnv:
    type_env: typechecker.TypeEnv
    type_res_scope: types.TypeResScope

    def get_node_type(self, node: ast.Node) -> types.Type:
        typ = self.type_env.get_node_type(node)
        return self.resolve(typ)

    def resolve(self, typ: types.Type) -> types.Type:
        return self.type_res_scope.resolve(typ, resolve_member_target_self_typ=True)


@dataclass
class FnSpec:
    typ: types.Fn
    type_env: TypeEnv
    fn_def: ast.FnDef
    call_args: list[types.Type] | None

    def debug(self) -> str:
        return f"    {self.fn_def.decl}\n => {self.type_env.resolve(self.typ).debug().replace('\n', '\n    ')}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self) -> str:
        node = ast.to_str_withoud_nid(self.fn_def.decl)
        typ = self.type_env.resolve(self.typ).signature()
        return f"    {node.replace('\n', '\n    ')}\n => {typ.replace('\n', '\n    ')}"
