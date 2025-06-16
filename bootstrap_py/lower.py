from __future__ import annotations

from dataclasses import dataclass

from . import ast, typechecker, types


@dataclass
class TypeEnv:
    type_env: typechecker.TypeEnv
    type_map: types.TypeMap
    overrides: dict[ast.NodeId, types.Type]

    def get_node_type(self, node: ast.Node) -> types.Type:
        typ = self.overrides.get(node.id, self.type_env.get_node_type(node)[0])
        assert typ is not None, f"Type for {node} not found"
        return self.resolve(typ, node)

    def set_node_type(self, node: ast.Node, typ: types.Type) -> None:
        self.overrides[node.id] = typ

    def resolve(self, typ: types.Type, node: ast.Node | None = None) -> types.Type:
        typ = self.type_map.resolve(typ)
        if isinstance(typ, types.TypeParam):
            assert typ.bound is not None, f"Type parameter {typ.debug()} has no bound at {(node or typ).span}"
            typ = self.resolve(typ.bound)
        return typ

    def debug(self) -> str:
        return f"TypeEnv(type_env={self.type_env}, type_map={self.type_map.debug()}, overrides={self.overrides})"


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
