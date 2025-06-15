from __future__ import annotations

from typing import cast

from . import ast, lower, typechecker, types


class Monomorphize:
    type_env: typechecker.TypeEnv
    fn_defs: dict[types.TypeId, ast.FnDef]
    fn_specs: list[lower.FnSpec]
    queue: list[lower.FnSpec]
    current: lower.FnSpec | None
    type_map: types.TypeMap
    seen: set[str]

    def __init__(
        self,
        type_env: typechecker.TypeEnv,
        fn_defs: dict[types.TypeId, ast.FnDef],
    ) -> None:
        self.type_env = type_env
        self.fn_defs = fn_defs
        self.fn_specs = []
        self.queue = []
        self.seen = set()
        self.type_map = types.TypeMap({}, None)
        self.current = None

    def run(self) -> None:
        while self.queue:
            self.current = self.queue.pop()
            self.type_map = self.current.type_env.type_map
            self.scan_fn_def(self.current.fn_def, None)
            self.fn_specs.append(self.current)

    def scan_fn_def(self, node: ast.Node, parent: ast.Node | None) -> ast.Node:
        ast.walk(node, self.scan_fn_def)
        match node:
            case ast.Ident():
                typ = self.type_env.get_node_type(node)[0]
                if not isinstance(typ, types.Fn) or not typ.is_named:
                    return node
                if self.type_env.builtins.is_builtin(typ):
                    return node
                receiver: types.Struct | None = None
                call_args: list[types.Type] | None = None
                if isinstance(parent, ast.Call) and node == parent.callee:
                    call_args = [self.type_env.get_node_type(x)[0] for x in parent.args]
                    if typ.is_instance_method():
                        receiver = self.type_env.get_node_type(parent.args[0])[0]
                        if isinstance(receiver, types.TypeParam) and isinstance(receiver.bound, types.Trait):
                            # This is a trait method. Replace it with the concrete type we find
                            # by resolving the receiver.
                            fn_name = typ.fqn.path[-1]
                            receiver = cast(types.Struct, self.type_map.resolve(receiver))
                            typ = receiver.method(fn_name)
                            assert isinstance(typ, types.Fn)
                            assert self.current is not None
                            self.current.type_env.set_node_type(node, typ)
                        assert isinstance(receiver, types.Struct), f"Expected struct, got {receiver}"
                self.enqueue_if_needed(typ, receiver, call_args)
        return node

    def enqueue_if_needed(
        self, typ: types.Fn, receiver: types.Struct | None, call_args: list[types.Type] | None
    ) -> None:
        type_map = types.TypeMap({}, self.type_map)
        for p, a in zip(typ.type_params, typ.type_args):
            a = type_map.resolve(a)
            type_map.bind(p, a)
        if receiver:
            for p, a in zip(receiver.type_params, receiver.type_args):
                a = type_map.resolve(a)
                type_map.bind(p, a)
        fn_def = self.fn_defs.get(typ.id)
        assert fn_def, f"No FnDef for {typ.debug()}"
        typ = cast(types.Fn, type_map.resolve(typ))
        for type_arg in typ.type_args:
            assert not isinstance(type_arg, types.TypeParam), f"Type arg {type_arg} not resolved"
        key = types.full_id(typ)
        if key in self.seen:
            return
        self.seen.add(key)
        self.queue.append(lower.FnSpec(typ, lower.TypeEnv(self.type_env, type_map, {}), fn_def, call_args))


def monomorphize(module: ast.Module, type_env: typechecker.TypeEnv) -> list[lower.FnSpec]:
    fn_defs: dict[types.TypeId, ast.FnDef] = {}
    main = None

    runner = Monomorphize(type_env, fn_defs)

    def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        match node:
            case ast.FnDef():
                typ = type_env.get_node_type(node.decl)[0]
                assert isinstance(typ, types.Fn)
                fn_defs[typ.id] = node
                if node.decl.name == "main":
                    nonlocal main
                    main = typ
        ast.walk(node, visit)
        return node

    visit(module, None)
    assert isinstance(main, types.Fn), f"main function not found: {main}"
    runner.enqueue_if_needed(main, None, None)
    runner.run()
    return runner.fn_specs
