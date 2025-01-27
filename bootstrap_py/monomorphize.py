from __future__ import annotations

from . import ast, lower, typechecker, types


class Monomorphize:
    type_env: typechecker.TypeEnv
    fn_defs: dict[types.TypeId, ast.FnDef]
    fn_specs: list[lower.FnSpec]
    queue: list[lower.FnSpec]
    current: lower.FnSpec | None
    seen: set[str]

    def __init__(self, type_env: typechecker.TypeEnv, fn_defs: dict[types.TypeId, ast.FnDef]) -> None:
        self.type_env = type_env
        self.fn_defs = fn_defs
        self.fn_specs = []
        self.queue = []
        self.seen = set()
        self.type_res_scope = None
        self.current = None

    def run(self) -> None:
        while self.queue:
            self.current = self.queue.pop()
            self.scan_fn_def(self.current.fn_def)
            self.fn_specs.append(self.current)

    def scan_fn_def(self, node: ast.Node) -> None:
        ast.walk(node, self.scan_fn_def)
        match node:
            case ast.Ident():
                typ = self.type_env.get_node_type(node)
                if not isinstance(typ, types.Instance):
                    return
                assert isinstance(typ.typ, types.Fn)
                fn = typ.typ
                if self.type_env.builtins.is_builtin(fn):
                    return
                assert isinstance(fn, types.Fn)
                self.enqueue_if_needed(typ)

    def enqueue_if_needed(self, instance: types.Instance) -> None:
        """Add to the queue if is not already in `self.queue` or `self.fn_specs`."""
        fn_def = self.fn_defs[instance.id]
        type_res_scope = types.TypeResScope(
            instance.type_res_scope, self.current.type_env.type_res_scope if self.current else None
        )
        instance = types.Instance(instance.typ, type_res_scope)
        fn = type_res_scope.resolve(instance)
        assert isinstance(fn, types.Fn)
        assert all(not isinstance(x, types.TypeParam) for x in instance.type_args()), (
            f"at least one type param unresolved: {instance}"
        )
        key = types.full_id(instance)
        if key in self.seen:
            return
        self.seen.add(key)
        self.queue.append(lower.FnSpec(fn, lower.TypeEnv(self.type_env, type_res_scope), fn_def))


def monomorphize(module: ast.Module, type_env: typechecker.TypeEnv) -> list[lower.FnSpec]:
    fn_defs: dict[types.TypeId, ast.FnDef] = {}
    main = None

    runner = Monomorphize(type_env, fn_defs)

    def visit(node: ast.Node) -> None:
        match node:
            case ast.FnDef():
                typ = type_env.get_node_type(node.decl)
                assert isinstance(typ, types.Fn)
                fn_defs[typ.id] = node
                if node.decl.name == "main":
                    nonlocal main
                    main = typ
        ast.walk(node, visit)

    visit(module)
    assert isinstance(main, types.Fn)
    runner.enqueue_if_needed(types.Instance(main, types.TypeResScope(None, None)))
    runner.run()
    return runner.fn_specs
