from __future__ import annotations

from . import ast, lower, typechecker, types


class Monomorphize:
    type_env: typechecker.TypeEnv
    fn_defs: dict[types.TypeId, ast.FnDef]
    fn_specs: list[lower.FnSpec]
    queue: list[lower.FnSpec]
    current: lower.FnSpec
    type_res_scope: types.TypeResScope
    seen: set[str]

    def __init__(self, type_env: typechecker.TypeEnv, fn_defs: dict[types.TypeId, ast.FnDef]) -> None:
        self.type_env = type_env
        self.fn_defs = fn_defs
        self.fn_specs = []
        self.queue = []
        self.seen = set()
        self.type_res_scope = types.TypeResScope(None, None)

    def run(self) -> None:
        while self.queue:
            self.current = self.queue.pop()
            self.type_res_scope = self.current.type_env.type_res_scope
            self.scan_fn_def(self.current.fn_def, None)
            self.fn_specs.append(self.current)

    def scan_fn_def(self, node: ast.Node, parent: ast.Node | None) -> None:
        ast.walk(node, self.scan_fn_def)
        match node:
            case ast.Ident():
                typ = self.type_env.get_node_type(node)
                if not isinstance(typ, types.ParameterizedType):
                    return
                if self.type_env.builtins.is_builtin(typ):
                    return
                call_args: list[types.Type] | None = None
                if isinstance(parent, ast.Call) and node == parent.callee:
                    call_args = [self.type_env.get_node_type(x) for x in parent.args]
                self.enqueue_if_needed(typ, call_args)

    def enqueue_if_needed(self, typ: types.ParameterizedType, call_args: list[types.Type] | None) -> None:
        type_res_scope = types.TypeResScope(typ.type_res_scope, self.type_res_scope)
        fn = type_res_scope.resolve(typ, resolve_member_target_self_typ=True)
        if not isinstance(fn, types.Fn) or not fn.is_named:
            return
        assert all(not isinstance(x, (types.TypeParam, types.Trait)) for x in fn.type_args), (
            f"at least one type param unresolved or resolved to a trait: {fn.debug()} at {fn.span}"
        )
        fn_def = self.fn_defs[fn.id]
        key = types.full_id(fn)
        if key in self.seen:
            return
        self.seen.add(key)
        for p, a in zip(fn.type_params, fn.type_args):
            type_res_scope.declare(p, type_res_scope.resolve(a))
        if call_args:
            assert all(not isinstance(type_res_scope.resolve(x), (types.TypeParam, types.Trait)) for x in call_args), (
                f"at least one call arg is unresolved or resolved to a trait: {call_args} {fn.debug()} at {fn.span}"
            )
        self.queue.append(lower.FnSpec(fn, lower.TypeEnv(self.type_env, type_res_scope), fn_def, call_args))


def monomorphize(module: ast.Module, type_env: typechecker.TypeEnv) -> list[lower.FnSpec]:
    fn_defs: dict[types.TypeId, ast.FnDef] = {}
    main = None

    runner = Monomorphize(type_env, fn_defs)

    def visit(node: ast.Node, _parent: ast.Node | None) -> None:
        match node:
            case ast.FnDef():
                typ = type_env.get_node_type(node.decl)
                assert isinstance(typ, types.Fn)
                fn_defs[typ.id] = node
                if node.decl.name == "main":
                    nonlocal main
                    main = typ
        ast.walk(node, visit)

    visit(module, None)
    assert isinstance(main, types.Fn), f"main function not found: {main}"
    runner.enqueue_if_needed(types.instance(main, None), None)
    runner.run()
    return runner.fn_specs
