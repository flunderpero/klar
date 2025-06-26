"""Monomorphization phase - generates concrete function instantiations for generic code.

This phase:
1. Identifies all generic functions that need concrete instantiations based on their usage
2. Resolves concrete types for all type parameters in each instantiation
3. Handles instance method lowering (receiver.method → method with receiver as first argument)
4. Resolves trait method calls to their concrete implementations

Example transformations:

Generic function instantiation:
    fn id<T>(x T) T => x
    id<Str>("hello")  // Creates id$Str instantiation
    id<Int>(42)       // Creates id$Int instantiation

Instance method lowering:
    value.print()  →  print(value)

Trait method resolution:
    trait Container<T> {
        fn get(self) T
    }

    struct Value<U> {
        value U
    }
    fn (Container<U>) Value.get(self) U => self.value

    fn get_it<C Container<Str>>(c C) { c.get() }

    When calling `get_it()` with Value<Str>, resolves:
    - C → Value<Str>
    - c.get() → Value<Str>.get()
    - Return type Container<T> → concrete Value<Str>

The module tracks concrete types through variable assignments to ensure trait
return types resolve to their implementing types.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import cast

from . import ast, lower, typechecker, types


class Monomorphize:
    type_env: typechecker.TypeEnv
    fn_defs: dict[types.TypeId, ast.FnDef]
    fn_specs: list[lower.FnSpec]
    queue: list[lower.FnSpec]
    current: lower.FnSpec
    type_map: types.TypeMap
    seen: set[str]
    lowered_method_member_nodes: dict[ast.NodeId, ast.Member]

    def __init__(
        self,
        type_env: typechecker.TypeEnv,
        fn_defs: dict[types.TypeId, ast.FnDef],
        lowered_method_member_nodes: dict[ast.NodeId, ast.Member],
    ) -> None:
        self.type_env = type_env
        self.fn_defs = fn_defs
        self.fn_specs = []
        self.queue = []
        self.seen = set()
        self.type_map = types.TypeMap({}, None)
        self.lowered_method_member_nodes = lowered_method_member_nodes

    def run(self) -> None:
        while self.queue:
            self.current = self.queue.pop()
            self.type_map = self.current.type_env.type_map
            evaluate_types_based_on_parameters(
                self.current.fn_def,
                self.current.type_env,
                self.lowered_method_member_nodes,
                self.current.call_args or [],
            )
            self.scan_fn_def(self.current.fn_def, None)
            self.fn_specs.append(self.current)

    def scan_fn_def(self, node: ast.Node, parent: ast.Node | None) -> ast.Node:
        ast.walk(node, self.scan_fn_def)
        match node:
            case ast.Ident():
                typ = self.current.type_env.get_node_type(node)
                if not isinstance(typ, types.Fn) or not typ.is_named:
                    return node
                if self.type_env.builtins.is_builtin(typ):
                    return node
                receiver: types.Type | None = None
                call_args: list[types.Type] | None = None
                if isinstance(parent, ast.Call) and node == parent.callee:
                    call_args = [self.current.type_env.get_node_type(x) for x in parent.args]
                    if typ.is_instance_method():
                        receiver = self.current.type_env.get_node_type(parent.args[0])
                        assert isinstance(receiver, types.Struct), f"Expected struct, got {receiver} at {node.span}"
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
        assert fn_def, f"No FnDef for {typ.debug()} at {typ.span}"
        typ = cast(types.Fn, type_map.resolve(typ))
        for type_arg in typ.type_args:
            assert not isinstance(type_arg, types.TypeParam), (
                f"Function {typ} has unresolved type paramaters at {typ.span}"
            )
        key = types.full_id(typ)
        if key in self.seen:
            return
        self.seen.add(key)
        self.queue.append(lower.FnSpec(typ, lower.TypeEnv(self.type_env, type_map, {}), fn_def, call_args))


def evaluate_types_based_on_parameters(
    fn_def: ast.FnDef,
    type_env: lower.TypeEnv,
    lowered_method_member_nodes: dict[ast.NodeId, ast.Member],
    call_args: list[types.Type],
) -> None:
    @dataclass
    class Scope:
        parent: Scope | None
        names: dict[str, types.Type]

        def bind(self, name: str, typ: types.Type) -> None:
            self.names[name] = typ

        def find(self, name: str) -> types.Type | None:
            res = self.names.get(name)
            if res:
                return res
            if self.parent:
                return self.parent.find(name)
            return None

    scope = Scope(None, {})
    for param, arg in zip(fn_def.decl.params, call_args):
        scope.bind(param.name, arg)

    def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        match node:
            case ast.Block():
                nonlocal scope
                scope = Scope(scope, {})
                try:
                    ast.walk(node, visit)
                finally:
                    scope = scope.parent
            case ast.Ident():
                ast.walk(node, visit)
                member_node = lowered_method_member_nodes.get(node.id)
                if member_node is not None:
                    # Look up the method on the concrete target type.
                    target_node = visit(member_node.target, node)
                    target = type_env.get_node_type(target_node)
                    assert isinstance(target, types.Struct), f"Expected Struct, got {target} at {node.span}"
                    method = target.method(member_node.name)
                    assert method is not None, f"Method {member_node.name} not found in {target} at {node.span}"

                    # Copy all type arguments over from the type that has been determined by the typechecker.
                    tc_typ = type_env.get_node_type(node)
                    assert isinstance(tc_typ, types.Fn), f"Expected Fn, got {tc_typ} at {node.span}"
                    method = replace(method, type_args=tc_typ.type_args, type_map=tc_typ.type_map)
                    type_env.set_node_type(node, method)
                    return node
                scoped_type = scope.find(node.name)
                if scoped_type is not None:
                    type_env.set_node_type(node, scoped_type)
            case ast.Call():
                ast.walk(node, visit)
                callee = type_env.get_node_type(node.callee)
                if isinstance(callee, types.Fn) and callee.is_instance_method():
                    type_env.set_node_type(node, callee.result)
            case ast.Let():
                ast.walk(node, visit)
                value = type_env.get_node_type(node.value)
                scope.bind(node.name, types.normalize_type(value))
            case ast.Assign():
                ast.walk(node, visit)
                if isinstance(node.target, ast.Ident):
                    value = type_env.get_node_type(node.value)
                    scope.bind(node.target.name, value)
            case _:
                ast.walk(node, visit)

        return node

    visit(fn_def, None)


def monomorphize(module: ast.Module, type_env: typechecker.TypeEnv) -> list[lower.FnSpec]:
    fn_defs: dict[types.TypeId, ast.FnDef] = {}
    main = None

    lowered_method_member_nodes: dict[ast.NodeId, ast.Member] = {}
    receivers: dict[types.TypeId, ast.Expr] = {}

    def find_fn_defs_and_lower_instance_methods(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        """Find all function definitions and convert all `ast.Member` nodes to `ast.Ident` nodes.
        Add the receiver node to the method call arguments, i.e. convert `receiver.method` to `method(receiver)`.
        """
        ast.walk(node, find_fn_defs_and_lower_instance_methods)
        match node:
            case ast.FnDef():
                typ = type_env.get_node_type(node.decl)[0]
                assert isinstance(typ, types.Fn)
                fn_defs[typ.id] = node
                if node.decl.name == "main":
                    nonlocal main
                    main = typ
            case ast.Member():
                callee = type_env.get_node_type(node)[0]
                if isinstance(callee, types.Fn) and callee.is_instance_method():
                    ident = ast.Ident(node.id, str(callee.fqn), [], node.span)
                    lowered_method_member_nodes[node.id] = node
                    receivers[callee.id] = node.target
                    return ident
            case ast.Call():
                callee = type_env.get_node_type(node.callee)[0]
                receiver_expr = receivers.get(callee.id)
                if receiver_expr is not None:
                    node.args = [receiver_expr, *node.args]
        return node

    find_fn_defs_and_lower_instance_methods(module, None)
    assert isinstance(main, types.Fn), f"main function not found: {main}"
    runner = Monomorphize(type_env, fn_defs, lowered_method_member_nodes)
    runner.enqueue_if_needed(main, None, None)
    runner.run()
    return runner.fn_specs
