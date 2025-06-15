from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, replace
from typing import TYPE_CHECKING, Callable, cast

from . import ast, error, types
from .debug import debug
from .span import FQN

if TYPE_CHECKING:
    from collections.abc import Generator


class TypeEnv:
    node_types: dict[ast.NodeId, types.Type]
    builtins: types.Builtins

    def __init__(self, builtins: types.Builtins) -> None:
        self.builtins = builtins
        self.node_types = {}

    def set_node_type(self, node: ast.Node, type: types.Type) -> None:
        self.node_types[node.id] = type

    def get_node_type[T: types.Type](
        self, node: ast.Node, typ: type[T] | None = None
    ) -> tuple[T, types.TypeCheckError | None]:
        res = self.node_types[node.id]
        if typ is not None:
            if isinstance(res, types.TypeCheckError):
                return cast(T, res), res
            assert isinstance(res, typ), f"Expected {typ}, got {type(res)}"
        return cast(T, res), None


@dataclass
class ScopeDeclared[T: types.Type]:
    name: str
    typ: T
    mutable: bool


class Scope:
    parent: Scope | None
    node: ast.Node
    names: dict[str, ScopeDeclared]
    forwards: dict[str, ScopeDeclared]
    type_map: types.TypeMap

    def __init__(self, node: ast.Node, parent: Scope | None) -> None:
        self.node = node
        self.parent = parent
        self.names = {}
        self.forwards = {}
        if parent is None:
            self.type_map = types.TypeMap({}, None)
        else:
            self.type_map = types.TypeMap({}, parent.type_map)

    def declare[T: types.Type](self, name: str, typ: T, *, mutable: bool = False) -> ScopeDeclared[T] | None:
        existing = self.names.get(name)
        assert not existing, f"Duplicate declaration of {name} in {self.node}: {existing.typ} vs {typ}"
        self.names[name] = ScopeDeclared(name, typ, mutable)
        return None

    def forward_declare[T: types.Type](self, name: str, typ: T, *, mutable: bool = False) -> ScopeDeclared[T] | None:
        self.declare(name, typ, mutable=mutable)
        self.forwards[name] = ScopeDeclared(name, typ, mutable)
        return None

    def declare_type_params(self, type_params: types.TypeParams) -> None:
        for type_param in type_params:
            self.declare(type_param.name, type_param)

    def get_forward_declared[T: types.Type](self, name: str, typ: type[T] | None) -> ScopeDeclared[T]:
        res = self.forwards[name]
        if typ:
            assert isinstance(res.typ, typ), f"Expected {typ}, got {type(res.typ)}"
        return cast(ScopeDeclared[T], res)

    def is_declared_in_this_scope(self, name: str) -> bool:
        return name in self.names

    def finish_forward_declared(self, name: str) -> None:
        del self.forwards[name]

    def find(self, name: str) -> ScopeDeclared | None:
        res = self.names.get(name)
        if not res and self.parent:
            return self.parent.find(name)
        return res

    def is_within(self, node_typ: type) -> bool:
        if not isinstance(self.node, node_typ):
            return self.parent.is_within(node_typ) if self.parent else False
        return True

    def fqn(self) -> FQN:
        scope = self
        path = []
        while scope:
            match scope.node:
                case ast.Module():
                    path = scope.node.fqn.path + path
                case ast.FnDef():
                    path.insert(0, scope.node.decl.name)
            scope = scope.parent
        return FQN(path)

    def resolve(self, typ: types.Type) -> types.Type:
        return self.type_map.resolve(typ)

    def debug(self) -> str:
        lines = []
        scope = self
        indent = "    "
        while scope:
            lines.append(indent + "Node: " + ast.to_str_withoud_nid(scope.node).replace("\n", "\n" + indent))
            for name, decl in scope.names.items():
                lines.append(indent + f"    {name}: {decl.typ.debug()}")
            indent += "        "
            scope = scope.parent
        return f"Scope(fqn={self.fqn()}\n{'\n'.join(lines)}\n"


class TypeChecker:
    def __init__(
        self,
        module: ast.Module,
        next_id: Callable[[], int],
    ) -> None:
        self.builtins = types.Builtins.new(next_id)
        self.module = module
        self.next_id = next_id
        self.scope = Scope(module, None)
        self.type_env = TypeEnv(self.builtins)
        self.errors: list[error.Error] = []
        for k, v in self.builtins.builtins().items():
            self.scope.declare(k, v)

    def error(self, err: error.Error) -> types.Type:
        self.errors.append(err)
        return types.TypeCheckError(self.next_id(), err.short_message(), err.span)

    @contextmanager
    def child_scope(self, node: ast.Node) -> Generator[None]:
        prev = self.scope
        self.scope = Scope(node, self.scope)
        try:
            yield
        finally:
            self.scope = prev

    def parse_type_node(self, node: ast.Type) -> types.Type:
        match node:
            case ast.NamedType():
                declared = self.scope.find(node.name)
                if declared is None:
                    return self.error(error.undefined_name(node.name, node.span))
                if node.type_args:
                    # todo: Check that the number of type arguments matches the number of type parameters.
                    type_args = [self.parse_type_node(x) for x in node.type_args]
                    type_map = types.TypeMap({k.id: v for k, v in zip(declared.typ.type_params, type_args)}, None)
                    typ = replace(declared.typ)
                    typ.type_map = type_map
                    return typ
                return declared.typ
            case _:
                raise AssertionError(f"Not implemented for {node} ({type(node)})")

    def parse_type_param_node(self, node: ast.TypeParam) -> types.TypeParam:
        bound = None
        if node.trait_bound:  # todo: rename to node.bound
            bound = self.parse_type_node(node.trait_bound)
            if not isinstance(bound, (types.Trait, types.Struct)):
                self.error(error.invalid_type_param_bound(node.name, node.span))
                bound = None
        return types.TypeParam(self.next_id(), node.name, bound, node.span)

    def declare_all(self, parent_node: ast.Node) -> None:
        assert isinstance(parent_node, ast.Module), f"Expected Module, got {parent_node}"
        fn_defs = [x for x in parent_node.nodes if isinstance(x, ast.FnDef)]
        structs = [x for x in parent_node.nodes if isinstance(x, ast.Struct)]
        traits = [x for x in parent_node.nodes if isinstance(x, ast.Trait)]
        trait_default_impls: dict[types.TypeId, ast.FnDef] = {}

        # Stage 1: Make all type names known.
        for node in fn_defs:
            decl = node.decl
            fqn = self.scope.fqn()
            if decl.receiver:
                fqn = fqn.concat(decl.receiver)
            fqn = fqn.concat(decl.name)
            typ = types.Fn(
                self.next_id(), fqn, [], [], self.scope.type_map, [], self.builtins.NoneTyp, node.span, is_named=True
            )
            existing = self.scope.forward_declare(decl.fullname(), typ)
            if existing:
                self.error(error.duplicate_declaration(decl.fullname(), node.span, existing.typ.span))
        for node in structs:
            fqn = self.scope.fqn().concat(node.name)
            self_typ = types.TypeParam(self.next_id(), "Self", None, node.span)
            typ = types.Struct(self.next_id(), fqn, [], [], self.scope.type_map, self_typ, [], [], [], node.span)
            self_typ.bound = typ
            existing = self.scope.forward_declare(node.name, typ)
            if existing:
                self.error(error.duplicate_declaration(node.name, node.span, existing.typ.span))
        for node in traits:
            fqn = self.scope.fqn().concat(node.name)
            self_typ = types.TypeParam(self.next_id(), "Self", None, node.span)
            typ = types.Trait(self.next_id(), fqn, [], [], self.scope.type_map, self_typ, [], [], node.span)
            typ.self_typ.bound = typ
            existing = self.scope.forward_declare(node.name, typ)
            if existing:
                self.error(error.duplicate_declaration(node.name, node.span, existing.typ.span))
                continue
            for method_node in node.methods:
                fqn = self.scope.fqn().concat(node.name).concat(method_node.name)
                method_typ = types.Fn(
                    self.next_id(),
                    fqn,
                    [],
                    [],
                    self.scope.type_map,
                    [],
                    self.builtins.NoneTyp,
                    node.span,
                    is_named=True,
                )
                existing = typ.method(method_node.name)
                if existing:
                    # todo: test
                    self.error(error.duplicate_declaration(method_node.fullname(), node.span, existing.span))
                else:
                    typ.methods.append(types.Field(method_node.name, method_typ))

        # Stage 2: Make all type parameters known.
        for node in fn_defs + structs + traits:
            name = node.decl.fullname() if isinstance(node, ast.FnDef) else node.name
            type_params = node.decl.type_params if isinstance(node, ast.FnDef) else node.type_params
            declared = self.scope.get_forward_declared(name, None)
            with self.child_scope(node):
                for type_param_node in type_params:
                    type_param = self.parse_type_param_node(type_param_node)
                    declared.typ.type_params.append(type_param)
                    declared.typ.type_args.append(type_param)
                    self.scope.declare(type_param.name, type_param)

        # Stage 3: Complete all declared types.
        for node in fn_defs:
            decl = node.decl
            declared = self.scope.get_forward_declared(decl.fullname(), types.Fn)
            if decl.receiver is not None and not self.scope.is_declared_in_this_scope(decl.receiver):
                typ = self.error(error.not_declared_in_current_scope(decl.receiver, node.span))
                self.type_env.set_node_type(node, typ)
                self.type_env.set_node_type(decl, typ)
                continue
            with self.child_scope(node):
                self.scope.declare_type_params(declared.typ.type_params)
                if decl.receiver is not None:
                    # todo: check that the first parameter is self.
                    receiver = self.scope.find(decl.receiver)
                    assert receiver is not None, f"receiver should be found: {decl.receiver}"
                    self.scope.declare("Self", receiver.typ.self_typ)
                    self.scope.declare_type_params(receiver.typ.type_params)
                    receiver.typ.methods.append(types.Field(decl.name, declared.typ))
                    if decl.trait_qualifier is not None:
                        # todo: check trait method implementation
                        trait = self.parse_type_node(decl.trait_qualifier)
                        receiver.typ.traits.append(trait)
                elif any(x.name == "self" for x in decl.params):
                    err = self.error(error.self_not_allowed_here(node.span))
                    self.type_env.set_node_type(node, err)
                for param_node in decl.params:
                    param_typ = self.parse_type_node(param_node.typ)
                    declared.typ.params.append(types.Field(param_node.name, param_typ))
                if decl.result:
                    declared.typ.result = self.parse_type_node(decl.result)
            typ = declared.typ
            if (
                typ.is_instance_method()
                and isinstance(typ.params[0].typ, types.TypeParam)
                and isinstance(typ.params[0].typ.bound, types.Trait)
            ):
                # This is a default implementation of a trait method.
                # Add the self type as a type parameter.
                typ.type_params.append(typ.params[0].typ)
                typ.type_args.append(typ.params[0].typ)
                trait_default_impls[declared.typ.id] = node
            self.scope.finish_forward_declared(decl.fullname())
            self.type_env.set_node_type(node, typ)
            self.type_env.set_node_type(decl, typ)
        for node in structs:
            declared = self.scope.get_forward_declared(node.name, types.Struct)
            with self.child_scope(node):
                self.scope.declare_type_params(declared.typ.type_params)
                for field_node in node.fields:
                    field_typ = self.parse_type_node(field_node.typ)
                    # todo: check for duplicate fields
                    declared.typ.fields.append(types.Field(field_node.name, field_typ))
            self.scope.finish_forward_declared(node.name)
            self.type_env.set_node_type(node, declared.typ)
        for node in traits:
            declared = self.scope.get_forward_declared(node.name, types.Trait)
            with self.child_scope(node):
                self.scope.declare_type_params(declared.typ.type_params)
                for method_node in node.methods:
                    typ = declared.typ.method(method_node.name)
                    assert typ is not None, f"Method {method_node.name} not found in {declared.typ}"
                    for i, param_node in enumerate(method_node.params):
                        if param_node.name == "self":
                            if i != 0:
                                self.error(error.self_not_allowed_here(param_node.span))
                                continue
                            param_typ = declared.typ.self_typ
                        else:
                            param_typ = self.parse_type_node(param_node.typ)
                        typ.params.append(types.Field(param_node.name, param_typ))
                    if method_node.result:
                        typ.result = self.parse_type_node(method_node.result)
            self.scope.finish_forward_declared(node.name)
            self.type_env.set_node_type(node, declared.typ)

        # Stage 4: Add trait default implementations to implementing types.
        for node in structs:
            typ, err = self.type_env.get_node_type(node, types.Struct)
            if err:
                continue
            for trait in typ.traits:
                for method in trait.methods:
                    fn_def = trait_default_impls.get(method.typ.id)
                    if not fn_def:
                        continue
                    if typ.method(method.name) is None:
                        # Make a copy of the method and bind the trait's self type to the struct.
                        method_typ = replace(method.typ, type_map=method.typ.type_map.flatten())
                        method_typ.type_map.bind(trait.self_typ, typ)
                        method_typ = types.resolve(method_typ)
                        typ.methods.append(types.Field(method.name, method_typ))

    def tc_binary_expr(self, node: ast.BinaryExpr) -> types.Type:
        debug(9, node, "ast.BinaryExpr")
        ast.walk(node, self.tc)
        lhs, err = self.type_env.get_node_type(node.lhs)
        if err:
            return err
        rhs, err = self.type_env.get_node_type(node.rhs)
        if err:
            return err
        # todo: why is this needed?
        # lhs = types.normalize_type(self.next_id, lhs)
        # rhs = types.normalize_type(self.next_id, rhs)
        match node.op:
            case ast.BinaryOp.eq | ast.BinaryOp.ne:
                if not isinstance(lhs, (types.Bool, types.Int)):
                    return self.error(error.unexpected_type("Bool or Int", lhs.signature(), node.lhs.span))
                if not types.is_assignable_from(lhs, rhs):
                    return self.error(error.type_not_assignable_from(node.rhs.span, lhs.signature(), rhs.signature()))
            case ast.BinaryOp.add | ast.BinaryOp.sub:
                match lhs:
                    case types.Int():
                        pass
                    case types.TypeCheckError():
                        return types.TypeCheckError(self.next_id(), "lhs is an error", node.span)
                    case _:
                        return self.error(error.unexpected_type("Int", lhs.signature(), node.lhs.span))
                if not types.is_assignable_from(lhs, rhs):
                    return self.error(error.type_not_assignable_from(node.rhs.span, lhs.signature(), rhs.signature()))
                return lhs
            case _:
                raise AssertionError(f"Type checking not implemented for: {node}")
        return self.builtins.Bool

    def tc_block(self, node: ast.Block) -> types.Type:
        debug(9, node, "ast.Block")
        typ = self.type_env.builtins.NoneTyp
        if node.nodes:
            with self.child_scope(node):
                ast.walk(node, self.tc)
                typ, err = self.type_env.get_node_type(node.nodes[-1])
                if err:
                    return err
        return typ

    def tc_call(self, node: ast.Call) -> types.Type:
        debug(9, node, "ast.Call")
        ast.walk(node, self.tc)
        callee, err = self.type_env.get_node_type(node.callee)
        if err:
            return err
        match callee:
            case types.Fn():
                return callee.result
            case types.Struct():
                return callee
            case _:
                return self.error(error.not_callable(node.callee.span, callee.span))

    def tc_fn_def(self, node: ast.FnDef) -> types.Type:
        debug(9, node, f"ast.FnDef {node.decl.fullname()}")
        fn, err = self.type_env.get_node_type(node.decl, types.Fn)
        if err:
            return err
        with self.child_scope(node):
            if fn.is_instance_method():
                assert len(fn.params) > 0, "Instance methods must have at least one parameter (self)"
                self_typ = fn.params[0].typ
                assert isinstance(self_typ, types.TypeParam) and self_typ.bound is not None, (
                    "Instance methods must have a type parameter bound to a struct"
                )
                self.scope.declare_type_params(self_typ.bound.type_params)
                for type_param in self_typ.bound.type_params:
                    self.scope.type_map.bind(type_param, self_typ.type_map.resolve(type_param))
            self.scope.declare_type_params(fn.type_params)
            for _, param in enumerate(fn.params):
                self.scope.declare(param.name, param.typ)
            self.tc(node.body, node)
            # todo: assert block_typ is assignable to function result
            # block_typ = self.type_env.get_node_type(node.body)
        return self.builtins.NoneTyp

    def tc_ident(self, node: ast.Ident) -> types.Type:
        debug(9, node, "ast.Ident")
        declared = self.scope.find(node.name)
        if declared is None:
            return self.error(error.undefined_name(node.name, node.span))
        if node.type_args:
            if len(node.type_args) != len(declared.typ.type_params):
                return self.error(
                    error.wrong_number_of_type_args(
                        len(declared.typ.type_params), len(node.type_args), node.span, declared.typ.span
                    )
                )
            # todo: check that type args are assignable.
            with self.child_scope(node):
                for type_param, type_arg_node in zip(declared.typ.type_params, node.type_args):
                    type_arg = self.parse_type_node(type_arg_node)
                    if isinstance(type_arg, types.TypeCheckError):
                        return type_arg
                    self.scope.type_map.bind(type_param, type_arg)
                return self.scope.resolve(declared.typ)
        return declared.typ

    def tc_if(self, node: ast.If) -> types.Type:
        debug(9, node, "ast.If")
        ast.walk(node, self.tc)
        cond, err = self.type_env.get_node_type(node.cond)
        if err:
            return err
        if not isinstance(cond, types.Bool):
            return self.error(error.unexpected_type("Bool", cond.signature(), node.cond.span))

        then_block, err = self.type_env.get_node_type(node.then_block)
        if err:
            return err
        typ: types.Type = self.type_env.builtins.NoneTyp
        if node.else_block:
            else_block, err = self.type_env.get_node_type(node.else_block)
            if err:
                return err
            if not types.is_assignable_from(then_block, else_block) or not types.is_assignable_from(
                else_block, then_block
            ):
                # For now, both branches must have the same type.
                return self.error(
                    error.unexpected_type(then_block.signature(), else_block.signature(), node.else_block.span)
                )
            typ = types.normalize_type(self.next_id, then_block)
        self.type_env.set_node_type(node, typ)
        return typ

    def tc_let(self, node: ast.Let) -> types.Type:
        debug(9, node, "ast.Let")
        self.tc(node.value, node)
        typ, err = self.type_env.get_node_type(node.value)
        if err:
            return err
        self.scope.declare(node.name, typ)
        return self.builtins.NoneTyp

    def tc_member(self, node: ast.Member) -> types.Type:
        debug(9, node, "ast.Member")
        ast.walk(node, self.tc)
        target, err = self.type_env.get_node_type(node.target)
        if err:
            return err
        if isinstance(target, types.TypeParam):
            if target.bound is None:
                return self.error(error.type_param_not_bound(node.name, node.span))
            target = target.bound
        match target:
            case types.TypeParam():
                bound = target.bound
                if bound is None:
                    return self.error(error.type_param_not_bound(node.name, node.span))
                target = bound
            case types.Struct():
                for field in target.fields:
                    if field.name == node.name:
                        return field.typ
                for method in target.methods:
                    if method.name == node.name:
                        return method.typ
                print("no member", id(target), target, node.name)
                return self.error(error.no_member(node.name, str(target.fqn), node.span, target.span))
            case types.Trait():
                for method in target.methods:
                    if method.name == node.name:
                        return method.typ
                return self.error(error.no_member(node.name, str(target.fqn), node.span, target.span))
        return self.error(error.unexpected_type("struct", target.signature(), node.span))

    def tc_module(self, node: ast.Module) -> types.Type:
        debug(9, node, "ast.Module")
        self.declare_all(node)
        ast.walk(node, self.tc)
        return self.builtins.NoneTyp

    def tc_struct(self, node: ast.Struct) -> types.Type:
        debug(9, node, "ast.Struct")
        return self.type_env.get_node_type(node, types.Struct)[0]

    def tc_trait(self, node: ast.Trait) -> types.Type:
        debug(9, node, "ast.Trait")
        return self.type_env.get_node_type(node, types.Trait)[0]

    def tc(self, node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        typ: types.Type
        match node:
            case ast.BinaryExpr():
                typ = self.tc_binary_expr(node)
            case ast.Block():
                typ = self.tc_block(node)
            case ast.BoolLit():
                typ = self.type_env.builtins.Bool
            case ast.Call():
                typ = self.tc_call(node)
            case ast.Ident():
                typ = self.tc_ident(node)
            case ast.FnDef():
                typ = self.tc_fn_def(node)
            case ast.If():
                typ = self.tc_if(node)
            case ast.IntLit():
                typ = self.type_env.builtins.Int
            case ast.Let():
                typ = self.tc_let(node)
            case ast.Member():
                typ = self.tc_member(node)
            case ast.Module():
                typ = self.tc_module(node)
            case ast.StrLit():
                typ = self.type_env.builtins.Str
            case ast.Struct():
                typ = self.tc_struct(node)
            case ast.Trait():
                typ = self.tc_trait(node)
            case _:
                raise AssertionError(f"Not implemented for {node} ({type(node)})")
        self.type_env.set_node_type(node, typ)
        return node


def typecheck(module: ast.Module, next_id: Callable[[], int]) -> tuple[TypeEnv, list[error.Error]]:
    tc = TypeChecker(module, next_id)
    tc.tc(module, None)
    return tc.type_env, tc.errors
