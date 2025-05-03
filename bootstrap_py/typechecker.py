from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from typing import TYPE_CHECKING, Callable

from . import ast, error, types
from .span import FQN, Span

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

    def get_node_type(self, node: ast.Node) -> types.Type:
        return self.node_types[node.id]

    def get_node_type_if_exists(self, node: ast.Node) -> types.Type | None:
        return self.node_types.get(node.id)


@dataclass
class ScopeDeclared:
    name: str
    typ: types.Type
    mutable: bool


class Scope:
    parent: Scope | None
    node: ast.Node
    names: dict[str, ScopeDeclared]
    forwards: dict[str, ScopeDeclared]

    def __init__(self, node: ast.Node, parent: Scope | None) -> None:
        self.node = node
        self.parent = parent
        self.names = {}
        self.forwards = {}

    def declare(self, name: str, typ: types.Type, *, mutable: bool = False) -> ScopeDeclared | None:
        existing = self.names.get(name)
        if existing:
            return existing
        self.names[name] = ScopeDeclared(name, typ, mutable)
        return None

    def forward_declare(self, name: str, typ: types.Type, *, mutable: bool = False) -> ScopeDeclared | None:
        existing = self.declare(name, typ, mutable=mutable)
        if existing:
            return existing
        self.forwards[name] = ScopeDeclared(name, typ, mutable)
        return None

    def get_forward_declared(self, name: str) -> ScopeDeclared:
        return self.forwards[name]

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


class TypeChecker:
    type_env: TypeEnv
    scope: Scope
    next_id: Callable[[], int]
    errors: list[error.Error]

    def __init__(self, type_env: TypeEnv, scope: Scope, next_id: Callable[[], int]) -> None:
        self.type_env = type_env
        self.scope = scope
        self.next_id = next_id
        self.errors = []
        self.scope.declare("Str", self.type_env.builtins.Str)
        self.scope.declare("Int", self.type_env.builtins.Int)
        self.scope.declare("Bool", self.type_env.builtins.Bool)
        self.scope.declare("None", self.type_env.builtins.NoneTyp)
        self.scope.declare("print", self.type_env.builtins.print)
        self.scope.declare("int_to_str", self.type_env.builtins.int_to_str)
        self.scope.declare("bool_to_str", self.type_env.builtins.bool_to_str)

    @contextmanager
    def child_scope(self, node: ast.Node) -> Generator[None]:
        prev = self.scope
        self.scope = Scope(node, self.scope)
        try:
            yield
        finally:
            self.scope = prev

    def error(self, err: error.Error, node: ast.Node | None = None) -> None:
        """Add an error to the internal error list. Set the node type to `types.TypeCheckError` if given."""
        self.errors.append(err)
        if node is not None:
            type_check_error = types.TypeCheckError(self.next_id(), err.short_message(), node.span)
            self.type_env.set_node_type(node, type_check_error)

    def id(self) -> types.TypeId:
        return self.next_id()

    def self_typ_(self) -> types.TypeParam:
        return types.TypeParam(self.id(), "Self", None, types.built_in_span)

    def type_node_type(self, node: ast.Type | ast.TypeParam) -> types.Type:
        match node:
            case ast.FnType():
                type_params = []
                for tp in node.type_params:
                    declared = self.scope.find(tp.name)
                    if declared is None:
                        self.error(error.undefined_name(tp.name, tp.span))
                        return types.TypeCheckError(self.id(), f"`{tp.name}` not found", tp.span)
                    if not isinstance(declared.typ, types.TypeParam):
                        self.error(error.unexpected_type(types.TypeParam.__name__, str(declared.typ), tp.span))
                        return types.TypeCheckError(self.id(), "not a type param", tp.span)
                    type_params.append(declared.typ)
                return types.Fn(
                    self.id(),
                    self.scope.fqn().concat("<anonymous>"),
                    type_params,
                    type_params,
                    None,
                    [types.FieldOrParam(f"p{i + 1}", self.type_node_type(x)) for i, x in enumerate(node.params)],
                    self.type_node_type(node.result),
                    node.span,
                    is_named=False,
                )
            case ast.NamedType():
                declared = self.scope.find(node.name)
                if declared is None:
                    self.error(error.undefined_name(node.name, node.span))
                    return types.TypeCheckError(self.id(), f"`{node.name}` not found", node.span)
                if node.type_args:
                    if not isinstance(declared.typ, types.ParameterizedType):
                        self.error(error.not_generic(node.span, declared.typ.span))
                        return types.TypeCheckError(self.id(), "not generic", node.span)
                    return self.instance(declared.typ, node.type_args, node.span)
                return declared.typ
            case ast.TypeParam():
                type_param = types.TypeParam(self.id(), node.name, None, node.span)
                if node.trait_bound:
                    tb = self.type_node_type(node.trait_bound)
                    if not isinstance(tb, types.Trait):
                        self.error(error.unexpected_type("trait", str(tb), node.trait_bound.span))
                        return types.TypeCheckError(self.id(), "not a trait", node.trait_bound.span)
                    type_param.trait_bound = tb
                return type_param
            case _:
                raise AssertionError(f"Type checking not implemented for: {node.__class__}")

    def declare_all(self, scope_node: ast.Node) -> None:
        """Forward declare all types."""
        match scope_node:
            case ast.Module():
                nodes = scope_node.nodes
            case _:
                raise AssertionError(f"Unexpected node type: {scope_node}")
        # Stage 1: Make all types known without actually parsing them.
        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef():
                    decl = node if isinstance(node, ast.FnDecl) else node.decl
                    fqn = self.scope.fqn().concat(decl.fullname())
                    typ = types.Fn(
                        self.id(), fqn, [], [], None, [], self.type_env.builtins.NoneTyp, decl.span, is_named=True
                    )
                    existing = self.scope.forward_declare(decl.fullname(), typ)
                    if existing:
                        self.error(error.duplicate_declaration(decl.fullname(), decl.span, existing.typ.span))
                case ast.Struct():
                    fqn = self.scope.fqn().concat(node.name)
                    typ = types.Struct(self.id(), fqn, [], [], self.self_typ_(), None, [], [], [], node.span)
                    existing = self.scope.forward_declare(node.name, typ)
                    if existing:
                        self.error(error.duplicate_declaration(node.name, node.span, existing.typ.span))
                case ast.Trait():
                    fqn = self.scope.fqn().concat(node.name)
                    typ = types.Trait(self.id(), fqn, [], [], self.self_typ_(), None, [], node.span)
                    existing = self.scope.forward_declare(node.name, typ)
                    if existing:
                        self.error(error.duplicate_declaration(node.name, node.span, existing.typ.span))
        # Stage 2: Parse type parameters.
        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef() | ast.Struct() | ast.Trait():
                    decl = node if isinstance(node, ast.ParameterizedNode) else node.decl
                    name = decl.name if isinstance(decl, (ast.Struct, ast.Trait)) else decl.fullname()
                    typ = self.scope.get_forward_declared(name).typ
                    assert isinstance(typ, types.ParameterizedType)
                    for type_param in decl.type_params:
                        type_param_typ = self.type_node_type(type_param)
                        assert isinstance(type_param_typ, types.TypeParam)
                        typ.type_params.append(type_param_typ)
                        typ.type_args.append(type_param_typ)
                    if isinstance(decl, ast.FnDecl) and decl.receiver is not None:
                        # This is a method, add `Self` to the list of type parameters.
                        if not self.scope.is_declared_in_this_scope(decl.receiver):
                            if not self.scope.find(decl.receiver):
                                continue
                            self.error(error.not_declared_in_current_scope(decl.receiver, decl.span), decl)
                            continue
                        receiver = self.scope.get_forward_declared(decl.receiver)
                        assert receiver is not None and isinstance(receiver.typ, types.ImplementableType)
                        typ.type_params.append(receiver.typ.self_typ)

        # Stage 3: Fully parse all previously forward declared types.
        def stage3_fndecl(node: ast.Node, decl: ast.FnDecl, typ: types.Fn) -> None:
            with self.child_scope(node):
                for type_param in typ.type_params:
                    self.scope.declare(type_param.name, type_param)
                impl_typ: types.ImplementableType | None = None
                if decl.receiver is not None:
                    # This is an instance method.
                    receiver = self.scope.find(decl.receiver)
                    if receiver is None:
                        self.error(error.undefined_name(decl.receiver, decl.span), decl)
                        return
                    assert isinstance(receiver.typ, types.ImplementableType)
                    impl_typ = receiver.typ
                    existing = impl_typ.member(decl.name)
                    if existing is not None:
                        self.error(error.duplicate_declaration(decl.name, decl.span, existing.typ.span), decl)
                        return
                    # Declare all type parameters of the struct.
                    for type_param in impl_typ.type_params:
                        self.scope.declare(type_param.name, type_param)
                    # Mark a trait as implemented if this is a trait impl.
                    # Also type check the trait qualifier.
                    if decl.trait_qualifier is not None:
                        assert isinstance(impl_typ, types.Struct)
                        trait_typ = self.type_node_type(decl.trait_qualifier)
                        if not isinstance(trait_typ, types.Trait):
                            self.error(error.unexpected_type("trait", str(trait_typ), decl.trait_qualifier.span), decl)
                            return
                        if len(decl.trait_qualifier.type_args) != len(trait_typ.type_params):
                            self.error(
                                error.wrong_number_of_type_args(
                                    len(trait_typ.type_params),
                                    len(decl.trait_qualifier.type_args),
                                    decl.trait_qualifier.span,
                                    trait_typ.span,
                                ),
                                decl,
                            )
                            return
                        trait_typ = self.instance(trait_typ, decl.trait_qualifier.type_args, decl.trait_qualifier.span)
                        if isinstance(trait_typ, types.TypeCheckError):
                            return
                        existing = impl_typ.trait(trait_typ.fqn)
                        if existing:
                            existing = types.resolve(existing)
                            if not types.is_same(existing, types.resolve(trait_typ)):
                                self.error(
                                    error.trait_qualifier_mismatch(
                                        existing.signature(), str(impl_typ.fqn), trait_typ.span, decl.span
                                    ),
                                    decl,
                                )
                        else:
                            impl_typ.traits.append(trait_typ)
                elif any(x.name == "self" for x in decl.params):
                    self.error(error.self_not_allowed_here(decl.span), decl)
                    return
                params: list[types.FieldOrParam] = []
                for param in decl.params:
                    param_typ = self.type_node_type(param.typ)
                    params.append(types.FieldOrParam(param.name, param_typ))
                result = self.type_env.builtins.NoneTyp
                if decl.result:
                    result = self.type_node_type(decl.result)
                typ.params = params
                typ.result = result
                if impl_typ is not None:
                    impl_typ.methods.append(types.FieldOrParam(decl.name, typ))

        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef():
                    decl = node if isinstance(node, ast.FnDecl) else node.decl
                    typ = self.scope.get_forward_declared(decl.fullname()).typ
                    assert isinstance(typ, types.Fn)
                    stage3_fndecl(node, decl, typ)
                    self.scope.finish_forward_declared(decl.fullname())
                    self.type_env.set_node_type(decl, typ)
                case ast.Struct():
                    typ = self.scope.get_forward_declared(node.name).typ
                    assert isinstance(typ, types.Struct)
                    if typ.type_res_scope is None:
                        typ.type_res_scope = types.TypeResScope(None, None)
                    with self.child_scope(node):
                        for type_param in typ.type_params:
                            self.scope.declare(type_param.name, type_param)
                        fields: list[types.FieldOrParam] = []
                        for m_node in node.fields:
                            m_typ = self.type_node_type(m_node.typ)
                            fields.append(types.FieldOrParam(m_node.name, m_typ))
                        typ.fields = fields
                    self.scope.finish_forward_declared(node.name)
                    self.type_env.set_node_type(node, typ)
                case ast.Trait():
                    typ = self.scope.get_forward_declared(node.name).typ
                    assert isinstance(typ, types.Trait)
                    with self.child_scope(node):
                        for type_param in typ.type_params:
                            self.scope.declare(type_param.name, type_param)
                        for m_node in node.methods:
                            fqn = typ.fqn.concat(m_node.name)
                            m_typ = types.Fn(
                                self.id(),
                                fqn,
                                [],
                                [],
                                None,
                                [],
                                self.type_env.builtins.NoneTyp,
                                m_node.span,
                                is_named=True,
                            )
                            for type_param in m_node.type_params:
                                type_param_typ = self.type_node_type(type_param)
                                assert isinstance(type_param_typ, types.TypeParam)
                                m_typ.type_params.append(type_param_typ)
                                m_typ.type_args.append(type_param_typ)
                            m_typ.type_params.append(typ.self_typ)
                            m_typ.type_args.append(typ.self_typ)
                            stage3_fndecl(node, m_node, m_typ)
                            self.type_env.set_node_type(m_node, self.type_env.builtins.NoneTyp)
                    self.scope.finish_forward_declared(node.name)
                    self.type_env.set_node_type(node, typ)
        # Finalize and type check trait implementations.
        # In Klar, all instance methods have to be defined in the same scope as the
        # type declaration. So we now can make sure that all traits are fully implemented.
        for node in nodes:
            if not isinstance(node, ast.ImplementableNode):
                continue
            impl_typ = self.type_env.get_node_type(node)
            if isinstance(impl_typ, types.Trait):
                continue
            assert isinstance(impl_typ, types.ImplementableType)
            for m_typ in impl_typ.methods:
                self_typ = next((x for x in m_typ.typ.type_params if x.name == "Self"), None)
                assert self_typ is not None, f"Expected type parameter `Self` in {m_typ.typ.signature()}"
                assert self_typ.name == "Self", f"Expected `Self` as the first type parameter, got {self_typ.name}"
                if m_typ.typ.type_res_scope is None:
                    m_typ.typ.type_res_scope = types.TypeResScope(None, impl_typ.type_res_scope)
                m_typ.typ.type_res_scope.declare(self_typ, impl_typ)

            # If there are errors up until now, we don't check any further.
            if self.errors:
                continue

            # Resolving the `impl_typ` will also resolve all `impl_typ.traits` so the
            # method signatures should match.
            impl_typ = types.resolve(impl_typ)
            for trait in impl_typ.traits:
                trait_method_names = [method.name for method in trait.methods]
                for t_typ in trait.methods:
                    m_typ = next((x for x in impl_typ.methods if x.name == t_typ.name), None)
                    if m_typ is None:
                        impl_span = (
                            next((x.typ.span for x in impl_typ.methods if x.name in trait_method_names), None)
                            or node.span
                        )
                        trait_span = next(x.typ.span for x in trait.methods if x.name == t_typ.name)
                        self.error(error.trait_method_impl_missing(str(trait.fqn), t_typ.name, trait_span, impl_span))
                        continue
                    if not types.is_assignable_from(m_typ.typ, t_typ.typ):
                        self.error(
                            error.trait_method_impl_mismatch(
                                t_typ.typ.signature(), m_typ.typ.signature(), t_typ.typ.span, m_typ.typ.span
                            )
                        )
                        continue

    def typecheck_call(self, node: ast.Call) -> None:
        ast.walk(node, self.typecheck)
        callee = self.type_env.get_node_type(node.callee)
        if isinstance(callee, types.TypeCheckError):
            typ = types.TypeCheckError(self.id(), "cascaded error", node.span)
            self.type_env.set_node_type(node, typ)
            return
        if isinstance(callee, types.Member):
            callee = callee.deep_member().typ
        if not isinstance(callee, types.CallableType):
            self.error(error.unexpected_type("a callable type", callee.signature(), node.callee.span), node)
            return

        callee_type_res_scope = callee.type_res_scope
        types.infer_type_args_from_call_args(callee, [self.type_env.get_node_type(x) for x in node.args])
        callee = types.resolve(callee)

        params: list[types.FieldOrParam]
        result: types.Type
        match callee:
            case types.Fn():
                params = callee.params_without_self()
                result = callee.result
            case types.Struct():
                params = callee.fields
                result = callee
            case _ as t:
                raise AssertionError(f"Type checking not implemented for: {t}")

        if len(node.args) != len(params):
            self.error(error.wrong_number_of_args(node.callee.span, len(params), len(node.args), callee.span), node)
            return
        for param, arg_node in zip(params, node.args):
            arg_typ = types.resolve(self.type_env.get_node_type(arg_node))
            if not types.is_assignable_from(param.typ, arg_typ):
                self.error(
                    error.type_not_assignable_from(arg_node.span, param.typ.signature(), arg_typ.signature()), node
                )
                return
        if isinstance(callee, types.Struct):
            assert isinstance(result, types.Struct)
            result = types.instance(result, callee_type_res_scope)
        self.type_env.set_node_type(node, result)

    def instance[T: types.ParameterizedType](
        self,
        typ: T,
        type_args: ast.TypeArgs,
        span: Span,
        parent_type_res_scope: types.TypeResScope | None = None,
    ) -> T | types.TypeCheckError:
        type_params = [x for x in typ.type_params if x.name != "Self"]
        if type_args and len(type_args) != len(type_params):
            self.error(error.wrong_number_of_type_args(len(type_params), len(type_args), span, typ.span))
            return types.TypeCheckError(typ.id, "wrong number of type args", span)
        type_res_scope = types.TypeResScope(parent_type_res_scope, None)
        for arg, type_param in zip(type_args, type_params):
            type_arg = self.type_node_type(arg)
            type_res_scope.declare(type_param, type_arg)
        return types.instance(typ, type_res_scope)

    def typecheck(self, node: ast.Node, _parent: ast.Node | None) -> None:
        if isinstance(self.type_env.get_node_type_if_exists(node), types.TypeCheckError):
            # This node has already been marked as an error, so there is little point
            # in checking it further.
            return
        match node:
            case ast.Module():
                self.declare_all(node)
                ast.walk(node, self.typecheck)
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.StrLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Str)
            case ast.IntLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Int)
            case ast.BoolLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Bool)
            case ast.Ident():
                declared = self.scope.find(node.name)
                if not declared:
                    self.error(error.undefined_name(node.name, node.span), node)
                else:
                    typ = declared.typ
                    if isinstance(typ, types.ParameterizedType):
                        typ = self.instance(typ, node.type_args, node.span)
                    self.type_env.set_node_type(node, typ)
            case ast.Member():
                ast.walk(node, self.typecheck)
                target_instance = self.type_env.get_node_type(node.target)
                if isinstance(target_instance, types.TypeCheckError):
                    self.type_env.set_node_type(node, target_instance)
                    return
                type_res_scope = None
                if isinstance(target_instance, types.ParameterizedType):
                    type_res_scope = target_instance.type_res_scope
                target_instance = types.resolve(target_instance)
                if isinstance(target_instance, types.Member):
                    member_typ = target_instance.direct_member()
                    if not member_typ:
                        self.error(
                            error.no_member(
                                node.name,
                                str(target_instance.target.fqn),
                                node.span,
                                target_instance.span,
                            ),
                            node,
                        )
                        return
                    self.type_env.set_node_type(node, member_typ.typ)
                    return
                target_typ: types.Struct | types.Trait
                match target_instance:
                    case types.Struct():
                        target_typ = target_instance
                    case types.TypeParam():
                        if target_instance.trait_bound is None:
                            self.error(error.type_param_not_bound(target_instance.name, node.target.span), node)
                            return
                        target_typ = target_instance.trait_bound
                    case _:
                        self.error(
                            error.unexpected_type("a struct or trait", target_instance.signature(), node.target.span),
                            node,
                        )
                        return
                target_typ = types.resolve(target_typ)
                field = target_typ.member(node.name)
                if not field:
                    self.error(error.no_member(node.name, str(target_typ.fqn), node.span, target_typ.span), node)
                    return
                typ = types.Member(
                    self.next_id(), types.TypeResScope(type_res_scope, None), target_instance, node.name, node.span
                )
                self.type_env.set_node_type(node, typ)
            case ast.Call():
                self.typecheck_call(node)
            case ast.FnDef():
                fn = self.type_env.get_node_type(node.decl)
                assert isinstance(fn, types.Fn), f"Expected a function type, got {fn}"
                with self.child_scope(node):
                    for i, param in enumerate(fn.params):
                        if param.name == "self":
                            if node.decl.receiver is None:
                                self.error(error.self_not_allowed_here(node.decl.params[i].span), node)
                                return
                            if i != 0:
                                self.error(error.self_not_allowed_here(node.decl.params[i].span), node)
                                return
                            struct_type = self.scope.find(node.decl.receiver)
                            assert struct_type and isinstance(struct_type.typ, types.Struct)
                            existing = self.scope.declare("self", struct_type.typ)
                            assert existing is None, f"self is already declared: {existing}"
                            continue
                        self.scope.declare(param.name, param.typ)
                    for type_param in fn.type_params:
                        self.scope.declare(type_param.name, type_param)
                    self.typecheck(node.body, node)
                    self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.Block():
                with self.child_scope(node):
                    typ = self.type_env.builtins.NoneTyp
                    if node.nodes:
                        ast.walk(node, self.typecheck)
                        typ = self.type_env.get_node_type(node.nodes[-1])
                    self.type_env.set_node_type(node, typ)
            case ast.If():
                ast.walk(node, self.typecheck)
                cond = self.type_env.get_node_type(node.cond)
                if not isinstance(cond, types.Bool):
                    self.error(error.unexpected_type("Bool", cond.signature(), node.cond.span))
                then_block = self.type_env.get_node_type(node.then_block)
                typ: types.Type = self.type_env.builtins.NoneTyp
                if node.else_block:
                    else_block = self.type_env.get_node_type(node.else_block)
                    if not types.is_same(then_block, else_block):
                        # For now, both branches must have the same type.
                        self.error(
                            error.unexpected_type(then_block.signature(), else_block.signature(), node.else_block.span)
                        )
                        typ = types.TypeCheckError(self.id(), "then and else blocks have different types", node.span)
                    typ = types.normalize_type(self.next_id, then_block)
                self.type_env.set_node_type(node, typ)
            case ast.Loop():
                with self.child_scope(node):
                    ast.walk(node, self.typecheck)
                    self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.Continue():
                if not self.scope.is_within(ast.Loop):
                    self.error(error.continue_outside_loop(node.span))
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.Break():
                if not self.scope.is_within(ast.Loop):
                    self.error(error.break_outside_loop(node.span))
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.Let():
                ast.walk(node, self.typecheck)
                typ = self.type_env.get_node_type(node.value)
                typ = types.normalize_type(self.next_id, typ)
                declared = self.scope.declare(node.name, typ, mutable=node.mutable)
                if declared:
                    self.error(error.duplicate_declaration(node.name, node.span, declared.typ.span), node)
                    return
                self.type_env.set_node_type(node, typ)
            case ast.Assign():
                ast.walk(node, self.typecheck)
                assert isinstance(node.target, ast.Ident)
                var_typ = self.type_env.get_node_type(node.target)
                value_typ = self.type_env.get_node_type(node.value)
                declared = self.scope.find(node.target.name)
                if not declared:
                    self.error(error.undefined_name(node.target.name, node.target.span))
                elif not declared.mutable:
                    self.error(error.not_mutable(node.target.name, node.span))
                elif not types.is_assignable_from(var_typ, value_typ):
                    self.error(
                        error.type_not_assignable_from(node.value.span, var_typ.signature(), value_typ.signature())
                    )
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.BinaryExpr():
                ast.walk(node, self.typecheck)
                lhs = self.type_env.get_node_type(node.lhs)
                rhs = self.type_env.get_node_type(node.rhs)
                lhs = types.normalize_type(self.next_id, lhs)
                rhs = types.normalize_type(self.next_id, rhs)
                match node.op:
                    case ast.BinaryOp.eq | ast.BinaryOp.ne:
                        typ = self.type_env.builtins.Bool
                        if not isinstance(lhs, (types.Bool, types.Int)):
                            self.error(error.unexpected_type("Bool or Int", lhs.signature(), node.lhs.span))
                            typ = types.TypeCheckError(self.id(), "lhs not Bool or Int", node.span)
                        if not types.is_assignable_from(lhs, rhs):
                            self.error(error.type_not_assignable_from(node.rhs.span, lhs.signature(), rhs.signature()))
                            typ = types.TypeCheckError(self.id(), "rhs not assignable to lhs", node.span)
                    case ast.BinaryOp.add | ast.BinaryOp.sub:
                        typ = lhs
                        match lhs:
                            case types.Int():
                                pass
                            case types.TypeCheckError():
                                typ = types.TypeCheckError(self.id(), "lhs is an error", node.span)
                            case _:
                                self.error(error.unexpected_type("Int", lhs.signature(), node.lhs.span))
                                typ = types.TypeCheckError(self.id(), "lhs not an Int", node.span)
                        if not types.is_assignable_from(lhs, rhs):
                            self.error(error.type_not_assignable_from(node.rhs.span, lhs.signature(), rhs.signature()))
                            typ = types.TypeCheckError(self.id(), "rhs not assignable to lhs", node.span)
                    case _:
                        raise AssertionError(f"Type checking not implemented for: {node}")
                self.type_env.set_node_type(node, typ)
            case ast.FnDecl() | ast.Struct() | ast.Trait():
                # Declaration has already been handled in `self.declare_all()`.
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case _:
                raise AssertionError(f"Type checking not implemented for: {node.__class__}")


def typecheck(module: ast.Module, next_id: Callable[[], int]) -> tuple[TypeEnv, list[error.Error]]:
    tc = TypeChecker(type_env=TypeEnv(builtins=types.Builtins.new(next_id)), scope=Scope(module, None), next_id=next_id)
    tc.typecheck(module, None)
    return tc.type_env, tc.errors
