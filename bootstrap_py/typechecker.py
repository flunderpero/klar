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

    # todo: return None if the node is a typecheck error
    def get_node_type(self, node: ast.Node) -> types.Type:
        return self.node_types[node.id]


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
        self.type_res_scope = types.TypeResScope(None, None)
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

    def error(self, err: error.Error) -> None:
        self.errors.append(err)

    def id(self) -> types.TypeId:
        return self.next_id()

    def type_node_type(self, node: ast.Type) -> types.Type:
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
                    return self.instance(declared.typ, node.type_args, node.span)
                return declared.typ
            case _:
                raise AssertionError(f"Type checking not implemented for: {node.__class__}")

    def declare_all(self, scope_node: ast.Node) -> None:
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
                    typ = types.Fn(self.id(), fqn, [], [], [], self.type_env.builtins.NoneTyp, decl.span, is_named=True)
                    existing = self.scope.forward_declare(decl.fullname(), typ)
                    if existing:
                        self.error(error.duplicate_fn(decl.fullname(), decl.span, existing.typ.span))
                case ast.Struct():
                    fqn = self.scope.fqn().concat(node.name)
                    typ = types.Struct(self.id(), fqn, [], [], [], [], node.span)
                    existing = self.scope.forward_declare(node.name, typ)
                    if existing:
                        self.error(error.duplicate_struct(node.name, node.span, existing.typ.span))
        # Stage 2: Parse type parameters.
        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef() | ast.Struct():
                    decl = node if isinstance(node, (ast.FnDecl, ast.Struct)) else node.decl
                    name = decl.name if isinstance(decl, ast.Struct) else decl.fullname()
                    typ = self.scope.get_forward_declared(name).typ
                    assert isinstance(typ, (types.Fn, types.Struct))
                    for type_param in decl.type_params:
                        t = types.TypeParam(self.id(), type_param.name, type_param.span)
                        typ.type_params.append(t)
                        typ.type_args.append(t)
        # Stage 3: Fully parse all previously forward declared types.
        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef():
                    decl = node if isinstance(node, ast.FnDecl) else node.decl
                    typ = self.scope.get_forward_declared(decl.fullname()).typ
                    assert isinstance(typ, types.Fn)
                    with self.child_scope(node):
                        for type_param in typ.type_params:
                            self.scope.declare(type_param.name, type_param)
                        if decl.receiver is not None:
                            # This is an instance method.
                            struct = self.scope.find(decl.receiver)
                            if struct is None:
                                self.error(error.undefined_name(decl.receiver, decl.span))
                                typ = types.TypeCheckError(self.id(), f"`{decl.receiver}` not found", decl.span)
                                self.type_env.set_node_type(decl, typ)
                                continue
                            struct_typ = struct.typ
                            assert isinstance(struct_typ, types.Struct)
                            existing_field = struct_typ.field_or_method(decl.name)
                            if existing_field is not None:
                                self.error(error.duplicate_field(decl.name, decl.span, existing_field.typ.span))
                                typ = types.TypeCheckError(self.id(), "duplicate field", decl.span)
                                self.type_env.set_node_type(decl, typ)
                                continue
                            struct_typ.methods.append(
                                types.FieldOrParam(decl.name, types.Instance(typ, self.type_res_scope))
                            )
                            # Declare all type parameters of the struct.
                            for type_param in struct_typ.type_params:
                                self.scope.declare(type_param.name, type_param)
                            self.scope.declare("Self", struct.typ)
                        elif any(x.name == "self" for x in decl.params):
                            self.error(error.self_not_allowed_here(decl.span))
                            typ = types.TypeCheckError(self.id(), "self not allowed here", decl.span)
                            self.type_env.set_node_type(decl, typ)
                            continue
                        params: list[types.FieldOrParam] = []
                        for param in decl.params:
                            param_typ = self.type_node_type(param.typ)
                            params.append(types.FieldOrParam(param.name, param_typ))
                        result = self.type_env.builtins.NoneTyp
                        if decl.result:
                            result = self.type_node_type(decl.result)
                        typ.params = params
                        typ.result = result
                    self.scope.finish_forward_declared(decl.fullname())
                    self.type_env.set_node_type(decl, typ)
                case ast.Struct():
                    typ = self.scope.get_forward_declared(node.name).typ
                    assert isinstance(typ, types.Struct)
                    with self.child_scope(node):
                        for type_param in typ.type_params:
                            self.scope.declare(type_param.name, type_param)
                        fields: list[types.FieldOrParam] = []
                        for field in node.fields:
                            field_typ = self.type_node_type(field.typ)
                            fields.append(types.FieldOrParam(field.name, field_typ))
                        typ.fields = fields
                    self.scope.finish_forward_declared(node.name)
                    self.type_env.set_node_type(node, typ)

    def typecheck_call(self, node: ast.Call) -> None:
        ast.walk(node, self.typecheck)
        callee = self.type_env.get_node_type(node.callee)
        callee_instance: types.Instance | None = None
        if isinstance(callee, types.Instance):
            callee_instance = callee
            callee_instance.infer_type_args_from_call_args([self.type_env.get_node_type(x) for x in node.args])
            callee = callee.resolve()

        params: list[types.FieldOrParam]
        result: types.Type
        match callee:
            case types.Fn():
                params = callee.params_without_self()
                result = callee.result
            case types.Struct():
                params = callee.fields
                result = callee
            case types.TypeCheckError():
                typ = types.TypeCheckError(self.id(), "cascaded error", node.span)
                self.type_env.set_node_type(node, typ)
                return
            case _ as t:
                self.error(error.unexpected_type("a callable type", str(t), node.callee.span))
                typ = types.TypeCheckError(self.id(), "unexpected type", node.span)
                self.type_env.set_node_type(node, typ)
                return

        if len(node.args) != len(params):
            self.error(error.wrong_number_of_args(node.callee.span, len(params), len(node.args), callee.span))
            self.type_env.set_node_type(node, types.TypeCheckError(self.id(), "wrong number of args", node.span))
            return
        for param, arg_node in zip(params, node.args):
            arg_typ = self.type_env.get_node_type(arg_node)
            if not types.is_assignable_from(param.typ, arg_typ):
                self.error(
                    error.type_not_assignable_from(arg_node.span, types.pretty(param.typ), types.pretty(arg_typ))
                )
                self.type_env.set_node_type(node, types.TypeCheckError(self.id(), "type not assignable", node.span))
                return
        if isinstance(callee, types.Struct):
            assert callee_instance and isinstance(result, types.Struct)
            result = types.Instance(result, callee_instance.type_res_scope)
        self.type_env.set_node_type(node, result)

    def instance(
        self,
        typ: types.Type,
        type_args: ast.TypeArgs,
        span: Span,
        parent_type_res_scope: types.TypeResScope | None = None,
    ) -> types.Instance | types.TypeCheckError:
        if not isinstance(typ, (types.Fn, types.Struct)):
            self.error(error.not_generic(span, typ.span))
            return types.TypeCheckError(typ.id, "not generic", span)
        type_params = typ.type_params
        if type_args and len(type_args) != len(type_params):
            self.error(error.wrong_number_of_type_args(len(type_params), len(type_args), span, typ.span))
            return types.TypeCheckError(typ.id, "wrong number of type args", span)
        type_res_scope = types.TypeResScope(parent_type_res_scope, None)
        for arg, type_param in zip(type_args, type_params):
            type_arg = self.type_node_type(arg)
            type_res_scope.declare(type_param, type_arg)
        return types.Instance(typ, type_res_scope)

    def typecheck(self, node: ast.Node, _parent: ast.Node | None) -> None:
        match node:
            case ast.Module():
                self.declare_all(node)
                ast.walk(node, self.typecheck)
            case ast.StrLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Str)
            case ast.IntLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Int)
            case ast.BoolLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Bool)
            case ast.Ident():
                declared = self.scope.find(node.name)
                if not declared:
                    self.error(error.undefined_name(node.name, node.span))
                    self.type_env.set_node_type(
                        node, types.TypeCheckError(self.id(), f"`{node.name}` not found", node.span)
                    )
                else:
                    typ = declared.typ
                    match declared.typ:
                        case types.Fn() | types.Struct():
                            typ = self.instance(typ, node.type_args, node.span)
                    self.type_env.set_node_type(node, typ)
            case ast.Member():
                ast.walk(node, self.typecheck)
                target_instance = self.type_env.get_node_type(node.target)
                assert isinstance(target_instance, types.Instance), f"Expected an instance, got: {target_instance}"
                target_typ = target_instance.resolve()
                assert isinstance(target_typ, types.Struct)
                field = target_typ.field_or_method(node.name)
                if not field:
                    self.error(error.no_member(node.name, str(target_typ.fqn), node.span, target_typ.span))
                    typ = types.TypeCheckError(self.id(), "no member", node.span)
                else:
                    typ = field.typ
                if isinstance(typ, (types.Struct, types.Fn)):
                    typ = self.instance(typ, node.type_args, node.span, target_instance.type_res_scope)
                self.type_env.set_node_type(node, typ)
            case ast.Call():
                self.typecheck_call(node)
            case ast.Struct():
                # The type is already fully forward declared.
                pass
            case ast.FnDef():
                fn = self.type_env.get_node_type(node.decl)
                if isinstance(fn, types.TypeCheckError):
                    return
                assert isinstance(fn, types.Fn), f"Expected a function type, got {fn}"
                with self.child_scope(node):
                    for i, param in enumerate(fn.params):
                        if param.name == "self":
                            if node.decl.receiver is None:
                                self.error(error.self_not_allowed_here(node.decl.params[i].span))
                                return
                            if i != 0:
                                self.error(error.self_not_allowed_here(node.decl.params[i].span))
                                return
                            struct_type = self.scope.find(node.decl.receiver)
                            assert struct_type and isinstance(struct_type.typ, types.Struct)
                            existing = self.scope.declare("self", struct_type.typ)
                            assert existing is None, f"self is already declared: {existing}"
                            continue
                        self.scope.declare(param.name, param.typ)
                    for type_param in fn.type_params:
                        self.scope.declare(type_param.name, type_param)
                    ast.walk(node.body, self.typecheck)
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
                    self.error(error.unexpected_type("Bool", types.pretty(cond), node.cond.span))
                then_block = self.type_env.get_node_type(node.then_block)
                typ: types.Type = self.type_env.builtins.NoneTyp
                if node.else_block:
                    else_block = self.type_env.get_node_type(node.else_block)
                    if not types.is_same(then_block, else_block):
                        # For now, both branches must have the same type.
                        self.error(
                            error.unexpected_type(
                                types.pretty(then_block), types.pretty(else_block), node.else_block.span
                            )
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
                    self.error(error.duplicate_param_name(node.name, node.span, declared.typ.span))
                    typ = types.TypeCheckError(self.id(), "duplicate name", node.span)
                self.type_env.set_node_type(node, typ)
            case ast.Assign():
                ast.walk(node, self.typecheck)
                assert isinstance(node.target, ast.Ident)
                target_typ = self.type_env.get_node_type(node.target)
                value_typ = self.type_env.get_node_type(node.value)
                declared = self.scope.find(node.target.name)
                if not declared:
                    self.error(error.undefined_name(node.target.name, node.target.span))
                elif not declared.mutable:
                    self.error(error.not_mutable(node.target.name, node.span))
                elif not types.is_assignable_from(target_typ, value_typ):
                    self.error(
                        error.type_not_assignable_from(
                            node.value.span, types.pretty(target_typ), types.pretty(value_typ)
                        )
                    )
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.BinaryExpr():
                ast.walk(node, self.typecheck)
                lhs = self.type_env.get_node_type(node.lhs)
                rhs = self.type_env.get_node_type(node.rhs)
                match node.op:
                    case ast.BinaryOp.eq | ast.BinaryOp.ne:
                        typ = self.type_env.builtins.Bool
                        if not isinstance(lhs, (types.Bool, types.Int)):
                            self.error(error.unexpected_type("Bool or Int", types.pretty(lhs), node.lhs.span))
                            typ = types.TypeCheckError(self.id(), "lhs not Bool or Int", node.span)
                        if not types.is_assignable_from(lhs, rhs):
                            self.error(
                                error.type_not_assignable_from(node.rhs.span, types.pretty(lhs), types.pretty(rhs))
                            )
                            typ = types.TypeCheckError(self.id(), "rhs not assignable to lhs", node.span)
                    case ast.BinaryOp.add | ast.BinaryOp.sub:
                        typ = lhs
                        match lhs:
                            case types.Int():
                                pass
                            case types.TypeCheckError():
                                typ = types.TypeCheckError(self.id(), "lhs is an error", node.span)
                            case _:
                                self.error(error.unexpected_type("Int", types.pretty(lhs), node.lhs.span))
                                typ = types.TypeCheckError(self.id(), "lhs not an Int", node.span)
                        if not types.is_assignable_from(lhs, rhs):
                            self.error(
                                error.type_not_assignable_from(node.rhs.span, types.pretty(lhs), types.pretty(rhs))
                            )
                            typ = types.TypeCheckError(self.id(), "rhs not assignable to lhs", node.span)
                    case _:
                        raise AssertionError(f"Type checking not implemented for: {node}")
                self.type_env.set_node_type(node, typ)
            case ast.FnDecl():
                # Declaration has already been handled in `self.declare_all()`.
                pass
            case _:
                raise AssertionError(f"Type checking not implemented for: {node.__class__}")


def typecheck(module: ast.Module, next_id: Callable[[], int]) -> tuple[TypeEnv, list[error.Error]]:
    tc = TypeChecker(type_env=TypeEnv(builtins=types.Builtins.new(next_id)), scope=Scope(module, None), next_id=next_id)
    tc.typecheck(module, None)
    return tc.type_env, tc.errors
