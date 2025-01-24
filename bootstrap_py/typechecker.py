from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from typing import TYPE_CHECKING, Callable

from . import ast, error, types

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

    def error(self, err: error.Error) -> None:
        self.errors.append(err)

    def id(self) -> types.TypeId:
        return self.next_id()

    def type_node_type(self, node: ast.Type) -> types.Type:
        declared = self.scope.find(node.name)
        if not declared:
            self.error(error.undefined_name(node.name, node.span))
            return types.TypeCheckError(self.id(), f"`{node.name}` not found", node.span)
        return declared.typ

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
                    typ = types.Fn(self.id(), decl.span, [], self.type_env.builtins.NoneTyp)
                    existing = self.scope.forward_declare(decl.name, typ)
                    if existing:
                        self.error(error.duplicate_fn(decl.name, decl.span, existing.typ.span))
        # Stage 2: Fully parse all previously forward declared types.
        for node in nodes:
            match node:
                case ast.FnDecl() | ast.FnDef():
                    decl = node if isinstance(node, ast.FnDecl) else node.decl
                    typ = self.scope.get_forward_declared(decl.name).typ
                    assert isinstance(typ, types.Fn)
                    params: list[types.Param] = []
                    for param in decl.params:
                        param_typ = self.type_node_type(param.typ)
                        params.append(types.Param(param.name, param_typ))
                    result = self.type_env.builtins.NoneTyp
                    if decl.result:
                        result = self.type_node_type(decl.result)
                    typ.params = params
                    typ.result = result
                    self.scope.finish_forward_declared(decl.name)
                    self.type_env.set_node_type(decl, typ)

    def typecheck_call(self, node: ast.Call) -> None:
        ast.walk(node, self.typecheck)
        match self.type_env.get_node_type(node.callee):
            case types.Fn() as fn:
                if len(node.args) != len(fn.params):
                    self.error(error.wrong_number_of_args(node.callee.span, len(fn.params), len(node.args), fn.span))
                    typ = types.TypeCheckError(self.id(), "wrong number of args", node.span)
                else:
                    for param, arg_node in zip(fn.params, node.args):
                        arg_typ = self.type_env.get_node_type(arg_node)
                        if not types.is_assignable_from(param.typ, arg_typ):
                            self.error(
                                error.type_not_assignable_from(
                                    arg_node.span, types.pretty(param.typ), types.pretty(arg_typ)
                                )
                            )
                            typ = types.TypeCheckError(self.id(), "type not assignable", node.span)
                            break
                    else:
                        typ = fn.result
            case types.TypeCheckError as err:
                self.error(error.cascaded_error(node.callee.span, err.message, err.span))
                typ = types.TypeCheckError(self.id(), "cascaded error", node.span)
            case _ as t:
                self.error(error.unexpected_type("a callable type", str(t), node.callee.span))
                typ = types.TypeCheckError(self.id(), "unexpected type", node.span)
        self.type_env.set_node_type(node, typ)

    def typecheck(self, node: ast.Node) -> None:
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
                    self.type_env.set_node_type(node, declared.typ)
            case ast.Call():
                self.typecheck_call(node)
            case ast.FnDef():
                fn = self.type_env.get_node_type(node.decl)
                assert isinstance(fn, types.Fn)
                with self.child_scope(node):
                    for param in fn.params:
                        self.scope.declare(param.name, param.typ)
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
                    typ = then_block
                self.type_env.set_node_type(node, typ)
            case ast.Let():
                ast.walk(node, self.typecheck)
                value_typ = self.type_env.get_node_type(node.value)
                typ = value_typ
                declared = self.scope.declare(node.name, value_typ, mutable=node.mutable)
                if declared:
                    self.error(error.duplicate_param_name(node.name, node.span, declared.typ.span))
                    typ = types.TypeCheckError(self.id(), "duplicate name", node.span)
                self.type_env.set_node_type(node, typ)
            case ast.Assign():
                ast.walk(node, self.typecheck)
                assert isinstance(node.target, ast.Ident)
                typ = self.type_env.get_node_type(node.target)
                value_typ = self.type_env.get_node_type(node.value)
                declared = self.scope.find(node.target.name)
                if not declared:
                    self.error(error.undefined_name(node.target.name, node.target.span))
                    typ = types.TypeCheckError(self.id(), "undefined name", node.span)
                elif not declared.mutable:
                    self.error(error.not_mutable(node.target.name, node.span))
                    typ = types.TypeCheckError(self.id(), "immutable", node.span)
                elif not types.is_assignable_from(typ, value_typ):
                    self.error(
                        error.type_not_assignable_from(node.value.span, types.pretty(typ), types.pretty(value_typ))
                    )
                    typ = types.TypeCheckError(self.id(), "value not assignable to target", node.span)
                self.type_env.set_node_type(node, typ)
            case ast.BinaryExpr():
                match node.op:
                    case ast.BinaryOp.add | ast.BinaryOp.sub:
                        ast.walk(node, self.typecheck)
                        lhs = self.type_env.get_node_type(node.lhs)
                        rhs = self.type_env.get_node_type(node.rhs)
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
                        self.type_env.set_node_type(node, typ)
                    case _:
                        raise AssertionError(f"Type checking not implemented for: {node}")
            case ast.FnDecl():
                # Declaration has already been handled in `self.declare_all()`.
                pass
            case _:
                raise AssertionError(f"Type checking not implemented for: {node}")


def typecheck(module: ast.Module, next_id: Callable[[], int]) -> tuple[TypeEnv, list[error.Error]]:
    tc = TypeChecker(type_env=TypeEnv(builtins=types.Builtins.new(next_id)), scope=Scope(module, None), next_id=next_id)
    tc.typecheck(module)
    return tc.type_env, tc.errors
