from __future__ import annotations

from typing import Callable

from . import ast, error, types


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


class Scope:
    names: dict[str, types.Type]

    def __init__(self) -> None:
        self.names = {}

    def declare(self, name: str, typ: types.Type) -> types.Type | None:
        existing = self.names.get(name)
        if existing:
            return existing
        self.names[name] = typ
        return None

    def find(self, name: str) -> types.Type | None:
        return self.names.get(name)


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
        self.scope.declare("None", self.type_env.builtins.NoneTyp)
        self.scope.declare("print", self.type_env.builtins.print)
        self.scope.declare("int_to_str", self.type_env.builtins.int_to_str)

    def error(self, err: error.Error) -> None:
        self.errors.append(err)

    def id(self) -> types.TypeId:
        return self.next_id()

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
                            self.error(error.type_not_assignable_from(arg_node.span, str(param.typ), str(arg_typ)))
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
            case ast.StrLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Str)
            case ast.IntLit():
                self.type_env.set_node_type(node, self.type_env.builtins.Int)
            case ast.Ident():
                typ = self.scope.find(node.name)
                if not typ:
                    self.error(error.undefined_name(node.name, node.span))
                    typ = types.TypeCheckError(self.id(), f"`{node.name}` not found", node.span)
                self.type_env.set_node_type(node, typ)
            case ast.Call():
                self.typecheck_call(node)
            case ast.FnDecl():
                typ = types.Fn(self.id(), node.span, [], self.type_env.builtins.NoneTyp)
                if existing := self.scope.declare(node.name, typ):
                    self.error(error.duplicate_fn(node.name, node.span, existing.span))
                self.type_env.set_node_type(node, typ)
            case ast.FnDef():
                ast.walk(node, self.typecheck)
                self.type_env.set_node_type(node, self.type_env.builtins.NoneTyp)
            case ast.Block():
                typ = self.type_env.builtins.NoneTyp
                if node.nodes:
                    ast.walk(node, self.typecheck)
                    typ = self.type_env.get_node_type(node.nodes[-1])
                self.type_env.set_node_type(node, typ)
            case _:
                raise AssertionError(f"Type checking not implemented for: {node}")


def typecheck(module: ast.Module, next_id: Callable[[], int]) -> tuple[TypeEnv, list[error.Error]]:
    tc = TypeChecker(type_env=TypeEnv(builtins=types.Builtins.new(next_id)), scope=Scope(), next_id=next_id)
    ast.walk(module, tc.typecheck)
    return tc.type_env, tc.errors
