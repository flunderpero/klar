from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable

from .span import FQN, Span

TypeId = int


def tid(id: TypeId) -> str:
    return f"{{{id}}}"


@dataclass
class TypeCheckError:
    id: TypeId
    message: str
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + f"TypeCheckError({self.message})"


@dataclass
class Str:
    id: TypeId
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + "Str"


@dataclass
class Int:
    id: TypeId
    bits: int
    signed: bool
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + f"I{self.bits}" if self.signed else f"U{self.bits}"


@dataclass
class Bool:
    id: TypeId
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + "Bool"


@dataclass
class NoneTyp:
    id: TypeId
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + "None"


@dataclass
class Param:
    name: str
    typ: Type


@dataclass
class Fn:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    params: list[Param]
    result: Type
    span: Span

    def __str__(self) -> str:
        type_params = type_params_to_str(self.type_params)
        params = ", ".join(f"{p.name}: {p.typ}" for p in self.params)
        return tid(self.id) + f"fn {self.fqn}{type_params}({params}) -> {self.result}"


@dataclass
class TypeParam:
    id: TypeId
    name: str
    span: Span

    def __str__(self) -> str:
        return tid(self.id) + self.name


TypeParams = list[TypeParam]
TypeArgs = list["Type"]


def type_params_to_str(params: TypeParams) -> str:
    if not params:
        return ""
    return f"<{', '.join(str(x) for x in params)}>"


def type_args_to_str(type_params: TypeParams, type_args: TypeArgs) -> str:
    if not type_args:
        return ""
    return f"<{', '.join(f'{p}={a}' for p, a in zip(type_params, type_args))}>"


class TypeResScope:
    parent: TypeResScope | None
    overrides: TypeResScope | None
    types: dict[TypeId, Type]

    def __init__(self, parent: TypeResScope | None, overrides: TypeResScope | None) -> None:
        self.parent = parent
        self.overrides = overrides
        self.types = {}

    def declare(self, type_param: TypeParam, typ: Type) -> None:
        if type_param.id in self.types:
            raise ValueError(f"Type `{type_param}` already declared in the current type resolution scope")
        self.types[type_param.id] = typ

    def find(self, type_param: TypeParam) -> Type | None:
        res = self.types.get(type_param.id)
        if res:
            return res
        if self.overrides:
            res = self.overrides.find(type_param)
            if res:
                return res
        if self.parent:
            return self.parent.find(type_param)
        return None

    def resolve(self, typ: Type) -> Type:
        match typ:
            case Instance():
                return self.resolve(typ.typ)
            case TypeParam():
                res = typ
                while isinstance(res, TypeParam):
                    res2 = self.find(res)
                    if not res2:
                        return res
                    res = res2
                return res
            case Fn():
                return Fn(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    [Param(p.name, self.resolve(p.typ)) for p in typ.params],
                    self.resolve(typ.result),
                    typ.span,
                )
        return typ

    def keys(self) -> set[TypeId]:
        res = set(self.types.keys())
        if self.parent:
            res.update(self.parent.keys())
        if self.overrides:
            res.update(self.overrides.keys())
        return res

    def flatten(self) -> dict[TypeId, Type]:
        res = {}
        if self.parent:
            res.update(self.parent.flatten())
        if self.overrides:
            res.update(self.overrides.flatten())
        res.update(self.types)
        return res

    def __str__(self) -> str:
        return f"TypeResScope({self.flatten()})"


@dataclass
class Instance:
    typ: TypeParam | Fn
    type_res_scope: TypeResScope

    def __str__(self) -> str:
        return f"Instance{type_args_to_str(self.type_params(), self.type_args())}({self.typ})"

    @property
    def id(self) -> TypeId:
        return self.typ.id

    @property
    def span(self) -> Span:
        return self.typ.span

    def type_params(self) -> TypeParams:
        match self.typ:
            case TypeParam():
                return [self.typ]
            case Fn():
                return self.typ.type_params
            case _:
                raise AssertionError(f"unhandled type: {self.typ}")

    def type_args(self) -> TypeArgs:
        return [self.type_res_scope.resolve(p) for p in self.type_params()]


def full_id(typ: Type) -> str:
    """Return an Id that takes type parameters and type arguments into account."""
    res = [str(typ.id)]
    match typ:
        case Instance():
            res += [full_id(x) for x in typ.type_args()]
    return ":".join(res)


@dataclass
class Builtins:
    @staticmethod
    def new(next_id: Callable[[], int]) -> Builtins:
        span = Span("<built-in>", "", 0, 0)
        str_typ = Str(next_id(), span)
        int_typ = Int(next_id(), bits=64, signed=True, span=span)
        bool_typ = Bool(next_id(), span)
        none_typ = NoneTyp(next_id(), span)
        print_typ = Fn(next_id(), FQN(["print"]), [], [Param("s", str_typ)], none_typ, span)
        int_to_str = Fn(next_id(), FQN(["int_to_str"]), [], [Param("i", int_typ)], str_typ, span)
        bool_to_str = Fn(next_id(), FQN(["bool_to_str"]), [], [Param("b", bool_typ)], str_typ, span)
        return Builtins(str_typ, int_typ, bool_typ, none_typ, print_typ, int_to_str, bool_to_str)

    Str: Str
    Int: Int
    Bool: Bool
    NoneTyp: NoneTyp
    print: Fn
    int_to_str: Fn
    bool_to_str: Fn

    def is_builtin(self, typ: Type) -> bool:
        return typ.id in {
            self.Str.id,
            self.Int.id,
            self.Bool.id,
            self.NoneTyp.id,
            self.print.id,
            self.int_to_str.id,
            self.bool_to_str.id,
        }


Type = Int | Str | Bool | Fn | NoneTyp | TypeParam | Instance | TypeCheckError


def pretty(typ: Type) -> str:
    """Return the type string without any debug information like type-ids."""
    pattern = re.compile(r"\{\d+\}")
    return pattern.sub("", str(typ))


def is_assignable_from(target: Type, from_: Type) -> bool:
    match target:
        case Str():
            return isinstance(from_, Str)
        case Int():
            return isinstance(from_, Int)
        case Bool():
            return isinstance(from_, Bool)
        case TypeCheckError():
            return False
        case TypeParam():
            return target.id == from_.id
        case _:
            raise AssertionError(f"unhandled target type: {target}")


def is_same(target: Type, from_: Type) -> bool:
    return target.id == from_.id
