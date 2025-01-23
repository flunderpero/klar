from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable

from .span import Span

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
    span: Span
    params: list[Param]
    result: Type

    def __str__(self) -> str:
        params = ", ".join(f"{p.name}: {p.typ}" for p in self.params)
        return tid(self.id) + f"Fn({params}) -> {self.result}"


@dataclass
class Builtins:
    @staticmethod
    def new(next_id: Callable[[], int]) -> Builtins:
        span = Span("<built-in>", "", 0, 0)
        str_typ = Str(next_id(), span)
        int_typ = Int(next_id(), bits=64, signed=True, span=span)
        bool_typ = Bool(next_id(), span)
        none_typ = NoneTyp(next_id(), span)
        print_typ = Fn(next_id(), span, [Param("s", str_typ)], none_typ)
        int_to_str = Fn(next_id(), span, [Param("i", int_typ)], str_typ)
        bool_to_str = Fn(next_id(), span, [Param("b", bool_typ)], str_typ)
        return Builtins(str_typ, int_typ, bool_typ, none_typ, print_typ, int_to_str, bool_to_str)

    Str: Str
    Int: Int
    Bool: Bool
    NoneTyp: NoneTyp
    print: Fn
    int_to_str: Fn
    bool_to_str: Fn


Type = Int | Str | Bool | Fn | NoneTyp | TypeCheckError


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
        case _:
            raise AssertionError(f"unhandled target type: {target}")


def is_same(target: Type, from_: Type) -> bool:
    return target.id == from_.id
