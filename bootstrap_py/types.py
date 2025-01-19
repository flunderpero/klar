from __future__ import annotations

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
        none_typ = NoneTyp(next_id(), span)
        print_typ = Fn(next_id(), span, [Param("s", str_typ)], none_typ)
        return Builtins(str_typ, none_typ, print_typ)

    Str: Str
    NoneTyp: NoneTyp
    print: Fn


Type = Str | Fn | NoneTyp | TypeCheckError


def is_assignable_from(target: Type, from_: Type) -> bool:
    match target:
        case Str():
            return isinstance(from_, Str)
        case _:
            raise AssertionError(f"unhandled target type: {target}")
