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
class FieldOrParam:
    name: str
    typ: Type


@dataclass
class Fn:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    params: list[FieldOrParam]
    result: Type
    span: Span

    def __str__(self) -> str:
        type_params = type_params_to_str(self.type_params)
        params = ", ".join(f"{p.name}: {p.typ}" for p in self.params)
        return tid(self.id) + f"fn {self.fqn}{type_params}({params})->{self.result}"

    def is_instance_method(self) -> bool:
        return len(self.params) > 0 and self.params[0].name == "self"

    def params_without_self(self) -> list[FieldOrParam]:
        if self.is_instance_method():
            return self.params[1:]
        return self.params


@dataclass
class Struct:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    fields: list[FieldOrParam]
    methods: list[FieldOrParam]
    span: Span

    def __str__(self) -> str:
        type_params = type_params_to_str(self.type_params)
        fields = ", ".join(f"{f.name}: {f.typ}" for f in self.fields)
        return tid(self.id) + f"struct {self.fqn}{type_params}{{{fields}}}"

    def field_or_method(self, name: str) -> FieldOrParam | None:
        for x in self.fields:
            if x.name == name:
                return x
        for x in self.methods:
            if x.name == name:
                return x
        return None

    def field_index(self, name: str) -> int | None:
        for i, x in enumerate(self.fields):
            if x.name == name:
                return i
        return None


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

    def resolve(self, typ: Type, seen: dict[TypeId, Type] | None = None) -> Type:
        if seen is not None:
            seen_typ = seen.get(typ.id)
            if seen_typ is not None:
                return seen_typ

        def resolve_field_or_param(typ: Type) -> Type:
            typ = self.resolve(typ, seen)
            if isinstance(typ, (Fn, Struct)):
                typ = Instance(typ, self)
            return typ

        match typ:
            case Instance():
                typ = typ.resolve()
                typ = self.resolve(typ)
            case TypeParam():
                while isinstance(typ, TypeParam):
                    typ2 = self.find(typ)
                    if not typ2:
                        break
                    typ = typ2
            case Fn():
                typ = Fn(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    list(typ.params),
                    typ.result,
                    typ.span,
                )
                if seen is None:
                    seen = {}
                seen[typ.id] = typ
                typ.params = [FieldOrParam(x.name, resolve_field_or_param(x.typ)) for x in typ.params]
                typ.result = resolve_field_or_param(typ.result)
            case Struct():
                typ = Struct(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    list(typ.fields),
                    list(typ.methods),
                    typ.span,
                )
                if seen is None:
                    seen = {}
                seen[typ.id] = typ
                typ.fields = [FieldOrParam(x.name, self.resolve(x.typ, seen)) for x in typ.fields]
                typ.methods = [FieldOrParam(x.name, self.resolve(x.typ, seen)) for x in typ.methods]
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
    typ: TypeParam | Fn | Struct
    type_res_scope: TypeResScope

    def __str__(self) -> str:
        return f"Instance{type_args_to_str(self.type_params(), self.type_args())}({self.typ})"

    @property
    def id(self) -> TypeId:
        return self.typ.id

    @property
    def span(self) -> Span:
        return self.typ.span

    def resolve(self) -> Type:
        return self.type_res_scope.resolve(self.typ)

    def type_params(self) -> TypeParams:
        match self.typ:
            case TypeParam():
                return [self.typ]
            case Fn() | Struct():
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
        print_typ = Fn(next_id(), FQN(["print"]), [], [FieldOrParam("s", str_typ)], none_typ, span)
        int_to_str = Fn(next_id(), FQN(["int_to_str"]), [], [FieldOrParam("i", int_typ)], str_typ, span)
        bool_to_str = Fn(next_id(), FQN(["bool_to_str"]), [], [FieldOrParam("b", bool_typ)], str_typ, span)
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


Type = Int | Str | Bool | Fn | Struct | NoneTyp | TypeParam | Instance | TypeCheckError


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
        case Struct():
            return target.id == from_.id
        case Instance():
            # todo: This does not include type arguments
            if target.id != from_.id:
                return False
            target_args = target.type_args()
            if not isinstance(from_, Instance):
                if len(target_args) == 0:
                    return target.id == from_.id
                return False
            from_args = from_.type_args()
            return all(
                isinstance(x, TypeParam) or is_assignable_from(x, from_args[i]) for i, x in enumerate(target_args)
            )
        case _:
            raise AssertionError(f"unhandled target type: {target}")


def is_same(target: Type, from_: Type) -> bool:
    return target.id == from_.id
