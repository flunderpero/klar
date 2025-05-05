from __future__ import annotations

import itertools
from dataclasses import dataclass, replace
from functools import wraps
from typing import Any, Callable, cast

from .span import FQN, Span

TypeId = int


def tid(id: TypeId) -> str:
    return f"{{{id}}}"


def nocycle(func: Any) -> Any:  # noqa: ANN401
    @wraps(func)
    def wrapper(self: Any, seen: dict[int, str] | None = None, *args: Any, **kwargs: Any) -> Any:  # noqa: ANN401
        if seen is None:
            seen = {}
        if id(self) in seen:
            return seen[id(self)]
        if hasattr(self, "id"):
            seen[id(self)] = tid(self.id)
        else:
            seen[id(self)] = type(self).__name__
        s = func(self, seen, *args, **kwargs)
        seen[id(self)] = s
        return s

    return wrapper


@dataclass
class TypeCheckError:
    id: TypeId
    message: str
    span: Span

    def debug(self, _seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + f"TypeCheckError({self.message})"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return f"TypeCheckError({self.message})"


@dataclass
class Str:
    id: TypeId
    span: Span

    def debug(self, seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + self.signature(seen)

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return "Str"


@dataclass
class Int:
    id: TypeId
    bits: int
    signed: bool
    span: Span

    def debug(self, seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + self.signature(seen)

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return f"{'I' if self.signed else 'U'}{self.bits}"


@dataclass
class Bool:
    id: TypeId
    span: Span

    def debug(self, seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + self.signature(seen)

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return "Bool"


@dataclass
class NoneTyp:
    id: TypeId
    span: Span

    def debug(self, seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + self.signature(seen)

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return "None"


@dataclass
class FieldOrParam[T: Type = Type]:
    name: str
    typ: T

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        return f"{self.name}: {self.typ.debug(seen)}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        return f"{self.name} {self.typ.signature(seen)}"


@dataclass
class Member:
    id: TypeId
    type_res_scope: TypeResScope | None
    target: Struct | Trait | TypeParam
    field: str
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        return tid(self.id) + f"{self.target.debug(seen)}.{self.field}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        return f"{self.target.signature(seen)}.{self.field}"

    @property
    def type_params(self) -> TypeParams:
        if isinstance(self.target, ParameterizedType):
            return self.target.type_params
        return []

    @property
    def type_args(self) -> TypeArgs:
        if isinstance(self.target, ParameterizedType):
            return self.target.type_args
        return []

    def direct_member(self) -> FieldOrParam:
        if isinstance(self.target, TypeParam):
            assert isinstance(self.target.trait_bound, Trait)
            res = self.target.trait_bound.member(self.field)
        else:
            res = self.target.member(self.field)
        assert res, f"member `{self.field}` not found in {self.target}"
        return res

    def deep_member(self) -> FieldOrParam:
        typ = self
        while True:
            member = typ.direct_member()
            if isinstance(member.typ, Member):
                typ = member.typ
                continue
            return member


@dataclass
class Fn:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    type_res_scope: TypeResScope | None
    params: list[FieldOrParam]
    result: Type
    span: Span
    is_named: bool

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        type_params = type_args_debug(self.type_params, self.type_args, seen)
        params = ", ".join(x.typ.debug(seen) for x in self.params)
        name = f"fn {self.fqn}" if self.is_named else ""
        return tid(self.id) + f"{name}{type_params}({params}) {self.result.debug(seen)}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        type_args = self.type_args
        if self.type_params and self.type_params[0].name == "Self":
            type_args = self.type_args[1:]
        type_args = type_args_signature(type_args, seen)
        params = ", ".join(x.signature(seen) for x in self.params)
        return f"{self.fqn}{type_args}({params}) {self.result.signature(seen)}"

    def is_instance_method(self) -> bool:
        return len(self.params) > 0 and self.params[0].name == "self"

    def params_without_self(self) -> list[FieldOrParam]:
        if self.is_instance_method():
            return self.params[1:]
        return self.params

    def is_same(self, other: Fn) -> bool:
        """Functions are equal if they structurally match, i.e. their parameter and result types match."""
        if len(self.params) != len(other.params):
            return False
        for sp, op in zip(self.params, other.params):
            if not is_same(sp.typ, op.typ):
                return False
        if not type_params_and_args_are_same(self, other):
            return False
        return is_same(self.result, other.result)


@dataclass
class Struct:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    self_typ: TypeParam
    type_res_scope: TypeResScope | None
    fields: list[FieldOrParam]
    methods: list[FieldOrParam[Fn]]
    traits: list[Trait]
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        type_params = type_args_debug(self.type_params, self.type_args, seen)
        fields = ", ".join(f"{x.debug(seen)}" for x in self.fields)
        return tid(self.id) + f"struct {self.fqn}{type_params}{{{fields}}}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        type_args = type_args_signature(self.type_args, seen)
        return f"{self.fqn}{type_args}"

    def member(self, name: str) -> FieldOrParam | None:
        for x in self.fields:
            if x.name == name:
                return x
        for x in self.methods:
            if x.name == name:
                return x  # pyright:ignore[reportReturnType]
        return None

    def field_index(self, name: str) -> int | None:
        for i, x in enumerate(self.fields):
            if x.name == name:
                return i
        return None

    def trait(self, fqn: FQN) -> Trait | None:
        for x in self.traits:
            if x.fqn == fqn:
                return x
        return None


@dataclass
class Trait:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    self_typ: TypeParam
    type_res_scope: TypeResScope | None
    methods: list[FieldOrParam[Fn]]
    default_impls: list[FieldOrParam[Fn]]
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        methods = "\n".join("    " + x.debug(seen) for x in self.methods)
        type_params = type_args_debug(self.type_params, self.type_args, seen)
        return tid(self.id) + f"trait {self.fqn}{type_params}{{\n{methods}\n}}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        type_args = type_args_signature(self.type_args, seen)
        return f"{self.fqn}{type_args}"

    def member(self, name: str) -> FieldOrParam | None:
        for x in self.methods:
            if x.name == name:
                return x  # pyright:ignore[reportReturnType]
        return None

    def default_impl(self, name: str) -> FieldOrParam[Fn] | None:
        for x in self.default_impls:
            if x.name == name:
                return x
        return None


@dataclass
class TypeParam:
    id: TypeId
    name: str
    trait_bound: Trait | None
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        tb = " " + self.trait_bound.debug(seen) if self.trait_bound else ""
        return tid(self.id) + self.name + tb

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, seen: dict[int, str] | None = None) -> str:
        tb = " " + self.trait_bound.signature(seen) if self.trait_bound and self.name != "Self" else ""
        return self.name + tb

    @property
    def fqn(self) -> FQN:
        return FQN([self.name])

    @property
    def type_params(self) -> TypeParams:
        if isinstance(self.trait_bound, ParameterizedType):
            return self.trait_bound.type_params
        return []

    @property
    def type_args(self) -> TypeArgs:
        if isinstance(self.trait_bound, ParameterizedType):
            return self.trait_bound.type_args
        return []

    @property
    def type_res_scope(self) -> TypeResScope | None:
        if isinstance(self.trait_bound, ParameterizedType):
            return self.trait_bound.type_res_scope
        return None

    @type_res_scope.setter
    def type_res_scope(self, value: TypeResScope | None) -> None:
        if isinstance(self.trait_bound, ParameterizedType):
            self.trait_bound.type_res_scope = value

    def member(self, name: str) -> FieldOrParam | None:
        if self.trait_bound:
            return self.trait_bound.member(name)
        return None


TypeParams = list[TypeParam]
TypeArgs = list["Type"]


def type_args_debug(type_params: TypeParams, type_args: TypeArgs, seen: dict[int, str] | None) -> str:
    if not type_args:
        return ""
    s = ", ".join(f"{p.debug(seen)}={(a or p).debug(seen)}" for p, a in itertools.zip_longest(type_params, type_args))
    return f"<{s}>"


def type_args_signature(type_args: TypeArgs, seen: dict[int, str] | None) -> str:
    if not type_args:
        return ""
    s = ", ".join(x.signature(seen) for x in type_args)
    return f"<{s}>"


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

    def resolve(self, typ: Type, seen: dict[str, Type] | None = None) -> Type:
        """Resolve the type based on all the declared types."""
        if seen is not None:
            seen_typ = seen.get(full_id(typ))
            if seen_typ is not None:
                return seen_typ

        match typ:
            case TypeParam():
                res = typ
                while isinstance(res, TypeParam):
                    typ2 = self.find(res)
                    if not typ2 or res.id == typ2.id:
                        break
                    res = typ2
                typ = res
                if isinstance(typ, TypeParam) and typ.trait_bound:
                    scope = self
                    if typ.type_res_scope != self:
                        scope = TypeResScope(typ.type_res_scope, self)
                    if seen is None:
                        seen = {}
                    tb = typ.trait_bound
                    typ = TypeParam(
                        typ.id,
                        typ.name,
                        tb,
                        typ.span,
                    )
                    seen[full_id(typ)] = typ
                    typ.trait_bound = cast(Trait, scope.resolve(tb, seen))
            case Member():
                scope = self
                if typ.type_res_scope != self:
                    scope = TypeResScope(typ.type_res_scope, self)
                target = scope.resolve(typ.target)
                assert isinstance(target, Trait | Struct | TypeParam), (
                    f"expected Trait, Struct, or TypeParam, got {target}"
                )
                if isinstance(target, TypeParam):
                    target = target.trait_bound
                    assert target is not None
                field = target.member(typ.field)
                assert field, f"member `{typ.field}` not found in {target}"
                typ = scope.resolve(field.typ)
            case Fn():
                scope = self
                if typ.type_res_scope != self:
                    scope = TypeResScope(typ.type_res_scope, self)
                typ = Fn(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    [scope.resolve(x, seen) for x in typ.type_args],
                    None,
                    typ.params,
                    typ.result,
                    typ.span,
                    typ.is_named,
                )
                if seen is None:
                    seen = {}
                seen[full_id(typ)] = typ
                typ.params = [FieldOrParam(x.name, scope.resolve(x.typ, seen)) for x in typ.params]
                typ.result = scope.resolve(typ.result, seen)
                return typ
            case Struct():
                scope = self
                if typ.type_res_scope != self:
                    # todo: We reversed the order of scopes here compared to Fn or Trait.
                    #       This is needed so recursive types work.
                    #       But it feels weird and surely hides a bug.
                    scope = TypeResScope(self, typ.type_res_scope)
                typ = Struct(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    [scope.resolve(x, seen) for x in typ.type_args],
                    typ.self_typ,
                    None,
                    typ.fields,
                    typ.methods,
                    typ.traits,
                    typ.span,
                )
                if seen is None:
                    seen = {}
                seen[full_id(typ)] = typ
                typ.fields = [FieldOrParam(x.name, scope.resolve(x.typ, seen)) for x in typ.fields]
                typ.methods = [FieldOrParam[Fn](x.name, cast(Fn, scope.resolve(x.typ, seen))) for x in typ.methods]
                for i, trait in enumerate(typ.traits):
                    scope = TypeResScope(scope, None)
                    scope.declare(trait.self_typ, typ)
                    typ.traits[i] = cast("Trait", scope.resolve(trait, seen))
                return typ
            case Trait():
                scope = self
                if typ.type_res_scope != self:
                    scope = TypeResScope(typ.type_res_scope, self)
                typ = Trait(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    [scope.resolve(x, seen) for x in typ.type_args],
                    typ.self_typ,
                    None,
                    typ.methods,
                    typ.default_impls,
                    typ.span,
                )
                if seen is None:
                    seen = {}
                seen[full_id(typ)] = typ
                typ.methods = [FieldOrParam[Fn](x.name, cast(Fn, scope.resolve(x.typ, seen))) for x in typ.methods]
                typ.default_impls = [
                    FieldOrParam[Fn](x.name, cast(Fn, scope.resolve(x.typ, seen))) for x in typ.default_impls
                ]
                return typ
        if seen is not None:
            seen[full_id(typ)] = typ
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

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        s = []
        s.append(f"    self: {id(self)}")
        if self.parent:
            s.append(f"    self.parent: {id(self.parent)}")
        if self.overrides:
            s.append(f"    self.overrides: {id(self.overrides)}")
        for k, v in self.flatten().items():
            s.append(f"   {k}={v.debug(seen).replace('\n', '\n    ')}")
        return f"TypeResScope(\n{'\n'.join(s)}\n)"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    def signature(self, _seen: dict[int, str] | None = None) -> str:
        return f"TypeResScope({len(self.flatten())})"


def resolve[T: Type](typ: T) -> T:
    if isinstance(typ, ParameterizedType) and typ.type_res_scope is not None:
        return cast("T", typ.type_res_scope.resolve(typ))
    return typ


def full_id(typ: Type) -> str:
    """Return an Id that takes type parameters and type arguments into account."""
    res = [str(typ.id)]
    if isinstance(typ, ParameterizedType):
        res += [full_id(x) for x in typ.type_args]
    return ":".join(res)


def instance[T: ParameterizedType](typ: T, type_res_scope: TypeResScope | None) -> T:
    clone = replace(typ)
    if typ.type_res_scope is not None:
        type_res_scope = TypeResScope(typ.type_res_scope, type_res_scope)
    clone.type_res_scope = type_res_scope
    return clone


def infer_type_args_from_call_args(callee: CallableType, call_args: list[Type]) -> None:
    """Declare all type parameters with their type if we find a match in `call_args`.

    `typ.type_res_scope` is modified.
    """

    def infer(param_: TypeParam, arg: Type, type_res_scope: TypeResScope) -> TypeResScope:
        param = type_res_scope.resolve(param_)
        if not isinstance(param, TypeParam):
            # Already resolved.
            return type_res_scope
        type_res_scope = TypeResScope(type_res_scope, None)
        arg = type_res_scope.resolve(arg)
        if param.id not in type_res_scope.types:
            type_res_scope.declare(param, arg)
        tb = param.trait_bound
        if not tb:
            return type_res_scope
        assert isinstance(arg, ParameterizedType)
        assert len(tb.type_params) == len(arg.type_args), f"type params and args mismatch: {tb} {arg}"
        for tp, ta in zip(tb.type_params, tb.type_args):
            if tp.id not in type_res_scope.types:
                type_res_scope.declare(tp, ta)
        for tp, ta in zip(tb.type_params, arg.type_args):
            if isinstance(tp, TypeParam):
                type_res_scope = TypeResScope(infer(tp, ta, type_res_scope), None)
        return type_res_scope

    params: list[Type]
    match callee:
        case Fn():
            params = [x.typ for x in callee.params]
        case Struct():
            params = [x.typ for x in callee.fields]
        case _:
            raise AssertionError(f"unhandled type: {callee}")
    assert len(params) == len(call_args), f"expected {len(params)} call args, got {len(call_args)}"
    if callee.type_res_scope is None:
        callee.type_res_scope = TypeResScope(None, None)
    for arg, param in zip(call_args, params):
        if isinstance(param, TypeParam):
            type_res_scope = infer(param, arg, callee.type_res_scope)
            for tp in callee.type_params:
                ta = type_res_scope.find(tp)
                if ta and tp.id not in callee.type_res_scope.types:
                    callee.type_res_scope.declare(tp, ta)


built_in_span = Span("<built-in>", "", 0, 0)


@dataclass
class Builtins:
    @staticmethod
    def new(next_id: Callable[[], int]) -> Builtins:
        span = Span("<built-in>", "", 0, 0)
        str_typ = Str(next_id(), span)
        int_typ = Int(next_id(), bits=64, signed=True, span=span)
        bool_typ = Bool(next_id(), span)
        none_typ = NoneTyp(next_id(), span)
        args = {"span": span, "is_named": True}
        print_typ = Fn(next_id(), FQN(["print"]), [], [], None, [FieldOrParam("s", str_typ)], none_typ, **args)
        int_to_str = Fn(next_id(), FQN(["int_to_str"]), [], [], None, [FieldOrParam("i", int_typ)], str_typ, **args)
        bool_to_str = Fn(next_id(), FQN(["bool_to_str"]), [], [], None, [FieldOrParam("b", bool_typ)], str_typ, **args)
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


Type = Int | Str | Bool | Fn | Struct | Trait | Member | NoneTyp | TypeParam | TypeCheckError
ParameterizedType = Fn | Struct | Trait | Member | TypeParam
ImplementableType = Struct | Trait
CallableType = Fn | Struct


def is_assignable_from(target: Type, from_: Type) -> bool:
    if isinstance(target, Member):
        target = target.deep_member().typ
    if isinstance(from_, Member):
        from_ = from_.deep_member().typ
    match target:
        case Str():
            return isinstance(from_, Str)
        case Int():
            return isinstance(from_, Int)
        case Bool():
            return isinstance(from_, Bool)
        case NoneTyp():
            return isinstance(from_, NoneTyp)
        case TypeCheckError():
            return False
        case TypeParam():
            if target.id == from_.id:
                return True
            if not target.trait_bound:
                return False
            return is_assignable_from(target.trait_bound, from_)
        case Struct():
            return target.id == from_.id
        case Trait():
            if isinstance(from_, Trait):
                return target.id == from_.id
            if isinstance(from_, Struct):
                return any(x.id == target.id for x in from_.traits)
            return False
        case Fn():
            if not isinstance(from_, Fn):
                return False
            # Two functions are equal if their parameter and result types match.
            if len(target.params) != len(from_.params):
                return False
            for sp, op in zip(target.params, from_.params):
                if not is_assignable_from(sp.typ, op.typ):
                    return False
            return is_assignable_from(target.result, from_.result)
        case _:
            raise AssertionError(f"unhandled target type: {target}")


def type_params_and_args_are_same(target: ParameterizedType, from_: ParameterizedType) -> bool:
    if len(target.type_params) != len(from_.type_params):
        return False
    if not all(is_same(t, f) for t, f in zip(target.type_params, from_.type_params)):
        return False
    return all(is_same(t, f) for t, f in zip(target.type_args, from_.type_args))


def is_same(target: Type, from_: Type) -> bool:
    if isinstance(target, Member):
        target = target.deep_member().typ
    if isinstance(from_, Member):
        from_ = from_.deep_member().typ
    if isinstance(target, Fn) and isinstance(from_, Fn):
        return target.is_same(from_)
    if target.id != from_.id:
        return False
    if isinstance(target, ParameterizedType) and isinstance(from_, ParameterizedType):
        return type_params_and_args_are_same(target, from_)
    return True


def normalize_type(next_id: Callable[[], int], typ: Type) -> Type:
    """Return a normalized version of the type.

    The returned type is used for merging values of different origins
    (e.g., different branches of an if-expression).

    - For most types, the type is returned unchanged.
    - For function types (Fn):
      - If the function is named, a new anonymous copy is created
        with the same parameter and result types.
      - If the function is already anonymous, it is returned as-is.
    - For member types (MemberType):
      - The deep member is returned.
    - For type check errors, the error is returned unchanged.

    """
    match typ:
        case Member():
            return typ.deep_member().typ
        case Fn():
            if not typ.is_named:
                return typ
            return Fn(
                next_id(),
                FQN([]),
                typ.type_params,
                typ.type_args,
                typ.type_res_scope,
                list(typ.params),
                typ.result,
                typ.span,
                is_named=False,
            )
    return typ
