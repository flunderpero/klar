from __future__ import annotations

from dataclasses import dataclass, fields, replace
from functools import wraps
from typing import Any, Callable, cast

from .span import FQN, Span

TypeId = int


def tid(id: TypeId) -> str:
    return f"{{{id}}}"


def nocycle(func: Any) -> Any:  # noqa: ANN401
    @wraps(func)
    def wrapper(self: Any, seen: dict[str, str] | None = None, *args: Any, **kwargs: Any) -> Any:  # noqa: ANN401
        if seen is None:
            seen = {}
        i = type(self).__name__
        if isinstance(self, Type):
            i = full_id(self)
        if not isinstance(self, Field) and i in seen:
            return seen[i]
        seen[i] = tid(self.id) if isinstance(self, Type) else i
        s = func(self, seen, *args, **kwargs)
        seen[i] = s
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
class TypeParam:
    id: TypeId
    name: str
    bound: Trait | Struct | None
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        tb = " " + self.bound.debug(seen) if self.bound else ""
        return tid(self.id) + self.name + tb

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        tb = " " + self.bound.signature(seen) if self.bound and self.name != "Self" else ""
        return self.name + tb

    @property
    def fqn(self) -> FQN:
        return FQN([self.name])

    @property
    def type_params(self) -> TypeParams:
        if self.bound is not None:
            return self.bound.type_params
        return []

    @property
    def type_args(self) -> TypeArgs:
        if self.bound is not None:
            return self.bound.type_args
        return []

    @property
    def type_map(self) -> TypeMap:
        return self.bound.type_map if self.bound else TypeMap({}, None)


def type_params_debug(type_params: TypeParams, type_args: TypeArgs, seen: dict[int, str] | None) -> str:
    if not type_params:
        return ""
    params = []
    for p, a in zip(type_params, type_args):
        ps = p.debug(seen)
        if a != p:
            ps = f"{ps} {a.debug(seen)}"
        params.append(ps)
    s = ", ".join(params)
    return f"<{s}>"


def type_args_signature(type_args: TypeArgs, seen: dict[int, str] | None) -> str:
    if not type_args:
        return ""
    params = [x.signature(seen) for x in type_args]
    s = ", ".join(params)
    return f"<{s}>"


@dataclass
class Field[T: Type]:
    name: str
    typ: T

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        return f"{self.name}={self.typ.debug(seen)}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        return f"{self.name} {self.typ.signature(seen)}"


@dataclass
class Fn:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    type_map: TypeMap
    params: list[Field]
    result: Type
    span: Span
    is_named: bool

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        type_params = type_params_debug(self.type_params, self.type_args, seen)
        if self.is_named:
            params = ", ".join(x.debug(seen) for x in self.params)
            name = f"fn {self.fqn}"
        else:
            params = ", ".join(f"{x.typ.debug(seen)}" for x in self.params)
            name = "fn"
        return tid(self.id) + f"{name}{type_params}({params}) {self.result.debug(seen)}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        type_args = type_args_signature(self.type_args, seen)
        if self.is_named:
            params = ", ".join(x.signature(seen) for x in self.params)
            name = f"fn {self.fqn}"
        else:
            params = ", ".join(f"{x.typ.signature(seen)}" for x in self.params)
            name = "fn"
        return f"{name}{type_args}({params}) {self.result.signature(seen)}"

    def is_instance_method(self) -> bool:
        return len(self.params) > 0 and self.params[0].name == "self"

    def params_without_self(self) -> list[Field]:
        if self.is_instance_method():
            return self.params[1:]
        return self.params


@dataclass
class Struct:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    type_map: TypeMap
    self_typ: TypeParam
    fields: list[Field]
    methods: list[Field[Fn]]
    traits: list[Trait]
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        type_params = type_params_debug(self.type_params, self.type_args, seen)
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

    def field_index(self, name: str) -> int | None:
        for i, x in enumerate(self.fields):
            if x.name == name:
                return i
        return None

    def method(self, name: str) -> Fn | None:
        for method in self.methods:
            if method.name == name:
                return method.typ
        return None


@dataclass
class Trait:
    id: TypeId
    fqn: FQN
    type_params: TypeParams
    type_args: TypeArgs
    type_map: TypeMap
    self_typ: TypeParam
    methods: list[Field[Fn]]
    default_impls: list[Field[Fn]]
    span: Span

    @nocycle
    def debug(self, seen: dict[int, str] | None = None) -> str:
        methods = "\n".join("    " + x.debug(seen) for x in self.methods)
        type_params = type_params_debug(self.type_params, self.type_args, seen)
        return tid(self.id) + f"trait {self.fqn}{type_params}{{\n{methods}\n}}"

    def __repr__(self) -> str:
        return self.debug()

    def __str__(self) -> str:
        return self.signature()

    @nocycle
    def signature(self, seen: dict[int, str] | None = None) -> str:
        type_args = type_args_signature(self.type_args, seen)
        return f"{self.fqn}{type_args}"

    def method(self, name: str) -> Fn | None:
        for method in self.methods:
            if method.name == name:
                return method.typ
        return None


Type = Str | Int | Bool | NoneTyp | Fn | Struct | Trait | TypeParam | TypeCheckError
ParameterizedType = Fn | Struct | Trait | TypeParam
CallableType = Fn | Struct
TypeParams = list[TypeParam]
TypeArgs = list[Type]


def resolve[T: Type](typ: T) -> T:
    if isinstance(typ, ParameterizedType):
        return cast(T, typ.type_map.resolve(typ))
    return typ


@dataclass
class Builtins:
    @staticmethod
    def new(next_id: Callable[[], int]) -> Builtins:
        span = Span("<built-in>", "", 0, 0)
        type_map = TypeMap({}, None)
        str_typ = Str(next_id(), span)
        int_typ = Int(next_id(), bits=64, signed=True, span=span)
        bool_typ = Bool(next_id(), span)
        none_typ = NoneTyp(next_id(), span)
        args = {"span": span, "is_named": True}
        print_typ = Fn(next_id(), FQN(["print"]), [], [], type_map, [Field("s", str_typ)], none_typ, **args)
        int_to_str = Fn(next_id(), FQN(["int_to_str"]), [], [], type_map, [Field("i", int_typ)], str_typ, **args)
        bool_to_str = Fn(next_id(), FQN(["bool_to_str"]), [], [], type_map, [Field("b", bool_typ)], str_typ, **args)
        return Builtins(
            Str=str_typ,
            Int=int_typ,
            Bool=bool_typ,
            NoneTyp=none_typ,
            print=print_typ,
            int_to_str=int_to_str,
            bool_to_str=bool_to_str,
        )

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

    def builtins(self) -> dict[str, Type]:
        res = {}
        for x in fields(self):
            if x.name == "NoneTyp":
                res["None"] = getattr(self, x.name)
                continue
            res[x.name] = getattr(self, x.name)
        return res


@dataclass
class TypeMap:
    types: dict[TypeId, Type]
    parent: TypeMap | None

    def resolve(self, typ: Type, seen: dict[str, Type] | None = None) -> Type:
        """Recursively resolve a type (either a TypeParam or a Type that contains TypeParams) to its concrete type."""
        if seen is None:
            seen = {}
        typ_id = full_id(typ)
        if typ_id in seen:
            return seen[typ_id]
        match typ:
            case TypeParam():
                # Resolve the type parameter until it is resolved to a concrete type or to itself
                # or if resolution is cyclic.
                stack = [typ.id]
                while True:
                    resolved = self.find(typ)
                    if resolved is None:
                        # Type parameter not found.
                        break
                    if id(resolved) == id(typ):
                        # Type parameter is resolved to itself.
                        break
                    if not isinstance(resolved, TypeParam):
                        # Type parameter is resolved to a concrete type.
                        typ = resolved
                        break
                    if resolved.id in stack:
                        # Cyclic type parameter resolution: break the cycle and return the previous type.
                        break
                    typ = resolved
                    stack.append(typ.id)
                # if not isinstance(typ, TypeParam):
                #     typ = self.resolve(typ, seen)
                if isinstance(typ, TypeParam) and typ.bound is not None:
                    typ = replace(typ, bound=self.resolve(typ.bound, seen))

                seen[full_id(typ)] = typ
                return typ
            case Fn():
                type_map = self
                if id(self) != id(typ.type_map):
                    type_map = self.flatten(typ.type_map)
                fn = Fn(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    typ.type_args,
                    type_map,
                    typ.params,
                    typ.result,
                    typ.span,
                    typ.is_named,
                )
                fn.type_args = [type_map.resolve(x, seen) for x in fn.type_args]
                seen[full_id(fn)] = fn
                fn.params = [Field(x.name, type_map.resolve(x.typ, seen)) for x in fn.params]
                fn.result = type_map.resolve(fn.result, seen)
                return fn
            case Struct():
                type_map = self
                if id(self) != id(typ.type_map):
                    type_map = self.flatten(typ.type_map)
                struct = Struct(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    typ.type_args,
                    type_map,
                    typ.self_typ,
                    typ.fields,
                    typ.methods,
                    typ.traits,
                    typ.span,
                )
                struct.type_args = [type_map.resolve(x, seen) for x in struct.type_args]
                seen[full_id(struct)] = struct
                struct.fields = [Field(x.name, type_map.resolve(x.typ, seen)) for x in struct.fields]
                struct.methods = [Field(x.name, cast(Fn, type_map.resolve(x.typ, seen))) for x in struct.methods]
                struct.traits = [cast(Trait, type_map.resolve(x, seen)) for x in struct.traits]
                return struct
            case Trait():
                type_map = self
                if id(self) != id(typ.type_map):
                    type_map = self.flatten(typ.type_map)
                trait = Trait(
                    typ.id,
                    typ.fqn,
                    typ.type_params,
                    typ.type_args,
                    type_map,
                    typ.self_typ,
                    typ.methods,
                    typ.default_impls,
                    typ.span,
                )
                trait.type_args = [type_map.resolve(x, seen) for x in trait.type_args]
                seen[full_id(trait)] = trait
                trait.methods = [Field(x.name, cast(Fn, type_map.resolve(x.typ, seen))) for x in trait.methods]
                trait.default_impls = [
                    Field(x.name, cast(Fn, type_map.resolve(x.typ, seen))) for x in trait.default_impls
                ]
                return trait
        return typ

    def find(self, type_param: TypeParam) -> Type | None:
        """Look up the type parameter in the TypeMap hierarchy."""
        type_map = self
        while True:
            typ = type_map.types.get(type_param.id)
            if typ is not None:
                return typ
            if type_map.parent is None:
                return None
            type_map = type_map.parent

    def bind(self, type_param: TypeParam, type_arg: Type) -> None:
        assert isinstance(type_param, TypeParam)
        assert type_param.id not in self.types, f"{type_param} already bound to {self.types[type_param.id]}"
        self.types[type_param.id] = type_arg

    def is_empty(self) -> bool:
        return not self.types and not self.parent

    def debug(self) -> str:
        lines = []
        if self.types:
            lines.append("")
        for id_, typ in self.types.items():
            lines.append(f"  {tid(id_)}: {typ.debug()}")
        if self.types:
            lines.append("")
        if self.parent and not self.parent.is_empty():
            lines.append(f"    {self.parent.debug().replace('\n', '\n    ')}")
        entries = "\n".join(lines)
        return f"TypeMap({entries})"

    def flatten(self, new_parent: TypeMap | None = None) -> TypeMap:
        """Collapse the TypeMap hierarchy into a new TypeMap with the given new parent."""
        res = TypeMap({}, new_parent)
        parents = []
        p = self
        while p.parent:
            parents.append(p.parent)
            p = p.parent
        for parent in reversed(parents):
            res.types.update(parent.types)
        res.types.update(self.types)
        return res


def full_id(typ: Type) -> str:
    """Return an id that takes all resolved type arguments into account."""
    res = [str(typ.id)]
    if isinstance(typ, ParameterizedType):
        res.extend(full_id(x) for x in typ.type_args)
    return "$".join(res)


def is_assignable_from(target: Type, from_: Type) -> bool:
    match target:
        case Str() | Int() | Bool() | NoneTyp():
            return isinstance(from_, type(target))
        case TypeCheckError():
            return False
        case TypeParam():
            if not target.bound:
                # A type parameter without a bound is assignable to any type.
                return True
            return is_assignable_from(target.bound, from_)
        case Struct():
            if not isinstance(from_, Struct):
                return False
            return target.id == from_.id and all(
                is_assignable_from(x, y) for x, y in zip(target.type_args, from_.type_args)
            )
        case Trait():
            if isinstance(from_, TypeParam) and from_.bound is not None:
                return is_assignable_from(target, from_.bound)
            if isinstance(from_, Trait):
                return target.id == from_.id
            if isinstance(from_, Struct):
                return any(x.id == target.id for x in from_.traits)
            return False
        case Fn():
            if not isinstance(from_, Fn):
                return False
            if len(target.params) != len(from_.params):
                return False
            # Two functions are equal if their parameters, and result types match.
            for target_param, from_param in zip(target.params, from_.params):
                if not is_assignable_from(target_param.typ, from_param.typ):
                    return False
            return is_assignable_from(target.result, from_.result)
        case _:
            raise AssertionError(f"unhandled target type: {target}")


def type_args_are_same(a: list[Type], b: list[Type]) -> bool:
    if len(a) != len(b):
        return False
    return all(is_same(x, y) for x, y in zip(a, b))


def is_same(a: Type, b: Type) -> bool:
    match a:
        case Str() | Int() | Bool() | NoneTyp():
            return isinstance(a, type(b))
        case TypeCheckError():
            return False
        case TypeParam():
            if a.id == b.id:
                return True
            if not isinstance(b, TypeParam):
                return False
            if a.bound is not None:
                if b.bound is None:
                    return False
                return is_same(a.bound, b.bound)
            return b.bound is None
        case Struct() | Trait():
            if a.id != b.id:
                return False
            assert isinstance(b, (Struct, Trait))
            return type_args_are_same(a.type_args, b.type_args)
        case Fn():
            if not isinstance(b, Fn):
                return False
            if not type_args_are_same(a.type_args, b.type_args):
                return False
            an = normalize_type(a)
            bn = normalize_type(b)
            return all(is_same(x.typ, y.typ) for x, y in zip(an.params, bn.params)) and is_same(an.result, bn.result)
        case _:
            raise AssertionError(f"unhandled type: {a}")


def infer_type_arguments_from_call_args(callee: CallableType, call_args: list[Type]) -> TypeMap:
    """Infer type arguments for a function call based on the types of the arguments."""
    type_map = TypeMap({}, callee.type_map)
    type_params = {x.id for x in callee.type_params}

    def infer(param: Type, arg: Type) -> None:
        param = resolve(param)
        if isinstance(param, TypeParam):
            if param.id in type_params and param.id not in type_map.types:
                type_map.bind(param, arg)
            if param.bound is not None:
                param = param.bound
        if isinstance(param, ParameterizedType) and isinstance(arg, ParameterizedType):
            for p, a in zip(param.type_args, arg.type_args):
                infer(p, a)
        if isinstance(param, Fn) and isinstance(arg, Fn):
            infer(param.result, arg.result)

    callee = resolve(callee)
    match callee:
        case Fn():
            params = callee.params_without_self()
        case Struct():
            params = callee.fields
        case _:
            raise AssertionError(f"Unsupported callee type: {callee}")
    for param, arg in zip(params, call_args):
        infer(param.typ, arg)
    return type_map


def normalize_type[T: Type](typ: T) -> T:
    """Return a normalized version of the type.

    The returned type is used for merging values of different origins
    (e.g., different branches of an if-expression).

    - For most types, the type is returned unchanged.
    - For function types (Fn):
      - If the function is named, a new anonymous copy is created
        with the same id, parameters, and result types but without
        type parameters/arguments.
      - If the function is already anonymous, it is returned as-is.
    - For type check errors, the error is returned unchanged.

    """
    match typ:
        case Fn():
            if not typ.is_named:
                return typ
            return cast(
                T,
                Fn(
                    typ.id,
                    FQN([]),
                    [],
                    [],
                    TypeMap({}, None),
                    typ.params,
                    typ.result,
                    typ.span,
                    is_named=False,
                ),
            )
    return typ
