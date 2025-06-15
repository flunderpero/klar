from __future__ import annotations

from dataclasses import replace

from . import types
from .span import FQN, Span

span = Span("<built-in>", "", 0, 0)


type_int = types.Int(42, 16, False, span)  # noqa: FBT003
type_param = types.TypeParam(43, "A", None, span)


def test_full_id_primitive_types() -> None:
    assert types.full_id(type_int) == "42"
    assert types.full_id(type_param) == "43"


def test_full_id_parameterized_unresolved() -> None:
    struct = types.Struct(
        44, FQN(["Foo"]), [type_param], [type_param], types.TypeMap({}, None), type_param, [], [], [], span
    )
    assert struct.signature() == "Foo<A>"
    assert types.full_id(struct) == "44$43"


def test_full_id_parameterized_resolved() -> None:
    struct = types.Struct(
        44, FQN(["Foo"]), [type_param], [type_int], types.TypeMap({}, None), type_param, [], [], [], span
    )
    assert struct.signature() == "Foo<U16>"
    assert types.full_id(struct) == "44$42"


def test_full_id_parameterized_recursive() -> None:
    struct = types.Struct(
        44, FQN(["Foo"]), [type_param], [type_param], types.TypeMap({}, None), type_param, [], [], [], span
    )
    struct_param = replace(struct, type_map=types.TypeMap({}, None))
    struct_param.type_map.bind(type_param, type_int)
    struct_param = types.resolve(struct_param)
    struct.type_map.bind(type_param, struct_param)
    struct = types.resolve(struct)
    assert struct.signature() == "Foo<Foo<U16>>"
    assert types.full_id(struct) == "44$44$42"
