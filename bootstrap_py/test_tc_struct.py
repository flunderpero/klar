from . import ast, types
from .conftest import typecheck, typecheck_err


def test_basics() -> None:
    tc = typecheck("""

        struct Value {
            value Str
        }
        fn main() {
            let v = Value("PASS")
            v.value
        }

    """)
    typ = tc.type_at(1, 1, ast.Struct, types.Struct)
    assert len(typ.fields) == 1
    assert typ.fields[0].signature() == "value Str"
    assert tc.type_at(5, 1, ast.Ident).signature() == "test.Value"
    assert tc.type_at(6, 1, ast.Member).signature() == "Str"


def test_method_basics() -> None:
    tc = typecheck("""

        struct Value {
            value Str
        }
        fn Value.return_it(self) Str => self.value
        fn main() {
            Value("PASS").return_it()
        }

    """)
    assert tc.type_at(4, 1, ast.FnDecl).signature() == "fn test.Value.return_it(self Self) Str"
    assert tc.type_at(6, 1, ast.Ident).signature() == "test.Value"
    assert tc.type_at(6, 1, ast.Call).signature() == "Str"


def test_method_on_unknown_struct() -> None:
    err = typecheck_err("""
        fn Value.print(self) {}
    """)
    assert len(err) == 1
    assert err[0].short_message() == "`Value` is not declared in the current scope"


def test_generic_basics() -> None:
    tc = typecheck("""

        struct Value<A> {
            value A
        }
        fn Value.return_it(self) A => self.value
        fn main() {
            let v = Value<Str>("PASS")
            v.value

        }

    """)
    assert tc.type_at(4, 1, ast.FnDecl).signature() == "fn test.Value.return_it(self Self) A"
    assert tc.type_at(6, 1, ast.Ident).signature() == "test.Value<Str>"
    assert tc.type_at(7, 1, ast.Member).signature() == "Str"


# def test_generic_recursive_type() -> None:
#     tc = typecheck("""
#         struct Value<A> {
#             value A
#         }
#
#         struct ValueWrapper<B> {
#             wrapped Value<B>
#         }
#
#         fn ValueWrapper.unwrap(self) Value<B> => self.wrapped
#
#         fn Value.wrap(self) ValueWrapper<A> => ValueWrapper<A>(self)
#
#         fn main() {
#             let value = Value("PASS")
#             print(value.wrap().unwrap().value)
#         }
#         """)
#     assert tc.type_at(1, 1, ast.Struct).signature() == "test.Value<A>"
