from . import ast, types
from .conftest import typecheck, typecheck_err


def test_basics() -> None:
    tc = typecheck("""

        trait Stringer {
            fn stringer(self) Str
        }
        struct Value {
            value Str
        }
        fn (Stringer) Value.stringer(self) Str => self.value
        fn main() {
            let v = Value("PASS")
            v.stringer()
        }

    """)
    typ = tc.type_at(1, 1, ast.Trait, types.Trait)
    assert len(typ.methods) == 1
    assert typ.methods[0].signature() == "stringer fn test.Stringer.stringer(self Self) Str"
    assert tc.type_at(10, 1, ast.Member).signature() == "fn test.Value.stringer(self Self) Str"
    assert tc.type_at(10, 1, ast.Call).signature() == "Str"


def test_trait_bounds() -> None:
    tc = typecheck("""

        trait Stringer {
            fn stringer(self) Str
        }
        struct Value {
            value Str
        }
        fn (Stringer) Value.stringer(self) Str => self.value
        fn print_stringer<T Stringer>(s T) => print(s.stringer())
        fn main() {
            let v = Value("PASS")
            print_stringer(v)
        }

    """)
    typ = tc.type_at(1, 1, ast.Trait, types.Trait)
    assert len(typ.methods) == 1
    assert typ.methods[0].signature() == "stringer fn test.Stringer.stringer(self Self) Str"
    assert tc.type_at(11, 1, ast.Call).signature() == "None"


def test_trait_bounds_undefined_method() -> None:
    errors = typecheck_err("""

        trait Stringer {
            fn stringer(self) Str
        }
        struct Value {
            value Str
        }
        fn (Stringer) Value.stringer(self) Str => self.value
        fn print_stringer<T Stringer>(s T) => print(s.not_defined())

    """)
    assert len(errors) == 2
    assert errors[0].short_message() == "No member `not_defined` in type `test.Stringer`"
    assert errors[1].short_message() == "Only functions and structs can be called"
