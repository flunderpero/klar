from . import ast
from .conftest import typecheck, types


def test_basics() -> None:
    tc = typecheck("""

        fn return_it<A>(value A) A => value
        fn main() {
            return_it("PASS")
            return_it(42)
        }

    """)
    assert tc.type_at(1, 1, ast.FnDecl).signature() == "fn test.return_it<A>(value A) A"
    assert tc.type_at(3, 1, ast.Ident).signature() == "fn test.return_it<Str>(value Str) Str"
    assert tc.type_at(4, 1, ast.Ident).signature() == "fn test.return_it<I64>(value I64) I64"


def test_function_argument() -> None:
    tc = typecheck("""

        fn process<A, B>(a A, f fn(A) B) B => f(a)
        fn to_string(i Int) Str => int_to_str(i)
        fn main() {
            process(42, to_string)
        }

    """)
    assert tc.type_at(4, 1, ast.Ident).signature() == "fn test.process<I64, Str>(a I64, f fn(I64) Str) Str"


def test_function_argument_and_different_type_parameters() -> None:
    tc = typecheck("""

        fn identity<T>(x T) T => x
        fn apply_twice<U>(x U, f fn(U) U) U => f(f(x))
        fn main() {
            apply_twice("hello", identity<Str>)
        }

    """)
    assert tc.type_at(4, 1, ast.Ident).signature() == "fn test.apply_twice<Str>(x Str, f fn(Str) Str) Str"


def test_nested_struct() -> None:
    tc = typecheck("""

        struct Value<A> {
            value A
        }

        fn main() {
            let v = Value("PASS")
            let v2 = Value(Value(42))
        }

    """)
    assert tc.type_at(6, 1, ast.Ident).signature() == "test.Value<Str>"
    assert tc.type_at(7, 1, ast.Ident).signature() == "test.Value<test.Value<I64>>"


def test_nested_struct_instantiation() -> None:
    tc = typecheck("""
        struct Pair<A, B> {
            first A
            second B
        }
        fn make_pair<X, Y>(x X, y Y) Pair<X, Y> => Pair(x, y)
        fn main() {
            make_pair("hello", 42)
            make_pair(true, make_pair(1, 2))
        }
    """)
    assert tc.type_at(7, 1, ast.Ident).signature() == "fn test.make_pair<Str, I64>(x Str, y I64) test.Pair<Str, I64>"
    assert tc.type_at(8, 1, ast.Ident).signature() == (
        "fn test.make_pair<Bool, test.Pair<I64, I64>>("
        "x Bool, y test.Pair<I64, I64>) test.Pair<Bool, test.Pair<I64, I64>>"
    )


def test_dependent_type_parameters_and_type_inference() -> None:
    tc = typecheck("""

        struct Value<A> {
            value A
        }
        trait Wrapped<B> {
            fn unwrap(self) B
        }
        fn (Wrapped<A>) Value.unwrap(self) A => self.value
        fn unwrap<D, E Wrapped<D>>(value E) D => value.unwrap()
        fn main() {
            unwrap(Value("PASS"))
            unwrap(Value(42))
        }

    """)
    assert tc.type_at(1, 1, ast.Struct).signature() == "test.Value<A>"
    assert tc.type_at(7, 1, ast.FnDecl).signature() == "fn test.Value.unwrap(self Self) A"
    assert (
        types.resolve(tc.type_at(8, 1, ast.FnDecl)).signature()
        == "fn test.unwrap<D, E test.Wrapped<D>>(value E test.Wrapped<D>) D"
    )
    assert (
        types.resolve(tc.type_at(10, 1, ast.Ident)).signature()
        == "fn test.unwrap<Str, test.Value<Str>>(value test.Value<Str>) Str"
    )
    assert (
        types.resolve(tc.type_at(11, 1, ast.Ident)).signature()
        == "fn test.unwrap<I64, test.Value<I64>>(value test.Value<I64>) I64"
    )
