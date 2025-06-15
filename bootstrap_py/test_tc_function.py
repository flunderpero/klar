from . import ast
from .conftest import typecheck


def test_generic_basics() -> None:
    tc = typecheck("""

        fn return_it<A>(value A) A => value

    """)
    assert tc.type_at(1, 1, ast.FnDecl).signature() == "fn test.return_it<A>(value A) A"


def test_generic_assign_fn_to_variable() -> None:
    tc = typecheck("""

        fn return_it<A>(value A) A => value
        fn main() {
            let return_str = return_it<Str>
            return_str
            return_str("PASS")
        }

    """)
    assert tc.type_at(1, 1, ast.FnDecl).signature() == "fn test.return_it<A>(value A) A"
    assert tc.type_at(3, 1, ast.Ident).signature() == "fn test.return_it<Str>(value Str) Str"
    assert tc.type_at(4, 1, ast.Ident).signature() == "fn test.return_it<Str>(value Str) Str"
    assert tc.type_at(5, 1, ast.Call).signature() == "Str"


def test_generic_with_multiple_type_parameters() -> None:
    tc = typecheck("""

        fn return_first<A, B>(a A, b B) A => a
        fn return_second<C, D>(c C, d D) D {
            return_first<D, C>(d, c)
        }
        fn main() {
            return_second<Str, Int>("PASS", 42)
        }

    """)
    assert tc.type_at(1, 1, ast.FnDecl).signature() == "fn test.return_first<A, B>(a A, b B) A"
    assert tc.type_at(2, 1, ast.FnDecl).signature() == "fn test.return_second<C, D>(c C, d D) D"
    assert tc.type_at(3, 1, ast.Ident).signature() == "fn test.return_first<D, C>(a D, b C) D"
    assert tc.type_at(6, 1, ast.Ident).signature() == "fn test.return_second<Str, I64>(c Str, d I64) I64"


def test_generic_nested_function_calls() -> None:
    tc = typecheck("""

        fn return_it<A>(value A) A => value
        fn return_it_again<B>(value B) B {
            return_it<B>(value)
        }
        fn main() {
            return_it_again<Str>("PASS")
        }

    """)
    assert tc.type_at(1, 1, ast.FnDecl).signature() == "fn test.return_it<A>(value A) A"
    assert tc.type_at(2, 1, ast.FnDecl).signature() == "fn test.return_it_again<B>(value B) B"
    assert tc.type_at(3, 1, ast.Ident).signature() == "fn test.return_it<B>(value B) B"
    assert tc.type_at(6, 1, ast.Ident).signature() == "fn test.return_it_again<Str>(value Str) Str"
