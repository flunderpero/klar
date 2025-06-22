from __future__ import annotations

import pytest

from .conftest import compile_and_run_success, strip


def test_happy_path() -> None:
    stdout = compile_and_run_success('fn main() => print("PASS")')
    assert stdout == "PASS\n"


def test_assign_to_variable() -> None:
    stdout = compile_and_run_success(
        """
        fn main() {
            mut x = 41
            x = x + 1
            print(int_to_str(x))
        }
        """
    )
    assert stdout == strip(
        """
        42
        """
    )


def test_function_types_are_structurally_equal() -> None:
    stdout = compile_and_run_success(
        """
        fn add(a Int, b Int) Int => a + b

        fn sub(a Int, b Int) Int => a - b

        fn main() {
            let f = if false => sub else => add
            print(int_to_str(f(40, 2)))
            mut f2 = add
            if true => f2 = sub
            print(int_to_str(f2(140, 3)))
        }
        """
    )
    assert stdout == strip(
        """
        42
        137
        """
    )


def test_function_generics_basics() -> None:
    stdout = compile_and_run_success(
        """
        fn return_it<A>(value A) A => value

        fn return_it_again<B>(value B) B => value

        fn main() {
            print(return_it_again<Str>("PASS"))
            print(int_to_str(return_it_again<Int>(42)))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        42
        """
    )


def test_function_generics_with_multiple_type_parameters() -> None:
    stdout = compile_and_run_success(
        """
        fn return_first<A, B>(a A, b B) A => a
        fn return_second<C, D>(c C, d D) D {
            return_first<D, C>(d, c)
        }
        fn main() {
            print(return_second<Int, Str>(42, "PASS"))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_function_generics_assigned_to_variable() -> None:
    stdout = compile_and_run_success(
        """
        fn return_it<A>(value A) A => value

        fn main() {
            let return_str = return_it<Str>("PASS")
            print(return_str)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_struct_field_read_and_assign() -> None:
    stdout = compile_and_run_success(
        """
        struct StrInt {
            s Str
            i Int
        }

        fn main() {
            let x = StrInt("PASS", 41)
            x.i = x.i + 1
            print(x.s)
            print(int_to_str(x.i))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        42
        """
    )


def test_struct_method_basics() -> None:
    stdout = compile_and_run_success(
        """
        struct Value {
            value Str
        }

        fn Value.print(self) => print(self.value)

        fn main() {
            let x = Value("PASS")
            x.print()
        }
        """
    )
    assert stdout == "PASS\n"


def test_struct_method_assigned_to_variable() -> None:
    stdout = compile_and_run_success(
        """
        struct Value {
            value Str
        }

        fn Value.print(self) => print(self.value)

        fn main() {
            let x = Value("PASS")
            let p = x.print
            p()
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_struct_method_passed_as_argument() -> None:
    pytest.skip("Not implemented")


def test_struct_generic_field() -> None:
    stdout = compile_and_run_success(
        """
        struct Pair<A, B> {
            s A
            i B
        }

        fn main() {
            let x = Pair<Str, Int>("PASS", 42)
            print(x.s)
            print(int_to_str(x.i))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        42
        """
    )


def test_struct_generic_function_field() -> None:
    stdout = compile_and_run_success(
        """
        struct FuncBox<A> {
            f fn(A) A
        }

        fn apply<B>(fb FuncBox<B>, x B) B => fb.f(x)

        fn id<C>(x C) C => x

        fn main() {
            let fb = FuncBox<Str>(id<Str>)
            print(apply<Str>(fb, "PASS"))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_trait_basics() -> None:
    stdout = compile_and_run_success(
        """
        trait Printable {
            fn print(self)
        }

        struct StrValue {
            value Str
        }
        fn (Printable) StrValue.print(self) => print(self.value)

        struct IntValue {
            value Int
        }
        fn (Printable) IntValue.print(self) => print(int_to_str(self.value))

        fn print_printable<P Printable>(p P) => p.print()

        fn main() {
            let x = StrValue("PASS")
            x.print()
            print_printable<StrValue>(x)

            let y = IntValue(42)
            y.print()
            print_printable<IntValue>(y)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        PASS
        42
        42
        """
    )


def test_generic_trait() -> None:
    stdout = compile_and_run_success(
        """
        trait Printable<A> {
            fn print(self, more A)
        }

        struct Value {
            value Str
        }

        fn (Printable<Int>) Value.print(self, more Int) {
            print(self.value)
            print(int_to_str(more))
        }

        fn main() {
            let x = Value("PASS")
            x.print(42)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        42
        """
    )


def test_trait_with_default_implementation() -> None:
    stdout = compile_and_run_success(
        """
        trait HelloWorld {
            fn hello(self) Str
            fn world(self) Str
            fn print(self) None
        }

        fn HelloWorld.hello(self) Str => "Hello"

        fn HelloWorld.print(self) {
            print(self.hello())
            print(self.world())
        }

        struct TheHelloWorld {}
        fn (HelloWorld) TheHelloWorld.world(self) Str => "world"

        struct TheHiYou {}
        fn (HelloWorld) TheHiYou.hello(self) Str => "Hi"
        fn (HelloWorld) TheHiYou.world(self) Str => "you"

        fn main() {
            TheHelloWorld().print()
            TheHiYou().print()
        }
        """
    )
    assert stdout == strip(
        """
        Hello
        world
        Hi
        you
        """
    )


def test_trait_method_assigned_to_variable() -> None:
    stdout = compile_and_run_success(
        """
        trait Printable {
            fn print(self)
        }

        struct Value {
            value Str
        }

        fn (Printable) Value.print(self) => print(self.value)

        fn main() {
            let v = Value("PASS")
            let p = v.print
            p()
        }
        """
    )
    assert stdout == "PASS\n"


def test_dependent_type_parameters() -> None:
    stdout = compile_and_run_success(
        """
        struct Value<A> {
            value A
        }

        trait Wrapped<B> {
            fn unwrap(self) B
        }

        fn (Wrapped<A>) Value.unwrap(self) A => self.value

        fn unwrap<D, E Wrapped<D>>(value E) D => value.unwrap()

        fn main() {
            print(unwrap<Str, Value<Str>>(Value<Str>("PASS")))
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_recursive_type_basics() -> None:
    stdout = compile_and_run_success(
        """
        struct Value<A> {
            value A
        }

        struct ValueWrapper<B> {
            wrapped Value<B>
        }

        fn ValueWrapper.unwrap(self) Value<B> => self.wrapped

        fn Value.wrap(self) ValueWrapper<A> => ValueWrapper<A>(self)

        fn main() {
            let value = Value<Str>("PASS")
            value.wrap()
            print(value.wrap().unwrap().value)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_recursive_type_across_multiple_types() -> None:
    stdout = compile_and_run_success(
        """
        trait Getter<A> {
            fn get(self) A
        }

        struct Value<B> {
            value B
        }

        struct ValueGetter<C> {
            value Value<C>
        }

        fn (Getter<C>) ValueGetter.get(self) C {
            self.value.value
        }

        fn Value.getter(self) ValueGetter<B> => ValueGetter<B>(self)

        fn main() {
            let v = Value<Str>("PASS")
            let g = v.getter()
            print(g.get())
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_recursive_type_through_type_parameter() -> None:
    stdout = compile_and_run_success(
        """
        struct Value<A> {
            value A
        }

        fn main() {
            let v = Value<Value<Str>>(Value<Str>("PASS"))
            print(v.value.value)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )


def test_loop() -> None:
    stdout = compile_and_run_success(
        """
        fn main() {
            mut i = 0
            loop {
                i = i + 1
                if i == 2 => continue
                print(int_to_str(i))
                if i == 5 => break
            }
        }
        """
    )
    assert stdout == strip(
        """
        1
        3
        4
        5
        """
    )
