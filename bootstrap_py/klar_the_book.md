# Klar - Coding Should Be Fun

Let's start with - of course - "Hello, world!":

```klar
fn main() {
    print("Hello, world!")
}
```

```
Hello, world!
```

## Types

### Str

`Str` is Klar's only string type. It is internally represented as an UTF-8 byte array.

```klar
fn main() {
    print("This is a Str literal.")
}
```

```
This is a Str literal.
```

### Integer

The default integer type in Klar is `Int` which is a 64-bit signed integer and an alias for `I64`.

```klar
fn main() {
    print(int_to_str(123))
    print(int_to_str(-123))
}
```

```
123
-123
```

There are all the available integer types:

I8, I16, I32, I64, U8, U16, U32, U64.

todo: implement and document

### Floating Point

The default floating point type is `Float` which is a 64-bit double precision floating point number and an alias for `F64`.

There is also `F32`, a 32-bit floating point type.

todo: implement and document

### Bool

```klar
fn main() {
    print(bool_to_str(true))
    print(bool_to_str(false))
}
```

```
true
false
```

Boolean expressions:

```klar
fn main() {
    print(bool_to_str(1 == 1))
    print(bool_to_str(1 == 2))
    print(bool_to_str(true == true))
    print(bool_to_str(true == false))
}
```

```
true
false
true
false
```

<details>
    <summary>More Examples</summary>

Not equal (`!=`):

```klar
fn main() {
    print(bool_to_str(1 != 2))
    print(bool_to_str(2 != 2))
    print(bool_to_str(true != false))
    print(bool_to_str(true != true))
}
```

```
true
false
true
false
```

</details>

### Product Types (Struct)

```klar
struct Planet {
    radius Int
    has_moons Bool
    name Str
}

fn print_planet(p Planet) {
    print(p.name)
    print(int_to_str(p.radius))
    print(bool_to_str(p.has_moons))
}

fn main() {
    let p = Planet(6371, true, "Earth")
    print_planet(p)
}
```

```
Earth
6371
true
```

Nested structs:

```klar
struct A {
    a Str
}

struct B {
    b A
}

fn print_nested(b B) {
    print(b.b.a)
}

fn main() {
    let b = B(A("PASS"))
    print_nested(b)
}
```

```
PASS
```

#### Struct Implementations

```klar
struct Planet {
    radius Int
    has_moons Bool
    name Str
}

fn Planet.diameter(self, s Str) Int {
    self.radius + self.radius
}

fn main() {
    let p = Planet(6371, true, "Earth")
    print(int_to_str(p.diameter("test")))
}
```

```
12742
```

<details>
    <summary>More Examples</summary>

Implementations must be in the same scope:

```todo
struct Value {
    value Str
}

fn main() {
    fn Value.hello(self) {} -- ERROR: `Value` is not declared in the current scope
}
```

Of course, the target type has to exist.

```klar

fn Value.hello(self) {} -- ERROR: Undefined name `Value`

fn main() {}
```

</details>

### Sum Types (Tagged Unions)

Klar has strong support for tagged unions, also called discriminated unions or enum types in other
languages.

todo: implement and document

### Traits

Klar has a simple trait system that allows you to define interfaces that can be implemented by
other types.

```klar
trait Greeter {
    fn greet(self) Str
}

struct Foo {}

fn (Greeter) Foo.greet(self) Str => "PASS"

fn main() {
    let foo = Foo()
    print(foo.greet())
}
```

```
PASS
```

Traits can offer default implementations that can be overridden:

```klar
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

```

```
Hello
world
Hi
you
```

<details>
    <summary>More Examples</summary>

All methods of a trait must be implemented:

```klar
trait HelloWorld {
    fn hello(self) Str
    fn world(self) Str
}

struct Foo {}

fn (HelloWorld) Foo.hello(self) Str => "Hello" -- ERROR: Missing implementation of trait method `world` in trait `test.HelloWorld`
fn main() {}
```

The method signature must match the trait method signature:

```klar
trait HelloWorld {
    fn hello(self) Str
}

struct Foo {}

fn (HelloWorld) Foo.hello(self) Int => 42 -- ERROR: Method signature `test.Foo.hello(self test.Foo) I64` does not match trait method signature `test.HelloWorld.hello(self test.Foo) Str`
fn main() {}
```

```klar
trait HelloWorld {
    fn hello(self) Str
}

struct Foo {}

fn (HelloWorld) Foo.hello<Int>(self) Str => "Hello" -- ERROR: Method signature `test.Foo.hello<Int, test.Foo>(self test.Foo) Str` does not match trait method signature `test.HelloWorld.hello(self test.Foo) Str`

```

Trait types cannot be used as function parameters:
todo: we should totally support that

```todo
trait Stringify {
    fn stringify(self) Str
}

struct Foo {}

fn (Stringify) Foo.stringify(self) Str => "Foo"

fn print_str(s Stringify) => print(s.stringify())

fn main() {
    print_str(Foo())
}
```

Trait types cannot be used as return type:
todo: we should totally support that

```todo
trait Stringify {
    fn stringify(self) Str
}

struct Foo {}

fn (Stringify) Foo.stringify(self) Str => "Foo"

fn some_stringifier() Stringify => Foo()

fn main() {
    print(some_stringifier().stringify())
}
```

</details>

### Function Types

In Klar, functions are first class citizens. They can be passed around like any other value.

```klar
fn add(a Int, b Int) Int {
    a + b
}

fn adder(a Int, b Int, f fn(Int, Int) Int) Int {
    f(a, b)
}

fn main() {
    print(int_to_str(adder(40, 2, add)))
    let a = adder
    print(int_to_str(a(130, 7, add)))
}
```

```
42
137
```

Function types can also be used as struct fields:

```klar
struct Adder {
    f fn(Int, Int) Int
}

fn Adder.add(self, a Int, b Int) Int {
    self.f(a, b)
}

fn add(a Int, b Int) Int {
    a + b
}

fn add_with_adder(adder Adder, a Int, b Int) Int {
    let adder_fn = adder.f
    print(int_to_str(adder_fn(130, 7)))
}

fn main() {
    let adder = Adder(add)
    print(int_to_str(adder.add(40, 2)))
    add_with_adder(adder, 130, 7)
}
```

```
42
137
```

Function types are structurally equal to their function signature:

```klar
fn add(a Int, b Int) Int => a + b

fn sub(a Int, b Int) Int => a - b

fn main() {
    let f = if false => sub else => add
    print(int_to_str(f(40, 2)))
    mut f2 = add
    if true => f2 = sub
    print(int_to_str(f2(140, 3)))
}
```

```
42
137
```

<details>
    <summary>More Examples</summary>

Function types work with generic types:

```klar
fn choose_first<T>(a T, b T) T => a

fn choose_second<T>(a T, b T) T => b

fn choose<T>(f fn(T, T) T, a T, b T) T => f(a, b)

fn main() {
    print(choose<Str>(choose_first<Str>, "PASS", "FAIL"))
}
```

```
PASS
```

```klar
struct FuncBox<T> {
    f fn(T) T
}

fn apply<T>(fb FuncBox<T>, x T) T => fb.f(x)

fn id<T>(x T) T => x

fn main() {
    let fb = FuncBox<Str>(id<Str>)
    print(apply(fb, "PASS"))
}
```

```
PASS
```

Functions can be returned from functions:

```klar
fn add(a Int, b Int) Int => a + b

fn provide_add() fn(Int, Int) Int {
    add
}

fn main() {
    let add = provide_add()
    print(int_to_str(add(40, 2)))
}
```

```
42
```

```todo return local functions.

fn provide_add() fn(Int, Int) Int {
    fn add(a Int, b Int) Int => a + b
}

fn main() {
    let add = provide_add()
    print(int_to_str(add(40, 2)))
}
```

```
42
```

Functions can be returned from instance methods:

```klar
struct FuncBox {
    f fn() Str
}

fn FuncBox.get(self) fn() Str {
    self.f
}

fn hello() Str => "PASS"

fn main() {
    let s = FuncBox(hello)
    let v = s.get()
    print(v())
}

```

```
PASS
```

Nested calls:

```klar
fn call_twice(f fn(Int) Int, x Int) Int {
    f(f(x))
}

fn incr(x Int) Int => x + 1

fn main() {
    print(int_to_str(call_twice(incr, 40)))
}
```

```
42
```

</details>

### Recursive Data Structures

Transient recursive data structures:

```klar
struct Value {
    value Str
}

struct ValueWrapper {
    wrapped Value
}

fn ValueWrapper.unwrap(self) Value => self.wrapped

fn Value.wrap(self) ValueWrapper => ValueWrapper(self)

fn main() {
    let value = Value("PASS")
    print(value.wrap().unwrap().value)
}
```

```
PASS
```

Transient recursive data structures with generics:

```klar
struct Value<A> {
    value A
}

struct ValueWrapper<B> {
    wrapped Value<B>
}

fn ValueWrapper.unwrap(self) Value<B> => self.wrapped

fn Value.wrap(self) ValueWrapper<A> => ValueWrapper<A>(self)

fn main() {
    let value = Value("PASS")
    print(value.wrap().unwrap().value)
}
```

```
PASS
```

Type recursion through type arguments:

```klar
struct Value<A> {
    value A
}

fn main() {
    let v = Value<Value<Str>>(Value<Str>("Hello"))
    print(v.value.value)
}
```

```
Hello
```

Recursive function calls:

```klar
-- You should not write code this dense. :-)
fn fib(n Int) Int => if n == 0 => 0 else => if n == 1 => 1 else => fib(n - 1) + fib(n - 2)

fn main() {
    print(int_to_str(fib(10)))
}
```

```
55
```

Recursive parameterized function calls:

```klar
-- This is a silly function but at the time of writing, the language did
-- not have enough features to write a better example.
fn return_it<T>(value T, n Int) T {
    if n == 0 => value
    else => return_it<T>(value, n - 1)
}

fn main() {
    print(return_it("Hello", 2))
}
```

```
Hello
```

## Block Expression

In Klar, blocks are expressions, i.e. they represent a value.

A regular block is enclosed by curly braces (`{ }`).

```klar
fn main() {
    print(
        {
            "PASS"
        }
    )
}
```

```
PASS
```

Klar also supports a shorthand notation for single-expression blocks:

```klar
fn main() {
    print(
        => "PASS"
    )
}
```

```
PASS
```

## Variables

```klar
fn main() {
    let s = "PASS"
    print(s)
}
```

```
PASS
```

Variables are scoped:

```klar
fn main() {
    let s = "world"
    if true {
        let s = "Hello"
        print(s)
    }
    print(s)
}
```

```
Hello
world
```

The value can be a block expression, too:

```klar
fn main() {
    let s = {
        if true => "PASS" else => "world"
    }
    print(s)
}
```

```
PASS
```

### Mutability

Declare mutable variables with `mut` instead of `let`:

```klar
fn main() {
    mut s = "Hello"
    print(s)
    s = "world"
    print(s)
}
```

```
Hello
world
```

Trying to mutate an immutable variable is a compile error:

```klar
fn main() {
    let s = "Hello"
    s = "world" -- ERROR: `s` is not mutable
}
```

<details>
    <summary>More Examples</summary>

Mutating a variable in both branches of an `if` expression:

```klar
fn main() {
    mut s = ""
    if true => s = "PASS1" else => s = "FAIL"
    print(s)
    if false => s = "FAIL" else => s = "PASS2"
    print(s)
}
```

```
PASS1
PASS2
```

Mutating a variable in the `then` branch only of an `if` expression:

```klar
fn main() {
    mut s = "BEFORE"
    if true => s = "PASS" else => print(s)
    print(s)
}
```

```
PASS
```

Mutating a variable in the `else` branch only of an `if` expression:

```klar
fn main() {
    mut s = "FAIL"
    if false {} else => s = "PASS"
    print(s)
}
```

```
PASS
```

Function parameters are immutable by default:

```klar
fn foo(x Int) {
    x = 12  -- ERROR: `x` is not mutable
}
```

</details>

## Control Flow

### `if` Expressions

```klar
fn main() {
    if true => print("PASS") else => print("FAIL")
}
```

```
PASS
```

An `if` expression can be used just like every other expression:

```klar
fn main() {
    print(
        if true => "PASS" else => "FAIL"
    )
}
```

```
PASS
```

<details>
    <summary>More examples</summary>

Nested `if` expression:

```klar
fn main() {
    print(
        if true {
            if false {
                "FAIL"
            } else {
                if true {
                    "PASS"
                } else {
                    "FAIL"
                }
            }
        } else {
            "FAIL"
        }
    )
}
```

```
PASS
```

Conditions must be of type `Bool`:

```klar
fn main() {
    if 1 => print("Hello") -- ERROR: Expected Bool, got I64
}
```

Then and else block must have the same type:

```klar
fn main() {
    if true => "str" else => 42 -- ERROR: Expected Str, got I64
}
```

</details>

### Loops

Klar supports simple unconditional loops with the `loop` keyword.

```klar
fn main() {
    mut i = 0
    loop {
        i = i + 1
        if i == 2 => continue
        print(int_to_str(i))
        if i == 5 => break
    }
}
```

```
1
3
4
5
```

<details>
    <summary>More Examples</summary>

Nested loops:

```klar
fn main() {
    mut i = 0
    loop {
        i = i + 1
        if i == 3 => break
        print("A")
        mut j = 0
        loop {
            j = j + 1
            if j == 4 => break
            if j == 2 => continue
            print(int_to_str(j))
        }
    }
}
```

```
A
1
3
A
1
3
```

`break` can only occur inside a loop:

```klar
fn main() {
    break -- ERROR: `break` outside of a loop
}
```

`continue` can only occur inside a loop:

```klar
fn main() {
    continue -- ERROR: `continue` outside of a loop
}
```

</details>

## Functions

```klar
fn double(i Int) Int {
    i + i -- The last expression is the implicit return value.
}

fn main() {
    print(int_to_str(double(2)))
}
```

```
4
```

<details>
<summary>More examples</summary>

Shadowing:

```klar
fn str(i Int) Str => int_to_str(i)

fn foo(str Str) {
    -- Here, the parameter `str` shadows the function `str()`.
    print(str)
}

fn main() {
    foo("PASS")
}
```

```
PASS
```

</details>

## Arithmetic

Integer operators:

```klar
fn main() {
    print(int_to_str(40 + 2))
    print(int_to_str(44 - 2))
}
```

```
42
42
```

All arithmetic operators are left-associative. So the following example evaluates to `(40 - 2) + 4`
and not `40 - (2 + 4)`.

```klar
fn main() {
    print(int_to_str(40 - 2 + 4))
}
```

```
42
```

### Overflow, Underflow, Divide By Zero

Klar does not prevent addition (`+`), subtraction (`-`), or multiplication (`*`) operations from
over- or underflowing. We think that this is expected behavior and should be natural to every
developer. If you need over- or underflow safety, use `Int.add()`, `Int.sub()`, and `Int.mul()`.
Division by zero using the division operator (`/`) causes a `panic` for integer types and
results in `NaN` for floating point types. Use `Int.div()` to catch division by zero errors.

## Parametric Polymorphism - Generics

Klar aims to provide robust parametric polymorphism without going overboard with the type-system
becoming too complex.

```klar
fn return_it<T>(value T) T => value

fn main() {
    print(return_it<Str>("PASS"))
    print(int_to_str(return_it<Int>(42)))
}
```

```
PASS
42
```

Type parameter resolution is cascading:

```klar
fn return_it_again<U>(value U) U => value

fn return_it<T>(value T) T => return_it_again<T>(value)

fn main() {
    print(return_it<Str>("PASS"))
}
```

```
PASS
```

Parameterized structs:

```klar
struct Pair<A, B> {
    a A
    b B
}

fn print_str_int_pair(p Pair<Str, Int>) {
    print(p.a)
    print(int_to_str(p.b))
}

fn main() {
    let p1 = Pair<Str, Int>("Hello", 42)
    print_str_int_pair(p1)
    let p2 = Pair<Bool, Str>(true, "world")
    print(bool_to_str(p2.a))
    print(p2.b)
}
```

```
Hello
42
true
world
```

Parameterized struct with instance methods:

```klar
struct Value<T> {
    value T
}

fn Value.get(self) T {
    self.value
}

fn main() {
    let v = Value<Str>("PASS")
    print(v.get())
}
```

```
PASS
```

Parameterized traits and trait implementation:

```klar
struct Value {
    value Str
}

trait ReturnIt<T> {
    fn return_it(self, t T) T
}

fn (ReturnIt<Str>) Value.return_it(self, t Str) Str => t

fn main() {
    let v = Value("FAIL")
    print(v.return_it("PASS"))

}
```

```
PASS
```

Type arguments can be inferred in most cases:

```klar
fn return_it<T>(value T) T => value

fn main() {
    print(return_it("PASS"))
}

```

```
PASS
```

```klar
struct Value<A> {
    value A
}

fn main() {
    print(Value("PASS").value)
}
```

```
PASS
```

<details>
<summary>More Examples</summary>

Multiple type parameters:

```klar

fn ignore_second<T, U>(first T, second U) T => first

fn main() {
    print(ignore_second<Str, Int>("PASS", 42))
}

```

```
PASS
```

Only one specialized version of a function is created:

```klar
fn return_it<T>(value T) T => value

fn main() {
    print(return_it<Str>("Hello"))
    print(return_it<Str>("world"))
}
```

```
Hello
world
```

The number of type arguments must match the number of type parameters:

```klar
fn return_it<T>(value T) T => value

fn main() {
    return_it<Str, Int>("test") -- ERROR: Expected 1 type arguments, got 2
}
```

Parameter type must match type argument:

```klar
fn return_it<T>(value T) T => value

fn main() {
    return_it<Str>(42) -- ERROR: Type `I64` is not assignable to type `Str`
}

```

Nested type arguments:

```klar
struct Value<T> {
    value T
}

struct Pair<A, B> {
    a Value<A>
    b Value<B>
}

fn print_pair(p Pair<Str, Int>) {
    print(p.a.value)
    print(int_to_str(p.b.value))
}

fn main() {
    let v = Value<Str>("PASS")
    let p = Pair<Str, Int>(v, Value<Int>(42))
    print_pair(p)
}
```

```
PASS
42
```

```klar
struct ValueA<T> {
    a T
}

fn ValueA.get(self) T => self.a

struct ValueB<B> {
    b B
}

fn ValueB.get(self) B => self.b

fn main() {
    let v = ValueA(ValueB("PASS"))
    print(v.a.get())
}
```

```
PASS
```

Parameterized instance methods:

```klar
struct Value<A> {
    value A
}

-- This shadows the type parameter `A` of `Value<A>`.
fn Value.pass_through<A>(self, x A) A {
    x
}

fn main() {
    let v = Value<Bool>(false)
    print(int_to_str(v.pass_through<Int>(42)))
    print(v.pass_through<Str>("PASS"))
}
```

```
42
PASS
```

Parameterized traits must be implemented with the same trait qualifier:

```klar
struct Value {
    value Str
}

trait ReturnIt<T> {
    fn return_it(self, t T) T
    fn return_it_again(self, t T) T
}

fn (ReturnIt<Str>) Value.return_it(self, t Str) Str => t

fn (ReturnIt<Int>) Value.return_it_again(self, t Int) Int => t -- ERROR: Trait has already been implemented for `test.Value` with signature `test.ReturnIt<Str>`

fn main() {}
```

Parameterized traits can be implemented on paramerized types:

```klar
struct Value<T> {
    value T
}

trait ReturnIt<T> {
    fn return_it(self, t T) T
}

fn (ReturnIt<T>) Value.return_it(self, t T) T => self.value

fn main() {
    let v = Value("PASS")
    print(v.return_it("FAIL"))
}
```

```
PASS
```

</details>

### Trait Bounds

```klar
trait Stringify {
    fn stringify(self) Str
}

struct IntValue {
    value Int
}

fn (Stringify) IntValue.stringify(self) Str => int_to_str(self.value)

fn print_any<T Stringify>(s T) {
    print(s.stringify())
}

fn main() {
    let iv = IntValue(42)
    print_any(iv)
}

```

```
42
```

Trait bounds in structs:

```klar
trait Stringify {
    fn stringify(self) Str
}

struct Value<T Stringify> {
    value T
}

fn (Stringify) Value.stringify(self) Str => self.value.stringify()

struct Pass {}

fn (Stringify) Pass.stringify(self) Str => "PASS"

fn main() {
    let v = Value(Pass())
    print(v.stringify())
}
```

```
PASS
```

Dependent type parameters:

```klar
struct Value<A> {
    value A
}

trait Wrapped<B> {
    fn unwrap(self) B
}

fn (Wrapped<A>) Value.unwrap(self) A => self.value

fn unwrap<D, E Wrapped<D>>(value E) D => value.unwrap()

fn main() {
    print(unwrap<Str, Value<Str>>(Value("PASS1")))
    -- Type arguments can also be inferred.
    print(unwrap(Value("PASS2")))
}

```

```
PASS1
PASS2
```

```klar
struct Value<A> {
    value A
}

trait Wrapped<B> {
    fn unwrap(self) B
}

fn (Wrapped<A>) Value.unwrap(self) A => self.value

fn unwrap_first<D, E Wrapped<D>, F, G Wrapped<F>>(first E, second G) D {
    second.unwrap()
    first.unwrap()
}

fn unwrap_second<D, E Wrapped<D>, F, G Wrapped<F>>(first E, second G) F {
    first.unwrap()
    second.unwrap()
}

fn main() {
    let s = Value("PASS")
    let i = Value(42)
    print(unwrap_first<Str, Value<Str>, Int, Value<Int>>(s, i))
    -- Type arguments can also be inferred.
    print(int_to_str(unwrap_second(s, i)))
}

```

```
PASS
42
```

<details>
    <summary>More Examples</summary>

Traits cannot implement other traits:

```klar
trait Stringify {
    fn stringify(self) Str
}

trait Intify {
    fn intify(self) Int
}

fn Stringify.stringify(self) Str => "Hello"

fn (Intify) Stringify.intify(self) Int => 42 -- ERROR: Traits cannot implement other traits

fn main() {}
```

The signature of a default method implementation must match the trait method declaration:

```klar
trait Stringify {
    fn stringify(self) Str
}

fn Stringify.stringify(self) Int => 42 -- ERROR: Method signature `test.Stringify.stringify(self Self) I64` does not match trait method signature `test.Stringify.stringify(self Self) Str`
```

</details

## Appendix - The Tokenizer

<details>
    <summary>Tokenize Errors</summary>

```klar
fn main() {
    "unterminated str literal -- ERROR: Unterminated string literal
    print("Hello, world!")
}
```

</details>

## Appendix - The Parser

<details>
    <summary>Parse Errors</summary>

```klar
fn main()
    "Hello, world!" -- ERROR: Expected one of `{`, `=>`, got `str literal`
```

Duplicate parameter names:

```klar
fn test(a Int, a Int) {} -- ERROR: Duplicate `a`
```

</details>

## Appendix - The Type-Checker

### Forward Declaration

In Klar, all types are forward declared, i.e. they can be used before they are declared in the
source code.

```klar
fn main() {
    forward()
}

fn forward() => forward2(Named("PASS"))

fn forward2(named Named) => print(named.name)

struct Named {
    name Str
}
```

```
PASS
```

### Further Tests

<details>
    <summary>Type-Check Errors</summary>

```klar
fn main() {
    print(true) -- ERROR: Type `Bool` is not assignable to type `Str`
}
```

```klar
fn main() {
    1 + "str" -- ERROR: Type `Str` is not assignable to type `I64`
}
```

`self` parameter must be the first parameter:

```klar
struct Value {
    value Str
}

fn Value.print(i Int, self) {} -- ERROR: `self` is not allowed here
```

`self` parameter cannot be used in regular functions:

```klar
fn say_hello(self) => {} -- ERROR: `self` is not allowed here
```

</details>
