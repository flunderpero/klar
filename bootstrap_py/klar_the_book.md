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

## Built-in Types And Literals

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

### Integer Types

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

## Block Expression

In Klar, blocks are expressions, i.e. they represent a value.

A regular block is enclosed by curly braces (`{ }`).

```klar
fn main() {
    print(
        {
            "Hello"
        }
    )
}
```

```
Hello
```

Klar also supports a shorthand notation for single-expression blocks:

```klar
fn main() {
    print(
        => "Hello"
    )
}
```

```
Hello
```

## Variables

```klar
fn main() {
    let s = "Hello"
    print(s)
}
```

```
Hello
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
        if true => "Hello" else => "world"
    }
    print(s)
}
```

```
Hello
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
    foo("Hello")
}
```

```
Hello
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

fn forward() => forward2()

fn forward2() => print("PASS")
```

```
PASS
```

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

</details>
