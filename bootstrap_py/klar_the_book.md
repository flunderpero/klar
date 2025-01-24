# Klar - Coding Should Be Fun

Let's start with - of course - "Hello, world!":

```klar
fn main() {
    print("Hello, world!")
}

-- Output:
-- Hello, world!
```

## Built-in Types And Literals

### Str

`Str` is Klar's only string type. It is internally represented as an UTF-8 byte array.

```klar
"This is a Str literal."
print("This is a Str literal.")

-- Output:
-- This is a Str literal.
```

### Integer Types

The default integer type in Klar is `Int` which is a 64-bit signed integer and an alias for `I64`.

```klar
123 -- This is an Int literal.
-123 -- A negative Int literal.
print(int_to_str(123))
print(int_to_str(-123))

-- Output:
-- 123
-- -123
```

### Boolean

```klar
true
false
print(bool_to_str(true))
print(bool_to_str(false))

-- Output:
-- true
-- false
```

## Block Expression

In Klar, blocks are expressions, i.e. they represent a value.

A regular block is enclosed by curly braces (`{ }`).

```klar
print(
    {
        "Hello"
    }
)
-- Output:
-- Hello
```

Klar also supports a shorthand notation for single-expression blocks:

```klar
print(
    => "Hello"
)
-- Output:
-- Hello
```

## Variables

```klar
let s = "Hello"
print(s)
-- Output:
-- Hello
```

Variables are scoped:

```klar
let s = "world"
if true {
    let s = "Hello"
    print(s)
}
print(s)
-- Output:
-- Hello
-- world
```

The value can be a block expression, too:

```klar
let s = {
    if true => "Hello" else => "world"
}
print(s)
-- Output:
-- Hello
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
-- Output:
-- Hello
-- world
```

```klar
fn main() {
    let s = "Hello"
    s = "world" -- Compile error: `s` is not mutable
}
```

<detail>
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
-- Output:
-- PASS1
-- PASS2
```

Mutating a variable in the `then` branch only of an `if` expression:

```klar
fn main() {
    mut s = "BEFORE"
    if true => s = "PASS" else => print(s)
    print(s)
}
-- Output:
-- PASS
```

Mutating a variable in the `else` branch only of an `if` expression:

```klar
fn main() {
    mut s = "FAIL"
    if false {} else => s = "PASS"
    print(s)
}
-- Output:
-- PASS
```

Function parameters are immutable by default:

````klar
fn foo(x Int) {
    x = 12  -- Compile error: `x` is not mutable
}
</detail>

## Control Flow

### `if` Expressions

```klar
if true => print("PASS") else => print("FAIL")
-- Output:
-- PASS
```

An `if` expression can be used just like every other expression:

```klar
print(
    if true => "PASS" else => "FAIL"
)
-- Output:
-- PASS
```

<details>
    <summary>More examples</summary>

Nested `if` expression:

```klar
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
-- Output:
-- PASS
```

Conditions must be of type `Bool`:

```klar
if 1 => print("Hello") -- Compile error: Expected Bool, got I64
```

Then and else block must have the same type:

```klar
if true => "str" else => 42 -- Compile error: Expected Str, got I64
```

</details>

## Functions

```klar
fn double(i Int) Int => i + i

fn main() {
    print(int_to_str(double(2)))
}
-- Output:
-- 4
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
-- Output:
-- Hello
```

</details>

## Arithmetic

Integer operators:

```klar
print(int_to_str(40 + 2))
print(int_to_str(44 - 2))
-- Output:
-- 42
-- 42
```

All arithmetic operators are left-associative. So the following example evaluates to `(40 - 2) + 4`
and not `40 - (2 + 4)`.

```klar
print(int_to_str(40 - 2 + 4))
-- Output:
-- 42
```

> **Note**: Klar does not prevent addition (`+`), subtraction (`-`), or multiplication (`*`) operations from
> over- or underflowing. We think that this is expected behavior and should be natural to every
> developer. If you need over- or underflow safety, use `Int.add()`, `Int.sub()`, and `Int.mul()`.
> Division by zero using the division operator (`/`) causes a `panic` for integer types and
> results in `NaN` for floating point types. Use `Int.div()` to catch division by zero errors.

## Appendix - The Tokenizer

```klar
"unterminated str literal -- Compile error: Unterminated string literal
print("Hello, world!")
```

## Appendix - The Parser

<details>
    <summary>Parse Errors</summary>

```klar
fn main()
    "Hello, world!" -- Compile error: Expected one of `{`, `=>`, got `str literal`
```

Duplicate parameter names:

```klar
fn test(a Int, a Int) {} -- Compile error: Duplicate `a`
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
-- Output:
-- PASS
```

<details>
    <summary>Type-Check Errors</summary>

```klar
print(true) -- Compile error: Type `Bool` is not assignable to type `Str`
```

```klar
1 + "str" -- Compile error: Type `Str` is not assignable to type `I64`

```

</details>
````

