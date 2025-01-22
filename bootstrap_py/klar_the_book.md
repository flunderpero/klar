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

## Functions

```klar
fn double(i Int) Int {
    i + i
}

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
fn str(i Int) Str {
    int_to_str(i)
}

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

### Arithmetic

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
> results in `NaN` for floating point types.

## Appendix - The Tokenizer

```klar
"unterminated str literal -- Compile error: Unterminated string literal
print("Hello, world!")
```

## Appendix - The Parser

```klar
fn main()
    "Hello, world!" -- Compile error: Expected `{`, got `str literal`
```

Duplicate parameter names:

```klar
fn test(a Int, a Int) {} -- Compile error: Duplicate `a`
```

## Appendix - The Type-Checker

```klar
print(true) -- Compile error: Type `Bool` is not assignable to type `Str`
```

```klar
1 + "str" -- Compile error: Type `Str` is not assignable to type `I64`

```
