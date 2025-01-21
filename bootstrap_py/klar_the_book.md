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

## Appendix - The Tokenizer

```klar
"unterminated str literal -- Compile error: Unterminated string literal
print("Hello, world!")
```

## Appendix - The Parser

```klar
fn main()
    "Hello, world!" -- Compile error: Expected `curly_left`
```

Duplicate parameter names:

```klar
fn test(a Int, a Int) {} -- Compile error: Duplicate `a`
```

## Appendix - The Type-Checker

```klar
fn main() {
    print(true) -- Compile error: Type `Bool` is not assignable to type `Str`
}
```
