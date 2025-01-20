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
