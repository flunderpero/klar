# Klar - Coding Should Be Fun

Let's start with - of course - "Hello, world!":

```klar
fn main() {
    print("Hello, world!")
}
-- Output: Hello, world!
```

## Built-in Types And Literals

### Str

`Str` is Klar's only string type. It is internally represented as an UTF-8 byte array.

```klar
"This is a Str literal."
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
