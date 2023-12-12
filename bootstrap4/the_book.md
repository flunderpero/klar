# Klar - The Official Guide

## Common Concepts

### Comments

```klar
fn main():
    ---
    This is a multi-line comment.
    ---

    let a = 42 -- This is a single line comment.
end
```

Documentation comments are multi-line comments put a above
the declaration of the thing it is supposed to document.

```klar

---
A planet is a body circumventing a star.
---
struct Planet:
    --- The mass in kg. ---
    mass i32 -- TODO: change to u64
    -- This is a regular comment and not a documentation comment.
    name str
end

fn main():
end
```

### Types

#### Built-in Types

##### Number types

Unsigned integers: `u8`, `u16`, `u32`, `u64`, `u128`, `usize`
Signed integers: `i8`, `i16`, `i32`, `i64`, `i128`, `isize`
Floating point numbers: `f32`, `f64`

```klar

fn main():
    let a = 2

    assert(a == 2)
end
```

##### The Boolean Type

```klar
fn main():
    let a = true
    let b bool = false
end
```


##### The String Type

```klar
fn main():
    let a = "Hello, World!"
    let b str = """
        This is a 
        multi-line string.
        """
end
```

##### The Unit Type

The unit type `()` is a type with only one value, also called `()`. It basically
means "nothing" or "no value".

It cannot be instantiated and it cannot be assigned to a variable.

```klar
fn main():
    let a = ()  -- Compile error: Cannot assign the unit value
end
```

It is also the default return type of functions.

```klar
fn test():
    -- This function returns the unit value.
    return
end

fn main():
    test()
end
```

##### The Array Type

```klar
fn main():
    let a = [1, 2, 3]
    let b = a[1]
end
```

#### Compound Types

##### Tuples

```klar
fn main():
    -- Definition of a tuple.
    let tuple = (1, 2, 3)

    -- Accessing element by index.
    let first = tuple.0
    assert(first == 1)
    assert(tuple.1 == 2)
    assert(tuple.2 == 3)
end
```

##### Vector

```klar
fn main():
    mut vector = Vector<i32>.new()
    vector.push(1)
    vector.push(2)
    vector.push(3)
    assert(vector.len() == 3)

    -- Accessing elements by index.
    let second = vector[1]
    assert(second == 2)

    -- Setting elements by index.
    vector[1] = 42
    assert(vector[1] == 42)

    -- Accessing an index out of bounds results in a panic.
    -- TODO: We should not have to add a `return` so that the type becomes `()`.
    assert_panic(fn(): vector[3] return end)
end
```

### Error handling

In Klar, errors are values and may be returned from functions. 
The shorthand syntax looks like this:

fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0:
        throw Error.new("Division by zero")
    end
    return divisor / dividend
end

-- The built-in `Error` type looks like this:
struct Error<T=()>:
    message str
    data T
end

This is syntactic sugar for the following:

```klar
fn divide(divisor i32, dividend i32) Result<i32>:
    if dividend == 0:
        return Result<i32>.Err(Error.from_str("Division by zero"))
    end
    return Result<i32>.Ok(divisor / dividend)
end

--- 
-- The built-in `Result` type looks like this:
enum Result<T, E=()>:
    Ok(T)
    Err(Error<E>)
end
---
```
