# Klar - The Official Guide

This is the living documentation and spec of Klar.

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

Documentation comments are multi-line comments.
Put these _inside_ of block elements like `fn`, `struct`, or `trait`
or right above single line declarations like fields.

```klar

struct Planet:
    --- A planet is a body circumventing a star.
        Multi-line comments are indented by 4 spaces.
    ---

    --- The mass in kg.
    ---
    mass i32 -- TODO: change to u64

    --- The circumference in km.
    ---
    circumference i32

    -- This is a regular comment and not a documentation comment.
    name str
end

trait Orbit:
    --- An orbit describes the path around a bigger mass object.
    ---

    --- This is just a declaration, so we document above it.
    ---
    fn mean_distance(self) i32
end

fn calculate_radius(planet Planet) i32:
    --- Calculate the radius of a planet.
    ---
    planet.circumference / 2
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
    --- This function returns the unit value.
    ---
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

The canonical way of error handling is as follows:

```klar
fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0:
        return Error.from_str("Division by zero")
    end
    divisor / dividend
end

fn main():
    let result = match divide(10, 2):
        Result<i32>.Ok(value) => value
        Result<i32>.Err(error) => panic("Should not be reached")
    end
    assert(result == 5)

    -- Or using the `is_ok()` and `is_err()` functions.
    assert(divide(10, 0).is_err())
    assert(divide(10, 2).is_ok())

    -- Or just `unwrap()` which will cause a runtime panic if
    -- the result is `Result.Err`.
    assert_panic(fn(): divide(10, 0).unwrap() return end)
end
```

Note: This is syntactic sugar for this code:

```klar
fn divide(divisor i32, dividend i32) Result<i32>:
    if dividend == 0:
        return Result<i32>.Err(Error.from_str("Division by zero"))
    end
    Result<i32>.Ok(divisor / dividend)
end
```

Always write idiomatic error handling using `throws`. `klarfmt` will
always transform your code to the idiomatic form.

#### Error Propagation

Errors can be propagated using the `!` operator.

```klar
fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0 => return Error.from_str("Division by zero")
    divisor / dividend
end

fn number_of_orbits(total_time i32, orbit_time i32) i32 throws:
    --- Staying with the space theme, we calculate the number of orbits
        of an object in a given time.
    ---
    let orbits = divide(total_time, orbit_time)!  -- Propagate the error.

    -- The type of `orbits` is `i32` here, the `!` operator automatically
    -- unwraps the `Result` type.
    if orbits < 0 => return Error.from_str("Negative number of orbits")
    orbits
end

fn main():
    assert(number_of_orbits(10, 2).unwrap() == 5)
    assert(number_of_orbits(10, 0).is_err())
end
```
