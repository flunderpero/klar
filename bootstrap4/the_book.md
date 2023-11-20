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

### Built-in Data Types

#### Integer Types

```klar

fn main():
    let a = 2

    assert(a == 2)
end
```

#### The Boolean Type

```klar
fn main():
    let a = true
    let b bool = false
end
```

#### Tuples

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

#### Vector

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
    assert_panic(fn() => vector[3])
end
```

Just demonstrating a compile error.
```klar
fn main():
    unknown_function() -- Compile error: Identifier `unknown_function` not found
end
```
