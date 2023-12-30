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

##### The Character Type

A character is a single [Unicode](https://www.unicode.org/glossary/#unicode_scalar_value) scalar value.

```klar
fn main():
    let a = 'a'
    let b char = '\n' -- Escape sequences are supported.
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
    -- Concatenate string and characters.
    mut s = "".push(a).push_char('\n').push("This is Klar.")
    assert(s == "Hello, World!\nThis is Klar.")

    -- Iterate over the characters of a string.
    mut iter = s.iter()
    assert(iter.next().unwrap() == 'H')
    assert(iter.next().unwrap() == 'e')
end
```

##### Interpolated Strings

Klar lets you interpolate variables into strings. The syntax is `f"..."`.

```klar
fn main():
    let a = 1
    let b = true
    let c = "Hello"
    let interpolated = f"a = {a}, b = {b}, c = {c}"
    assert(interpolated == "a = 1, b = true, c = Hello")
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

##### The `Option` Type

The `Option` type is a generic type that can either be `Some` value or `None`.

```klar
fn main():
    let a = Option.Some(42)
    let b = Option.None

    -- Use a match expression to get the value.
    let value = match a:
        -- Enum variants are automatically imported into the current scope.
        -- So we can just use `Some` and `None` here.
        Some(value) => value
        None => panic("Should not be reached")
    end
end
```

The idiomatic way of using `Option` is to append a `?` to the type.
Within functions you don't need to wrap the return value in `Option` anymore.

```klar
fn divide(divisor i32, dividend i32?) i32?:
    if dividend.is_some():
        if (dividend.unwrap() == 0):
            return None
        end
        return divisor / dividend.unwrap()
    end
    None
end

fn main():
    assert(divide(10, 2).is_some())
end
```

#### Generic Types

##### Trait Bounds

```klar
struct Planet:
    name str
end 

trait HasId:
    fn id(self) str
end

impl HasId for Planet:
    fn id(self) str => self.name
end

struct CelestialBody<T impl HasId>:
    body T
end

impl ToStr for CelestialBody<T>:
    -- We can use the trait function `HasId.id` here, because we
    -- know that `self.body` is of a type that implements `HasId`.
    fn to_str(self) str => self.body.id()
end

-- You can have multiple trait bounds. This means that a type
-- has to implement _all_ traits.
fn combine_id_and_to_str<T impl HasId and ToStr>(obj T) str:
    let obj_id = obj.id()
    let obj_str = obj.to_str()
    f"{obj_id} + {obj_str}"
end

impl ToStr for Planet:
    fn to_str(self) str => f"Planet {self.name}"
end

fn main():
    let planet = Planet{name: "Earth"}
    let body = CelestialBody{body: planet}
    assert(body.to_str() == "Earth")
    -- todo (lang-feat): We should not need to specify the type `<Planet>` here.
    assert(combine_id_and_to_str(planet) == "Earth + Planet Earth")
end
```

Traits themselves can have trait bounds.

```klar
trait HasId impl ToStr:
    fn id(self) str
end

struct Planet:
    name str
end

-- Now `Planet` has to implement `ToStr` as well.
impl ToStr for Planet:
    fn to_str(self) str => f"Planet {self.name}"
end

impl HasId for Planet:
    fn id(self) str => self.name
end

fn main():
    let planet = Planet{name: "Earth"}
    let id = planet.id()
    assert(planet.id() == "Earth")
    assert(planet.to_str() == "Planet Earth")
end
```

#### Compound Types

##### Structs

```klar
struct Planet:
    name str
    circumference i32
end

fn main():
    let name = "Earth"
    let earth = Planet{name, circumference: 40075}
    assert(earth.name == "Earth")
    assert(earth.circumference == 40075)
end
```

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
    assert_panic(fn() => vector[3];)
end
```

##### Map

```klar
fn main():
    mut map = Map<str, i32>.new()
    map["one"] = 1  -- Idiomatic way of setting elements.
    map.set("two", 2) -- Alternative way of setting elements.
    map["three"] = 3
    assert(map.len() == 3)

    -- Accessing elements by key.
    let two = map["two"] -- Idiomatic way of getting elements.
    -- `two` is of type `Option<i32>`.
    assert(two.is_some())

    let four = map.get("four") -- Alternative way of getting elements.
    assert(four.is_none())
end
```

### Expressions and Statements

In Klar, almost everything is an expression. Expressions return a value.

In contrast, statements do not return a value and have the unit type `()`.

Statements are:

- `let` declarations

```klar
    let a = 42
```

- Assignments

```klar
    mut a = 42
    let b = (a = 43) -- Compile error: Cannot assign the unit value
```

- Struct, trait, function declarations, and implementation blocks:

```klar
    struct Planet:
        mass i32
        circumference i32
        name str
    end

    trait Orbit:
        fn mean_distance(self) i32
    end

    impl Orbit for Planet:
        fn mean_distance(self) i32:
            self.circumference / 2
        end
    end

    fn calculate_radius(planet Planet) i32:
        planet.circumference / 2
    end
```

- Loops, break and continue

```klar
fn main():
    mut i = 0
    loop:
        if i == 10:
            break
        end
        i = i + 1
        if i == 5:
            continue
        end
    end
end
```

#### The Unit Operator `;`

Sometimes you need to convert an expression to a statement like in
this example:

```klar
fn apply(func (fn(i32))):
    func(42)
end

fn main():
    -- The following would not compile, because `x + 1` is an expression
    -- of the type `i32` but `apply` expects a closure that returns
    -- the unit type `()`.
    --   apply(fn(x) => x + 1)

    -- Using the unit operator `;` we can convert the expression to a
    -- statement. Statements always have the unit type `()`.
    apply(fn(x) => x + 1;)
end
```

### Error handling

In Klar, errors are values and may be returned from functions.

Errors must implement the `Error` trait which only requires 
to implement the `ToStr` trait.

The canonical way of error handling is as follows:

```klar
fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0:
        return Error("division by zero")
    end
    divisor / dividend
end

fn main():
    let result = match divide(10, 2):
        Ok(value) => value
        Error(error) => panic("should not be reached")
    end
    assert(result == 5)

    -- Or using the `is_ok()` and `is_err()` functions.
    assert(divide(10, 0).is_error())
    assert(divide(10, 2).is_ok())

    -- Or just `unwrap()` which will cause a runtime panic if
    -- the result is `Result.Error`.
    assert(divide(10, 2).unwrap() == 5)
    assert_panic(fn() => divide(10, 0).unwrap();)
    -- The opposite of `unwrap()` is `unwrap_error()` which panics
    -- if the result is `Result.Ok`.
    assert(divide(10, 0).unwrap_error().to_str() == "division by zero")
    assert_panic(fn() => divide(10, 2).unwrap_error();)
end
```

Note: This is syntactic sugar for this code:

```klar
fn divide(divisor i32, dividend i32) Result<i32, str>:
    if dividend == 0:
        return Result<i32, str>.Error("division by zero")
    end
    Result<i32, str>.Ok(divisor / dividend)
end
```

Always write idiomatic error handling using `throws`. `klarfmt` will
always transform your code to the idiomatic form.

#### Error Propagation

Errors can be propagated using the `!` operator.

```klar
fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0 => return Error("division by zero")
    divisor / dividend
end

fn number_of_orbits(total_time i32, orbit_time i32) i32 throws:
    --- Staying with the space theme, we calculate the number of orbits
        of an object in a given time.
    ---
    let orbits = divide(total_time, orbit_time)!  -- Propagate the error.

    -- The type of `orbits` is `i32` here, the `!` operator automatically
    -- unwraps the `Result` type.
    if orbits < 0 => return Error("negative number of orbits")
    orbits
end

fn main():
    assert(number_of_orbits(10, 2).unwrap() == 5)
    assert(number_of_orbits(10, 0).is_error())
end
```

#### Errors Must Be Handled

```klar 
fn divide(divisor i32, dividend i32) i32 throws:
    if dividend == 0 => return Error("division by zero")
    divisor / dividend
end

fn main():
    divide(10, 0) -- Compile error: The function being called might return an error
    panic("expected a compile error")
end
```

#### Custom Error Types

```klar
struct MyError:
    code i32
end

impl ToStr for MyError:
    fn to_str(self) str => f"MyError {self.code}"
end

fn provoke_my_error() throws MyError:
    return Error(MyError{code: 42})
end

fn propagate_my_error() throws:
    -- The `!` operator propagates the error.
    -- Even though `provoke_my_error()` throws a `MyError` we can 
    -- propagate it as the default error type, because the default
    -- type is the trait `ToStr` which must be implemented by every
    -- error type.
    provoke_my_error()!
end

fn main():
    assert(provoke_my_error().unwrap_error().code == 42)
    -- All errors can be coerced to the default error type which
    -- only implements the `ToStr` trait.
    assert(propagate_my_error().unwrap_error().to_str() == "MyError 42")
end
```

#### Runtime Errors with `panic`

```klar
fn main():
    assert_panic(fn() => panic("This is a runtime error");)
end
```

You can register a panic handler with `register_panic_handler`. It will be called
when a panic occurs.

```klar
fn main():
    mut panic_count = 0
    let old_handler = register_panic_handler(fn(message, location, src): 
        panic_count = panic_count + 1
    end)
    panic("This is a runtime error")
    assert(panic_count == 1)
    -- Restore the old panic handler.
    register_panic_handler(old_handler);
end
```


### Pattern Matching

```klar
enum Planet:
    Earth(i32)
    Mars
    Venus
end

fn main():
    let planet = Earth(15) -- Remember: Enum variants are visible automatically.
    let name = match planet:
        -- Capture the value of the `Earth` variant.
        Earth(average_temp) => f"Earth (average temp: {average_temp})"
        Mars => "Mars"
        -- `_` matches everything.
        _ => "Unknown"
    end
    assert(name == "Earth (average temp: 15)")
end
```

Patterns must be exhaustive. This means that you have to match all possible
values of the match expression.

```klar
enum Planet:
    Earth
    Mars
    Venus
end

fn main():
    let planet = Earth
    let name = match planet: -- Compile error: Match is not exhaustive
        Earth => "Earth"
        Mars => "Mars"
    end
end
```

#### Single Pattern Matches With `if let`

Sometimes you just want to match a single pattern. In this case you can use
the `if let` expression.

```klar
fn main():
    let a = Some(42)
    if let Some(value) = a:
        assert(value == 42)
    else:
        panic("should not be reached")
    end

    -- `if let` is a regular expression.
    let b = if let Some(value) = a:
        value
    else:
        panic("should not be reached")
    end
    assert(b == 42)
end
```

#### Range Patterns

Match a numeric range:
```klar
fn main():
    let result = match 42:
        0..39 => "0..39"    -- By default, ranges are closed.
        40..<42 => "40..41" -- Use `..<` for an half-open range.
        _ => "unknown"
    end
    assert(result == "unknown")
end
```

```klar 
fn main():
    let result = match 'c':
        'a' => "a"          -- Match a single character.
        'a'..'z' => "range" -- Match a range of characters.
        _ => "unknown"
    end
    assert(result == "range")
end
```

#### Alternative Patterns

```klar
fn main():
    let result = match 42:
        0 | 1 | 2 => "0, 1, or 2"
        31 | 42 | 53 => "31, 42, or 53"
        _ => "unknown"
    end
    assert(result == "31, 42, or 53")
end
```

#### Nested Patterns

```klar
struct Planet:
    name str
end

fn main():
    let planet = Some(Planet{name: "Earth"})
    let result = match planet:
        Some(Planet{name: "Earth" | "Venus" | "Mars" | "Mercury"}) => "inner planet"
        Some(Planet{name}) => name
        _ => "unknown"
    end
    assert(result == "inner planet")
end
```

### Imports

#### Modules

In Klar, modules are files. The name of the module is the name of the file.

This is how you import a type from a local module:

```klar
use .the_book_use_example.SomeStruct
use .the_book_use_example.SomeEnum

fn main():
    let s = SomeStruct.new(1)
    assert(s.value == 1)
    s.increment()
    assert(s.value == 2)

    let e = SomeEnum.SomeA
    assert(e.to_str() == "some a")
end
```

#### Importing Enum Variants

```klar
enum Planets:
    Earth
    Mars
    Venus
end

fn main():
    -- Import `Planets.Earth` into the current scope, so we can refer to 
    -- it as `Earth`.
    use Planets.Earth
    let earth = Earth

    -- Import all variants of `Planets` into the current scope.
    use Planets.*
    let mars = Mars
end
```

## Environment

### Arguments

```klar
fn main():
    let args = env.args
    assert(args.len() > 0)
end
```
