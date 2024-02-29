# Klar - The Official Guide

This is the living documentation and spec of Klar.

## Goals

Klar should be - in no particular order - the following:

### Simple And - As The Name Says - Clear

The language should be easy to learn and use. It should be expressive but not
overly complex. The type system should be powerful but not overwhelming. Working
with errors and optional values should be idiomatic and easy.

### Memory Safe

Klar will use an optimized reference counting garbage collector. With the help
of static analysis, many use cases should not require runtime garbage collection.

#### Cyclic References

Resolving cyclic references is a challenge for reference counting garbage collectors.
Klar will not go the route of other reference counting garbage collectors (like Python)
and use a second garbage collector (like a mark-and-sweep collector) for that. And unlike
Swift, Klar will not leave the burden on the programmer to break or prevent cyclic 
references. Klar will first determine through static analysis if a reference may be
part of a cycle and then add the proper logic to break that specific cycle. 
It remains to be seen if this is feasible.

#### Explicit Mutability

Klar will not have implicit mutability. This means that you have to declare a variable 
or a parameter as mutable. This improves the readability of the code and improves static
analysis.

#### References By Default And No Implicit Copying

By default, everything except for primitive types (number and boolean types) will be 
references. Value types (especially in structs) might be introduced later. It should
always be clear if a value is copied or referenced.

#### No Null References

Klar will not have null references. Instead, it will have an `Option` type that is
idiomatic to use.

### Easy To Write Secure And Fast Concurrent Code

Klar will have a powerful concurrency model that lends itself from Go's principle: 

Share memory by communicating, don't communicate by sharing memory. 

The developer should be able to write asynchronous code without the need for callbacks,
promises, or async/await. There will be no "colored functions" - i.e. types of functions
that cannot be called from other types of functions - like in other languages 
that support concurrent programming with async/await.

Working with threads should be easy and safe. The language should be able

### Suitable For Application And Web Development

Klar should compile to native code. WebAssembly support is also a first-class citizen.

### Fast To Compile

This one goes without saying.

### Fast To Execute

The goal is to be at most 20% slower than C in most scenarios. This is a very ambitious 
goal and it remains to be seen if this is feasible. 
It should be faster than JavaScript and Java, though.

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
    mass Int -- TODO: change to U64

    --- The circumference in km.
    ---
    circumference Int

    -- This is a regular comment and not a documentation comment.
    name Str
end

trait Orbit:
    --- An orbit describes the path around a bigger mass object.
    ---

    --- This is just a declaration, so we document above it.
    ---
    fn mean_distance(self) Int
end

fn calculate_radius(planet Planet) Int:
    --- Calculate the radius of a planet.
    ---
    planet.circumference / 2
end
```

### Identifiers

Rules for identifiers:

- Must start with a letter and must not start with an underscore.
- Can contain letters, numbers, and underscores.
- Must not be a keyword.
- Type identifiers must start with an uppercase letter.
- Function, variable, and field identifiers must start with a lowercase letter.

### Types

#### Built-in Types

##### Number types

Unsigned integers: `UInt`, `U8`, `U16`, `U32`, `U64`
Signed integers: `Int`, `I8`, `I16`, `I32`, `I64`
Floating point numbers: `F32`, `F64`

`Int` and `UInt` are aliases for the platform dependent integer types.
On 32-bit platforms they are 32-bit integers, on 64-bit platforms they are 64-bit integers.
Unless you need a specific integer type, you should always use `Int` and `UInt`. This
reduces the need for type conversions and allows the compiler to optimize your code better.

```klar
fn main():
    let a = 2
    let b Int = 2

    assert(a == b)
end
```

##### The Boolean Type

```klar
fn main():
    let a = true
    let b Bool = false
end
```

##### The Character Type

A character is a single [Unicode](https://www.unicode.org/glossary/#unicode_scalar_value) scalar value.

```klar
fn main():
    let a = 'a'
    let b Char = '\n' -- Escape sequences are supported.
end
```
<details>
<summary>More Examples</summary>

###### Escape Sequences

```klar
fn main():
    assert('\x41' == 'A') -- Hexadecimal escape sequence
    assert('\u0041' == 'A') -- Unicode escape sequence
    assert('\0' == '\u0000') -- Null character
    assert('\t' == '\u0009') -- Horizontal tab
    assert('\n' == '\u000a') -- Line feed
    assert('\r' == '\u000d') -- Carriage return
    assert('\'' == '\u0027') -- Single quote
    assert('\\' == '\u005c') -- Backslash
end
```

</details>

##### The String Type

```klar
fn main():
    let a = "Hello, World!"
    let b Str = """
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

<details>
<summary>More Examples</summary>

###### Escape Sequences

```klar
fn main():
    assert("\x41" == "A") -- Hexadecimal escape sequence
    assert("\u0041" == "A") -- Unicode escape sequence
    assert("\0" == "\u0000") -- Null character
    assert("\t" == "\u0009") -- Horizontal tab
    assert("\n" == "\u000a") -- Line feed
    assert("\r" == "\u000d") -- Carriage return
    assert("\"" == "\u0022") -- Double quote
    assert("\\" == "\u005c") -- Backslash
end
```

</details>

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
    return ()
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
    let a = Option::Some(42)
    let b = Option::None

    -- Use a match expression to get the value.
    let value = match a:
        Option::Some(value) => value
        Option::None => panic("Should not be reached")
    end
end
```

The idiomatic way of using `Option` is to append a `?` to the type.
Within functions you don't need to wrap the return value in `Option` anymore.

```klar
fn divide(divisor Int, dividend Int?) Int?:
    if dividend.is_some():
        if (dividend.unwrap() == 0):
            return None
        end
        -- This is the same as `Some(divisor / dividend.unwrap())`.
        return divisor / dividend.unwrap()
    end
    None
end

fn main():
    assert(divide(10, 2).is_some())
end
```

#### Compound Types

##### Structs

```klar
struct Planet:
    name Str
    circumference Int
end

fn main():
    let name = "Earth"
    let earth = Planet{name, circumference: 40075}
    assert(earth.name == "Earth")
    assert(earth.circumference == 40075)
end
```

##### Enums

###### Importing Enum Variants

```klar
enum Planets:
    Earth
    Mars
    Venus
end

-- Bring `Planets.Earth` into the current scope, so we can refer to
-- it as `Earth`.
use Planets::Earth

fn main():
    let earth = Earth
    let mars = Mars -- Compile error: Unknown `Mars`
end
```

You can also import all enum variants:

```klar
enum Planets:
    Earth
    Mars
    Venus
end

use Planets::*

fn main():
    let earth = Earth
    let mars = Mars
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
    mut vector = Vector<Int>::new()
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
    mut map = Map<Str, Int>::new()
    map["one"] = 1  -- Idiomatic way of setting elements.
    map.set("two", 2) -- Alternative way of setting elements.
    map["three"] = 3
    assert(map.len() == 3)

    -- Accessing elements by key.
    let two = map["two"] -- Idiomatic way of getting elements.
    -- `two` is of type `Option<Int>`.
    assert(two.is_some())

    let four = map.get("four") -- Alternative way of getting elements.
    assert(four.is_none())
end
```

#### Generic Types

##### Trait Bounds

```klar
struct Planet:
    name Str
end

trait HasId:
    fn id(self) Str
end

impl HasId for Planet:
    fn id(self) Str => self.name
end

struct CelestialBody<T impl HasId>:
    body T
end

impl ToStr for CelestialBody<T>:
    -- We can use the trait function `HasId.id` here, because we
    -- know that `self.body` is of a type that implements `HasId`.
    fn to_str(self) Str => self.body.id()
end

-- You can have multiple trait bounds. This means that a type
-- has to implement _all_ traits.
fn combine_id_and_to_str<T impl HasId and ToStr>(obj T) Str:
    let obj_id = obj.id()
    let obj_str = obj.to_str()
    f"{obj_id} + {obj_str}"
end

impl ToStr for Planet:
    fn to_str(self) Str => f"Planet {self.name}"
end

fn main():
    let planet = Planet{name: "Earth"}
    let body = CelestialBody{body: planet}
    assert(body.to_str() == "Earth")
    assert(combine_id_and_to_str(planet) == "Earth + Planet Earth")
end
```

Traits themselves can have trait bounds.

```klar
trait HasId impl ToStr:
    fn id(self) Str
end

struct Planet:
    name Str
end

-- Now `Planet` has to implement `ToStr` as well.
impl ToStr for Planet:
    fn to_str(self) Str => f"Planet {self.name}"
end

impl HasId for Planet:
    fn id(self) Str => self.name
end

fn main():
    let planet = Planet{name: "Earth"}
    let id = planet.id()
    assert(planet.id() == "Earth")
    assert(planet.to_str() == "Planet Earth")
end
```

### Expressions And Statements

In Klar, almost everything is an expression. Expressions return a value.

In contrast, statements do not return a value and have the unit type `()`.

Statements are:

- `let` declarations

```klar
fn main():
    let a = 42
end
```

- Assignments

```klar
fn main():
    mut a = 42
    let b = (a = 43) -- Compile error: Cannot assign the unit value
end
```

- Struct, trait, function declarations, and implementation blocks:

```klar
    struct Planet:
        mass Int
        circumference Int
        name Str
    end

    trait Orbit:
        fn mean_distance(self) Int
    end

    impl Orbit for Planet:
        fn mean_distance(self) Int:
            self.circumference / 2
        end
    end

    fn calculate_radius(planet Planet) Int:
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

#### Let Declarations

```klar
fn main():
    let a = 42  -- Implicit type inference
    let b Int = 42  -- Explicit type annotation
    mut s = "Hello" -- Mutable variable
end
```

#### The Unit Operator `;`

Sometimes you need to convert an expression to a statement like in
this example:

```klar
fn apply(func (fn(Int))):
    func(42)
end

fn main():
    -- The following would not compile, because `x + 1` is an expression
    -- of the type `Int` but `apply` expects a closure that returns
    -- the unit type `()`.
    --   apply(fn(x) => x + 1)

    -- Using the unit operator `;` we can convert the expression to a
    -- statement. Statements always have the unit type `()`.
    apply(fn(x) => x + 1;)
end
```

#### Unary Operators

```klar
fn main():
    let a = false
    assert(not a)
    -- `not` has lower precedence than `and` and `or`.
    assert((not a and 42 < 23) == false)
end
```

### Functions

#### Return Values

In Klar, the last expression of a function is the return value.

```klar
fn add(a Int, b Int) Int:
    a + b -- This is the return value.
end

fn main():
    assert(add(1, 2) == 3)
end
```

##### Explicit And Early Returns

```klar
fn divide(divisor Int, dividend Int) Int:
    if dividend == 0:
        return 0 -- Early return.
    end
    return divisor / dividend -- Explicit return.
end

fn main():
    assert(divide(10, 2) == 5)
    assert(divide(10, 0) == 0)
end
```

The `return` keyword must be followed by an expression.

```klar
fn main():
    return 
end -- Compile error: Unexpected token `end`
```

The correct code is:

```klar
fn main():
    return ()
end
```

### Error handling

In Klar, errors are values and may be returned from functions.

Errors must implement the `Error` trait which only requires
to implement the `ToStr` trait.

The canonical way of error handling is as follows:

```klar
fn divide(divisor Int, dividend Int) Int throws:
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
fn divide(divisor Int, dividend Int) Result<Int, Str>:
    if dividend == 0:
        return Result<Int, Str>::Error("division by zero")
    end
    Result<Int, Str>::Ok(divisor / dividend)
end
```

Always write idiomatic error handling using `throws`. `klarfmt` will
always transform your code to the idiomatic form.

#### Error Propagation

Errors can be propagated using the `!` operator.

```klar
fn divide(divisor Int, dividend Int) Int throws:
    if dividend == 0 => return Error("division by zero")
    divisor / dividend
end

fn number_of_orbits(total_time Int, orbit_time Int) Int throws:
    --- Staying with the space theme, we calculate the number of orbits
        of an object in a given time.
    ---
    let orbits = divide(total_time, orbit_time)!  -- Propagate the error.

    -- The type of `orbits` is `Int` here, the `!` operator automatically
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
fn divide(divisor Int, dividend Int) Int throws:
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
    code Int
end

impl ToStr for MyError:
    fn to_str(self) Str => f"MyError {self.code}"
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
    Earth(Int)
    Mars
    Venus
end

use Planet::* -- Bring all enum variants into the current scope.

fn main():
    let planet = Earth(15) 
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

use Planet::* -- Bring all enum variants into the current scope.

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

TODO: This does not work as expected yet (variable captures are buggy). We are still
      contemplating if we should keep this feature.
```
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
    name Str
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
use .the_book_use_example::SomeStruct
use .the_book_use_example::SomeEnum

fn main():
    let s = SomeStruct::new(1)
    assert(s.value == 1)
    s.increment()
    assert(s.value == 2)

    let e = SomeEnum::SomeA
    assert(e.to_str() == "some a")
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

## Known Issues

### `if / else` Can Be Ambiguous

```
if a:
    if b => 42
else:
    23
end
```

This is ambiguous. The `else` could belong to the inner `if` or the outer `if`. For
now, you have to use the multi-line block syntax to disambiguate:

```
if a:
    if b:
        42
    else:
        23
    end
else:
    23
end
```

## RFC

### Get Rid of `if let` And Propose Guard Clauses

The syntax is ugly. Most of the time you want to match a single pattern in a
guard clause. I don't know what the best course of action is here. For now, I
am completely fine with writing full match expressions.

### Introduce `..` For Concatenation (Union)

I don't really like to use `+` for string or container concatenation. Either we don't have that
at all or we should introduce other syntax. I propose `..` because it reads nice and Lua already
uses that.

```
trait Concatenate<T>:
    fn concatenate(self, lhs T, rhs T) Self
end

fn main():
    let s = "Hello" .. " world".
    let a = [1, 2] .. [3, 4]
end

```

### Get Rid of the `;` Operator

We only need it in contexts where an expression is returned from a function that 
returns the unit type `()`:

```
fn test():
    42;  -- The `;` is needed here because `test` returns the unit type `()`.
end
```

This is especially problematic in closures:

```
-- The `;` is needed here because `for_each` returns the unit type `()`.
my_iter.for_each(fn(x) => x + 1;)  
```

We could instead introduce the rule that expressions returned in unit-type-returning functions
are automatically converted to statements.

If there are other cases that need the conversion into a statement (i.e. having the 
unit type `()`) you can always use the unit literal `()`. The example above could also be
written as:

```
fn test():
    42
    ()  -- This is the unit literal.
end
```

#### Type Inference

This should not lead to problems with type inference because we require return types to be 
explicitly annotated in the function signature.
