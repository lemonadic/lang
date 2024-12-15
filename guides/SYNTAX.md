# Language Syntax Guide

## Imports
`import` statements are used to include external modules or libraries into your program.

```rust
import lang.io
import lang.async
```

## Open
`open` adds all definitions or some specific ones from a module into the current namespace to avoid repetitive references.

```rust
open lang.io
open lang.collections {List, Vector}
```

## Type Aliases
You can define type aliases for existing types to make code more readable or abstract.
Warning: Not sure if i'll keep it because in lean we have `abbrev` and `def`, `def` does not change with reductions too much, but `abbrev` just changes in the first chance it gets.

```rust
pub type Id := Int
```

## Algebraic Data Types (ADTs)
Two primary forms of ADTs are **product types** and **sum types**.

### Product Types
These represent combinations of fields (like records or structs).

```rust
type User := {
    name : String,
    kind : Kind,
    id : Id
}
```

### Sum Types
These represent choices between variants (like enums).

```rust
type Result(T, U) :=
  | Ok(T)
  | Err(U)
```

### Indexed Types
Indexed types let you define types that depend on a parameter.

```rust
type Term : Nat -> Type :=
  | Zero              : Term(0)
  | Succ(t : Term(n)) : Term(n + 1)
```

## Functions
Functions in dependently typed languages can depend on values, making them more expressive and allowing types to reflect the function's behavior based on arguments.

```rust
let add (x: Nat, y: Nat) : Nat := {
    x + y
}
```

Functions can also perform side-effects such as IO actions:

```rust
let add (x, y: Nat) : IO Nat := do {
    print("meoow");
    return x + y
}
```

You can also define functions with `let` and `fn`.

```rust
let add : Nat -> Nat -> Nat := fn(x, y) => x + y
```

Or with implicit arguments.

```rust
let addVec {x, y : Nat} (v1: Vector(x), v2: Vector(x2)) : Nat := {
    x + y
}
```

## Matching
Pattern matching allows you to destructure types, like sum types or product types, and handle them according to their form.

```rust
match expr {
  | .variant1 => 2
  | .variant2 => 3
  | _ => 0
}
```

### Pattern Matching with Guards
You can also use guards to further restrict the conditions.

```rust
match expr {
  | .variant1 if condition => 2
  | .variant2 => 3
  | _ => 0
}
```

## DO Notation
`do` notation allows for easier handling of monadic actions, such as those involving IO or asynchronous tasks.

```rust
do {
    let x <- 2;
    let y := 3;
    return x + y
}
```

You can chain multiple actions inside the `do` block, such as IO operations:

```rust
do {
    let result <- readFile("data.txt");
    let parsed <- parseData(result);
    return parsed
}
```

We can lift some operations with `<-` like this


```rust
do {
    if <- readFile("data.txt") == "err" {
        return false
    } else {
        return true
    }
}
```

## If Expression
Conditional expressions can also be written as:

```rust
let a := if condition { 1 } else { 2 }
```

You can also use `if` in a `do` block:

```rust
do {
    if condition {
        return "yes"
    } else {
        return "no"
    }
}
```

## Comments
Comments can be written using `//` for single-line comments and `/* */` for multi-line comments.

```rust
// This is a single-line comment

/*
This is a
multi-line comment
*/
```

## Let Expression
`let` is used to bind values to variables.

```rust
let x := 5;
let y := x + 10;
```

You can also destructure types in the `let` expression:

```rust
let User(name, id) := user;
```

## Optional Parameters and Named Parameters
You can define functions with optional parameters or named parameters.

### Optional Parameters

```rust
let greet(name : String, greeting : String := "Hello") : String := {
    return greeting + " " + name
}
```

Parameters are named by default, you can change them by changing the call syntax
```rust
greet(name: "Sofia")
```

## External Definitions
You can import external modules or define external types that are implemented in another language.

```rust
// Defines an opaque type
opaque Mutex : Type

// Links external function for multiple backends
external createMutex : IO Mutex := "llvm: something_dumb, js: something_in_js"

// IO function that is called before the program begins.
initialize onStart
```

This is useful when integrating with other languages or libraries, such as low-level system operations or external services.

## Examples

```rust
type Result(T, U) =
  | Ok(T)
  | Err(U)

let process_file(file : String) : IO(Result(Data, String)) := do {
    if <- file_exists(file) {
        return Ok(read_file(file))
    } else {
        return Err("File not found")
    }
}
```

```rust
type Vector (t : Type) : Nat -> Type =
  | Nil                            : Vector(t, 0)     // A vector of size 0
  | Cons(x : t, xs : Vector(t, n)) : Vector(t, n + 1) // A vector of size n+1

let appendVectors {n1, n2 : Nat} (v1 : Vector(n1), v2 : Vector(n2)) : Vector(n1 + n2) := {
    match v1 {
      | .Nil => v2
      | .Cons(x, xs) => .Cons(x, appendVectors(xs, v2))
    }
}
```

# Equality

In dependently typed languages, equality can be expressed in different forms. There are three common kinds of equality:

- `Definitional Equality (:=)`: Direct, no proof needed. Example: `a := 2`.
- `Propositional Equality (=)`: Equality as a proposition, requiring a proof. Example: `1 + 1 = 2` (proven with rfl).
- `Boolean Equality (==)`: Runtime check that returns true or false. Example: 1 + 1 == 2 (evaluates to true).

```rust
// Definitional equality (`:=`):
// Two terms are equal because they are literally the same (no proof needed).
let idk := 3

// Propositional equality (`=`):
// Proving that two terms are equal. Requires a proof.
let eq : 1 + 1 = 2 =
    rfl  // Simple proof: 1 + 1 is definitionally equal to 2

// Boolean equality (`==`):
// Checks if two terms are equal at runtime, returning a boolean.
let isEqual : Bool = 
    1 + 1 == 2  // Evaluates to true
```