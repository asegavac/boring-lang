# Boring Lang

The Boring Programming Language (Boring-Lang) is an attempt to create an easy, productive, general purpose programming language that makes as few interesting choices as possible while still being in line with modern concepts in programming languages.

The language:
* is compiled with a run-time (llvm for convenience + c/rust compatibility)
* has managed memory (via strong/weak pointers and automatic reference counting)
* uses async-await for all IO, with a built-in multi-core scheduler (tokio)
* supports algebraic data types (Result type for errors, Maybe/Optional type for nullables)
* supports parametric polymorphism (generics) with higher kinded types
* uses struct+traits, rather than classes or stuct+interfaces
* has a rich standard library (similar scale to python or go)
* is immutable by default
* is sandboxed by default

It's a middle-ground of Rust, Golang, Swift, Typescript, and Python. The goal is not to break any new ground in PL theory, or even create a language anyone likes, but rather to create a language with as few deal-breakers as possible for maximum day-to-day industrial programming ergonomics.

This language is under active development, progress will be marked here as the language is developed.

- [x] Functions
- [x] Int Literals
- [x] Float Literals
- [x] Block expression
- [x] Return keyword
- [x] Normal assignment
- [x] Structs
  - [x] Define
  - [x] Literal
  - [x] Getter
  - [x] Setter
- [x] Type Aliases
- [x] Methods
  - [x] Declaration
  - [x] Use
- [ ] Traits
- [ ] Generics
  - [ ] Basic
  - [ ] Higher kinded types
- [ ] Control Flow
  - [ ] If
  - [ ] While
  - [ ] For
- [ ] Enums
- [ ] Lambdas
- [ ] Async-Await
- [ ] Imports
- [ ] Visibility
- [ ] Const / Mut

This project is actively looking for contributors, so if you're interested in programming language design or have experience working with LLVM, don't hesitate to contact.


## Http Server Example

```rust
import net.http as http
import logging as logging
import json as json

type ExampleResponse struct {
  id: I32
  name: Str
  email: Str
}

async fn handle(req: http.Request, resp: mut http.Response): {
  let response_data = ExampleResponse{
    id: 4,
    name: "Andrew",
    email: "andrew@example.com"
  };
  await resp.set_status(200);
  await resp.write(json.encode[ExampleResponse](response_data));
}

async fn main(args: Vec[Str], os: OS): I32 {
    let log = logging.new_logger(os.fs());
    let router = http.Router("").add_route("/myroute", handle);
    let http_server = http.Server(os.net(), "localhost", 8080, router);
    let err = await http_server.serve_forever();
    await log.info("error serving: ", err);
    return 1;
}
```

## Mutability

All variables are immutable by default, to make them mutable use the `mut` keyword. Once a variable becomes immutable it cannot become mutable again. If you need it to become mutable, either implement the `clone` trait, or simply create a new one with the same data.

```rust
let mut foo = Dict[String, Int32].new(); // constructor returns a mutable reference
foo.insert("eggs", 12);
foo.insert("bananas", 2);
foo.insert("grapes", 2);

let bar = foo; // bar is not mutable

bar.insert("apples", 4); // fails with compiler error

let mut baz = bar.clone();
baz.insert("apples", 4); // fine
```

Methods on a struct must specify if they mutate the struct.

```rust
impl Dict[Key: Hashable, Value] {
  fn insert(self: mut Self, key: Key, value: Value) {
    // mutate self here
  }

  fn get(self: Self, key: Key) Optional[Value] {
    // no need for `mut`
  }
}
```

## Context

Context is an exceptionally useful feature in golang, but a common complaint is that:

1. Because it works as an arbitrary map, it can be used to pass arguments into a function that aren't explicitly stated.
2. It is used for both passing context parameters and cancellation, two fundamentally different tasks that have no reason to be in the same object.

The boring standard library solves this by using parametric polymorphism. Context is by default an empty object passed through the chain, and each function/set of context parameters is an additional trait condition applied at compile time.

```rust
type HTTPRequest[Ctx: Context] = async fn(Ctx, http.Request, mut http.Response);

pub fn tracing_middleware[Ctx: Tracing](handler: HTTPRequest[Ctx]): HTTPRequest {
  return async fn(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    with tracing.Span(ctx, "request_duration") {
      await handler(ctx, req, resp);
    }
  };
}

pub fn auth_middleware[Ctx: Auth](handler: HTTPRequest[Ctx], scope: Str): HTTPRequest {
  return async fn(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    if ctx.has_scope(scope) {
      await handler(ctx, req, resp);
    }
    await resp.set_status(403);
    await resp.write("missing scope");
  };
}

pub fn cancel_middleware[Ctx: Cancel](handler: HTTPRequest[Ctx]): HTTPRequest {
  return async fn(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    if !(await ctx.is_cancelled()) { // check cancel token
      await handler(ctx, req, resp);
    }
    await resp.set_status(400);
    await resp.write("cancelled");
  };
}
```

for the above examples, you would pass a context type that implements all three traits.

## Sandboxing

Unlike many other programming languages, boringlang's `main` function take in two arguments, a vector of command line arguments, and a reference to the OS which is the program's only link to the outside world. To open a file in boringlang, you cannot just open it anywhere, you *must* call `os.fs().open("path")`. All `os.whatever()` methods return an interface for interacting with that part of the OS, such as `fs`, `net`, `datetime`, and `syscall`. Because this is the only way to interact with the world outside of the program, this means that any IO the program does can be trivially mocked for testing, and that all operations the program can perform are sandboxed. If a function doesn't require a reference to the `FS` trait, you can be sure it doesn't interact with the file system.

## Import System

Similar to python, folders/files represent the `.` seperated import path, but relative imports are *not* supported. Exported values must be marked with `pub`. All imports take the form:

```rust
import package.path as local_name

pub type MyStruct struct {
  pub id: I32
}
```


## Basic Statements
### `if`

`if` is an expression in boring-lang, with the last expression in a block being the return value.

```rust
let a = if true {
  4
} else {
  2
}

// a == 4

```

Conditions do not require parenthesis and *must* evaluate to the Boolean type.

### Loops

Boring-lang supports `for` and `while` loops, with `for` having an `async` variant. `while` loops require an expression of Boolean type, while `for` loops require an expression that implements the `Iter` or `AIter` traits.

```rust
let mut i = 0;
while i < 100 {
  i = i + 1;
  // do something here
}


for i in range(100) {
  // do something here
}

async for result in paginated_list {
  // do something with result
}
```

`continue` and `break` work similar to other languages.

```rust
while true {
  break; // do nothing
}

for i in range(100) {
  continue; // do nothing
}

```

### `with`

`with` and `async with` blocks are similar to the python statement with the same name. But unlike the python version, `with` blocks are expressions. `with` blocks take in an expression that implements the `With` or `AWith` trait, and execute a block that *may* return a result (non-result returns are assumed success).

```rust
// commits on success, aborts on error.
// transation.aexit may just return an error as a pass-through after aborting,
// but it may also transform it into another error adding context.

return async with db.transation(ctx) as t {
    await t.insert(ctx, record); // returns result type
};
```

### `return`

`return` statements exit a function early, returning the given value. They are purely optional as the last expression in a function will automatically return its value.

### `match`

`match` expressions provide pattern matching, similar to a `C` switch statement.

```rust
let number = 3;
let result = match number {
  1 => 'foo',
  3 => 'bar',
  _ => 'baz',
};

// result = 'bar'
```
