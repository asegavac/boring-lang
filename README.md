# Boring Lang Proposal

**NOTE: This repo is a work in progress as I learn compiler writing, I may abandon this.**

The Boring Programming Language (Boring-Lang) is an attempt to create an easy, productive, general purpose programming language that makes as few interesting choices as possible while still being in line with modern concepts in programming languages.

The language (wish list):
* is compiled with a run-time (llvm for convenience + c/rust compatibility)
* is garbage collected
* uses async-await for all IO, with a built-in multi-core scheduler
* supports algebraic data types (Result type for errors, Maybe/Optional type for nullables)
* supports parametric polymorphism (generics)
* uses struct+traits, rather than classes or stuct+interfaces
* has a rich standard library (http server, actor model)
* is immutable by default

It's basically a middle-ground of Rust, Golang, Swift, and Typescript.

## Http Server Example
```
import net.http as http
import logging as log
import json as json

struct ExampleResponse {
  id: Int32
  name: Str
  email: Str
}

async func handle(req: http.Request, resp: mut http.Response) {
  log.info("request: ", req.body)
  let response_data := ExampleResponse{id: 4, name: "Steven", email: "swerbenjagermanjensen@example.com"}
  await resp.set_status(200)
  await resp.write(json.encode<ExampleResponse>(response_data))
}

async func main(args: Array<Str>) Int32 {
    let router := http.Router("").add_route("/myroute", handle)
    http_server := http.Server("localhost", 8080, router)
    let err := await http_server.server_forever()
    await log.info("error serving: ", err)
    return 1
}
```

## Mutability

All variables are immutable by default, to make them mutable use the `mut` keyword. Once a variable becomes immutable it cannot become mutable again. If you need it to become mutable, either implement the `clone` trait, or simply create a new one with the same data.

```
let mut foo = Dict<String, Int32>() // constructors always return a mutable reference
foo.insert("eggs", 12)
foo.insert("bananas", 2)
foo.insert("grapes", 2)

let bar = foo // bar is not mutable

bar.insert("apples", 4) // fails with compiler error

let mut baz = bar.clone()
baz.insert("apples", 4) // fine
```

Methods on a struct must specify if they mutate the struct.

```
impl Dict<Key: Hashable, Value> {
  func insert(self: mut Dict, key: Key, value: Value) {
    // mutate self here
  }

  func get(self: Dict, key: Key) Optional<Value> {
    // no need for `mut`
  }
}
```

## Context

Context is an exceptionally useful feature in golang, but a common complaint is that:

1. Because it works as an arbitrary map, it can be used to pass arguments into a function that aren't explicitly stated.
2. It is used for both passing context parameters and cancellation, two fundamentally different tasks that have no reason to be in the same object.

The boring standard library solves this by using parametric polymorphism. Context is by default an empty object passed through the chain, and each function/set of context parameters is an additional trait condition applied at compile time.

```
type HTTPRequest<Ctx: Context> = async func(Ctx, http.Request, mut http.Response)

pub func tracing_middleware<Ctx: Tracing>(handler: HTTPRequest<Ctx>) HTTPRequest {
  return async func(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    with tracing.Span(ctx, "request_duration") {
      await handler(ctx, req, resp)
    }
  }
}

pub func auth_middleware<Ctx: Auth>(handler: HTTPRequest<Ctx>, scope: Str) HTTPRequest {
  return async func(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    if ctx.has_scope(scope) {
      await handler(ctx, req, resp)
    }
    await resp.set_status(403)
    await resp.write("missing scope")
  }
}

pub func cancel_middleware<Ctx: Cancel>(handler: HTTPRequest<Ctx>) HTTPRequest {
  return async func(ctx: Ctx, req: http.Request, resp: mut http.Response) {
    if !(await ctx.is_cancelled()) { // check cancel token
      await handler(ctx, req, resp)
    }
    await resp.set_status(400)
    await resp.write("cancelled")
  }
}
```

for the above examples, you would pass a context type that implements all three traits.

## Monadic function modifiers

Boring uses function modifiers to implement functionality like `async/await` and `coroutines`. These function by [rewriting the code into a state machine](https://tmandry.gitlab.io/blog/posts/optimizing-await-1/) prior to compilation. The table below describes the modifiers currently available.

|Type|Change To Return Type|Introduces to Scope|
|---|---|---|
|`async`|`Promise<ReturnType>`|`await`|
|`coroutine`|`FirstReturnType,func(Next,Params)...`|`yield`|
|`error<ErrorType>`|`Result<ReturnType,ErrorType>`|`?`|

## Import System

Similar to python, folders/files represent the `.` seperated import path, but relative imports are *not* supported. Exported values must be marked with `pub`. All imports take the form:

```
import package.path as local_name

pub struct MyStruct {
  id: Int
}
```
