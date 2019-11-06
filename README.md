# Boring Lang

**NOTE: This repo is a work in progress as I learn compiler writing, I may abandon this.**

The Boring Programming Language (Boring-Lang) is an attempt to create an easy, productive, general purpose programming language that makes as few interesting choices as possible while still being in line with modern concepts in programming languages.

The language:
* is compiled with a run-time (llvm for convenience + c compatibility)
* is garbage collected
* uses async-await for all IO, with a built-in multi-core scheduler
* supports algebraic data types (Result type for errors, Maybe type for nullables)
* supports parametric polymorphism
* uses struct+traits, rather than classes or stuct+interfaces
* has a rich standard library (http server, actor model)

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

async func handle(req http.Request, resp http.Response) {
  log.info("request: ", req.body)
  response_data := ExampleResponse{id: 4, name: "Steven", email: "swerbenjagermanjensen@example.com"}
  resp.write(json.encode<ExampleResponse>(response_data))
  resp.set_status(200)
}

async func main(args: Array<Str>) int {
    router := http.Router("").add_route("/myroute", handle)
    http_server := http.Server("localhost", 8080, router)
    err := await http_server.server_forever()
    await log.info("error serving: ", err)
    return 1
}
```
