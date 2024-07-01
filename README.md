# Alaska language

This is the Alaska programming language written in C. Alaska is a combination of `Go` and `Rust`.

To run the binary, do `make run input=compiler/tests/test.ask`. If you want to add Debug information, add the `debug=1` on the end of the previous command.

## Why another language?

Because it is fun and I always wanted to make one myself. Last time I did this was at University and it was a subset of `C`.

## Goals

The goals of Alaska are:

- [ ] To be used in the data world with strong typing but easy learning curve
- [ ] Designed to use multiple threads with ease
- [ ] ...

## TODO

Here is a list of features that I'd like to implement in the language:

- [ ] Implement better Parsing error message to user
- [ ] Use LLVM as backend for multiple architectures
- [ ] Implement arenas for memory pool
- [ ] Implement the `match` like Rust does
- [ ] Implement `spawn` similar to `go`
- [ ] Implement `as` for type casting
- [ ] Implement `&` and `*` for dereference and pointers
- [ ] Garbage collection
- [ ] Implement `modules`, `packages`, `registry`, etc for both internal/external dependencies
- [ ] Implement Language Server for IDE
- [ ] ....and more
