# Alaska language

This is the Alaska programming language written in C. Alaska is a combination of `Go` and `Rust`.

To run the binary, do `make run input=compiler/tests/test.ask`. If you want to add Debug information, add the `debug=1` on the end of the previous command.

## Why another language?

Because it is fun and I always wanted to make one myself. Last time I did this was at University and it was a subset of `C`. I love Go and I find Rust quite intriguing with the concept of `borrow` (so no Garbage Collector). Rust has other interesting features like `match` and a nice looking Generics implementation. However, the language is super difficult to learn and despite my efforts in writing code for learning it, I never managed to do it. Perhaps I'm getting to old for it :)

## Goals

The goals of Alaska are:

- [ ] Don't know...

## TODO

Here is a list of features that I'd like to implement in the language:

- [ ] `mut` word after `let` to control the mutability of an object
- [ ] Add `Generics` like Rust does with `<>` and not Go style
- [ ] Implement the `match` like Rust does
- [ ] Implement `enum` instead of Go `const` - do not confuse with the `num` of Rust which looks like the `interface` type
- [ ] Implement `Result` and `Option` like Rust does
- [ ] Implement `&` and `*` for dereference and pointers
