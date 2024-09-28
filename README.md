# BLAC

A compiler for the TDS24 language.

## Building

### Requirements

A C17 compliant compiler and the following binaries/libraries:

- `flex`
- `bison`
- `gtest` (optional)

### Compiling

Just running the makefile should suffice

```sh
make
```

That makes the `c-tds` binary available to run the compiler

### Testing

This project uses `gtest` for testing, with that library installed tests can run with the following command

```sh
make test
```

## Design principles

One of the most prevalent principles in the codebase is minimizing memory allocations, `malloc()` `calloc()` `realloc()` and `free()` in particular. We achieve this by making use of pool allocators which are basically a dynamic array that would store the many instances of the type that would be otherwise allocated with `malloc` in the code the instances of the types are managed as an integer that represents the index of the array.

Examples of this can be seen in `Ast` at `ast.{c,h}` and `StrPool` at `str_pool.{c,h}`.
