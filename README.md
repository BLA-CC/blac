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

### Running and Usage

The previous sub-section should result in a `c-tds` binary of the compiler, the cli can be seen with

```sh
./c-tds --help

# Usage: c-tds [options] input
#     -h, --help            show this help message and exit
#
# Compile options
#     -o, --output=<str>    output file (default=./a.out)
#     -t, --target=<str>    scan|parse|ir|asm (default=asm)
#     -O, --optimize=<str>  none|all (default=none)
#     -d, --debug           allow extra debug info (off by default)
```

### Testing

This project uses `gtest` for testing, with that library installed tests can run with the following command

```sh
make test
```

## Design principles

One of the most prevalent principles in the codebase is minimizing memory allocations, `malloc()` `calloc()` `realloc()` and `free()` in particular. We achieve this by making use of pool allocators which are basically a dynamic array that would store the many instances of the type that would be otherwise allocated with `malloc` in the code the instances of the types are managed as an integer that represents the index of the array.

Examples of this can be seen in `Ast` at `ast.{c,h}` and `StrPool` at `str_pool.{c,h}`.
