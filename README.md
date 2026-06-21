# Pressure programming Language

Interpreter for the Pressure programming language, built with educational
purposes for the course "Programming Languages: Concepts & Paradigms".

The idea is to build a simple but useful imperative programming language.

```odin
main :: fn() {
    // Declares a constant.
    x :: 42;

    // Declares variable.
    y := "variables";
    y = "can vary";

    // The type can be also specified:
    i : [_]byte : "type being specified";

    // Booleans
    btrue :: true;
    bfalse :: false;
}
```

For more information see [SPEC.md](./SPEC.md).

## Usage

You can run the repl with:

```bash
cabal run
```

Or optionally pass a file for the project:

```bash
cabal run -- main.ps
```

For testing use:

```bash
cabal test
```

## Contributing

Check the todo items in [TODO.md](./TODO.md).
