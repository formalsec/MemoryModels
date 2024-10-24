# MemoryModel

## Installation

1. Install [opam](https://opam.ocaml.org/doc/Install.html).
2. Bootstrap the OCaml compiler:

```sh
opam init
opam switch create memory 4.14.0
eval $(opam env)
```

3. Install the library dependencies:

```sh
opam update
opam install . --deps-only --with-test
```

4. Build the application and run the available test suit:

```sh
dune build
dune runtest
dune install
```