# Lambdrador Installation Guide

These installation guides were written for macOS Sonoma 14.5, using zsh, however should work on Linux too

### OCaml

In order to install the OCaml interpreter, a copy of OPAM, OCaml's package manager is required.

Using OPAM, install version 5.1.1 of OCaml and from there install Dune, the ocaml build system

Move to the top level of the interpreters directory, and run this command.

```dune build p lambdrador @install && dune install```

This should install the interpreter under the name `lambdrador`, which can be used as a command to run the REPL. A file can be given after this command to run as a program.

Alternately, you can run

```dune build```

The native and bytecode executables can then be found in the _build/default/bin/ directory, under the names main.exe and main.bc. These files can then be used as a copy of the interpreter.

### Python

All needed for the python interpreter installation is a copy of python 3.12. src/main.py can be used to invoke the interpreter. Files can be given after this to run them as programs
