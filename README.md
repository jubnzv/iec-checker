# IEC Checker
This project aims to implement an open source tool for static code analysis of IEC61131-3 programs.

It is currently at an early stage of development, binary releases and installation instructions will be available later.

The following features are currently supported:
+ IEC61131-3 3 ed. parser for [Structured Text](https://en.wikipedia.org/wiki/Structured_text) language written using modern [menhir](http://gallium.inria.fr/~fpottier/menhir/) syntax. Almost all IEC features are supported, excluding some user-defines types and OO features.
+ Some checks for [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines);
+ WIP control and data flow analysis;
+ Dump AST of IEC program in json file (`-dump true` argument);

## Setup

For development you need to install [ocaml](https://ocaml.org/docs/install.html) environment and setup [dune](https://dune.readthedocs.io/en/stable/quick-start.html) build system:
```bash
opam switch create 4.08.1
opam install dune
```

Running unit tests:
```bash
dune runtest
```

Checking demo programs written in Structured Text:
```bash
dune exe -- src/bin/iec_checker.exe -output-format plain test/st/configurations.st
```
