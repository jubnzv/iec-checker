# IEC Checker
This (very WIP) project aims to implement a tool for static code analysis of IEC61131-3 programs.

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
dune exe -- src/bin/iec_checker.exe test/st/configurations.st
```

