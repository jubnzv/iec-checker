# IEC Checker

![](https://github.com/jubnzv/iec-checker/workflows/Unit%20tests/badge.svg)

This project aims to implement an open source tool for static code analysis of IEC61131-3 programs.

It is currently at an early stage of development, binary releases and installation instructions will be available later.

The following features are currently supported:
+ IEC61131-3 3 ed. parser for [Structured Text](https://en.wikipedia.org/wiki/Structured_text) language written using modern [menhir](http://gallium.inria.fr/~fpottier/menhir/) syntax. Almost all IEC features are supported, excluding some user-defines types and OO features.
+ Some checks for [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines);
+ WIP control and data flow analysis;
+ Dump AST of IEC program in json file (`-dump true` argument);

## Installation

For development you need to install [ocaml](https://ocaml.org/docs/install.html) environment and [dune](https://dune.readthedocs.io/en/stable/quick-start.html) build system:
```bash
opam switch create 4.08.1
opam install dune
```

Running unit tests:
```bash
dune runtest
```

Building and installing OCaml package to the current directory:
```bash
dune build @install
dune install --preifx ./output
```

You will also need a Python interpreter with some additional packages:
```bash
apt-get install python3 python3-virtualenv
virtualenv venv --python=/usr/bin/python3
source venv/bin/activate
pip3 install -r requirements.txt
```

## Usage

Checking demo programs written in Structured Text:
```bash
python3 checker.py test/st/*.st
```

This will gives the following output:
```
Report for test/st/configurations.st:
No errors found!
Report for test/st/constant-zero-division.st:
No errors found!
Report for test/st/control-statements.st:
42:8: PLCOPEN-L17: Each IF instruction should have an ELSE clause
Report for test/st/declaration-analysis.st:
0:0: DeclarationAnalysis: Length of initialization string literal exceeds string length (6 > 5)
Report for test/st/function-blocks.st:
No errors found!
Report for test/st/function-declaration.st:
No errors found!
Report for test/st/literals.st:
17:6: PLCOPEN-N3: IEC data types and standard library objects must be avoided
Report for test/st/multiple-pous.st:
No errors found!
Report for test/st/multiple-variables.st:
No errors found!
Report for test/st/plcopen-cp13.st:
8:31: PLCOPEN-CP13: POUs shall not call themselves directly or indirectly
Report for test/st/plcopen-l17.st:
10:4: PLCOPEN-L17: Each IF instruction should have an ELSE clause
Report for test/st/plcopen-n3.st:
6:7: PLCOPEN-N3: IEC data types and standard library objects must be avoided
Report for test/st/st-statements.st:
No errors found!
Report for test/st/types.st:
No errors found!
```
