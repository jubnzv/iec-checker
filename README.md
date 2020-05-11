# IEC Checker

![](https://github.com/jubnzv/iec-checker/workflows/Unit%20tests/badge.svg)

This project aims to implement an open source tool for static code analysis of IEC 61131-3 programs.

The following features are currently implemented:
+ IEC 61131-3 3 ed. parser for [Structured Text](https://en.wikipedia.org/wiki/Structured_text) written using modern [menhir](http://gallium.inria.fr/~fpottier/menhir/) syntax. Almost all syntactic constructions are supported, excluding some user-defined types and OO features.
+ Some checks for [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines);
+ Declaration analysis for derived types;
+ Intraprocedural control flow analysis: searching for unreachable code blocks inside the [POUs](https://en.wikipedia.org/wiki/IEC_61131-3#Program_organization_unit_(POU));
+ Found unused variables;
+ Ability to integrate with other tools. Checker can dump AST with IR into a JSON file (`-dump true` argument) and produce warnings in JSON format (`-output-format json`);
+ Can be extended with plugins in Python. See demo plugin that plots control flow graph: [cfg_plotter.py](./src/python/plugins/cfg_plotter.py).

## Installation

For development you need to install [ocaml](https://ocaml.org/docs/install.html) environment and [dune](https://dune.readthedocs.io/en/stable/quick-start.html) build system:
```bash
opam switch create 4.10.0
opam install dune
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

Running unit tests:
```bash
pip3 install -r requirements-dev.txt
pytest
```

## Usage

Check some demo programs written in Structured Text:
```bash
python3 checker.py test/st/*.st
```

This will gives you the following output:
```
Report for test/st/dead-code.st:
[PLCOPEN-L17] 17:6 Each IF instruction should have an ELSE clause
[UnusedVariable] 15:5 Found unused local variable: A
[UnreachableCode] 25:7 Code block will never be reached
[UnreachableCode] 20:7 Code block will never be reached
[UnreachableCode] 9:10 Code block will never be reached
Report for test/st/declaration-analysis.st:
[DeclarationAnalysis] Initial subrange value -4096 does not fit specified range (-4095 .. 4095)
[DeclarationAnalysis] Initial subrange value 4099 does not fit specified range (-4095 .. 4095)
[DeclarationAnalysis] Length of initialization string literal exceeds string length (6 > 5)
Report for test/st/plcopen-cp13.st:
[PLCOPEN-CP13] 8:30 POUs shall not call themselves directly or indirectly
Report for test/st/plcopen-l17.st:
[PLCOPEN-L17] 10:4 Each IF instruction should have an ELSE clause
Report for test/st/plcopen-n3.st:
[PLCOPEN-N3] 6:7 IEC data types and standard library objects must be avoided
[UnusedVariable] 6:7 Found unused local variable: TOF
[UnusedVariable] 7:14 Found unused local variable: OK_ALLOWED
[UnusedVariable] 4:21 Found unused local variable: NO_FALSE_POSITIVE
Report for test/st/this.st:
[UnusedVariable] 3:3 Found unused local variable: A
Report for test/st/unused-variable.st:
[UnreachableCode] 10:10 Code block will never be reached
Report for test/st/zero-division.st:
[ZeroDivision] 7:12 Constant 19 is divided by zero!
[ZeroDivision] 9:14 Variable VAR2 is divided by zero!
```
