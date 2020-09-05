# IEC Checker

![](https://github.com/jubnzv/iec-checker/workflows/Unit%20tests/badge.svg)

This project aims to implement an open source tool for static code analysis of IEC 61131-3 programs.

The following features are currently implemented:
+ [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines) checks:
  - CP13: POUs shall not call themselves directly or indirectly
  - CP9: Limit the complexity of POU code
  - L17: Each IF instruction should have an ELSE clause
  - N3: Define the names to avoid
+ Declaration analysis for derived types
+ Intraprocedural control flow analysis: detection of unreachable code blocks inside the [POUs](https://en.wikipedia.org/wiki/IEC_61131-3#Program_organization_unit_(POU))
+ Detection of unused variables
+ Ability to integrate with other tools. Checker can dump its IR into JSON file (`-dump true` argument) and produce warnings in JSON format (`-output-format json`);
+ Can be extended with plugins written in Python. See demo plugin that plots control flow graph: [cfg_plotter.py](./src/python/plugins/cfg_plotter.py).

## Installation

You can download the latest binary release for Linux x86_64 from [GitHub releases](https://github.com/jubnzv/iec-checker/releases).

### Build from sources

If you want to build it from sources, you need to install OCaml compiler with some additional packages.

First, get the latest OCaml compiler and opam. Consider installation instructions at [ocaml.org](https://ocaml.org/docs/install.html) and [opam.ocaml.org](https://opam.ocaml.org/doc/Install.html).

Then install create OCaml switch and install the required dependencies:
```bash
opam switch create 4.10.0
opam pin menhir 20200211
opam install -y dune core menhir menhirLib ppx_deriving ppx_deriving_yojson \
                ppx_fields_conv ppx_jane ppx_variants_conv ppxlib re yojson xmlm
eval $(opam env)
```

Build and install iec_checker binary to the `output` directory:
```bash
dune build @install
mkdir output
dune install --preifx ./output
```

### Python scripts and test suite
There is also a convenient [checker.py](./checker.py) script that wraps OCaml binary and provide additional options like extended formatting support and running the Python plugins. The test suite is also written in Python and requires a Python interpreter with some additional packages.

Get [Python 3](https://www.python.org/downloads/) and install missing dependencies in [virtual environment](https://docs.python.org/3/library/venv.html):
```bash
virtualenv venv --python=/usr/bin/python3
source venv/bin/activate
pip3 install -r requirements.txt
```

Then run unit tests:
```bash
pip3 install -r requirements-dev.txt
pytest
```

## Usage example

Check some demo programs written in Structured Text:
```bash
python3 checker.py test/st/*.st
```

This will gives you the following output:
```
Report for test/st/dead-code.st:
[PLCOPEN-L17] 17:6 Each IF instruction should have an ELSE clause
[UnusedVariable] 15:5 Found unused local variable: A
[UnreachableCode] 9:10 Code block will never be reached
[UnreachableCode] 25:7 Code block will never be reached
[UnreachableCode] 20:7 Code block will never be reached
Report for test/st/declaration-analysis.st:
[OutOfBounds] Initial subrange value -4096 does not fit the specified range (-4095 .. 4095)
[OutOfBounds] Initial subrange value 4099 does not fit the specified range (-4095 .. 4095)
[OutOfBounds] Length of initialization string literal exceeds string length (6 > 5)
Report for test/st/plcopen-cp13.st:
[PLCOPEN-CP13] 8:30 POUs shall not call themselves directly or indirectly
Report for test/st/plcopen-cp9.st:
[PLCOPEN-L17] 44:10 Each IF instruction should have an ELSE clause
[PLCOPEN-L17] 39:10 Each IF instruction should have an ELSE clause
[PLCOPEN-CP9] Code is too complex (16)
Report for test/st/plcopen-l17.st:
[PLCOPEN-L17] 10:4 Each IF instruction should have an ELSE clause
Report for test/st/plcopen-n3.st:
[PLCOPEN-N3] 6:7 IEC data types and standard library objects must be avoided
[UnusedVariable] 6:7 Found unused local variable: TOF
[UnusedVariable] 7:14 Found unused local variable: OK_ALLOWED
[UnusedVariable] 4:21 Found unused local variable: NO_FALSE_POSITIVE
Report for test/st/unused-variable.st:
[UnreachableCode] 10:10 Code block will never be reached
Report for test/st/zero-division.st:
[ZeroDivision] 7:12 Constant 19 is divided by zero!
[ZeroDivision] 9:14 Variable VAR2 is divided by zero!
```
