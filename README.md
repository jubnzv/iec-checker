# IEC Checker

![](https://github.com/jubnzv/iec-checker/workflows/Unit%20tests/badge.svg)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)

This project aims to implement an open source tool for static code analysis of IEC 61131-3 programs.

You can try [Online Demo](https://idie.ru/bin/iec-checker).

The following features are currently implemented:
+ [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines) checks:
  - CP1: Access to a member shall be by name
  - CP2: All code shall be used in the application
  - CP3: All variables shall be initialized before being used
  - CP4: Direct addressing should not overlap
  - CP6: Avoid external variables in functions, function blocks and classes
  - CP8: Floating point comparison shall not be equality or inequality
  - CP9: Limit the complexity of POU code
  - CP13: POUs shall not call themselves directly or indirectly
  - CP25: Data type conversion should be explicit
  - CP28: Time and physical measures comparisons shall not be equality or inequality
  - L17: Each IF instruction should have an ELSE clause
  - N3: Define the names to avoid
+ Declaration analysis for derived types
+ Intraprocedural control flow analysis: detection of unreachable code blocks inside the [POUs](https://en.wikipedia.org/wiki/IEC_61131-3#Program_organization_unit_(POU))
+ Detection of unused variables
+ Ability to integrate with other tools. Checker can dump its IR into JSON file (`-dump true` argument) and produce warnings in JSON format (`-output-format json`);
+ Can be extended with plugins written in Python. See demo plugin that plots control flow graph: [cfg_plotter.py](./src/python/plugins/cfg_plotter.py).

## Installation

You can download the latest binary release for Linux and Windows x86_64 from [GitHub releases](https://github.com/jubnzv/iec-checker/releases).

### Build from sources

#### Linux

Install the latest OCaml compiler and opam. Consider installation instructions at [ocaml.org](https://ocaml.org/docs/install.html) and [opam.ocaml.org](https://opam.ocaml.org/doc/Install.html).

Then install the required dependencies:

```bash
opam install --deps-only .    # first time only
```

Build and install `iec_checker` binary to the `output` directory:

```bash
dune build @install
mkdir output
dune install --prefix ./output
```

#### Windows

Install [OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/) according to the [installation guide](https://fdopen.github.io/opam-repository-mingw/installation/). The graphic installer works well "out of the box".

Then open installed Cygwin shell, clone the repository and use the installation instructions from the "Linux" section.

### Optional: Python scripts and test suite
There is also a convenient [checker.py](./checker.py) script that wraps OCaml binary and provide additional options like extended formatting support and running the Python plugins. The test suite is also written in Python and requires a Python interpreter with some additional packages.

Get [Python 3](https://www.python.org/downloads/) and install dependencies in the [virtual environment](https://docs.python.org/3/library/venv.html):
```bash
virtualenv venv --python=/usr/bin/python3
source venv/bin/activate
pip3 install -r requirements.txt
```

Then run unit tests:
```bash
pip3 install -r requirements-dev.txt
cd test
pytest
```

## Usage

Check some demo programs written in Structured Text:

```bash
output/bin/iec_checker test/st/*.st
```

This will gives you the following output:

```
Running check for function DEAD_CODE_AFTER_RETURN
Running check for program DEAD_CODE_IN_THE_LOOPS
20:7 UnreachableCode: Code block will never be reached
25:7 UnreachableCode: Code block will never be reached
9:10 UnreachableCode: Code block will never be reached
15:5 UnusedVariable: Found unused local variable: A
17:6 PLCOPEN-L17: Each IF instruction should have an ELSE clause
Parsing test/st/declaration-analysis.st ...
Running check for derived type
0:0 OutOfBounds: Length of initialization string literal exceeds string length (6 > 5)
0:0 OutOfBounds: Initial subrange value 4099 does not fit the specified range (-4095 .. 4095)
0:0 OutOfBounds: Initial subrange value -4096 does not fit the specified range (-4095 .. 4095)
Parsing test/st/plcopen-cp13.st ...
Running check for function FACTORIAL
Running check for function FACTORIAL_GOOD
8:30 PLCOPEN-CP13: POUs shall not call themselves directly or indirectly
Parsing test/st/plcopen-cp9.st ...
Running check for function block CHARCURVE
0:0 PLCOPEN-CP9: Code is too complex (16 McCabe complexity)
44:10 PLCOPEN-L17: Each IF instruction should have an ELSE clause
49:10 PLCOPEN-L17: Each IF instruction should have an ELSE clause
Parsing test/st/plcopen-l17.st ...
Running check for program PROGRAM0
10:4 PLCOPEN-L17: Each IF instruction should have an ELSE clause
Parsing test/st/plcopen-n3.st ...
Running check for program PROGRAM0
4:21 UnusedVariable: Found unused local variable: NO_FALSE_POSITIVE
7:14 UnusedVariable: Found unused local variable: OK_ALLOWED
6:7 UnusedVariable: Found unused local variable: TOF
6:7 PLCOPEN-N3: IEC data types and standard library objects must be avoided
Parsing test/st/zero-division.st ...
Running check for program PROGRAM0
9:14 ZeroDivision: Variable VAR2 is divided by zero!
7:12 ZeroDivision: Constant 19 is divided by zero!
```
