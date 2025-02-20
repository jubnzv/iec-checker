# IEC Checker

> ⚠️ **Note:** Development is on hold. More advanced analysis techniques are needed to cover real-world issues. Some companies are using the analyzer as it is, which is fine if they comply with the LGPL. But for serious results, consider collaborating with a skilled team dedicated to security tooling for safety-critical systems.
>
>📩 **Contact:** [oi@nowarp.io](mailto:oi@nowarp.io)

This project aims to implement an open source tool for static code analysis of [IEC 61131-3](https://en.wikipedia.org/wiki/IEC_61131-3) programs.

## Supported languages

This tool currently supports [Structured Text](https://en.wikipedia.org/wiki/Structured_text) programming language, [PLCOpen XML](https://plcopen.org/technical-activities/xml-exchange) and [SEL XML](https://selinc.com/products/3530/) formats.
It works with extended Structured Text dialect that is completely compatible with [matiec](https://github.com/sm1820/matiec) transpiler.

If you find, that `iec-checker` doesn't work with Structured Text extensions provided by your PLC vendor, please [let me know](https://github.com/jubnzv/iec-checker/issues). This can probably be easily implemented with some tweaks in the parser.

## Features

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
  - L10: Usage of CONTINUE and EXIT instruction should be avoided
  - L17: Each IF instruction should have an ELSE clause
  - N3: Define the names to avoid
+ Declaration analysis for derived types
+ Intraprocedural control flow analysis: detection of unreachable code blocks inside the [POUs](https://en.wikipedia.org/wiki/IEC_61131-3#Program_organization_unit_(POU))
+ Detection of unused variables
+ Ability to integrate with other tools. Checker can dump its IR into a JSON file (`--dump` argument) and produce warnings in JSON format (`--output-format-format json`).
+ Can be extended with plugins written in Python. See demo plugin that plots the control flow graph: [cfg_plotter.py](./src/python/plugins/cfg_plotter.py).

## Installation

You can download the latest binary release for Linux and Windows x86_64 from [GitHub releases](https://github.com/jubnzv/iec-checker/releases).

### Build from sources

#### Linux

Install the latest OCaml compiler and opam. Consider installation instructions at [ocaml.org](https://ocaml.org/docs/install.html) and [opam.ocaml.org](https://opam.ocaml.org/doc/Install.html).

Then install the required dependencies:

```bash
opam install --deps-only .    # first time only
```

Build and install the `bin/iec_checker` binary:

```bash
make build
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
pip3 install -r requirements-dev.txt
```

Then run unit tests:
```bash
make test
```

## Usage examples

Check some demo programs written in Structured Text:

```
bin/iec_checker test/st/*.st
```

You can also use `--help` argument to display help.
