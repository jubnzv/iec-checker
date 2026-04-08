# IEC Checker

> 💡 **Note:** `iec-checker` is an open-source project that can be extended for your needs. I take on funded work to add things like:
>
> - **New language support** — Structured Text extensions used by your specific PLC vendor, or other IEC 61131-3 languages (IL, FBD, LD, SFC)
> - **Additional safety standards** — checks beyond the PLCOpen Guidelines already implemented
> - **Custom detectors and checks** — adding new custom analyses for a specific project
>
> 📩 **Contact:** [jubnzv@gmail.com](mailto:jubnzv@gmail.com)

IEC Checker is an open source static analyzer for [IEC 61131-3](https://en.wikipedia.org/wiki/IEC_61131-3) programs.

## Supported languages and formats

`iec-checker` analyzes [Structured Text](https://en.wikipedia.org/wiki/Structured_text) with the language extensions accepted by the [matiec](https://github.com/sm1820/matiec) compiler.

Supported input formats:
* Plain ST source code
* [PLCOpen XML](https://plcopen.org/technical-activities/xml-exchange)
* [SEL XML](https://selinc.com/products/3530/) (vendor-specific)

If `iec-checker` chokes on Structured Text extensions from your PLC vendor, please [open an issue](https://github.com/jubnzv/iec-checker/issues/new). Adding new dialects is usually a small parser change.

## Features

### PLCOpen Guidelines coverage

The following [PLCOpen Guidelines](https://plcopen.org/software-construction-guidelines) checks are supported:
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
- L10: Usage of `CONTINUE` and `EXIT` instruction should be avoided
- L17: Each IF instruction should have an `ELSE` clause
- N3: Define the names to avoid

### Additional features
- Declaration analysis for derived types
- Intraprocedural control flow analysis: detection of unreachable code blocks inside [POUs](https://en.wikipedia.org/wiki/IEC_61131-3#Program_organization_unit_(POU))
- Detection of unused variables
- JSON output for tool integration: dump the IR with `--dump` and emit warnings via `--output-format json`
- Python plugin support — see [cfg_plotter.py](./src/python/plugins/cfg_plotter.py), which plots the control flow graph

## Installation

You can download the latest binary release for Linux and Windows x86_64 from [GitHub releases](https://github.com/jubnzv/iec-checker/releases).

### Build from sources

#### Linux

Install [OCaml](https://ocaml.org/docs/install.html) 5.1 or later and [opam](https://opam.ocaml.org/doc/Install.html).

Then install the required dependencies:

```bash
opam install --deps-only .    # first time only
```

Build and install the `bin/iec_checker` binary:

```bash
make build
```

#### Windows

Install [OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/) according to the [installation guide](https://fdopen.github.io/opam-repository-mingw/installation/). The graphical installer works well out of the box.

Then open the Cygwin shell, clone the repository, and follow the Linux instructions above.

### Optional: Python scripts and test suite
The [checker.py](./checker.py) script wraps the OCaml binary and adds extras like extended formatting and Python plugin support. The test suite is also written in Python and needs a few extra packages.

Get [Python 3](https://www.python.org/downloads/) and install dependencies into a [virtual environment](https://docs.python.org/3/library/venv.html):
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

Check the demo programs:

```
bin/iec_checker test/st/*.st
```

Pass `--help` for the full list of options.
