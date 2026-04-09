# IEC Checker

**⚡ [Try it online](https://iec-checker.github.io/playground)** | **📚 [Documentation](https://iec-checker.github.io/docs/intro)** | **🛡️ [Detectors reference](https://iec-checker.github.io/docs/detectors)** | **🔧 [Sponsor Custom Features](https://iec-checker.github.io/docs/sponsor-development)**

IEC Checker is an open source static analyzer for [IEC 61131-3](https://en.wikipedia.org/wiki/IEC_61131-3) programs. It catches bugs and enforces [PLCOpen coding guidelines](https://plcopen.org/software-construction-guidelines) before code reaches the PLC.

## What it looks like

```
$ iec_checker program.st
PLCOPEN-CP3: Variable X shall be initialized before being used
  --> program.st:4:9
  See: https://iec-checker.github.io/docs/detectors/PLCOPEN-CP3

PLCOPEN-CP13: POUs shall not call themselves directly or indirectly
  --> program.st:8:30
  See: https://iec-checker.github.io/docs/detectors/PLCOPEN-CP13
```

## Features

- 13 [PLCOpen Software Construction Guidelines](https://iec-checker.github.io/docs/detectors/plcopen-overview) checks
- Declaration analysis, unreachable code detection, unused variable detection
- Structured Text, [PLCOpen XML](https://plcopen.org/technical-activities/xml-exchange), and [SEL XML](https://selinc.com/products/3530/) input formats
- JSON output for [CI/CD integration](https://iec-checker.github.io/docs/ci-cd) and [Python tooling](https://iec-checker.github.io/docs/python)

The ST dialect is compatible with the [matiec](https://github.com/sm1820/matiec) compiler. If `iec-checker` chokes on extensions from your PLC vendor, please [open an issue](https://github.com/jubnzv/iec-checker/issues/new).

## Installation

Download a prebuilt binary for Linux or Windows x86_64 from [GitHub releases](https://github.com/jubnzv/iec-checker/releases).

### Build from source

Requires [OCaml](https://ocaml.org/docs/install.html) 5.1+ and [opam](https://opam.ocaml.org/doc/Install.html).

```bash
opam install --deps-only .
make
```

See the [installation guide](https://iec-checker.github.io/docs/installation) for Windows instructions and optional Python setup.

## Usage

```bash
iec_checker test/st/*.st          # check ST files
iec_checker -i xml schemes/*.xml  # check PLCOpen XML
iec_checker --help                # all options
```

See the [CLI reference](https://iec-checker.github.io/docs/cli) for the full option list, output formats, and exit codes.
