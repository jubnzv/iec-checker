## Unreleased

- Support OCaml 5.1+ and Core 0.16+
- Migrate the project from `Core_kernel` to `Core`
- Driver: remove deprecated `Caml` module calls in the driver and lexer
- Driver: replace *nix-specific `Sys` functions with portable equivalents
- Driver: merge multiple input files via the command line
- Driver: force newlines after raising warnings
- Parser: fix errors after updating Menhir
- Build: simplify installation; add a top-level `Makefile`
- Build: build the project before running the test suite
- CI: update GitHub Actions to the latest packages
- CI: fix `dune not found` error when running `make test`
- Deps: bump `pytest` version to support Python 3.10
- Add `make doc` target and odoc package landing page (#24)
- Integrate `codespell` to the CI and fix errors found (#29)
- Uniform `Detector` API (#27)
- CLI option `--list-checks` (#28)
- Nuke `ZeroDiv` check
- Improve warnings output format (#30)
- Add `--no-color` CLI option (#30)
- Add configuration file (#44)
- lexer: Case-sensitive identifiers
- Add `PLCOpen-N1` (#15)
- Add `PLCOpen-N2` (#34)
- Add `PLCOpen-N4` (#35)
- Add `PLCOpen-N5` (#36)
- Add `PLCOpen-N6` (#37)
- Add `PLCOpen-N8` (#38)
- Add `PLCOpen-N9` (#39)
- Add `PLCOpen-CP16` (#47)
- Add `PLCOpen-CP26` (#52)
- Add `PLCOpen-L13` (#55)
- Add `PLCOpen-L22` (#56)
- Add `PLCOpen-N10` (#40)
- Parser/Lexer: show the problematic token and a source snippet for `ParserError`/`LexingError` (#58)
- Lexer: fix a bug where the `TRUE` boolean literal was silently lexed as `false`, corrupting CP2, CP8, CP25, N3 detectors
- `UnusedVariable`: fix a false positive where variables accessed only via struct-member syntax were reported as unused
- fix(parser): Allow whitespace only `fb_body`/`fc_body` (#62)

## 0.4.0 (2022-02-26)

### New PLCOpen rules

- Add `PLCOPEN-CP1` (access to a member shall be by name)
- Rename the `UnreachableCode` check to PLCOPEN-CP2 (all code shall be used)
- Add `PLCOPEN-CP3` (variables shall be initialized before being used); fix a
  false positive on direct variables
- Add `PLCOPEN-CP4` (direct addressing should not overlap)
- Add `PLCOPEN-CP6` (avoid external variables in functions, function blocks
  and classes)
- Add `PLCOPEN-CP8` (floating-point comparison shall not be equality or
  inequality)
- Add `PLCOPEN-CP25` (data type conversion should be explicit), along with the
  variable declaration analysis it requires
- Add `PLCOPEN-CP28` (time and physical measures shall not be compared for
  equality or inequality)
- Add `PLCOPEN-L10` (avoid `CONTINUE` and `EXIT` instructions)

### New input format

- Initial support for the SEL XML input format; allow the `<Type>` XML tag
  to be used instead of `<POUKind>`

### Language / parser support

- Initial support for OOP features of the ST language
- Lexer: allow time literals in uppercase
- Lexer: allow identifiers starting with an underscore
- Lexer: workaround for struct access
- Parser: switch to the new Menhir style throughout
- Parser: allow direct-variable assignment
- Parser: make the semicolon after a type declaration optional
- Parser: allow skipping semicolons after statements
- Parser: support empty variable sections
- Parser: finish access and located variables
- Parser: allow arrays inside `IF` statements
- Parser: fix a parser error on `v16 AT %MW12:INT`

### Driver / CLI

- Recursively traverse nested input directories
- Get rid of magic numbers in return codes
- Define config options for selecting which checks are executed
- Add a Python argument to specify the OCaml binary (#7)
- Fix a crash on non-existent input files
- Fix the case where the checker is run with stdin input

### Bug fixes

- plcopen.xml: fix an XML parser error
- ast_util: fix `get_stmts` for `FOR` loops

### Build / CI

- dune: force the previous Menhir version to avoid regressions
- dune: generate a correct opam file with dependencies
- dune: fix the license metadata
- Remove an unused dependency
- github-actions: update the OCaml version and `avsm/setup-ocaml`
- github-actions: run unit tests on every PR
- ci: fix `pytest`

### Refactor / cleanup

- python: fix formatting and typing warnings
- driver: fix formatting and deprecation warnings
- warn_output: simplify code
- parser: remove dead code

## 0.3.0 (2020-10-27)

- Add the ability to build for Windows; update dependencies
- Fix compatibility with the latest Menhir
- Fix codespell typos across the source tree
- CI: update GitHub Actions configuration; install the `xmlm` dependency

## 0.2.0 (2020-09-05)

- Initial support for the PLCOpen XML input format, including derived types,
  pointers, xhtml tags inside source code, and a workaround for unimplemented
  configurations
- Initial implementation of the PLCOPEN-CP9 rule (limit POU complexity), with
  support for both McCabe complexity and statement count; raise the McCabe
  threshold to a more practical value
- Initial data-flow analysis for arrays, including out-of-bound detection in
  declaration analysis
- Detect declaration errors inside POU local variables, not just globals
- Add `VarUse` module to track additional variable-usage info; record array
  subscription info on `SymVar.t`
- Track variable initialization info in the AST; refactor the AST to separate
  the specification of derived types
- Parser: support reference assign statements (`?=`)
- Parser: allow empty POU bodies
- Parser: support direct variables as task data sources
- Parser: support arrays of all constant expression types
- Parser: simplify `func_call`
- Rewrite the CFG structure
- Build: add a `release` env to dune; add `xmlm` as a dependency
- CI: add a workaround for Menhir regressions

## 0.1.0 (2020-05-11)

- Initial release.
