# IEC Checker
This (very WIP) project aims to implement a tool for static code analysis of IEC61131-3 programs.

## Building from sources

Running unit tests:
```bash
dune runtest
```

Checking demo programs written in Structured Text:
```bash
dune exe src/bin/iec_checker.exe test/st/configurations.st
```
