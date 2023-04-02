"""Tests for parser and lexer."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, check_program  # noqa
from python.dump import DumpManager  # noqa


def test_lexing_error():
    f = 'st/bad/lexing-error.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 1
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'LexingError'
    assert cv.linenr == 10
    assert cv.column == 6
    with DumpManager(fdump):
        pass


def test_parser_errors():
    for fname in os.listdir('st/bad/'):
        f = os.path.join('st/bad/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker([f])
        assert rc == 1, f"Incorrect exit code for {f}"
        assert len(checker_warnings) > 0
        with DumpManager(fdump):
            pass


def test_no_parser_errors():
    for fname in os.listdir('st/good/'):
        if not fname.endswith('.st'):
            continue
        f = os.path.join('st/good/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker([f])
        assert rc == 0, f"Incorrect exit code for {f}"
        with DumpManager(fdump):
            pass


def test_direct_variables():
    f = 'st/good/direct-variables.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump) as dm:
        _ = dm.scheme  # TODO


def test_statements_order():
    """Test that POU statements are arranged in the correct order."""
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM p
        VAR a : INT; i : INT; END_VAR
        a := 1;
        i := 22;
        a := 16#42;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        # TODO: need recursive traverse in om
        # p = scheme.programs[0]
        # assert len(p.statemets) == 3


def test_enum_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE
          Traffic_Light: (Red, Amber, Green);
        END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'TRAFFIC_LIGHT'
        assert ty.type == 'Enum'


def test_struct_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE
          Cooler: STRUCT
            Temp: INT;
            Cooling: TOF;
          END_STRUCT;
        END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'COOLER'
        assert ty.type == 'Struct'


def test_ref_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE myRef: REF_TO INT; END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'MYREF'
        assert ty.type == 'Ref'


def test_array_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE BITS: ARRAY [0..7] OF BOOL; END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'BITS'
        assert ty.type == 'Array'
