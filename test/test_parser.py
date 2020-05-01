"""Tests for parser and lexer."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, check_program  # noqa
from python.dump import DumpManager  # noqa


def test_lexing_error():
    f = './test/st/bad/lexing-error.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 1
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'LexingError'
    assert cv.linenr == 9
    assert cv.column == 6
    with DumpManager(fdump):
        pass


def test_parser_errors():
    for fname in os.listdir('./test/st/bad/'):
        f = os.path.join('./test/st/bad/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker(f)
        assert rc == 1, f"Incorrect exit code for {f}"
        assert len(checker_warnings) > 0
        with DumpManager(fdump):
            pass


def test_no_parser_errors():
    for fname in os.listdir('./test/st/good/'):
        f = os.path.join('./test/st/good/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker(f)
        assert rc == 0, f"Incorrect exit code for {f}"
        with DumpManager(fdump):
            pass


def test_direct_variables():
    f = './test/st/good/direct-variables.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme


def test_statements_order():
    """Test that POU statements are arranged in the correct order."""
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM f
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
