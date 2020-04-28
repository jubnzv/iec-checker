"""Tests for parser and lexer."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import process_dump, remove_dump  # noqa


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
    remove_dump(fdump)


def test_parser_errors():
    for fname in os.listdir('./test/st/bad/'):
        f = os.path.join('./test/st/bad/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker(f)
        assert rc == 1
        assert len(checker_warnings) > 0
        remove_dump(fdump)


def test_no_parser_errors():
    for fname in os.listdir('./test/st/good/'):
        f = os.path.join('./test/st/good/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker(f)
        assert rc == 0
        remove_dump(fdump)
