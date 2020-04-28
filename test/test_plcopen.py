"""Tests for PLCOpen inspections."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import remove_dump  # noqa


def test_cp13():
    f = './test/st/plcopen-cp13.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-CP13'
    assert cv.linenr == 8
    assert cv.column == 31
    remove_dump(fdump)


def test_l17():
    f = './test/st/plcopen-l17.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-L17'
    assert cv.linenr == 10
    assert cv.column == 4
    remove_dump(fdump)


def test_n3():
    f = './test/st/plcopen-n3.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-N3'
    assert cv.linenr == 6
    assert cv.column == 7
    remove_dump(fdump)
