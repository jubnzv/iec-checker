"""Tests for internal errors."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa


def test_missing_file():
    f = './test/st/foo.bar'
    checker_warnings, rc = run_checker(f)
    assert rc == 127
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'FileNotFoundError'


def test_large_file():
    """Make sure that there are no stack overflows on large programs."""
    fname = './test/st/_TEMP_large.st'
    with open(fname, 'w') as f:
        f.write(f"""
        PROGRAM test_for
        VAR a : INT; i : INT; END_VAR
        FOR i := 0 TO 10 BY 2 DO
        """)
        for _ in range(1000):
            f.write('a := a + i;\n')
        f.write(f"""
        END_FOR;
        i := 0;
        END_PROGRAM
        """)
    checker_warnings, rc = run_checker(fname)
    os.remove(fname)
