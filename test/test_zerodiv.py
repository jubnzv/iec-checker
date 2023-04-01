import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa


def test_zerodiv():
    f = 'st/zero-division.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('ZeroDivision') == 2
    with DumpManager(fdump):
        pass
