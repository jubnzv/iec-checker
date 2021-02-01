import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa


def test_zerodiv():
    f = 'st/zero-division.st'
    fdump = f'{f}.dump.json'
    cvs, rc = run_checker(f)
    assert rc == 0
    assert len(cvs) == 3  # + unused variable

    assert cvs[1].id == 'ZeroDivision'
    assert cvs[1].linenr == 7
    assert cvs[1].column == 12
    assert 'Constant' in cvs[1].msg

    assert cvs[0].id == 'ZeroDivision'
    assert cvs[0].linenr == 9
    assert cvs[0].column == 14
    assert 'Variable' in cvs[0].msg

    with DumpManager(fdump):
        pass
