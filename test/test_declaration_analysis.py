import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa


def test_initialization_literal():
    f = './test/st/declaration-analysis.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) == 3
    cv = checker_warnings[0]
    assert cv.id == 'OutOfBounds'
    # assert cv.linenr == 8
    # assert cv.column == 31
    with DumpManager(fdump):
        pass
