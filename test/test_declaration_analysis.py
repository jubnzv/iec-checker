import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, check_program, filter_warns  # noqa
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


def test_array_initialized_list():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE BITS: ARRAY [1..2, 1..3] OF BOOL := [0,0,0,0,0,0,1,1,1]; END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    assert len(warns) >= 1
    w0 = filter_warns(warns, 'OutOfBounds')[0]
    assert w0.id == 'OutOfBounds'
    assert '3 values will be lost' in w0.msg
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
