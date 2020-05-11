import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import check_program, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_unused_local_variable():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        PROGRAM p
        VAR
          a : INT;
          b : INT;
          c : INT;
        END_VAR
        b := 1 + c;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert len(filter_warns(warns, 'UnusedVariable')) == 1
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
