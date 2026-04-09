import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, check_program, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_use_define_array():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        PROGRAM test_arr_len
          VAR
            ARR1: ARRAY [1..2] OF BOOL;
          END_VAR
          ARR1[0] := 19; (* error *)
          ARR1[1] := 19; (* no false positive *)
          ARR1[2] := 19; (* no false positive *)
          ARR1[3] := 19; (* error *)
          ARR1[2,1] := 19; (* error *)
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert len(warns) >= 3
    ws = filter_warns(warns, 'OutOfBounds')
    assert len(ws) == 3
    msgs = [w.msg for w in ws]
    assert any("index 0 is out" in m for m in msgs)
    assert any("index 3 is out" in m for m in msgs)
    assert any("addressed to 2 dimension" in m for m in msgs)
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
