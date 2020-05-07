"""Tests for control flow analysis inspections provided by OCaml core."""
import sys
import os
import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import check_program, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_cfa_dead_code_top_statements():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        FUNCTION test_dead_code_to_stmts : INT
          VAR
            counter : INT := 0;
            some_var : INT;
          END_VAR
          counter := counter + 1;
          counter := 2 + 2;
          RETURN;
          some_var := SQRT(16#42); (* UnreachableCode error *)
          some_var := 16#42; (* No additional warnings *)
          some_var := 19;
        END_FUNCTION
        """.replace('\n', ''))
    assert rc == 0
    assert len(warns) >= 1
    assert len(filter_warns(warns, 'UnreachableCode')) == 1
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
