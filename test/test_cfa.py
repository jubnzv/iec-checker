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


def test_cfa_dead_code_in_the_loops():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        PROGRAM dead_code_in_the_loops
        VAR a : INT; i : INT; END_VAR
        WHILE i < 10 DO
          IF i = 5 THEN
            i := i + 1;
            EXIT;
            i := 19; (* UnreachableCode error *)
            i := 42; (* No additional warnings *)
            i := 42;
          ELSIF i = 6 THEN
            CONTINUE;
            i := 3; (* UnreachableCode error *)
            i := 44; (* No additional warnings *)
            i := 19;
          END_IF;
          i := i + 2;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert len(filter_warns(warns, 'UnreachableCode')) == 2
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme


def test_cfa_multiple_pous():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        FUNCTION dead_code_after_return_1 : INT
          VAR some_var : INT; END_VAR
          RETURN;
          some_var := SQRT(16#42); (* UnreachableCode error *)
        END_FUNCTION

        FUNCTION dead_code_after_return_2 : INT
          VAR some_var : INT; END_VAR
          RETURN;
          some_var := SQRT(16#42); (* UnreachableCode error *)
        END_FUNCTION
        """.replace('\n', ''))
    assert rc == 0
    assert len(filter_warns(warns, 'UnreachableCode')) == 2
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
