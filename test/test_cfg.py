"""Tests for structure of generated control flow graph."""
import sys
import os
import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import check_program  # noqa
from python.dump import DumpManager  # noqa


def test_cfg_single_bb_has_exit_type():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM f
        VAR a : INT; END_VAR
        a := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 1
        # a := 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == set()


def test_cfg_single_bb_from_linear_statements_sequence():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM linear
        VAR a : INT; END_VAR
        a := 1;
        a := 2;
        a := 3;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 1
        # a := 1
        # a := 2
        # a := 3
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == set()


# No matter, I don't use this in checks for now. Anyway it would be completely
# refactored when added classes/methods.
@pytest.mark.skip(reason="TODO")
def test_cfg_function_calls_in_condition_stmt():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM func_calls
        VAR a : INT; END_VAR
        IF fn1(a1 := 19) AND fn2(a1 := 35, a2 := 40) THEN
            a := 30;
        END_IF;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 2
        # if .. then
        # and
        # fn1()
        # a := 19
        # fn2()
        # a := 35
        # a := 40
        assert len(bbs[0].stmt_ids) == 7


def test_cfg_single_if_statement_bb_has_exit_type():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM f
        VAR a : INT := 0; END_VAR
        IF a < 16 THEN
            a := 1;
        END_IF;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 2
        # if .. then
        # a < 16
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # a := 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BBExit"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == set()


def test_cfg_no_condition_self_reference():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM p
        VAR a : INT; i : INT; END_VAR
        a := 1;
        IF a > 1 THEN
            i := 1;
        END_IF;
        a := 2;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme and len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 3
        # a := 1
        # if ... then
        # a > 1
        assert bbs[0].preds == set()


def test_cfg_if_else():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM p
        VAR a : INT; i : INT; END_VAR
        a := 1;
        IF a > 1 THEN
            i := 1;
        ELSE
            i := 42;
        END_IF;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 4
        # a := 1
        # if ... then
        # a > 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1, 2}
        # i := 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {3}
        # else
        # i := 42
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {0}
        assert bbs[2].succs == {3}
        # i := 0
        assert bbs[3].id == 3
        assert bbs[3].type == "BBExit"
        assert bbs[3].preds == {1, 2}
        assert bbs[3].succs == set()


def test_cfg_if_elsif():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM f
        VAR a : INT; i : INT; END_VAR
        a := 1;
        IF a > 1 THEN
            i := 1;
        ELSIF a > 2 THEN
            i := 2;
        ELSIF a > 3 THEN
            i := 3;
        END_IF;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 7
        # a := 1
        # if ... then
        # a > 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1, 2}
        # i := 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {6}
        # elsif ... then
        # a > 2
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {0}
        assert bbs[2].succs == {3, 4}
        # i := 2
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {6}
        # elsif ... then
        # a > 3
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {2}
        assert bbs[4].succs == {5, 6}
        # i := 3
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == {4}
        assert bbs[5].succs == {6}
        # i := 0
        assert bbs[6].id == 6
        assert bbs[6].type == "BBExit"
        assert bbs[6].preds == {1, 3, 4, 5}
        assert bbs[6].succs == set()


def test_cfg_case_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM case_test
        VAR a : INT; END_VAR
        CASE a OF
          1 : a := 3;
          2 : a := 5;
          3,4 : a := 42;
        ELSE a := 19;
             a := 20;
        END_CASE;
        a := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 9
        # (switch) case .. of
        # a
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # (case) 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 3}
        # a := 3
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {8}
        # (case) 2
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {1}
        assert bbs[3].succs == {4, 5}
        # a := 5
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {3}
        assert bbs[4].succs == {8}
        # (case) 3
        # (case) 4
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == {3}
        assert bbs[5].succs == {6, 7}
        # a := 42
        assert bbs[6].id == 6
        assert bbs[6].type == "BB"
        assert bbs[6].preds == {5}
        assert bbs[6].succs == {8}
        # (else)
        # a := 19
        # a := 20
        assert bbs[7].id == 7
        assert bbs[7].type == "BB"
        assert bbs[7].preds == {5}
        assert bbs[7].succs == {8}
        # a := 0
        assert bbs[8].id == 8
        assert bbs[8].type == "BBExit"
        assert bbs[8].preds == {2, 4, 6, 7}
        assert bbs[8].succs == set()


def test_cfg_for_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_for
        VAR a : INT; i : INT; END_VAR
        FOR i := 0 TO 10 BY 2 DO
          a := a + i;
          a := a + 1;
        END_FOR;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 3
        # for
        # i := 0
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {1}
        assert bbs[0].succs == {1, 2}
        # a := a + i
        # a := a + 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {0}
        # i := 0
        assert bbs[2].id == 2
        assert bbs[2].type == "BBExit"
        assert bbs[2].preds == {0}
        assert bbs[2].succs == set()


def test_cfg_while_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_while
        VAR a : INT; i : INT; END_VAR
        WHILE i <= 10 DO
          a := i + 2;
          i := i - 1;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 3
        # while
        # i <= 10
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {1}
        assert bbs[0].succs == {1, 2}
        # a := i + 2
        # i := i - 1
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {0}
        # i := 0
        assert bbs[2].id == 2
        assert bbs[2].type == "BBExit"
        assert bbs[2].preds == {0}
        assert bbs[2].succs == set()


def test_cfg_while_nested_statements():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_cfg_while_nested_statements
        VAR a : INT; i : INT; END_VAR
        WHILE i < 10 DO
          a := 1;
          IF i > 10 THEN
            a := 2;
          END_IF;
          a := 3;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 5
        # while
        # i < 10
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {3}
        assert bbs[0].succs == {1, 4}
        # a := 1
        # if
        # if > 10
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 3}
        # a := 2
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # a := 3
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {1, 2}
        assert bbs[3].succs == {0}
        # i := 0
        assert bbs[4].id == 4
        assert bbs[4].type == "BBExit"
        assert bbs[4].preds == {0}
        assert bbs[4].succs == set()


def test_cfg_repeat_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_repeat
        VAR j : INT := 0; END_VAR
        REPEAT
          j := j + 2;
          UNTIL j < 100
        END_REPEAT;
        j := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 3
        # repeat
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {1}
        assert bbs[0].succs == {1}
        # j := j + 2
        # until
        # j < 100
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {0, 2}
        # j := 0
        assert bbs[2].id == 2
        assert bbs[2].type == "BBExit"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == set()


def test_cfg_func_call_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_func_call
        VAR j : INT := 0; END_VAR
        j := fn0(INVAL := 19);
        j := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 1
        # NOTE: I'm not sure about evaluation order for expressions in function
        # paramters. Need check how does it implemented in the modern IDEs.
        # Need revisit Mario de Sousa's paper for this topic.
        # j := fn0(INVAL := 19)
        #   INVAL := 19
        #   j := 0
        #   fn0()
        # P.S. This is a good idea for the additional inspection.
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == set()


def test_cfg_return_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        FUNCTION test_return : INT
        VAR A : INT; END_VAR
        A := 0;
        RETURN;
        A := 1;
        A := 42;
        END_FUNCTION
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.functions) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 2
        # a := 0
        # RETURN
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == set()


def test_cfg_single_return_statement():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        FUNCTION test_single_return : INT
        VAR A : INT; END_VAR
        RETURN;
        END_FUNCTION
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.functions) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 1
        # RETURN
        assert bbs[0].id == 0
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == set()
        assert bbs[0].succs == set()


def test_cfg_return_statement_inside_branch():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        FUNCTION test_return_nested : INT
        VAR i : INT; a : INT; END_VAR
        IF i = 0 THEN
          a := a + 1;
          RETURN;
          a := a + 2;
        END_IF;
        a := a + 3;
        END_FUNCTION
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.functions) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 4
        # if
        # i = 0
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1, 3}
        # a := a + 1
        # RETURN
        assert bbs[1].id == 1
        assert bbs[1].type == "BBExit"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == set()
        # a := a + 2
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == set()
        assert bbs[2].succs == {3}
        # a := a + 3
        assert bbs[3].id == 3
        assert bbs[3].type == "BBExit"
        assert bbs[3].preds == {0, 2}
        assert bbs[3].succs == set()


def test_cfg_exit():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_exit
        VAR a : INT; i : INT; END_VAR
        WHILE i < 10 DO
          IF i = 5 THEN
            i := i + 1;
            EXIT;
          END_IF;
          i := i + 2;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 5
        # while
        # i < 10
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {3}
        assert bbs[0].succs == {1, 4}
        # if
        # i = 5
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 3}
        # i := i + 1
        # exit
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {4}
        assert len(bbs[2].stmt_ids) == 2
        # i := i + 2
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {1}
        assert bbs[3].succs == {0}
        # i := 0
        assert bbs[4].id == 4
        assert bbs[4].type == "BBExit"
        assert bbs[4].preds == {0, 2}
        assert bbs[4].succs == set()


def test_cfg_continue():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_exit
        VAR a : INT; i : INT; END_VAR
        WHILE i < 10 DO
          IF i = 5 THEN
            i := i + 1;
            CONTINUE;
          END_IF;
          i := i + 2;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 5
        # while
        # i < 10
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {2, 3}
        assert bbs[0].succs == {1, 4}
        # if
        # i = 5
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 3}
        # i := i + 1
        # continue
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {0}
        assert len(bbs[2].stmt_ids) == 2
        # i := i + 2
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {1}
        assert bbs[3].succs == {0}
        # i := 0
        assert bbs[4].id == 4
        assert bbs[4].type == "BBExit"
        assert bbs[4].preds == {0}
        assert bbs[4].succs == set()


def test_cfg_single_continue_in_the_first_bb():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_continue
        VAR a : INT; i : INT; END_VAR
        WHILE i < 10 DO
          IF i = 5 THEN
            CONTINUE;
          END_IF;
          i := i + 2;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        bbs = dm.scheme.cfgs[0].basic_blocks
        assert len(bbs) == 5
        assert bbs[0].preds == {2, 3}
        assert bbs[2].succs == {0}
        assert len(bbs[2].stmt_ids) == 1


def test_cfg_exit_continue():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM test_exit_continue
        VAR a : INT; i : INT; END_VAR
        WHILE i <= 10 DO
          IF i = 5 THEN
            i := i + 1;
            EXIT;
          ELSIF i = 6 THEN
            CONTINUE;
            i := 3;
          END_IF;
          i := i + 2;
        END_WHILE;
        i := 0;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        assert len(scheme.cfgs) == 1
        cfg = scheme.cfgs[0]
        bbs = cfg.basic_blocks
        assert len(bbs) == 8
        # while
        # i <= 10
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {4, 6}
        assert bbs[0].succs == {1, 7}
        # if
        # i = 5
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 3}
        # i := i + 1
        # exit
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {7}
        assert len(bbs[2].stmt_ids) == 2
        # elsif
        # i = 6
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {1}
        assert bbs[3].succs == {4, 6}
        # continue
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {3}
        assert bbs[4].succs == {0}
        # i := 3
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == set()
        assert bbs[5].succs == {6}
        # i := i + 2
        assert bbs[6].id == 6
        assert bbs[6].type == "BB"
        assert bbs[6].preds == {5, 3}
        assert bbs[6].succs == {0}
        # i := 0
        assert bbs[7].id == 7
        assert bbs[7].type == "BBExit"
        assert bbs[7].preds == {0, 2}
        assert bbs[7].succs == set()
