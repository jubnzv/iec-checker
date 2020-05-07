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
        assert len(bbs) == 3
        # if .. then
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # a < 16
        assert bbs[1].id == 1
        assert bbs[1].type == "BBExit"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # a := 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BBExit"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == set()


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
        assert len(bbs) == 6
        # a := 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # if ... then
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # a > 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3, 4}
        # i := 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {5}
        # i := 42
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {2}
        assert bbs[4].succs == {5}
        # i := 0
        assert bbs[5].id == 5
        assert bbs[5].type == "BBExit"
        assert bbs[5].preds == {3, 4}
        assert bbs[5].succs == set()


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
        assert len(bbs) == 11
        # a := 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # if ... then
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # a > 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3, 4, 10, 7}
        # i := 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {10}
        # elsif ... then
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {2}
        assert bbs[4].succs == {5}
        # a > 2
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == {4}
        assert bbs[5].succs == {6}
        # i := 2
        assert bbs[6].id == 6
        assert bbs[6].type == "BB"
        assert bbs[6].preds == {5}
        assert bbs[6].succs == {10}
        # elsif ... then
        assert bbs[7].id == 7
        assert bbs[7].type == "BB"
        assert bbs[7].preds == {2}
        assert bbs[7].succs == {8}
        # a > 3
        assert bbs[8].id == 8
        assert bbs[8].type == "BB"
        assert bbs[8].preds == {7}
        assert bbs[8].succs == {9}
        # i := 3
        assert bbs[9].id == 9
        assert bbs[9].type == "BB"
        assert bbs[9].preds == {8}
        assert bbs[9].succs == {10}
        # i := 0
        assert bbs[10].id == 10
        assert bbs[10].type == "BBExit"
        assert bbs[10].preds == {9, 2, 3, 6}
        assert bbs[10].succs == set()


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
        assert len(bbs) == 12
        # case .. of
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # a
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 4, 6, 7, 9}
        # 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # a := 3
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {11}
        # 2
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {1}
        assert bbs[4].succs == {5}
        # a := 5
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == {4}
        assert bbs[5].succs == {11}
        # 3
        assert bbs[6].id == 6
        assert bbs[6].type == "BB"
        assert bbs[6].preds == {1}
        assert bbs[6].succs == {8}
        # 4
        assert bbs[7].id == 7
        assert bbs[7].type == "BB"
        assert bbs[7].preds == {1}
        assert bbs[7].succs == {8}
        # a := 42
        assert bbs[8].id == 8
        assert bbs[8].type == "BB"
        assert bbs[8].preds == {6, 7}
        assert bbs[8].succs == {11}
        # a := 19
        assert bbs[9].id == 9
        assert bbs[9].type == "BB"
        assert bbs[9].preds == {1}
        assert bbs[9].succs == {10}
        # a := 20
        assert bbs[10].id == 10
        assert bbs[10].type == "BB"
        assert bbs[10].preds == {9}
        assert bbs[10].succs == {11}
        # a := 0
        assert bbs[11].id == 11
        assert bbs[11].type == "BBExit"
        assert bbs[11].preds == {8, 3, 5, 10}
        assert bbs[11].succs == set()


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
        assert len(bbs) == 5
        # for
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {3}
        assert bbs[0].succs == {1, 4}
        # i := 0
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # a := a + i
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # a := a + 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {0}
        # i := 0
        assert bbs[4].id == 4
        assert bbs[4].type == "BBExit"
        assert bbs[4].preds == {0}
        assert bbs[4].succs == set()


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
        assert len(bbs) == 5
        # while
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {3}
        assert bbs[0].succs == {1, 4}
        # i <= 10
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # a := i + 2
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # i := i - 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
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
        assert len(bbs) == 4
        # repeat
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {2}
        assert bbs[0].succs == {1}
        # j := j + 2
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # j < 100
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {0, 3}
        # j := 0
        assert bbs[3].id == 3
        assert bbs[3].type == "BBExit"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == set()


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
        assert len(bbs) == 4
        # j := fn0(INVAL := 19)
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1, 3}
        # fn0()
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0, 2}
        assert bbs[1].succs == {2}
        # INVAL := 19
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {1}
        # j := 0
        assert bbs[3].id == 3
        assert bbs[3].type == "BBExit"
        assert bbs[3].preds == {0}
        assert bbs[3].succs == set()


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
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # RETURN
        assert bbs[1].id == 1
        assert bbs[1].type == "BBExit"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == set()


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
        assert len(bbs) == 6
        # IF
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == set()
        assert bbs[0].succs == {1}
        # i = 0
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2, 5}
        # a := a + 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # RETURN
        assert bbs[3].id == 3
        assert bbs[3].type == "BBExit"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == set()
        # a := a + 2
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == set()
        assert bbs[4].succs == {5}
        # a := a + 3
        assert bbs[5].id == 5
        assert bbs[5].type == "BBExit"
        assert bbs[5].preds == {1, 4}
        assert bbs[5].succs == set()


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
        assert len(bbs) == 12
        # while
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == {8, 10}
        assert bbs[0].succs == {11, 1}
        # i <= 10
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == {0}
        assert bbs[1].succs == {2}
        # if
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == {1}
        assert bbs[2].succs == {3}
        # i = 5
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == {2}
        assert bbs[3].succs == {4, 6, 10}
        # i := i + 1
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == {3}
        assert bbs[4].succs == {5}
        # exit
        assert bbs[5].id == 5
        assert bbs[5].type == "BB"
        assert bbs[5].preds == {4}
        assert bbs[5].succs == {11}
        # elsif
        assert bbs[6].id == 6
        assert bbs[6].type == "BB"
        assert bbs[6].preds == {3}
        assert bbs[6].succs == {7}
        # i = 6
        assert bbs[7].id == 7
        assert bbs[7].type == "BB"
        assert bbs[7].preds == {6}
        assert bbs[7].succs == {8}
        # continue
        assert bbs[8].id == 8
        assert bbs[8].type == "BB"
        assert bbs[8].preds == {7}
        assert bbs[8].succs == {0}
        # i := 3
        assert bbs[9].id == 9
        assert bbs[9].type == "BB"
        assert bbs[9].preds == set()
        assert bbs[9].succs == {10}
        # i := i + 2
        assert bbs[10].id == 10
        assert bbs[10].type == "BB"
        assert bbs[10].preds == {9, 3}
        assert bbs[10].succs == {0}
        # i := 0
        assert bbs[11].id == 11
        assert bbs[11].type == "BBExit"
        assert bbs[11].preds == {5, 0}
        assert bbs[11].succs == set()
