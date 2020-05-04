"""Tests for structure of generated control flow graph."""
import sys
import os

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
        assert bbs[0].preds == []
        assert bbs[0].succs == []


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
        assert bbs[0].type == "BBExit"
        assert bbs[0].preds == []
        assert bbs[0].succs == [1]
        # a < 16
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == [1]
        assert bbs[1].succs == [2]
        # a := 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == [1]
        assert bbs[2].succs == []


def test_cfg_if():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM f
        VAR a : INT; i : INT; END_VAR
        a := 1;
        IF a > 1 THEN
            i := 1;
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
        assert len(bbs) == 5
        # a := 1
        assert bbs[0].id == 0
        assert bbs[0].type == "BBEntry"
        assert bbs[0].preds == []
        assert bbs[0].succs == [1]
        # if ... then
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == [0]
        assert bbs[1].succs == [2]
        # a > 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == [1]
        assert bbs[2].succs == [3, 4]
        # i := 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == [2]
        assert bbs[3].succs == [4]
        # i := 0
        assert bbs[4].id == 4
        assert bbs[4].type == "BBExit"
        assert bbs[4].preds == [2, 3]
        assert bbs[4].succs == []


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
        assert bbs[0].preds == []
        assert bbs[0].succs == [1]
        # if ... then
        assert bbs[1].id == 1
        assert bbs[1].type == "BB"
        assert bbs[1].preds == [0]
        assert bbs[1].succs == [2]
        # a > 1
        assert bbs[2].id == 2
        assert bbs[2].type == "BB"
        assert bbs[2].preds == [1]
        assert bbs[2].succs == [3, 4]
        # i := 1
        assert bbs[3].id == 3
        assert bbs[3].type == "BB"
        assert bbs[3].preds == [2]
        assert bbs[3].succs == [5]
        # i := 42
        assert bbs[4].id == 4
        assert bbs[4].type == "BB"
        assert bbs[4].preds == [2]
        assert bbs[4].succs == [5]
        # i := 0
        assert bbs[5].id == 5
        assert bbs[5].type == "BBExit"
        assert bbs[5].preds == [3, 4]
        assert bbs[5].succs == []
