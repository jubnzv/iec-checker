"""Tests for generated control flow graph."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager # noqa


def test_cfg_structure():
    # TODO: Pass data from stdin.
    f = './test/st/cfg.st'
    _, rc = run_checker(f)
    assert rc == 0

    dump_name = f'{f}.dump.json'
    with DumpManager(dump_name) as dm:
        assert dm.scheme
        assert len(dm.scheme.cfgs) == 1
        cfg0 = dm.scheme.cfgs[0]
        assert len(cfg0.basic_blocks) == 4
