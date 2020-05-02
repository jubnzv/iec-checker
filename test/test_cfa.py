"""Tests for control flow analysis inspections provided by OCaml core."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager # noqa


def test_cfa_dead_code():
    f = './test/st/dead-code.st'
    warns, rc = run_checker(f)
    assert rc == 0
    assert len(warns) == 1

    w = warns[0]
    assert w.id == 'UnreachableCode'
    assert w.linenr == 10
    assert w.column == 9

    dump_name = f'{f}.dump.json'
    with DumpManager(dump_name):
        pass
