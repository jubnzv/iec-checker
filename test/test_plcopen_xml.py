"""Tests PLCOpen XML parser."""
import sys
import os
import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa


@pytest.mark.skip(reason="TODO")
def test_no_parser_errors():
    f = os.path.join('./test/plcopen/example.xml')
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f, '-i', 'xml')
    assert rc == 0, f"Incorrect exit code for {f}"
    with DumpManager(fdump):
        pass
