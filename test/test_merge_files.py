"""Test if the checker merges multiple input files correctly."""
import sys
import os
import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa


def test_merge_multiple_files():
    files = ['st/merge-1.st', 'st/merge-2.st']
    fdump = 'merged-input.st.dump.json'
    checker_warnings, rc = run_checker(files, args=["-m"])
    assert rc == 0
    with DumpManager(fdump):
        pass

def test_merge_multiple_files_different_order():
    files = ['st/merge-2.st', 'st/merge-1.st']
    fdump = 'merged-input.st.dump.json'
    checker_warnings, rc = run_checker(files, args=["-m"])
    assert rc == 0
    with DumpManager(fdump):
        pass

def merge_is_disabled_for_a_single_input_file():
    files = ['st/merge-2.st']
    fdump = 'merged-input.st.dump.json'
    checker_warnings, rc = run_checker(files, args=["-m"])
    assert rc == 0
    with DumpManager(fdump):
        pass
