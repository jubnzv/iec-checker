"""Tests for self-assignment detection."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa


def test_self_assignment():
    f = "st/self-assignment.st"
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    # 16 self-assignments in the test file are expected
    assert checker_warnings.count("SELF-ASSIGNMENT") == 16
