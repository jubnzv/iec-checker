"""Tests for internal errors."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa


def test_missing_file():
    f = './test/st/foo.bar'
    checker_warnings, rc = run_checker(f)
    assert rc == 127
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'FileNotFoundError'
