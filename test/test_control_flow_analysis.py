import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker  # noqa
from python.dump import remove_dump  # noqa


# TODO:
def test_dead_code():
    f = './test/st/dead-code.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    # assert rc == 0
    # assert len(checker_warnings) == 1
    # cv = checker_warnings[0]
    # assert cv.id == 'DeclarationAnalysis'
    # assert cv.linenr == 8
    # assert cv.column == 31
    remove_dump(fdump)

