import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from plugins.core import run_checker  # noqa
from plugins.dump import process_dump, remove_dump  # noqa


def test_parser_error():
    f = './test/st/parser-errors.st'
    checker_warnings, errors, rc = run_checker(f)
    assert rc == 1


def test_configurations():
    f = './test/st/configurations.st'

    checker_warnings, errors, rc = run_checker(f)
    assert rc == 0

    dump_name = f'{f}.dump.json'
    process_dump(dump_name)
    remove_dump(dump_name)
