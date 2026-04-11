"""Tests for PLCOpen inspections."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_cp1():
    f = 'st/plcopen-cp1.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP1') == 1
    with DumpManager(fdump):
        pass


def test_cp3():
    f = 'st/plcopen-cp3.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP3') == 8
    with DumpManager(fdump):
        pass


def test_cp6():
    f = 'st/plcopen-cp6.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP6') == 2
    with DumpManager(fdump):
        pass


def test_cp8():
    f = 'st/plcopen-cp8.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP8') == 4
    with DumpManager(fdump):
        pass


def test_cp28():
    f = 'st/plcopen-cp28.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP28') == 4
    with DumpManager(fdump):
        pass


def test_cp13():
    f = 'st/plcopen-cp13.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP13') == 3
    with DumpManager(fdump):
        pass


def test_cp25():
    f = 'st/plcopen-cp25.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-CP25') == 2
    with DumpManager(fdump):
        pass


def test_l10():
    f = 'st/plcopen-l10.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    checker_warnings.count('PLCOPEN-L10') == 3
    with DumpManager(fdump):
        pass


def test_l17():
    f = 'st/plcopen-l17.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    l17_warns = filter_warns(checker_warnings, 'PLCOPEN-L17')
    assert any(w.linenr == 10 and w.column == 4 for w in l17_warns)
    with DumpManager(fdump):
        pass


def test_n3():
    f = 'st/plcopen-n3.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    n3_warns = filter_warns(checker_warnings, 'PLCOPEN-N3')
    assert any(w.linenr == 6 and w.column == 7 for w in n3_warns)
    with DumpManager(fdump):
        pass


def test_cp9():
    f = 'st/plcopen-cp9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-CP9')) == 2
    with DumpManager(fdump):
        pass


def test_n1():
    f = 'st/plcopen-n1.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N1')) == 1
    with DumpManager(fdump):
        pass


def test_n2():
    f = 'st/plcopen-n2.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n2.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N2')) == 2
    with DumpManager(fdump):
        pass


def test_n4():
    f = 'st/plcopen-n4.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n4.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N4')) == 2
    with DumpManager(fdump):
        pass


def test_n5():
    f = 'st/plcopen-n5.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N5')) == 1
    with DumpManager(fdump):
        pass


def test_n6():
    f = 'st/plcopen-n6.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n6.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N6')) == 2
    with DumpManager(fdump):
        pass


def test_n8():
    f = 'st/plcopen-n8.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N8')) == 0
    with DumpManager(fdump):
        pass


def test_n9():
    f = 'st/plcopen-n9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N9')) == 2
    with DumpManager(fdump):
        pass


def test_n10():
    f = 'st/plcopen-n10.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n10.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N10')) == 2
    with DumpManager(fdump):
        pass
