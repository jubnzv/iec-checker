import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import check_program, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_unused_local_variable():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        PROGRAM p
        VAR
          a : INT;
          b : INT;
          c : INT;
        END_VAR
        b := 1 + c;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert len(filter_warns(warns, 'UnusedVariable')) == 1
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme


def test_struct_member_access_counts_as_use():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE MyStruct : STRUCT A : INT; B : INT; END_STRUCT; END_TYPE
        PROGRAM p
        VAR ms : MyStruct; END_VAR
        ms.B := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 0, (
        f'ms is used via member access, must not be reported unused; '
        f'got: {[(w.linenr, w.msg) for w in unused]}')
    with DumpManager(fdump):
        pass


def test_struct_nested_member_access_counts_as_use():
    """Accessing `a.b.c` must count as a use of `a` (deep chain)."""
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE
          Inner : STRUCT x : INT; END_STRUCT;
          Outer : STRUCT inn : Inner; END_STRUCT;
        END_TYPE
        PROGRAM p
        VAR o : Outer; END_VAR
        o.inn.x := 42;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 0, (
        f'o used via nested member access, must not be reported unused; '
        f'got: {[(w.linenr, w.msg) for w in unused]}')
    with DumpManager(fdump):
        pass


def test_truly_unused_struct_still_warned():
    """A struct never referenced at all must still be flagged."""
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE MyStruct : STRUCT A : INT; END_STRUCT; END_TYPE
        PROGRAM p
        VAR ms : MyStruct; other : INT; END_VAR
        other := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 1, (
        f'ms is never accessed, must be flagged; got {len(unused)} warnings')
    assert 'MS' in unused[0].msg
    with DumpManager(fdump):
        pass
