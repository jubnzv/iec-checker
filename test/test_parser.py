"""Tests for parser and lexer."""
import sys
import os
import json

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, check_program  # noqa
from python.dump import DumpManager  # noqa


def _find_bool_constants(node):
    if isinstance(node, list):
        if (len(node) == 3 and node[0] == 'Bool'
                and isinstance(node[2], bool)):
            yield node[2]
        for child in node:
            yield from _find_bool_constants(child)
    elif isinstance(node, dict):
        for child in node.values():
            yield from _find_bool_constants(child)


def test_bool_literals_preserve_value():
    """`TRUE` must be lexed as the boolean value `true`, not `false`."""
    dump = 'stdin.dump.json'
    _, rc = check_program(
        """
        PROGRAM p
        VAR t : BOOL := TRUE; f : BOOL := FALSE; END_VAR
        t := TRUE;
        f := FALSE;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    try:
        with open(dump, 'r') as fp:
            scheme = json.load(fp)
        bools = list(_find_bool_constants(scheme))
        assert len(bools) > 0, 'no Bool constants found in dump'
        assert bools.count(True) > 0, (
            f'every Bool literal serialized as `false` — '
            f'`bool_true` lexer rule is flipped again. All bools: {bools}')
        assert bools.count(True) == bools.count(False), (
            f'asymmetric counts — TRUEs={bools.count(True)}, '
            f'FALSEs={bools.count(False)}, all={bools}')
    finally:
        if os.path.exists(dump):
            os.remove(dump)


def test_lexing_error():
    f = 'st/bad/lexing-error.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 1
    assert len(checker_warnings) == 1
    cv = checker_warnings[0]
    assert cv.id == 'LexingError'
    assert cv.linenr == 10
    assert cv.column == 6
    with DumpManager(fdump):
        pass


def test_lexing_error_context_snippet():
    """LexingError must carry the offending source line and a caret."""
    f = 'st/bad/lexing-error.st'
    fdump = f'{f}.dump.json'
    checker_warnings, _ = run_checker([f])
    cv = checker_warnings[0]
    assert cv.id == 'LexingError'
    assert 'wtf?' in cv.context
    assert '10 |' in cv.context
    # Caret must land under column 6 (the '?' char)
    caret_line = [l for l in cv.context.splitlines() if '^' in l][0]
    assert caret_line == '     |      ^'
    with DumpManager(fdump):
        pass


def test_parser_error_context_snippet():
    """ParserError must name the offending token and show a source snippet."""
    f = 'st/bad/semantic-error.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 1
    parser_warns = [w for w in checker_warnings if w.id == 'ParserError']
    assert len(parser_warns) == 1
    cv = parser_warns[0]
    assert cv.msg == 'unexpected token `"`'
    assert 'ANALOG_DATA_BAD' in cv.context
    assert '3 |' in cv.context
    assert '^' in cv.context
    with DumpManager(fdump):
        pass


def test_no_parser_error_no_context():
    """Valid programs must not emit any parser/lexer warnings with context."""
    _, rc = check_program(
        """
        PROGRAM p
        VAR a : INT; END_VAR
        a := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager('stdin.dump.json'):
        pass


def test_parser_errors():
    for fname in os.listdir('st/bad/'):
        f = os.path.join('st/bad/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker([f])
        assert rc == 1, f"Incorrect exit code for {f}"
        assert len(checker_warnings) > 0
        with DumpManager(fdump):
            pass


def test_no_parser_errors():
    for fname in os.listdir('st/good/'):
        if not fname.endswith('.st'):
            continue
        f = os.path.join('st/good/', fname)
        fdump = f'{f}.dump.json'
        checker_warnings, rc = run_checker([f])
        assert rc == 0, f"Incorrect exit code for {f}"
        with DumpManager(fdump):
            pass


def test_direct_variables():
    f = 'st/good/direct-variables.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump) as dm:
        _ = dm.scheme  # TODO


def test_statements_order():
    """Test that POU statements are arranged in the correct order."""
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        PROGRAM p
        VAR a : INT; i : INT; END_VAR
        a := 1;
        i := 22;
        a := 16#42;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.programs) == 1
        # TODO: need recursive traverse in om
        # p = scheme.programs[0]
        # assert len(p.statements) == 3


def test_enum_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE
          Traffic_Light: (Red, Amber, Green);
        END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'TRAFFIC_LIGHT'
        assert ty.type == 'Enum'


def test_struct_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE
          Cooler: STRUCT
            Temp: INT;
            Cooling: TOF;
          END_STRUCT;
        END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'COOLER'
        assert ty.type == 'Struct'


def test_ref_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE myRef: REF_TO INT; END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'MYREF'
        assert ty.type == 'Ref'


def test_array_types():
    fdump = f'stdin.dump.json'
    checker_warnings, rc = check_program(
        """
        TYPE BITS: ARRAY [0..7] OF BOOL; END_TYPE
        """.replace('\n', ''))
    assert rc == 0
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme
        assert len(scheme.types) == 1
        ty = scheme.types[0]
        assert ty.name == 'BITS'
        assert ty.type == 'Array'
