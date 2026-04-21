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

def test_literal_elementary_ty_inference():
    """Test that CInteger, CReal, CBitString literals carry the correct
    elementary_ty when a type prefix is used, and carry None otherwise."""
    fdump = 'stdin.dump.json'
    _, rc = check_program("""
        PROGRAM p
        VAR
            (* Variable declaration and initialization - type-prefixed literals should infer concrete types *)
            a : INT := 123;           (* unprefixed integer → CInteger(ty=None) *)
            b : REAL := 1.23;         (* unprefixed real → CReal(ty=None) *)
            c : INT := INT#456;       (* INT# prefix → CInteger(ty=Some INT) *)
            d : REAL := REAL#7.89;    (* REAL# prefix → CReal(ty=Some REAL) *)
            e : UINT := UINT#789;     (* UINT# prefix → CInteger(ty=Some UINT) *)
            f : LREAL := LREAL#12.34; (* LREAL# prefix → CReal(ty=Some LREAL) *)
            g : BYTE := BYTE#16#FF;   (* BYTE# prefix → CBitString(ty=Some BYTE) *)
            h : WORD := WORD#16#ABCD; (* WORD# prefix → CBitString(ty=Some WORD) *)
        END_VAR

        (* Literals in assignment statements - also need to be verified *)
        a := 100;                    (* unprefixed → ty=None *)
        c := DINT#1000;             (* DINT# prefix → ty=Some DINT *)
        d := REAL#3.14;             (* REAL# prefix → ty=Some REAL *)
        END_PROGRAM
    """.replace('\n', ''))
    assert rc == 0

    with open(fdump, 'r') as fp:
        scheme = json.load(fp)

    # ------------------------------------------------------------------
    # Auxiliary recursive lookup function: extract all constant node 
    # information from the JSON tree
    # ------------------------------------------------------------------
    def find_constants(node):
        """Traverse the JSON tree recursively, returning a list of all constant nodes.
        
        Each element is a (tag, type_str_or_none, value) tuple:
          - tag: "Integer" / "Real" / "BitString"
          - type_str_or_none: typename string list or None
          - value: int or float
        """
        results = []
        if isinstance(node, list):
            # yojson encoding of variant: ["Tagname", arg0, arg1, arg2, ...]
            # Three forms of constant:
            #   ["Integer", ti_dict, type_opt, int_val]
            #   ["Real",    ti_dict, type_opt, float_val]
            #   ["BitString", ti_dict, type_opt, int_val]
            if (len(node) >= 4 and isinstance(node[0], str)
                    and node[0] in ("Integer", "Real", "BitString")):
                tag = node[0]
                # node[1] is Tok_info dict, node[2] is elementary_ty option, node[3] is value
                ty_opt = node[2]  # Array of strings such as ["INT"] or None
                val = node[3]
                results.append((tag, ty_opt, val))
            for child in node:
                results.extend(find_constants(child))
        elif isinstance(node, dict):
            for child in node.values():
                results.extend(find_constants(child))
        return results

    consts = find_constants(scheme)
    assert len(consts) > 0, 'no constants found in dump'

    # Build index by tag+value to assert
    by_key = {(tag, val): ty_opt for tag, ty_opt, val in consts}

    # ---- CInteger: no prefix → None -------------------------------------------------
    assert ("Integer", 123) in by_key
    assert by_key[("Integer", 123)] is None, \
        f'bare integer 123 should have ty=None, got {by_key[("Integer", 123)]}'

    assert ("Integer", 100) in by_key
    assert by_key[("Integer", 100)] is None, \
        f'bare integer 100 should have ty=None, got {by_key[("Integer", 100)]}'

    print(by_key)
    # ---- CInteger: type prefix → Some(typename) --------------------------------------
    assert ("Integer", 456) in by_key
    assert by_key[("Integer", 456)] == ["INT"], \
        f'INT#456 should have ty="INT", got {by_key[("Integer", 456)]}'

    assert ("Integer", 789) in by_key
    assert by_key[("Integer", 789)] == ["UINT"], \
        f'UINT#789 should have ty="UINT", got {by_key[("Integer", 789)]}'

    assert ("Integer", 1000) in by_key
    assert by_key[("Integer", 1000)] == ["DINT"], \
        f'DINT#1000 should have ty="DINT", got {by_key[("Integer", 1000)]}'

    # ---- CReal: no prefix → None ----------------------------------------------------
    # Find the Real that value≈1.23
    real_123 = [(tag, ty, v) for tag, ty, v in consts
                if tag == "Real" and abs(v - 1.23) < 1e-9]
    assert len(real_123) >= 1, 'no Real constant ~1.23 found'
    _, ty_123, _ = real_123[0]
    assert ty_123 is None, \
        f'bare real 1.23 should have ty=None, got {ty_123}'

    # ---- CReal: type prefix → Some(typename) ----------------------------------------
    real_789 = [(tag, ty, v) for tag, ty, v in consts
                if tag == "Real" and abs(v - 7.89) < 1e-9]
    assert len(real_789) >= 1, 'no Real constant ~7.89 found'
    _, ty_789, _ = real_789[0]
    assert ty_789 == ["REAL"], \
        f'REAL#7.89 should have ty="REAL", got {ty_789}'

    real_1234 = [(tag, ty, v) for tag, ty, v in consts
                 if tag == "Real" and abs(v - 12.34) < 1e-9]
    assert len(real_1234) >= 1, 'no Real constant ~12.34 found'
    _, ty_1234, _ = real_1234[0]
    assert ty_1234 == ["LREAL"], \
        f'LREAL#12.34 should have ty="LREAL", got {ty_1234}'

    real_314 = [(tag, ty, v) for tag, ty, v in consts
                if tag == "Real" and abs(v - 3.14) < 1e-9]
    assert len(real_314) >= 1, 'no Real constant ~3.14 found'
    _, ty_314, _ = real_314[0]
    assert ty_314 == ["REAL"], \
        f'REAL#3.14 should have ty="REAL", got {ty_314}'

    # ---- CBitString: type prefix → Some(typename) -----------------------------------
    # BYTE#16#FF → value is 255 (0xFF)
    assert ("BitString", 255) in by_key, \
        f'BYTE#16#FF (value=255) not found. All BitStrings: {[(t,v) for t,_,v in consts if t=="BitString"]}'
    assert by_key[("BitString", 255)] == ["BYTE"], \
        f'BYTE#16#FF should have ty="BYTE", got {by_key[("BitString", 255)]}'

    # WORD#16#ABCD → value is 43981 (0xABCD)
    assert ("BitString", 43981) in by_key, \
        f'WORD#16#ABCD (value=43981) not found'
    assert by_key[("BitString", 43981)] == ["WORD"], \
        f'WORD#16#ABCD should have ty="WORD", got {by_key[("BitString", 43981)]}'
    os.remove(fdump)
