(* Test lexing/parsing of integer literals defined in Table 5. *)
PROGRAM program0
  VAR
    a : ULINT;
    b : BOOL;
  END_VAR

  (* integer *)
  a := 3;
  a := 3_000;
  a := 3_000_000;
  a := 3_00_0000;
  a := ULINT#3;
  a := ULINT#3_000;

  (* binary_integer *)
  a := 2#1;
  a := 2#010110;
  a := 2#11_011_0;
  a := ULINT#2#1;
  a := ULINT#2#1_000;

  (* octal_integer *)
  a := 8#7;
  a := 8#71_061_0;
  a := ULINT#8#7;
  a := ULINT#8#7_000;

  (* hex_integer *)
  a := 16#0;
  a := 16#10;
  a := 16#A;
  a := 16#A_B_C;
  a := ULINT#16#A;
  a := ULINT#16#A_B00;

  (* bool_literal *)
  b := 1;
  b := 0;
  b := FALSE;
  b := TRUE;
  b := BOOL#1;
  b := BOOL#0;
  b := BOOL#FALSE;
  b := BOOL#TRUE;

END_PROGRAM

