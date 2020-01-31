(* Test lexing/parsing of literals. *)
PROGRAM program0
  VAR
    a : ULINT;
    b : BOOL;
    r : REAL;
    t : TIME;
    td : TOD;
    d : DATE;
    dtv : DATE_AND_TIME;

    (* The following identifiers are not defined as reserved keywords in the standard.
       So they could be used as a regular identifiers. *)
    T : INT;
    LT : INT;
    (* TODO: D : INT; *)
    LD : INT;
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

  (* real_lieral *)
  r := 1.0;
  r := 0.0;
  r := 3.000_300;
  r := +3.2;
  r := 3.2E1;
  r := +3.2E1;
  r := -3.2E1;
  r := -3.2E+1;
  r := -3.2E-1;
  r := REAL#2.0;
  r := REAL#-2.0;
  r := REAL#-2.0E8;
  r := REAL#-2.0E-8;

  (* time_literal *)
  t := TIME#3.0d;
  t := TIME#3d;
  t := TIME#3.0h;
  t := TIME#3.0m;
  t := TIME#3.0s;
  t := TIME#3.0ms;
  t := TIME#3.0us;
  t := TIME#3.0ns;
  t := TIME#3.0d 4.0h;
  t := TIME#3.0h3m;
  t := LTIME#3.0d;
  t := T#3.0d;
  t := LT#3.0d;
  t := TIME#3.0d;

  (* time_of_day *)
  td := TOD#03:12:20;
  td := LTOD#03:12:20;
  td := TIME_OF_DAY#03:12:20;
  td := LTIME_OF_DAY#03:12:20;
  td := LTIME_OF_DAY#03:12:20.5;

  (* date *)
  d := DATE#2020-01-30;
  d := LDATE#2020-01-30;
  d := D#2020-01-30;
  d := LD#2020-01-30;

  (* date_and_time *)
  dtv := LDT#2020-01-30-18:01:38;
  dtv := LDT#2020-01-30-18:01:38.33;
  dtv := DT#2020-01-30-18:01:38;
  dtv := DATE_AND_TIME#2020-01-30-18:01:38;
  dtv := LDATE_AND_TIME#2020-01-30-18:01:38;

END_PROGRAM
(* vim: set foldmethod=marker foldlevel=0 foldenable sw=2 tw=120 : *)
