(*
  Self-assignment detection test file.
  This file aims to cover all relevant AST cases.

  Test annotation:
    POSITIVE: should trigger self-assignment warning(s)
    NEGATIVE: should NOT trigger self-assignment warning(s)

  Expected total self-assignment warnings in this file : 16
*)



(* ---- FUNCTIONS ---- *)

(* POSITIVE: one (1) self-assignment warning expected *)
FUNCTION f0_p : INT
  VAR_INPUT
    n0 : INT := 0;
    n1 : INT := 0;
  END_VAR
  
  VAR
    x : INT := 0;
  END_VAR

  x := n1;
  x := x;
  f0_p := n0 + x;

END_FUNCTION

(* NEGATIVE *)
FUNCTION f0_n : INT
  VAR_INPUT
    n0 : INT := 1;
  END_VAR

  f0_n := n0 + n0;

END_FUNCTION

(* POSITIVE: three (3) self-assignment warnings expected *)
FUNCTION f1_p : BOOL
  VAR
    b0, b1, b2, b3 : BOOL := TRUE;
  END_VAR

  IF b0 THEN
    b0 := b0;
  ELSIF b1 THEN
    b1 := b1;
  ELSIF b2 THEN
    f1_p := TRUE;
  ELSE
    b3 := b3;
  END_IF

  f1_p := FALSE;

END_FUNCTION

(* NEGATIVE *)
FUNCTION f1_n : BOOL
  VAR
    b0, b1 : BOOL := FALSE;
  END_VAR

  IF NOT b0 THEN
    b0 := FALSE;
    b1 := FALSE;
  ELSIF b1 THEN
    b0 := TRUE;
    b1 := TRUE;
  ELSE
    b0 := FALSE;
    b1 := TRUE;
  END_IF

  f1_n := b0 OR b1;

END_FUNCTION

(* POSITIVE: two (2) self-assignment warnings expected *)
FUNCTION f2_p : CHAR
  VAR
    c0, c1 : CHAR := '.';
  END_VAR

  CASE c0 OF
  'y':
      c0 := 'y';
      c1 := c1;
  'n':
      c1 := 'n';
  ELSE
      c1 := (c1);
  END_CASE;

  f2_p := 'x';

END_FUNCTION

(* NEGATIVE *)
FUNCTION f2_n : CHAR
  VAR
    c0, c1 : CHAR := '.';
  END_VAR

  IF c1 = 's' THEN
    CASE c0 OF
    '0':
        c0 := '1';
    '1':
        c0 := '0';
    ELSE
      c0 := 'e';
    END_CASE;
  ELSE
    ;
  END_IF

  f2_n := c0;

END_FUNCTION

(* POSITIVE: two (2) self-assignment warnings expected *)
FUNCTION f3_p : WORD
  VAR
    w0 : WORD := 16#0000;
    n0 : SINT := 0;
  END_VAR

  FOR n0 := n0 TO 10 BY 2 DO
    w0 := w0;
  END_FOR

  f3_p := w0;

END_FUNCTION

(* NEGATIVE *)
FUNCTION f3_n : WORD
  VAR_INPUT
    w0 : WORD := 16#0000;
  END_VAR

  VAR_TEMP
    i : UINT := 0;
    result : WORD := 16#1234;
  END_VAR

  result := w0;

  FOR i := 0 TO 100 DO
    result := result + 1;
  END_FOR;

  f3_n := result;

END_FUNCTION

(* POSITIVE: one (1) self-assignment warning expected *)
FUNCTION f4_p : SINT
  VAR_TEMP
    x : SINT := 127;
    y : UINT := 34873;
  END_VAR

  WHILE x > 0 DO
    x := x - 1;
    y := y;   (* should trigger warning *)
  END_WHILE;

f4_p := x;

END_FUNCTION

(* POSITIVE: one (1) self-assignment warning expected *)
FUNCTION f5_p : LREAL
  VAR
    lr0 : LREAL := 100.0;
    lr1 : LREAL := -100.0;
    lr2 : REAL := 45.50;
  END_VAR

  REPEAT
    lr0 := lr1 - 0.1;
    lr1 := lr1 + 0.1;
    lr2 := lr2;
  UNTIL
    lr0 <> lr1
  END_REPEAT

  f5_p := lr2 * 2;

END_FUNCTION

(* NEGATIVE *)
FUNCTION f5_n : LREAL
  VAR
    lr0 : LREAL := 8192.0;
  END_VAR

  VAR
    lr1 : LREAL := 0.0;
  END_VAR

  REPEAT
    lr0 := lr0 / 2;
    lr1 := lr1 + 1.0;
  UNTIL
    lr0 < 1.0
  END_REPEAT

  f5_n := lr1;

END_FUNCTION

(* POSITIVE: two (2) self-assignment warnings expected *)
FUNCTION f6_p : UINT
  VAR_TEMP
    i0 : INT := 4;
    u0 : UINT := 102;
    b0 : BOOL := TRUE;
  END_VAR

  i0 := f0_n (i0 := i0)

  b4 := NOT (f1_n (b0 := b0));

  f6_p := u0 / 2;

END_FUNCTION

(* NEGATIVE *)
FUNCTION f6_n : UINT
  VAR_TEMP
    u0 : UINT := 47;
    b0 : BOOL := FALSE;
  END_VAR

  IF f1_n (b0 := TRUE) THEN
    u0 := u0 * 2;
  ELSE
    ;
  END_IF

  f6_n := u0;

END_FUNCTION

(* ---- FUNCTION BLOCKS ---- *)

(* POSITIVE: one (1) self-assignment warning expected *)
FUNCTION_BLOCK fb0_p
  VAR_TEMP
    k : REAL := 3.14;
  END_VAR

  k := k * k;
  k := k;

END_FUNCTION_BLOCK

(* NEGATIVE *)
FUNCTION_BLOCK fb0_n
  VAR_TEMP
    x : INT := 42;
  END_VAR

  IF x > 12 THEN
    x := x * 2;
  ELSE
    x := x / 2;
  END_IF

END_FUNCTION_BLOCK

(* ---- PROGRAMS ---- *)

(* POSITIVE: one (1) self-assignment warning expected *)
PROGRAM program0_p
  VAR
    x : INT := 0;
    y : INT := 4;
  END_VAR

  x := x + 8;
  y := y;

END_PROGRAM

(* NEGATIVE *)
PROGRAM program0_n
  VAR
    a : INT := 2;
    b : INT := 4;
  END_VAR

  a := b;
  b := a;

END_PROGRAM



(* CLASSES *)

(* POSITIVE: two (2) self-assignment warnings expected *)
CLASS class0_p
  VAR
    a : REAL := 1.0120;
    b : REAL := 6.1094;
  END_VAR

  METHOD PUBLIC m0
    a := a;
  END_METHOD

  METHOD PUBLIC m1
    b := b * 2;
  END_METHOD

  METHOD PRIVATE m2
    b := a;
  END_METHOD

  METHOD PRIVATE m3
    b := (b);
  END_METHOD

END_CLASS

(* NEGATIVE *)
CLASS class0_n
  VAR
    x : INT := 0;
    y : INT := 1;
    z : INT := 1;
    tmp : INT := 0;
  END_VAR

  METHOD PUBLIC fibo
    tmp := y;
    y := z;
    z := z + tmp;
    x := tmp;
  END_METHOD

END_CLASS
