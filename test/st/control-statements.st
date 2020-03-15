(* This file contains various control statements of the ST language. *)

(* Table 72 -- №4 *)
PROGRAM square_root
  VAR
    a, b, c, d : REAL;
    x1, x2 : REAL;
    nroots : INT;
  END_VAR

  d := b*b - 4.0*a;
  d := b*b - 4.0*a*c;
  IF d < 0.0
  THEN nroots := 0;
    ELSIF d = 0.0
    THEN
      nroots := 1;
      X1 := -b/(2.0*A);
      X1 := -b;
    ELSE
      nroots := 2;
      X1 := (-b + SQRT(d))/(2.0*a);
      X2 := (-b - SQRT(d))/(2.0*a);
  END_IF;

END_PROGRAM

PROGRAM test_for
  VAR
    i, j : INT;
    flag : INT;
    counter : INT := 0;
    some_var : INT;
  END_VAR

  FOR i := 0 TO 10 DO
    FOR j := 10 TO 100 BY 2 DO
      IF flag THEN
        EXIT;
      END_IF;
      counter := counter + 1;
      IF j = 10 THEN
        CONTINUE;
      END_IF;
    END_FOR;
  END_FOR;
END_PROGRAM

PROGRAM test_switch_case
  VAR
    TW : INT;
    THUMBWHEEL : WORD;
    TW_ERROR : INT;
    some_var : INT;
  END_VAR

  TW := WORD_BCD_TO_INT(THUMBWHEEL);
  TW_ERROR:= 0;
  CASE TW OF
    1,5:     DISPLAY := OVEN_TEMP;
    2:       DISPLAY := MOTOR_SPEED;
    3:       DISPLAY := GROSS - TARE;
    4,6..10: DISPLAY := STATUS(TW-4);
  ELSE DISPLAY := 0;
       TW_ERROR:= 1;
  END_CASE;
  QW100:= INT_TO_BCD(DISPLAY);
END_PROGRAM
