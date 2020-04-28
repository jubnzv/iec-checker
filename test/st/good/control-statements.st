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

FUNCTION fn0 : INT
  VAR_INPUT
    INVAL : INT;
  END_VAR
  VAR_OUTPUT
    OUTVAL : INT;
  END_VAR

  OUTVAL := 0;
END_FUNCTION

PROGRAM p0
  VAR_INPUT
    vi1 : INT;
  END_VAR
  VAR_OUTPUT
    vo1 : INT;
    vo2 : INT;
  END_VAR
  VAR
    i : INT;
    x : INT;
    acc : INT;
    j : INT;
  END_VAR

  IF (vi1 = 0)
    THEN vo1 := 0;
    ELSE vo1 := vi1 - 42;
  END_IF;

  CASE vi1 OF
    1 : vo1 := 19;
    2 : vo1 := 29;
    3,4 : vo1 := 39;
    (* TODO: 3..10: vo1 := 42; *)
    ELSE vo1 := 1; vo2 := -1;
  END_CASE;

  FOR i := 1 TO 100 DO
    acc := acc * x;
  END_FOR;

  FOR i := 100 TO 0 BY -2 DO
    acc := acc * x;
  END_FOR;

  J := 1;
  WHILE J <= 100 DO
    J := J + 2;
  END_WHILE;

  J := -1;
  REPEAT
    J := J + 2;
    UNTIL J = 101
  END_REPEAT;

  (* Invocation statements *)
  acc := fn0();
  acc := fn0(19);
  acc := fn0(INVAL := 19);

END_PROGRAM
