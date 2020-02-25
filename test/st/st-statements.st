(* Test for various statements of ST language.
   See: table 71-72; chapter 7.3.3 -- operators *)

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

  (* Invocation statements *)
  acc := fn0();
  acc := fn0(19);
  acc := fn0(INVAL := 19);

END_PROGRAM
