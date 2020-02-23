(* Test for various statements of ST language.
   See: table 71-72; chapter 7.3.3 -- operators *)
PROGRAM p0
  VAR_INPUT
    vi1 : INT;
  END_VAR
  VAR_OUTPUT
    vo1 : INT;
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

  FOR i := 1 TO 100 DO
    acc := acc * x;
  END_FOR;

  FOR i := 100 TO 0 BY -2 DO
    acc := acc * x;
  END_FOR;

END_PROGRAM
