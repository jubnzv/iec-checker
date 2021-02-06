FUNCTION_BLOCK Example
    VAR_EXTERNAL
        global_var : INT;
    END_VAR
    global_var := 42;
END_FUNCTION_BLOCK

PROGRAM Simple
  VAR_INPUT
    x : TIME;
  END_VAR
  VAR
    temp : DINT;
    i : DINT;
    arr1: ARRAY [1..2] OF BOOL;
    unused_var AT %IW1.2 : REAL := 200.0;
    head AT %B0 : INT;
  END_VAR

  WHILE i < 10 DO
    IF i = 5 THEN
      temp := i;
      EXIT;
      i := 42;
    END_IF
    i := i + 1;
  END_WHILE

  ARR1[3] := 19;
  ARR1[2,1] := 19;
  IF x = T#100MS THEN
    %B0 := 42;
  END_IF
END_PROGRAM
