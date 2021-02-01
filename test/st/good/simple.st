PROGRAM Simple
  VAR
    temp : DINT;
    i : DINT;
    arr1: ARRAY [1..2] OF BOOL;
  END_VAR

  VAR
    unused_var AT %IW1.2 : REAL := 200.0;
    head AT %B0 : INT;
  END_VAR

  WHILE i < 10 DO
    IF i = 5 THEN
      temp := i;
      EXIT;
      i := 42;
    END_IF;
    i := i + 1;
  END_WHILE;

  ARR1[3] := 19;
  ARR1[2,1] := 19;
  %B0 := 42;
END_PROGRAM
