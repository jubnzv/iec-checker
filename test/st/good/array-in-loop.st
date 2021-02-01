PROGRAM Simple

  VAR
    i : DINT;
    arr1: ARRAY [1..2] OF BOOL;
  END_VAR

  VAR
    unused_var AT %IW1.2 : REAL := 200.0;
  END_VAR

  IF ARR1[i] < 10 THEN
    i := i + 1;
  END_IF;
END_PROGRAM
