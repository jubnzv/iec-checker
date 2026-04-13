PROGRAM l13
  VAR
    i : INT := 0;
    j : INT := 0;
    x : INT := 0;
    WORDS : ARRAY[0..100] OF STRING;
    value : STRING := 'val';
  END_VAR

  (* Case 1: Use after FOR — 2 violations *)
  FOR i := 0 TO 100 BY 2 DO
    IF WORDS[i] = 'Key' THEN EXIT; END_IF;
  END_FOR;
  IF i <= 100 THEN
    WORDS[i + 1] := value;
  END_IF;

  (* Case 2: No violation — variable not used after loop *)
  FOR j := 0 TO 10 DO
    x := j;
  END_FOR;

  (* Case 3: Reuse as control var in subsequent FOR — no violation *)
  FOR i := 0 TO 10 DO
    x := i;
  END_FOR;
  FOR i := 0 TO 20 DO
    x := i;
  END_FOR;

  (* Case 4: Nested FOR — outer var used after inner loop — violation *)
  FOR i := 0 TO 10 DO
    FOR j := 0 TO 5 DO
      x := j;
    END_FOR;
    x := j;
  END_FOR;

  (* Case 5: Use in expression after FOR — violation *)
  FOR j := 0 TO 10 DO
    x := j;
  END_FOR;
  x := j + 1;
END_PROGRAM
