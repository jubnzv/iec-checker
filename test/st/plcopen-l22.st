PROGRAM l22
  VAR
    i : INT := 0;
    j : INT := 0;
    arr : ARRAY[0..10] OF INT;
  END_VAR

  (* Case 1: Direct assignment in body — violation *)
  FOR i := 0 TO 10 BY 2 DO
    IF arr[i] = 42 THEN
      i := 10;
    END_IF;
  END_FOR;

  (* Case 2: No violation — different variable *)
  FOR i := 0 TO 10 DO
    j := i + 1;
  END_FOR;

  (* Case 3: Nested FOR — inner loop modifies outer var — violation *)
  FOR i := 0 TO 10 DO
    FOR j := 0 TO 5 DO
      i := j;
    END_FOR;
  END_FOR;

  (* Case 4: Nested FOR — inner loop modifies its own var — violation *)
  FOR i := 0 TO 10 DO
    FOR j := 0 TO 5 DO
      j := 0;
    END_FOR;
  END_FOR;
END_PROGRAM
