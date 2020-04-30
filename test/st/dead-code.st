FUNCTION test_dead_code : INT
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

  RETURN;
  some_var := SQRT(16#42); (* CFA error: unreachable code *)
END_FUNCTION

