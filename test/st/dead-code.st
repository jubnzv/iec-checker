FUNCTION test_dead_code : INT
  VAR
    counter : INT := 0;
    some_var : INT;
  END_VAR

  counter := counter + 1;
  RETURN;

  some_var := SQRT(16#42); (* CFA error: unreachable code *)
END_FUNCTION

