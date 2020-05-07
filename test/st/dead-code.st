FUNCTION test_dead_code_to_stmts : INT
  VAR
    counter : INT := 0;
    some_var : INT;
  END_VAR
  counter := counter + 1;
  counter := 2 + 2;
  RETURN;
  some_var := SQRT(16#42); (* UnreachableCode error *)
  some_var := 16#42; (* No additional warnings *)
  some_var := 19;
END_FUNCTION
