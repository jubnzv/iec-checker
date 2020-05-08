FUNCTION dead_code_after_return : INT
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

PROGRAM dead_code_in_the_loops
VAR a : INT; i : INT; END_VAR
  WHILE i < 10 DO
    IF i = 5 THEN
      i := i + 1;
      EXIT;
      i := 19; (* UnreachableCode error *)
      i := 42; (* No additional warnings *)
      i := 42;
    ELSIF i = 6 THEN
      CONTINUE;
      i := 3; (* UnreachableCode error *)
      i := 44; (* No additional warnings *)
      i := 19;
    END_IF;
    i := i + 2;
  END_WHILE;
  I := 0;
END_PROGRAM
