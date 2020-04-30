(* To debug intraprocedural control flow graphs *)
FUNCTION test_cfg : INT
  VAR
    i : INT;
  END_VAR

  i := 0;
  i := i + 16#42;
END_FUNCTION

