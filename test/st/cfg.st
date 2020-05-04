(* To debug intraprocedural control flow graphs *)
FUNCTION test_cfg : INT
  VAR
    i : INT;
  END_VAR

  i := 0;
  IF a > 1 THEN
      i := 1;
      a := 2;
  END_IF;
  i := 0;

END_FUNCTION
