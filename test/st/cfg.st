(* For debugging intraprocedural control flow graphs *)
FUNCTION test_cfg : INT
  VAR
    i : INT;
  END_VAR

  i := 0;
  i := i + 0x42;
END_FUNCTION

