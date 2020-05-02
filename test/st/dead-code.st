FUNCTION test_dead_code_1 : INT
  VAR
    counter : INT := 0;
    some_var : INT;
  END_VAR

  counter := counter + 1;
  counter := 2 + 2;
  RETURN;

  some_var := SQRT(16#42); (* UnreachableCode error *)
  some_var := 16#42; (* no warning                  *)
END_FUNCTION

FUNCTION test_dead_code_2 : INT
  VAR
    I : INT;
    Acc : INT := 1;
    Result : INT;
  END_VAR

  (* no false positive *)
  FOR I := 0 TO 100 DO
      Acc := Acc * X;
      Acc := Acc + 33;
  END_FOR;
  Result := Acc;
  Result := Acc;
END_FUNCTION

(* PROGRAM test_dead_code_3                         *)
(*   VAR                                            *)
(*     foo : INT := 0;                              *)
(*   END_VAR                                        *)

(*   IF FALSE THEN                                  *)
(*     foo := 19; (* TODO: UnreachableCode error *) *)
(*   ELSE                                           *)
(*     foo := 42;                                   *)
(*   END_IF;                                        *)

(* END_PROGRAM                                      *)
