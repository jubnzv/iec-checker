PROGRAM REF_DEMO
  (* Elementary-typed variables *)
  VAR
    error_state : BOOL;
    (* error_state : BOOL := FALSE; *)
  END_VAR

  (* Reference variables *)
  VAR
    myRefInt: REF_TO INT;
  END_VAR

  (* Empty case *)
  VAR
  END_VAR

  (* Retain / non-retain variables *)
  VAR RETAIN foo : INT; END_VAR
  VAR NON_RETAIN bar : INT; END_VAR

  myRefInt := 1;
END_PROGRAM
