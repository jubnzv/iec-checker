PROGRAM REF_DEMO
  (* Elementary *)
  VAR
    v : INT;
    v : INT := 1;
    v : BOOL;
    v : BOOL := FALSE;
    v : REAL := 10.0;
  END_VAR

  (* Direct and partly-located *)
  VAR
    v AT %IW1.2 : REAL;
    v AT %IW1.2 : REAL := 200.0;
  END_VAR

  (* Reference variables *)
  VAR
    myRefInt: REF_TO INT;
  END_VAR

  (* Arrays *)
  VAR
    A : ARRAY[0..5] OF INT;
    A : ARRAY[0..5] OF INT := [1,2,3];
    aWStrings: ARRAY[0..1] OF WSTRING := ["1234", "5678"];
  END_VAR

  (* Empty case *)
  VAR
  END_VAR

  (* Retain / non-retain variables *)
  VAR RETAIN foo : INT; END_VAR
  VAR NON_RETAIN bar : INT; END_VAR

  myRefInt := 1;
END_PROGRAM

PROGRAM no_variables
  myRefInt := 1;
END_PROGRAM
