FUNCTION demo : INT
    VAR_INPUT
        x1 : INT := 0;
        x2 : INT; (* PLCOPEN-CP3 *)
        x3 : STRING[5]; (* PLCOPEN-CP3 *)
        x4 : WSTRING[25] := 'spaces_disallowed';
        x5 : ARRAY [1..2, 1..3] OF INT; (* PLCOPEN-CP3 *)
        x6: ARRAY [0..7] OF BOOL := [0,1,1,0,0,1,0,0];
    END_VAR
    x1 := 42;
END_FUNCTION
