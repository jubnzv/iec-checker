FUNCTION demo : INT
    VAR_INPUT
        x1 AT %MX40 : INT; (* PLCOPEN-CP4 *)
        x2 AT %MX41 : INT; (* PLCOPEN-CP4 *)
        x3 AT %MX510 : INT;
    END_VAR
    x1 := 42;
END_FUNCTION
