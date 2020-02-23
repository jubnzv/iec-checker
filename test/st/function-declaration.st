(* Test for declaration of Function POU. *)
FUNCTION fn1 : INT
    VAR_INPUT
        vi1 : INT;
        vi2 : DINT;
    END_VAR
    VAR_OUTPUT
        vo1 : INT;
    END_VAR
    VAR_IN_OUT
        vio1 : INT;
    END_VAR
    VAR_IN_OUT
        vio1 : INT;
    END_VAR
    VAR
        v1 : INT;
    END_VAR
    VAR_EXTERNAL
        ve1 : INT;
    END_VAR
    VAR_TEMP
        vt1 : INT;
    END_VAR

    vio1 := 42;
END_FUNCTION
