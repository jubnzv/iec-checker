(* Uncompatible: Recursive functions are forbidden *)
FUNCTION Factorial : INT
    VAR_INPUT
        X : INT;
    END_VAR

    IF X > 1 THEN
        Factorial := Factorial(X - 1) * X;
    ELSE
        Factorial := X;
    END_IF;
END_FUNCTION

(* Compatible iterative implementation *)
FUNCTION Factorial_good : INT
    VAR_INPUT
        X : INT;
    END_VAR
    VAR
        Acc : INT;
    END_VAR

    FOR I := 0 TO 100 DO
        Acc := Acc * X;
    END_FOR;
    Factorial := Acc;
END_FUNCTION
