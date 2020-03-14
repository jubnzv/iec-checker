(* This demo includes some declaration errors *)
TYPE
    ST0 : STRING[5];                               (* OK *)
    ST0 : STRING;                                  (* OK *)
    ST1 : STRING[5] := 'foo';                      (* OK *)
    ST1 : STRING := 'platform_dependent';          (* OK *)
    ST2 : STRING[5] := "foobar";                   (* Error *)
END_TYPE
