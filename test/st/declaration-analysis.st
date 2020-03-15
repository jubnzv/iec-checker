(* This demo includes some declaration errors *)
TYPE
    ST0 : STRING[5];                               (* OK *)
    ST1 : STRING;                                  (* OK *)
    ST2 : STRING[5] := 'foo';                      (* OK *)
    ST3 : STRING := 'platform_dependent';          (* OK *)
    ST4 : STRING[5] := "foobar";                   (* Error *)
END_TYPE
