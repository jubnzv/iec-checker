(* This demo includes some declaration errors *)
TYPE
    (* String types *)
    ST0 : STRING[5];                               (* OK *)
    ST1 : STRING;                                  (* OK *)
    ST2 : STRING[5] := 'foo';                      (* OK *)
    ST3 : STRING := 'platform_dependent';          (* OK *)
    ST4 : STRING[5] := "foobar";                   (* Error *)

    (* Subrange types *)
    ANALOG_DATA1 : INT (-4095 .. 4095);          (* OK *)
    ANALOG_DATA1 : INT (-4095 .. 4095) := 2000;  (* OK *)
    ANALOG_DATA1 : INT (-4095 .. 4095) := 4095;  (* OK: See 6.4.4.4.1 *)
    ANALOG_DATA1 : INT (-4095 .. 4095) := -4095; (* OK: See 6.4.4.4.1 *)
    ANALOG_DATA2 : INT (-4095 .. 4095) := 4099;  (* Error *)
    ANALOG_DATA2 : INT (-4095 .. 4095) := -4096; (* Error *)
END_TYPE
