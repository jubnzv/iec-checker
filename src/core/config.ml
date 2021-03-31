(** Configuration values including platform and implementation dependent options. *)

(** Maximum size of STRING and WSTRING data types. *)
let max_string_len = 4096

(** Threshold of McCabe complexity to generate warnings. *)
let mccabe_complexity_threshold = 15

(** Threshold of maximum number of statements in POU to generate warnings. *)
let statements_num_threshold = 25

(* {{{ List of the enabled checks *)
let check_plcopen_cp1 = true
let check_plcopen_cp2 = true
let check_plcopen_cp3 = true
let check_plcopen_cp4 = true
let check_plcopen_cp6 = true
let check_plcopen_cp8 = true
let check_plcopen_cp9 = true
let check_plcopen_cp13 = true
let check_plcopen_cp25 = true
let check_plcopen_cp28 = true
let check_plcopen_l10 = true
let check_plcopen_l17 = true
let check_plcopen_n3 = true
(* }}} *)
