(** Configuration values including platform and implementation dependent options. *)

val max_string_len : int
(** Maximum size of STRING and WSTRING data types. *)

val mccabe_complexity_threshold : int
(** Threshold of McCabe complexity to generate warnings. *)

val statements_num_threshold : int
(** Threshold of maximum number of statements in POU to generate warnings. *)

(* {{{ List of the enabled checks *)
val check_plcopen_cp1 : bool
val check_plcopen_cp2 : bool
val check_plcopen_cp3 : bool
val check_plcopen_cp4 : bool
val check_plcopen_cp6 : bool
val check_plcopen_cp8 : bool
val check_plcopen_cp9 : bool
val check_plcopen_cp13 : bool
val check_plcopen_cp25 : bool
val check_plcopen_cp28 : bool
val check_plcopen_l10 : bool
val check_plcopen_l17 : bool
val check_plcopen_n3 : bool
(* }}} *)
