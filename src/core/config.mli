(** Configuration values including platform and implementation dependent options. *)

val max_string_len : int
(** Maximum size of STRING and WSTRING data types. *)

val mccabe_complexity_threshold : int
(** Threshold of McCabe complexity to generate warnings. *)

val statements_num_threshold : int
(** Threshold of maximum number of statements in POU to generate warnings. *)
