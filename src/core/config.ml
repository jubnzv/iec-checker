(**
   Default configuration values including platform and implementation dependent
   options.
*)

(** Maximum size of STRING and WSTRING data types. *)
let max_string_len = 4096

(** Threshold of McCabe complexity to generate warnings. *)
let mccabe_complexity_threshold = 20

(** Threshold of maximum number of statements in POU to generate warnings. *)
let statements_num_threshold = 25
