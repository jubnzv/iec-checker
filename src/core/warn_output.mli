(** Output interfaces for static analysis warnings. *)
module W = Warn

type output_format =
  | Plain
  | Json

val print_report : W.t list -> output_format -> unit
(** Print warnings in selected format to stdout. *)
