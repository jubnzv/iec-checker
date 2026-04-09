(** Output interfaces for static analysis warnings. *)
module W = Warn

type output_format =
  | Plain
  | Json

val print_report :
  ?doc_urls:(string * string) list ->
  ?use_color:bool ->
  W.t list -> output_format -> unit
(** [print_report] Print warnings in selected format to stdout. *)
