open Core
module W = Warn

type output_format =
  | Plain
  | Json

let print_report warnings fmt =
  match fmt with
  | Plain ->
    List.map warnings ~f:W.to_string
    |> String.concat ~sep:"\n"
    |> Printf.printf "%s\n"
  | Json ->
    let json_list = List.map warnings ~f:W.to_yojson in
    Yojson.Safe.to_string (`List json_list)
    |> Printf.printf "%s\n"
