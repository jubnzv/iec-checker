open Core
module W = Warn

type output_format =
  | Plain
  | Json

let print_report warnings fmt =
  match fmt with
  | Plain -> begin
      List.fold_left warnings
        ~f:(fun acc w -> acc @ [W.to_string w])
        ~init:[]
      |> String.concat ~sep:"\n"
      |> Printf.printf "%s"
    end
  | Json -> begin
      let json_list =
        List.fold_left warnings
          ~f:(fun out w -> (W.to_yojson w) :: out)
          ~init:[]
      in
      Yojson.Safe.to_string (`List json_list)
      |> Printf.printf "%s\n"
    end
