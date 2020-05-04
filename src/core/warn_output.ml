open Core_kernel
module W = Warn

type output_format =
  | Plain
  | Json

let print_report warnings fmt =
  match fmt with
  | Plain -> begin
      List.fold_left warnings
        ~f:(fun out w -> out ^ (W.to_string w) ^ "\n")
        ~init:""
      |> Printf.printf "%s"
    end
  | Json -> begin
      let json_list = List.fold_left warnings
          ~f:(fun out w ->
              let j = W.to_yojson w in
              j :: out)
          ~init:[] in
      Yojson.Safe.to_string (`List json_list)
      |> Printf.printf "%s\n"
    end
