open Core_kernel
module W = Warn

type output_format =
  | Plain
  | Json

(** Generate JSON schema for given [warnings]. *)

let print_report warnings fmt =
  let out =
    match fmt with
    | Plain -> List.fold_left warnings
                 ~f:(fun out w -> out ^ (W.to_string w) ^ "\n")
                 ~init:""
    | Json -> (let json_list = List.fold_left warnings
                   ~f:(fun out w ->
                       let j = W.to_yojson w in
                       j :: out)
                   ~init:[] in
               Yojson.Safe.to_string (`List json_list))
  in
  Printf.printf "%s\n" out
