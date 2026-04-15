open Core
module W = Warn

type output_format =
  | Plain
  | Json

(* ANSI escape helpers *)
let bold s use_color = if use_color then "\027[1m" ^ s ^ "\027[0m" else s
let blue s use_color = if use_color then "\027[34m" ^ s ^ "\027[0m" else s
let cyan s use_color = if use_color then "\027[36m" ^ s ^ "\027[0m" else s

let format_plain_warning doc_urls use_color (w : W.t) =
  match w.ty with
  | W.InternalError ->
    Printf.sprintf "%s: %s" (bold w.id use_color) w.msg
  | W.Inspection ->
    let header = Printf.sprintf "%s: %s" (bold w.id use_color) w.msg in
    let location =
      if String.is_empty w.file then
        Printf.sprintf "  %s %d:%d" (blue "-->" use_color) w.linenr w.column
      else
        Printf.sprintf "  %s %s:%d:%d" (blue "-->" use_color) w.file w.linenr w.column
    in
    let context_block =
      if String.is_empty w.context then ""
      else "\n" ^ w.context
    in
    let doc_line =
      match List.Assoc.find doc_urls ~equal:String.equal w.id with
      | Some url when not (String.is_empty url) ->
        Printf.sprintf "\n  %s %s" (cyan "See:" use_color) url
      | _ -> ""
    in
    header ^ "\n" ^ location ^ context_block ^ doc_line

let print_report ?(doc_urls=[]) ?(use_color=true) warnings fmt =
  match fmt with
  | Plain ->
    if not (List.is_empty warnings) then begin
      List.map warnings ~f:(format_plain_warning doc_urls use_color)
      |> String.concat ~sep:"\n\n"
      |> Printf.printf "%s\n"
    end
  | Json ->
    let json_list = List.map warnings ~f:W.to_yojson in
    Yojson.Safe.to_string (`List json_list)
    |> Printf.printf "%s\n"
