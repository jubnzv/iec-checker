open Core
module S = Syntax

type pou_ty =
  | FB
  | Function
  | Program

type parse_result = {
  mutable interface: string option;
  mutable implementation: string option;
  mutable pou_type: pou_ty option;
}

(** Parses the text string from the XMLM node. *)
let pull_data i =
  if Xmlm.eoi i then None
  else
    match Xmlm.input i with
    | `Data d -> Some(d)
    | _ -> None

let pull_pou_type i =
  if Xmlm.eoi i then None
  else
    match Xmlm.input i with
    | `Data d -> begin
        if (String.equal d "FunctionBlock") then
          Some(FB)
        else if (String.equal d "Function") then
          Some(Function)
        else if (String.equal d "Program") then
          Some(Program)
        else
          None
      end
    | _ -> None
let get_end_tag = function
  | FB -> "END_FUNCTION_BLOCK"
  | Function -> "END_FUNCTION"
  | Program -> "END_PROGRAM"

(** Creates a source code from the parse_result entry and returns the source code. *)
let reconstruct_source res : (string option) =
  match res.interface, res.implementation, res.pou_type with
  | Some(interface), Some(impl), Some(pou_type) ->
    Some(String.concat ~sep:"\n" [interface; impl; (get_end_tag pou_type)])
  | _  -> None


(** Iterate over all XML elements in schema to parse their source code. *)
let rec parse_source i d acc =
  if Xmlm.eoi i then reconstruct_source acc
  else
    match Xmlm.input i with
    |`El_start ((_, tag), _) when (String.equal tag "Interface") -> begin
        acc.interface <- (pull_data i);
        parse_source i d acc
      end
    |`El_start ((_, tag), _) when (String.equal tag "Implementation") -> begin
        acc.implementation <- (pull_data i);
        parse_source i d acc
      end
    |`El_start ((_, tag), _) when (String.equal tag "POUKind" || String.equal tag "Type") -> begin
        acc.pou_type <- (pull_pou_type i);
        parse_source i d acc
      end
    |`El_start _ -> parse_source i (d + 1) acc
    | `El_end -> if (phys_equal d 1) then reconstruct_source acc else parse_source i (d - 1) acc
    | _ -> parse_source i d acc

let reconstruct_from_channel_opt ic =
  let i_opt = try Some(Xmlm.make_input (`Channel ic)) with
    | _ -> None
  in
  match i_opt with
  | None -> None
  | Some(i) -> begin
      let result = { interface = None;
                     implementation = None;
                     pou_type = None } in
      try (parse_source i 1 result) with
      | _ -> None
    end
