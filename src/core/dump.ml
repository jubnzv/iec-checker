open Core_kernel
module S = Syntax

type dump_scheme = {
  version: string; (** Scheme version *)
  functions: S.function_decl list;
  function_blocks: S.fb_decl list;
  programs: S.program_decl list;
  configurations: S.configuration_decl list;
  types: S.derived_ty_decl list;
  environments: Env.t list;
  cfgs: Cfg.t list;
} [@@deriving to_yojson]

let create_dump elements environments cfgs src_filename =
  let dest_filename = Printf.sprintf "%s.dump.json" src_filename in
  let version = "0.1" in
  let functions =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECFunction f -> acc @ [f] | _ -> acc)
      ~init:[]
  in
  let function_blocks =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECFunctionBlock fb -> acc @ [fb] | _ -> acc)
      ~init:[]
  in
  let programs =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECProgram p -> acc @ [p] | _ -> acc)
      ~init:[]
  in
  let configurations =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECConfiguration c -> acc @ [c] | _ -> acc)
      ~init:[]
  in
  let types =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECType ty -> acc @ [ty] | _ -> acc)
      ~init:[]
  in
  let scheme = {
    version;
    functions;
    function_blocks;
    programs;
    configurations;
    types;
    environments;
    cfgs;
  } in
  Yojson.Safe.to_file dest_filename (dump_scheme_to_yojson scheme)
