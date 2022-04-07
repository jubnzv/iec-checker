open Core
open IECCheckerCore

module S = Syntax

let eval_mccabe cfg =
  (* The McCabe complexity is calculated by:
       M = E - N + 2P
     Where E is the number of edges in the CFG, N is the number of nodes
     and P is the number of disconnected parts of the graph. *)
  let e = Cfg.get_number_of_edges cfg
  and n = List.length (Cfg.get_reachable_ids cfg) in
  let p = n - (List.length (Cfg.get_all_ids cfg)) in
  (e - n + 2 * p)
