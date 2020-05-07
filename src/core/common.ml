open Core_kernel

exception InternalError of string

let next_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let sublist l low high =
  List.filteri l ~f:(fun i _ -> i >= low && i < high)

let head_exn = function
  | [] -> raise @@ InternalError "List is empty!\n"
  | x::_ -> x

let append_tr xs ys = List.rev_append (List.rev xs) ys
