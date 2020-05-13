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

(* {{{ Basic routines to work with monads *)
(* The implementation is based on CS3110 Maybe Monad:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/ads/ex_maybe_monad.html *)
let bind o f =
  match o with
  | Some x  -> f x
  | None    -> None

let ( >>= ) o f = bind o f

let return (x : int) : int option =
  Some x

let return_binary op x y =
  return (op x y)

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b ->
  op a b

let ( + ) = upgrade_binary (return_binary Caml.( + ))
let sum_maybe_list values =
  let rec aux acc values =
    match acc with
    | Some _ -> begin
        match values with
        | [] -> acc
        | [x] -> ( + ) acc x
        | x :: xs -> aux (( + ) acc x) xs
      end
    | None -> None
  in
  aux (Some 0) values
(* }}} *)
