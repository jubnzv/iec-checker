open Core

exception InternalError of string

let ignore v =
  let _ = v in ()

let next_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let sublist l low high =
  List.filteri l ~f:(fun i _ -> i >= low && i < high)

let rec list_flatten = function
  | [] -> []
  | [] :: t -> list_flatten t
  | (x::y) :: t -> x :: (list_flatten (y::t))

let head_exn = function
  | [] -> raise @@ InternalError "List is empty!\n"
  | x::_ -> x

(** Tail-recursive append to process large lists. *)
let append_tr xs ys = List.rev_append (List.rev xs) ys

(* {{{ Basic routines to work with monads *)
(* The implementation is based on CS3110 Maybe Monad:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/ads/ex_maybe_monad.html *)
let ( >>| ) = Option.( >>| )
let ( >>= ) = Option.( >>= )

let unwrap_list = function
  | Some l -> l
  | None -> []

let return (x : int) : int option =
  Some x

let return_binary op x y =
  return (op x y)

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b ->
  op a b

let sum_maybe_list values =
  let ( + ) = upgrade_binary (return_binary Caml.( + )) in
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
