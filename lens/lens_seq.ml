(** This is the lazy evaluated sequence module for OCaml 4.06 compatability. The signatures and comments are based on the standard OCaml library. *)

type 'a t = unit -> 'a node

and 'a node = Nil | Cons of 'a * 'a t

let empty () = Nil

let return v () = Cons (v, empty)

let rec map f v () =
  match v () with
  | Nil -> Nil
  | Cons (v, r) -> Cons (f v, map f r)

let rec filter f v () =
  match v () with
  | Nil -> Nil
  | Cons (v, r) -> if f v then Cons (v, filter f r) else filter f r ()

let rec filter_map f v () =
  match v () with
  | Nil -> Nil
  | Cons (v, r) -> (
      match f v with
      | None -> filter_map f r ()
      | Some v -> Cons (v, filter_map f r))

let rec fold_left f initial seq =
  match seq () with
  | Nil -> initial
  | Cons (v, r) -> fold_left f (f initial v) r

let rec iter f v =
  match v () with
  | Nil -> ()
  | Cons (v, r) ->
      f v;
      iter f r
