open Value
open Types
open Utility
open Proc
open Lib

let to_pair r = 
  let f = (fun rcd -> 
    match rcd with
    | (_, v) -> v) in
  let l = List.map f (unbox_record r) in
  match l with
  | [x; y] -> (x, y)
  | _ -> assert false

let to_record p = 
  match p with
  | (x, y) -> (box_record [("1", x); ("2", y)])

let to_list_in_pair p = 
  match p with
  | (x, y) -> to_record ((box_list x), (box_list y))

let compare a b = 
  if a < b then (-1)
  else if a > b then 1
  else 0

let rcompare a b = 
  if a > b then (-1)
  else if a < b then 1
  else 0

let _ = 
	print_endline "adding Ocaml List module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
    "lsit_length",
    (p1 (unbox_list ->- List.length ->- box_int),
    datatype "([a]) -> int",
    PURE);

    "list_cons",
    (p2 (fun x xs ->
      box_list (List.cons x (unbox_list xs))),
    datatype "(a, [a]) -> [a]",
    PURE);

    "list_hd",
    (p1 (unbox_list ->- List.hd),
    datatype "([a]) ~> a",
    IMPURE);

    "list_tl",
    (p1 (unbox_list ->- List.tl ->- box_list),
    datatype "([a]) -> [a]",
    IMPURE);

    "list_nth",
    (p2 (fun lst n ->
      List.nth (unbox_list lst) (unbox_int n)),
    datatype "([a], Int) -> a",
    PURE);

  	"list_rev",
	  (p1 (unbox_list ->- List.rev ->- box_list),
   	datatype "([a]) -> [a]",
  	PURE);

    "list_append",
    (p2 (fun l1 l2 ->
      box_list (List.append (unbox_list l1) (unbox_list l2))),
    datatype "([a], [a]) -> [a]",
    PURE);

    "list_rev_append",
    (p2 (fun l1 l2 -> 
      box_list (List.rev_append (unbox_list l1) (unbox_list l2))),
    datatype "([a], [a]) -> [a]",
    PURE);

    "list_concat",
    (p1 (fun lst ->
      box_list (List.concat (List.map unbox_list (unbox_list lst)) )),
    datatype "([[a]]) -> [a]",
    PURE);

    "list_mem",
    (p2 (fun a l ->
      box_bool (List.mem a (unbox_list l))),
    datatype "(a, [a]) -> Bool",
    PURE);

    "list_memq",
    (p2 (fun a l ->
      box_bool (List.memq a (unbox_list l))),
    datatype "(a, [a]) -> Bool",
    PURE);

    (* list of pairs *)

    "list_assoc",
    (p2 (fun a lst ->
      List.assoc a (List.map to_pair (unbox_list lst))),
    datatype "(a, [(a, b)]) -> b",
    PURE);

    "list_assq",
    (p2 (fun a lst ->
      List.assq a (List.map to_pair (unbox_list lst))),
    datatype "(a, [(a, b)]) -> b",
    PURE);

    "list_mem_assoc",
    (p2 (fun a lst ->
      box_bool (List.mem_assoc a (List.map to_pair (unbox_list lst)))),
    datatype "(a, [(a, b)]) -> Bool",
    PURE);

    "list_mem_assq",
    (p2 (fun a lst ->
      box_bool (List.mem_assq a (List.map to_pair (unbox_list lst)))),
    datatype "(a, [(a, b)]) -> Bool",
    PURE);

    "list_remove_assoc",
    (p2 (fun a lst ->
      box_list (List.map to_record (List.remove_assoc a (List.map to_pair (unbox_list lst))))),
    datatype "(a, [(a, b)]) -> [(a, b)]",
    PURE);

    "list_remove_assq",
    (p2 (fun a lst ->
      box_list (List.map to_record (List.remove_assq a (List.map to_pair (unbox_list lst))))),
    datatype "(a, [(a, b)]) -> [(a, b)]",
    PURE);

    "list_split",
    (p1 (fun lst ->
      to_list_in_pair (List.split (List.map to_pair (unbox_list lst)))),
    datatype "([(a, b)]) -> ([a], [b])",
    PURE);

    "list_combine",
    (p2 (fun l1 l2 ->
      box_list (List.map to_record (List.combine (unbox_list l1) (unbox_list l2)))),
    datatype "([a], [b]) -> [(a, b)]",
    PURE);

    (* sorting only supports default comparison *)

    "list_sort",
    (p1 (unbox_list ->- (List.sort compare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_rsort",
    (p1 (unbox_list ->- (List.sort rcompare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_stable_sort",
    (p1 (unbox_list ->- (List.stable_sort compare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_stable_rsort",
    (p1 (unbox_list ->- (List.stable_sort rcompare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_fast_sort",
    (p1 (unbox_list ->- (List.fast_sort compare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_fast_rsort",
    (p1 (unbox_list ->- (List.fast_sort rcompare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_sort_uniq",
    (p1 (unbox_list ->- (List.sort_uniq compare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_rsort_uniq",
    (p1 (unbox_list ->- (List.sort_uniq rcompare) ->- box_list),
    datatype "([a]) -> [a]",
    PURE);

    "list_merge",
    (p2 (fun l1 l2 ->
      box_list (List.merge compare (unbox_list l1) (unbox_list l2))),
    datatype "([a], [a]) -> [a]",
    PURE);

    "list_rmerge",
    (p2 (fun l1 l2 ->
      box_list (List.merge rcompare (unbox_list l1) (unbox_list l2))),
    datatype "([a], [a]) -> [a]",
    PURE);

  	] in
	env := List.append (!env) new_env ;;