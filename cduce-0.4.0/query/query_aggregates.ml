(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let query_min v =
  let (hd,tl) = Value.get_pair v in
  Value.fold_sequence 
    (fun accu x -> if Value.compare x accu < 0 then x else accu)
    hd
    tl

let query_max v =
  let (hd,tl) = Value.get_pair v in
  Value.fold_sequence 
    (fun accu x -> if Value.compare x accu > 0 then x else accu)
    hd
    tl
  
let query_sum v =
  let s =
    Value.fold_sequence
      (fun accu x -> Intervals.V.add accu (Value.get_integer x))
      Intervals.V.zero
      v in
  Value.Integer s

let query_avg v =
  let n = ref 0 in
  let s =
    Value.fold_sequence
      (fun accu x -> incr n; Intervals.V.add accu (Value.get_integer x))
      Intervals.V.zero
      v in
  Value.Integer (Intervals.V.div s (Intervals.V.from_int !n))

let query_count v =
  let n = Value.fold_sequence (fun accu _ -> succ accu) 0 v in
  Value.ocaml2cduce_int n

let query_member v =
  let (e,p) = Value.get_pair v in
  Value.vbool (List.exists (Value.equal e) (Value.get_sequence p))

let query_distinct v =
  let s = Value.fold_sequence (fun accu x -> Value.ValueSet.add x accu)
	    Value.ValueSet.empty v in
  Value.sequence (Value.ValueSet.elements s)

open Operators
open Builtin_defs;;

register_unary "min" (fun f _ _ ->  Sequence.approx (f any true)) query_min;
register_unary "max" (fun f _ _ ->  Sequence.approx (f any true)) query_max;
register_fun "avg" (Sequence.plus int) int query_avg;;
register_fun "count" Sequence.any int query_count;;
register_fun "member" (Sequence.plus any) bool query_member;;
register_unary "distinct_values" 
  (fun f _ _ -> Sequence.star (Sequence.approx (f any true))) query_distinct;;

