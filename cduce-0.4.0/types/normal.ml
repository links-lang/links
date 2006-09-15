(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module type S =
sig
  type t

  val any: t
  val empty: t
  val cup: t -> t -> t
  val cap: t -> t -> t
  val diff: t -> t -> t
  val is_empty: t -> bool
end

type 'a bool = ('a list * 'a list) list

module Make(X1 : S)(X2 : S) =
struct
  type t = (X1.t * X2.t) list

(* Possible optimizations:
   - check whether t1 or t2 is empty initially
   - check s1 = t1 (structural equility)
*)
  let rec add root t1 t2 = function
    | [] -> (t1,t2) :: root
    | (s1,s2) :: rem ->
	let i = X1.cap t1 s1 in
	if X1.is_empty i then add ((s1,s2)::root) t1 t2 rem
	else (
	  let root = (i, X2.cup t2 s2) :: root in
	  let k = X1.diff s1 t1 in
	  let root = if not (X1.is_empty k) then (k, s2) :: root else root in
	  let j = X1.diff t1 s1 in
	  if not (X1.is_empty j) 
	  then add root j t2 rem 
	  else List.rev_append root rem
	)

  let normal x =
    List.fold_left (fun accu (t1,t2) -> add [] t1 t2 accu) [] x

  let rec bigcap_aux t1 t2 = function
    | (s1,s2)::rem -> bigcap_aux (X1.cap t1 s1) (X2.cap t2 s2) rem
    | [] -> (t1,t2)
  let bigcap = bigcap_aux X1.any X2.any

(* optimize: one knows that the t1 are pairwise disjoint ... *)
  let line accu (p,n) =
    let (d1,d2) = bigcap p in
    if not ((X1.is_empty d1) || (X2.is_empty d2)) then
      (let resid = ref X1.empty in
       let accu = List.fold_left
	 (fun accu (t1,t2) ->
	    let i = X1.cap d1 t1 in
	    if not (X1.is_empty i) then
	      (resid := X1.cup !resid t1;
	       let t2 = X2.diff d2 t2 in
	       if not (X2.is_empty t2) then add [] i t2 accu else accu
	      ) else accu
	 ) accu (normal n) in
       let d1 = X1.diff d1 !resid in
       if not (X1.is_empty d1) then add [] d1 d2 accu else accu)
    else accu

  (* Merge t1's with same t2 *)
  let equiv t s = X2.is_empty (X2.diff t s) && X2.is_empty (X2.diff s t)
  let cleanup l =
    let rec aux accu (t1,t2) =
      match accu with
	| [] -> [ (t1,t2) ]
	| (s1,s2) :: rem when equiv t2 s2 -> (X1.cup s1 t1, s2) :: rem
	| (s1,s2) :: rem -> (s1,s2) :: (aux rem (t1,t2)) in
    List.fold_left aux [] l
      
  let boolean_normal x =
    cleanup (List.fold_left line [] x)

  let boolean x =
    List.fold_left (fun accu x -> (line [] x) @ accu) [] x

  let pi1 =
    List.fold_left (fun accu (t1,t2) -> X1.cup accu t1) X1.empty

  let pi2 =
    List.fold_left (fun accu (t1,t2) -> X2.cup accu t2) X2.empty

  let pi2_restricted restr = 
    List.fold_left (fun accu (t1,t2) -> 
		      if X1.is_empty (X1.cap t1 restr) then accu 
		      else X2.cup accu t2) X2.empty
end

