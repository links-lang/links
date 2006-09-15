(*
 * Ptmap: Maps over integers implemented as Patricia trees.
 * Copyright (C) 2000 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(*i $Id: ptmap.ml,v 1.7 2004/07/21 08:39:44 filliatr Exp $ i*)

(*s Maps of integers implemented as Patricia trees, following Chris
    Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
    ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
    See the documentation of module [Ptset] which is also based on the
    same data-structure. *)

type key = int

type 'a t =
  | Empty
  | Leaf of int * 'a
  | Branch of int * int * 'a t * 'a t

let empty = Empty

let is_empty t = t = Empty

let singleton k x = Leaf (k,x)

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf (j,_) -> k == j
  | Branch (_, m, l, r) -> mem k (if zero_bit k m then l else r)

let rec find k = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if k == j then x else raise Not_found
  | Branch (_, m, l, r) -> find k (if zero_bit k m then l else r)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then 
    Branch (mask p0 m, m, t0, t1)
  else 
    Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let add k x t =
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j,_) as t -> 
	if j == k then Leaf (k,x) else join (k, Leaf (k,x), j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then 
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf (k,x), p, t)
  in
  ins t

let set k f t =
  let rec ins = function
    | Empty -> Leaf (k, f None)
    | Leaf (j,x) as t -> 
	if j == k then Leaf (k,f (Some x)) else join (k, Leaf (k,f None), j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then 
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf (k,f None), p, t)
  in
  ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t -> if k == j then Empty else t
    | Branch (p,m,t0,t1) as t -> 
	if match_prefix k p m then
	  if zero_bit k m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let may_leaf k = function
  | None -> Empty
  | Some x -> Leaf (k,x)

let unset k f t =
  let rec ins = function
    | Empty -> Empty
    | Leaf (j,x) as t -> 
	if j == k then may_leaf k (f x) 
	else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then 
	    branch (p, m, ins t0, t1)
	  else
	    branch (p, m, t0, ins t1)
	else
	  t
  in
  ins t

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)
      
let rec mapi f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f k x)
  | Branch (p,m,t0,t1) -> Branch (p, m, mapi f t0, mapi f t1)
      
let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

(* we order constructors as Empty < Leaf < Branch *)
let compare cmp t1 t2 =
  let rec compare_aux t1 t2 = match t1,t2 with
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1
    | Leaf (k1,x1), Leaf (k2,x2) ->
	let c = compare k1 k2 in 
	if c <> 0 then c else cmp x1 x2
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	let c = compare p1 p2 in
	if c <> 0 then c else
	let c = compare m1 m2 in
	if c <> 0 then c else
        let c = compare_aux l1 l2 in
        if c <> 0 then c else
        compare_aux r1 r2
  in
  compare_aux t1 t2

let equal eq t1 t2 =
  let rec equal_aux t1 t2 = match t1, t2 with
    | Empty, Empty -> true
    | Leaf (k1,x1), Leaf (k2,x2) -> k1 = k2 && eq x1 x2
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	p1 = p2 && m1 = m2 && equal_aux l1 l2 && equal_aux r1 r2
    | _ -> false
  in
  equal_aux t1 t2

let rec hash h = function
  | Empty -> 0
  | Leaf (k,x) -> k + 17 * (h x)
  | Branch (p,m,l,r) ->  p + 17 * m + 257 * (hash h l) + 65537 * (hash h r)

let rec subset f s1 s2 = match (s1,s2) with
  | Empty, _ -> true
  | _, Empty -> false
  | Leaf (k1,x1), _ -> (try f x1 (find k1 s2) with Not_found -> false)
  | Branch _, Leaf _ -> false
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
        subset f l1 l2 && subset f r1 r2
      else if m1 > m2 && match_prefix p1 p2 m2 then
        if zero_bit p1 m2 then 
          subset f l1 l2 && subset f r1 l2
        else 
          subset f l1 r2 && subset f r1 r2
      else
        false

let rec disjoint f s1 s2 = match (s1,s2) with
  | Empty, _ | _, Empty -> true
  | Leaf (k,x), s 
  | s, Leaf (k,x) -> (try f x (find k s) with Not_found -> true)
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
        disjoint f l1 l2 && disjoint f r1 r2
      else if m1 > m2 && match_prefix p1 p2 m2 then
        if zero_bit p1 m2 then 
          disjoint f l1 l2 && disjoint f r1 l2
        else 
          disjoint f l1 r2 && disjoint f r1 r2
      else if m2 > m1 && match_prefix p2 p1 m1 then
        if zero_bit p2 m1 then 
          disjoint f l1 l2 && disjoint f l1 r2
        else 
          disjoint f r1 l2 && disjoint f r1 r2
      else true

let rec union f t1 t2 = match (t1,t2) with
  | Empty, t | t, Empty -> t
  | (Leaf (k,x) as t0), t | t, (Leaf (k,x) as t0) -> 
      let rec ins = function
	| Empty -> Leaf (k, x)
	| Leaf (j,y) as t -> 
	    if j == k then Leaf (k,f x y) else join (k, t0, j, t)
	| Branch (p,m,l,r) as t ->
	    if match_prefix k p m then
	      if zero_bit k m then 
		Branch (p, m, ins l, r)
	      else
		Branch (p, m, l, ins r)
	    else
	      join (k, t0, p, t) in
      ins t
  | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
      if m == n && match_prefix q p m then
	Branch (p, m, union f s0 t0, union f s1 t1)
      else if m < n && match_prefix q p m then
	(* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	if zero_bit q m then 
	  Branch (p, m, union f s0 t, s1)
        else 
	  Branch (p, m, s0, union f s1 t)
      else if m > n && match_prefix p q n then
	(* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	if zero_bit p n then
	  Branch (q, n, union f s t0, t1)
	else
	  Branch (q, n, t0, union f s t1)
      else
	(* The prefixes disagree. *)
	join (p, s, q, t)

let is_singleton f = function
  | Leaf (k,x) -> f x
  | _ -> None

let rec diff f t1 t2 = match (t1,t2) with
  | t, Empty -> t
  | Empty, t -> Empty
  | Leaf (k,x), _ -> (try may_leaf k (f x (find k t2)) with Not_found -> t1)
  | _, Leaf (k,x) ->
      let rec ins = function
	| Empty -> Empty
	| Leaf (j,y) as t -> 
	    if j == k then may_leaf k (f y x) 
	    else t
	| Branch (p,m,t0,t1) as t ->
	    if match_prefix k p m then
	      if zero_bit k m then 
		branch (p, m, ins t0, t1)
	      else
		branch (p, m, t0, ins t1)
	    else
	      t
      in
      ins t1
  | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
      if m == n && match_prefix q p m then
	branch (p,m,diff f s0 t0,diff f s1 t1)
      else if m < n && match_prefix q p m then
	(* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	if zero_bit q m then 
	  branch (p,m,diff f s0 t,s1)
        else 
	  branch (p,m,s0,diff f s1 t)
      else if m > n && match_prefix p q n then
	(* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	if zero_bit p n then
	  diff f s t0
	else 
	  diff f s t1
      else
	s

let rec inter f t1 t2 = match (t1,t2) with
  | Empty, _ | _, Empty -> Empty
  | t, Leaf (k,x) | Leaf (k,x), t ->
      (try may_leaf k (f x (find k t2)) with Not_found -> Empty)
  | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
      if m == n && match_prefix q p m then
	branch (p,m,inter f s0 t0,inter f s1 t1)
      else if m < n && match_prefix q p m then
	(* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	if zero_bit q m then 
	  branch (p,m,inter f s0 t,s1)
        else 
	  branch (p,m,s0,inter f s1 t)
      else if m > n && match_prefix p q n then
	(* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	if zero_bit p n then
	  branch (q,n,inter f t0 s,t1)
	else 
	  branch (q,n,t0,inter f t1 s)
      else
	Empty

