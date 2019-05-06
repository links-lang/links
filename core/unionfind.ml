(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: unionfind.ml,v 1.3 2006/03/16 12:38:04 s0456219 Exp $ *)

(** This module implements a simple and efficient union/find algorithm. See
   Robert E. Tarjan, ``Efficiency of a Good But Not Linear Set Union
   Algorithm'', JACM 22(2), 1975. *)

(** The abstraction defined by this module is a set of points, partitioned into
   equivalence classes. With each equivalence class, a piece of information, of
   abstract type 'a, is associated; we call it a descriptor.

    A point is implemented as a cell, whose (mutable) contents consist of a
   single link to either information about the equivalence class, or another
   point. Thus, points form a graph, which must be acyclic, and whose connected
   components are the equivalence classes. In every equivalence class, exactly
   one point has no outgoing edge, and carries information about the class
   instead. It is the class's representative element.

    Information about a class consists of an integer weight (the number of
   elements in the class) and of the class's descriptor. *)

type 'a point = {
  mutable link: 'a link
}
and 'a link =
  | Info of 'a info
  | Link of 'a point

and 'a info = {
  mutable weight: int;
  mutable descriptor: 'a
}



(** fresh desc creates a fresh point and returns it. It forms an equivalence
   class of its own, whose descriptor is desc. *)
let fresh desc = {
  link = Info { weight = 1; descriptor = desc }
}

(** repr point returns the representative element of point's equivalence
   class. It is found by starting at point and following the links. For
   efficiency, the function performs path compression at the same time. *)
let rec repr point =
  match point.link with
    | Link point' ->
    let point'' = repr point' in
      if point'' != point' then

            (* [point''] is [point']'s representative element. Because we
               just invoked [repr point'], [point'.link] must be [Link
               point'']. We write this value into [point.link], thus
               performing path compression. Note that this function never
               performs memory allocation. *)

            point.link <- point'.link;
      point''
    | Info _ ->
    point

(** find point returns the descriptor associated with point's equivalence
   class. *)
let rec find point =

  (* By not calling [repr] immediately, we optimize the common cases
     where the path starting at [point] has length 0 or 1, at the
     expense of the general case. *)

  match point.link with
    | Info info
    | Link { link = Info info } ->
    info.descriptor
    | Link { link = Link _ } ->
    find (repr point)

let rec change point v =
  match point.link with
    | Info info
    | Link { link = Info info } ->
    info.descriptor <- v
(* [SL]
  changed the return type to unit to negate the need for
  lots of 'ignores'
*)
(*; info.descriptor*)
    | Link { link = Link _ } ->
    change (repr point) v

(** equivalent point1 point2 tells whether point1 and point2 belong to the same
   equivalence class. *)
let equivalent point1 point2 =
  repr point1 == repr point2

(** union point1 point2 merges the equivalence classes associated with point1
   and point2 (which must be distinct) into a single class whose descriptor is
   that originally associated with point2.

    The fact that point1 and point2 do not originally belong to the same class
   guarantees that we do not create a cycle in the graph.

    The weights are used to determine whether point1 should be made to point to
   point2, or vice-versa. By making the representative of the smaller class
   point to that of the larger class, we guarantee that paths remain of
   logarithmic length (not accounting for path compression, which makes them yet
   smaller). *)
(* [SL]
     modified to allow point1 and point2 to be equal - in this case do nothing
*)
let union point1 point2 =
  if not (equivalent point1 point2) then
    let point1 = repr point1
    and point2 = repr point2 in
      assert (point1 != point2);
      match point1.link, point2.link with
    | Info info1, Info info2 ->
        let weight1 = info1.weight
        and weight2 = info2.weight in
          if weight1 >= weight2 then begin
        point2.link <- Link point1;
        info1.weight <- weight1 + weight2;
        info1.descriptor <- info2.descriptor
          end
          else begin
        point1.link <- Link point2;
        info2.weight <- weight1 + weight2
          end
    | _, _ ->
        assert false (* [repr] guarantees that [link] matches [Info _]. *)

(* Prints the address of the representative point in order to debug sharing
   effects *)
let pp_point pf formatter p =
  let address_of (x:'a) : nativeint =
  if Obj.is_block (Obj.repr x) then
    Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1
  else
    invalid_arg "Can only find address of boxed values." in
  let repr_elt = repr p in
  let descriptor = match repr_elt.link with
    | Info i -> i.descriptor
    | _ -> assert false (* guaranteed not to happend by repr *) in
  let addr_of_repr  = address_of repr_elt in
  Format.pp_print_string formatter  (Printf.sprintf "Point @ 0x%nx " addr_of_repr);
  pf formatter descriptor

let show_point f v = Format.asprintf "%a" (pp_point f) v
