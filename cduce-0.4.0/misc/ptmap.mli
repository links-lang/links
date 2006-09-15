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

(*i $Id: ptmap.mli,v 1.6 2004/07/21 08:39:44 filliatr Exp $ i*)

(*s Maps over integers implemented as Patricia trees.
    The following signature is exactly [Map.S with type key = int],
    with the same specifications. *)

type (+'a) t

type key = int

val empty : 'a t

val is_empty : 'a t -> bool

val singleton : int -> 'a -> 'a t

val add : int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val remove : int -> 'a t -> 'a t

val mem :  int -> 'a t -> bool

val iter : (int -> 'a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val set : int -> ('a option -> 'a) -> 'a t -> 'a t

val unset : int -> ('a -> 'a option) -> 'a t -> 'a t

val hash : ('a -> int) -> 'a t -> int

val subset : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val is_singleton : ('a -> 'b option) -> 'a t -> 'b option

val disjoint : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val diff : ('a -> 'b -> 'a option) -> 'a t -> 'b t -> 'a t

val inter : ('a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
