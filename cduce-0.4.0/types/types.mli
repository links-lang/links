(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

type const = 
  | Integer of Intervals.V.t
  | Atom of Atoms.V.t
  | Char of Chars.V.t
  | Pair of const * const
  | Xml of const * const
  | Record of const label_map
  | String of U.uindex * U.uindex * U.t * const


module Const: Custom.T with type t = const

(*
module CompUnit : sig
  include Custom.T

  val get_current: unit -> t
  val mk: U.t -> t
  val value: t -> U.t
  val print_qual: Format.formatter -> t -> unit

  val enter: t -> unit
  val leave: unit -> unit
  val close_serialize: unit -> t list

  val pervasives: t

  module Tbl : Inttbl.S with type key = t
end
*)

module Abstract : sig
  module T : Custom.T with type t = string
  type abs = T.t
  type t
  val any: t
  val atom: abs -> t
  val compare: t -> t -> int

  module V : sig
    type t = abs * Obj.t
  end

  val contains: abs -> t -> bool
end

(** Algebra **)

include Custom.T
module Node : Custom.T

type descr = t

val make: unit -> Node.t
val define: Node.t -> t -> unit

val cons: t -> Node.t
val internalize: Node.t -> Node.t

val id: Node.t -> int
val descr: Node.t -> t

(** Boolean connectives **)

val cup    : t -> t -> t
val cap    : t -> t -> t
val diff   : t -> t -> t
val neg    : t -> t
val empty  : t
val any    : t

val any_node : Node.t
val empty_node : Node.t

val non_constructed : t
val non_constructed_or_absent : t

(** Constructors **)

type pair_kind = [ `Normal | `XML ]

val interval : Intervals.t -> t
val atom     : Atoms.t -> t
val times    : Node.t -> Node.t -> t
val xml      : Node.t -> Node.t -> t
val arrow    : Node.t -> Node.t -> t
val record   : label -> Node.t -> t
  (* bool = true -> open record; bool = false -> closed record *)
val record_fields : bool * Node.t label_map -> t
val char     : Chars.t -> t
val constant : const -> t
val abstract : Abstract.t -> t

(** Helpers *)

val tuple : Node.t list -> t

val rec_of_list: bool -> (bool * Ns.Label.t * t) list -> t

val empty_closed_record: t
val empty_open_record: t

(** Positive systems and least solutions **)

module Positive :
sig
  type v
  val forward: unit -> v
  val define: v -> v -> unit
  val ty: t -> v
  val cup: v list -> v
  val times: v -> v -> v
  val xml: v -> v -> v

  val solve: v -> Node.t
end

(** Normalization **)

module Product : sig
  val any : t
  val any_xml : t
  val any_of: pair_kind -> t
  val other : ?kind:pair_kind -> t -> t
  val is_product : ?kind:pair_kind -> t -> bool

  (* List of non-empty rectangles *)
  type t = (descr * descr) list
  val is_empty: t -> bool
  val get: ?kind:pair_kind -> descr -> t
  val pi1: t -> descr
  val pi2: t -> descr
  val pi2_restricted: descr -> t -> descr
    
  (* Intersection with (pi1,Any) *)
  val restrict_1: t -> descr -> t

  (* List of non-empty rectangles whose first projection
     are pair-wise disjunct *)
  type normal = t
  val normal: ?kind:pair_kind -> descr -> normal

  val constraint_on_2: normal -> descr -> descr
    (* constraint_on_2 n t1:  maximal t2 such that (t1,t2) <= n *)
    (* Assumption: t1 <= pi1(n) *)

  val need_second: t -> bool
    (* Is there more than a single rectangle ? *)


  val clean_normal: t -> t
    (* Merge rectangles with same second component *)
end

module Record : sig
  val any : t
  val absent : t
  val absent_node : Node.t
  val or_absent: t -> t
  val any_or_absent: t
  val any_or_absent_node : Node.t

  val has_absent: t -> bool
  val has_record: t -> bool

  val split : t -> label -> Product.t
  val split_normal : t -> label -> Product.normal

  val pi : label -> t -> t
    (* May contain absent *)

  val project : t -> label -> t
    (* Raise Not_found if label is not necessarily present *)

  val condition : t -> label -> t -> t
    (* condition t1 l t2 : What must follow if field l hash type t2 *)
  val project_opt : t -> label -> t
  val has_empty_record: t -> bool


  val first_label: t -> label
  val all_labels: t -> LabelSet.t

  val empty_cases: t -> bool * bool

  val merge: t -> t -> t
  val remove_field: t -> label -> t

  val get: t -> ((bool * t) label_map * bool * bool) list
end

module Arrow : sig
  val any : t

  val sample: t -> t

  val check_strenghten: t -> t -> t
    (* [check_strenghten t s]
       Assume that [t] is an intersection of arrow types
       representing the interface of an abstraction;
       check that this abstraction has type [s] (otherwise raise Not_found)
       and returns a refined type for this abstraction.
    *)

  val check_iface: (t * t) list -> t -> bool

  type t
  val is_empty: t -> bool
  val get: descr -> t
    (* Always succeed; no check <= Arrow.any *)

  val domain: t -> descr
  val apply: t -> descr -> descr
    (* Always succeed; no check on the domain *)

  val need_arg : t -> bool
    (* True if the type of the argument is needed to obtain
       the type of the result (must use [apply]; otherwise,
       [apply_noarg] is enough *)
  val apply_noarg : t -> descr
end


module Int : sig
  val has_int : t -> Intervals.V.t -> bool
  val get: t -> Intervals.t
  val any : t
end

module Atom : sig
  val has_atom : t -> Atoms.V.t -> bool
  val get: t -> Atoms.t
  val any : t
end

module Char : sig
  val has_char : t -> Chars.V.t -> bool
  val is_empty : t -> bool
  val get: t -> Chars.t
  val any : t
end

val get_abstract: t -> Abstract.t

val normalize : t -> t

(** Subtyping  **)

val is_empty : t -> bool
val non_empty: t -> bool
val subtype  : t -> t -> bool
val disjoint : t -> t -> bool
val equiv : t -> t -> bool

(** Tools for compilation of PM **)

val cond_partition: t -> (t * t) list -> t list
  (* The second argument is a list of pair of types (ti,si)
     interpreted as the question "member of ti under the assumption si".
     The result is a partition of the first argument which is precise enough
     to answer all the questions. *)


module Print :
sig
  val register_global : string -> Ns.QName.t -> t -> unit
  val print_const : Format.formatter -> const -> unit
  val print: Format.formatter -> t -> unit
  val print_node: Format.formatter -> Node.t -> unit

  (* Don't try to find a global name at toplevel *)
  val print_noname: Format.formatter -> t -> unit

  val to_string: t -> string
end


