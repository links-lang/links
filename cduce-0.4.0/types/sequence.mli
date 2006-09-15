(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type error = 
  | CopyTag of Types.t * Types.t 
  | CopyAttr of Types.t * Types.t
  | UnderTag of Types.t * exn
exception Error of error

val nil_type: Types.t
val nil_node: Types.Node.t
val nil_atom: Atoms.V.t
val nil_cst:  Types.Const.t
val any: Types.t
val seqseq: Types.t
val string: Types.t
val string_latin1: Types.t
val char_latin1: Types.t
val any_xtransformable: Types.t

val concat: Types.t -> Types.t -> Types.t
val flatten: Types.t -> Types.t
val map: (Types.t -> Types.t) -> Types.t -> Types.t


val map_tree: 
  Types.t -> (Types.t -> Types.t -> Types.t * Types.t) -> Types.t -> Types.t
 (* maximal output type -> input type -> (result, residual) *)         
  (* sequence type *)

val map_mono: Types.t -> Types.t list * (Types.t list -> Types.t)
val map_tree_mono: Types.t -> Types.t -> Types.t * Types.t list * (Types.t list -> Types.t)
  (* Arguments: domain of all branches, input type.
     Results: input type for all branches, all the individual input types,
     the function to call when all the individual output types have been
     computed *)

val star: Types.t -> Types.t
  (* For a type t, returns [t*] *)
val plus: Types.t -> Types.t
val option: Types.Node.t -> Types.t
  (* returns [t?] *)

val repet: int -> int option -> Types.t -> Types.t
  (* min, max *)

val approx: Types.t -> Types.t
  (*  For a type t <= [Any*], returns the least type s such that:
      t <= [s*]
      
      In general, for an arbitrary type, returns the least type s such that:
      t <= (X where X = (s, X) |  Any \ (Any,Any))
  *)

(* Added this interface needed in cdo2cmo -- Julien *)
val star_node : Types.Node.t -> Types.Node.t

  (** given a list of descrs create the sequence type from them *)
val seq_of_list: Types.t list -> Types.t

