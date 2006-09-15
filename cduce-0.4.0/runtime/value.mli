(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident
open Encodings

type t =
(* Canonical representation *)
  | Pair of t * t
  | Xml of t * t * t
  | XmlNs of t * t * t * Ns.table
  | Record of t Imap.t
  | Atom of Atoms.V.t
  | Integer of Intervals.V.t
  | Char of Chars.V.t
  | Abstraction of (Types.descr * Types.descr) list option * (t -> t)
  | Abstract of Types.Abstract.V.t

(* Derived forms *)
  | String_latin1 of int * int * string * t
  | String_utf8 of U.uindex * U.uindex * U.t * t
  | Concat of t * t

(* Special value for absent record fields, and failed pattern matching *)
  | Absent

module ValueSet: Set.S with type elt = t

exception CDuceExn of t
val raise': t -> 'a           (* "raise" for CDuce exceptions *)
val failwith': string -> 'a   (* "failwith" for CDuce exceptions *)
val tagged_tuple: string -> t list -> t

val print: Format.formatter -> t -> unit
val dump_xml: Format.formatter -> t -> unit

val normalize: t -> t
  (* Transform a derived form to its canonical equivalent *)

val const : Types.const -> t  (* extract the const value from a const type *)
val inv_const : t -> Types.const  (* build a const type from a const value *)
val string_latin1 : string -> t
val string_utf8 : U.t -> t
val substring_utf8: U.uindex -> U.uindex -> U.t -> t -> t
val nil : t
val vtrue : t
val vfalse : t
val vbool : bool -> t

  (** @return a Record value from an associative list of fields *)
val vrecord : (Label.t * t) list -> t

val sequence : t list -> t
val sequence_rev : t list -> t
val get_sequence : t -> t list
val get_sequence_rev : t -> t list

val fold_sequence: ('a -> t -> 'a) -> 'a -> t -> 'a

val atom_ascii : string -> t
val label_ascii : string -> label
val record : (label * t) list -> t
val record_ascii : (string * t) list -> t
val get_field : t -> label -> t
val get_field_ascii : t -> string -> t

val get_variant : t -> string * t option

val abstract : Types.Abstract.abs -> 'a -> t
val get_abstract : t -> 'a

val mk_ref : Types.t -> t -> t
val mk_ext_ref : Types.t option -> (unit -> t) -> (t -> unit) -> t

  (* iterator on the content of an Xml value. First callback is invoked on Utf8
  character children; second callback is invoked on other children values *)
val iter_xml : (U.t -> unit) -> (t -> unit) -> t -> unit

(*
  (* as above for map *)
val map_xml : (U.t -> U.t) -> (t -> t) -> t -> t
*)

val concat : t -> t -> t
val flatten : t -> t
val append : t -> t -> t

val float: float -> t

val get_string_latin1 : t -> string
val get_string_utf8 : t -> U.t * t
val is_str : t -> bool
val is_seq : t -> bool

val get_int : t -> int
val get_integer : t -> Intervals.V.t

  (** @return an associative list of fields from a Record value *)
val get_fields : t -> (Label.t * t) list

val get_pair : t -> t * t

val hash: t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val ( |<| ): t -> t -> bool
val ( |>| ): t -> t -> bool
val ( |<=| ): t -> t -> bool
val ( |>=| ): t -> t -> bool
val ( |=| ): t -> t -> bool
val ( |<>| ): t -> t -> bool


val set_cdr : t -> t -> unit
val append_cdr : t -> t -> t


val ocaml2cduce_int : int -> t
val cduce2ocaml_int : t -> int
val ocaml2cduce_string : string -> t
val cduce2ocaml_string : t -> string
val ocaml2cduce_string_utf8 : U.t -> t
val cduce2ocaml_string_utf8 : t -> U.t
val ocaml2cduce_char : char -> t
val cduce2ocaml_char : t -> char
val ocaml2cduce_bigint : Big_int.big_int -> t
val cduce2ocaml_bigint : t -> Big_int.big_int
val ocaml2cduce_option: ('a -> t) -> 'a option -> t
val cduce2ocaml_option: (t -> 'a) -> t -> 'a option
val ocaml2cduce_wchar : int -> t
val ocaml2cduce_atom : Atoms.V.t -> t
val cduce2ocaml_atom : t -> Atoms.V.t
val ocaml2cduce_list : ('a -> t) -> 'a list -> t
val cduce2ocaml_list : (t -> 'a) -> t -> 'a list
val ocaml2cduce_array : ('a -> t) -> 'a array -> t
val cduce2ocaml_array : (t -> 'a) -> t -> 'a array
val ocaml2cduce_constr: t -> t array -> t
val cduce2ocaml_constr: int Atoms.map -> t -> Obj.t
val cduce2ocaml_variant: int Atoms.map -> t -> Obj.t
val ocaml2cduce_int32 : int32 -> t
val cduce2ocaml_int32 : t -> int32
val ocaml2cduce_int64 : int64 -> t
val cduce2ocaml_int64 : t -> int64
val ocaml2cduce_fun: (t -> 'a) -> ('b -> t) -> ('a -> 'b) -> t
val cduce2ocaml_fun: ('a -> t) -> (t -> 'b) -> t -> ('a -> 'b)

val print_utf8: U.t -> unit


val add: t -> t -> t
val merge: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t
val modulo: t -> t -> t
val pair: t -> t -> t
val xml: t -> t -> t -> t
val apply: t -> t -> t
val mk_record: label array -> t array -> t

val transform: (t -> t) -> t -> t
val xtransform: (t -> t) -> t -> t
val remove_field: label -> t -> t



type pools
val extract_all: unit -> pools
val intract_all: pools -> unit
