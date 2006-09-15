(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Locations in source file,
   and presentation of results and errors *)

type source = 
    [ `None | `File of string | `Stream | `String of string 
    | `Buffer of Buffer.t ]

type loc = source * int * int
type precise = [ `Full | `Char of int ]

exception Location of loc * precise * exn
exception Generic of string

val noloc:loc
val nopos:int * int

val merge_loc: loc -> loc -> loc

val raise_loc: int -> int -> exn -> 'a
val raise_generic: string -> 'a
val raise_loc_generic: loc -> string -> 'a

val push_source: source -> unit
val pop_source: unit -> unit

val current_dir : unit -> string

val set_viewport: Html.t -> unit
val get_viewport: unit -> Html.t

(*
val protect: Format.formatter -> (Format.formatter -> unit) -> unit
*)

val print_loc: Format.formatter -> loc * precise -> unit
val dump_loc: loc * precise -> unit
val html_hilight: loc * precise -> unit

type 'a located = { loc : loc; descr : 'a }
val mk: int * int -> 'a -> 'a located
val mk_loc: loc -> 'a -> 'a located
val mknoloc: 'a -> 'a located

val loc_of_pos : int * int -> loc


(* Are we working in a protected environement (web prototype ...) ? *)
val set_protected : bool -> unit
val is_protected : unit -> bool
val protect_op : string -> unit
