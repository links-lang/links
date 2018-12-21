(* This module contains module signatures used by SmartConstructors module.
   Putting them here allows to avoid repetition. *)

open Sugartypes
open Operators

(* An abstract type of positions and operations on them.  The core type of
   positions used in the compilation pipeline is Sugartypes.position, which
   again is an alias to SourceCode.pos.  However, in several places we operate
   on positions of a different type.  Most importantly, the parser produces a
   positions in a different format.  What is important is that there needs to be
   a way of converting such positions to Sugartypes.position and this is what
   modules implementing this signature provide. *)
module type Pos = sig
  (* Type of positions *)
  type t
  (* Convert a position to Sugartypes.position *)
  val pos      : t -> Sugartypes.position
  (* Produce a syntax tree node with a position attached *)
  val with_pos : t -> 'a -> 'a Sugartypes.with_pos
end

(* Various smart constructors for elements of Links AST.  Module implementing
   this signature is actually a functor on a module of type Pos, defined
   above. *)
module type SmartConstructorsSig = sig

  (* Positions and functions on them.  Repeated here to avoid need for name
     qualification and additional module opens. *)
  type t
  val pos      : t -> Sugartypes.position
  val with_pos : t -> 'a -> 'a with_pos

  (* Fresh type variables *)
  val fresh_type_variable           : unit -> datatypenode
  val fresh_rigid_type_variable     : unit -> datatypenode
  val fresh_row_variable            : unit -> row_var
  val fresh_rigid_row_variable      : unit -> row_var
  val fresh_presence_variable       : unit -> fieldspec
  val fresh_rigid_presence_variable : unit -> fieldspec

  (* Helper data types and functions for passing arguments to smart constructors
     *)
  type name_or_pat = Name of name with_pos
                   | Pat of pattern

  type signature   = Sig of (name with_pos * datatype') with_pos
                   | NoSig

  val sig_of_opt : (name with_pos * datatype') with_pos option -> signature

  (* Common stuff *)
  val block        : block_body with_pos -> phrase
  val datatype     : datatype -> datatype * 'a option
  val cp_unit      : t -> cp_phrase
  val record       : t -> (name * phrase) list -> phrase
  val tuple        : t -> phrase list -> phrase
  val variable_pat : t -> name with_pos -> pattern

  (* Fieldspec *)
  val present : fieldspec

  (* Rows *)
  val fresh_row         : unit -> row
  val row_with_wp       : row -> row
  val hear_arrow_prefix : datatype -> row -> row

  (* Various phrases *)
  val fun_lit
      : t -> declared_linearity -> pattern list list -> block_body with_pos
     -> phrase
  val hnlit_arg
      : [`Deep | `Shallow ] -> pattern -> clause list * pattern list list option
     -> handlerlit
  val handler_lit
      : t -> handlerlit -> phrase
  val spawn
      : t -> spawn_kind -> given_spawn_location -> block_body with_pos
     -> phrase

  (* Bindings *)
  val fun_binding
      : t -> signature
     -> (declared_linearity * name with_pos * pattern list list * location *
         block_body with_pos)
     -> binding
  val handler_binding
      : t -> signature -> (name with_pos * handlerlit)
     -> binding
  val val_binding
      : t -> signature -> (name_or_pat * phrase * location)
     -> binding

  (* Database queries *)
  val db_exps
      : t -> (name * phrase) list -> phrase
  val db_insert
      : t -> phrase -> name list -> phrase -> string with_pos option
     -> phrase
  val query
      : t -> (phrase * phrase) option -> block_body with_pos -> phrase

  (* Operator applications *)
  val infix_appl' : t -> phrase -> binop    -> phrase -> phrase
  val infix_appl  : t -> phrase -> string   -> phrase -> phrase
  val unary_appl  : t ->           unary_op -> phrase -> phrase

  (* XML *)
  val xml
      : t -> (string * string) option -> name
     -> (name * (phrase list)) list -> block_body with_pos option -> phrase list
     -> phrase

  (* Handlers *)
  val untyped_handler
      : ?val_cases:(clause list) -> ?parameters:((phrase * pattern) list)
     -> phrase -> clause list -> [`Deep | `Shallow]
     -> handler
end
