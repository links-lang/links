(* This module contains module signatures used by SugarConstructors module.
   Putting them here allows to avoid repetition. *)

open Sugartypes

(* An abstract type of positions and operations on them.  The core type of
   positions used in the compilation pipeline is Sugartypes.position, which
   again is an alias to SourceCode.pos.  However, in several places we operate
   on positions of a different type.  Most importantly, the parser produces
   positions in a different format.  What is important is that there needs to be
   a way of converting such positions to Sugartypes.position and this is what
   modules implementing this signature provide. *)
module type Pos = sig
  (* Type of positions. *)
  type t
  (* Convert a position to Sugartypes.position. *)
  val pos      : t -> Sugartypes.position
  (* Produce a syntax tree node with a position attached. *)
  val with_pos : t -> 'a -> 'a Sugartypes.with_pos
  (* Default (dummy) position *)
  val dp       : t
end

(* Various smart constructors for elements of Links AST.  Module implementing
   this signature is actually a functor on a module of type Pos, defined
   above. *)
module type SugarConstructorsSig = sig

  (* Positions and functions on them.  Repeated here to avoid need for name
     qualification and additional module opens. *)
  type t
  val pos      : t -> Sugartypes.position
  val with_pos : t -> 'a -> 'a with_pos
  val dp       : t

  (* Attach a dummy position to a node. *)
  val with_dummy_pos : 'a -> 'a with_pos

  (* Fresh type variables. *)
  val fresh_type_variable           : unit -> Datatype.t
  val fresh_rigid_type_variable     : unit -> Datatype.t
  val fresh_row_variable            : unit -> Datatype.row_var
  val fresh_rigid_row_variable      : unit -> Datatype.row_var
  val fresh_presence_variable       : unit -> Datatype.fieldspec
  val fresh_rigid_presence_variable : unit -> Datatype.fieldspec

  (* Helper data types and functions for passing arguments to smart
     constructors.  *)
  type name_or_pat = PatName of name
                   | Pat     of Pattern.with_pos

  type signature   = Sig of (name with_pos * datatype') with_pos
                   | NoSig

  val sig_of_opt : (name with_pos * datatype') with_pos option -> signature

  (* Common stuff *)
  val var         : ?ppos:t -> name -> phrase
  val block       : ?ppos:t -> block_body -> phrase
  val block_node  :            block_body -> phrasenode
  val datatype    : Datatype.with_pos -> Datatype.with_pos * 'a option
  val cp_unit     : t -> cp_phrase
  val record      : ?ppos:t -> ?exp:phrase -> (name * phrase) list -> phrase
  val tuple       : ?ppos:t -> phrase list -> phrase
  val list        :
    ?ppos:t -> ?ty:Types.datatype -> phrase list -> phrase
  val constructor :
    ?ppos:t -> ?body:phrase -> ?ty:Types.datatype -> name -> phrase

  (* Constants *)
  val constant      : ?ppos:t -> constant -> phrase
  val constant_str  : ?ppos:t -> string   -> phrase
  val constant_char : ?ppos:t -> char     -> phrase

  (* Binders *)
  val binder   : ?ppos:t -> ?ty:Types.datatype -> name -> binder

  (* Patterns *)
  val variable_pat : ?ppos:t -> ?ty:Types.datatype -> name -> Pattern.with_pos
  val tuple_pat    : ?ppos:t -> Pattern.with_pos list -> Pattern.with_pos
  val any_pat      : t -> Pattern.with_pos

  (* Fieldspec *)
  val present : Datatype.fieldspec

  (* Rows *)
  val fresh_row         : unit -> Datatype.row
  val row_with_wp       : Datatype.row -> Datatype.row
  val hear_arrow_prefix : Datatype.with_pos -> Datatype.row -> Datatype.row

  (* Various phrases *)
  val fun_lit
      : ?ppos:t -> ?args:((Types.datatype * Types.row) list)
     -> ?location:location -> declared_linearity -> Pattern.with_pos list list -> phrase
     -> phrase
  val hnlit_arg
      : handler_depth -> Pattern.with_pos -> clause list * Pattern.with_pos list list option
     -> handlerlit
  val handler_lit
      : ?ppos:t -> handlerlit -> phrase
  val spawn
      : ?ppos:t
     -> ?row:Types.row -> spawn_kind -> given_spawn_location -> phrase
     -> phrase
  val fn_appl_node
      : ?ppos:t -> name -> tyarg list -> phrase list -> phrasenode
  val fn_appl
      : ?ppos:t -> name -> tyarg list -> phrase list -> phrase
  val fn_appl_var
      : ?ppos:t -> name -> name -> phrase

  (* Bindings *)
  val fun_binding
      : ?ppos:t -> signature
     -> (declared_linearity * name * Pattern.with_pos list list * location * phrase)
     -> binding
  val fun_binding'
      : ?ppos:t -> ?linearity:declared_linearity -> ?tyvars:tyvar list
     -> ?location:location -> ?annotation:datatype' -> binder -> funlit
     -> binding
  val handler_binding
      : ?ppos:t -> signature -> (name * handlerlit)
     -> binding
  val val_binding'
      : ?ppos:t -> signature -> (name_or_pat * phrase * location)
     -> binding
  val val_binding
      : ?ppos:t -> Pattern.with_pos -> phrase
     -> binding

  (* Database queries *)
  val db_exps
      : ?ppos:t -> (name * phrase) list -> phrase
  val db_insert
      : ?ppos:t -> phrase -> name list -> phrase -> string option
     -> phrase
  val query
      : ?ppos:t -> (phrase * phrase) option -> phrase -> phrase

  (* Operator applications *)
  val infix_appl' : ?ppos:t -> phrase -> binop    -> phrase -> phrase
  val infix_appl  : ?ppos:t -> phrase -> string   -> phrase -> phrase
  val unary_appl  : ?ppos:t ->           unary_op -> phrase -> phrase

  (* XML *)
  val xml
      : ?ppos:t -> ?tags:(string * string) -> name
     -> (name * (phrase list)) list -> phrase option -> phrase list
     -> phrase

  (* Handlers *)
  val untyped_handler
      : ?val_cases:(clause list) -> ?parameters:((phrase * Pattern.with_pos) list)
     -> phrase -> clause list -> handler_depth
     -> handler
end
