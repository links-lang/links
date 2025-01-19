(* This module contains module signatures used by SugarConstructors module.
   Putting them here allows to avoid repetition. *)

open CommonTypes
open Operators
open SourceCode
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
  val pos      : t -> Position.t
  (* Produce a syntax tree node with a position attached. *)
  val with_pos : t -> 'a -> 'a WithPos.t
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
  val pos      : t -> Position.t
  val with_pos : t -> 'a -> 'a WithPos.t
  val dp       : t

  (* Attach a dummy position to a node. *)
  val with_dummy_pos : 'a -> 'a WithPos.t

  (* Fresh type variables. *)
  val fresh_type_variable  : Freedom.t -> SugarTypeVar.t

  (* Helper data types and functions for passing arguments to smart
     constructors.  *)
  type name_or_pat = PatName of Name.t
                   | Pat     of Pattern.with_pos

  type signature = (Name.t WithPos.t * datatype') WithPos.t option

  (* Common stuff *)
  val var         : ?ppos:t -> Name.t -> phrase
  val freeze_var  : ?ppos:t -> Name.t -> phrase
  val block       : ?ppos:t -> block_body -> phrase
  val block_node  :            block_body -> phrasenode
  val datatype    : Datatype.with_pos -> Datatype.with_pos * 'a option
  val cp_unit     : t -> cp_phrase
  val record      : ?ppos:t -> ?exp:phrase -> (Name.t * phrase) list -> phrase
  val tuple       : ?ppos:t -> phrase list -> phrase
  val orderby_tuple : ?ppos:t -> phrase list -> phrase
  val list        :
    ?ppos:t -> ?ty:Types.datatype -> phrase list -> phrase
  val constructor :
    ?ppos:t -> ?body:phrase -> ?ty:Types.datatype -> Name.t -> phrase
  val type_abstraction : ?ppos:t -> SugarQuantifier.t list -> phrase -> phrase


  (* Constants *)
  val constant      : ?ppos:t -> Constant.t -> phrase
  val constant_str  : ?ppos:t -> string     -> phrase
  val constant_char : ?ppos:t -> char       -> phrase

  (* Binders *)
  val binder   : ?ppos:t -> ?ty:Types.datatype -> Name.t -> Binder.with_pos

  (* Imports *)
  val import : ?ppos:t -> ?pollute:bool -> Name.t list -> binding

  (* Patterns *)
  val variable_pat : ?ppos:t -> ?ty:Types.datatype -> Name.t -> Pattern.with_pos
  val tuple_pat    : ?ppos:t -> Pattern.with_pos list -> Pattern.with_pos
  val any_pat      : t -> Pattern.with_pos

  (* Fieldspec *)
  val present : Datatype.fieldspec

  (* Rows *)
  val row_with_wp       : Datatype.row -> Datatype.row
  val hear_arrow_prefix : Datatype.with_pos -> Datatype.row -> Datatype.row

  (* Various phrases *)
  val fun_lit
      : ?ppos:t -> ?args:((Types.datatype * Types.row) list)
     -> ?location:Location.t -> DeclaredLinearity.t
     -> Pattern.with_pos list list -> phrase
     -> phrase
  val switch_fun_lit
      : ?ppos:t -> ?args:((Types.datatype * Types.row) list)
     -> ?location:Location.t -> DeclaredLinearity.t
     -> Pattern.with_pos list list -> switch_funlit_body
     -> phrase
  val spawn
      : ?ppos:t
     -> ?row:Types.row -> spawn_kind -> given_spawn_location -> phrase
     -> phrase
  val fn_appl_node
      : ?ppos:t -> Name.t -> tyarg list -> phrase list -> phrasenode
  val fn_appl
      : ?ppos:t -> Name.t -> tyarg list -> phrase list -> phrase
  val fn_appl_var
      : ?ppos:t -> Name.t -> Name.t -> phrase

  (* Bindings *)
  val fun_binding
      : ?ppos:t -> signature -> ?unsafe_sig:bool
     -> ((DeclaredLinearity.t * bool) * Name.t * Pattern.with_pos list list * Location.t * phrase)
     -> binding
  val fun_binding'
      : ?ppos:t -> ?linearity:DeclaredLinearity.t -> ?tyvars:SugarQuantifier.t list
     -> ?location:Location.t -> ?annotation:datatype'
     -> Binder.with_pos -> funlit
     -> binding
  val switch_fun_binding
      : ?ppos:t -> signature -> ?unsafe_sig:bool
     -> ((DeclaredLinearity.t * bool) * Name.t * Pattern.with_pos list list * Location.t * switch_funlit_body)
     -> binding
  val val_binding'
      : ?ppos:t -> signature -> (name_or_pat * phrase * Location.t)
     -> binding
  val val_binding
      : ?ppos:t -> Pattern.with_pos -> phrase
     -> binding

  val module_binding
      : ?ppos:t -> Binder.with_pos -> binding list
     -> binding

  (* Database queries *)
  val db_exps
      : ?ppos:t -> (Name.t * phrase) list -> phrase
  val db_insert
      : ?ppos:t -> temporal_insertion option -> phrase -> Name.t list -> phrase -> string option
     -> phrase
  val query
      : ?ppos:t -> (phrase * phrase) option -> QueryPolicy.t -> phrase -> phrase
  val temporal_join
      : ?ppos:t -> Temporality.t -> phrase -> phrase

  (* Operator applications *)
  val infix_appl' : ?ppos:t -> phrase -> BinaryOp.t -> phrase -> phrase
  val infix_appl  : ?ppos:t -> phrase -> string     -> phrase -> phrase
  val unary_appl  : ?ppos:t ->           UnaryOp.t  -> phrase -> phrase

  (* XML *)
  val validate_xml
      : ?tags:(string * string) -> phrase -> unit
  val xml
      : ?ppos:t -> ?tags:(string * string) -> Name.t
     -> (Name.t * (phrase list)) list -> phrase option -> phrase list
     -> phrase

  (* Handlers *)
  val untyped_handler
      :?parameters:((Pattern.with_pos * phrase) list)
     -> phrase -> (clause list * clause list) -> handler_depth
     -> handler

  val table
      : ?ppos:t
     -> tbl_keys:(phrase option)
     -> phrase (* Name *)
     -> Datatype.with_pos (* Type *)
     -> (Name.t * fieldconstraint list) list (* Field constraints *)
     -> (Temporality.t * (string * string)) option (* Temporal to/from fields *)
     -> phrase (* Database *)
     -> phrase (* TableLit *)
end
