open Sugartypes
open Operators

module Make : sig
  (* Positions *)
  type ppos = SourceCode.lexpos * SourceCode.lexpos
  val pos : ppos -> position
  val with_pos : ppos -> 'a -> 'a with_pos

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
  val cp_unit      : ppos -> cp_phrase
  val record       : ppos -> (name * phrase) list -> phrase
  val tuple        : ppos -> phrase list -> phrase
  val variable_pat : ppos -> name with_pos -> pattern

  (* Fieldspec *)
  val present : fieldspec

  (* Rows *)
  val fresh_row         : unit -> row
  val row_with_wp       : row -> row
  val hear_arrow_prefix : datatype -> row -> row

  (* Various phrases *)
  val fun_lit
      : ppos -> declared_linearity -> pattern list list -> block_body with_pos
     -> phrase
  val hnlit_arg
      : [`Deep | `Shallow ] -> pattern -> clause list * pattern list list option
     -> handlerlit
  val handler_lit
      : ppos -> handlerlit -> phrase
  val spawn
      : ppos -> spawn_kind -> given_spawn_location -> block_body with_pos
     -> phrase

  (* Bindings *)
  val fun_binding
      : signature -> ppos
     -> (declared_linearity * name with_pos * pattern list list * location *
         block_body with_pos)
     -> binding
  val handler_binding
      : signature -> ppos -> (name with_pos * handlerlit)
     -> binding
  val val_binding
      : signature -> ppos -> (name_or_pat * phrase * location)
     -> binding

  (* Database queries *)
  val db_exps
      : ppos -> (name * phrase) list -> phrase
  val db_insert
      : ppos -> phrase -> name list -> phrase -> string with_pos option
     -> phrase
  val query
      : ppos -> (phrase * phrase) option -> block_body with_pos -> phrase

  (* Operator applications *)
  val infix_appl' : ppos -> phrase -> binop    -> phrase -> phrase
  val infix_appl  : ppos -> phrase -> string   -> phrase -> phrase
  val unary_appl  : ppos ->           unary_op -> phrase -> phrase

  (* XML *)
  val xml
      : ppos -> (string * string) option -> name
     -> (name * (phrase list)) list -> block_body with_pos option -> phrase list
     -> phrase
end
