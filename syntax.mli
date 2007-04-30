(*pp deriving *)
type location = [ `Client | `Native | `Server | `Unknown ]
type comparison = [`Less | `LessEq | `Equal | `NotEq] deriving (Typeable, Show, Pickle, Eq, Shelve)
type label deriving (Typeable, Show, Pickle)

type constant =
  | Boolean of bool
  | Integer of Num.num
  | Char of char
  | String of string
  | Float of float

type 'a expression' =
  | Constant of (constant * 'a)
  | Variable of (string * 'a)
  | Apply of ('a expression' * 'a expression' list * 'a)
  | Condition of ('a expression' * 'a expression' * 'a expression' * 'a)
  | Comparison of ('a expression' * comparison * 'a expression' * 'a)
  | Abstr of (string list * 'a expression' * 'a)
  | Let of (string * 'a expression' * 'a expression' * 'a)
  | Rec of ((string * 'a expression' * Types.datatype option) list * 'a expression' * 'a)
  | Xml_node of (string * (string * 'a expression') list * 'a expression' list * 'a)
  | Record_intro of (('a expression') Utility.stringmap * ('a expression') option * 'a)
  | Record_selection of (string * string * string * 'a expression' * 'a expression' * 'a)
  | Project of ('a expression' * string * 'a)
  | Erase of ('a expression' * string * 'a)
  | Variant_injection of (string * 'a expression' * 'a)
  | Variant_selection of ('a expression' * string * string * 'a expression' * 
                            string * 'a expression' * 'a)
  | Variant_selection_empty of ('a expression' * 'a)
  | Nil of 'a
  | List_of of ('a expression' * 'a)
  | Concat of ('a expression' * 'a expression' * 'a)
  | For of ('a expression' * string * 'a expression' * 'a)
  | Database of ('a expression' * 'a)
  | TableQuery of ((string * 'a expression') list * Query.query * 'a)
  | TableHandle of ('a expression' * 'a expression' * (Types.datatype * Types.datatype) * 'a)
  | SortBy of ('a expression' * 'a expression' * 'a)
  | Call_cc of ('a expression' * 'a)
  | Wrong of 'a
  | HasType of ('a expression' * Types.datatype * 'a)
  | Placeholder of (label * 'a)

type 'a definition' =
  | Define of (string * 'a expression' * location * 'a)
  | Alias of (string * int list * Types.datatype * 'a)
  | Alien of (string * string * Types.assumption * 'a)

type 'a program' = Program of ('a definition' list * 'a expression')

type position = Lexing.position * string * string

type untyped_data = [`U of position]
type typed_data = [`T of (position * Types.datatype * label option)]

type expression = typed_data expression' deriving (Typeable, Show, Eq)
type untyped_expression = untyped_data expression'
type stripped_expression = unit expression' deriving (Show)

type definition = typed_data definition' deriving (Typeable, Show, Eq)
type untyped_definition = untyped_data definition'
type stripped_definition = unit definition'

type program = typed_data program' deriving (Typeable, Show, Eq)
type untyped_program = untyped_data program'
type stripped_program = unit program'

exception ASTSyntaxError of position * string

val unit_expression : 'a -> 'a expression'

val list_expr : 'a -> 'a expression' list -> 'a expression'

val is_value : 'a expression' -> bool

val string_of_expression : 'a expression' -> string
val labelled_string_of_expression : expression -> string

val string_of_definition : 'a definition' -> string
val labelled_string_of_definition : definition -> string

val string_of_program : 'a program' -> string
val labelled_string_of_program : program -> string

val stringlit_value : 'a expression' -> string

val freevars : 'a expression' -> string list
val freevars_def : 'a definition' -> string list
val freevars_program : 'a program' -> string list

(** {0 Variable-substitution functions} 
    TBD: gen'ize these for typed & other exprs as well *)
val subst_free : string -> untyped_expression -> untyped_expression -> untyped_expression
val rename_free : string -> string -> untyped_expression -> untyped_expression
val subst_fast : string -> expression -> expression -> expression
val rename_fast : string -> string -> expression -> expression

val subst_fast_def : string -> expression -> definition -> definition

val reduce_expression :
  (('a expression' -> 'b) -> 'a expression' -> 'b) -> 
  ('a expression' * 'b list -> 'b) -> 'a expression' -> 'b
val reduce_definition :
  (('a expression' -> 'b) -> 'a expression' -> 'b) -> 
  ('a expression' * 'b list -> 'b) ->
  ('a definition' * 'b list -> 'b) ->
  'a definition' -> 'b
val reduce_program :
  (('a expression' -> 'b) -> 'a expression' -> 'b) -> 
  ('a expression' * 'b list -> 'b) ->
  ('a definition' * 'b list -> 'b) ->
  ('b list * 'b -> 'b) ->
  'a program' -> 'b

val set_subnodes : 'a expression' -> 'a expression' list -> 'a expression'

val expression_data : 'a expression' -> 'a
val strip_data : 'a expression' -> stripped_expression
val node_datatype : expression -> Types.datatype
val def_datatype : definition -> Types.datatype

type data = [untyped_data | typed_data]

val data_position : [<data] -> position
val position : [<data] expression' -> position

val erase : expression -> untyped_expression
val labelize : program -> program

val dummy_position : position
val no_expr_data : typed_data

(* Which variables are l:name-bound? *)
val lname_bound_vars : 'data expression' -> string list

module RewriteUntypedExpression : Rewrite.Rewrite with type t = untyped_expression
module RewriteSyntax : Rewrite.Rewrite with type t = expression

val transform_def :
  ('a expression' -> 'a expression') -> 'a definition' -> 'a definition'
val transform_program :
  ('a expression' -> 'a expression') -> 'a program' -> 'a program'

val rewrite_def :
  ('a expression' -> 'a expression' option) -> 'a definition' -> 'a definition'
val rewrite_program :
  ('a expression' -> 'a expression' option) -> 'a program' -> 'a program'

module Functor_expression' : Functor.Functor with type 'a f = 'a expression'

