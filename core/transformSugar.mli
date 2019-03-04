open Operators
open CommonTypes
open SourceCode
open Sugartypes

(*
  These functions are contravariant in the object type so if
  included as methods in the transform class they would prevent
  upcasting from subclasses of transform to transform.
*)
val option :
    'self_type ->
  ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
  'a option -> ('self_type * ('a option) * (Types.datatype option))

val optionu :
    'self_type ->
    ('self_type -> 'a -> ('self_type * 'a)) ->
  'a option -> ('self_type * ('a option))

val list :
    'self_type ->
    ('self_type -> 'a -> 'self_type * 'a * Types.datatype) ->
  'a list -> 'self_type * 'a list * Types.datatype list

val listu :
    'self_type ->
    ('self_type -> 'a -> 'self_type * 'a) ->
  'a list -> 'self_type * 'a list

(* Transform a term and construct its type *)
class transform : Types.typing_environment ->
object ('self)
  val var_env : Types.environment
  val tycon_env : Types.tycon_environment
  val effect_row : Types.row

  method get_var_env     : unit -> Types.environment
  method get_tycon_env   : unit -> Types.tycon_environment
  method get_formlet_env : unit -> Types.environment

  method backup_envs     :  Types.environment * Types.tycon_environment * Types.environment * Types.row
  method restore_envs    : (Types.environment * Types.tycon_environment * Types.environment * Types.row) -> 'self

  method with_var_env     : Types.environment -> 'self
  method with_formlet_env : Types.environment -> 'self

  method bind_tycon      : string -> Types.tycon_spec -> 'self

  method lookup_type     : name -> Types.datatype
  method lookup_effects  : Types.row
  method with_effects    : Types.row -> 'self

  method binder          : Binder.with_pos -> 'self * Binder.with_pos
  method binding         : binding -> 'self * binding
  method bindingnode     : bindingnode -> 'self * bindingnode
  method binop           : BinaryOp.t -> 'self * BinaryOp.t * Types.datatype
  method constant        : Constant.t -> 'self * Constant.t * Types.datatype
  method funlit          : Types.row -> funlit -> 'self * funlit * Types.datatype
  method handlerlit      : Types.datatype -> handlerlit -> 'self * handlerlit * Types.datatype
  method iterpatt        : iterpatt -> 'self * iterpatt

  method quantifiers     : Types.quantifier list -> 'self * Types.quantifier list
  method backup_quantifiers : Utility.IntSet.t
  method restore_quantifiers : Utility.IntSet.t -> 'self

  method rec_bodies :
    (Binder.with_pos * DeclaredLinearity.t *
     ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) *
     Location.t * datatype' option * Position.t) list ->
    ('self * (Binder.with_pos * DeclaredLinearity.t *
              ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) *
              Location.t * datatype' option * Position.t) list)

  method rec_activate_outer_bindings :
    (Binder.with_pos * DeclaredLinearity.t *
     ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) *
     Location.t * datatype' option * Position.t) list ->
    ('self * (Binder.with_pos * DeclaredLinearity.t *
              ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) *
              Location.t * datatype' option * Position.t) list)

  method rec_activate_inner_bindings :
    (Binder.with_pos * DeclaredLinearity.t *
     ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) *
     Location.t * datatype' option * Position.t) list -> 'self

  method sugar_datatype   : Datatype.with_pos -> 'self * Datatype.with_pos
  method datatype         : Types.datatype -> 'self * Types.datatype
  method datatype'        : datatype' -> 'self * datatype'
  method lens_sort        : Lens_sort.t -> 'self * Lens_sort.t
  method row              : Types.row -> 'self * Types.row

  method patternnode     : Pattern.t -> 'self * Pattern.t
  method pattern         : Pattern.with_pos -> 'self * Pattern.with_pos
  method phrase          : phrase -> 'self * phrase * Types.datatype
  method given_spawn_location : given_spawn_location -> 'self * given_spawn_location
  method phrasenode      : phrasenode -> 'self * phrasenode * Types.datatype
  method cp_phrase       : cp_phrase -> 'self * cp_phrase * Types.datatype
  method cp_phrasenode   : cp_phrasenode -> 'self * cp_phrasenode * Types.datatype
  method program         : program -> 'self * program * Types.datatype option
  method regex           : regex -> 'self * regex
  method section         : Section.t -> 'self * Section.t * Types.datatype
  method sentence        : sentence -> 'self * sentence
(*
  method sentence'       : sentence' -> 'self * sentence'
  method directive       : directive -> 'self * directive
*)
  method unary_op        : UnaryOp.t -> 'self * UnaryOp.t * Types.datatype
end

val fun_effects : Types.datatype -> Sugartypes.Pattern.with_pos list list -> Types.row
