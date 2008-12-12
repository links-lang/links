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
class transform : (Types.environment * Types.tycon_environment) ->
object ('self)
  val var_env : Types.environment
  val tycon_env : Types.tycon_environment

  method get_var_env     : unit -> Types.environment
  method get_tycon_env   : unit -> Types.tycon_environment
  method get_formlet_env : unit -> Types.environment

  method backup_envs     : Types.environment * Types.tycon_environment * Types.environment
  method restore_envs    : (Types.environment * Types.tycon_environment * Types.environment) -> 'self
    
  method lookup_type     : name -> Types.datatype
  method lookup_mb       : unit -> Types.datatype
  method with_mb         : Types.datatype -> 'self

  method binder          : binder -> 'self * binder
  method binding         : binding -> 'self * binding
  method bindingnode     : bindingnode -> 'self * bindingnode
  method binop           : binop -> 'self * binop * Types.datatype
  method constant        : constant -> 'self * constant * Types.datatype
  method funlit          : Types.datatype -> funlit -> 'self * funlit * Types.datatype
  method iterpatt        : iterpatt -> 'self * iterpatt

  method patternnode     : patternnode -> 'self * patternnode
  method pattern         : pattern -> 'self * pattern
  method phrase          : phrase -> 'self * phrase * Types.datatype
  method phrasenode      : phrasenode -> 'self * phrasenode * Types.datatype
  method program         : program -> 'self * program * Types.datatype option
  method regex           : regex -> 'self * regex
  method sec             : sec -> 'self * sec * Types.datatype
  method sentence        : sentence -> 'self * sentence
(*
  method sentence'       : sentence' -> 'self * sentence'
  method directive       : directive -> 'self * directive
*)
  method unary_op        : unary_op -> 'self * unary_op * Types.datatype
end

val fun_mailbox : Types.datatype -> Sugartypes.pattern list list -> Types.datatype
