open Sugartypes

(* Transform a term and construct its type *)
class transform : (Types.environment * Types.tycon_environment) ->
object ('self)
  val var_env : Types.environment
  val tycon_env : Types.tycon_environment
    
  method lookup_type     : name -> Types.datatype
  method lookup_mb       : unit -> Types.datatype
  method with_mb         : Types.datatype -> 'self

  method binder          : binder -> 'self * binder
  method binding         : binding -> 'self * binding
  method bindingnode     : bindingnode -> 'self * bindingnode
  method binop           : binop -> 'self * binop * Types.datatype
  method constant        : constant -> 'self * constant * Types.datatype
  method funlit          : funlit -> 'self * funlit
  method iterpatt        : iterpatt -> 'self * iterpatt

  method list            : 'a . ('self -> 'a -> 'self * 'a * Types.datatype) ->
                                  'a list -> 'self * 'a list * Types.datatype list
  method listu           : 'a . ('self -> 'a -> 'self * 'a) -> 'a list -> 'self * 'a list
  method option          : 'a . ('self -> 'a -> 'self * 'a * Types.datatype) ->
                                  'a option -> 'self * 'a option * Types.datatype option
  method optionu         : 'a . ('self -> 'a -> 'self * 'a) -> 'a option -> 'self * 'a option

  method patternnode     : patternnode -> 'self * patternnode
  method pattern         : pattern -> 'self * pattern
  method phrase          : phrase -> 'self * phrase * Types.datatype
  method phrasenode      : phrasenode -> 'self * phrasenode * Types.datatype
  method program         : program -> 'self * program * Types.datatype option
  method regex           : regex -> 'self * regex
  method sec             : sec -> 'self * sec * Types.datatype
(*
  method sentence        : sentence -> 'self * sentence
  method sentence'       : sentence' -> 'self * sentence'
  method directive       : directive -> 'self * directive
*)
  method unary_op        : unary_op -> 'self * unary_op * Types.datatype
end

