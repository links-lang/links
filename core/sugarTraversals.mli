open Operators
open CommonTypes
open SourceCode
open Sugartypes

(* Make a copy of a value.  You can override any method(s) to get a
   different operation at that type.  For example, the function

      object
        inherit map
        method logical_binop = function
         | `And -> `Or
         | p -> super#logical_binop p
      end # ppattern : ppattern -> ppattern

   will find every occurrence of `And within a ppattern value and
   change it to `Or.
*)
class map :
  object ('self)
    method string          : string -> string
    method option          : 'a 'a_out. ('self -> 'a -> 'a_out) -> 'a option -> 'a_out option
    method list            : 'a 'a_out. ('self -> 'a -> 'a_out) -> 'a list -> 'a_out list
    method int             : int -> int
    method float           : float -> float
    method char            : char -> char
    method bool            : bool -> bool
    method unary_op        : UnaryOp.t -> UnaryOp.t
    method tyunary_op      : tyarg list * UnaryOp.t -> tyarg list * UnaryOp.t
    method binder          : Binder.with_pos -> Binder.with_pos
    method sentence        : sentence -> sentence
    method section         : Section.t -> Section.t
    method subkind         : subkind -> subkind
    method kind            : kind -> kind
    method freedom         : freedom -> freedom
    method type_variable   : type_variable -> type_variable
    method known_type_variable   : known_type_variable -> known_type_variable
    method row_var         : Datatype.row_var -> Datatype.row_var
    method row             : Datatype.row -> Datatype.row
    method replace_rhs     : replace_rhs -> replace_rhs
    method regexflag       : regexflag -> regexflag
    method regex           : regex -> regex
    method position        : Position.t -> Position.t
    method given_spawn_location : given_spawn_location -> given_spawn_location
    method phrasenode      : phrasenode -> phrasenode
    method phrase          : phrase -> phrase
    method cp_phrasenode   : cp_phrasenode -> cp_phrasenode
    method cp_phrase       : cp_phrase -> cp_phrase
    method patternnode     : Pattern.t -> Pattern.t
    method pattern         : Pattern.with_pos -> Pattern.with_pos
    method name            : name -> name
    method location        : Location.t -> Location.t
    method iterpatt        : iterpatt -> iterpatt
    method funlit          : funlit -> funlit
    method handle_params   : handler_parameterisation -> handler_parameterisation
    method fieldspec       : Datatype.fieldspec -> Datatype.fieldspec
    method fieldconstraint : fieldconstraint -> fieldconstraint
    method directive       : directive -> directive
    method datatype        : Datatype.with_pos -> Datatype.with_pos
    method datatypenode    : Datatype.t -> Datatype.t
    method datatype'       : datatype' -> datatype'
    method type_arg        : Datatype.type_arg -> Datatype.type_arg
    method type_arg'       : type_arg' -> type_arg'
    method constant        : Constant.t -> Constant.t
    method binop           : BinaryOp.t -> BinaryOp.t
    method tybinop         : tyarg list * BinaryOp.t -> tyarg list * BinaryOp.t
    method bindingnode     : bindingnode -> bindingnode
    method binding         : binding -> binding
    method function_definition : function_definition -> function_definition
    method recursive_function  : recursive_function -> recursive_function
    method program         : program -> program
    method typ             : Types.datatype -> Types.datatype
    method type_row        : Types.row -> Types.row
    method tyarg           : tyarg -> tyarg
    method tyvar           : tyvar -> tyvar
    method type_field_spec : Types.field_spec -> Types.field_spec
    method unknown         : 'a. 'a -> 'a
  end

(* Reduce a value.  See

   http://brion.inria.fr/gallium/index.php/Camlp4FoldGenerator

   for the details.

   This example counts all the names in a datatype.

   object
     inherit fold
     val count = 0
     method count = count
     method name = {< count = self#count + 1 >}
   end
*)
class fold :
  object ('self)
    method string          : string -> 'self
    method option          : 'a. ('self -> 'a -> 'self) -> 'a option -> 'self
    method list            : 'a. ('self -> 'a -> 'self) -> 'a list -> 'self
    method int             : int -> 'self
    method float           : float -> 'self
    method char            : char -> 'self
    method bool            : bool -> 'self
    method unary_op        : UnaryOp.t -> 'self
    method tyunary_op      : tyarg list * UnaryOp.t -> 'self
    method binder          : Binder.with_pos -> 'self
    method sentence        : sentence -> 'self
    method section         : Section.t -> 'self
    method subkind         : subkind -> 'self
    method kind            : kind -> 'self
    method freedom         : freedom -> 'self
    method type_variable   : type_variable -> 'self
    method known_type_variable : known_type_variable -> 'self
    method row_var         : Datatype.row_var -> 'self
    method row             : Datatype.row -> 'self
    method replace_rhs     : replace_rhs -> 'self
    method regexflag       : regexflag -> 'self
    method regex           : regex -> 'self
    method position        : Position.t -> 'self
    method given_spawn_location : given_spawn_location -> 'self
    method phrasenode      : phrasenode -> 'self
    method phrase          : phrase -> 'self
    method cp_phrasenode   : cp_phrasenode -> 'self
    method cp_phrase       : cp_phrase -> 'self
    method patternnode     : Pattern.t -> 'self
    method pattern         : Pattern.with_pos -> 'self
    method name            : name -> 'self
    method location        : Location.t -> 'self
    method iterpatt        : iterpatt -> 'self
    method funlit          : funlit -> 'self
    method handle_params   : handler_parameterisation -> 'self
    (* method quantifier      : quantifier -> 'self *)
    method fieldspec       : Datatype.fieldspec -> 'self
    method fieldconstraint : fieldconstraint -> 'self
    method directive       : directive -> 'self
    method tyvar           : tyvar -> 'self
    method datatype        : Datatype.with_pos -> 'self
    method datatypenode    : Datatype.t -> 'self
    method datatype'       : datatype' -> 'self
    method type_arg        : Datatype.type_arg -> 'self
    method type_arg'       : type_arg' -> 'self
    method constant        : Constant.t -> 'self
    method binop           : BinaryOp.t -> 'self
    method tybinop         : tyarg list * BinaryOp.t -> 'self
    method bindingnode     : bindingnode -> 'self
    method binding         : binding -> 'self
    method function_definition : function_definition -> 'self
    method recursive_function  : recursive_function -> 'self
    method program         : program -> 'self
    method unknown         : 'a. 'a -> 'self
  end


(*
  The special casse of a predicate class
*)
class virtual predicate :
object
  inherit fold
  method virtual satisfied : bool
end

(* A combination of fold and map *)
class fold_map :
object ('self)
  method binder          : Binder.with_pos -> 'self * Binder.with_pos
  method binding         : binding -> 'self * binding
  method bindingnode     : bindingnode -> 'self * bindingnode
  method binop           : BinaryOp.t -> 'self * BinaryOp.t
  method tybinop         : tyarg list * BinaryOp.t -> 'self * (tyarg list * BinaryOp.t)
  method bool            : bool -> 'self * bool
  method char            : char -> 'self * char
  method constant        : Constant.t -> 'self * Constant.t
  method datatype        : Datatype.with_pos -> 'self * Datatype.with_pos
  method datatypenode    : Datatype.t -> 'self * Datatype.t
  method datatype'       : datatype' -> 'self * datatype'
  method type_arg'       : type_arg' -> 'self * type_arg'
  method directive       : directive -> 'self * directive
  method fieldconstraint : fieldconstraint -> 'self * fieldconstraint
  method fieldspec       : Datatype.fieldspec -> 'self * Datatype.fieldspec
  method int             : int -> 'self * int
  method float           : float -> 'self * float
  method funlit          : funlit -> 'self * funlit
  method handle_params   : handler_parameterisation -> 'self * handler_parameterisation
  method iterpatt        : iterpatt -> 'self * iterpatt
  method list            : 'a . ('self -> 'a -> 'self * 'a) -> 'a list -> 'self * 'a list
  method location        : Location.t -> 'self * Location.t
  method name            : name -> 'self * name
  method option          : 'a . ('self -> 'a -> 'self * 'a) -> 'a option -> 'self * 'a option
  method patternnode     : Pattern.t -> 'self * Pattern.t
  method pattern         : Pattern.with_pos -> 'self * Pattern.with_pos
  method phrase          : phrase -> 'self * phrase
  method given_spawn_location : given_spawn_location -> 'self * given_spawn_location
  method phrasenode      : phrasenode -> 'self * phrasenode
  method cp_phrasenode   : cp_phrasenode -> 'self * cp_phrasenode
  method cp_phrase       : cp_phrase -> 'self * cp_phrase
  method position        : Position.t -> 'self * Position.t
  method program         : program -> 'self * program
  (* method quantifier      : quantifier -> 'self * quantifier *)
  method regex           : regex -> 'self * regex
  method regexflag       : regexflag -> 'self * regexflag
  method replace_rhs     : replace_rhs -> 'self * replace_rhs
  method row             : Datatype.row -> 'self * Datatype.row
  method row_var         : Datatype.row_var -> 'self * Datatype.row_var
  method section         : Section.t -> 'self * Section.t
  method sentence        : sentence -> 'self * sentence
  method string          : name -> 'self * name
  method subkind         : subkind -> 'self * subkind
  method kind            : kind -> 'self * kind
  method freedom         : freedom -> 'self * freedom
  method type_variable   : type_variable -> 'self * type_variable
  method known_type_variable : known_type_variable -> 'self * known_type_variable
  method type_arg        : Datatype.type_arg -> 'self * Datatype.type_arg
  method tyunary_op      : tyarg list * UnaryOp.t -> 'self * (tyarg list * UnaryOp.t)
  method unary_op        : UnaryOp.t -> 'self * UnaryOp.t
  method function_definition : function_definition -> 'self * function_definition
  method recursive_function  : recursive_function -> 'self * recursive_function
  method unknown         : 'a . 'a -> 'self * 'a
end
