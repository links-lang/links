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
    method linearity       : DeclaredLinearity.t -> DeclaredLinearity.t
    method timestamp       : Timestamp.t -> Timestamp.t
    method unary_op        : UnaryOp.t -> UnaryOp.t
    method tyunary_op      : tyarg list * UnaryOp.t -> tyarg list * UnaryOp.t
    method binder          : Binder.with_pos -> Binder.with_pos
    method sentence        : sentence -> sentence
    method section         : Section.t -> Section.t
    method subkind         : Subkind.t -> Subkind.t
    method kind            : kind -> kind
    method freedom         : Freedom.t -> Freedom.t
    method quantifier      :  SugarQuantifier.t -> SugarQuantifier.t
    method type_variable   : SugarTypeVar.t -> SugarTypeVar.t
    method row_var         : Datatype.row_var -> Datatype.row_var
    method row             : Datatype.row -> Datatype.row
    method replace_rhs     : replace_rhs -> replace_rhs
    method regexflag       : regexflag -> regexflag
    method regex           : regex -> regex
    method position        : Position.t -> Position.t
    method temporal_update : temporal_update -> temporal_update
    method temporal_deletion : temporal_deletion -> temporal_deletion
    method given_spawn_location : given_spawn_location -> given_spawn_location
    method phrasenode      : phrasenode -> phrasenode
    method phrase          : phrase -> phrase
    method cp_phrasenode   : cp_phrasenode -> cp_phrasenode
    method cp_phrase       : cp_phrase -> cp_phrase
    method patternnode     : Pattern.t -> Pattern.t
    method pattern         : Pattern.with_pos -> Pattern.with_pos
    method foreign_language : ForeignLanguage.t -> ForeignLanguage.t
    method name            : Name.t -> Name.t
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
    method row'            : row' -> row'
    method type_arg        : Datatype.type_arg -> Datatype.type_arg
    method type_arg'       : type_arg' -> type_arg'
    method constant        : Constant.t -> Constant.t
    method binop           : BinaryOp.t -> BinaryOp.t
    method tybinop         : tyarg list * BinaryOp.t -> tyarg list * BinaryOp.t
    method bindingnode     : bindingnode -> bindingnode
    method binding         : binding -> binding
    method aliasnode       : aliasnode -> aliasnode
    method alias           : alias -> alias
    method aliasbody       : aliasbody -> aliasbody
    method function_definition : function_definition -> function_definition
    method recursive_function  : recursive_function -> recursive_function
    method recursive_functionnode : recursive_functionnode -> recursive_functionnode
    method program         : program -> program
    method typ             : Types.datatype -> Types.datatype
    method type_row        : Types.row -> Types.row
    method tyarg           : tyarg -> tyarg
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
    method timestamp       : Timestamp.t -> 'self
    method bool            : bool -> 'self
    method linearity       : DeclaredLinearity.t -> 'self
    method unary_op        : UnaryOp.t -> 'self
    method tyunary_op      : tyarg list * UnaryOp.t -> 'self
    method binder          : Binder.with_pos -> 'self
    method sentence        : sentence -> 'self
    method section         : Section.t -> 'self
    method subkind         : Subkind.t -> 'self
    method kind            : kind -> 'self
    method freedom         : Freedom.t -> 'self
    method type_variable   : SugarTypeVar.t -> 'self
    method quantifier      : SugarQuantifier.t -> 'self
    method row_var         : Datatype.row_var -> 'self
    method row             : Datatype.row -> 'self
    method replace_rhs     : replace_rhs -> 'self
    method regexflag       : regexflag -> 'self
    method regex           : regex -> 'self
    method position        : Position.t -> 'self
    method given_spawn_location : given_spawn_location -> 'self
    method temporal_update : temporal_update -> 'self
    method temporal_deletion : temporal_deletion -> 'self
    method phrasenode      : phrasenode -> 'self
    method phrase          : phrase -> 'self
    method cp_phrasenode   : cp_phrasenode -> 'self
    method cp_phrase       : cp_phrase -> 'self
    method patternnode     : Pattern.t -> 'self
    method pattern         : Pattern.with_pos -> 'self
    method foreign_language : ForeignLanguage.t -> 'self
    method name            : Name.t -> 'self
    method location        : Location.t -> 'self
    method iterpatt        : iterpatt -> 'self
    method funlit          : funlit -> 'self
    method handle_params   : handler_parameterisation -> 'self
    method fieldspec       : Datatype.fieldspec -> 'self
    method fieldconstraint : fieldconstraint -> 'self
    method directive       : directive -> 'self
    method datatype        : Datatype.with_pos -> 'self
    method datatypenode    : Datatype.t -> 'self
    method datatype'       : datatype' -> 'self
    method row'            : row' -> 'self
    method type_arg        : Datatype.type_arg -> 'self
    method type_arg'       : type_arg' -> 'self
    method constant        : Constant.t -> 'self
    method binop           : BinaryOp.t -> 'self
    method tybinop         : tyarg list * BinaryOp.t -> 'self
    method bindingnode     : bindingnode -> 'self
    method binding         : binding -> 'self
    method aliasnode       : aliasnode -> 'self
    method alias           : alias -> 'self
    method aliasbody       : aliasbody -> 'self
    method function_definition : function_definition -> 'self
    method recursive_function  : recursive_function -> 'self
    method recursive_functionnode  : recursive_functionnode -> 'self
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
  method aliasnode       : aliasnode -> 'self * aliasnode
  method alias           : alias -> 'self * alias
  method aliasbody       : aliasbody -> 'self * aliasbody
  method binop           : BinaryOp.t -> 'self * BinaryOp.t
  method tybinop         : tyarg list * BinaryOp.t -> 'self * (tyarg list * BinaryOp.t)
  method bool            : bool -> 'self * bool
  method linearity       : DeclaredLinearity.t -> 'self * DeclaredLinearity.t
  method char            : char -> 'self * char
  method timestamp       : Timestamp.t -> 'self * Timestamp.t
  method constant        : Constant.t -> 'self * Constant.t
  method datatype        : Datatype.with_pos -> 'self * Datatype.with_pos
  method datatypenode    : Datatype.t -> 'self * Datatype.t
  method datatype'       : datatype' -> 'self * datatype'
  method row'            : row' -> 'self * row'
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
  method foreign_language : ForeignLanguage.t -> 'self * ForeignLanguage.t
  method name            : Name.t -> 'self * Name.t
  method option          : 'a . ('self -> 'a -> 'self * 'a) -> 'a option -> 'self * 'a option
  method patternnode     : Pattern.t -> 'self * Pattern.t
  method pattern         : Pattern.with_pos -> 'self * Pattern.with_pos
  method phrase          : phrase -> 'self * phrase
  method temporal_update : temporal_update -> ('self * temporal_update)
  method temporal_deletion : temporal_deletion -> ('self * temporal_deletion)
  method given_spawn_location : given_spawn_location -> 'self * given_spawn_location
  method phrasenode      : phrasenode -> 'self * phrasenode
  method cp_phrasenode   : cp_phrasenode -> 'self * cp_phrasenode
  method cp_phrase       : cp_phrase -> 'self * cp_phrase
  method position        : Position.t -> 'self * Position.t
  method program         : program -> 'self * program
  method regex           : regex -> 'self * regex
  method regexflag       : regexflag -> 'self * regexflag
  method replace_rhs     : replace_rhs -> 'self * replace_rhs
  method row             : Datatype.row -> 'self * Datatype.row
  method row_var         : Datatype.row_var -> 'self * Datatype.row_var
  method section         : Section.t -> 'self * Section.t
  method sentence        : sentence -> 'self * sentence
  method string          : Name.t -> 'self * Name.t
  method subkind         : Subkind.t -> 'self * Subkind.t
  method kind            : kind -> 'self * kind
  method freedom         : Freedom.t -> 'self * Freedom.t
  method quantifier      : SugarQuantifier.t -> 'self * SugarQuantifier.t
  method type_variable   : SugarTypeVar.t -> 'self * SugarTypeVar.t
  method typ             : Types.datatype -> ('self * Types.datatype)
  method type_row        : Types.row -> ('self* Types.row)
  method type_arg        : Datatype.type_arg -> 'self * Datatype.type_arg
  method type_field_spec : Types.field_spec -> ('self * Types.field_spec)
  method tyarg           : Types.type_arg -> ('self * Types.type_arg)
  method tyunary_op      : tyarg list * UnaryOp.t -> 'self * (tyarg list * UnaryOp.t)
  method unary_op        : UnaryOp.t -> 'self * UnaryOp.t
  method function_definition : function_definition -> 'self * function_definition
  method recursive_function  : recursive_function -> 'self * recursive_function
  method recursive_functionnode  : recursive_functionnode -> 'self * recursive_functionnode
  method unknown         : 'a . 'a -> 'self * 'a
end
