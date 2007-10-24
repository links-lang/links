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
  object
    method float : float -> float
    method int : int -> int
    method num : num -> num
    method bool : bool -> bool
    method char : char -> char
    method ref : ('a -> 'b) -> 'a ref -> 'b ref
    method array : ('a -> 'b) -> 'a array -> 'b array
    method list : ('a -> 'b) -> 'a list -> 'b list
    method option : ('a -> 'b) -> 'a option -> 'b option
    method string : string -> string

    method _Lexing_position : Lexing.position -> Lexing.position
    method _Num_num : num -> num
    method _Regex_repeat : Regex.repeat -> Regex.repeat
    method _Syntax_location : location -> location
    method _Syntax_untyped_definition : Syntax.untyped_definition -> Syntax.untyped_definition
    method _Syntax_untyped_expression : Syntax.untyped_expression -> Syntax.untyped_expression
    method _Types_primitive : Types.primitive -> Types.primitive

    method assumption : assumption -> assumption
    method binding : binding -> binding
    method binding' : ('c -> 'd) -> ('e -> 'f) -> ('c, 'e) binding' -> ('d, 'f) binding'
    method binop : binop -> binop
    method constant : constant -> constant
    method datatype : datatype -> datatype
    method directive : directive -> directive
    method fieldconstraint : fieldconstraint -> fieldconstraint
    method fieldspec : fieldspec -> fieldspec
    method funlit : funlit -> funlit
    method funlit' : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) funlit' -> ('c, 'd) funlit'
    method iterpatt : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) iterpatt -> ('c, 'd) iterpatt
    method location : location -> location
    method logical_binop : logical_binop -> logical_binop
    method name : name -> name
    method operator : operator -> operator
    method pattern : pattern -> pattern
    method pattern' : ('a -> 'b) -> 'a pattern' -> 'b pattern'
    method phrase : phrase -> phrase
    method phrasenode : phrasenode -> phrasenode
    method phrasenode' : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) -> ('a, 'b, 'c) phrasenode' -> ('d, 'e, 'f) phrasenode'
    method ppattern : ppattern -> ppattern
    method pposition : pposition -> pposition
    method quantifier : quantifier -> quantifier
    method regex : regex -> regex
    method regex' : ('a -> 'b) -> 'a regex' -> 'b regex'
    method regexflag : regexflag -> regexflag
    method replace_rhs : ('a -> 'b) -> 'a replace_rhs -> 'b replace_rhs
    method row : row -> row
    method row_var : row_var -> row_var
    method sec : sec -> sec
    method sentence : sentence -> sentence
    method sentence' : sentence' -> sentence'
    method sentence'' : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) sentence'' -> ('c, 'd) sentence''
    method unary_op : unary_op -> unary_op
  end
