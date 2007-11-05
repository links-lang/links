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
  object ('SELF)
    method float : float -> 'SELF
    method int : int -> 'SELF
    method num : num -> 'SELF
    method bool : bool -> 'SELF
    method char : char -> 'SELF
    method ref : ('SELF -> 'a -> 'SELF) -> 'a ref -> 'SELF
    method array : ('SELF -> 'a -> 'SELF) -> 'a array -> 'SELF
    method list : ('SELF -> 'a -> 'SELF) -> 'a list -> 'SELF
    method option : ('SELF -> 'a -> 'SELF) -> 'a option -> 'SELF
    method string : name -> 'SELF

    method _Lexing_position : Lexing.position -> 'SELF
    method _Num_num : num -> 'SELF
    method _Regex_repeat : Regex.repeat -> 'SELF
    method _Syntax_location : location -> 'SELF
    method _Syntax_untyped_definition : Syntax.untyped_definition -> 'SELF
    method _Syntax_untyped_expression : Syntax.untyped_expression -> 'SELF
    method _Types_primitive : Types.primitive -> 'SELF

    method assumption : assumption -> 'SELF
    method binding : binding -> 'SELF
    method binding' : ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a, 'b) binding' -> 'SELF
    method binop : binop -> 'SELF
    method constant : constant -> 'SELF
    method datatype : datatype -> 'SELF
    method directive : directive -> 'SELF
    method fieldconstraint : fieldconstraint -> 'SELF
    method fieldspec : fieldspec -> 'SELF
    method funlit : funlit -> 'SELF
    method funlit' : ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a, 'b) funlit' -> 'SELF
    method iterpatt : ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a, 'b) iterpatt -> 'SELF
    method location : location -> 'SELF
    method logical_binop : logical_binop -> 'SELF
    method name : name -> 'SELF
    method operator : operator -> 'SELF
    method pattern : pattern -> 'SELF
    method pattern' : ('SELF -> 'a -> 'SELF) -> 'a pattern' -> 'SELF
    method phrase : phrase -> 'SELF
    method phrasenode : phrasenode -> 'SELF
    method phrasenode' : ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('SELF -> 'c -> 'SELF) -> ('a, 'b, 'c) phrasenode' -> 'SELF
    method ppattern : ppattern -> 'SELF
    method pposition : pposition -> 'SELF
    method quantifier : quantifier -> 'SELF
    method regex : regex -> 'SELF
    method regex' : ('SELF -> 'a -> 'SELF) -> 'a regex' -> 'SELF
    method regexflag : regexflag -> 'SELF
    method replace_rhs : ('SELF -> 'a -> 'SELF) -> 'a replace_rhs -> 'SELF
    method row : row -> 'SELF
    method row_var : row_var -> 'SELF
    method sec : sec -> 'SELF
    method sentence : sentence -> 'SELF
    method sentence' : sentence' -> 'SELF
    method sentence'' : ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a, 'b) sentence'' -> 'SELF
    method unary_op : unary_op -> 'SELF
  end
