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
    method float           : float -> float
    method char            : char -> char
    method bool            : bool -> bool
    method unary_op        : unary_op -> unary_op
    method typed_name      : typed_name -> typed_name
    method sentence'       : sentence' -> sentence'
    method sentence        : sentence -> sentence
    method sec             : sec -> sec
    method row_var         : row_var -> row_var
    method row             : row -> row
    method replace_rhs     : replace_rhs -> replace_rhs
    method regexflag       : regexflag -> regexflag
    method regex           : regex -> regex
    method quantifier      : quantifier -> quantifier
    method position        : position -> position
    method phrasenode      : phrasenode -> phrasenode
    method phrase          : phrase -> phrase
    method patternnode     : patternnode -> patternnode
    method pattern         : pattern -> pattern
    method operator        : operator -> operator
    method num             : num -> num
    method name            : name -> name
    method logical_binop   : logical_binop -> logical_binop
    method location        : location -> location
    method iterpatt        : iterpatt -> iterpatt
    method funlit          : funlit -> funlit
    method fieldspec       : fieldspec -> fieldspec
    method fieldconstraint : fieldconstraint -> fieldconstraint
    method directive       : directive -> directive
    method datatype        : datatype -> datatype
    method constant        : constant -> constant
    method binop           : binop -> binop
    method bindingnode     : bindingnode -> bindingnode
    method binding         : binding -> binding
    method assumption      : assumption -> assumption
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
    method float           : float -> 'self
    method char            : char -> 'self
    method bool            : bool -> 'self
    method unary_op        : unary_op -> 'self
    method typed_name      : typed_name -> 'self
    method sentence'       : sentence' -> 'self
    method sentence        : sentence -> 'self
    method sec             : sec -> 'self
    method row_var         : row_var -> 'self
    method row             : row -> 'self
    method replace_rhs     : replace_rhs -> 'self
    method regexflag       : regexflag -> 'self
    method regex           : regex -> 'self
    method quantifier      : quantifier -> 'self
    method position        : position -> 'self
    method phrasenode      : phrasenode -> 'self
    method phrase          : phrase -> 'self
    method patternnode     : patternnode -> 'self
    method pattern         : pattern -> 'self
    method operator        : operator -> 'self
    method num             : num -> 'self
    method name            : name -> 'self
    method logical_binop   : logical_binop -> 'self
    method location        : location -> 'self
    method iterpatt        : iterpatt -> 'self
    method funlit          : funlit -> 'self
    method fieldspec       : fieldspec -> 'self
    method fieldconstraint : fieldconstraint -> 'self
    method directive       : directive -> 'self
    method datatype        : datatype -> 'self
    method constant        : constant -> 'self
    method binop           : binop -> 'self
    method bindingnode     : bindingnode -> 'self
    method binding         : binding -> 'self
    method assumption      : assumption -> 'self
    method unknown         : 'a. 'a -> 'self
  end
  
