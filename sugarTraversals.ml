(* Generated mostly automatically using Camlp4 in OCaml 3.10.0.

   NB: DO NOT EDIT (except to keep it in line with sugartypes.ml).

   When we switch to OCaml 3.10.x, and when the Camlp4MapGenerator
   filter which comes with camlp4 3.10.0 works sufficiently well we'll
   generate all this automatically instead of maintaining this file.
*)

open Sugartypes

class map =
object (o)
  method string = fun x -> (x : string)
  method int = fun x -> (x : int)
  method float = fun x -> (x : float)
  method char = fun x -> (x : char)
  method bool = fun x -> (x : bool)
  method list : 'a 'b. ('a -> 'b) -> 'a list -> 'b list = List.map
  method option : 'a 'b. ('a -> 'b) -> 'a option -> 'b option =
    fun f -> function | None -> None | Some x -> Some (f x)
  method array : 'a 'b. ('a -> 'b) -> 'a array -> 'b array = Array.map
  method ref : 'a 'b. ('a -> 'b) -> 'a ref -> 'b ref =
    fun f { contents = x } -> { contents = f x; }

  method _Types_primitive : Types.primitive -> Types.primitive = fun x -> x
  method _Syntax_untyped_expression :
    Syntax.untyped_expression -> Syntax.untyped_expression = fun x -> x
  method _Syntax_untyped_definition :
    Syntax.untyped_definition -> Syntax.untyped_definition = fun x -> x
  method _Syntax_location : Syntax.location -> Syntax.location = fun x -> x
  method _Regex_repeat : Regex.repeat -> Regex.repeat = fun x -> x
  method _Num_num : Num.num -> Num.num = fun x -> x
  method _Lexing_position : Lexing.position -> Lexing.position = fun x -> x

  (*
    type ('ppattern, 'phrase, 'binding) phrasenode' = [
    | `Constant of constant
    | `Var of (name)
    | `FunLit of ('ppattern, 'phrase) funlit'
    | `Spawn of 'phrase
    | `SpawnWait of 'phrase
    | `ListLit of ('phrase list)
    | `Iteration of ((('ppattern, 'phrase) iterpatt) list * 'phrase * (*where:*)'phrase option 
                     * (*orderby:*)'phrase option)
    | `Escape of (name * 'phrase)
    | `Section of (sec)
    | `Conditional of ('phrase * 'phrase * 'phrase)
    | `Block of 'binding list * 'phrase
    | `InfixAppl of (binop * 'phrase * 'phrase)
    | `Regex of ('phrase regex')
    | `UnaryAppl of (unary_op * 'phrase)
    | `FnAppl of ('phrase * 'phrase list)
    | `TupleLit of ('phrase list)
    | `RecordLit of ((name * 'phrase) list * 'phrase option)
    | `Projection of ('phrase * name)
    | `With of ('phrase * (name * 'phrase) list)
    | `TypeAnnotation of ('phrase * datatype)
    | `Upcast of ('phrase * datatype * datatype)
    | `ConstructorLit of (name * 'phrase option)
    | `Switch of ('phrase * ('ppattern * 'phrase) list)
    | `Receive of ('ppattern * 'phrase) list
    | `DatabaseLit of ('phrase * ('phrase option * 'phrase option))
    | `TableLit of ('phrase * datatype * (string * fieldconstraint list) list * 'phrase)
    | `DBDelete of ('ppattern * 'phrase * 'phrase option)
    | `DBInsert of ('phrase * 'phrase)
    | `DBUpdate of ('ppattern * 'phrase * 'phrase option * (name * 'phrase) list)
    | `Xml of (name * (string * ('phrase list)) list * 'phrase option * 'phrase list)
    | `TextNode of (string)
    | `Formlet of ('phrase * 'phrase)
    | `Page of 'phrase
    | `FormletPlacement of ('phrase * 'phrase)
    | `PagePlacement of ('phrase)
    | `FormBinding of ('phrase * 'ppattern) ]
    *)
    method phrasenode' : 'a 'b 'c 'd 'e 'f . ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) -> ('a, 'b, 'c) phrasenode' -> ('d, 'e, 'f) phrasenode' =
      fun atod btoe ctof -> (* partially handwritten: camlp4 can't cope with types with multiple parameters *)
      function
      | `Constant x -> `Constant (o#constant x)
      | `Var x -> `Var (o#name x)
      | `FunLit x ->
          `FunLit
            (o#funlit' atod btoe x)
      | `Spawn x -> `Spawn (btoe x)
      | `SpawnWait x -> `SpawnWait (btoe x)
      | `ListLit x -> `ListLit (o#list btoe x)
      | `Iteration x ->
          `Iteration
            ((fun (_x0, _x1, _x2, _x3) ->
                ((o#list (o#iterpatt atod btoe)
                    _x0),
                 (btoe _x1), (o#option btoe _x2),
                 (o#option btoe _x3)))
               x)
      | `Escape x ->
          `Escape
            ((fun (_x0, _x1) -> ((o#name _x0), (btoe _x1))) x)
      | `Section x -> `Section (o#sec x)
      | `Conditional x ->
          `Conditional
            ((fun (_x0, _x1, _x2) ->
                ((btoe _x0), (btoe _x1),
                 (btoe _x2)))
               x)
      | `Block x ->
          `Block
            ((fun (_x0, _x1) ->
                ((o#list ctof _x0), (btoe _x1)))
               x)
      | `InfixAppl x ->
          `InfixAppl
            ((fun (_x0, _x1, _x2) ->
                ((o#binop _x0), (btoe _x1),
                 (btoe _x2)))
               x)
      | `Regex x -> `Regex (o#regex' btoe x)
      | `UnaryAppl x ->
          `UnaryAppl
            ((fun (_x0, _x1) -> ((o#unary_op _x0), (btoe _x1)))
               x)
      | `FnAppl x ->
          `FnAppl
            ((fun (_x0, _x1) ->
                ((btoe _x0), (o#list btoe _x1)))
               x)
      | `TupleLit x -> `TupleLit (o#list btoe x)
      | `RecordLit x ->
          `RecordLit
            ((fun (_x0, _x1) ->
                ((o#list
                    (fun (_x0, _x1) ->
                       ((o#name _x0), (btoe _x1)))
                    _x0),
                 (o#option btoe _x1)))
               x)
      | `Projection x ->
          `Projection
            ((fun (_x0, _x1) -> ((btoe _x0), (o#name _x1))) x)
      | `With x ->
          `With
            ((fun (_x0, _x1) ->
                ((btoe _x0),
                 (o#list
                    (fun (_x0, _x1) ->
                       ((o#name _x0), (btoe _x1)))
                    _x1)))
               x)
      | `TypeAnnotation x ->
          `TypeAnnotation
            ((fun (_x0, _x1) -> ((btoe _x0), (o#datatype _x1)))
               x)
      | `Upcast x ->
          `Upcast
            ((fun (_x0, _x1, _x2) ->
                ((btoe _x0), (o#datatype _x1),
                 (o#datatype _x2)))
               x)
      | `ConstructorLit x ->
          `ConstructorLit
            ((fun (_x0, _x1) ->
                ((o#name _x0), (o#option btoe _x1)))
               x)
      | `Switch x ->
          `Switch
            ((fun (_x0, _x1) ->
                ((btoe _x0),
                 (o#list
                    (fun (_x0, _x1) ->
                       ((atod _x0), (btoe _x1)))
                    _x1)))
               x)
      | `Receive x ->
          `Receive
            (o#list
               (fun (_x0, _x1) ->
                  ((atod _x0), (btoe _x1)))
               x)
      | `DatabaseLit x ->
          `DatabaseLit
            ((fun (_x0, _x1) ->
                ((btoe _x0),
                 ((fun (_x0, _x1) ->
                     ((o#option btoe _x0),
                      (o#option btoe _x1)))
                    _x1)))
               x)
      | `TableLit x ->
          `TableLit
            ((fun (_x0, _x1, _x2, _x3) ->
                ((btoe _x0), (o#datatype _x1),
                 (o#list
                    (fun (_x0, _x1) ->
                       ((o#string _x0), (o#list o#fieldconstraint _x1)))
                    _x2),
                 (btoe _x3)))
               x)
      | `DBDelete x ->
          `DBDelete
            ((fun (_x0, _x1, _x2) ->
                ((atod _x0), (btoe _x1),
                 (o#option btoe _x2)))
               x)
      | `DBInsert x ->
          `DBInsert
            ((fun (_x0, _x1) ->
                ((btoe _x0), (btoe _x1)))
               x)
      | `DBUpdate x ->
          `DBUpdate
            ((fun (_x0, _x1, _x2, _x3) ->
                ((atod _x0), (btoe _x1),
                 (o#option btoe _x2),
                 (o#list
                    (fun (_x0, _x1) ->
                       ((o#name _x0), (btoe _x1)))
                    _x3)))
               x)
      | `Xml x ->
          `Xml
            ((fun (_x0, _x1, _x2, _x3) ->
                ((o#name _x0),
                 (o#list
                    (fun (_x0, _x1) ->
                       ((o#string _x0), (o#list btoe _x1)))
                    _x1),
                 (o#option btoe _x2),
                 (o#list btoe _x3)))
               x)
      | `TextNode x -> `TextNode (o#string x)
      | `Formlet x ->
          `Formlet
            ((fun (_x0, _x1) ->
                ((btoe _x0), (btoe _x1)))
               x)
      | `Page x -> `Page (btoe x)
      | `FormletPlacement x ->
          `FormletPlacement
            ((fun (_x0, _x1) ->
                ((btoe _x0), (btoe _x1)))
               x)
      | `PagePlacement x -> `PagePlacement (btoe x)
      | `FormBinding x ->
          `FormBinding
            ((fun (_x0, _x1) ->
                ((btoe _x0), (atod _x1)))
               x)



    (*
      type ('ppattern, 'phrase) binding' = [
      | `Val of 'ppattern * 'phrase * location * datatype option
      | `Fun of name * ('ppattern, 'phrase) funlit' * location * datatype option
      | `Funs of (name * ('ppattern, 'phrase) funlit' * location * datatype option) list
      | `Foreign of name * name * datatype
      | `Type of (name * name list * datatype)
      | `Infix
      | `Exp of 'phrase ]
    *)
    method binding' : 'a 'b 'c 'd . ('a -> 'c) -> ('b -> 'd) -> ('a,'b) binding' -> ('c,'d) binding' =
      fun atoc btod -> (* partially handwritten: camlp4 can't cope with types with multiple parameters *)
      function
      | `Val x ->
          `Val
            ((fun (_x0, _x1, _x2, _x3) ->
                ((atoc _x0), (btod _x1),
                 (o#location _x2), (o#option o#datatype _x3)))
               x)
      | `Fun x ->
          `Fun
            ((fun (_x0, _x1, _x2, _x3) ->
                ((o#name _x0),
                 (o#funlit' atoc btod _x1),
                 (o#location _x2), (o#option o#datatype _x3)))
               x)
      | `Funs x ->
          `Funs
            (o#list
               (fun (_x0, _x1, _x2, _x3) ->
                  ((o#name _x0),
                   (o#funlit' atoc btod _x1),
                   (o#location _x2), (o#option o#datatype _x3)))
               x)
      | `Foreign x ->
          `Foreign
            ((fun (_x0, _x1, _x2) ->
                ((o#name _x0), (o#name _x1), (o#datatype _x2)))
               x)
      | `Type x ->
          `Type
            ((fun (_x0, _x1, _x2) ->
                ((o#name _x0), (o#list o#name _x1), (o#datatype _x2)))
               x)
      | `Infix -> `Infix
      | `Exp x -> `Exp (btod x)

    (* type (*('phrase, 'binding)*) sentence'' = [ 
       | `Definitions of 'binding list
       | `Expression of 'phrase
       | `Directive of directive ]
    *)
    method sentence'' : 'a 'b 'c 'd . ('a -> 'c) -> ('b -> 'd) -> ('a,'b) sentence'' -> ('c,'d) sentence'' =
      fun atoc btod -> (* partially handwritten: camlp4 can't cope with types with multiple parameters *)
        function
          | `Definitions x -> `Definitions (o#list btod x)
          | `Expression x -> `Expression (atoc x)
          | `Directive x -> `Directive (o#directive x)

    (* type unary_op = [ `Minus | `FloatMinus | `Name of name | `Abs ] *)
    method unary_op : unary_op -> unary_op =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `Name x -> `Name (o#name x)
      | `Abs -> `Abs

    (* type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name | `App ] *)
    method binop : binop -> binop = (* handwritten: camlp4 can't cope with polymorphic variant extension *)
      function
        | `Minus -> `Minus
        | `FloatMinus -> `FloatMinus
        | `RegexMatch rflags -> `RegexMatch (o#list o#regexflag rflags)
        | #logical_binop as l -> (o#logical_binop l :> binop)
        | `Cons -> `Cons
        | `Name name -> `Name (o#name name)
        | `App -> `App

    (*type operator = [ unary_op | binop | `Project of name ]*)
    method operator : operator -> operator = (* handwritten: camlp4 can't cope with polymorphic variant extension *)
      function
        | #unary_op as u -> (o#unary_op u :> operator)
        | #binop as b -> (o#binop b :> operator)
        | `Project name -> `Project (o#name name)

    (* type ('ppattern, 'phrase) funlit' = 'ppattern list list * 'phrase *)
    method funlit' : 'a 'b 'c 'd . ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) funlit' -> ('c, 'd) funlit' 
      = fun atoc btod -> (* handwritten: camlp4 can't cope with types with multiple parameters *)
        fun (pll, p) -> (o#list (o#list atoc) pll, btod p)

    (* type ('ppattern, 'phrase) iterpatt = [ `List of 'ppattern * 'phrase | `Table of 'ppattern * 'phrase ] *)
    method iterpatt : 'a 'b 'c 'd . ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) iterpatt -> ('c, 'd) iterpatt =
      fun atoc btod -> (* handwritten: camlp4 can't cope with types with multiple parameters *)
        function
          | `List (pp, p) -> `List (atoc pp, btod p)
          | `Table (pp, p) -> `Table (atoc pp, btod p)

    (* type sentence' =
       [ `Definitions of Syntax.untyped_definition list
       | `Expression of Syntax.untyped_expression | `Directive of directive
       ]
    *)
    method sentence' : sentence' -> sentence' =
      function
      | `Definitions x ->
          `Definitions (o#list o#_Syntax_untyped_definition x)
      | `Expression x -> `Expression (o#_Syntax_untyped_expression x)
      | `Directive x -> `Directive (o#directive x)
          
    (* type sentence = (phrase, binding) sentence'' *)
    method sentence : sentence -> sentence = o#sentence'' o#phrase o#binding

    (* type sec = [ `Minus | `FloatMinus | `Project of name | `Name of name ] *)
    method sec : sec -> sec =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `Project x -> `Project (o#name x)
      | `Name x -> `Name (o#name x)

    (* and row_var =
       [ `Closed | `Open of string | `Recursive of (string * row) ] *)
    method row_var : row_var -> row_var =
      function
      | `Closed -> `Closed
      | `Open x -> `Open (o#string x)
      | `Recursive x ->
          `Recursive ((fun (_x0, _x1) -> ((o#string _x0), (o#row _x1))) x)

    (* and row = (((string * fieldspec) list) * row_var) *)
    method row : row -> row =
      fun (_x0, _x1) ->
        ((o#list (fun (_x0, _x1) -> ((o#string _x0), (o#fieldspec _x1))) _x0),
         (o#row_var _x1))

    (* type 'phrase replace_rhs = [ `Literal of string | `Splice of 'phrase ] *)
    method replace_rhs :
      'a 'b. ('a -> 'b) -> 'a replace_rhs -> 'b replace_rhs =
      fun _f_phrase ->
        function
        | `Literal x -> `Literal (o#string x)
        | `Splice x -> `Splice (_f_phrase x)

    (* and regexflag = [ `RegexList | `RegexNative | `RegexGlobal | `RegexReplace ] *)
    method regexflag : regexflag -> regexflag =
      function
      | `RegexList -> `RegexList
      | `RegexNative -> `RegexNative
      | `RegexGlobal -> `RegexGlobal
      | `RegexReplace -> `RegexReplace

    (*
      type 'phrase regex' =
      [ `Range of (char * char) | `Simply of string | `Quote of 'phrase regex'
      | `Any | `StartAnchor | `EndAnchor | `Seq of ('phrase regex') list
      | `Alternate of (('phrase regex') * ('phrase regex'))
      | `Group of 'phrase regex' | `Repeat of (Regex.repeat * ('phrase regex'))
      | `Splice of 'phrase
      | `Replace of (('phrase regex') * ('phrase replace_rhs))
      ]
    *)
    method regex' : 'a 'b. ('a -> 'b) -> 'a regex' -> 'b regex' =
      fun _f_phrase ->
        function
        | `Range x ->
            `Range ((fun (_x0, _x1) -> ((o#char _x0), (o#char _x1))) x)
        | `Simply x -> `Simply (o#string x)
        | `Quote x -> `Quote (o#regex' _f_phrase x)
        | `Any -> `Any
        | `StartAnchor -> `StartAnchor
        | `EndAnchor -> `EndAnchor
        | `Seq x -> `Seq (o#list (o#regex' _f_phrase) x)
        | `Alternate x ->
            `Alternate
              ((fun (_x0, _x1) ->
                  ((o#regex' _f_phrase _x0), (o#regex' _f_phrase _x1)))
                 x)
        | `Group x -> `Group (o#regex' _f_phrase x)
        | `Repeat x ->
            `Repeat
              ((fun (_x0, _x1) ->
                  ((o#_Regex_repeat _x0), (o#regex' _f_phrase _x1)))
                 x)
        | `Splice x -> `Splice (_f_phrase x)
        | `Replace x ->
            `Replace
              ((fun (_x0, _x1) ->
                  ((o#regex' _f_phrase _x0), (o#replace_rhs _f_phrase _x1)))
                 x)

    (*type regex = phrase regex' *)
    method regex : regex -> regex = o#regex' o#phrase

    (*type quantifier =
      [ `TypeVar of string | `RigidTypeVar of string | `RowVar of string ] *)
    method quantifier : quantifier -> quantifier =
      function
      | `TypeVar x -> `TypeVar (o#string x)
      | `RigidTypeVar x -> `RigidTypeVar (o#string x)
      | `RowVar x -> `RowVar (o#string x)

    (* type pposition = (Lexing.position * Lexing.position) *)
    method pposition : pposition -> pposition =
      fun (_x0, _x1) -> ((o#_Lexing_position _x0), (o#_Lexing_position _x1))

    (* and ppattern = ((ppattern pattern') * pposition) *)
    method ppattern : ppattern -> ppattern =
      fun (_x0, _x1) -> ((o#pattern' o#ppattern _x0), (o#pposition _x1))

    (* type phrasenode = (ppattern, phrase, binding) phrasenode' *)
    method phrasenode : phrasenode -> phrasenode =
      o#phrasenode' o#ppattern o#phrase o#binding

    (* type phrase = (((ppattern, phrase, binding) phrasenode') * pposition) *)
    method phrase : phrase -> phrase =
      fun (_x0, _x1) ->
        ((o#phrasenode' o#ppattern o#phrase o#binding _x0),
         (o#pposition _x1))

    (*
      type 'ppattern pattern' =
      [ `Any | `Nil | `Cons of ('ppattern * 'ppattern) | `List of 'ppattern list
      | `Variant of (string * ('ppattern option))
      | `Record of (((string * 'ppattern) list) * ('ppattern option))
      | `Tuple of 'ppattern list | `Constant of constant | `Variable of string
      | `As of (string * 'ppattern) | `HasType of ('ppattern * datatype)
      ]
    *)
    method pattern' : 'a 'b. ('a -> 'b) -> 'a pattern' -> 'b pattern' =
      fun _f_ppattern ->
        function
        | `Any -> `Any
        | `Nil -> `Nil
        | `Cons x ->
            `Cons
              ((fun (_x0, _x1) -> ((_f_ppattern _x0), (_f_ppattern _x1))) x)
        | `List x -> `List (o#list _f_ppattern x)
        | `Variant x ->
            `Variant
              ((fun (_x0, _x1) ->
                  ((o#string _x0), (o#option _f_ppattern _x1)))
                 x)
        | `Record x ->
            `Record
              ((fun (_x0, _x1) ->
                  ((o#list
                      (fun (_x0, _x1) -> ((o#string _x0), (_f_ppattern _x1)))
                      _x0),
                   (o#option _f_ppattern _x1)))
                 x)
        | `Tuple x -> `Tuple (o#list _f_ppattern x)
        | `Constant x -> `Constant (o#constant x)
        | `Variable x -> `Variable (o#string x)
        | `As x ->
            `As ((fun (_x0, _x1) -> ((o#string _x0), (_f_ppattern _x1))) x)
        | `HasType x ->
            `HasType
              ((fun (_x0, _x1) -> ((_f_ppattern _x0), (o#datatype _x1))) x)

    (* type pattern = ppattern pattern' *)
    method pattern : pattern -> pattern = o#pattern' o#ppattern

    (* type num = Num.num *)
    method num : num -> num = o#_Num_num

    (* type name = string *)
    method name : name -> name = o#string

    (* type logical_binop = [ `And | `Or ] *)
    method logical_binop : logical_binop -> logical_binop =
      function | `And -> `And | `Or -> `Or

    (* type location = Syntax.location *)
    method location : location -> location = o#_Syntax_location

    (* type funlit = (ppattern, phrase) funlit' *)
    method funlit : funlit -> funlit = o#funlit' o#ppattern o#phrase

    (* and fieldspec = [ `Present of datatype | `Absent ] *)
    method fieldspec : fieldspec -> fieldspec =
      function | `Present x -> `Present (o#datatype x) | `Absent -> `Absent

    (* type fieldconstraint = [ `Readonly ] *)
    method fieldconstraint : fieldconstraint -> fieldconstraint =
      function | `Readonly -> `Readonly

    (* type directive = (string * (string list)) *)
    method directive : directive -> directive =
      fun (_x0, _x1) -> ((o#string _x0), (o#list o#string _x1))

    (* type datatype =
       | TypeVar of string | RigidTypeVar of string
       | FunctionType of datatype list * datatype * datatype
       | MuType of string * datatype | UnitType | TupleType of datatype list
       | RecordType of row | VariantType of row | TableType of datatype * datatype
       | ListType of datatype | TypeApplication of (string * (datatype list))
       | PrimitiveType of Types.primitive | DBType
    *)
    method datatype : datatype -> datatype =
      function
      | TypeVar _x0 -> TypeVar (o#string _x0)
      | RigidTypeVar _x0 -> RigidTypeVar (o#string _x0)
      | FunctionType (_x0, _x1, _x2) ->
          FunctionType (o#list o#datatype _x0, o#datatype _x1,
            o#datatype _x2)
      | MuType (_x0, _x1) -> MuType (o#string _x0, o#datatype _x1)
      | UnitType -> UnitType
      | TupleType _x0 -> TupleType (o#list o#datatype _x0)
      | RecordType _x0 -> RecordType (o#row _x0)
      | VariantType _x0 -> VariantType (o#row _x0)
      | TableType (_x0, _x1) -> TableType (o#datatype _x0, o#datatype _x1)
      | ListType _x0 -> ListType (o#datatype _x0)
      | TypeApplication _x0 ->
          TypeApplication
            ((fun (_x0, _x1) -> ((o#string _x0), (o#list o#datatype _x1)))
               _x0)
      | PrimitiveType _x0 -> PrimitiveType (o#_Types_primitive _x0)
      | DBType -> DBType

    (* type constant =
       [ `Float of float | `Int of num | `String of string | `Bool of bool
       | `Char of char ] *)
    method constant : constant -> constant =
      function
      | `Float x -> `Float (o#float x)
      | `Int x -> `Int (o#num x)
      | `String x -> `String (o#string x)
      | `Bool x -> `Bool (o#bool x)
      | `Char x -> `Char (o#char x)

    (* and binding =(((ppattern, phrase) binding') * pposition) *)
    method binding : binding -> binding =
      fun (_x0, _x1) ->
        ((o#binding' o#ppattern o#phrase _x0), (o#pposition _x1))

    (* type assumption = ((quantifier list) * datatype) *)
    method assumption : assumption -> assumption =
      fun (_x0, _x1) -> ((o#list o#quantifier _x0), (o#datatype _x1))
  end
