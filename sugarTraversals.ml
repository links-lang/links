(* Generated mostly automatically using Camlp4 in OCaml 3.10.0.

   NB: DO NOT EDIT (except to keep it in line with sugartypes.ml).

   When we switch to OCaml 3.10.x, and when the Camlp4MapGenerator
   filter which comes with camlp4 3.10.0 works sufficiently well we'll
   generate all this automatically instead of maintaining this file.
*)

(*
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
    | `FormletPlacement of ('phrase * 'phrase * 'phrase)
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
            ((fun (_x0, _x1, _x2) ->
                ((btoe _x0), (btoe _x1), (btoe _x2)))
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
      | `Include str ->
          `Include str
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

class fold =
  object ((o : 'SELF))
    method string = fun (_ : string) -> (o : 'SELF)
    method int = fun (_ : int) -> (o : 'SELF)
    method char = fun (_ : char) -> (o : 'SELF)
    method float = fun (_ : float) -> (o : 'SELF)
    method bool = fun (_ : bool) -> (o : 'SELF)
    method list :
      'a. ('SELF -> 'a -> 'SELF) -> 'a list -> 'SELF =
      fun f -> List.fold_left f o
    method option :
      'a. ('SELF -> 'a -> 'SELF) -> 'a option -> 'SELF =
      fun f -> function | None -> o | Some x -> f o x
    method array :
      'a. ('SELF -> 'a -> 'SELF) -> 'a array -> 'SELF =
      fun f -> Array.fold_left f o
    method ref :
      'a. ('SELF -> 'a -> 'SELF) -> 'a ref -> 'SELF =
      fun f { contents = x } -> f o x


    method _Types_primitive : Types.primitive -> 'SELF = fun _ -> o
    method _Syntax_untyped_expression :
      Syntax.untyped_expression -> 'SELF = fun _ -> o
    method _Syntax_untyped_definition :
      Syntax.untyped_definition -> 'SELF = fun _ -> o
    method _Syntax_location : Syntax.location -> 'SELF = fun _ -> o
    method _Regex_repeat : Regex.repeat -> 'SELF = fun _ -> o
    method _Num_num : Num.num -> 'SELF = fun _ -> o

    method _Lexing_position : Lexing.position -> 'SELF = fun _ -> o
      

    (*type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name | `App ]*)
    method binop : binop -> 'SELF = (* handwritten *)
      function
        | `Minus -> o
        | `FloatMinus -> o
        | `RegexMatch regexflags -> o#list (fun o -> o#regexflag) regexflags
        | #logical_binop as l -> o#logical_binop l
        | `Cons -> o
        | `Name name -> o#name name
        | `App -> o

    (*type operator = [ unary_op | binop | `Project of name ] *)
    method operator : operator -> 'SELF = (* handwritten *)
      function 
        | #unary_op as u -> o#unary_op u
        | #binop as b -> o#binop b
        | `Project name -> o#name name

    (* type ('phrase, 'binding) sentence'' = [
       | `Definitions of 'binding list
       | `Expression of 'phrase
       | `Directive of directive ] *)
    method sentence'' : 'a 'b . ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a,'b) sentence'' -> 'SELF =
      fun phrase binding -> (* partially handwritten *)
      function
        | `Definitions x -> o#list (fun o -> binding o) x
        | `Expression x -> phrase o x
        | `Directive x -> o#directive x

    (* type ('ppattern, 'phrase, 'binding) phrasenode' = [
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
       | `FormletPlacement of ('phrase * 'phrase * 'phrase)
       | `PagePlacement of ('phrase)
       | `FormBinding of ('phrase * 'ppattern) ]
    *)    
    method phrasenode'
      : 'a 'b 'c . ('SELF -> 'a -> 'SELF)
     -> ('SELF -> 'b -> 'SELF)
     -> ('SELF -> 'c -> 'SELF)
    -> ('a,'b,'c) phrasenode' -> 'SELF =
      fun ppattern phrase binding -> (* partially handwritten *)
      function
      | `Constant x -> o#constant x
      | `Var x -> o#name x
      | `FunLit x ->
          o#funlit' ppattern phrase x
      | `Spawn x -> phrase o x
      | `SpawnWait x -> phrase o x
      | `ListLit x -> o#list phrase x
      | `Iteration x ->
          (fun (_x0, _x1, _x2, _x3) ->
             ((phrase
                 (o#list (fun o -> o#iterpatt ppattern phrase) _x0)
                 _x1)
                #option phrase _x2)
               #option phrase _x3)
            x
      | `Escape x -> (fun (_x0, _x1) -> phrase (o#name _x0) _x1) x
      | `Section x -> o#sec x
      | `Conditional x ->
          (fun (_x0, _x1, _x2) ->
             phrase (phrase (phrase o _x0) _x1) _x2)
            x
      | `Block x ->
          (fun (_x0, _x1) ->
             phrase (o#list (fun o -> binding o) _x0) _x1)
            x
      | `InfixAppl x ->
          (fun (_x0, _x1, _x2) -> phrase (phrase (o#binop _x0) _x1) _x2) x
      | `Regex x -> o#regex' (fun o -> phrase o) x
      | `UnaryAppl x ->
          (fun (_x0, _x1) -> phrase (o#unary_op _x0) _x1) x
      | `FnAppl x ->
          (fun (_x0, _x1) ->
             (phrase o _x0)#list (fun o -> phrase o) _x1)
            x
      | `TupleLit x -> o#list (fun o -> phrase o) x
      | `RecordLit x ->
          (fun (_x0, _x1) ->
             (o#list (fun o (_x0, _x1) -> phrase (o#name _x0) _x1)
                _x0)#
               option (fun o -> phrase o) _x1)
            x
      | `Projection x ->
          (fun (_x0, _x1) -> (phrase o _x0)#name _x1) x
      | `With x ->
          (fun (_x0, _x1) ->
             (phrase o _x0)#list
               (fun o (_x0, _x1) -> phrase (o#name _x0) _x1) _x1)
            x
      | `TypeAnnotation x ->
          (fun (_x0, _x1) -> (phrase o _x0)#datatype _x1) x
      | `Upcast x ->
          (fun (_x0, _x1, _x2) ->
             ((phrase o _x0)#datatype _x1)#datatype _x2)
            x
      | `ConstructorLit x ->
          (fun (_x0, _x1) ->
             (o#name _x0)#option (fun o -> phrase o) _x1)
            x
      | `Switch x ->
          (fun (_x0, _x1) ->
             (phrase o _x0)#list
               (fun o (_x0, _x1) ->
                  phrase (ppattern o _x0) _x1)
               _x1)
            x
      | `Receive x ->
          o#list
            (fun o (_x0, _x1) -> phrase (ppattern o _x0) _x1)
            x
      | `DatabaseLit x ->
          (fun (_x0, _x1) ->
             (fun (_x0, _x1) ->
                (o#option (fun o -> phrase o) _x0)#option
                  (fun o -> phrase o) _x1)
               _x1)
            x
      | `TableLit x ->
          (fun (_x0, _x1, _x2, _x3) ->
             phrase (((phrase o _x0)#datatype _x1)#list
                       (fun o (_x0, _x1) ->
                          (o#string _x0)#list (fun o -> o#fieldconstraint) _x1)
                       _x2)
               _x3)
            x
      | `DBDelete x ->
          (fun (_x0, _x1, _x2) ->
             (phrase (ppattern o _x0) _x1)#option
               (fun o -> phrase o) _x2)
            x
      | `DBInsert x ->
          (fun (_x0, _x1) -> phrase (phrase o _x0) _x1) x
      | `DBUpdate x ->
          (fun (_x0, _x1, _x2, _x3) ->
             ((phrase (ppattern o _x0) _x1)#option
                (fun o -> phrase o) _x2)#
               list (fun o (_x0, _x1) -> phrase (o#name _x0) _x1)
               _x3)
            x
      | `Xml x ->
          (fun (_x0, _x1, _x2, _x3) ->
             (((o#name _x0)#list
                 (fun o (_x0, _x1) ->
                    (o#string _x0)#list (fun o -> phrase o) _x1)
                 _x1)#
                option (fun o -> phrase o) _x2)#
               list (fun o -> phrase o) _x3)
            x
      | `TextNode x -> o#string x
      | `Formlet x ->
          (fun (_x0, _x1) -> phrase (phrase o _x0) _x1) x
      | `Page x -> phrase o x
      | `FormletPlacement x ->
          (fun (_x0, _x1, _x2) -> phrase (phrase (phrase o _x0) _x1) _x2) x
      | `PagePlacement x -> phrase o x
      | `FormBinding x ->
          (fun (_x0, _x1) -> ppattern (phrase o _x0) _x1)
            x

    (* type ('ppattern, 'phrase) iterpatt = [ `List of 'ppattern * 'phrase | `Table of 'ppattern * 'phrase ] *)
    method iterpatt : 'a 'b . ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a,'b) iterpatt -> 'SELF =
      fun ppattern phrase -> (* partially handwritten *)
      function
      | `List x ->
          (fun (_x0, _x1) -> phrase (ppattern o _x0) _x1) x
      | `Table x ->
          (fun (_x0, _x1) -> phrase (ppattern o _x0) _x1) x

    (*type ('ppattern, 'phrase) funlit' = 'ppattern list list * 'phrase *)
    method funlit' : 'a 'b . ('SELF -> 'a -> 'SELF) -> ('SELF -> 'b -> 'SELF) -> ('a,'b) funlit' -> 'SELF =
      fun ppattern phrase -> (* partially handwritten *)
      fun (_x0, _x1) ->
        phrase (o#list (fun o -> o#list (fun o -> ppattern o)) _x0) _x1
          (* FIXME: this doesn't seem right: _x0 isn't even used! *) 

    (* type ('ppattern, 'phrase) binding' = [
       | `Val of 'ppattern * 'phrase * location * datatype option
       | `Fun of name * ('ppattern, 'phrase) funlit' * location * datatype option
       | `Funs of (name * ('ppattern, 'phrase) funlit' * location * datatype option) list
       | `Foreign of name * name * datatype
       | `Type of (name * name list * datatype)
       | `Infix
       | `Exp of 'phrase ] *)
    method binding'  : 'a 'b . ('SELF -> 'a -> 'SELF) ->  ('SELF -> 'b -> 'SELF) -> ('a,'b) binding' -> 'SELF =
      fun ppattern phrase -> (* partially handwritten *)
      function
      | `Val x ->
          (fun (_x0, _x1, _x2, _x3) ->
             ((phrase (ppattern o _x0) _x1)#location _x2)#
               option (fun o -> o#datatype) _x3)
            x
      | `Fun x ->
          (fun (_x0, _x1, _x2, _x3) ->
             (((o#name _x0)#funlit' ppattern phrase _x1)#
                location _x2)#
               option (fun o -> o#datatype) _x3)
            x
      | `Funs x ->
          o#list
            (fun o (_x0, _x1, _x2, _x3) ->
               (((o#name _x0)#funlit' (fun o -> ppattern o)
                   (fun o -> phrase o) _x1)#
                  location _x2)#
                 option (fun o -> o#datatype) _x3)
            x
      | `Foreign x ->
          (fun (_x0, _x1, _x2) -> ((o#name _x0)#name _x1)#datatype _x2) x
      | `Include x ->
          (fun  _x0  -> (o#string _x0)) x
      | `Type x ->
          (fun (_x0, _x1, _x2) ->
             ((o#name _x0)#list (fun o -> o#name) _x1)#datatype _x2)
            x
      | `Infix -> o
      | `Exp x -> phrase o x


    (*
      type unary_op = [
      | `Minus
      | `FloatMinus
      | `Name of name
      | `Abs
      ]
    *)
    method unary_op : unary_op -> 'SELF =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `Name x -> o#name x
      | `Abs -> o

    (* type sentence' = [ `Definitions of Syntax.untyped_definition list
       | `Expression of Syntax.untyped_expression
       | `Directive of directive ] 
    *)
    method sentence' : sentence' -> 'SELF =
      function
      | `Definitions x -> o#list (fun o -> o#_Syntax_untyped_definition) x
      | `Expression x -> o#_Syntax_untyped_expression x
      | `Directive x -> o#directive x

    (* type sentence = (phrase, binding) sentence'' *)
    method sentence : sentence -> 'SELF =
      o#sentence'' (fun o -> o#phrase) (fun o -> o#binding)

    (* type sec = [`Minus | `FloatMinus|`Project of name|`Name of name] *)
    method sec : sec -> 'SELF =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `Project x -> o#name x
      | `Name x -> o#name x

    (* and row_var = [ `Closed | `Open of string | `Recursive of string * row ] *)
    method row_var : row_var -> 'SELF =
      function
      | `Closed -> o
      | `Open x -> o#string x
      | `Recursive x -> (fun (_x0, _x1) -> (o#string _x0)#row _x1) x

    (* and row = (string * fieldspec) list * row_var *)
    method row : row -> 'SELF =
      fun (_x0, _x1) ->
        (o#list (fun o (_x0, _x1) -> (o#string _x0)#fieldspec _x1) _x0)#
          row_var _x1

    (* type 'phrase replace_rhs = [`Literal of string | `Splice of 'phrase] *)
    method replace_rhs :
      'a. ('SELF -> 'a -> 'SELF) -> 'a replace_rhs -> 'SELF =
      fun _f_phrase ->
        function | `Literal x -> o#string x | `Splice x -> _f_phrase o x

    (* and regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ] *)
    method regexflag : regexflag -> 'SELF =
      function
      | `RegexList -> o
      | `RegexNative -> o
      | `RegexGlobal -> o
      | `RegexReplace -> o

    (* type 'phrase regex' = [
       | `Range of (char * char)
       | `Simply of string
       | `Quote of 'phrase regex'
       | `Any
       | `StartAnchor
       | `EndAnchor
       | `Seq of 'phrase regex' list
       | `Alternate of ('phrase regex' * 'phrase regex')
       | `Group of 'phrase regex'
       | `Repeat of (Regex.repeat * 'phrase regex')
       | `Splice of 'phrase
       | `Replace of ('phrase regex' * 'phrase replace_rhs) ] *)
    method regex' :
      'a. ('SELF -> 'a -> 'SELF) -> 'a regex' -> 'SELF =
      fun _f_phrase ->
        function
        | `Range x -> (fun (_x0, _x1) -> (o#char _x0)#char _x1) x
        | `Simply x -> o#string x
        | `Quote x -> o#regex' (fun o -> _f_phrase o) x
        | `Any -> o
        | `StartAnchor -> o
        | `EndAnchor -> o
        | `Seq x -> o#list (fun o -> o#regex' (fun o -> _f_phrase o)) x
        | `Alternate x ->
            (fun (_x0, _x1) ->
               (o#regex' (fun o -> _f_phrase o) _x0)#regex'
                 (fun o -> _f_phrase o) _x1)
              x
        | `Group x -> o#regex' (fun o -> _f_phrase o) x
        | `Repeat x ->
            (fun (_x0, _x1) ->
               (o#_Regex_repeat _x0)#regex' (fun o -> _f_phrase o) _x1)
              x
        | `Splice x -> _f_phrase o x
        | `Replace x ->
            (fun (_x0, _x1) ->
               (o#regex' (fun o -> _f_phrase o) _x0)#replace_rhs
                 (fun o -> _f_phrase o) _x1)
              x

    (* type regex = phrase regex' *)
    method regex : regex -> 'SELF = o#regex' (fun o -> o#phrase)

    (* type quantifier = [`TypeVar of string | `RigidTypeVar of string | `RowVar of string] *)
    method quantifier : quantifier -> 'SELF =
      function
      | `TypeVar x -> o#string x
      | `RigidTypeVar x -> o#string x
      | `RowVar x -> o#string x

    (* type pposition = Lexing.position * Lexing.position *)
    method pposition : pposition -> 'SELF =
      fun (_x0, _x1) -> o#_Lexing_position _x1

    (* and ppattern = ppattern pattern' * pposition *)
    method ppattern : ppattern -> 'SELF =
      fun (_x0, _x1) -> (o#pattern' (fun o -> o#ppattern) _x0)#pposition _x1

    (* type phrasenode = (ppattern, phrase, binding) phrasenode' *)
    method phrasenode : phrasenode -> 'SELF =
      o#phrasenode' (fun o -> o#ppattern) (fun o -> o#phrase) (fun o -> o#binding) 

    (* type phrase = (ppattern, phrase, binding) phrasenode' * pposition *)
    method phrase : phrase -> 'SELF =
      fun (_x0, _x1) ->
        (o#phrasenode' (fun o -> o#ppattern) (fun o -> o#phrase) (fun o -> o#binding) _x0)#
          pposition _x1

    (* type 'ppattern pattern' = [
       | `Any
       | `Nil
       | `Cons of ('ppattern * 'ppattern)
       | `List of ('ppattern list)
       | `Variant of (string * 'ppattern option)
       | `Record of ((string * 'ppattern) list * 'ppattern option)
       | `Tuple of ('ppattern list)
       | `Constant of constant
       | `Variable of string
       | `As of (string * 'ppattern)
       | `HasType of 'ppattern * datatype
       ] 
    *)
    method pattern' :
      'a. ('SELF -> 'a -> 'SELF) -> 'a pattern' -> 'SELF =
      fun _f_ppattern ->
        function
        | `Any -> o
        | `Nil -> o
        | `Cons x -> (fun (_x0, _x1) -> _f_ppattern o _x1) x
        | `List x -> o#list (fun o -> _f_ppattern o) x
        | `Variant x ->
            (fun (_x0, _x1) ->
               (o#string _x0)#option (fun o -> _f_ppattern o) _x1)
              x
        | `Record x ->
            (fun (_x0, _x1) ->
               (o#list (fun o (_x0, _x1) -> _f_ppattern o _x1) _x0)#option
                 (fun o -> _f_ppattern o) _x1)
              x
        | `Tuple x -> o#list (fun o -> _f_ppattern o) x
        | `Constant x -> o#constant x
        | `Variable x -> o#string x
        | `As x -> (fun (_x0, _x1) -> _f_ppattern o _x1) x
        | `HasType x ->
            (fun (_x0, _x1) -> (_f_ppattern o _x0)#datatype _x1) x

    (* type pattern = ppattern pattern' *)
    method pattern : pattern -> 'SELF = o#pattern' (fun o -> o#ppattern)

    (* type num = Num.num *)
    method num : num -> 'SELF = o#_Num_num

    (* type name = string *)
    method name : name -> 'SELF = o#string

    (* type logical_binop = [`And | `Or ] *)
    method logical_binop : logical_binop -> 'SELF =
      function | `And -> o | `Or -> o

    (* type location = Syntax.location *)
    method location : location -> 'SELF = o#_Syntax_location

    (* type funlit = (ppattern, phrase) funlit' *)
    method funlit : funlit -> 'SELF =
      o#funlit' (fun o -> o#ppattern) (fun o -> o#phrase)

    (* and fieldspec = [`Present of datatype | `Absent] *)
    method fieldspec : fieldspec -> 'SELF =
      function | `Present x -> o#datatype x | `Absent -> o

    (* type fieldconstraint = [ `Readonly ] *)
    method fieldconstraint : fieldconstraint -> 'SELF =
      function | `Readonly -> o

    (* type directive = string * string list *)
    method directive : directive -> 'SELF =
      fun (_x0, _x1) -> (o#string _x0)#list (fun o -> o#string) _x1

    (* type datatype = 
       | TypeVar of string
       | RigidTypeVar of string
       | FunctionType of datatype list * datatype * datatype
       | MuType of string * datatype
       | UnitType
       | TupleType of (datatype list)
       | RecordType of row
       | VariantType of row
       | TableType of datatype * datatype
       | ListType of datatype
       | TypeApplication of (string * datatype list)
       | PrimitiveType of Types.primitive
       | DBType *)
    method datatype : datatype -> 'SELF =
      function
      | TypeVar _x0 -> o#string _x0
      | RigidTypeVar _x0 -> o#string _x0
      | FunctionType (_x0, _x1, _x2) ->
          ((o#list (fun o -> o#datatype) _x0)#datatype _x1)#datatype _x2
      | MuType (_x0, _x1) -> (o#string _x0)#datatype _x1
      | UnitType -> o
      | TupleType _x0 -> o#list (fun o -> o#datatype) _x0
      | RecordType _x0 -> o#row _x0
      | VariantType _x0 -> o#row _x0
      | TableType (_x0, _x1) -> (o#datatype _x0)#datatype _x1
      | ListType _x0 -> o#datatype _x0
      | TypeApplication _x0 ->
          (fun (_x0, _x1) -> (o#string _x0)#list (fun o -> o#datatype) _x1)
            _x0
      | PrimitiveType _x0 -> o#_Types_primitive _x0
      | DBType -> o

    (* type constant = [
       | `Float of float
       | `Int of num
       | `String of string
       | `Bool of bool
       | `Char of char ]
    *)
    method constant : constant -> 'SELF =
      function
      | `Float x -> o#float x
      | `Int x -> o#num x
      | `String x -> o#string x
      | `Bool x -> o#bool x
      | `Char x -> o#char x

    (* and binding = (ppattern, phrase) binding' * pposition *)
    method binding : binding -> 'SELF =
      fun (_x0, _x1) ->
        (o#binding' (fun o -> o#ppattern) (fun o -> o#phrase) _x0)#pposition
          _x1

    (* type assumption = quantifier list * datatype *)
    method assumption : assumption -> 'SELF =
      fun (_x0, _x1) -> (o#list (fun o -> o#quantifier) _x0)#datatype _x1
  end
*)
