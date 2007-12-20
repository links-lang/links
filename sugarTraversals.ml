(* Generated mostly automatically using Camlp4 in OCaml 3.10.0.

   NB: DO NOT EDIT (except to keep it in line with sugartypes.ml).

   When we switch to OCaml 3.10.x, and when the Camlp4MapGenerator
   filter which comes with camlp4 3.10.0 works sufficiently well we'll
   generate all this automatically instead of maintaining this file.
*)

open Sugartypes

class map =
  object ((o : 'self_type))
    method string : string -> string = o#unknown
      
    method option :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
      fun _f_a ->
        function | None -> None | Some _x -> let _x = _f_a o _x in Some _x
      
    method list :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
      fun _f_a ->
        function
        | [] -> []
        | _x :: _x_i1 ->
            let _x = _f_a o _x in
            let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
      
    method float : float -> float = o#unknown
      
    method char : char -> char = o#unknown
      
    method bool : bool -> bool = function | false -> false | true -> true
      
    method unary_op : unary_op -> unary_op =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `Name _x -> let _x = o#name _x in `Name _x
      | `Abs -> `Abs
      
    method typed_name : typed_name -> typed_name =
      fun (_x, _x_i1) ->
        let _x = o#name _x in
        let _x_i1 = o#option (fun o -> o#unknown) _x_i1 in (_x, _x_i1)
      
    method sentence' : sentence' -> sentence' =
      function
      | `Definitions _x ->
          let _x = o#list (fun o -> o#unknown) _x in `Definitions _x
      | `Expression _x -> let _x = o#unknown _x in `Expression _x
      | `Directive _x -> let _x = o#directive _x in `Directive _x
      
    method sentence : sentence -> sentence =
      function
      | `Definitions _x ->
          let _x = o#list (fun o -> o#binding) _x in `Definitions _x
      | `Expression _x -> let _x = o#phrase _x in `Expression _x
      | `Directive _x -> let _x = o#directive _x in `Directive _x
      
    method sec : sec -> sec =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `Project _x -> let _x = o#name _x in `Project _x
      | `Name _x -> let _x = o#name _x in `Name _x
      
    method row_var : row_var -> row_var =
      function
      | `Closed -> `Closed
      | `Open _x -> let _x = o#string _x in `Open _x
      | `Recursive ((_x, _x_i1)) ->
          let _x = o#string _x in
          let _x_i1 = o#row _x_i1 in `Recursive ((_x, _x_i1))
      
    method row : row -> row =
      fun (_x, _x_i1) ->
        let _x =
          o#list
            (fun o (_x, _x_i1) ->
               let _x = o#string _x in
               let _x_i1 = o#fieldspec _x_i1 in (_x, _x_i1))
            _x in
        let _x_i1 = o#row_var _x_i1 in (_x, _x_i1)
      
    method replace_rhs : replace_rhs -> replace_rhs =
      function
      | `Literal _x -> let _x = o#string _x in `Literal _x
      | `Splice _x -> let _x = o#phrase _x in `Splice _x
      
    method regexflag : regexflag -> regexflag =
      function
      | `RegexList -> `RegexList
      | `RegexNative -> `RegexNative
      | `RegexGlobal -> `RegexGlobal
      | `RegexReplace -> `RegexReplace
      
    method regex : regex -> regex =
      function
      | `Range ((_x, _x_i1)) ->
          let _x = o#char _x in
          let _x_i1 = o#char _x_i1 in `Range ((_x, _x_i1))
      | `Simply _x -> let _x = o#string _x in `Simply _x
      | `Quote _x -> let _x = o#regex _x in `Quote _x
      | `Any -> `Any
      | `StartAnchor -> `StartAnchor
      | `EndAnchor -> `EndAnchor
      | `Seq _x -> let _x = o#list (fun o -> o#regex) _x in `Seq _x
      | `Alternate ((_x, _x_i1)) ->
          let _x = o#regex _x in
          let _x_i1 = o#regex _x_i1 in `Alternate ((_x, _x_i1))
      | `Group _x -> let _x = o#regex _x in `Group _x
      | `Repeat ((_x, _x_i1)) ->
          let _x = o#unknown _x in
          let _x_i1 = o#regex _x_i1 in `Repeat ((_x, _x_i1))
      | `Splice _x -> let _x = o#phrase _x in `Splice _x
      | `Replace ((_x, _x_i1)) ->
          let _x = o#regex _x in
          let _x_i1 = o#replace_rhs _x_i1 in `Replace ((_x, _x_i1))
      
    method quantifier : quantifier -> quantifier =
      function
      | `TypeVar _x -> let _x = o#string _x in `TypeVar _x
      | `RigidTypeVar _x -> let _x = o#string _x in `RigidTypeVar _x
      | `RowVar _x -> let _x = o#string _x in `RowVar _x
      
    method position : position -> position =
      fun (_x, _x_i1) ->
        let _x = o#unknown _x in let _x_i1 = o#unknown _x_i1 in (_x, _x_i1)
      
    method phrasenode : phrasenode -> phrasenode =
      function
      | `Constant _x -> let _x = o#constant _x in `Constant _x
      | `Var _x -> let _x = o#name _x in `Var _x
      | `FunLit _x -> let _x = o#funlit _x in `FunLit _x
      | `Spawn _x -> let _x = o#phrase _x in `Spawn _x
      | `SpawnWait _x -> let _x = o#phrase _x in `SpawnWait _x
      | `ListLit _x -> let _x = o#list (fun o -> o#phrase) _x in `ListLit _x
      | `Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#list (fun o -> o#iterpatt) _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 = o#option (fun o -> o#phrase) _x_i3
          in `Iteration ((_x, _x_i1, _x_i2, _x_i3))
      | `Escape ((_x, _x_i1)) ->
          let _x = o#typed_name _x in
          let _x_i1 = o#phrase _x_i1 in `Escape ((_x, _x_i1))
      | `Section _x -> let _x = o#sec _x in `Section _x
      | `Conditional ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in `Conditional ((_x, _x_i1, _x_i2))
      | `Block ((_x, _x_i1)) ->
          let _x = o#list (fun o -> o#binding) _x in
          let _x_i1 = o#phrase _x_i1 in `Block ((_x, _x_i1))
      | `InfixAppl ((_x, _x_i1, _x_i2)) ->
          let _x = o#binop _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in `InfixAppl ((_x, _x_i1, _x_i2))
      | `Regex _x -> let _x = o#regex _x in `Regex _x
      | `UnaryAppl ((_x, _x_i1)) ->
          let _x = o#unary_op _x in
          let _x_i1 = o#phrase _x_i1 in `UnaryAppl ((_x, _x_i1))
      | `FnAppl ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#phrase) _x_i1
          in `FnAppl ((_x, _x_i1))
      | `TupleLit _x ->
          let _x = o#list (fun o -> o#phrase) _x in `TupleLit _x
      | `RecordLit ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#phrase) _x_i1
          in `RecordLit ((_x, _x_i1))
      | `Projection ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#name _x_i1 in `Projection ((_x, _x_i1))
      | `With ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1
          in `With ((_x, _x_i1))
      | `TypeAnnotation ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype _x_i1 in `TypeAnnotation ((_x, _x_i1))
      | `Upcast ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype _x_i1 in
          let _x_i2 = o#datatype _x_i2 in `Upcast ((_x, _x_i1, _x_i2))
      | `ConstructorLit ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#option (fun o -> o#phrase) _x_i1
          in `ConstructorLit ((_x, _x_i1))
      | `Switch ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1
          in `Switch ((_x, _x_i1))
      | `Receive _x ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x
          in `Receive _x
      | `DatabaseLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            (fun (_x, _x_i1) ->
               let _x = o#option (fun o -> o#phrase) _x in
               let _x_i1 = o#option (fun o -> o#phrase) _x_i1 in (_x, _x_i1))
              _x_i1
          in `DatabaseLit ((_x, _x_i1))
      | `TableLit ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype _x_i1 in
          let _x_i2 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in
                 let _x_i1 = o#list (fun o -> o#fieldconstraint) _x_i1
                 in (_x, _x_i1))
              _x_i2 in
          let _x_i3 = o#phrase _x_i3 in `TableLit ((_x, _x_i1, _x_i2, _x_i3))
      | `DBDelete ((_x, _x_i1, _x_i2)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2
          in `DBDelete ((_x, _x_i1, _x_i2))
      | `DBInsert ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in `DBInsert ((_x, _x_i1))
      | `DBUpdate ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i3
          in `DBUpdate ((_x, _x_i1, _x_i2, _x_i3))
      | `Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#name _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in
                 let _x_i1 = o#list (fun o -> o#phrase) _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 = o#list (fun o -> o#phrase) _x_i3
          in `Xml ((_x, _x_i1, _x_i2, _x_i3))
      | `TextNode _x -> let _x = o#string _x in `TextNode _x
      | `Formlet ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in `Formlet ((_x, _x_i1))
      | `Page _x -> let _x = o#phrase _x in `Page _x
      | `FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2
          in `FormletPlacement ((_x, _x_i1, _x_i2))
      | `PagePlacement _x -> let _x = o#phrase _x in `PagePlacement _x
      | `FormBinding ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#pattern _x_i1 in `FormBinding ((_x, _x_i1))
      
    method phrase : phrase -> phrase =
      fun (_x, _x_i1) ->
        let _x = o#phrasenode _x in
        let _x_i1 = o#position _x_i1 in (_x, _x_i1)
      
    method patternnode : patternnode -> patternnode =
      function
      | `Any -> `Any
      | `Nil -> `Nil
      | `Cons ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#pattern _x_i1 in `Cons ((_x, _x_i1))
      | `List _x -> let _x = o#list (fun o -> o#pattern) _x in `List _x
      | `Variant ((_x, _x_i1)) ->
          let _x = o#string _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in `Variant ((_x, _x_i1))
      | `Record ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in
                 let _x_i1 = o#pattern _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in `Record ((_x, _x_i1))
      | `Tuple _x -> let _x = o#list (fun o -> o#pattern) _x in `Tuple _x
      | `Constant _x -> let _x = o#constant _x in `Constant _x
      | `Variable _x -> let _x = o#typed_name _x in `Variable _x
      | `As ((_x, _x_i1)) ->
          let _x = o#typed_name _x in
          let _x_i1 = o#pattern _x_i1 in `As ((_x, _x_i1))
      | `HasType ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#datatype _x_i1 in `HasType ((_x, _x_i1))
      
    method pattern : pattern -> pattern =
      fun (_x, _x_i1) ->
        let _x = o#patternnode _x in
        let _x_i1 = o#position _x_i1 in (_x, _x_i1)
      
    method operator : operator -> operator =
      function
      | (#unary_op as x) -> (o#unary_op x :> operator)
      | (#binop as x) -> (o#binop x :> operator)
      | `Project _x -> let _x = o#name _x in `Project _x
      
    method num : num -> num = o#unknown
      
    method name : name -> name = o#string
      
    method logical_binop : logical_binop -> logical_binop =
      function | `And -> `And | `Or -> `Or
      
    method location : location -> location = o#unknown
      
    method iterpatt : iterpatt -> iterpatt =
      function
      | `List ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in `List ((_x, _x_i1))
      | `Table ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in `Table ((_x, _x_i1))
      
    method funlit : funlit -> funlit =
      fun (_x, _x_i1) ->
        let _x = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
        let _x_i1 = o#phrase _x_i1 in (_x, _x_i1)
      
    method fieldspec : fieldspec -> fieldspec =
      function
      | `Present _x -> let _x = o#datatype _x in `Present _x
      | `Absent -> `Absent
      
    method fieldconstraint : fieldconstraint -> fieldconstraint =
      function | `Readonly -> `Readonly
      
    method directive : directive -> directive =
      fun (_x, _x_i1) ->
        let _x = o#string _x in
        let _x_i1 = o#list (fun o -> o#string) _x_i1 in (_x, _x_i1)
      
    method datatype : datatype -> datatype =
      function
      | TypeVar _x -> let _x = o#string _x in TypeVar _x
      | RigidTypeVar _x -> let _x = o#string _x in RigidTypeVar _x
      | FunctionType (_x, _x_i1, _x_i2) ->
          let _x = o#list (fun o -> o#datatype) _x in
          let _x_i1 = o#datatype _x_i1 in
          let _x_i2 = o#datatype _x_i2 in FunctionType (_x, _x_i1, _x_i2)
      | MuType (_x, _x_i1) ->
          let _x = o#string _x in
          let _x_i1 = o#datatype _x_i1 in MuType (_x, _x_i1)
      | UnitType -> UnitType
      | TupleType _x ->
          let _x = o#list (fun o -> o#datatype) _x in TupleType _x
      | RecordType _x -> let _x = o#row _x in RecordType _x
      | VariantType _x -> let _x = o#row _x in VariantType _x
      | TableType (_x, _x_i1) ->
          let _x = o#datatype _x in
          let _x_i1 = o#datatype _x_i1 in TableType (_x, _x_i1)
      | ListType _x -> let _x = o#datatype _x in ListType _x
      | TypeApplication _x ->
          let _x =
            (fun (_x, _x_i1) ->
               let _x = o#string _x in
               let _x_i1 = o#list (fun o -> o#datatype) _x_i1 in (_x, _x_i1))
              _x
          in TypeApplication _x
      | PrimitiveType _x -> let _x = o#unknown _x in PrimitiveType _x
      | DBType -> DBType
      
    method constant : constant -> constant =
      function
      | `Float _x -> let _x = o#float _x in `Float _x
      | `Int _x -> let _x = o#num _x in `Int _x
      | `String _x -> let _x = o#string _x in `String _x
      | `Bool _x -> let _x = o#bool _x in `Bool _x
      | `Char _x -> let _x = o#char _x in `Char _x
      
    method binop : binop -> binop =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `RegexMatch _x ->
          let _x = o#list (fun o -> o#regexflag) _x in `RegexMatch _x
      | (#logical_binop as x) -> (o#logical_binop x :> binop)
      | `Cons -> `Cons
      | `Name _x -> let _x = o#name _x in `Name _x
      | `App -> `App
      
    method bindingnode : bindingnode -> bindingnode =
      function
      | `Val ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#location _x_i2 in
          let _x_i3 = o#option (fun o -> o#datatype) _x_i3
          in `Val ((_x, _x_i1, _x_i2, _x_i3))
      | `Fun ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#typed_name _x in
          let _x_i1 = o#funlit _x_i1 in
          let _x_i2 = o#location _x_i2 in
          let _x_i3 = o#option (fun o -> o#datatype) _x_i3
          in `Fun ((_x, _x_i1, _x_i2, _x_i3))
      | `Funs _x ->
          let _x =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let _x = o#typed_name _x in
                 let _x_i1 = o#funlit _x_i1 in
                 let _x_i2 = o#location _x_i2 in
                 let _x_i3 = o#option (fun o -> o#datatype) _x_i3
                 in (_x, _x_i1, _x_i2, _x_i3))
              _x
          in `Funs _x
      | `Foreign ((_x, _x_i1, _x_i2)) ->
          let _x = o#name _x in
          let _x_i1 = o#name _x_i1 in
          let _x_i2 = o#datatype _x_i2 in `Foreign ((_x, _x_i1, _x_i2))
      | `Include _x -> let _x = o#string _x in `Include _x
      | `Type ((_x, _x_i1, _x_i2)) ->
          let _x = o#name _x in
          let _x_i1 = o#list (fun o -> o#name) _x_i1 in
          let _x_i2 = o#datatype _x_i2 in `Type ((_x, _x_i1, _x_i2))
      | `Infix -> `Infix
      | `Exp _x -> let _x = o#phrase _x in `Exp _x
      
    method binding : binding -> binding =
      fun (_x, _x_i1) ->
        let _x = o#bindingnode _x in
        let _x_i1 = o#position _x_i1 in (_x, _x_i1)
      
    method assumption : assumption -> assumption =
      fun (_x, _x_i1) ->
        let _x = o#list (fun o -> o#quantifier) _x in
        let _x_i1 = o#datatype _x_i1 in (_x, _x_i1)
      
    method program : program -> program = 
      fun (bindings, phrase) ->
        let bindings = o#list (fun o -> o#binding) bindings in
        let phrase = o#option (fun o -> o#phrase) phrase in
          (bindings, phrase)

    method unknown : 'a. 'a -> 'a = fun x -> x
      
  end
  
class fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
      
    method option :
      'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
      fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
      
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
      
    method float : float -> 'self_type = o#unknown
      
    method char : char -> 'self_type = o#unknown
      
    method bool : bool -> 'self_type = function | false -> o | true -> o
      
    method unary_op : unary_op -> 'self_type =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `Name _x -> let o = o#name _x in o
      | `Abs -> o
      
    method typed_name : typed_name -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#name _x in let o = o#option (fun o -> o#unknown) _x_i1 in o
      
    method sentence' : sentence' -> 'self_type =
      function
      | `Definitions _x -> let o = o#list (fun o -> o#unknown) _x in o
      | `Expression _x -> let o = o#unknown _x in o
      | `Directive _x -> let o = o#directive _x in o
      
    method sentence : sentence -> 'self_type =
      function
      | `Definitions _x -> let o = o#list (fun o -> o#binding) _x in o
      | `Expression _x -> let o = o#phrase _x in o
      | `Directive _x -> let o = o#directive _x in o
      
    method sec : sec -> 'self_type =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `Project _x -> let o = o#name _x in o
      | `Name _x -> let o = o#name _x in o
      
    method row_var : row_var -> 'self_type =
      function
      | `Closed -> o
      | `Open _x -> let o = o#string _x in o
      | `Recursive ((_x, _x_i1)) ->
          let o = o#string _x in let o = o#row _x_i1 in o
      
    method row : row -> 'self_type =
      fun (_x, _x_i1) ->
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#string _x in let o = o#fieldspec _x_i1 in o)
            _x in
        let o = o#row_var _x_i1 in o
      
    method replace_rhs : replace_rhs -> 'self_type =
      function
      | `Literal _x -> let o = o#string _x in o
      | `Splice _x -> let o = o#phrase _x in o
      
    method regexflag : regexflag -> 'self_type =
      function
      | `RegexList -> o
      | `RegexNative -> o
      | `RegexGlobal -> o
      | `RegexReplace -> o
      
    method regex : regex -> 'self_type =
      function
      | `Range ((_x, _x_i1)) ->
          let o = o#char _x in let o = o#char _x_i1 in o
      | `Simply _x -> let o = o#string _x in o
      | `Quote _x -> let o = o#regex _x in o
      | `Any -> o
      | `StartAnchor -> o
      | `EndAnchor -> o
      | `Seq _x -> let o = o#list (fun o -> o#regex) _x in o
      | `Alternate ((_x, _x_i1)) ->
          let o = o#regex _x in let o = o#regex _x_i1 in o
      | `Group _x -> let o = o#regex _x in o
      | `Repeat ((_x, _x_i1)) ->
          let o = o#unknown _x in let o = o#regex _x_i1 in o
      | `Splice _x -> let o = o#phrase _x in o
      | `Replace ((_x, _x_i1)) ->
          let o = o#regex _x in let o = o#replace_rhs _x_i1 in o
      
    method quantifier : quantifier -> 'self_type =
      function
      | `TypeVar _x -> let o = o#string _x in o
      | `RigidTypeVar _x -> let o = o#string _x in o
      | `RowVar _x -> let o = o#string _x in o
      
    method position : position -> 'self_type =
      fun (_x, _x_i1) -> let o = o#unknown _x in let o = o#unknown _x_i1 in o
      
    method phrasenode : phrasenode -> 'self_type =
      function
      | `Constant _x -> let o = o#constant _x in o
      | `Var _x -> let o = o#name _x in o
      | `FunLit _x -> let o = o#funlit _x in o
      | `Spawn _x -> let o = o#phrase _x in o
      | `SpawnWait _x -> let o = o#phrase _x in o
      | `ListLit _x -> let o = o#list (fun o -> o#phrase) _x in o
      | `Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#list (fun o -> o#iterpatt) _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o = o#option (fun o -> o#phrase) _x_i3 in o
      | `Escape ((_x, _x_i1)) ->
          let o = o#typed_name _x in let o = o#phrase _x_i1 in o
      | `Section _x -> let o = o#sec _x in o
      | `Conditional ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `Block ((_x, _x_i1)) ->
          let o = o#list (fun o -> o#binding) _x in
          let o = o#phrase _x_i1 in o
      | `InfixAppl ((_x, _x_i1, _x_i2)) ->
          let o = o#binop _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `Regex _x -> let o = o#regex _x in o
      | `UnaryAppl ((_x, _x_i1)) ->
          let o = o#unary_op _x in let o = o#phrase _x_i1 in o
      | `FnAppl ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#phrase) _x_i1 in o
      | `TupleLit _x -> let o = o#list (fun o -> o#phrase) _x in o
      | `RecordLit ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#phrase) _x_i1 in o
      | `Projection ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#name _x_i1 in o
      | `With ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x_i1
          in o
      | `TypeAnnotation ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#datatype _x_i1 in o
      | `Upcast ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#datatype _x_i1 in let o = o#datatype _x_i2 in o
      | `ConstructorLit ((_x, _x_i1)) ->
          let o = o#name _x in
          let o = o#option (fun o -> o#phrase) _x_i1 in o
      | `Switch ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x_i1
          in o
      | `Receive _x ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x
          in o
      | `DatabaseLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            (fun (_x, _x_i1) ->
               let o = o#option (fun o -> o#phrase) _x in
               let o = o#option (fun o -> o#phrase) _x_i1 in o)
              _x_i1
          in o
      | `TableLit ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#phrase _x in
          let o = o#datatype _x_i1 in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in
                 let o = o#list (fun o -> o#fieldconstraint) _x_i1 in o)
              _x_i2 in
          let o = o#phrase _x_i3 in o
      | `DBDelete ((_x, _x_i1, _x_i2)) ->
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in o
      | `DBInsert ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#phrase _x_i1 in o
      | `DBUpdate ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x_i3
          in o
      | `Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#name _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in
                 let o = o#list (fun o -> o#phrase) _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o = o#list (fun o -> o#phrase) _x_i3 in o
      | `TextNode _x -> let o = o#string _x in o
      | `Formlet ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#phrase _x_i1 in o
      | `Page _x -> let o = o#phrase _x in o
      | `FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `PagePlacement _x -> let o = o#phrase _x in o
      | `FormBinding ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#pattern _x_i1 in o
      
    method phrase : phrase -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#phrasenode _x in let o = o#position _x_i1 in o
      
    method patternnode : patternnode -> 'self_type =
      function
      | `Any -> o
      | `Nil -> o
      | `Cons ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#pattern _x_i1 in o
      | `List _x -> let o = o#list (fun o -> o#pattern) _x in o
      | `Variant ((_x, _x_i1)) ->
          let o = o#string _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | `Record ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#pattern _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | `Tuple _x -> let o = o#list (fun o -> o#pattern) _x in o
      | `Constant _x -> let o = o#constant _x in o
      | `Variable _x -> let o = o#typed_name _x in o
      | `As ((_x, _x_i1)) ->
          let o = o#typed_name _x in let o = o#pattern _x_i1 in o
      | `HasType ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#datatype _x_i1 in o
      
    method pattern : pattern -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#patternnode _x in let o = o#position _x_i1 in o
      
    method operator : operator -> 'self_type =
      function
      | (#unary_op as x) -> o#unary_op x
      | (#binop as x) -> o#binop x
      | `Project _x -> let o = o#name _x in o
      
    method num : num -> 'self_type = o#unknown
      
    method name : name -> 'self_type = o#string
      
    method logical_binop : logical_binop -> 'self_type =
      function | `And -> o | `Or -> o
      
    method location : location -> 'self_type = o#unknown
      
    method iterpatt : iterpatt -> 'self_type =
      function
      | `List ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#phrase _x_i1 in o
      | `Table ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#phrase _x_i1 in o
      
    method funlit : funlit -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
        let o = o#phrase _x_i1 in o
      
    method fieldspec : fieldspec -> 'self_type =
      function | `Present _x -> let o = o#datatype _x in o | `Absent -> o
      
    method fieldconstraint : fieldconstraint -> 'self_type =
      function | `Readonly -> o
      
    method directive : directive -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#string _x in let o = o#list (fun o -> o#string) _x_i1 in o
      
    method datatype : datatype -> 'self_type =
      function
      | TypeVar _x -> let o = o#string _x in o
      | RigidTypeVar _x -> let o = o#string _x in o
      | FunctionType (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#datatype) _x in
          let o = o#datatype _x_i1 in let o = o#datatype _x_i2 in o
      | MuType (_x, _x_i1) ->
          let o = o#string _x in let o = o#datatype _x_i1 in o
      | UnitType -> o
      | TupleType _x -> let o = o#list (fun o -> o#datatype) _x in o
      | RecordType _x -> let o = o#row _x in o
      | VariantType _x -> let o = o#row _x in o
      | TableType (_x, _x_i1) ->
          let o = o#datatype _x in let o = o#datatype _x_i1 in o
      | ListType _x -> let o = o#datatype _x in o
      | TypeApplication _x ->
          let o =
            (fun (_x, _x_i1) ->
               let o = o#string _x in
               let o = o#list (fun o -> o#datatype) _x_i1 in o)
              _x
          in o
      | PrimitiveType _x -> let o = o#unknown _x in o
      | DBType -> o
      
    method constant : constant -> 'self_type =
      function
      | `Float _x -> let o = o#float _x in o
      | `Int _x -> let o = o#num _x in o
      | `String _x -> let o = o#string _x in o
      | `Bool _x -> let o = o#bool _x in o
      | `Char _x -> let o = o#char _x in o
      
    method binop : binop -> 'self_type =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `RegexMatch _x -> let o = o#list (fun o -> o#regexflag) _x in o
      | (#logical_binop as x) -> o#logical_binop x
      | `Cons -> o
      | `Name _x -> let o = o#name _x in o
      | `App -> o
      
    method bindingnode : bindingnode -> 'self_type =
      function
      | `Val ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#location _x_i2 in
          let o = o#option (fun o -> o#datatype) _x_i3 in o
      | `Fun ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#typed_name _x in
          let o = o#funlit _x_i1 in
          let o = o#location _x_i2 in
          let o = o#option (fun o -> o#datatype) _x_i3 in o
      | `Funs _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let o = o#typed_name _x in
                 let o = o#funlit _x_i1 in
                 let o = o#location _x_i2 in
                 let o = o#option (fun o -> o#datatype) _x_i3 in o)
              _x
          in o
      | `Foreign ((_x, _x_i1, _x_i2)) ->
          let o = o#name _x in
          let o = o#name _x_i1 in let o = o#datatype _x_i2 in o
      | `Include _x -> let o = o#string _x in o
      | `Type ((_x, _x_i1, _x_i2)) ->
          let o = o#name _x in
          let o = o#list (fun o -> o#name) _x_i1 in
          let o = o#datatype _x_i2 in o
      | `Infix -> o
      | `Exp _x -> let o = o#phrase _x in o
      
    method binding : binding -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#bindingnode _x in let o = o#position _x_i1 in o
      
    method assumption : assumption -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#list (fun o -> o#quantifier) _x in
        let o = o#datatype _x_i1 in o
      
    method program : program -> 'self_type = 
      fun (bindings, phrase) ->
        let o = o#list (fun o -> o#binding) bindings in
        let o = o#option (fun o -> o#phrase) phrase in
          o


    method unknown : 'a. 'a -> 'self_type = fun _ -> o
      
  end
  
