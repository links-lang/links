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

    method int : int -> int = o#unknown

    method float : float -> float = o#unknown

    method char : char -> char = o#unknown

    method bool : bool -> bool = function | false -> false | true -> true

    method unary_op : unary_op -> unary_op =
      function
      | `Minus -> `Minus
      | `FloatMinus -> `FloatMinus
      | `Name _x -> let _x = o#name _x in `Name _x

    method tyunary_op : tyarg list * unary_op -> tyarg list * unary_op =
      fun (_x, _x_i1) -> (_x, o#unary_op _x_i1)

    method binder : binder -> binder =
      fun (_x, _x_i1, _x_i2) ->
        let _x = o#name _x in
        let _x_i1 = o#option (fun o -> o#unknown) _x_i1 in
        let _x_i2 = o#position _x_i2 in (_x, _x_i1, _x_i2)

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

    method subkind : subkind -> subkind = fun x -> x

    method kind : kind -> kind = fun x -> x

    method freedom : freedom -> freedom = fun x -> x

    method type_variable : type_variable -> type_variable =
      fun (_x, _x_i1, _x_i2) ->
        let _x = o#name _x in
        let _x_i1 = o#kind _x_i1 in
        let _x_i2 = o#freedom _x_i2 in (_x, _x_i1, _x_i2)

    method known_type_variable : known_type_variable -> known_type_variable =
      fun (_x, _x_i1, _x_i2) ->
        let _x = o#name _x in
        let _x_i1 = o#option (fun o -> o#subkind) _x_i1 in
        let _x_i2 = o#freedom _x_i2 in (_x, _x_i1, _x_i2)

    method row_var : row_var -> row_var =
      function
      | `Closed -> `Closed
      | `Open _x ->
          let _x = o#known_type_variable _x in `Open _x
      | `Recursive ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#row _x_i1 in `Recursive ((_x, _x_i1))

    method row : row -> row =
      fun (_x, _x_i1) ->
        let _x =
          o#list
            (fun o (_x, _x_i1) ->
               let _x = o#name _x in
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

    method position : position -> position =
      fun (_x, _x_i1, _x_i2) ->
        let _x = o#unknown _x in
        let _x_i1 = o#unknown _x_i1 in
        let _x_i2 = o#unknown _x_i2 in (_x, _x_i1, _x_i2)

    method datatype' : datatype' -> datatype' =
      fun (x, y) ->
        let x = o#datatype x in
        let y = o#unknown y in
          (x,y)

    method phrasenode : phrasenode -> phrasenode =
      function
      | `Constant _x -> let _x = o#constant _x in `Constant _x
      | `Var _x -> let _x = o#name _x in `Var _x
      | `FunLit (_x, _x1, _x_i1, _x_i2) -> let _x_i1 = o#funlit _x_i1 in
                                           let _x_i2 = o#location _x_i2 in `FunLit (_x, _x1, _x_i1, _x_i2)
      | `Spawn (_x, _x_i1, _x_i2, _x_i3) -> let _x_i1 = o#location _x_i1 in
                                            let _x_i2 = o#phrase _x_i2 in `Spawn (_x, _x_i1, _x_i2, _x_i3)
      | `Query (_x, _x_i1, _x_i2) ->
          let _x =
            o#option
              (fun o (_x, _x_i1) ->
                 let _x = o#phrase _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#phrase _x_i1 in `Query (_x, _x_i1, _x_i2)
      | `ListLit (_x, _x_i1) ->
          let _x = o#list (fun o -> o#phrase) _x in `ListLit (_x, _x_i1)
      | `Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#list (fun o -> o#iterpatt) _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 = o#option (fun o -> o#phrase) _x_i3
          in `Iteration ((_x, _x_i1, _x_i2, _x_i3))
      | `Escape ((_x, _x_i1)) ->
          let _x = o#binder _x in
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
          let _x = o#tybinop _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in `InfixAppl ((_x, _x_i1, _x_i2))
      | `RangeLit ((_x_i1, _x_i2)) ->
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in `RangeLit ((_x_i1, _x_i2))
      | `Regex _x -> let _x = o#regex _x in `Regex _x
      | `UnaryAppl ((_x, _x_i1)) ->
          let _x = o#tyunary_op _x in
          let _x_i1 = o#phrase _x_i1 in `UnaryAppl ((_x, _x_i1))
      | `FnAppl ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#phrase) _x_i1
          in `FnAppl ((_x, _x_i1))
      | `TAbstr ((_x, _x_i1)) ->
          let _x_i1 = o#phrase _x_i1 in `TAbstr ((_x, _x_i1))
      | `TAppl ((_x, _x_i1)) ->
          let _x = o#phrase _x in `TAppl ((_x, _x_i1))
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
          let _x_i1 = o#datatype' _x_i1 in `TypeAnnotation ((_x, _x_i1))
      | `Upcast ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype' _x_i1 in
          let _x_i2 = o#datatype' _x_i2 in `Upcast ((_x, _x_i1, _x_i2))
      | `ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#name _x in
          let _x_i1 = o#option (fun o -> o#phrase) _x_i1
          in `ConstructorLit ((_x, _x_i1, _x_i2))
      | `Switch ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2
          in `Switch ((_x, _x_i1, _x_i2))
      | `Receive ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#unknown) _x_i1
          in `Receive (_x, _x_i1)
      (* | `Fuse ((_x, _x_i1)) -> *)
      (*     let _x = o#phrase _x in *)
      (*     let _x_i1 = o#phrase _x_i1 in `Fuse ((_x, _x_i1)) *)
      | `Select ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#phrase _x_i1
          in `Select (_x, _x_i1)
      | `Offer ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2
          in `Offer (_x, _x_i1, _x_i2)
      | `CP p -> `CP (o#cp_phrase p)
      | `DatabaseLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            (fun (_x, _x_i1) ->
               let _x = o#option (fun o -> o#phrase) _x in
               let _x_i1 = o#option (fun o -> o#phrase) _x_i1 in (_x, _x_i1))
              _x_i1
          in `DatabaseLit ((_x, _x_i1))
      | `TableLit ((_x, (y, z), _x_i2, _x_i3, _x_i4)) ->
          let _x = o#phrase _x in
          let y = o#datatype y in
          let z = o#option
            (fun o r ->
               let r = o#unknown r in
                 r) z in
          let _x_i2 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#list (fun o -> o#fieldconstraint) _x_i1
                 in (_x, _x_i1))
              _x_i2 in
          let _x_i3 = o#phrase _x_i3 in 
	  let _x_i4 = o#phrase _x_i4 in `TableLit ((_x, (y, z), _x_i2, _x_i3, _x_i4))
      | `DBDelete ((_x, _x_i1, _x_i2)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2
          in `DBDelete ((_x, _x_i1, _x_i2))
      | `DBInsert ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#name) _x_i1 in
          let _x_i2 = o#phrase _x_i2 in
          let _x_i3 = o#option (fun o -> o#phrase) _x_i3 in `DBInsert ((_x, _x_i1, _x_i2, _x_i3))
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
                 let _x = o#name _x in
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

    method cp_phrasenode : cp_phrasenode -> cp_phrasenode =
      function
      | `Unquote (bs, e) -> `Unquote (o#list (fun o -> o#binding) bs, o#phrase e)
      | `Grab (c, x, p) -> `Grab (c, x, o#cp_phrase p)
      | `Give (c, e, p) -> `Give (c, o#option (fun o -> o#phrase) e, o#cp_phrase p)
      | `GiveNothing c -> `GiveNothing (o#binder c)
      | `Select (c, l, p) -> `Select (c, l, o#cp_phrase p)
      | `Offer (c, bs) -> `Offer (c, o#list (fun o (l, p) -> (l, o#cp_phrase p)) bs)
      | `Fuse (c, d) -> `Fuse (c, d)
      | `Comp (c, p, q) -> `Comp (c, o#cp_phrase p, o#cp_phrase q)

    method cp_phrase : cp_phrase -> cp_phrase =
      fun (p, pos) -> (o#cp_phrasenode p, o#position pos)

    method patternnode : patternnode -> patternnode =
      function
      | `Any -> `Any
      | `Nil -> `Nil
      | `Cons ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#pattern _x_i1 in `Cons ((_x, _x_i1))
      | `List _x -> let _x = o#list (fun o -> o#pattern) _x in `List _x
      | `Variant ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in `Variant ((_x, _x_i1))
      | `Negative _x ->
          let _x = o#list (fun o -> o#name) _x
          in `Negative _x
      | `Record ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#pattern _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in `Record ((_x, _x_i1))
      | `Tuple _x -> let _x = o#list (fun o -> o#pattern) _x in `Tuple _x
      | `Constant _x -> let _x = o#constant _x in `Constant _x
      | `Variable _x -> let _x = o#binder _x in `Variable _x
      | `As ((_x, _x_i1)) ->
          let _x = o#binder _x in
          let _x_i1 = o#pattern _x_i1 in `As ((_x, _x_i1))
      | `HasType ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#datatype' _x_i1 in `HasType ((_x, _x_i1))

    method pattern : pattern -> pattern =
      fun (_x, _x_i1) ->
        let _x = o#patternnode _x in
        let _x_i1 = o#position _x_i1 in (_x, _x_i1)

    method operator : operator -> operator =
      function
      | (#unary_op as x) -> (o#unary_op x :> operator)
      | (#binop as x) -> (o#binop x :> operator)
      | `Project _x -> let _x = o#name _x in `Project _x

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
      | `Var _x -> let _x = o#known_type_variable _x in `Var _x

    method fieldconstraint : fieldconstraint -> fieldconstraint =
      function | `Readonly -> `Readonly | `Default -> `Default

    method directive : directive -> directive =
      fun (_x, _x_i1) ->
        let _x = o#string _x in
        let _x_i1 = o#list (fun o -> o#string) _x_i1 in (_x, _x_i1)

    method datatype : datatype -> datatype =
      function
      | `TypeVar _x ->
          let _x = o#known_type_variable _x in `TypeVar _x
      | `Function (_x, _x_i1, _x_i2) ->
          let _x = o#list (fun o -> o#datatype) _x in
          let _x_i1 = o#row _x_i1 in
          let _x_i2 = o#datatype _x_i2 in `Function (_x, _x_i1, _x_i2)
      | `Lolli (_x, _x_i1, _x_i2) ->
          let _x = o#list (fun o -> o#datatype) _x in
          let _x_i1 = o#row _x_i1 in
          let _x_i2 = o#datatype _x_i2 in `Lolli (_x, _x_i1, _x_i2)
      | `Mu (_x, _x_i1) ->
          let _x = o#name _x in
          let _x_i1 = o#datatype _x_i1 in `Mu (_x, _x_i1)
      | `Forall (_x, _x_i1) ->
          let _x = _x in (*o#list (fun o -> o#quantifier) _x in*)
          let _x_i1 = o#datatype _x_i1 in `Forall (_x, _x_i1)
      | `Unit -> `Unit
      | `Tuple _x ->
          let _x = o#list (fun o -> o#datatype) _x in `Tuple _x
      | `Record _x -> let _x = o#row _x in `Record _x
      | `Variant _x -> let _x = o#row _x in `Variant _x
      | `Table (_x, _x_i1, _x_i2) ->
         let _x = o#datatype _x in
         let _x_i1 = o#datatype _x_i1 in
         let _x_i2 = o#datatype _x_i2 in `Table (_x, _x_i1, _x_i2)
      | `List _x -> let _x = o#datatype _x in `List _x
      | `TypeApplication _x ->
          let _x =
            (fun (_x, _x_i1) ->
               let _x = o#name _x in
               let _x_i1 = o#list (fun o -> o#type_arg) _x_i1 in (_x, _x_i1))
              _x
          in `TypeApplication _x
      | `Primitive _x -> let _x = o#unknown _x in `Primitive _x
      | `DB -> `DB
      | `Input (_x, _x_i1) ->
        let _x = o#datatype _x in
        let _x_i1 = o#datatype _x_i1 in `Input (_x, _x_i1)
      | `Output (_x, _x_i1) ->
        let _x = o#datatype _x in
        let _x_i1 = o#datatype _x_i1 in `Output (_x, _x_i1)
      | `Select _x ->
        let _x = o#row _x in `Select _x
      | `Choice _x ->
        let _x = o#row _x in `Choice _x
      | `Dual _x ->
        let _x = o#datatype _x in `Dual _x
      | `End -> `End

    method type_arg : type_arg -> type_arg =
      function
      | `Type _x -> let _x = o#datatype _x in `Type _x
      | `Row _x -> let _x = o#row _x in `Row _x
      | `Presence _x -> let _x = o#fieldspec _x in `Presence _x

    method constant : constant -> constant =
      function
      | `Float _x -> let _x = o#float _x in `Float _x
      | `Int _x -> let _x = o#int _x in `Int _x
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

    method tybinop : tyarg list * binop -> tyarg list * binop =
      fun (_x, _x_i1) -> (_x, o#binop _x_i1)

    method bindingnode : bindingnode -> bindingnode =
      function
      | `Val ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let _x_i1 = o#pattern _x_i1 in
          let _x_i2 = o#phrase _x_i2 in
          let _x_i3 = o#location _x_i3 in
          let _x_i4 = o#option (fun o -> o#datatype') _x_i4
          in `Val ((_x, _x_i1, _x_i2, _x_i3, _x_i4))
      | `Fun ((_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let _x = o#binder _x in
          let _x_i2 = o#funlit _x_i2 in
          let _x_i3 = o#location _x_i3 in
          let _x_i4 = o#option (fun o -> o#datatype') _x_i4
          in `Fun ((_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4))
      | `Funs _x ->
          let _x =
            o#list
              (fun o (_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4, _x_i5) ->
                 let _x = o#binder _x in
                 let _x_i2 = o#funlit _x_i2 in
                 let _x_i3 = o#location _x_i3 in
                 let _x_i4 = o#option (fun o -> o#datatype') _x_i4 in
                 let _x_i5 = o#position _x_i5
                 in (_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4, _x_i5))
              _x
          in `Funs _x
      | `Foreign ((_x, _x_i1, _x_i2)) ->
          let _x = o#binder _x in
          let _x_i1 = o#name _x_i1 in
          let _x_i2 = o#datatype' _x_i2 in `Foreign ((_x, _x_i1, _x_i2))
      | `Import _x -> let _x = o#string _x in `Import _x
      | `Type ((_x, _x_i1, _x_i2)) ->
          let _x = o#name _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = _x (*o#quantifier _x*) in
                 let _x_i1 = o#unknown _x_i1
                 in (_x, _x_i1))
              _x_i1
          in let _x_i2 = o#datatype' _x_i2 in `Type ((_x, _x_i1, _x_i2))
      | `Infix -> `Infix
      | `Exp _x -> let _x = o#phrase _x in `Exp _x
      | `Module (n, p) ->
          let n = o#name n in
          let p = o#phrase p in
          `Module (n, p)

    method binding : binding -> binding =
      fun (_x, _x_i1) ->
        let _x = o#bindingnode _x in
        let _x_i1 = o#position _x_i1 in (_x, _x_i1)

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

    method int : int -> 'self_type = o#unknown

    method float : float -> 'self_type = o#unknown

    method char : char -> 'self_type = o#unknown

    method bool : bool -> 'self_type = function | false -> o | true -> o

    method unary_op : unary_op -> 'self_type =
      function
      | `Minus -> o
      | `FloatMinus -> o
      | `Name _x -> let o = o#name _x in o

    method tyunary_op : tyarg list * unary_op -> 'self_type =
      fun (_x, _x_i1) -> o#unary_op _x_i1

    method binder : binder -> 'self_type =
      fun (_x, _x_i1, _x_i2) ->
        let o = o#name _x in
        let o = o#option (fun o -> o#unknown) _x_i1 in
        let o = o#position _x_i2 in o

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

    method subkind : subkind -> 'self_type = fun x -> o

    method kind : kind -> 'self_type = fun x -> o

    method freedom : freedom -> 'self_type = fun x -> o

    method type_variable : type_variable -> 'self_type =
      fun (_x, _x_i1, _x_i2) ->
        let o = o#name _x in
        let o = o#kind _x_i1 in
        let o = o#freedom _x_i2 in o

    method known_type_variable : known_type_variable -> 'self_type =
      fun (_x, _x_i1, _x_i2) ->
        let o = o#name _x in
        let o = o#option (fun o -> o#subkind) _x_i1 in
        let o = o#freedom _x_i2 in o

    method row_var : row_var -> 'self_type =
      function
      | `Closed -> o
      | `Open _x ->
          let o = o#known_type_variable _x in o
      | `Recursive ((_x, _x_i1)) ->
          let o = o#name _x in let o = o#row _x_i1 in o

    method row : row -> 'self_type =
      fun (_x, _x_i1) ->
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#name _x in let o = o#fieldspec _x_i1 in o)
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

    method position : position -> 'self_type =
      fun (_x, _x_i1, _x_i2) ->
        let o = o#unknown _x in
        let o = o#unknown _x_i1 in
        let o = o#unknown _x_i2 in o

    method datatype' : datatype' -> 'self_type =
      fun (x, y) ->
        let o = o#datatype x in
        let o = o#unknown y in
          o

    method phrasenode : phrasenode -> 'self_type =
      function
      | `Constant _x -> let o = o#constant _x in o
      | `Var _x -> let o = o#name _x in o
      | `FunLit (_x, _x1, _x_i1, _x_i2) -> let o = o#funlit _x_i1 in let _x_i2 = o#location _x_i2 in o
      | `Spawn (_x, _x_i1, _x_i2, _x_i3) -> let o = o#location _x_i1 in let o = o#phrase _x_i2 in o
      | `Query (_x, _x_i1, _x_i2) ->
          let o =
            o#option
              (fun o (_x, _x_i1) ->
                 let o = o#phrase _x in
                 let o = o#phrase _x_i1 in o)
              _x in
          let o = o#phrase _x_i1 in o
      | `ListLit (_x, _x_i1) -> let o = o#list (fun o -> o#phrase) _x in o
      | `Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#list (fun o -> o#iterpatt) _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o = o#option (fun o -> o#phrase) _x_i3 in o
      | `Escape ((_x, _x_i1)) ->
          let o = o#binder _x in let o = o#phrase _x_i1 in o
      | `Section _x -> let o = o#sec _x in o
      | `Conditional ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `Block ((_x, _x_i1)) ->
          let o = o#list (fun o -> o#binding) _x in
          let o = o#phrase _x_i1 in o
      | `InfixAppl ((_x, _x_i1, _x_i2)) ->
          let o = o#tybinop _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `RangeLit ((_x_i1, _x_i2)) ->
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | `Regex _x -> let o = o#regex _x in o
      | `UnaryAppl ((_x, _x_i1)) ->
          let o = o#tyunary_op _x in let o = o#phrase _x_i1 in o
      | `FnAppl ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#phrase) _x_i1 in o
      | `TAbstr ((_x, _x_i1)) ->
          let o = o#list (fun o -> o#tyvar) (Types.unbox_quantifiers _x) in
          let o = o#phrase _x_i1 in o
      | `TAppl ((_x, _x_i1)) ->
          let o = o#phrase _x in o
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
          let o = o#phrase _x in let o = o#datatype' _x_i1 in o
      | `Upcast ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#datatype' _x_i1 in let o = o#datatype' _x_i2 in o
      | `ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let o = o#name _x in
          let o = o#option (fun o -> o#phrase) _x_i1 in o
      | `Switch ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2
          in o
      | `Receive ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#unknown) _x_i1
          in o
      (* | `Fuse ((_x, _x_i1)) -> *)
      (*     let o = o#phrase _x in *)
      (*     let o = o#phrase _x_i1 *)
      (*     in o *)
      | `Select ((_x, _x_i1)) ->
          let o = o#name _x in
          let o = o#phrase _x_i1
          in o
      | `Offer ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2
          in o
      | `CP p -> o#cp_phrase p
      | `DatabaseLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            (fun (_x, _x_i1) ->
               let o = o#option (fun o -> o#phrase) _x in
               let o = o#option (fun o -> o#phrase) _x_i1 in o)
              _x_i1
          in o
      | `TableLit ((_x, (y,z), _x_i2, _x_i3, _x_i4)) ->
          let o = o#phrase _x in
          let o = o#datatype y in
          let o = o#option
            (fun o r ->
               let o = o#unknown r in
                 o) z in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in
                 let o = o#list (fun o -> o#fieldconstraint) _x_i1 in o)
              _x_i2 in
          let o = o#phrase _x_i3 in 
	  let o = o#phrase _x_i4 in
	    o
      | `DBDelete ((_x, _x_i1, _x_i2)) ->
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in o
      | `DBInsert ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#name) _x_i1 in
          let o = o#phrase _x_i2 in let o = o#option (fun o -> o#phrase) _x_i3 in o
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
                 let o = o#name _x in
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

    method cp_phrasenode : cp_phrasenode -> 'self_type =
      function
      | `Unquote (bs, e) -> (o#list (fun o -> o#binding) bs)#phrase e
      | `Grab (_c, _x, p) -> o#cp_phrase p
      | `Give (_c, e, p) -> (o#option (fun o -> o#phrase) e)#cp_phrase p
      | `GiveNothing c -> o#binder c
      | `Select (_c, _l, p) -> o#cp_phrase p
      | `Offer (_c, bs) -> o#list (fun o (_l, b) -> o#cp_phrase b) bs
      | `Fuse (_c, _d) -> o
      | `Comp (_c, p, q) -> (o#cp_phrase p)#cp_phrase q

    method cp_phrase : cp_phrase -> 'self_node =
      fun (p, pos) -> (o#cp_phrasenode p)#position pos

    method patternnode : patternnode -> 'self_type =
      function
      | `Any -> o
      | `Nil -> o
      | `Cons ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#pattern _x_i1 in o
      | `List _x -> let o = o#list (fun o -> o#pattern) _x in o
      | `Variant ((_x, _x_i1)) ->
          let o = o#name _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | `Negative _x ->
          let o = o#list (fun o -> o#name) _x in o
      | `Record ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#pattern _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | `Tuple _x -> let o = o#list (fun o -> o#pattern) _x in o
      | `Constant _x -> let o = o#constant _x in o
      | `Variable _x -> let o = o#binder _x in o
      | `As ((_x, _x_i1)) ->
          let o = o#binder _x in let o = o#pattern _x_i1 in o
      | `HasType ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#datatype' _x_i1 in o

    method pattern : pattern -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#patternnode _x in let o = o#position _x_i1 in o

    method operator : operator -> 'self_type =
      function
      | (#unary_op as x) -> o#unary_op x
      | (#binop as x) -> o#binop x
      | `Project _x -> let o = o#name _x in o

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
      function
      | `Present _x -> let o = o#datatype _x in o
      | `Absent -> o
      | `Var _x -> let o = o#known_type_variable _x in o

    method fieldconstraint : fieldconstraint -> 'self_type =
      function | `Readonly -> o | `Default -> o

    method directive : directive -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#string _x in let o = o#list (fun o -> o#string) _x_i1 in o

    method tyvar : tyvar -> 'self_type = fun _ -> o

    method datatype : datatype -> 'self_type =
      function
      | `TypeVar _x ->
          let o = o#known_type_variable _x in o
      | `Function (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#datatype) _x in
          let o = o#row _x_i1 in let o = o#datatype _x_i2 in o
      | `Lolli (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#datatype) _x in
          let o = o#row _x_i1 in let o = o#datatype _x_i2 in o
      | `Mu (_x, _x_i1) ->
          let o = o#name _x in let o = o#datatype _x_i1 in o
      | `Forall (_x, _x_i1) ->
          let o = o (*o#list (fun o -> o#quantifier) _x*) in let o = o#datatype _x_i1 in o
      | `Unit -> o
      | `Tuple _x -> let o = o#list (fun o -> o#datatype) _x in o
      | `Record _x -> let o = o#row _x in o
      | `Variant _x -> let o = o#row _x in o
      | `Table (_x, _x_i1, _x_i2) ->
          let o = o#datatype _x in let o = o#datatype _x_i1 in let o = o#datatype _x_i2 in o
      | `List _x -> let o = o#datatype _x in o
      | `TypeApplication _x ->
          let o =
            (fun (_x, _x_i1) ->
               let o = o#name _x in
               let o = o#list (fun o -> o#type_arg) _x_i1 in o)
              _x
          in o
      | `Primitive _x -> let o = o#unknown _x in o
      | `DB -> o
      | `Input (_x, _x_i1) ->
        let o = o#datatype _x in
        let o = o#datatype _x_i1 in o
      | `Output (_x, _x_i1) ->
        let o = o#datatype _x in
        let o = o#datatype _x_i1 in o
      | `Select _x ->
        let o = o#row _x in o
      | `Choice _x ->
        let o = o#row _x in o
      | `Dual _x ->
        let o = o#datatype _x in o
      | `End -> o

    method type_arg : type_arg -> 'self_type =
      function
      | `Type _x -> let o = o#datatype _x in o
      | `Row _x -> let o = o#row _x in o
      | `Presence _x -> let o = o#fieldspec _x in o

    method constant : constant -> 'self_type =
      function
      | `Float _x -> let o = o#float _x in o
      | `Int _x -> let o = o#int _x in o
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

    method tybinop : tyarg list * binop -> 'self_type =
      fun (_x, _x_i1) -> o#binop _x_i1

    method bindingnode : bindingnode -> 'self_type =
      function
      | `Val ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let o = o#list (fun o -> o#tyvar) _x in
          let o = o#pattern _x_i1 in
          let o = o#phrase _x_i2 in
          let o = o#location _x_i3 in
          let o = o#option (fun o -> o#datatype') _x_i4 in o
      | `Fun ((_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let o = o#binder _x in
          let o = o#list (fun o -> o#tyvar) _x_i1 in
          let o = o#funlit _x_i2 in
          let o = o#location _x_i3 in
          let o = o#option (fun o -> o#datatype') _x_i4 in o
      | `Funs _x ->
          let o =
            o#list
              (fun o (_x, _x1, ((_x_i1, _), _x_i2), _x_i3, _x_i4, _x_i5) ->
                 let o = o#binder _x in
                 let o = o#list (fun o -> o#tyvar) _x_i1 in
                 let o = o#funlit _x_i2 in
                 let o = o#location _x_i3 in
                 let o = o#option (fun o -> o#datatype') _x_i4 in
                 let o = o#position _x_i5 in o)
              _x
          in o
      | `Foreign ((_x, _x_i1, _x_i2)) ->
          let o = o#binder _x in
          let o = o#name _x_i1 in let o = o#datatype' _x_i2 in o
      | `Import _x -> let o = o#string _x in o
      | `Type ((_x, _x_i1, _x_i2)) ->
          let o = o#name _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o (* #quantifier _x*) in
                 let o = o#unknown _x_i1
                 in o)
              _x_i1
          in let o = o#datatype' _x_i2 in o
      | `Infix -> o
      | `Exp _x -> let o = o#phrase _x in o
      | `Module (n, p) ->
          let o = o#name n in
          let o = o#phrase p in
          o

    method binding : binding -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#bindingnode _x in let o = o#position _x_i1 in o

    method program : program -> 'self_type =
      fun (bindings, phrase) ->
        let o = o#list (fun o -> o#binding) bindings in
        let o = o#option (fun o -> o#phrase) phrase in
          o


    method unknown : 'a. 'a -> 'self_type = fun _ -> o

  end

class virtual predicate =
object
  inherit fold
  method virtual satisfied : bool
end

class fold_map =
  object ((o : 'self_type))
    method string : string -> ('self_type * string) = o#unknown

    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a option -> ('self_type * ('a option)) =
      fun _f_a ->
        function
        | None -> (o, None)
        | Some _x -> let (o, _x) = _f_a o _x in (o, (Some _x))

    method list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a list -> ('self_type * ('a list)) =
      fun _f_a ->
        function
        | [] -> (o, [])
        | _x :: _x_i1 ->
            let (o, _x) = _f_a o _x in
            let (o, _x_i1) = o#list _f_a _x_i1 in (o, (_x :: _x_i1))

    method int : int -> ('self_type * int) = o#unknown

    method float : float -> ('self_type * float) = o#unknown

    method char : char -> ('self_type * char) = o#unknown

    method bool : bool -> ('self_type * bool) =
      function | false -> (o, false) | true -> (o, true)

    method unary_op : unary_op -> ('self_type * unary_op) =
      function
      | `Minus -> (o, `Minus)
      | `FloatMinus -> (o, `FloatMinus)
      | `Name _x -> let (o, _x) = o#name _x in (o, (`Name _x))

    method tyunary_op : tyarg list * unary_op -> 'self_type * (tyarg list * unary_op) =
      fun (_x, _x_i1) ->
        let (o, _x_i1) = o#unary_op _x_i1 in (o, (_x, _x_i1))

    method sentence : sentence -> ('self_type * sentence) =
      function
      | `Definitions _x ->
          let (o, _x) = o#list (fun o -> o#binding) _x
          in (o, (`Definitions _x))
      | `Expression _x -> let (o, _x) = o#phrase _x in (o, (`Expression _x))
      | `Directive _x -> let (o, _x) = o#directive _x in (o, (`Directive _x))

    method sec : sec -> ('self_type * sec) =
      function
      | `Minus -> (o, `Minus)
      | `FloatMinus -> (o, `FloatMinus)
      | `Project _x -> let (o, _x) = o#name _x in (o, (`Project _x))
      | `Name _x -> let (o, _x) = o#name _x in (o, (`Name _x))

    method subkind : subkind -> ('self_type * subkind) = fun k -> (o, k)

    method kind : kind -> ('self_type * kind) = fun k -> (o, k)

    method freedom : freedom -> ('self_type * freedom) = fun k -> (o, k)

    method type_variable : type_variable -> ('self_type * type_variable) =
      fun (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#name _x in
        let (o, _x_i1) = o#kind _x_i1 in
        let (o, _x_i2) = o#freedom _x_i2 in (o, (_x, _x_i1, _x_i2))

    method known_type_variable : known_type_variable -> ('self_type * known_type_variable) =
      fun (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#name _x in
        let (o, _x_i1) = o#option (fun o -> o#subkind) _x_i1 in
        let (o, _x_i2) = o#freedom _x_i2 in (o, (_x, _x_i1, _x_i2))

    method row_var : row_var -> ('self_type * row_var) =
      function
      | `Closed -> (o, `Closed)
      | `Open _x ->
          let (o, _x) = o#known_type_variable _x in (o, (`Open _x))
      | `Recursive ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#row _x_i1 in (o, (`Recursive ((_x, _x_i1))))

    method row : row -> ('self_type * row) =
      fun (_x, _x_i1) ->
        let (o, _x) =
          o#list
            (fun o (_x, _x_i1) ->
               let (o, _x) = o#string _x in
               let (o, _x_i1) = o#fieldspec _x_i1 in (o, (_x, _x_i1)))
            _x in
        let (o, _x_i1) = o#row_var _x_i1 in (o, (_x, _x_i1))

    method replace_rhs : replace_rhs -> ('self_type * replace_rhs) =
      function
      | `Literal _x -> let (o, _x) = o#string _x in (o, (`Literal _x))
      | `Splice _x -> let (o, _x) = o#phrase _x in (o, (`Splice _x))

    method regexflag : regexflag -> ('self_type * regexflag) =
      function
      | `RegexList -> (o, `RegexList)
      | `RegexNative -> (o, `RegexNative)
      | `RegexGlobal -> (o, `RegexGlobal)
      | `RegexReplace -> (o, `RegexReplace)

    method regex : regex -> ('self_type * regex) =
      function
      | `Range ((_x, _x_i1)) ->
          let (o, _x) = o#char _x in
          let (o, _x_i1) = o#char _x_i1 in (o, (`Range ((_x, _x_i1))))
      | `Simply _x -> let (o, _x) = o#string _x in (o, (`Simply _x))
      | `Quote _x -> let (o, _x) = o#regex _x in (o, (`Quote _x))
      | `Any -> (o, `Any)
      | `StartAnchor -> (o, `StartAnchor)
      | `EndAnchor -> (o, `EndAnchor)
      | `Seq _x ->
          let (o, _x) = o#list (fun o -> o#regex) _x in (o, (`Seq _x))
      | `Alternate ((_x, _x_i1)) ->
          let (o, _x) = o#regex _x in
          let (o, _x_i1) = o#regex _x_i1 in (o, (`Alternate ((_x, _x_i1))))
      | `Group _x -> let (o, _x) = o#regex _x in (o, (`Group _x))
      | `Repeat ((_x, _x_i1)) ->
          let (o, _x) = o#unknown _x in
          let (o, _x_i1) = o#regex _x_i1 in (o, (`Repeat ((_x, _x_i1))))
      | `Splice _x -> let (o, _x) = o#phrase _x in (o, (`Splice _x))
      | `Replace ((_x, _x_i1)) ->
          let (o, _x) = o#regex _x in
          let (o, _x_i1) = o#replace_rhs _x_i1
          in (o, (`Replace ((_x, _x_i1))))

    method program : program -> ('self_type * program) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#binding) _x in
        let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
        in (o, (_x, _x_i1))

    method position : position -> ('self_type * position) =
      fun (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#unknown _x in
        let (o, _x_i1) = o#unknown _x_i1 in
        let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2
        in (o, (_x, _x_i1, _x_i2))

    method phrasenode : phrasenode -> ('self_type * phrasenode) =
      function
      | `Constant _x -> let (o, _x) = o#constant _x in (o, (`Constant _x))
      | `Var _x -> let (o, _x) = o#name _x in (o, (`Var _x))
      | `FunLit (_x, _x1, _x_i1, _x_i2) ->
        let (o, _x_i1) = o#funlit _x_i1 in
        let (o, _x_i2) = o#location _x_i2 in (o, (`FunLit (_x, _x1, _x_i1, _x_i2)))
      | `Spawn (_x, _x_i1, _x_i2, _x_i3) -> let (o, _x_i1) = o#location _x_i1 in
                                            let (o, _x_i2) = o#phrase _x_i2 in (o, (`Spawn (_x, _x_i1, _x_i2, _x_i3)))
      | `Query (_x, _x_i1, _x_i2) ->
          let (o, _x) =
            o#option
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#phrase _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`Query (_x, _x_i1, _x_i2)))
      | `ListLit (_x, _x_i1) ->
          let (o, _x) = o#list (fun o -> o#phrase) _x in (o, (`ListLit (_x, _x_i1)))
      | `RangeLit ((_x_i1, _x_i2)) ->
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (`RangeLit ((_x_i1, _x_i2))))
      | `Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#list (fun o -> o#iterpatt) _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2 in
          let (o, _x_i3) = o#option (fun o -> o#phrase) _x_i3
          in (o, (`Iteration ((_x, _x_i1, _x_i2, _x_i3))))
      | `Escape ((_x, _x_i1)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`Escape ((_x, _x_i1))))
      | `Section _x -> let (o, _x) = o#sec _x in (o, (`Section _x))
      | `Conditional ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (`Conditional ((_x, _x_i1, _x_i2))))
      | `Block ((_x, _x_i1)) ->
          let (o, _x) = o#list (fun o -> o#binding) _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`Block ((_x, _x_i1))))
      | `InfixAppl ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#tybinop _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (`InfixAppl ((_x, _x_i1, _x_i2))))
      | `Regex _x -> let (o, _x) = o#regex _x in (o, (`Regex _x))
      | `UnaryAppl ((_x, _x_i1)) ->
          let (o, _x) = o#tyunary_op _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`UnaryAppl ((_x, _x_i1))))
      | `FnAppl ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#list (fun o -> o#phrase) _x_i1
          in (o, (`FnAppl ((_x, _x_i1))))
      | `TAbstr ((_x, _x_i1)) ->
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`TAbstr ((_x, _x_i1))))
      | `TAppl ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in (o, (`TAppl ((_x, _x_i1))))
      | `TupleLit _x ->
          let (o, _x) = o#list (fun o -> o#phrase) _x in (o, (`TupleLit _x))
      | `RecordLit ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
          in (o, (`RecordLit ((_x, _x_i1))))
      | `Projection ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#name _x_i1 in (o, (`Projection ((_x, _x_i1))))
      | `With ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1
          in (o, (`With ((_x, _x_i1))))
      | `TypeAnnotation ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#datatype' _x_i1
          in (o, (`TypeAnnotation ((_x, _x_i1))))
      | `Upcast ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#datatype' _x_i1 in
          let (o, _x_i2) = o#datatype' _x_i2
          in (o, (`Upcast ((_x, _x_i1, _x_i2))))
      | `ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
          in (o, (`ConstructorLit ((_x, _x_i1, _x_i2))))
      | `Switch ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2
          in (o, (`Switch ((_x, _x_i1, _x_i2))))
      | `Receive ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1
          in (o, (`Receive ((_x, _x_i1))))
      (* | `Fuse ((_x, _x_i1)) -> *)
      (*     let (o, _x) = o#phrase _x in *)
      (*     let (o, _x_i1) = o#phrase _x in (o, (`Fuse(_x, _x_i1))) *)
      | `Select ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#phrase _x_i1
          in (o, (`Select (_x, _x_i1)))
      | `Offer ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2
          in (o, (`Offer ((_x, _x_i1, _x_i2))))
      | `CP p ->
         let (o, p) = o#cp_phrase p in
         o, `CP p
      | `DatabaseLit ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            (fun (_x, _x_i1) ->
               let (o, _x) = o#option (fun o -> o#phrase) _x in
               let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
               in (o, (_x, _x_i1)))
              _x_i1
          in (o, (`DatabaseLit ((_x, _x_i1))))
      | `TableLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            (fun (_x, _x_i1) ->
               let (o, _x) = o#datatype _x in
               let (o, _x_i1) =
                 o#option
                   (fun o _x ->
                      let (o, _x) = o#unknown _x in (o, _x))
                   _x_i1
               in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#list (fun o -> o#fieldconstraint) _x_i1
                 in (o, (_x, _x_i1)))
              _x_i2 in
          let (o, _x_i3) = o#phrase _x_i3 in
          let (o, _x_i4) = o#phrase _x_i4 
          in (o, (`TableLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4))))
      | `DBDelete ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2
          in (o, (`DBDelete ((_x, _x_i1, _x_i2))))
      | `DBInsert ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#list (fun o -> o#name) _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2 in
          let (o, _x_i3) = o#option (fun o -> o#phrase) _x_i3
          in (o, (`DBInsert ((_x, _x_i1, _x_i2, _x_i3))))
      | `DBUpdate ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2 in
          let (o, _x_i3) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i3
          in (o, (`DBUpdate ((_x, _x_i1, _x_i2, _x_i3))))
      | `Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#list (fun o -> o#phrase) _x_i1
                 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2 in
          let (o, _x_i3) = o#list (fun o -> o#phrase) _x_i3
          in (o, (`Xml ((_x, _x_i1, _x_i2, _x_i3))))
      | `TextNode _x -> let (o, _x) = o#string _x in (o, (`TextNode _x))
      | `Formlet ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`Formlet ((_x, _x_i1))))
      | `Page _x -> let (o, _x) = o#phrase _x in (o, (`Page _x))
      | `FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (`FormletPlacement ((_x, _x_i1, _x_i2))))
      | `PagePlacement _x ->
          let (o, _x) = o#phrase _x in (o, (`PagePlacement _x))
      | `FormBinding ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#pattern _x_i1
          in (o, (`FormBinding ((_x, _x_i1))))

    method phrase : phrase -> ('self_type * phrase) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#phrasenode _x in
        let (o, _x_i1) = o#position _x_i1 in (o, (_x, _x_i1))

    method cp_phrasenode : cp_phrasenode -> ('self_type * cp_phrasenode) =
      function
      | `Unquote (bs, e) ->
         let o, bs = o#list (fun o -> o#binding) bs in
         let o, e = o#phrase e in
         o, `Unquote (bs, e)
      | `Grab (c, x, p) ->
         let o, p = o#cp_phrase p in
         o, `Grab (c, x, p)
      | `Give (c, e, p) ->
         let o, e = o#option (fun o -> o#phrase) e in
         let o, p = o#cp_phrase p in
         o, `Give (c, e, p)
      | `GiveNothing c ->
         let o, c = o#binder c in
         o, `GiveNothing c
      | `Select (c, l, p) ->
         let o, p = o#cp_phrase p in
         o, `Select (c, l, p)
      | `Offer (c, bs) ->
         let o, bs = o#list (fun o (l, p) ->
                             let o, p = o#cp_phrase p in
                             o, (l, p)) bs in
         o, `Offer (c, bs)
      | `Fuse (c, d) ->
         o, `Fuse (c, d)
      | `Comp (c, p, q) ->
         let o, p = o#cp_phrase p in
         let o, q = o#cp_phrase q in
         o, `Comp (c, p, q)

    method cp_phrase : cp_phrase -> ('self_type * cp_phrase) =
      fun (p, pos) ->
      let o, p = o#cp_phrasenode p in
      let o, pos = o#position pos in
      o, (p, pos)

    method patternnode : patternnode -> ('self_type * patternnode) =
      function
      | `Any -> (o, `Any)
      | `Nil -> (o, `Nil)
      | `Cons ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#pattern _x_i1 in (o, (`Cons ((_x, _x_i1))))
      | `List _x ->
          let (o, _x) = o#list (fun o -> o#pattern) _x in (o, (`List _x))
      | `Variant ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#option (fun o -> o#pattern) _x_i1
          in (o, (`Variant ((_x, _x_i1))))
      | `Negative _x ->
          let (o, _x) = o#list (fun o -> o#name) _x in (o, (`Negative _x))
      | `Record ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#pattern _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#pattern) _x_i1
          in (o, (`Record ((_x, _x_i1))))
      | `Tuple _x ->
          let (o, _x) = o#list (fun o -> o#pattern) _x in (o, (`Tuple _x))
      | `Constant _x -> let (o, _x) = o#constant _x in (o, (`Constant _x))
      | `Variable _x -> let (o, _x) = o#binder _x in (o, (`Variable _x))
      | `As ((_x, _x_i1)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#pattern _x_i1 in (o, (`As ((_x, _x_i1))))
      | `HasType ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#datatype' _x_i1 in (o, (`HasType ((_x, _x_i1))))

    method pattern : pattern -> ('self_type * pattern) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#patternnode _x in
        let (o, _x_i1) = o#position _x_i1 in (o, (_x, _x_i1))

    method operator : operator -> ('self_type * operator) =
      function
      | (#unary_op as x) -> (o#unary_op x :> 'self_type * operator)
      | (#binop as x) -> (o#binop x :> 'self_type * operator)
      | `Project _x -> let (o, _x) = o#name _x in (o, (`Project _x))

    method name : name -> ('self_type * name) = o#string

    method logical_binop : logical_binop -> ('self_type * logical_binop) =
      function | `And -> (o, `And) | `Or -> (o, `Or)

    method location : location -> ('self_type * location) = o#unknown

    method iterpatt : iterpatt -> ('self_type * iterpatt) =
      function
      | `List ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`List ((_x, _x_i1))))
      | `Table ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (`Table ((_x, _x_i1))))

    method funlit : funlit -> ('self_type * funlit) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
        let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1))

    method fieldspec : fieldspec -> ('self_type * fieldspec) =
      function
      | `Present _x -> let (o, _x) = o#datatype _x in (o, `Present _x)
      | `Absent -> (o, `Absent)
      | `Var _x -> let (o, _x) = o#known_type_variable _x in (o, `Var _x)

    method fieldconstraint :
      fieldconstraint -> ('self_type * fieldconstraint) =
      function | `Readonly -> (o, `Readonly) | `Default -> (o, `Default)

    method directive : directive -> ('self_type * directive) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#string _x in
        let (o, _x_i1) = o#list (fun o -> o#string) _x_i1 in (o, (_x, _x_i1))

    method datatype' : datatype' -> ('self_type * datatype') =
      fun (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1
        in (o, (_x, _x_i1))

    method datatype : datatype -> ('self_type * datatype) =
      function
      | `TypeVar _x ->
          let (o, _x) = o#known_type_variable _x in (o, (`TypeVar _x))
      | `Function (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#list (fun o -> o#datatype) _x in
          let (o, _x_i1) = o#row _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2
          in (o, (`Function (_x, _x_i1, _x_i2)))
      | `Lolli (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#list (fun o -> o#datatype) _x in
          let (o, _x_i1) = o#row _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2
          in (o, (`Lolli (_x, _x_i1, _x_i2)))
      | `Mu (_x, _x_i1) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#datatype _x_i1 in (o, (`Mu (_x, _x_i1)))
      | `Forall (_x, _x_i1) ->
          (*let (o, _x) = o#list (fun o -> o#quantifier) _x in*)
          let (o, _x_i1) = o#datatype _x_i1 in (o, (`Forall (_x, _x_i1)))
      | `Unit -> (o, `Unit)
      | `Tuple _x ->
          let (o, _x) = o#list (fun o -> o#datatype) _x
          in (o, (`Tuple _x))
      | `Record _x -> let (o, _x) = o#row _x in (o, (`Record _x))
      | `Variant _x -> let (o, _x) = o#row _x in (o, (`Variant _x))
      | `Table (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#datatype _x in
          let (o, _x_i1) = o#datatype _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2 in (o, (`Table (_x, _x_i1, _x_i2)))
      | `List _x -> let (o, _x) = o#datatype _x in (o, (`List _x))
      | `TypeApplication _x ->
          let (o, _x) =
            (fun (_x, _x_i1) ->
               let (o, _x) = o#string _x in
               let (o, _x_i1) = o#list (fun o -> o#type_arg) _x_i1
               in (o, (_x, _x_i1)))
              _x
          in (o, (`TypeApplication _x))
      | `Primitive _x ->
          let (o, _x) = o#unknown _x in (o, (`Primitive _x))
      | `DB -> (o, `DB)
      | `Input (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#datatype _x_i1 in (o, `Input (_x, _x_i1))
      | `Output (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#datatype _x_i1 in (o, `Output (_x, _x_i1))
      | `Select _x ->
        let (o, _x) = o#row _x in (o, `Select _x)
      | `Choice _x ->
        let (o, _x) = o#row _x in (o, `Choice _x)
      | `Dual _x ->
        let (o, _x) = o#datatype _x in (o, `Dual _x)
      | `End -> (o, `End)

    method type_arg : type_arg -> ('self_type * type_arg) =
      function
      | `Type _x -> let (o, _x) = o#datatype _x in (o, `Type _x)
      | `Row _x -> let (o, _x) = o#row _x in (o, `Row _x)
      | `Presence _x -> let (o, _x) = o#fieldspec _x in (o, `Presence _x)

    method constant : constant -> ('self_type * constant) =
      function
      | `Float _x -> let (o, _x) = o#float _x in (o, (`Float _x))
      | `Int _x -> let (o, _x) = o#int _x in (o, (`Int _x))
      | `String _x -> let (o, _x) = o#string _x in (o, (`String _x))
      | `Bool _x -> let (o, _x) = o#bool _x in (o, (`Bool _x))
      | `Char _x -> let (o, _x) = o#char _x in (o, (`Char _x))

    method binop : binop -> ('self_type * binop) =
      function
      | `Minus -> (o, `Minus)
      | `FloatMinus -> (o, `FloatMinus)
      | `RegexMatch _x ->
          let (o, _x) = o#list (fun o -> o#regexflag) _x
          in (o, (`RegexMatch _x))
      | (#logical_binop as x) -> (o#logical_binop x :> 'self_type * binop)
      | `Cons -> (o, `Cons)
      | `Name _x -> let (o, _x) = o#name _x in (o, (`Name _x))

    method tybinop : tyarg list * binop -> 'self_type * (tyarg list * binop) =
      fun (_x, _x_i1) ->
        let (o, _x_i1) = o#binop _x_i1 in (o, (_x, _x_i1))

    method bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | `Val ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let (o, _x_i1) = o#pattern _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2 in
          let (o, _x_i3) = o#location _x_i3 in
          let (o, _x_i4) = o#option (fun o -> o#datatype') _x_i4
          in (o, (`Val ((_x, _x_i1, _x_i2, _x_i3, _x_i4))))
      | `Fun ((_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i2) = o#funlit _x_i2 in
          let (o, _x_i3) = o#location _x_i3 in
          let (o, _x_i4) = o#option (fun o -> o#datatype') _x_i4
          in (o, (`Fun ((_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4))))
      | `Funs _x ->
          let (o, _x) =
            o#list
              (fun o (_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4, _x_i5) ->
                 let (o, _x) = o#binder _x in
                 let (o, _x_i2) = o#funlit _x_i2 in
                 let (o, _x_i3) = o#location _x_i3 in
                 let (o, _x_i4) = o#option (fun o -> o#datatype') _x_i4 in
                 let (o, _x_i5) = o#position _x_i5
                 in (o, (_x, _x1, (_x_i1, _x_i2), _x_i3, _x_i4, _x_i5)))
              _x
          in (o, (`Funs _x))
      | `Foreign ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#name _x_i1 in
          let (o, _x_i2) = o#datatype' _x_i2
          in (o, (`Foreign ((_x, _x_i1, _x_i2))))
      | `Import _x -> let (o, _x) = o#string _x in (o, (`Import _x))
      | `Type ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 (*let (o, _x) = o#quantifier _x in*)
                 let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1
                 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#datatype' _x_i2
          in (o, (`Type ((_x, _x_i1, _x_i2))))
      | `Infix -> (o, `Infix)
      | `Exp _x -> let (o, _x) = o#phrase _x in (o, (`Exp _x))
      | `Module (n, p) ->
          let (o, n) = o#string n in
          let (o, p) = o#phrase p in
          (o, (`Module (n, p)))

    method binding : binding -> ('self_type * binding) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#bindingnode _x in
        let (o, _x_i1) = o#position _x_i1 in (o, (_x, _x_i1))

    method binder : binder -> ('self_type * binder) =
      fun (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#name _x in
        let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1 in
        let (o, _x_i2) = o#position _x_i2 in (o, (_x, _x_i1, _x_i2))

    method unknown : 'a. 'a -> ('self_type * 'a) = fun x -> (o, x)

  end
