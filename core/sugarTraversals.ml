(* This file is generated mostly automatically using Hand in Left-or-right-handed 1.0.0 *)
(* Generated mostly automatically using Camlp4 in OCaml 3.10.0.

   NB: DO NOT EDIT (except to keep it in line with sugartypes.ml).

   When we switch to OCaml 3.10.x, and when the Camlp4MapGenerator
   filter which comes with camlp4 3.10.0 works sufficiently well we'll
   generate all this automatically instead of maintaining this file.
*)

open Operators
open CommonTypes
open SourceCode
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

    method timestamp : Timestamp.t -> Timestamp.t = o#unknown

    method bool : bool -> bool = function | false -> false | true -> true

    method linearity : DeclaredLinearity.t -> DeclaredLinearity.t =
      DeclaredLinearity.(function | Lin -> Lin | Unl -> Unl)

    method unary_op : UnaryOp.t -> UnaryOp.t =
      let open UnaryOp in function
      | Minus -> Minus
      | FloatMinus -> FloatMinus
      | Name _x -> let _x = o#name _x in Name _x

    method tyunary_op : tyarg list * UnaryOp.t -> tyarg list * UnaryOp.t =
      fun (_x, _x_i1) -> (o#list (fun o -> o#tyarg) _x, o#unary_op _x_i1)

    method binder : Binder.with_pos -> Binder.with_pos =
      fun bndr ->
        let name = o#name (Binder.to_name bndr) in
        let ty  = Binder.to_type bndr |> o#typ in
        let pos = WithPos.pos bndr |> o#position in
        let bndr' = Binder.(set_type (set_name bndr name) ty) in
        WithPos.map2 bndr' ~f_pos:(fun _ -> pos) ~f_node:(fun x -> x)

    method sentence : sentence -> sentence =
      function
      | Definitions _x ->
          let _x = o#list (fun o -> o#binding) _x in Definitions _x
      | Expression _x -> let _x = o#phrase _x in Expression _x
      | Directive _x -> let _x = o#directive _x in Directive _x

    method section : Section.t -> Section.t =
      let open Section in function
      | Minus -> Minus
      | FloatMinus -> FloatMinus
      | Project _x -> let _x = o#name _x in Project _x
      | Name _x -> let _x = o#name _x in Name _x

    method subkind : Subkind.t -> Subkind.t = fun x -> x

    method kind : kind -> kind = fun x -> x

    method freedom : Freedom.t -> Freedom.t = fun x -> x

    method type_variable : SugarTypeVar.t -> SugarTypeVar.t =
      let open SugarTypeVar in
      function
        | TUnresolved (name, (is_eff, subkind_opt), freedom) ->
           let name' = o#name name in
           let is_eff' = o#bool is_eff in
           let subkind_opt' = o#option (fun o -> o#subkind) subkind_opt in
           let freedom' = o#freedom freedom in
           TUnresolved (name', (is_eff', subkind_opt'), freedom')
        | v -> o#unknown v


    method quantifier : SugarQuantifier.t -> SugarQuantifier.t =
      let open SugarQuantifier in
      function
      | QUnresolved (name, kind, freedom) ->
         let name' = o#name name in
         let kind' = o#kind kind in
         let freedom' = o#freedom freedom in
         QUnresolved (name', kind', freedom')
      | (QResolved _) as rq -> rq



    method row_var : Datatype.row_var -> Datatype.row_var =
      let open Datatype in
      function
      | EffectApplication (_x, _x_i1) ->
          let _x = o#name _x in
          let _x_i1 = o#list (fun o -> o#type_arg) _x_i1
          in EffectApplication (_x, _x_i1)
      | Closed -> Closed
      | Open _x ->
          let _x = o#type_variable _x in Open _x
      | Recursive ((_x, _x_i1)) ->
          let _x = o#type_variable _x in
          let _x_i1 = o#row _x_i1 in Recursive ((_x, _x_i1))

    method row : Datatype.row -> Datatype.row =
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
      | Literal    _x -> let _x = o#string _x in Literal    _x
      | SpliceExpr _x -> let _x = o#phrase _x in SpliceExpr _x

    method regexflag : regexflag -> regexflag =
      fun flag -> flag

    method regex : regex -> regex =
      function
      | Range ((_x, _x_i1)) ->
          let _x = o#char _x in
          let _x_i1 = o#char _x_i1 in Range ((_x, _x_i1))
      | Simply _x -> let _x = o#string _x in Simply _x
      | Quote _x -> let _x = o#regex _x in Quote _x
      | Any -> Any
      | StartAnchor -> StartAnchor
      | EndAnchor -> EndAnchor
      | Seq _x -> let _x = o#list (fun o -> o#regex) _x in Seq _x
      | Alternate ((_x, _x_i1)) ->
          let _x = o#regex _x in
          let _x_i1 = o#regex _x_i1 in Alternate ((_x, _x_i1))
      | Group _x -> let _x = o#regex _x in Group _x
      | Repeat ((_x, _x_i1)) ->
          let _x = o#unknown _x in
          let _x_i1 = o#regex _x_i1 in Repeat ((_x, _x_i1))
      | Splice _x -> let _x = o#phrase _x in Splice _x
      | Replace ((_x, _x_i1)) ->
          let _x = o#regex _x in
          let _x_i1 = o#replace_rhs _x_i1 in Replace ((_x, _x_i1))

    method position : Position.t -> Position.t =
      fun pos ->
        let start = Position.start pos |> o#unknown in
        let finish = Position.finish pos |> o#unknown in
        let code = Position.code pos |> o#unknown in
        Position.make ~start ~finish ~code

    method datatype' : datatype' -> datatype' =
      fun (x, y) ->
        let x = o#datatype x in
        let y = o#option (fun o -> o#typ) y in
        (x,y)

    method row' : row' -> row' =
      fun (x, y) ->
        let x = o#row x in
        let y = o#option (fun o -> o#typ) y in
        (x,y)

    method given_spawn_location : given_spawn_location -> given_spawn_location =
      function
        | ExplicitSpawnLocation p -> ExplicitSpawnLocation (o#phrase p)
        | l -> l

    method temporal_update : temporal_update -> temporal_update =
      function
        | ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
            let validity_from = o#phrase validity_from in
            let validity_to = o#phrase validity_to in
            ValidTimeUpdate (SequencedUpdate { validity_from; validity_to } )
        | ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
            let from_time = o#option (fun o -> o#phrase) from_time in
            let to_time = o#option (fun o -> o#phrase) to_time in
            ValidTimeUpdate (NonsequencedUpdate { from_time; to_time })
        | x -> x

    method temporal_deletion : temporal_deletion -> temporal_deletion =
      function
        | ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }) ->
            let validity_from = o#phrase validity_from in
            let validity_to = o#phrase validity_to in
            ValidTimeDeletion (SequencedDeletion { validity_from; validity_to })
        | x -> x

    method phrasenode : phrasenode -> phrasenode =
      function
      | Constant _x -> let _x = o#constant _x in Constant _x
      | Var _x -> let _x = o#name _x in Var _x
      | FreezeVar _x -> let _x = o#name _x in FreezeVar _x
      | QualifiedVar _xs ->
          let _xs = o#list (fun o -> o#name) _xs in QualifiedVar _xs
      | FunLit (_x, _x1, _x_i1, _x_i2) ->
          let _x = o#option (fun o -> o#list (fun o (t, r) -> (o#typ t, o#type_row r))) _x in
          let _x_i1 = o#funlit _x_i1 in
          let _x_i2 = o#location _x_i2 in
          FunLit (_x, _x1, _x_i1, _x_i2)
      | Spawn (_spawn_kind, _given_spawn_location, _block_phr, _dt) ->
          let _given_spawn_location = o#given_spawn_location _given_spawn_location in
          let _block_phr = o#phrase _block_phr in
          let _dt = o#option (fun o -> o#type_row) _dt in
          Spawn (_spawn_kind, _given_spawn_location, _block_phr, _dt)
      | Query (_x, _policy, _x_i1, _x_i2) ->
          let _x =
            o#option
              (fun o (_x, _x_i1) ->
                 let _x = o#phrase _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#phrase _x_i1 in Query (_x, _policy, _x_i1, _x_i2)
      | DBTemporalJoin (_mode, _block, _ty) ->
          let _block = o#phrase _block in
          DBTemporalJoin (_mode, _block, _ty)
      | ListLit (_x, _x_i1) ->
          let _x = o#list (fun o -> o#phrase) _x in
          let _x_i1 = o#option (fun o -> o#typ) _x_i1 in
         ListLit (_x, _x_i1)
      | Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#list (fun o -> o#iterpatt) _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 = o#option (fun o -> o#phrase) _x_i3
          in Iteration ((_x, _x_i1, _x_i2, _x_i3))
      | Escape ((_x, _x_i1)) ->
          let _x = o#binder _x in
          let _x_i1 = o#phrase _x_i1 in Escape ((_x, _x_i1))
      | Section _x -> let _x = o#section _x in Section _x
      | FreezeSection _x -> let _x = o#section _x in FreezeSection _x
      | Conditional ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in Conditional ((_x, _x_i1, _x_i2))
      | Block ((_x, _x_i1)) ->
          let _x = o#list (fun o -> o#binding) _x in
          let _x_i1 = o#phrase _x_i1 in Block ((_x, _x_i1))
      | InfixAppl ((_x, _x_i1, _x_i2)) ->
          let _x = o#tybinop _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in InfixAppl ((_x, _x_i1, _x_i2))
      | RangeLit ((_x_i1, _x_i2)) ->
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in RangeLit ((_x_i1, _x_i2))
      | Regex _x -> let _x = o#regex _x in Regex _x
      | UnaryAppl ((_x, _x_i1)) ->
          let _x = o#tyunary_op _x in
          let _x_i1 = o#phrase _x_i1 in UnaryAppl ((_x, _x_i1))
      | FnAppl ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#phrase) _x_i1
          in FnAppl ((_x, _x_i1))
      | TAbstr ((_x, _x_i1)) ->
          let _x = o#list (fun o -> o#quantifier) _x in
          let _x_i1 = o#phrase _x_i1 in
          TAbstr ((_x, _x_i1))
      | TAppl ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#type_arg') _x_i1 in
          TAppl ((_x, _x_i1))
      | TupleLit _x ->
          let _x = o#list (fun o -> o#phrase) _x in TupleLit _x
      | RecordLit ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#phrase) _x_i1
          in RecordLit ((_x, _x_i1))
      | Projection ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#name _x_i1 in Projection ((_x, _x_i1))
      | With ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1
          in With ((_x, _x_i1))
      | TypeAnnotation ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype' _x_i1 in TypeAnnotation ((_x, _x_i1))
      | Upcast ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#datatype' _x_i1 in
          let _x_i2 = o#datatype' _x_i2 in Upcast ((_x, _x_i1, _x_i2))
      | Instantiate _x ->
          let _x = o#phrase _x in
          Instantiate _x
      | Generalise _x ->
          let _x = o#phrase _x in
          Generalise _x
      | ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#name _x in
          let _x_i1 = o#option (fun o -> o#phrase) _x_i1 in
          let _x_i2 = o#option (fun o -> o#typ) _x_i2 in
          ConstructorLit ((_x, _x_i1, _x_i2))
      | DoOperation (op, ps, t, b) ->
          let op  = o#phrase op in
          let ps  = o#list (fun o -> o#phrase) ps in
          let t   = o#option (fun o -> o#typ) t in
          DoOperation (op, ps, t, b)
      | Operation _x ->
          let _x = o#name _x in
          Operation _x
      | Linlet _x ->
          let _x = o#phrase _x in Linlet _x
      | Unlet _x ->
          let _x = o#phrase _x in Unlet _x
      | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
         let m = o#phrase sh_expr in
         let params =
            o#option (fun o -> o#handle_params) sh_descr.shd_params
         in
         let eff_cases =
           o#list
             (fun o (lhs, rhs) ->
               let lhs = o#pattern lhs in
               let rhs = o#phrase rhs in (lhs, rhs)
         )
             sh_effect_cases
     in
         let val_cases =
           o#list
             (fun o (lhs, rhs) ->
               let lhs = o#pattern lhs in
               let rhs = o#phrase rhs in (lhs, rhs)
         )
             sh_value_cases
     in
         Handle { sh_expr = m; sh_effect_cases = eff_cases; sh_value_cases = val_cases; sh_descr = { sh_descr with shd_params = params } }
      | Switch ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#typ) _x_i2
          in Switch ((_x, _x_i1, _x_i2))
      | Receive ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#typ) _x_i1
          in Receive (_x, _x_i1)
      (* | Link ((_x, _x_i1)) -> *)
      (*     let _x = o#phrase _x in *)
      (*     let _x_i1 = o#phrase _x_i1 in Link ((_x, _x_i1)) *)
      | Select ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#phrase _x_i1
          in Select (_x, _x_i1)
      | Offer ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#pattern _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2
          in Offer (_x, _x_i1, _x_i2)
      | CP p -> CP (o#cp_phrase p)
      | DatabaseLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 =
            (fun (_x, _x_i1) ->
               let _x = o#option (fun o -> o#phrase) _x in
               let _x_i1 = o#option (fun o -> o#phrase) _x_i1 in (_x, _x_i1))
              _x_i1
          in DatabaseLit ((_x, _x_i1))
      | TableLit { tbl_name; tbl_type = (tmp, dt, rows_opt);
          tbl_field_constraints;
          tbl_keys; tbl_temporal_fields; tbl_database } ->
          let tbl_name = o#phrase tbl_name in
          let dt = o#datatype dt in
          let rows_opt = o#option
            (fun o (a, b, c) ->
              let a = o#typ a in
              let b = o#typ b in
              let c = o#typ c in
              (a, b, c)) rows_opt in
          let tbl_field_constraints =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#list (fun o -> o#fieldconstraint) _x_i1
                 in (_x, _x_i1))
              tbl_field_constraints in
          let tbl_keys = o#phrase tbl_keys in
          let tbl_database = o#phrase tbl_database in
          let tbl_type = (tmp, dt, rows_opt) in
          TableLit
            { tbl_name; tbl_type; tbl_field_constraints;
              tbl_keys; tbl_temporal_fields; tbl_database }
      | LensLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#option (fun o -> o#unknown) _x_i1 in
          LensLit (_x, _x_i1)
      | LensSerialLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          LensSerialLit(_x, _x_i1, _x_i2)
      | LensKeysLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2 in
          LensKeysLit (_x, _x_i1, _x_i2)
      | LensFunDepsLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2 in
          LensFunDepsLit (_x, _x_i1, _x_i2)
      | LensDropLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#string _x_i1 in
          let _x_i2 = o#string _x_i2 in
          let _x_i3 = o#phrase _x_i3 in
          let _x_i4 = o#option (fun o -> o#unknown) _x_i4 in
          LensDropLit((_x, _x_i1, _x_i2, _x_i3, _x_i4))
      | LensSelectLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#unknown) _x_i2 in
          LensSelectLit ((_x, _x_i1, _x_i2))
      | LensJoinLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2 in
          (* _x_i3 and _x_i4 are both phrases which are left unchanged *)
          let _x_i5 = o#option (fun o -> o#unknown) _x_i5 in
          LensJoinLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5))
      | LensCheckLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#option (fun o -> o#unknown) _x_i1 in
          LensCheckLit ((_x, _x_i1))
      | LensGetLit ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#option (fun o -> o#typ) _x_i1 in
            LensGetLit ((_x, _x_i1))
      | LensPutLit ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#typ) _x_i2 in
            LensPutLit ((_x, _x_i1, _x_i2))
      | DBDelete ((_del, _x, _x_i1, _x_i2)) ->
          let _del = o#option (fun o -> o#temporal_deletion) _del in
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2
          in DBDelete ((_del, _x, _x_i1, _x_i2))
      | DBInsert ((_mode, _x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#list (fun o -> o#name) _x_i1 in
          let _x_i2 = o#phrase _x_i2 in
          let _x_i3 = o#option (fun o -> o#phrase) _x_i3 in
          DBInsert ((_mode, _x, _x_i1, _x_i2, _x_i3))
      | DBUpdate ((_upd, _x, _x_i1, _x_i2, _x_i3)) ->
          let _upd = o#option (fun o -> o#temporal_update) _upd in
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#phrase _x_i1 in (_x, _x_i1))
              _x_i3
          in DBUpdate ((_upd, _x, _x_i1, _x_i2, _x_i3))
      | Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
          let _x = o#name _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#list (fun o -> o#phrase) _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#phrase) _x_i2 in
          let _x_i3 = o#list (fun o -> o#phrase) _x_i3
          in Xml ((_x, _x_i1, _x_i2, _x_i3))
      | TextNode _x -> let _x = o#string _x in TextNode _x
      | Formlet ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in Formlet ((_x, _x_i1))
      | Page _x -> let _x = o#phrase _x in Page _x
      | FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#phrase _x_i1 in
          let _x_i2 = o#phrase _x_i2
          in FormletPlacement ((_x, _x_i1, _x_i2))
      | PagePlacement _x -> let _x = o#phrase _x in PagePlacement _x
      | FormBinding ((_x, _x_i1)) ->
          let _x = o#phrase _x in
          let _x_i1 = o#pattern _x_i1 in FormBinding ((_x, _x_i1))
      | TryInOtherwise (_p1, _pat, _p2, _p3, _ty) ->
          let _p1 = o#phrase _p1 in
          let _pat = o#pattern _pat in
          let _p2 = o#phrase _p2 in
          let _p3 = o#phrase _p3 in
          let _ty = o#option (fun o -> o#typ) _ty in
          TryInOtherwise (_p1, _pat, _p2, _p3, _ty)
      | Raise -> Raise


    method phrase : phrase -> phrase =
      fun p ->
        WithPos.map2 ~f_pos:o#position ~f_node:o#phrasenode p

    method cp_phrasenode : cp_phrasenode -> cp_phrasenode =
      let arg_pair (o : 'self_type) =
        o#option (fun o (dt, args) ->
            let dt = o#typ dt in
            let args = o#list (fun o -> o#tyarg) args in
            (o#typ dt, args))
      in
      function
      | CPUnquote (bs, e)  -> CPUnquote (o#list (fun o -> o#binding) bs, o#phrase e)
      | CPGrab ((c, a), x, p)   -> CPGrab ((c, arg_pair o a), x, o#cp_phrase p)
      | CPGive ((c, a), e, p)   -> CPGive ((c, arg_pair o a), o#option (fun o -> o#phrase) e, o#cp_phrase p)
      | CPGiveNothing c    -> CPGiveNothing (o#binder c)
      | CPSelect (c, l, p) -> CPSelect (c, l, o#cp_phrase p)
      | CPOffer (c, bs)    -> CPOffer (c, o#list (fun o (l, p) -> (l, o#cp_phrase p)) bs)
      | CPLink (c, d)      -> CPLink (c, d)
      | CPComp (c, p, q)   -> CPComp (c, o#cp_phrase p, o#cp_phrase q)

    method cp_phrase : cp_phrase -> cp_phrase =
      fun p ->
      WithPos.map2 ~f_pos:o#position ~f_node:o#cp_phrasenode p

    method patternnode : Pattern.t -> Pattern.t =
      let open Pattern in
      function
      | Any -> Any
      | Nil -> Nil
      | Cons ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#pattern _x_i1 in Cons ((_x, _x_i1))
      | List _x -> let _x = o#list (fun o -> o#pattern) _x in List _x
      | Variant ((_x, _x_i1)) ->
          let _x = o#name _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in Variant ((_x, _x_i1))
      | Operation (name, ps, k, b) ->
         let name = o#name name in
         let ps = o#list (fun o -> o#pattern) ps in
         let k  = o#pattern k in
         let b = o#linearity b in
         Operation (name, ps, k, b)
      | Negative _x ->
          let _x = o#list (fun o -> o#name) _x
          in Negative _x
      | Record ((_x, _x_i1)) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#name _x in
                 let _x_i1 = o#pattern _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#option (fun o -> o#pattern) _x_i1
          in Record ((_x, _x_i1))
      | Tuple _x -> let _x = o#list (fun o -> o#pattern) _x in Tuple _x
      | Constant _x -> let _x = o#constant _x in Constant _x
      | Variable _x -> let _x = o#binder _x in Variable _x
      | As ((_x, _x_i1)) ->
          let _x = o#binder _x in
          let _x_i1 = o#pattern _x_i1 in As ((_x, _x_i1))
      | HasType ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#datatype' _x_i1 in HasType ((_x, _x_i1))

    method pattern : Pattern.with_pos -> Pattern.with_pos =
      fun p ->
        WithPos.map2 ~f_pos:o#position ~f_node:o#patternnode p

    method foreign_language : ForeignLanguage.t -> ForeignLanguage.t
      = fun lang -> lang

    method name : Name.t -> Name.t = o#string

    method location : Location.t -> Location.t = o#unknown

    method iterpatt : iterpatt -> iterpatt =
      function
      | List ((_x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in List ((_x, _x_i1))
      | Table ((_t, _x, _x_i1)) ->
          let _x = o#pattern _x in
          let _x_i1 = o#phrase _x_i1 in Table ((_t, _x, _x_i1))

    method funlit : funlit -> funlit =
      fun f ->
        match f with
          | NormalFunlit (_x, _x_i1) ->
            let _x = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
            let _x_i1 = o#phrase _x_i1 in NormalFunlit (_x, _x_i1)
          | SwitchFunlit (pat, body) ->
            let pat = o#list (fun o -> o#list (fun o -> o#pattern)) pat in
            let body =
              o#list (fun o (p, c) ->
                let p = o#pattern p in
                let c = o#phrase c in (p, c)) body in
            SwitchFunlit (pat, body)

    method handle_params : handler_parameterisation -> handler_parameterisation =
      fun { shp_bindings; shp_types }->
      let shp_bindings =
        o#list
          (fun o (pat, expr) ->
            let expr = o#phrase expr in
            let pat = o#pattern pat in
            (pat, expr))
          shp_bindings
      in
      let shp_types = o#list (fun o -> o#typ) shp_types in
      { shp_bindings; shp_types }

    method fieldspec : Datatype.fieldspec -> Datatype.fieldspec =
      let open Datatype in function
      | Present _x -> let _x = o#datatype _x in Present _x
      | Absent -> Absent
      | Var _x -> let _x = o#type_variable _x in Var _x

    method fieldconstraint : fieldconstraint -> fieldconstraint =
      fun fc -> fc

    method directive : directive -> directive =
      fun (_x, _x_i1) ->
        let _x = o#string _x in
        let _x_i1 = o#list (fun o -> o#string) _x_i1 in (_x, _x_i1)

    method datatypenode : Datatype.t -> Datatype.t =
      let open Datatype in
      function
      | TypeVar _x ->
          let _x = o#type_variable _x in TypeVar _x
      | QualifiedTypeApplication (ns, args) ->
          let ns = o#list (fun o -> o#name) ns in
          let args = o#list (fun o -> o#type_arg) args in
          QualifiedTypeApplication (ns, args)
      | Function (_x, _x_i1, _x_i2) ->
          let _x = o#list (fun o -> o#datatype) _x in
          let _x_i1 = o#row _x_i1 in
          let _x_i2 = o#datatype _x_i2 in Function (_x, _x_i1, _x_i2)
      | Lolli (_x, _x_i1, _x_i2) ->
          let _x = o#list (fun o -> o#datatype) _x in
          let _x_i1 = o#row _x_i1 in
          let _x_i2 = o#datatype _x_i2 in Lolli (_x, _x_i1, _x_i2)
      | Mu (_x, _x_i1) ->
          let _x = o#type_variable _x in
          let _x_i1 = o#datatype _x_i1 in Mu (_x, _x_i1)
      | Forall (_x, _x_i1) ->
          let _x_i1 = o#datatype _x_i1 in Forall (_x, _x_i1)
      | Unit -> Unit
      | Tuple _x ->
          let _x = o#list (fun o -> o#datatype) _x in Tuple _x
      | Record _x -> let _x = o#row _x in Record _x
      | Variant _x -> let _x = o#row _x in Variant _x
      | Effect r -> let r = o#row r in Effect r
      | Operation (_x, _x_i1, _x_i2) ->
        let _x = o#list (fun o -> o#datatype) _x in
        let _x_i1 = o#datatype _x_i1 in
        let _x_i2 = o#linearity _x_i2 in Operation (_x, _x_i1, _x_i2)
      | Table (_t, _x, _x_i1, _x_i2) ->
         let _x = o#datatype _x in
         let _x_i1 = o#datatype _x_i1 in
         let _x_i2 = o#datatype _x_i2 in Table (_t, _x, _x_i1, _x_i2)
      | List _x -> let _x = o#datatype _x in List _x
      | TypeApplication (_x, _x_i1) ->
          let _x = o#name _x in
          let _x_i1 = o#list (fun o -> o#type_arg) _x_i1
          in TypeApplication (_x, _x_i1)
      | Primitive _x -> let _x = o#unknown _x in Primitive _x
      | DB -> DB
      | Input (_x, _x_i1) ->
        let _x = o#datatype _x in
        let _x_i1 = o#datatype _x_i1 in Input (_x, _x_i1)
      | Output (_x, _x_i1) ->
        let _x = o#datatype _x in
        let _x_i1 = o#datatype _x_i1 in Output (_x, _x_i1)
      | Select _x ->
        let _x = o#row _x in Select _x
      | Choice _x ->
        let _x = o#row _x in Choice _x
      | Dual _x ->
        let _x = o#datatype _x in Dual _x
      | End -> End

    method datatype : Datatype.with_pos -> Datatype.with_pos =
        WithPos.map2 ~f_pos:o#position ~f_node:o#datatypenode

    method type_arg : Datatype.type_arg -> Datatype.type_arg =
      let open Datatype in function
      | Type _x -> let _x = o#datatype _x in Type _x
      | Row _x -> let _x = o#row _x in Row _x
      | Presence _x -> let _x = o#fieldspec _x in Presence _x

    method type_arg' : type_arg' -> type_arg' =
      fun (x, y) ->
        let x = o#type_arg x in
        let y = o#option (fun o -> o#tyarg) y in
        (x, y)

    method constant : Constant.t -> Constant.t =
      function
      | Constant.Float _x  -> let _x = o#float _x  in Constant.Float _x
      | Constant.Int _x    -> let _x = o#int _x    in Constant.Int _x
      | Constant.String _x -> let _x = o#string _x in Constant.String _x
      | Constant.Bool _x   -> let _x = o#bool _x   in Constant.Bool _x
      | Constant.Char _x   -> let _x = o#char _x   in Constant.Char _x
      | Constant.DateTime _x   -> let _x = o#timestamp _x   in Constant.DateTime _x

    method binop : BinaryOp.t -> BinaryOp.t =
      let open BinaryOp in function
      | Minus -> Minus
      | FloatMinus -> FloatMinus
      | RegexMatch _x ->
          let _x = o#list (fun o -> o#regexflag) _x in RegexMatch _x
      | And -> And
      | Or -> Or
      | Cons -> Cons
      | Name _x -> let _x = o#name _x in Name _x

    method tybinop : tyarg list * BinaryOp.t -> tyarg list * BinaryOp.t =
      fun (_x, _x_i1) -> (o#list (fun o -> o#tyarg) _x, o#binop _x_i1)

    method bindingnode : bindingnode -> bindingnode =
      function
      | Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let _x    = o#pattern _x in
          let _x_i1 = o#list (fun o -> o#quantifier) _x_i1 in
          let _x_i2 = o#phrase _x_i2 in
          let _x_i3 = o#location _x_i3 in
          let _x_i4 = o#option (fun o -> o#datatype') _x_i4
          in Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4))
      | Fun f -> Fun (o#function_definition f)
      | Funs _x ->
          let _x = o#list (fun o -> o#recursive_function) _x in
          Funs _x
      | Foreign alien ->
         let declarations =
           o#list
             (fun o (b, dt) ->
               let b = o#binder b in
               let dt = o#datatype' dt in
               (b, dt))
             (Alien.declarations alien)
         in
         let language = o#foreign_language (Alien.language alien) in
         Foreign (Alien.modify ~declarations ~language alien)
      | Import { pollute; path } ->
         Import { pollute; path = o#list (fun o -> o#name) path }
      | Open _xs ->
          let _xs = o#list (fun o -> o#name) _xs in
          Open _xs
      | Aliases ts ->
          let _x = o#list (fun o -> o#alias) ts in
          Aliases _x
      | Infix { name; assoc; precedence } ->
         Infix { name = o#name name; assoc; precedence }
      | Exp _x -> let _x = o#phrase _x in Exp _x
      | Module { binder; members } ->
          let binder = o#binder binder in
          let members = o#list (fun o -> o#binding) members in
          Module { binder; members }
      | AlienBlock alien ->
         let declarations =
           o#list
             (fun o (b, dt) ->
               let b = o#binder b in
               let dt = o#datatype' dt in
               (b, dt))
             (Alien.declarations alien)
         in
         let language = o#foreign_language (Alien.language alien) in
         AlienBlock (Alien.modify ~language ~declarations alien)

    method binding : binding -> binding =
      fun p ->
        WithPos.map2 ~f_pos:o#position ~f_node:o#bindingnode p

    method aliasnode : aliasnode -> aliasnode =
      fun (_x, _x_i1, _x_i2) ->
      let _x = o#name _x in
      let _x_i1 = o#list (fun o x -> o#quantifier x)
                    _x_i1 in
      let _x_i2 = o#aliasbody _x_i2 in
      (_x, _x_i1, _x_i2)

    method aliasbody : aliasbody -> aliasbody =
      function
        | Typename _x -> Typename (o#datatype' _x)
        | Effectname _x -> Effectname (o#row' _x)

    method alias : alias -> alias =
      fun p ->
        WithPos.map2 ~f_pos:o#position ~f_node:o#aliasnode p

    method function_definition : function_definition -> function_definition
      = fun { fun_binder;
              fun_linearity;
              fun_definition = (tyvar, lit);
              fun_location;
              fun_signature;
              fun_frozen;
              fun_unsafe_signature; } ->
      let fun_binder = o#binder fun_binder in
      let tyvar = o#list (fun o -> o#quantifier) tyvar in
      let lit = o#funlit lit in
      let fun_location = o#location fun_location in
      let fun_signature = o#option (fun o -> o#datatype') fun_signature in
      { fun_binder;
        fun_linearity;
        fun_definition = (tyvar, lit);
        fun_location;
        fun_signature;
        fun_frozen;
        fun_unsafe_signature; }

    method recursive_functionnode : recursive_functionnode -> recursive_functionnode
      = fun { rec_binder;
              rec_linearity;
              rec_definition = ((tyvar, ty), lit);
              rec_location;
              rec_signature;
              rec_unsafe_signature;
              rec_frozen } ->
      let rec_binder = o#binder rec_binder in
      let tyvar = o#list (fun o -> o#quantifier) tyvar in
      let ty = o#option (fun o (t, x)-> o#typ t, x) ty in
      let lit = o#funlit lit in
      let rec_location = o#location rec_location in
      let rec_signature = o#option (fun o -> o#datatype') rec_signature in
      { rec_binder;
        rec_linearity;
        rec_definition = ((tyvar, ty), lit);
        rec_location;
        rec_signature;
        rec_unsafe_signature;
        rec_frozen}

    method recursive_function  : recursive_function -> recursive_function
      = fun p ->
        WithPos.map2 ~f_pos:o#position ~f_node:o#recursive_functionnode p

    method program : program -> program =
      fun (bindings, phrase) ->
        let bindings = o#list (fun o -> o#binding) bindings in
        let phrase = o#option (fun o -> o#phrase) phrase in
          (bindings, phrase)

    method typ : Types.datatype -> Types.datatype = o#unknown
    method type_row : Types.row -> Types.row = o#unknown
    method type_field_spec : Types.field_spec -> Types.field_spec = o#unknown
    method tyarg : tyarg -> tyarg =
      let open PrimaryKind in
      function
      | Type, t     -> Type, o#typ t
      | Row, r      -> Row, o#type_row r
      | Presence, p -> Presence, o#type_field_spec p

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

    method timestamp : Timestamp.t -> 'self_type = o#unknown

    method bool : bool -> 'self_type = function | false -> o | true -> o

    method linearity : DeclaredLinearity.t -> 'self_type =
      DeclaredLinearity.(function | Unl -> o | Lin -> o)

    method unary_op : UnaryOp.t -> 'self_type =
      let open UnaryOp in function
      | Minus -> o
      | FloatMinus -> o
      | Name _x -> let o = o#name _x in o

    method tyunary_op : tyarg list * UnaryOp.t -> 'self_type =
      fun (_x, _x_i1) -> o#unary_op _x_i1

    method binder : Binder.with_pos -> 'self_type =
      fun bndr ->
        let o = o#name (Binder.to_name bndr) in
        let o = o#position (WithPos.pos bndr) in o

    method sentence : sentence -> 'self_type =
      function
      | Definitions _x -> let o = o#list (fun o -> o#binding) _x in o
      | Expression _x -> let o = o#phrase _x in o
      | Directive _x -> let o = o#directive _x in o

    method section : Section.t -> 'self_type =
      let open Section in function
      | Minus -> o
      | FloatMinus -> o
      | Project _x -> let o = o#name _x in o
      | Name _x -> let o = o#name _x in o

    method subkind : Subkind.t -> 'self_type = fun _ -> o

    method kind : kind -> 'self_type = fun _ -> o

    method freedom : Freedom.t -> 'self_type = fun _ -> o

    method type_variable : SugarTypeVar.t -> 'self_type =
      let open SugarTypeVar in
      function
        | TUnresolved (name, (is_eff, subkind_opt), freedom) ->
           let o = o#name name in
           let o = o#bool is_eff in
           let o = o#option (fun o -> o#subkind) subkind_opt in
           let o = o#freedom freedom in
           o
        | v -> o#unknown v

   method quantifier : SugarQuantifier.t -> 'self_type =
     let open SugarQuantifier in
     function
     | QUnresolved (name, kind, freedom) ->
        let o = o#name name in
        let o = o#kind kind in
        let o = o#freedom freedom in
        o
     | QResolved _  -> o

    method row_var : Datatype.row_var -> 'self_type =
      let open Datatype in function
      | EffectApplication (_x, _x_i1) ->
          let o = o#name _x in
          let o = o#list (fun o -> o#type_arg) _x_i1 in o
      | Closed -> o
      | Open _x ->
          let o = o#type_variable _x in o
      | Recursive ((_x, _x_i1)) ->
          let o = o#type_variable _x in let o = o#row _x_i1 in o

    method row : Datatype.row -> 'self_type =
      fun (_x, _x_i1) ->
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#name _x in let o = o#fieldspec _x_i1 in o)
            _x in
        let o = o#row_var _x_i1 in o

    method replace_rhs : replace_rhs -> 'self_type =
      function
      | Literal    _x -> let o = o#string _x in o
      | SpliceExpr _x -> let o = o#phrase _x in o

    method regexflag : regexflag -> 'self_type =
      fun _ -> o

    method regex : regex -> 'self_type =
      function
      | Range ((_x, _x_i1)) ->
          let o = o#char _x in let o = o#char _x_i1 in o
      | Simply _x -> let o = o#string _x in o
      | Quote _x -> let o = o#regex _x in o
      | Any -> o
      | StartAnchor -> o
      | EndAnchor -> o
      | Seq _x -> let o = o#list (fun o -> o#regex) _x in o
      | Alternate ((_x, _x_i1)) ->
          let o = o#regex _x in let o = o#regex _x_i1 in o
      | Group _x -> let o = o#regex _x in o
      | Repeat ((_x, _x_i1)) ->
          let o = o#unknown _x in let o = o#regex _x_i1 in o
      | Splice _x -> let o = o#phrase _x in o
      | Replace ((_x, _x_i1)) ->
          let o = o#regex _x in let o = o#replace_rhs _x_i1 in o

    method position : Position.t -> 'self_type =
      Position.traverse
        ~o
        ~f_start:(fun o v -> o#unknown v)
        ~f_finish:(fun o v -> o#unknown v)
        ~f_code:(fun o v -> o#unknown v)

    method datatype' : datatype' -> 'self_type =
      fun (x, y) ->
        let o = o#datatype x in
        let o = o#unknown y in
          o

    method row' : row' -> 'self_type =
      fun (x, y) ->
        let o = o#row x in
        let o = o#unknown y in
          o

    method given_spawn_location : given_spawn_location -> 'self_type = function
      | ExplicitSpawnLocation p -> let o = o#phrase p in o
      | _ -> o

    method temporal_update : temporal_update -> 'self_type =
      function
        | ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
            let o = o#phrase validity_from in
            let o = o#phrase validity_to in
            o
        | ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
            let o = o#option (fun o -> o#phrase) from_time in
            let o = o#option (fun o -> o#phrase) to_time in
            o
        | _ -> o

    method temporal_deletion : temporal_deletion -> 'self_type =
      function
        | ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }) ->
            let o = o#phrase validity_from in
            let o = o#phrase validity_to in
            o
        | _ -> o

    method phrasenode : phrasenode -> 'self_type =
      function
      | Constant _x -> let o = o#constant _x in o
      | Var _x -> let o = o#name _x in o
      | FreezeVar _x -> let o = o#name _x in o
      | QualifiedVar _xs ->
          let o = o#list (fun o -> o#name) _xs in o
      | FunLit (_x, _x1, _x_i1, _x_i2) -> let o = o#funlit _x_i1 in let _x_i2 = o#location _x_i2 in o
      | Spawn (_spawn_kind, _given_spawn_location, _block_phr, _dt) ->
         let o = o#given_spawn_location _given_spawn_location in
         let o = o#phrase _block_phr in
         o
      | Query (_x, _policy, _x_i1, _x_i2) ->
          let o =
            o#option
              (fun o (_x, _x_i1) ->
                 let o = o#phrase _x in
                 let o = o#phrase _x_i1 in o)
              _x in
          let o = o#phrase _x_i1 in o
      | DBTemporalJoin (_mode, _block, _ty) ->
          let o = o#phrase _block in
          o
      | ListLit (_x, _x_i1) -> let o = o#list (fun o -> o#phrase) _x in o
      | Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#list (fun o -> o#iterpatt) _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o = o#option (fun o -> o#phrase) _x_i3 in o
      | Escape ((_x, _x_i1)) ->
          let o = o#binder _x in let o = o#phrase _x_i1 in o
      | Section _x -> let o = o#section _x in o
      | FreezeSection _x -> let o = o#section _x in o
      | Conditional ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | Block ((_x, _x_i1)) ->
          let o = o#list (fun o -> o#binding) _x in
          let o = o#phrase _x_i1 in o
      | InfixAppl ((_x, _x_i1, _x_i2)) ->
          let o = o#tybinop _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | RangeLit ((_x_i1, _x_i2)) ->
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | Regex _x -> let o = o#regex _x in o
      | UnaryAppl ((_x, _x_i1)) ->
          let o = o#tyunary_op _x in let o = o#phrase _x_i1 in o
      | FnAppl ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#phrase) _x_i1 in o
      | TAbstr ((_x, _x_i1)) ->
          let o = o#list (fun o -> o#quantifier) (_x) in
          let o = o#phrase _x_i1 in o
      | TAppl ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#type_arg') _x_i1 in
          o
      | TupleLit _x -> let o = o#list (fun o -> o#phrase) _x in o
      | RecordLit ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#phrase) _x_i1 in o
      | Projection ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#name _x_i1 in o
      | With ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x_i1
          in o
      | TypeAnnotation ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#datatype' _x_i1 in o
      | Instantiate _x -> o#phrase _x
      | Generalise _x -> o#phrase _x
      | Upcast ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#datatype' _x_i1 in let o = o#datatype' _x_i2 in o
      | ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let o = o#name _x in
          let o = o#option (fun o -> o#phrase) _x_i1 in o
      | DoOperation (op,ps,t,b) ->
         let o = o#phrase op in
         let o = o#option (fun o -> o#unknown) t in
         let o = o#list (fun o -> o#phrase) ps in
         let o = o#linearity b in o
      | Operation (_x) ->
          let o = o#name _x in o
      | Linlet _x ->
          let o = o#phrase _x in o
      | Unlet _x ->
          let o = o#phrase _x in o
      | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
         let o = o#phrase sh_expr in
         let o =
           o#option (fun o -> o#handle_params) sh_descr.shd_params
         in
         let o =
           o#list
             (fun o (lhs, rhs) ->
               let o = o#pattern lhs in
           let o = o#phrase rhs in o
         )
             sh_effect_cases
     in
         let o =
           o#list
             (fun o (lhs, rhs) ->
               let o = o#pattern lhs in
           let o = o#phrase rhs in o
         )
             sh_value_cases
     in o
      | Switch ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2
          in o
      | Receive ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#unknown) _x_i1
          in o
      (* | Link ((_x, _x_i1)) -> *)
      (*     let o = o#phrase _x in *)
      (*     let o = o#phrase _x_i1 *)
      (*     in o *)
      | Select ((_x, _x_i1)) ->
          let o = o#name _x in
          let o = o#phrase _x_i1
          in o
      | Offer ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#phrase _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2
          in o
      | CP p -> o#cp_phrase p
      | DatabaseLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o =
            (fun (_x, _x_i1) ->
               let o = o#option (fun o -> o#phrase) _x in
               let o = o#option (fun o -> o#phrase) _x_i1 in o)
              _x_i1
          in o
      | TableLit { tbl_name; tbl_type = (_, dt, rows_opt);
          tbl_field_constraints;
          tbl_keys; tbl_database; _ } ->
          let o = o#phrase tbl_name in
          let o = o#datatype dt in
          let o = o#option
            (fun o r ->
               let o = o#unknown r in
                 o) rows_opt in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in
                 let o = o#list (fun o -> o#fieldconstraint) _x_i1 in o)
              tbl_field_constraints in
          let o = o#phrase tbl_keys in
          let o = o#phrase tbl_database in
          o
      | LensLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#option (fun o -> o#unknown) _x_i1 in
            o
      | LensSerialLit ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
            o
      | LensKeysLit ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2 in
            o
      | LensFunDepsLit ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#option (fun o -> o#unknown) _x_i2 in
            o
      | LensDropLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let o = o#phrase _x in
          let o = o#string _x_i1 in
          let o = o#string _x_i2 in
          let o = o#phrase _x_i3 in
          let o = o#option (fun o -> o#unknown) _x_i4 in
            o
      | LensSelectLit ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2 in
            o
      | LensJoinLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in
          let o = o#phrase _x_i2 in
          let o = o#option (fun o -> o#unknown) _x_i5 in
            o
      | LensCheckLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#option (fun o -> o#unknown) _x_i1 in
            o
      | LensGetLit ((_x, _x_i1)) ->
          let o = o#phrase _x in
          let o = o#option (fun o -> o#unknown) _x_i1 in
            o
      | LensPutLit ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#unknown) _x_i2 in
            o
      | DBDelete ((_del, _x, _x_i1, _x_i2)) ->
          let o = o#option (fun o -> o#temporal_deletion) _del in
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in o
      | DBInsert ((_mode, _x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#phrase _x in
          let o = o#list (fun o -> o#name) _x_i1 in
          let o = o#phrase _x_i2 in let o = o#option (fun o -> o#phrase) _x_i3 in o
      | DBUpdate ((_upd, _x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#option (fun o -> o#temporal_update) _upd in
          let o = o#pattern _x in
          let o = o#phrase _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#phrase _x_i1 in o)
              _x_i3
          in o
      | Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
          let o = o#name _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in
                 let o = o#list (fun o -> o#phrase) _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#phrase) _x_i2 in
          let o = o#list (fun o -> o#phrase) _x_i3 in o
      | TextNode _x -> let o = o#string _x in o
      | Formlet ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#phrase _x_i1 in o
      | Page _x -> let o = o#phrase _x in o
      | FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let o = o#phrase _x in
          let o = o#phrase _x_i1 in let o = o#phrase _x_i2 in o
      | PagePlacement _x -> let o = o#phrase _x in o
      | FormBinding ((_x, _x_i1)) ->
          let o = o#phrase _x in let o = o#pattern _x_i1 in o
      | TryInOtherwise (_p1, _pat, _p2, _p3, _ty) ->
          let o = o#phrase _p1 in
          let o = o#pattern _pat in
          let o = o#phrase _p2 in
          let o = o#phrase _p3 in
          o
      | Raise -> o

    method phrase : phrase -> 'self_type =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#phrasenode v)

    method cp_phrasenode : cp_phrasenode -> 'self_type =
      function
      | CPUnquote (bs, e)    -> (o#list (fun o -> o#binding) bs)#phrase e
      | CPGrab (_c, _x, p)   -> o#cp_phrase p
      | CPGive (_c, e, p)    -> (o#option (fun o -> o#phrase) e)#cp_phrase p
      | CPGiveNothing c      -> o#binder c
      | CPSelect (_c, _l, p) -> o#cp_phrase p
      | CPOffer (_c, bs)     -> o#list (fun o (_l, b) -> o#cp_phrase b) bs
      | CPLink (_c, _d)      -> o
      | CPComp (_c, p, q)    -> (o#cp_phrase p)#cp_phrase q

    method cp_phrase : cp_phrase -> 'self_node =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#cp_phrasenode v)

    method patternnode : Pattern.t -> 'self_type =
      let open Pattern in
      function
      | Any -> o
      | Nil -> o
      | Cons ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#pattern _x_i1 in o
      | List _x -> let o = o#list (fun o -> o#pattern) _x in o
      | Variant ((_x, _x_i1)) ->
          let o = o#name _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | Operation (name, ps, k, b) ->
         let o = o#name name in
         let o = o#list (fun o -> o#pattern) ps in
         let o = o#pattern k in
         let o = o#linearity b in
         o
      | Negative _x ->
          let o = o#list (fun o -> o#name) _x in o
      | Record ((_x, _x_i1)) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#name _x in let o = o#pattern _x_i1 in o)
              _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in o
      | Tuple _x -> let o = o#list (fun o -> o#pattern) _x in o
      | Constant _x -> let o = o#constant _x in o
      | Variable _x -> let o = o#binder _x in o
      | As ((_x, _x_i1)) ->
          let o = o#binder _x in let o = o#pattern _x_i1 in o
      | HasType ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#datatype' _x_i1 in o


    method pattern : Pattern.with_pos -> 'self_type =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#patternnode v)

    method foreign_language : ForeignLanguage.t -> 'self_type
      = fun _ -> o

    method name : Name.t -> 'self_type = o#string

    method location : Location.t -> 'self_type = o#unknown

    method iterpatt : iterpatt -> 'self_type =
      function
      | List ((_x, _x_i1)) ->
          let o = o#pattern _x in let o = o#phrase _x_i1 in o
      | Table ((_t, _x, _x_i1)) ->
          let o = o#pattern _x in let o = o#phrase _x_i1 in o

    method funlit : funlit -> 'self_type =
      fun f ->
        match f with
          | NormalFunlit (_x, _x_i1) ->
            let o = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
            let o = o#phrase _x_i1 in o
          | SwitchFunlit (pat, body) ->
            let o = o#list (fun o -> o#list (fun o -> o#pattern)) pat in
            let o = o#list (fun o (p, c) ->
              let o = o#pattern p in
              let o = o#phrase c in o) body in
            o


    method handle_params : handler_parameterisation -> 'self_type =
      fun params ->
        o#list
          (fun o (pat, expr) ->
            let o = o#phrase expr in
            o#pattern pat)
          params.shp_bindings

    method fieldspec : Datatype.fieldspec -> 'self_type =
      let open Datatype in function
      | Present _x -> let o = o#datatype _x in o
      | Absent -> o
      | Var _x -> let o = o#type_variable _x in o

    method fieldconstraint : fieldconstraint -> 'self_type =
      fun _ -> o

    method directive : directive -> 'self_type =
      fun (_x, _x_i1) ->
        let o = o#string _x in let o = o#list (fun o -> o#string) _x_i1 in o

    method datatypenode : Datatype.t -> 'self_type =
      let open Datatype in
      function
      | TypeVar _x ->
          let o = o#type_variable _x in o
      | QualifiedTypeApplication (ns, args) ->
          let o = o#list (fun o -> o#name) ns in
          let o = o#list (fun o -> o#type_arg) args in
          o
      | Function (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#datatype) _x in
          let o = o#row _x_i1 in let o = o#datatype _x_i2 in o
      | Lolli (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#datatype) _x in
          let o = o#row _x_i1 in let o = o#datatype _x_i2 in o
      | Mu (_x, _x_i1) ->
          let o = o#type_variable _x in
          let o = o#datatype _x_i1 in o
      | Forall (_x, _x_i1) ->
          let o = o#datatype _x_i1 in o
      | Unit -> o
      | Tuple _x -> let o = o#list (fun o -> o#datatype) _x in o
      | Record _x -> let o = o#row _x in o
      | Variant _x -> let o = o#row _x in o
      | Effect r -> let o = o#row r in o
      | Operation (_x, _x_i1, _x_i2) ->
        let o = o#list (fun o -> o#datatype) _x in
        let o = o#datatype _x_i1 in
        let o = o#linearity _x_i2 in o
      | Table (_t, _x, _x_i1, _x_i2) ->
          let o = o#datatype _x in
          let o = o#datatype _x_i1 in
          let o = o#datatype _x_i2 in o
      | List _x -> let o = o#datatype _x in o
      | TypeApplication (_x, _x_i1) ->
          let o = o#name _x in
          let o = o#list (fun o -> o#type_arg) _x_i1 in o
      | Primitive _x -> let o = o#unknown _x in o
      | DB -> o
      | Input (_x, _x_i1) ->
        let o = o#datatype _x in
        let o = o#datatype _x_i1 in o
      | Output (_x, _x_i1) ->
        let o = o#datatype _x in
        let o = o#datatype _x_i1 in o
      | Select _x ->
        let o = o#row _x in o
      | Choice _x ->
        let o = o#row _x in o
      | Dual _x ->
        let o = o#datatype _x in o
      | End -> o

    method datatype : Datatype.with_pos -> 'self_type =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#datatypenode v)

    method type_arg : Datatype.type_arg -> 'self_type =
      let open Datatype in function
      | Type _x -> let o = o#datatype _x in o
      | Row _x -> let o = o#row _x in o
      | Presence _x -> let o = o#fieldspec _x in o

    method type_arg' : type_arg' -> 'self_type =
      fun (x, y) ->
        let o = o#type_arg x in
        let o = o#unknown y in
        o

    method constant : Constant.t -> 'self_type =
      function
      | Constant.Float  _x -> let o = o#float  _x in o
      | Constant.Int    _x -> let o = o#int    _x in o
      | Constant.String _x -> let o = o#string _x in o
      | Constant.Bool   _x -> let o = o#bool   _x in o
      | Constant.Char   _x -> let o = o#char   _x in o
      | Constant.DateTime   _x -> let o = o#timestamp   _x in o

    method binop : BinaryOp.t -> 'self_type =
      let open BinaryOp in function
      | Minus -> o
      | FloatMinus -> o
      | RegexMatch _x -> let o = o#list (fun o -> o#regexflag) _x in o
      | And -> o
      | Or -> o
      | Cons -> o
      | Name _x -> let o = o#name _x in o

    method tybinop : tyarg list * BinaryOp.t -> 'self_type =
      fun (_x, _x_i1) -> o#binop _x_i1

    method bindingnode : bindingnode -> 'self_type =
      function
      | Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let o = o#pattern _x in
          let o = o#list (fun o -> o#quantifier) _x_i1 in
          let o = o#phrase _x_i2 in
          let o = o#location _x_i3 in
          let o = o#option (fun o -> o#datatype') _x_i4 in o
      | Fun f -> o#function_definition f
      | Funs _x ->
          let o = o#list (fun o -> o#recursive_function) _x in
          o
      | Foreign alien ->
         let o =
           o#list
             (fun o (b, dt) ->
               let o = o#binder b in
               o#datatype' dt)
             (Alien.declarations alien)
         in
         o#foreign_language (Alien.language alien)
      | Import { path; _ } ->
         let o = o#list (fun o -> o#name) path in
          o
      | Open _xs ->
          let o = o#list (fun o -> o#name) _xs in
          o
      | Aliases ts ->
          let o = o#list (fun o -> o#alias) ts in
          o
      | Infix { name; _ } ->
         o#name name
      | Exp _x -> let o = o#phrase _x in o
      | Module { binder; members } ->
          let o = o#binder binder in
          o#list (fun o -> o#binding) members
      | AlienBlock alien ->
         let o = o#foreign_language (Alien.language alien) in
         o#list
           (fun o (b, dt) ->
             let o = o#binder b in
             o#datatype' dt)
           (Alien.declarations alien)

    method binding : binding -> 'self_type =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#bindingnode v)

    method aliasnode : aliasnode -> 'self_type =
      fun (_x, _x_i1, _x_i2) ->
      let o = o#name _x in
      let o =
        o#list
          (fun o _x ->
            let o = o#quantifier _x
            in o) _x_i1 in
      let o = o#aliasbody _x_i2 in
      o

    method aliasbody : aliasbody -> 'self_type =
      function
        | Typename _x -> o#datatype' _x
        | Effectname _x -> o#row' _x

    method alias : alias -> 'self_type =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#aliasnode v)

    method function_definition : function_definition -> 'self
      = fun { fun_binder;
              fun_linearity = _;
              fun_definition = (tyvar, lit);
              fun_location;
              fun_signature;
              fun_frozen = _;
              fun_unsafe_signature = _ } ->
          let o = o#binder fun_binder in
          let o = o#list (fun o -> o#quantifier) tyvar in
          let o = o#funlit lit in
          let o = o#location fun_location in
          let o = o#option (fun o -> o#datatype') fun_signature in
          o

    method recursive_functionnode : recursive_functionnode -> 'self
      = fun { rec_binder;
              rec_linearity = _;
              rec_definition = ((tyvar, _), lit);
              rec_location;
              rec_signature;
              rec_unsafe_signature = _;
              rec_frozen = _} ->
      let o = o#binder rec_binder in
      let o = o#list (fun o -> o#quantifier) tyvar in
      let o = o#funlit lit in
      let o = o#location rec_location in
      let o = o#option (fun o -> o#datatype') rec_signature in
      o

    method recursive_function : recursive_function -> 'self =
      WithPos.traverse
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#recursive_functionnode v)

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

    method timestamp : Timestamp.t -> ('self_type * Timestamp.t) = o#unknown

    method bool : bool -> ('self_type * bool) =
      function | false -> (o, false) | true -> (o, true)

    method linearity : DeclaredLinearity.t -> ('self_type * DeclaredLinearity.t) =
      DeclaredLinearity.(function | Unl -> (o, Unl) | Lin -> (o, Lin))

    method unary_op : UnaryOp.t -> ('self_type * UnaryOp.t) =
      let open UnaryOp in function
      | Minus -> (o, Minus)
      | FloatMinus -> (o, FloatMinus)
      | Name _x -> let (o, _x) = o#name _x in (o, Name _x)

    method tyunary_op : tyarg list * UnaryOp.t -> 'self_type * (tyarg list * UnaryOp.t) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#tyarg) _x in
        let (o, _x_i1) = o#unary_op _x_i1 in (o, (_x, _x_i1))

    method sentence : sentence -> ('self_type * sentence) =
      function
      | Definitions _x ->
          let (o, _x) = o#list (fun o -> o#binding) _x
          in (o, Definitions _x)
      | Expression _x -> let (o, _x) = o#phrase _x in (o, Expression _x)
      | Directive _x -> let (o, _x) = o#directive _x in (o, Directive _x)

    method section : Section.t -> ('self_type * Section.t) =
      let open Section in function
      | Minus -> (o, Minus)
      | FloatMinus -> (o, FloatMinus)
      | Project _x -> let (o, _x) = o#name _x in (o, Project _x)
      | Name _x -> let (o, _x) = o#name _x in (o, Name _x)

    method subkind : Subkind.t -> ('self_type * Subkind.t) = fun k -> (o, k)

    method kind : kind -> ('self_type * kind) = fun k -> (o, k)

    method freedom : Freedom.t -> ('self_type * Freedom.t) = fun k -> (o, k)

    method type_variable : SugarTypeVar.t -> ('self_type * SugarTypeVar.t) =
      let open SugarTypeVar in
      function
        | TUnresolved (name, (is_eff, subkind_opt), freedom) ->
           let o, name' = o#name name in
           let o, is_eff' = o#bool is_eff in
           let o, subkind_opt' = o#option (fun o -> o#subkind) subkind_opt in
           let o, freedom' = o#freedom freedom in
           o, TUnresolved (name', (is_eff', subkind_opt'), freedom')
        | v -> o#unknown v


    method quantifier : SugarQuantifier.t -> ('self_type * SugarQuantifier.t) =
      let open SugarQuantifier in
      function
      | QUnresolved (name, kind, freedom) ->
         let o, name' = o#name name in
         let o, kind' = o#kind kind in
         let o, freedom' = o#freedom freedom in
         o, QUnresolved (name', kind', freedom')
      | (QResolved _) as rq -> o, rq


    method row_var : Datatype.row_var -> ('self_type * Datatype.row_var) =
      let open Datatype in function
      | EffectApplication (_x, _x_i1) ->
          let (o, _x) = o#string _x in
          let (o, _x_i1) = o#list (fun o -> o#type_arg) _x_i1
          in (o, EffectApplication (_x, _x_i1))
      | Closed -> (o, Closed)
      | Open _x ->
          let (o, _x) = o#type_variable _x in (o, (Open _x))
      | Recursive ((_x, _x_i1)) ->
          let (o, _x) = o#type_variable _x in
          let (o, _x_i1) = o#row _x_i1 in (o, Recursive ((_x, _x_i1)))

    method row : Datatype.row -> ('self_type * Datatype.row) =
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
      | Literal    _x -> let (o, _x) = o#string _x in (o, (Literal _x))
      | SpliceExpr _x -> let (o, _x) = o#phrase _x in (o, (SpliceExpr _x))

    method regexflag : regexflag -> ('self_type * regexflag) =
      fun flag -> (o, flag)

    method regex : regex -> ('self_type * regex) =
      function
      | Range ((_x, _x_i1)) ->
          let (o, _x) = o#char _x in
          let (o, _x_i1) = o#char _x_i1 in (o, (Range ((_x, _x_i1))))
      | Simply _x -> let (o, _x) = o#string _x in (o, (Simply _x))
      | Quote _x -> let (o, _x) = o#regex _x in (o, (Quote _x))
      | Any -> (o, Any)
      | StartAnchor -> (o, StartAnchor)
      | EndAnchor -> (o, EndAnchor)
      | Seq _x ->
          let (o, _x) = o#list (fun o -> o#regex) _x in (o, (Seq _x))
      | Alternate ((_x, _x_i1)) ->
          let (o, _x) = o#regex _x in
          let (o, _x_i1) = o#regex _x_i1 in (o, (Alternate ((_x, _x_i1))))
      | Group _x -> let (o, _x) = o#regex _x in (o, (Group _x))
      | Repeat ((_x, _x_i1)) ->
          let (o, _x) = o#unknown _x in
          let (o, _x_i1) = o#regex _x_i1 in (o, (Repeat ((_x, _x_i1))))
      | Splice _x -> let (o, _x) = o#phrase _x in (o, (Splice _x))
      | Replace ((_x, _x_i1)) ->
          let (o, _x) = o#regex _x in
          let (o, _x_i1) = o#replace_rhs _x_i1
          in (o, (Replace ((_x, _x_i1))))

    method program : program -> ('self_type * program) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#binding) _x in
        let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
        in (o, (_x, _x_i1))

    method position : Position.t -> ('self_type * Position.t) =
      Position.traverse_map
        ~o
        ~f_start:(fun o v -> o#unknown v)
        ~f_finish:(fun o v -> o#unknown v)
        ~f_code:(fun o v -> o#option (fun o -> o#unknown) v)

    method given_spawn_location : given_spawn_location -> ('self_type * given_spawn_location) = function
      | ExplicitSpawnLocation _p -> let (o, _p) = o#phrase _p in (o, ExplicitSpawnLocation _p)
      | l -> (o, l)

    method temporal_update : temporal_update -> ('self_type * temporal_update) =
      function
        | ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
            let (o, validity_from) = o#phrase validity_from in
            let (o, validity_to) = o#phrase validity_to in
            (o, ValidTimeUpdate (SequencedUpdate { validity_from; validity_to } ))
        | ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
            let (o, from_time) = o#option (fun o -> o#phrase) from_time in
            let (o, to_time) = o#option (fun o -> o#phrase) to_time in
            (o, ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }))
        | x -> (o, x)

    method temporal_deletion : temporal_deletion -> ('self_type * temporal_deletion) =
      function
        | ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }) ->
            let (o, validity_from) = o#phrase validity_from in
            let (o, validity_to) = o#phrase validity_to in
            (o, ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }))
        | x -> (o, x)

    method phrasenode : phrasenode -> ('self_type * phrasenode) =
      function
      | Constant _x -> let (o, _x) = o#constant _x in (o, (Constant _x))
      | Var _x -> let (o, _x) = o#name _x in (o, (Var _x))
      | FreezeVar _x -> let (o, _x) = o#name _x in (o, (FreezeVar _x))
      | QualifiedVar _xs ->
          let (o, _xs) = o#list (fun o n -> o#name n) _xs in
          (o, (QualifiedVar _xs))
      | FunLit (_x, _x1, _x_i1, _x_i2) ->
        let (o, _x_i1) = o#funlit _x_i1 in
        let handle_funtype o (t, r) =
          let (o, t) = o#typ t in
          let (o,r) = o#type_row r in
          (o, (t,r))
        in
        let (o, _x) = o#option (fun o -> o#list handle_funtype) _x in
        let (o, _x_i2) = o#location _x_i2 in (o, (FunLit (_x, _x1, _x_i1, _x_i2)))
      | Spawn (_spawn_kind, _given_spawn_location, _block_phr, _dt) ->
          let (o, _given_spawn_location) = o#given_spawn_location _given_spawn_location in
          let (o, _block_phr) = o#phrase _block_phr in
          let (o, _dt) = o#option (fun o -> o#type_row) _dt in
          (o, (Spawn (_spawn_kind, _given_spawn_location, _block_phr, _dt)))
      | Query (_x, _policy, _x_i1, _x_i2) ->
          let (o, _x) =
            o#option
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#phrase _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (Query (_x, _policy, _x_i1, _x_i2)))
      | DBTemporalJoin (_mode, _block, _ty) ->
          let (o, _block) = o#phrase _block in
          (o, DBTemporalJoin (_mode, _block, _ty))
      | ListLit (_x, _x_i1) ->
          let (o, _x) = o#list (fun o -> o#phrase) _x in
          let (o, _x_i1) = o#option (fun o -> o#typ) _x_i1 in
          (o, (ListLit (_x, _x_i1)))
      | RangeLit ((_x_i1, _x_i2)) ->
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (RangeLit ((_x_i1, _x_i2))))
      | Iteration ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#list (fun o -> o#iterpatt) _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2 in
          let (o, _x_i3) = o#option (fun o -> o#phrase) _x_i3
          in (o, (Iteration ((_x, _x_i1, _x_i2, _x_i3))))
      | Escape ((_x, _x_i1)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (Escape ((_x, _x_i1))))
      | Section _x -> let (o, _x) = o#section _x in (o, (Section _x))
      | FreezeSection _x -> let (o, _x) = o#section _x in (o, (FreezeSection _x))
      | Conditional ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (Conditional ((_x, _x_i1, _x_i2))))
      | Block ((_x, _x_i1)) ->
          let (o, _x) = o#list (fun o -> o#binding) _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (Block ((_x, _x_i1))))
      | InfixAppl ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#tybinop _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (InfixAppl ((_x, _x_i1, _x_i2))))
      | Regex _x -> let (o, _x) = o#regex _x in (o, (Regex _x))
      | UnaryAppl ((_x, _x_i1)) ->
          let (o, _x) = o#tyunary_op _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (UnaryAppl ((_x, _x_i1))))
      | FnAppl ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#list (fun o -> o#phrase) _x_i1
          in (o, (FnAppl ((_x, _x_i1))))
      | TAbstr ((_x, _x_i1)) ->
          let o, _x = o#list (fun o -> o#quantifier) _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          (o, (TAbstr ((_x, _x_i1))))
      | TAppl ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#list (fun o -> o#type_arg') _x_i1 in
          (o, (TAppl ((_x, _x_i1))))
      | TupleLit _x ->
          let (o, _x) = o#list (fun o -> o#phrase) _x in (o, (TupleLit _x))
      | RecordLit ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
          in (o, (RecordLit ((_x, _x_i1))))
      | Projection ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#name _x_i1 in (o, (Projection ((_x, _x_i1))))
      | With ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1
          in (o, (With ((_x, _x_i1))))
      | TypeAnnotation ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#datatype' _x_i1
          in (o, (TypeAnnotation ((_x, _x_i1))))
      | Upcast ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#datatype' _x_i1 in
          let (o, _x_i2) = o#datatype' _x_i2
          in (o, (Upcast ((_x, _x_i1, _x_i2))))
      | Instantiate _x ->
          let (o, _x) = o#phrase _x in
          (o, Instantiate _x)
      | Generalise _x ->
          let (o, _x) = o#phrase _x in
          (o, Generalise _x)
      | ConstructorLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1 in
          let o, _x_i2 = o#option (fun o -> o#typ) _x_i2 in
          (o, (ConstructorLit ((_x, _x_i1, _x_i2))))
      | DoOperation (op, ps, t, b) ->
          let (o, op) = o#phrase op in
          let (o, t) = o#option (fun o -> o#typ) t in
          let (o, ps) = o#list (fun o -> o#phrase) ps in
          (o, DoOperation (op, ps, t, b))
      | Operation _x ->
          let (o, _x) = o#name _x in
          (o, Operation _x)
      | Linlet _x ->
          let (o, _x) = o#phrase _x in
          (o, Linlet _x)
      | Unlet _x ->
          let (o, _x) = o#phrase _x in
          (o, Unlet _x)
      | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
          let (o, m) = o#phrase sh_expr in
          let (o, params) =
            o#option (fun o -> o#handle_params) sh_descr.shd_params
          in
          let (o, eff_cases) =
            o#list
              (fun o (lhs, rhs) ->
                 let (o, lhs) = o#pattern lhs in
                 let (o, rhs) = o#phrase rhs in (o, (lhs, rhs))
          )
              sh_effect_cases
      in
          let (o, val_cases) =
            o#list
              (fun o (lhs, rhs) ->
                 let (o, lhs) = o#pattern lhs in
                 let (o, rhs) = o#phrase rhs in (o, (lhs, rhs))
          )
              sh_value_cases
      in
          (o, (Handle { sh_expr = m; sh_effect_cases = eff_cases; sh_value_cases = val_cases; sh_descr = { sh_descr with shd_params = params } }))
      | Switch ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#typ) _x_i2
          in (o, (Switch ((_x, _x_i1, _x_i2))))
      | Receive ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#typ) _x_i1
          in (o, (Receive ((_x, _x_i1))))
      (* | Link ((_x, _x_i1)) -> *)
      (*     let (o, _x) = o#phrase _x in *)
      (*     let (o, _x_i1) = o#phrase _x in (o, (Link(_x, _x_i1))) *)
      | Select ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#phrase _x_i1
          in (o, (Select (_x, _x_i1)))
      | Offer ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#pattern _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2
          in (o, (Offer ((_x, _x_i1, _x_i2))))
      | CP p ->
         let (o, p) = o#cp_phrase p in
         o, CP p
      | DatabaseLit ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) =
            (fun (_x, _x_i1) ->
               let (o, _x) = o#option (fun o -> o#phrase) _x in
               let (o, _x_i1) = o#option (fun o -> o#phrase) _x_i1
               in (o, (_x, _x_i1)))
              _x_i1
          in (o, (DatabaseLit ((_x, _x_i1))))
      | TableLit { tbl_name; tbl_type; tbl_field_constraints;
          tbl_keys; tbl_temporal_fields; tbl_database } ->
            (*
      | TableLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          *)
          let (o, tbl_name) = o#phrase tbl_name in
          let (o, tbl_type) =
            (fun (tmp, dt, rows_opt) ->
               let (o, dt) = o#datatype dt in
               let (o, rows_opt) =
                 o#option
                   (fun o (a, b, c) ->
                     let o, a = o#typ a in
                     let o, b = o#typ b in
                     let o, c = o#typ c in
                     o, (a, b, c)) rows_opt
               in (o, (tmp, dt, rows_opt)))
              tbl_type in
          let (o, tbl_field_constraints) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#list (fun o -> o#fieldconstraint) _x_i1
                 in (o, (_x, _x_i1)))
              tbl_field_constraints in
          let (o, tbl_keys) = o#phrase tbl_keys in
          let (o, tbl_database) = o#phrase tbl_database in
          let tbl =
            TableLit { tbl_name; tbl_type; tbl_field_constraints;
            tbl_keys; tbl_temporal_fields; tbl_database }
          in
          (o, tbl)
      | LensLit ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1 in
            (o, (LensLit (_x, _x_i1)))
      | LensSerialLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
            (o, (LensSerialLit (_x, _x_i1, _x_i2)))
      | LensKeysLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2 in
            (o, (LensKeysLit (_x, _x_i1, _x_i2)))
      | LensFunDepsLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2 in
            (o, (LensFunDepsLit (_x, _x_i1, _x_i2)))
      | LensDropLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#string _x_i2 in
          let (o, _x_i3) = o#phrase _x_i3 in
          let (o, _x_i4) = o#option (fun o -> o#unknown) _x_i4 in
            (o, (LensDropLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4))))
      | LensSelectLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#unknown) _x_i2 in
            (o, (LensSelectLit ((_x, _x_i1, _x_i2))))

      | LensJoinLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2 in
          let (o, _x_i5) = o#option (fun o -> o#unknown) _x_i5 in
            (o, (LensJoinLit ((_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5))))
      | LensCheckLit ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#option (fun o -> o#unknown) _x_i1 in
            (o, (LensCheckLit ((_x, _x_i1))))
      | LensGetLit ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#option (fun o -> o#typ) _x_i1 in
            (o, (LensGetLit ((_x, _x_i1))))
      | LensPutLit ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#typ) _x_i2 in
            (o, (LensPutLit ((_x, _x_i1, _x_i2))))
      | DBDelete ((_del, _x, _x_i1, _x_i2)) ->
          let (o, _del) = o#option (fun o -> o#temporal_deletion) _del in
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2
          in (o, (DBDelete ((_del, _x, _x_i1, _x_i2))))
      | DBInsert ((_mode, _x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#list (fun o -> o#name) _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2 in
          let (o, _x_i3) = o#option (fun o -> o#phrase) _x_i3
          in (o, (DBInsert ((_mode, _x, _x_i1, _x_i2, _x_i3))))
      | DBUpdate ((_upd, _x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _upd) = o#option (fun o -> o#temporal_update) _upd in
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#option (fun o -> o#phrase) _x_i2 in
          let (o, _x_i3) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#phrase _x_i1 in (o, (_x, _x_i1)))
              _x_i3
          in (o, (DBUpdate ((_upd, _x, _x_i1, _x_i2, _x_i3))))
      | Xml ((_x, _x_i1, _x_i2, _x_i3)) ->
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
          in (o, (Xml ((_x, _x_i1, _x_i2, _x_i3))))
      | TextNode _x -> let (o, _x) = o#string _x in (o, (TextNode _x))
      | Formlet ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (Formlet ((_x, _x_i1))))
      | Page _x -> let (o, _x) = o#phrase _x in (o, (Page _x))
      | FormletPlacement ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#phrase _x_i1 in
          let (o, _x_i2) = o#phrase _x_i2
          in (o, (FormletPlacement ((_x, _x_i1, _x_i2))))
      | PagePlacement _x ->
          let (o, _x) = o#phrase _x in (o, (PagePlacement _x))
      | FormBinding ((_x, _x_i1)) ->
          let (o, _x) = o#phrase _x in
          let (o, _x_i1) = o#pattern _x_i1
          in (o, (FormBinding ((_x, _x_i1))))
      | TryInOtherwise (_p1, _pat, _p2, _p3, _ty) ->
          let (o, _p1) = o#phrase _p1 in
          let (o, _pat) = o#pattern _pat in
          let (o, _p2) = o#phrase _p2 in
          let (o, _p3) = o#phrase _p3 in
          let o, _ty = o#option (fun o -> o#typ) _ty in
          (o, (TryInOtherwise (_p1, _pat, _p2, _p3, _ty)))
      | Raise -> (o, Raise)

    method phrase : phrase -> ('self_type * phrase) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#phrasenode v)

    method cp_phrasenode : cp_phrasenode -> ('self_type * cp_phrasenode) =
      let arg_pair (o : 'self_type) =
        o#option (fun o (dt, args) ->
            let o, dt = o#typ dt in
            let o, args = o#list (fun o -> o#tyarg) args in
            let o, dt = o#typ dt in
            o, (dt, args))
      in

      function
      | CPUnquote (bs, e) ->
         let o, bs = o#list (fun o -> o#binding) bs in
         let o, e = o#phrase e in
         o, CPUnquote (bs, e)
      | CPGrab ((c, a), x, p) ->
         let o, a = arg_pair o a in
         let o, p = o#cp_phrase p in
         o, CPGrab ((c, a), x, p)
      | CPGive ((c, a), e, p) ->
         let o, a = arg_pair o a in
         let o, e = o#option (fun o -> o#phrase) e in
         let o, p = o#cp_phrase p in
         o, CPGive ((c, a), e, p)
      | CPGiveNothing c ->
         let o, c = o#binder c in
         o, CPGiveNothing c
      | CPSelect (c, l, p) ->
         let o, p = o#cp_phrase p in
         o, CPSelect (c, l, p)
      | CPOffer (c, bs) ->
         let o, bs = o#list (fun o (l, p) ->
                             let o, p = o#cp_phrase p in
                             o, (l, p)) bs in
         o, CPOffer (c, bs)
      | CPLink (c, d) ->
         o, CPLink (c, d)
      | CPComp (c, p, q) ->
         let o, p = o#cp_phrase p in
         let o, q = o#cp_phrase q in
         o, CPComp (c, p, q)

    method cp_phrase : cp_phrase -> ('self_type * cp_phrase) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#cp_phrasenode v)

    method patternnode : Pattern.t -> ('self_type * Pattern.t) =
      let open Pattern in
      function
      | Any -> (o, Any)
      | Nil -> (o, Nil)
      | Cons ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#pattern _x_i1 in (o, (Cons ((_x, _x_i1))))
      | List _x ->
          let (o, _x) = o#list (fun o -> o#pattern) _x in (o, (List _x))
      | Variant ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#option (fun o -> o#pattern) _x_i1
          in (o, (Variant ((_x, _x_i1))))
      | Operation (name, ps, k, b) ->
         let (o, name) = o#name name in
         let (o, ps) = o#list (fun o -> o#pattern) ps in
         let (o, k) = o#pattern k in
         let (o, b) = o#linearity b in
         (o, Operation (name, ps, k, b))
      | Negative _x ->
          let (o, _x) = o#list (fun o -> o#name) _x in (o, (Negative _x))
      | Record ((_x, _x_i1)) ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#name _x in
                 let (o, _x_i1) = o#pattern _x_i1 in (o, (_x, _x_i1)))
              _x in
          let (o, _x_i1) = o#option (fun o -> o#pattern) _x_i1
          in (o, (Record ((_x, _x_i1))))
      | Tuple _x ->
          let (o, _x) = o#list (fun o -> o#pattern) _x in (o, (Tuple _x))
      | Constant _x -> let (o, _x) = o#constant _x in (o, (Constant _x))
      | Variable _x -> let (o, _x) = o#binder _x in (o, (Variable _x))
      | As ((_x, _x_i1)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#pattern _x_i1 in (o, (As ((_x, _x_i1))))
      | HasType ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#datatype' _x_i1 in (o, (HasType ((_x, _x_i1))))

    method pattern : Pattern.with_pos -> ('self_type * Pattern.with_pos) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#patternnode v)

    method foreign_language : ForeignLanguage.t -> ('self_type * ForeignLanguage.t)
      = fun lang -> o, lang

    method name : Name.t -> ('self_type * Name.t) = o#string

    method location : Location.t -> ('self_type * Location.t) = o#unknown

    method iterpatt : iterpatt -> ('self_type * iterpatt) =
      function
      | List ((_x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (List ((_x, _x_i1))))
      | Table ((_t, _x, _x_i1)) ->
          let (o, _x) = o#pattern _x in
          let (o, _x_i1) = o#phrase _x_i1 in (o, (Table ((_t, _x, _x_i1))))

    method funlit : funlit -> ('self_type * funlit) =
      fun f ->
        match f with
          | NormalFunlit (_x, _x_i1) ->
            let (o, _x) = o#list (fun o -> o#list (fun o -> o#pattern)) _x in
            let (o, _x_i1) = o#phrase _x_i1 in (o, NormalFunlit (_x, _x_i1))
          | SwitchFunlit (pat, body) ->
            let (o, pat) = o#list (fun o -> o#list (fun o -> o#pattern)) pat in
            let (o, body) =
              o#list (fun o (p, c) ->
                let (o, p) = o#pattern p in
                let (o, c) = o#phrase c in
                (o, (p, c))) body in
            (o, SwitchFunlit (pat, body))

    method handle_params : handler_parameterisation -> ('self_type * handler_parameterisation) =
      fun { shp_bindings; shp_types } ->
        let (o, shp_bindings) =
          o#list
            (fun o (pat, expr) ->
              let (o, expr) = o#phrase expr in
              let (o, pat) = o#pattern pat in
              (o, (pat, expr)))
            shp_bindings
        in
        let o, shp_types = o#list (fun o -> o#typ) shp_types in
        (o, { shp_bindings; shp_types })

    method fieldspec : Datatype.fieldspec -> ('self_type * Datatype.fieldspec) =
      let open Datatype in function
      | Present _x -> let (o, _x) = o#datatype _x in (o, Present _x)
      | Absent -> (o, Absent)
      | Var _x -> let (o, _x) = o#type_variable _x in (o, Var _x)

    method fieldconstraint : fieldconstraint -> ('self_type * fieldconstraint) =
      fun fc -> (o, fc)

    method directive : directive -> ('self_type * directive) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#string _x in
        let (o, _x_i1) = o#list (fun o -> o#string) _x_i1 in (o, (_x, _x_i1))

    method datatype' : datatype' -> ('self_type * datatype') =
      fun (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#option (fun o -> o#typ) _x_i1
        in (o, (_x, _x_i1))

    method row' : row' -> ('self_type * row') =
      fun (_x, _x_i1) ->
        let (o, _x) = o#row _x in
        let (o, _x_i1) = o#option (fun o -> o#typ) _x_i1
        in (o, (_x, _x_i1))

    method datatypenode : Datatype.t -> ('self_type * Datatype.t) =
      let open Datatype in
      function
      | TypeVar _x ->
          let (o, _x) = o#type_variable _x in (o, (TypeVar _x))
      | QualifiedTypeApplication (ns, args) ->
          let (o, ns) = o#list (fun o -> o#name) ns in
          let (o, args) = o#list (fun o -> o#type_arg) args in
          (o, QualifiedTypeApplication (ns, args))
      | Function (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#list (fun o -> o#datatype) _x in
          let (o, _x_i1) = o#row _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2
          in (o, (Function (_x, _x_i1, _x_i2)))
      | Lolli (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#list (fun o -> o#datatype) _x in
          let (o, _x_i1) = o#row _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2
          in (o, (Lolli (_x, _x_i1, _x_i2)))
      | Mu (_x, _x_i1) ->
          let (o, _x) = o#type_variable _x in
          let (o, _x_i1) = o#datatype _x_i1 in (o, (Mu (_x, _x_i1)))
      | Forall (_x, _x_i1) ->
          (*let (o, _x) = o#list (fun o -> o#quantifier) _x in*)
          let (o, _x_i1) = o#datatype _x_i1 in (o, (Forall (_x, _x_i1)))
      | Unit -> (o, Unit)
      | Tuple _x ->
          let (o, _x) = o#list (fun o -> o#datatype) _x
          in (o, (Tuple _x))
      | Record _x -> let (o, _x) = o#row _x in (o, (Record _x))
      | Variant _x -> let (o, _x) = o#row _x in (o, (Variant _x))
      | Effect r -> let (o, r) = o#row r in (o, Effect r)
      | Operation (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#list (fun o -> o#datatype) _x in
        let (o, _x_i1) = o#datatype _x_i1 in
        let (o, _x_i2) = o#linearity _x_i2 in
        (o, Operation (_x, _x_i1, _x_i2))
      | Table (_t, _x, _x_i1, _x_i2) ->
          let (o, _x) = o#datatype _x in
          let (o, _x_i1) = o#datatype _x_i1 in
          let (o, _x_i2) = o#datatype _x_i2 in (o, (Table (_t, _x, _x_i1, _x_i2)))
      | List _x -> let (o, _x) = o#datatype _x in (o, (List _x))
      | TypeApplication (_x, _x_i1) ->
          let (o, _x) = o#string _x in
          let (o, _x_i1) = o#list (fun o -> o#type_arg) _x_i1
          in (o, TypeApplication (_x, _x_i1))
      | Primitive _x ->
          let (o, _x) = o#unknown _x in (o, (Primitive _x))
      | DB -> (o, DB)
      | Input (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#datatype _x_i1 in (o, Input (_x, _x_i1))
      | Output (_x, _x_i1) ->
        let (o, _x) = o#datatype _x in
        let (o, _x_i1) = o#datatype _x_i1 in (o, Output (_x, _x_i1))
      | Select _x ->
        let (o, _x) = o#row _x in (o, Select _x)
      | Choice _x ->
        let (o, _x) = o#row _x in (o, Choice _x)
      | Dual _x ->
        let (o, _x) = o#datatype _x in (o, Dual _x)
      | End -> (o, End)

    method datatype : Datatype.with_pos -> ('self_type * Datatype.with_pos) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#datatypenode v)

    method type_arg : Datatype.type_arg -> ('self_type * Datatype.type_arg) =
      let open Datatype in function
      | Type _x -> let (o, _x) = o#datatype _x in (o, Type _x)
      | Row _x -> let (o, _x) = o#row _x in (o, Row _x)
      | Presence _x -> let (o, _x) = o#fieldspec _x in (o, Presence _x)

    method type_arg' : type_arg' -> ('self_type * type_arg') =
      fun (x, y) ->
        let o, x = o#type_arg x in
        let o, y = o#option (fun o -> o#tyarg) y in
        (o, (x, y))

    method constant : Constant.t -> ('self_type * Constant.t) =
      function
      | Constant.Float _x ->
         let (o, _x) = o#float _x in (o, (Constant.Float _x))
      | Constant.Int _x ->
         let (o, _x) = o#int _x in (o, (Constant.Int _x))
      | Constant.String _x ->
         let (o, _x) = o#string _x in (o, (Constant.String _x))
      | Constant.Bool _x ->
         let (o, _x) = o#bool _x in (o, (Constant.Bool _x))
      | Constant.Char _x ->
         let (o, _x) = o#char _x in (o, (Constant.Char _x))
      | Constant.DateTime _x ->
         let (o, _x) = o#timestamp _x in (o, (Constant.DateTime _x))

    method binop : BinaryOp.t -> ('self_type * BinaryOp.t) =
      let open BinaryOp in function
      | Minus -> (o, Minus)
      | FloatMinus -> (o, FloatMinus)
      | RegexMatch _x ->
          let (o, _x) = o#list (fun o -> o#regexflag) _x
          in (o, (RegexMatch _x))
      | And -> (o, And)
      | Or -> (o, Or)
      | Cons -> (o, Cons)
      | Name _x -> let (o, _x) = o#name _x in (o, (Name _x))

    method tybinop : tyarg list * BinaryOp.t -> 'self_type * (tyarg list * BinaryOp.t) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#tyarg) _x in
        let (o, _x_i1) = o#binop _x_i1 in (o, (_x, _x_i1))

    method bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4)) ->
          let (o, _x   ) = o#pattern _x in
          let (o, _x_i2) = o#phrase _x_i2 in
          let (o, _x_i3) = o#location _x_i3 in
          let (o, _x_i4) = o#option (fun o -> o#datatype') _x_i4
          in (o, (Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4))))
      | Fun f -> let o, f = o#function_definition f in o, Fun f
      | Funs _x ->
          let (o, _x) = o#list (fun o -> o#recursive_function) _x in
          (o, (Funs _x))
      | Foreign alien ->
         let o, declarations =
           o#list
             (fun o (b, dt) ->
               let o, b = o#binder b in
               let o, dt = o#datatype' dt in
               o, (b, dt))
             (Alien.declarations alien)
         in
         let o, language = o#foreign_language (Alien.language alien) in
         o, Foreign (Alien.modify ~declarations ~language alien)
      | Import { pollute; path } ->
          let (o, path') = o#list (fun o n -> o#name n) path in
          (o, Import { pollute; path = path' })
      | Open _xs ->
          let (o, _xs) = o#list (fun o n -> o#name n) _xs in
          (o, Open _xs)
      | Aliases ts ->
          let (o, _x) = o#list (fun o -> o#alias) ts in
          (o, (Aliases _x))
      | Infix { name; assoc; precedence } ->
         let (o, name) = o#name name in
         (o, Infix { name; assoc; precedence })
      | Exp _x -> let (o, _x) = o#phrase _x in (o, (Exp _x))
      | Module { binder; members } ->
          let (o, binder) = o#binder binder in
          let (o, members) = o#list (fun o -> o#binding) members in
          (o, (Module { binder; members }))
      | AlienBlock alien ->
         let o, lang = o#foreign_language (Alien.language alien) in
         let o, declarations =
           o#list
             (fun o (b, dt) ->
               let o, b = o#binder b in
               let o, dt = o#datatype' dt in
               o, (b, dt))
             (Alien.declarations alien)
         in
         o, AlienBlock (Alien.modify ~language:lang ~declarations alien)

    method binding : binding -> ('self_type * binding) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#bindingnode v)

    method aliasnode : aliasnode -> ('self_type * aliasnode) =
      fun (_x, _x_i1, _x_i2) ->
      let (o, _x) = o#name _x in
      let (o, _x_i1) =
        o#list
          (fun o _x ->
            let (o, _x) = o#quantifier _x
            in (o, _x))
          _x_i1 in
      let (o, _x_i2) = o#aliasbody _x_i2
      in (o, (_x, _x_i1, _x_i2))

    method alias : alias -> ('self_type * alias) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#aliasnode v)

    method aliasbody : aliasbody -> ('self_type * aliasbody) =
      function
        | Typename   _x   -> let o, _x = o#datatype'   _x in (o, Typename     _x)
        | Effectname _x   -> let o, _x = o#row'        _x in (o, Effectname   _x)

    method function_definition : function_definition -> 'self * function_definition
      = fun { fun_binder;
              fun_linearity;
              fun_definition = (tyvar, lit);
              fun_location;
              fun_signature;
              fun_frozen;
              fun_unsafe_signature; }->
      let o, fun_binder = o#binder fun_binder in
      let o, tyvar = o#list (fun o -> o#quantifier) tyvar in
      let o, lit = o#funlit lit in
      let o, fun_location = o#location fun_location in
      let o, fun_signature = o#option (fun o -> o#datatype') fun_signature in
      (o, { fun_binder;
            fun_linearity;
            fun_definition = (tyvar, lit);
            fun_location;
            fun_signature;
            fun_frozen;
            fun_unsafe_signature; })

    method recursive_functionnode  : recursive_functionnode -> 'self * recursive_functionnode
      = fun { rec_binder;
              rec_linearity;
              rec_definition = ((tyvar, ty), lit);
              rec_location;
              rec_signature;
              rec_unsafe_signature;
              rec_frozen } ->
      let o, rec_binder = o#binder rec_binder in
      let o, tyvar = o#list (fun o -> o#quantifier) tyvar in
      let o, ty = o#option (fun o (t, x)-> let o, t = o#typ t in o, (t, x)) ty in
      let o, lit = o#funlit lit in
      let o, rec_location = o#location rec_location in
      let o, rec_signature = o#option (fun o -> o#datatype') rec_signature in
      (o, { rec_binder;
            rec_linearity;
            rec_definition = ((tyvar, ty), lit);
            rec_location;
            rec_signature;
            rec_unsafe_signature;
            rec_frozen})

    method recursive_function  : recursive_function -> 'self * recursive_function =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_node:(fun o v -> o#recursive_functionnode v)

    method binder : Binder.with_pos -> ('self_type * Binder.with_pos) =
      Binder.traverse_map
        ~o
        ~f_pos:(fun o v -> o#position v)
        ~f_name:(fun o v -> o#name v)
        ~f_ty:(fun o v -> o#typ v)

    method typ : Types.datatype -> ('self_type * Types.datatype) =
      o#unknown

    method type_row : Types.row -> ('self_type * Types.row) =
      o#unknown

    method tyarg : Types.type_arg -> ('self_type * Types.type_arg) =
      let open PrimaryKind in
      function
      | Type, t     -> let o, t = o#typ t in o, (Type, t)
      | Row, r      -> let o, r = o#type_row r in o, (Row, r)
      | Presence, p -> let o, p = o#type_field_spec p in o, (Presence, p)

    method type_field_spec : Types.field_spec -> ('self_type * Types.field_spec) =
      o#unknown

    method unknown : 'a. 'a -> ('self_type * 'a) = fun x -> (o, x)
  end
