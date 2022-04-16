open Utility
open CommonTypes
open Sugartypes
open SugarConstructors.DummyPositions

(*
  Desugaring n-ary for comprehensions with conditions and orderby
  ---------------------------------------------------------------


    [[ for (qs) e ]]
  ==
    concatMap (\ps.e) [[ qs ]]

    [[ for (qs)
        where b
         e ]]
  ==
    concatMap (\ps.if b then e else []) [[ qs ]]

    [[ for (qs)
        orderby o
         e ]]
  ==
    concatMap (\ps.e sortBy (\ps.o) [[ qs ]])

    [[ for (qs)
        where b
        orderby o
         e ]]
  ==
    concatMap (\ps.if b then e else []) (sortBy (\ps.o) [[ qs ]])

    [[ for () ... e ]]
  ==
    e

   (p <- t)* = t

   [[q; qs]] = concatMap (\q_v.map (\qs_v.(q_v,qs_v)) [[qs]]) [[q]]
       [[q]] = q*
       [[.]] = undefined

   (p <- t)_v = p
    (q; qs)_v = (q_v, qs_v)
*)

(**
  This function generates the code to extract the results.
  It roughly corresponds to [[qs]].
*)
let results :  Types.row ->
  (Sugartypes.phrase list * Name.t list * Types.datatype list) -> Sugartypes.phrase =
  fun eff (es, xs, ts) ->
    (* let results_type = Types.make_tuple_type ts in *)
    let rec results =
      function
        | ([], [], []) -> list ~ty:Types.unit_type [tuple []]
        | ([e], [_x], [_t]) -> e
        | (e::es, x::xs, t::ts) ->
            let r = results (es, xs, ts) in
            let qt = t in
            let qst = TypeUtils.pack_types ts in

            let ((qsb, qs) : Sugartypes.Pattern.with_pos list * Sugartypes.phrase list) =
              List.split
                (List.map2 (fun x t -> (variable_pat ~ty:t x, var x)) xs ts) in
            let qb, q = (variable_pat ~ty:t x, var x) in

            let open PrimaryKind in
            let inner : Sugartypes.phrase =
              let ps =
                match qsb with
                  | [p] -> [p]
                  | _ -> [tuple_pat qsb] in
              let a = Types.make_tuple_type [TypeUtils.pack_types ts] in
              fun_lit ~args:[a, eff] dl_unl [ps] (tuple (q::qs)) in
            let outer : Sugartypes.phrase =
              let a = Types.make_tuple_type (t :: ts) in
                fun_lit ~args:[Types.make_tuple_type [t], eff]
                        dl_unl [[qb]]
                        (fn_appl "map" [(Type, qst); (Row, eff); (Type, a)] [inner; r]) in
            let a = Types.make_tuple_type (t :: ts) in
            fn_appl "concatMap" [(Type, qt); (Row, eff); (Type, a)] [outer; e]
        | _, _, _ -> assert false
    in
      results (es, xs, ts)


class desugar_fors env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  (**
    Extract a quadruple (sources, patterns, constructors, types)
    from a list of qualifiers
  *)
  method qualifiers : Sugartypes.iterpatt list ->
    'self_type *
      (Sugartypes.phrase list * Sugartypes.Pattern.with_pos list * Name.t list *
         Types.datatype list) =
    fun qs ->
      let o, (es, ps, xs, ts) =
        List.fold_left
          (fun (o, (es, ps, xs, ts)) q ->
             match q with
               | List (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let element_type = TypeUtils.element_type t in

                   let var = Utility.gensym ~prefix:"_for_" () in
                   let xb = binder ~ty:element_type var in
                     o, (e::es, with_dummy_pos (Pattern.As (xb, p))::ps,
                         var::xs, element_type::ts)
               | Table (tmp, p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let r = TypeUtils.table_read_type   t in
                   let w = TypeUtils.table_write_type  t in
                   let n = TypeUtils.table_needed_type t in

                   let open PrimaryKind in
                   let fn_name, element_type =
                       let open Temporality in
                       let element_type = TypeUtils.table_read_type t in
                       match tmp with
                         | Current -> "AsList", element_type
                         | Transaction ->
                             "AsListT",
                             Types.make_transaction_time_data_type element_type
                         | Valid ->
                             "AsListV",
                             Types.make_valid_time_data_type element_type
                   in
                   let e = fn_appl fn_name [(Type, r); (Type, w); (Type, n)] [e] in
                   let var = Utility.gensym ~prefix:"_for_" () in
                   let xb = binder ~ty:element_type var in
                     o, (e::es, with_dummy_pos (Pattern.As (xb, p))::ps,
                         var::xs, element_type::ts))
          (o, ([], [], [], []))
          qs
      in
        o, (List.rev es, List.rev ps, List.rev xs, List.rev ts)

  method! phrasenode : Sugartypes.phrasenode ->
    ('self_type * Sugartypes.phrasenode * Types.datatype) =
    function
    | Iteration (generators, body, filter, sort) ->
        let eff = o#lookup_effects in
        let envs = o#backup_envs in
        let o, (es, ps, xs, ts) = o#qualifiers generators in
        let o, body, body_type = o#phrase body in
        let o, filter, _ = TransformSugar.option o (fun o -> o#phrase) filter in
        let o, sort, sort_type = TransformSugar.option o (fun o -> o#phrase) sort in
        let elem_type = TypeUtils.element_type body_type in

        let body : phrase =
          match filter with
            | None -> body
            | Some condition ->
                with_dummy_pos (Conditional (condition, body, list ~ty:elem_type [])) in

        let arg =
          match ps with
            | [p] -> [p]
            | ps -> [tuple_pat ps] in

        let arg_type = TypeUtils.pack_types ts in

        let f : phrase = fun_lit ~args:[Types.make_tuple_type [arg_type], eff]
                                 dl_unl [arg] body in

        let open PrimaryKind in

        let results = results eff (es, xs, ts) in
        let results =
          match sort, sort_type with
            | None, None -> results
            | Some sort, Some sort_type ->
               let sort_by, sort_row =
                 "sortByBase", TypeUtils.extract_row sort_type in
               let g : phrase =
                 fun_lit ~args:[Types.make_tuple_type [arg_type], eff]
                   dl_unl [arg] sort in
               fn_appl sort_by [(Type, arg_type); (Row, eff); (Row, sort_row)]
                 [g; results]
            | _, _ -> assert false in

        let e : phrasenode =
          fn_appl_node "concatMap" [(Type, arg_type); (Row, eff); (Type, elem_type)]
                       [f; results] in
        let o = o#restore_envs envs in
        (o, e, body_type)
    | e -> super#phrasenode e
end

let desugar_fors env = ((new desugar_fors env)
                          : desugar_fors :> TransformSugar.transform)

let has_no_fors =
object
  inherit SugarTraversals.predicate as super

  val has_no_fors = true
  method satisfied = has_no_fors

  method! phrasenode = function
    | Iteration _ -> {< has_no_fors = false >}
    | e -> super#phrasenode e
end

module Typeable
  = Transform.Typeable.Make(struct
        let name = "fors"
        let obj env = (desugar_fors env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
