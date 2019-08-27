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

let tt = function
  | [t] -> t
  | ts -> Types.make_tuple_type ts

(**
  This function generates the code to extract the results.
  It roughly corresponds to [[qs]].
*)
let results :  Types.row ->
  (Sugartypes.phrase list * Sugartypes.name list * Types.datatype list) -> Sugartypes.phrase =
  fun eff (es, xs, ts) ->
    (* let results_type = Types.make_tuple_type ts in *)
    let rec results =
      function
        | ([], [], []) -> list ~ty:Types.unit_type [tuple []]
        | ([e], [_x], [_t]) -> e
        | (e::es, x::xs, t::ts) ->
            let r = results (es, xs, ts) in
            let qt = t in
            let qst = tt ts in

            let ((qsb, qs) : Sugartypes.Pattern.with_pos list * Sugartypes.phrase list) =
              List.split
                (List.map2 (fun x t -> (variable_pat ~ty:t x, var x)) xs ts) in
            let qb, q = (variable_pat ~ty:t x, var x) in

            let inner : Sugartypes.phrase =
              let ps =
                match qsb with
                  | [p] -> [p]
                  | _ -> [tuple_pat qsb] in
              let a = Types.make_tuple_type [tt ts] in
              fun_lit ~args:[a, eff] dl_unl [ps] (tuple (q::qs)) in
            let outer : Sugartypes.phrase =
              let a = `Type qst in
              let b = `Type (Types.make_tuple_type (t :: ts)) in
                fun_lit ~args:[Types.make_tuple_type [t], eff]
                        dl_unl [[qb]]
                        (fn_appl "map" [a; `Row eff; b] [inner; r]) in
            let a = `Type qt in
            let b = `Type (Types.make_tuple_type (t :: ts)) in
            fn_appl "concatMap" [a; `Row eff; b] [outer; e]
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
      (Sugartypes.phrase list * Sugartypes.Pattern.with_pos list * Sugartypes.name list *
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
               | Table (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let element_type = TypeUtils.table_read_type t in

                   let r = `Type (TypeUtils.table_read_type t) in
                   let w = `Type (TypeUtils.table_write_type t) in
                   let n = `Type (TypeUtils.table_needed_type t) in

                   let e = fn_appl "AsList" [r; w; n] [e] in
                   let var = Utility.gensym ~prefix:"_for_" () in
                   let xb = binder ~ty:t var in
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

        let arg_type = tt ts in

        let f : phrase = fun_lit ~args:[Types.make_tuple_type [arg_type], eff]
                                 dl_unl [arg] body in

        let results = results eff (es, xs, ts) in
        let results =
          match sort, sort_type with
            | None, None -> results
            | Some sort, Some sort_type ->
                let sort_by, sort_type_arg =
                  "sortByBase", `Row (TypeUtils.extract_row sort_type) in

                let g : phrase =
                  fun_lit ~args:[Types.make_tuple_type [arg_type], eff]
                          dl_unl [arg] sort
                in
                fn_appl sort_by [`Type arg_type; `Row eff; sort_type_arg]
                        [g; results]
            | _, _ -> assert false in

        let e : phrasenode =
          fn_appl_node "concatMap" [`Type arg_type; `Row eff; `Type elem_type]
                       [f; results] in
        (o, e, body_type)
    | e -> super#phrasenode e
end

let desugar_fors env = ((new desugar_fors env)
                          : desugar_fors :> TransformSugar.transform)

let desugar_program : TransformSugar.program_transformer =
  fun env program -> snd3 ((desugar_fors env)#program program)

let desugar_sentence : TransformSugar.sentence_transformer =
  fun env sentence -> snd ((desugar_fors env)#sentence sentence)

let has_no_fors =
object
  inherit SugarTraversals.predicate as super

  val has_no_fors = true
  method satisfied = has_no_fors

  method! phrasenode = function
    | Iteration _ -> {< has_no_fors = false >}
    | e -> super#phrasenode e
end
