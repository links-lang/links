open Utility
open Sugartypes

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

let dp = Sugartypes.dummy_position

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
        | ([], [], []) -> `ListLit ([`TupleLit [], dp], Some Types.unit_type), dp
        | ([e], [_x], [_t]) -> e
        | (e::es, x::xs, t::ts) ->
            let r = results (es, xs, ts) in
            let qt = t in
            let qst = Types.make_tuple_type ts in

            let ((qsb, qs) : Sugartypes.pattern list * Sugartypes.phrase list) =
              List.split
                (List.map2 (fun x t ->
                     let q = QualifiedName.of_name x in
                              (`Variable (x, Some t, dp), dp), ((`Var q), dp)) xs ts) in
            let qb, q =
              let q = QualifiedName.of_name x in
              (`Variable (x, Some t, dp), dp), ((`Var q), dp) in

            let inner : Sugartypes.phrase =
              let ps =
                match qsb with
                  | [p] -> [p]
                  | _ -> [`Tuple qsb, dp] in
              let a =
                match ts with
                  | [t] -> Types.make_tuple_type [t]
                  | ts -> Types.make_tuple_type [Types.make_tuple_type ts]
              in
                `FunLit
                  (Some [a, eff], `Unl,
                   ([ps], (`TupleLit (q::qs), dp)), `Unknown), dp in
            let outer : Sugartypes.phrase =
              let a = `Type qst in
              let b = `Type (Types.make_tuple_type (t :: ts)) in
              let q = QualifiedName.of_name "map" in
                `FunLit
                  (Some [Types.make_tuple_type [t], eff],
                   `Unl,
                   ([[qb]],
                    (`FnAppl
                       ((`TAppl ((`Var q, dp), [a; `Row eff; b]), dp),
                        [inner; r]), dp)), `Unknown), dp in
            let a = `Type qt in
            let b = `Type (Types.make_tuple_type (t :: ts)) in
            let q = QualifiedName.of_name "concatMap" in
              `FnAppl
                ((`TAppl ((`Var q, dp), [a; `Row eff; b]), dp),
                 [outer; e]), dp
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
      (Sugartypes.phrase list * Sugartypes.pattern list * Sugartypes.name list *
         Types.datatype list) =
    fun qs ->
      let o, (es, ps, xs, ts) =
        List.fold_left
          (fun (o, (es, ps, xs, ts)) q ->
             match q with
               | `List (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let element_type = TypeUtils.element_type t in

                   let var = Utility.gensym ~prefix:"_for_" () in
                   let (xb, x) = (var, Some t, dp), var in
                     o, (e::es, ((`As (xb, p)), dp)::ps, x::xs, element_type::ts)
               | `Table (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let element_type = TypeUtils.table_read_type t in

                   let r = `Type (TypeUtils.table_read_type t) in
                   let w = `Type (TypeUtils.table_write_type t) in
                   let n = `Type (TypeUtils.table_needed_type t) in
                   let eff = `Row (o#lookup_effects) in

                   let e =
                     let q = QualifiedName.of_name "AsList" in
                     `FnAppl ((`TAppl ((`Var q, dp),
                                             [r; w; n; eff]), dp), [e]), dp in
                   let var = Utility.gensym ~prefix:"_for_" () in
                   let (xb, x) = (var, Some t, dp), var in
                     o, (e::es, ((`As (xb, p)), dp)::ps, x::xs, element_type::ts))
          (o, ([], [], [], []))
          qs
      in
        o, (List.rev es, List.rev ps, List.rev xs, List.rev ts)

  method! phrasenode : Sugartypes.phrasenode ->
    ('self_type * Sugartypes.phrasenode * Types.datatype) =
    function
    | `Iteration (generators, body, filter, sort) ->
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
                `Conditional (condition, body, (`ListLit ([], Some elem_type), dp)), dp in

        let arg =
          match ps with
            | [p] -> [p]
            | ps -> [`Tuple ps, dp] in

        let arg_type =
          match ts with
            | [t] -> t
            | ts -> Types.make_tuple_type ts in

        let f : phrase =
          `FunLit
            (Some [Types.make_tuple_type [arg_type], eff],
             `Unl,
             ([arg], body),
             `Unknown), dp in

        let results = results eff (es, xs, ts) in
        let results =
          match sort, sort_type with
            | None, None -> results
            | Some sort, Some sort_type ->
                let sort_by, sort_type_arg =
                  QualifiedName.of_name "sortByBase", `Row (TypeUtils.extract_row sort_type) in

                let g : phrase =
                  `FunLit
                    (Some [Types.make_tuple_type [arg_type], eff],
                     `Unl,
                     ([arg], sort),
                     `Unknown), dp
                in
                  `FnAppl
                    ((`TAppl ((`Var sort_by, dp), [`Type arg_type; `Row eff; sort_type_arg]), dp),
                     [g; results]), dp
            | _, _ -> assert false in

        let e : phrasenode =
          let q = QualifiedName.of_name "concatMap" in
          `FnAppl
            ((`TAppl ((`Var q, dp), [`Type arg_type; `Row eff; `Type elem_type]), dp),
             [f; results]) in
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
    | `Iteration _ -> {< has_no_fors = false >}
    | e -> super#phrasenode e
end
