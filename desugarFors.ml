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

(*
Alternative: just desugar to unary fors

    [[ for (p1 <- t1, ... pk <- tk)
        where c
         e ]]
  ==
    for (p1 <- t1)
     ... 
    for (pk <- tk)
      if (c) {e} else {[]}

    [[ for (p1 <- t1, ... pk <- tk)
         e ]]
  ==
    for (p1 <- t1)
     ... 
    for (pk <- tk)
      e


    | `Iteration (generators, body, filter, None) ->
        (* alternative desugaring to unary fors *)
        let o, generators =
          let iterpatt o =
            function
              | `List (p, e) ->
                  let (o, e, _) = o#phrase e in
                  let (o, p) = o#pattern p in
                    o, `List (p, e)
              | `Table (p, e) ->
                  let (o, e, t) = o#phrase e in
                  let (o, p) = o#pattern p in

                  let r = `Type (TypeUtils.table_read_type t) in
                  let w = `Type (TypeUtils.table_write_type t) in
                  let mb = `Type (o#lookup_mb ()) in

                  let e = `FnAppl ((`TAppl ((`Var ("asList"), dp), [r; mb; w]), dp), [e]), dp in
                    o, (`List (p, e))
          in
            TransformSugar.listu o iterpatt generators in
        let o, body, t = o#phrase body in

        let elem_type = TypeUtils.element_type t in
          
        let body : phrase =
          match filter with
            | None -> body
            | Some condition ->
                `Conditional (condition, body, (`ListLit ([], Some elem_type), dp)), dp in
        let e, _ =
          List.fold_right
            (fun generator body ->
               `Iteration ([generator], body, None, None), dp)
            generators
            body                   
        in
          (o, e, t)


*)

let dp = Sugartypes.dummy_position

(*
  This function generates the code to extract the results.
  It roughly corresponds to [[qs]].
*)
let results :  Types.datatype ->
  (Sugartypes.phrase list * Sugartypes.name list * Types.datatype list) -> Sugartypes.phrase =
  fun mb (es, xs, ts) ->
    let results_type = Types.make_tuple_type ts in
    let rec results =
      function
        | ([], [], []) -> `ListLit ([`TupleLit [], dp], Some Types.unit_type), dp
        | ([e], [x], [t]) -> e
        | (e::es, x::xs, t::ts) ->
            let r = results (es, xs, ts) in
            let qt = t in
            let qst = Types.make_tuple_type ts in
              
            let ((qsb, qs) : Sugartypes.pattern list * Sugartypes.phrase list) =
              List.split
                (List.map2 (fun x t ->
                              (`Variable (x, Some t, dp), dp), ((`Var x), dp)) xs ts) in
            let qb, q = (`Variable (x, Some t, dp), dp), ((`Var x), dp) in

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
                  (Some [a, mb],
                   ([ps], (`TupleLit (q::qs), dp))), dp in
            let outer : Sugartypes.phrase =
              let a = `Type qst in
              let b = `Type (Types.make_tuple_type (t :: ts)) in
                `FunLit
                  (Some [Types.make_tuple_type [t], mb],
                   ([[qb]],
                    (`FnAppl                   
                       ((`TAppl ((`Var "map", dp), [a; `Type mb; b]), dp),
                        [inner; r]), dp))), dp in
            let a = `Type qt in
            let b = `Type (Types.make_tuple_type (t :: ts)) in
              `FnAppl
                ((`TAppl ((`Var "concatMap", dp), [a; `Type mb; b]), dp),
                 [outer; e]), dp
    in
      results (es, xs, ts)


class desugar_fors {Types.var_env=var_env; Types.tycon_env=tycon_env} =
object (o : 'self_type)
  inherit (TransformSugar.transform (var_env, tycon_env)) as super

  (*
    extract a quadruple (sources, patterns, constructors, types)
    from a list of qualifiers
  *)
  method qualifiers : Sugartypes.iterpatt list ->
    'self_type *
      (Sugartypes.phrase list * Sugartypes.pattern list * Sugartypes.name list * Types.datatype list) =
    fun qs ->
      let o, (es, ps, xs, ts) =
        List.fold_left
          (fun (o, (es, ps, xs, ts)) q ->
             match q with
               | `List (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let t = TypeUtils.element_type t in
                   let var = Utility.gensym ~prefix:"_for_" () in
                   let (xb, x) = (var, Some t, dp), var in
                     o, (e::es, ((`As (xb, p)), dp)::ps, x::xs, t::ts)
               | `Table (p, e) ->
                   let (o, e, t) = o#phrase e in
                   let (o, p) = o#pattern p in

                   let r = `Type (TypeUtils.table_read_type t) in
                   let w = `Type (TypeUtils.table_write_type t) in
                   let mb = `Type (o#lookup_mb ()) in

                   let e = `FnAppl ((`TAppl ((`Var ("asList"), dp), [r; mb; w]), dp), [e]), dp in
                   let var = Utility.gensym ~prefix:"_for_" () in
                   let (xb, x) = (var, Some t, dp), var in
                     o, (e::es, ((`As (xb, p)), dp)::ps, x::xs, t::ts))
          (o, ([], [], [], []))
          qs
      in
        o, (List.rev es, List.rev ps, List.rev xs, List.rev ts)

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
(*     | `Iteration ([], (body, _), _, _) as e -> *)
(*         o#phrasenode body *)
    | `Iteration (generators, body, filter, sort) ->
        let mb = o#lookup_mb () in
        let o, (es, ps, xs, ts) = o#qualifiers generators in
        let o, body, body_type = o#phrase body in
        let o, filter, _ = TransformSugar.option o (fun o -> o#phrase) filter in
        let o, sort, sort_type = TransformSugar.option o (fun o -> o#phrase) sort in
        let elem_type = TypeUtils.element_type body_type in
(*         let body : phrase = *)
(*           match sort with *)
(*             | None -> body *)
(*             | Some sort ->  *)
(*                 let a = `Type elem_type in *)
(*                 let b = `Type (Types.make_tuple_type [body_type; Types.bool_type]) in *)
(*                 let var = Utility.gensym ~prefix:"_for_" () in *)
(*                 let (xb, x) = (var, Some body_type, dp), ((`Var var), dp) in *)
(*                   `FnAppl *)
(*                     ((`TAppl ((`Var "map", dp), [a; `Type mb; b]), dp), *)
(*                      [`FunLit (Some [Types.make_tuple_type [elem_type], mb], *)
(*                                ([[`Variable xb, dp]], (`TupleLit [x; sort], dp))), dp; body]), dp in *)
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
            (Some [Types.make_tuple_type [arg_type], mb],
             ([arg], body)), dp in
          
        let results = results mb (es, xs, ts) in
        let results =
          match sort, sort_type with
            | None, None -> results
            | Some sort, Some sort_type ->
                let g : phrase =
                  `FunLit
                    (Some [Types.make_tuple_type [arg_type], mb],
                     ([arg], sort)), dp
                in
                  `FnAppl
                    ((`TAppl ((`Var "sortBy", dp), [`Type arg_type; `Type mb; `Type sort_type]), dp),
                     [g; results]), dp in

        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "concatMap", dp), [`Type arg_type; `Type mb; `Type elem_type]), dp),
             [f; results])



(*         let e : phrasenode = *)
(*           match sort, sort_type with *)
(*             | None, None -> *)
(*                 let a = *)
(*                   match ts with *)
(*                     | [t] -> `Type t *)
(*                     | _ -> `Type (Types.make_tuple_type ts) in *)
(*                 let b = `Type elem_type in *)
(*                   `FnAppl *)
(*                     ((`TAppl ((`Var "concatMap", dp), [a; `Type mb; b]), dp), *)
(*                      [f; results]) *)
(*             | Some sort, Some sort_type -> *)
(*                 let bst = Types.make_tuple_type [body_type; sort_type] in *)
(*                 let e : phrase = *)
(*                   let a = *)
(*                     match ts with *)
(*                       | [t] -> `Type t *)
(*                       | _ -> `Type (Types.make_tuple_type ts) in *)
(*                   let b = `Type bst in *)
(*                     `FnAppl *)
(*                       ((`TAppl ((`Var "concatMap", dp), [a; `Type mb; b]), dp), *)
(*                        [f; results]), dp in *)
(*                 let e' : phrase = *)
(*                   let a = `Type bst in *)
(*                   let b = `Type sort_type in *)
(*                   let snd = `Section(`Project "2"), dp in  *)
(*                     `FnAppl *)
(*                       ((`TAppl ((`Var "sortBy", dp), [a; `Type mb; b]), dp), *)
(*                        [snd; e]), dp in *)
                  
(*                 let a = `Type bst in *)
(*                 let b = `Type elem_type in *)
(*                 let fst = `Section(`Project "1"), dp in  *)
(*                   `FnAppl *)
(*                     ((`TAppl ((`Var "map", dp), [a; `Type mb; b]), dp), *)
(*                      [fst; e']) *)
        in
(*           Debug.print ("comprehension: "^Show_phrasenode.show (`Iteration (generators, body, filter, sort))); *)
(*           Debug.print ("body_type: "^Types.string_of_datatype body_type); *)
(*           Debug.print ("desugared comprehension: "^Show_phrasenode.show e); *)
          (o, e, body_type)
    | e -> super#phrasenode e
end

let desugar_fors env = ((new desugar_fors env) : desugar_fors :> TransformSugar.transform)

let has_no_fors =
object
  inherit SugarTraversals.predicate as super

  val has_no_fors = true
  method satisfied = has_no_fors

  method phrasenode = function
    | `Iteration _ -> {< has_no_fors = false >}
    | e -> super#phrasenode e
end
