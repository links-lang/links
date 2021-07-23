(*****************************************************************************
 ** delateralize.ml - Implements the "query delateralization" algorithm:    **
 **                   * produces an equivalent query whose "from" inputs    **
 **                     are not expressed in terms of one another           **
 **                   * does not require SQL99 "lateral"                    **
 **                   * see Ricciotti-Cheney, ESOP2021                      **
 **                                                                         **
 ** author: Wilmer Ricciotti                                                **
 *****************************************************************************)
open Utility
open CommonTypes

module Q = MixingQuery.Lang

(* returns the "query graph"
     G(x <- q1; q2) := for x :- q1, #y :- q2 do {(x,#y)}
     i.e. a graph representation of the "function" mapping each element x of q1 to q2[x]

   Also returns the fieldtypes of the graph *)
let graph_query (q1,ty1) x (q2,ty2) =
    let y = Var.fresh_raw_var () in
    let p = Q.flattened_pair (Q.Var (x,ty1)) (Q.Var (y,ty2)) in
    let ftys = Q.flattened_pair_ft (Q.Var (x,ty1)) (Q.Var (y,ty2)) in
    Q.For (None, [(x, q1); (y, q2)], [], Q.Singleton p), ftys

(*
    DELATERALIZING REWRITE for Prom:
     for gs, y <- Prom(q3) do q1                          -- s.t. x <- q2 in gs
     ~> for gs, p <- Prom(G(x <- Dedup q2; q3))
        where x = p.1 do (\lambda y.q1) p.2
*)
let prom_delateralize gs q1 x (q2,ty2) y (q3,ty3) =
    let p = Var.fresh_raw_var () in
    let graph, ftys = graph_query (Q.Dedup q2,ty2) x (q3,ty3) in
    let vp = Q.Var (p,Types.make_record_type ftys) in
    let vx = Q.Var (x,ty2) in
    let eq_test a b = Q.Apply (Q.Primitive "==", [a;b]) in
    let and_query a b = Q.Apply (Q.Primitive "&&", [a;b]) in
    (* eta-expanded vx == p.1, with record flattening *)
    (* UNSMART? there may be a better way of doing this, because normalization already pushes equality of records
       to a conjunction of equalities over their fields; however, here we are using a flattened version of the
       record p, so extracting p.1 really amounts to building a new record; maybe it wouldn't be much smarter, after all *)
    let eq_query =
        StringMap.fold
            (fun f _ acc -> and_query acc (eq_test (Q.Project (vx, f)) (Q.Project (vp, Q.flatfield "1" f))))
            (Q.recdty_field_types ty2)
            (Q.Constant (Constant.Bool true))
    in
    (* eta-expanded p.2, with record flattening *)
    let rp =
        Q.Record
            (StringMap.fold
                (fun f _ acc -> StringMap.add f (Q.Project (vp, Q.flatfield "2" f)) acc)
                (Q.recdty_field_types ty3)
                StringMap.empty)
    in
    let q1_rp = Q.subst q1 y rp
    in
    Q.For (None,
        gs @ [(p, Q.Prom graph)],
        [],
        Q.If (eq_query, q1_rp, Q.nil))

(*  returns None if q is already delateralized
    returns Some q' if q simplifies to a less lateral q'
    (this actually performs PARALLEL delateralization steps) *)
let rec delateralize_step q =
    let ds = delateralize_step in
    match q with
    | Q.For (_tag, gs, os, q) ->
        let rec findgs gsx = function
        | (y,Q.Prom qy as gy)::gsy ->
            begin
                match Q.occurs_free_gens gsx qy with
                (* tail-consing is annoying, but occurs_free_list needs arguments in this order *)
                | None -> findgs (gsx@[gy]) gsy
                | Some (x,qx,tyx) -> Some (gsx,x,qx,tyx,y,qy,gsy)
            end
        | gy::gsy -> findgs (gy::gsx) gsy
        | [] -> None
        in begin
            match findgs [] gs with
            | Some (gsx,x,qx,tyx,y,qy,gsy) ->
                let qf = Q.For (None, gsy, [], q) in
                let tyy = Q.type_of_for_var qy in
                Some (prom_delateralize gsx qf x (qx,tyx) y (qy,tyy))
            | None ->
                let ogs = gs >>==? (fun (z,qz) -> ds qz >>=? fun qz' -> Some (z,qz')) in
                let oq = ds q in
                begin
                    match ogs, oq with
                    | None, None -> None
                    | _ -> Some (Q.For (None, from_option gs ogs, os, from_option q oq))
                end
        end
    | Q.If (c,t,e) ->
        begin
            match ds c, ds t, ds e with
            | None, None, None -> None
            | c', t', e' -> Some (Q.If (from_option c c', from_option t t', from_option e e'))
        end
     (* XXX: can t in Apply (t,...) even contain a For? however let's perform recursion for safety *)
    | Q.Apply (t,args) ->
        let ot = ds t in
        let oargs = args >>==? ds in
        begin
            match ot, oargs with
            | None, None -> None
            | _ -> Some (Q.Apply (from_option t ot, from_option args oargs))
        end
    | Q.Singleton t -> (match ds t with None -> None | Some t' -> Some (Q.Singleton t'))
    | Q.Concat tl -> (match tl >>==? ds with None -> None | Some tl' -> Some (Q.Concat tl'))
    | Q.Dedup t -> ds t >>=? fun t' -> Some (Q.Dedup t')
    | Q.Prom t -> ds t >>=? fun t' -> Some (Q.Prom t')
    | Q.Record fl ->
        let ofl = StringMap.to_alist fl >>==? fun (z,qz) -> ds qz >>=? fun qz' -> Some (z,qz') in
        ofl >>=? fun fl' -> Some (Q.Record (StringMap.from_alist fl'))
    | Q.Project (t,f) ->
        ds t >>=? fun t' -> Some (Q.Project (t',f))
    (* XXX: assumes no Closures are left *)
    | _ -> None

let rec delateralize env q =
    let q = MixingQuery.Eval.norm env q in
    (* Debug.print "*** normalization step\n";
    Debug.print (Q.show q ^ "\n\n"); *)
    match delateralize_step q with
    | Some q' ->
        (* Debug.print "*** delateralization step";
        Debug.print (Q.show q' ^ "\n\n"); *)
        delateralize env q'
    | None -> q

let eval policy env e =
    (*    Debug.print ("e: "^Ir.show_computation e); *)
    let env = MixingQuery.Eval.env_of_value_env policy env in
    Debug.debug_time "MixingQuery.eval" (fun () ->
        e
        |> MixingQuery.Eval.computation env
        |> delateralize env)
