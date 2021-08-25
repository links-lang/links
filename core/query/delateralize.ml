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

module QL = QueryLang
module Q = MixingQuery

(* returns the "query graph"
     G(x <- q1; q2) := for x :- q1, #y :- q2 do {(x,#y)}
     i.e. a graph representation of the "function" mapping each element x of q1 to q2[x]

   Also returns the fieldtypes of the graph *)
let graph_query (q1,ty1) x (q2,ty2) =
    let y = Var.fresh_raw_var () in
    let p = Q.flattened_pair (QL.Var (x,ty1)) (QL.Var (y,ty2)) in
    let ftys = Q.flattened_pair_ft (QL.Var (x,ty1)) (QL.Var (y,ty2)) in
    QL.For (None, [(x, q1); (y, q2)], [], QL.Singleton p), ftys

(*
    DELATERALIZING REWRITE for Prom:
     for gs, y <- Prom(q3) do q1                          -- s.t. x <- q2 in gs
     ~> for gs, p <- Prom(G(x <- Dedup q2; q3))
        where x = p.1 do (\lambda y.q1) p.2
*)
let prom_delateralize gs q1 x (q2,ty2) y (q3,ty3) =
    let p = Var.fresh_raw_var () in
    let graph, ftys = graph_query (QL.Dedup q2,ty2) x (q3,ty3) in
    let vp = QL.Var (p,Types.make_record_type ftys) in
    let vx = QL.Var (x,ty2) in
    let eq_test a b = QL.Apply (QL.Primitive "==", [a;b]) in
    let and_query a b = QL.Apply (QL.Primitive "&&", [a;b]) in
    (* eta-expanded vx == p.1, with record flattening *)
    (* UNSMART? there may be a better way of doing this, because normalization already pushes equality of records
       to a conjunction of equalities over their fields; however, here we are using a flattened version of the
       record p, so extracting p.1 really amounts to building a new record; maybe it wouldn't be much smarter, after all *)
    let eq_query =
        StringMap.fold
            (fun f _ acc -> and_query acc (eq_test (QL.Project (vx, f)) (QL.Project (vp, Q.flatfield "1" f))))
            (QL.recdty_field_types ty2)
            (QL.Constant (Constant.Bool true))
    in
    (* eta-expanded p.2, with record flattening *)
    let rp =
        QL.Record
            (StringMap.fold
                (fun f _ acc -> StringMap.add f (QL.Project (vp, Q.flatfield "2" f)) acc)
                (QL.recdty_field_types ty3)
                StringMap.empty)
    in
    let q1_rp = QL.subst q1 y rp
    in
    QL.For (None,
        gs @ [(p, QL.Prom graph)],
        [],
        QL.If (eq_query, q1_rp, QL.nil))

(*  returns None if q is already delateralized
    returns Some q' if q simplifies to a less lateral q'
    (this actually performs PARALLEL delateralization steps) *)
let rec delateralize_step q =
    let ds = delateralize_step in
    match q with
    | QL.For (_tag, gs, os, q) ->
        let rec findgs gsx = function
        | (y,QL.Prom qy as gy)::gsy ->
            begin
                match QL.occurs_free_gens gsx qy with
                (* tail-consing is annoying, but occurs_free_list needs arguments in this order *)
                | None -> findgs (gsx@[gy]) gsy
                | Some (x,qx,tyx) -> Some (gsx,x,qx,tyx,y,qy,gsy)
            end
        | gy::gsy -> findgs (gy::gsx) gsy
        | [] -> None
        in begin
            match findgs [] gs with
            | Some (gsx,x,qx,tyx,y,qy,gsy) ->
                let qf = QL.For (None, gsy, [], q) in
                let tyy = Q.type_of_for_var qy in
                Some (prom_delateralize gsx qf x (qx,tyx) y (qy,tyy))
            | None ->
                let ogs = gs >>==? (fun (z,qz) -> ds qz >>=? fun qz' -> Some (z,qz')) in
                let oq = ds q in
                begin
                    match ogs, oq with
                    | None, None -> None
                    | _ -> Some (QL.For (None, from_option gs ogs, os, from_option q oq))
                end
        end
    | QL.If (c,t,e) ->
        begin
            match ds c, ds t, ds e with
            | None, None, None -> None
            | c', t', e' -> Some (QL.If (from_option c c', from_option t t', from_option e e'))
        end
     (* XXX: can t in Apply (t,...) even contain a For? however let's perform recursion for safety *)
    | QL.Apply (t,args) ->
        let ot = ds t in
        let oargs = args >>==? ds in
        begin
            match ot, oargs with
            | None, None -> None
            | _ -> Some (QL.Apply (from_option t ot, from_option args oargs))
        end
    | QL.Singleton t -> (match ds t with None -> None | Some t' -> Some (QL.Singleton t'))
    | QL.Concat tl -> (match tl >>==? ds with None -> None | Some tl' -> Some (QL.Concat tl'))
    | QL.Dedup t -> ds t >>=? fun t' -> Some (QL.Dedup t')
    | QL.Prom t -> ds t >>=? fun t' -> Some (QL.Prom t')
    | QL.Record fl ->
        let ofl = StringMap.to_alist fl >>==? fun (z,qz) -> ds qz >>=? fun qz' -> Some (z,qz') in
        ofl >>=? fun fl' -> Some (QL.Record (StringMap.from_alist fl'))
    | QL.Project (t,f) ->
        ds t >>=? fun t' -> Some (QL.Project (t',f))
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
    let env = QL.env_of_value_env policy env in
    Debug.debug_time "MixingQuery.eval" (fun () ->
        e
        |> MixingQuery.Eval.computation env
        |> delateralize env)
