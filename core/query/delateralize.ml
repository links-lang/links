(*****************************************************************************
 ** Delateralize.ml - Implements the delateralization algorithm             **
 **                                                                         **
 ** author: Wilmer Ricciotti                                                **
 *****************************************************************************)
open Utility
open CommonTypes

module Q = MixingQuery.Lang

let (>>=) (o : 'a option) (f : 'a -> 'b option) =
    match o with
    | Some a -> f a
    | _ -> None

let (||=) o o' =
    match o with
    | None -> o'
    | _ -> o

let unopt_default ox x' = match ox with None -> x' | Some x'' -> x''

let rec (>>==) (l : 'a list) (f : 'a -> 'a option) : 'a list option =
    match l with
    | [] -> None
    | a::al ->
        match f a, al >>== f with
        | None, None -> None
        | fa, fal ->
            Some (unopt_default fa a::unopt_default fal al)

(* not the most efficient way of doing these two functions: why is this not in the standard library? *)
let map_tryPick f m =
    StringMap.fold
        (fun k v acc -> f k v ||= acc)
        m
        None

let list_tryPick f l =
    List.fold_left
        (fun acc x -> f x ||= acc)
        None
        l

let rec subst t x u =
    let srec t = subst t x u in
    match t with
    | Q.Var (var, _) when var = x -> u
    | Q.Record fl -> Q.Record (StringMap.map srec fl)
    | Q.Singleton v -> Q.Singleton (srec v)
    | Q.Concat xs -> Q.Concat (List.map srec xs)
    | Q.Project (r, label) -> Q.Project (srec r, label)
    | Q.Erase (r, labels) -> Q.Erase (srec r, labels)
    | Q.Variant (label, v) -> Q.Variant (label, srec v)
    | Q.Apply (f, xs) -> Q.Apply (srec f, List.map srec xs)
    | Q.For (_, gs, os, u) ->
        (* XXX: assuming fresh x!*)
        let gs' = List.map (fun (v,g) -> (v, srec g)) gs in
        let os' = List.map srec os in
        let u' = srec u in
        Q.For (None, gs', os', u')
    | Q.If (c, t, e) ->
        Q.If (srec c, srec t, srec e)
    | Q.Case (v, cases, default) ->
        let v' = srec v in
        let cases' = StringMap.map (fun (v,q) -> (v,srec q)) cases in
        let default' = default >>= fun d -> Some (fst d, srec (snd d)) in
        Q.Case (v', cases', default')
    | Q.Dedup v -> Q.Dedup (srec v)
    | Q.Prom v -> Q.Prom (srec v)
    | Q.Closure (c, closure_env) ->
        let cenv = MixingQuery.Lang.bind closure_env (x,u) in
        Q.Closure (c, cenv)
    | v -> v

(* C(q1, x.q2) := for x :- q1, #y :- q2 do {(x,#y)}
   also returns the fieltypes of the graph *)
let graph_query (q1,ty1) x (q2,ty2) =
    let y = Var.fresh_raw_var () in
    let p = Q.flattened_pair (Q.Var (x,ty1)) (Q.Var (y,ty2)) in
    let ftys = Q.flattened_pair_ft (Q.Var (x,ty1)) (Q.Var (y,ty2)) in
    Q.For (None, [(x, q1); (y, q2)], [], Q.Singleton p), ftys

(* DELATERALIZING REWRITE for iota
     for gs, y :- I(q3) do q1     -- s.t. x :- q2 in gs
    ~> for gs, p :- I(C(Dq2,x.q3)) where x = p.1 do ([y]q1) p.2 *)
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
    let q1_rp = subst q1 y rp
    in
    Q.For (None,
        gs @ [(p, Q.Prom graph)],
        [],
        Q.If (eq_query, q1_rp, Q.nil))

(* Returns (Some ty) if v occurs free with type ty, None otherwise *)
let occurs_free (v : Var.var) =
    let rec occf bvs = function
    | Q.Var (w,tyw) ->
        if w = v && not (List.mem v bvs)
            then Some tyw
            else None
    | Q.If (c,t,e) -> occf bvs c ||= occf bvs t ||= occf bvs e
    | Q.Closure ((_wl,_b),_e) ->
        (* XXX: to be checked
           we use this function only in normalized queries, so there shouldn't be any closure;
           recursion on b would require deeper analysis of computations, so for the moment
           let's not implement this and hope everything works fine
        let bvs' = bvs @ wl @ List.map (fun (w,_) -> w) (Query.Eval.query_bindings_of_env e) in
        occf bvs' b ||= tryPick (fun _ q -> occf bvs q) e *)
        failwith "Delateralize.occurs_free: unexpected Closure in query"
    | Q.Apply (t, args) -> occf bvs t ||= list_tryPick (occf bvs) args
    | Q.Singleton t
    | Q.Dedup t
    | Q.Prom t
    | Q.Project (t,_) -> occf bvs t
    | Q.Concat tl -> list_tryPick (occf bvs) tl
    | Q.For (_, gs, _os, b) ->
        (* FIXME: do we need to check os as well? *)
        let bvs'', res = List.fold_left (fun (bvs',acc) (w,q) -> w::bvs', acc ||= occf bvs' q) (bvs, None) gs in
        res ||= occf bvs'' b
    | Q.Record fl -> map_tryPick (fun _ t -> occf bvs t) fl
    | _ -> None
    in occf []

(* Returns Some (x,qx,tyx) for the first generator x <- qx such that x occurs free with type tyx *)
let rec occurs_free_gens (gs : (Var.var * Q.t) list) q =
    match gs with
    | [] -> None
    | (x,qx)::gs' ->
        match occurs_free x (Q.For (None, gs', [], q)) with
        | Some tyx -> Some (x,qx,tyx)
        | None -> occurs_free_gens gs' q

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
                match occurs_free_gens gsx qy with
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
                let ogs = gs >>== (fun (z,qz) -> ds qz >>= fun qz' -> Some (z,qz')) in
                let oq = ds q in
                begin
                    match ogs, oq with
                    | None, None -> None
                    | _ -> Some (Q.For (None, unopt_default ogs gs, os, unopt_default oq q))
                end
        end
    | Q.If (c,t,e) ->
        begin
            match ds c, ds t, ds e with
            | None, None, None -> None
            | c', t', e' -> Some (Q.If (unopt_default c' c, unopt_default t' t, unopt_default e' e))
        end
     (* XXX: can t in Apply (t,...) even contain a For? however let's perform recursion for safety *)
    | Q.Apply (t,args) ->
        let ot = ds t in
        let oargs = args >>== ds in
        begin
            match ot, oargs with
            | None, None -> None
            | _ -> Some (Q.Apply (unopt_default ot t, unopt_default oargs args))
        end
    | Q.Singleton t -> (match ds t with None -> None | Some t' -> Some (Q.Singleton t'))
    | Q.Concat tl -> (match tl >>== ds with None -> None | Some tl' -> Some (Q.Concat tl'))
    | Q.Dedup t -> ds t >>= fun t' -> Some (Q.Dedup t')
    | Q.Prom t -> ds t >>= fun t' -> Some (Q.Prom t')
    | Q.Record fl ->
        let ofl = StringMap.to_alist fl >>== fun (z,qz) -> ds qz >>= fun qz' -> Some (z,qz') in
        ofl >>= fun fl' -> Some (Q.Record (StringMap.from_alist fl'))
    | Q.Project (t,f) ->
        ds t >>= fun t' -> Some (Q.Project (t',f))
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
