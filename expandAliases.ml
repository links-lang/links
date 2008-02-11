open Utility

module Env = Env.String

let instantiate name ts env = 
  (* This is just type application.
  
     (\Lambda x1 ... xn . t) (t1 ... tn) ~> t[ti/xi]
  *)
  match Env.find env name with
    | Some (vars, _) when List.length vars <> List.length ts ->
        failwith (Printf.sprintf
                    "Type alias %s applied with incorrect arity (%d instead of %d)"
                    name (List.length ts) (List.length vars))
    | Some (vars, alias) ->
        let tenv = List.fold_right2 IntMap.add vars ts IntMap.empty in
          Instantiate.datatype (tenv, IntMap.empty) (Types.freshen_mailboxes alias)
    | None -> 
        failwith (Printf.sprintf "Unrecognised type constructor: %s" name)

(* This should really be done with a generic traversal function. *)
let rec expand_aliases (rec_vars : Types.TypeVarSet.t) env t = 
  let rec expand rec_vars = expand_aliases rec_vars env
  and expand_row rec_vars (fsm, rv) = 
    let f = function
      | `Present t -> `Present (expand rec_vars t)
      | `Absent    -> `Absent in
      (StringMap.map f fsm, rv)
  in
    match t with
      |(`Not_typed : Types.datatype)
      | `Primitive _ as t -> t
      | `Function (f, m, t) -> `Function (expand rec_vars f, expand rec_vars m, expand rec_vars t)
      | `Record r -> `Record (expand_row rec_vars r)
      | `Variant r -> `Variant (expand_row rec_vars r)
      | `Table (t1, t2) -> `Table (expand rec_vars t1, expand rec_vars t2)
      | `Alias _ -> assert false (* There shouldn't be an alias here yet *)
      | `Application (s, ds) when Env.has env s -> 
          let ds = List.map (expand rec_vars) ds in
          `Alias ((s, ds), instantiate s ds env)
      | `Application (s, ds) -> 
          `Application (s, List.map (expand rec_vars) ds)
      | `ForAll (qs, dt) -> `ForAll (qs, expand rec_vars dt)
      | `MetaTypeVar point -> 
          match Unionfind.find point with
            | `Flexible _ 
            | `Rigid _ -> t
            | `Recursive (var, body) ->
                if Types.TypeVarSet.mem var rec_vars then t
                else `MetaTypeVar 
                  (Unionfind.fresh 
                     (`Recursive (var, expand (Types.TypeVarSet.add var rec_vars) body)))
            | `Body t -> expand rec_vars t

let expand_aliases env t = 
(*  prerr_endline ("=> expand_aliases : " ^ Types.Show_datatype.show t);
  flush stderr;*)
  let r = expand_aliases Types.TypeVarSet.empty env t in
(*    prerr_endline ("<= expand_aliases : " ^ Types.Show_datatype.show r);
    flush stderr;*)
    r



class type alias_expander =
object
  inherit SugarTraversals.fold_map
  method aliases : Types.alias_environment
end

let expand_aliases (initial_env : Types.alias_environment) =
object (self)
  inherit SugarTraversals.fold_map as super

  val aliases = initial_env

  method phrasenode = function
    | `Block (bs, p) -> 
        (* aliases bound in `bs'
           should not escape the scope of the block *)
        let o = {<>} in
        let o, x = o#list (fun o -> o#binding) bs in
        let o, p = o#phrase p in 
          self, `Block (bs, p)
    | `TableLit (p1, (t, Some (t1, t2)), fcs, p2) ->
        let o, p1 = self#phrase p1 in
        let o, t  = o#datatype t in
        let t1    = expand_aliases aliases t1
        and t2    = expand_aliases aliases t1 in
        let o, p2 = o#phrase p2 in
          o, `TableLit (p1, (t, Some (t1, t2)), fcs, p2)

    (* Switch and receive type annotations are never filled in by
       this point, so we ignore them.  *)
    | p              -> super#phrasenode p

  method sentence = 
    (* return any aliases bound to the interactive loop so that they
       are available to future input.  The default definition will
       do fine here *)
    super#sentence

  method program (bindings, e) = 
    (* as with a block, bindings should not escape here *)
    let o = {<>} in
    let o, bindings = o#list (fun o -> o#binding) bindings in
    let o, e = o#option (fun o -> o#phrase) e in
      o, (bindings, e)

  method bindingnode = function
    | `Type (name, vars, (t, Some dt)) -> 
        let dt = expand_aliases aliases dt in 
          (* NB: type aliases are scoped; we allow shadowing.
             We also allow type aliases to shadow abstract types. *)
          {< aliases = Env.bind aliases (name, (List.map (snd ->- val_of) vars, dt)) >},
          `Type (name, vars, (t, Some dt))
    | bn -> super#bindingnode bn

  method datatype' = function
    | (t, Some dt) -> self, (t, Some (expand_aliases aliases dt))
    | _ ->
        failwith "Internal error: unexpanded datatype encountered during alias expansion"

  method aliases = aliases
end


let expand aliases dt =
  let _, (_, Some dt) = (expand_aliases aliases)#datatype' (Sugartypes.UnitType, Some dt) in
    dt
