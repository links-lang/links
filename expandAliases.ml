open Utility

module SEnv = Env.String

let instantiate name ts env = 
  (* This is just type application.
  
     (\Lambda x1 ... xn . t) (t1 ... tn) ~> t[ti/xi]
  *)
  match SEnv.find env name with
    | None -> 
        failwith (Printf.sprintf "Unrecognised type constructor: %s" name)
    | Some (vars, _) when List.length vars <> List.length ts ->
        failwith (Printf.sprintf
                    "Type alias %s applied with incorrect arity (%d instead of %d)"
                    name (List.length ts) (List.length vars))
    | Some (vars, body) ->
        let tenv = List.fold_right2 IntMap.add vars ts IntMap.empty in
          `Alias ((name, ts),
                  Instantiate.datatype (tenv, IntMap.empty) (Types.freshen_mailboxes body))

(* This should really be done with a generic traversal function. *)
let rec expand_aliases
    (rec_tys, rec_rows as rec_envs : Types.meta_type_var Env.Int.t * Types.meta_row_var Env.Int.t) 
    env t =
  let rec expand rec_envs = expand_aliases rec_envs env
  and expand_row rec_vars (fsm, rv) = 
    let fsm', rv = 
      match Unionfind.find rv with
        | `Closed
        | `Flexible _
        | `Rigid _    -> fsm, rv
        | `Recursive (var, body) ->
            begin match Env.Int.find rec_rows var with
              | Some t -> fsm, t
              | None   -> 
                  let point = Unionfind.fresh (`Recursive (var, (StringMap.empty,
                                                                 Unionfind.fresh (`Flexible var)))) in
                  let envs  = (rec_tys, Env.Int.bind rec_rows (var, point)) in
                  let body' = expand_row envs body in
                  let ()    = Unionfind.change point (`Recursive (var, body')) in
                    fsm, point
            end
        | `Body t -> expand_row rec_envs t
    in
    let f = function
      | `Present t -> `Present (expand rec_vars t)
      | `Absent    -> `Absent in
      (StringMap.map f fsm', rv)
  in
    match t with
      | `Not_typed
      | `Primitive _ as t -> t
      | `Function (f, m, t) -> `Function (expand rec_envs f, expand rec_envs m, expand rec_envs t)
      | `Record r -> `Record (expand_row rec_envs r)
      | `Variant r -> `Variant (expand_row rec_envs r)
      | `Table (t1, t2) -> `Table (expand rec_envs t1, expand rec_envs t2)
      | `Alias _ -> assert false (* There shouldn't be an alias here yet *)
      | `Application (s, ds) when SEnv.has env s -> 
          let ds = List.map (expand rec_envs) ds in
            instantiate s ds env
      | `Application (s, ds) -> 
          `Application (s, List.map (expand rec_envs) ds)
      | `ForAll (qs, dt) -> `ForAll (qs, expand rec_envs dt)
      | `MetaTypeVar point -> 
          match Unionfind.find point with
            | `Flexible _ 
            | `Rigid _ -> t
            | `Recursive (var, body) ->
                begin match Env.Int.find rec_tys var with
                  | Some t -> `MetaTypeVar t
                  | None ->
                      let point = Unionfind.fresh (`Recursive (var, `Not_typed))   in
                      let envs  = (Env.Int.bind rec_tys (var, point), rec_rows)    in
                      let body' = expand envs body                                 in
                      let ()    = Unionfind.change point (`Recursive (var, body')) in
                        `MetaTypeVar point
                end
            | `Body t -> expand rec_envs t

let expand_aliases env t = 
  expand_aliases (Env.Int.empty, Env.Int.empty) env t

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
        let o, bs = o#list (fun o -> o#binding) bs in
        let o, p  = o#phrase p in 
          self, `Block (bs, p)
    | `TableLit (p1, (t, Some (t1, t2)), fcs, p2) ->
        let o, p1 = self#phrase p1 in
        let o, t  = o#datatype t in
        let t1    = expand_aliases aliases t1
        and t2    = expand_aliases aliases t2 in
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
          {< aliases = SEnv.bind aliases (name, (List.map (snd ->- val_of) vars, dt)) >},
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

(*
let all_aliases_expanded aliases = 
object (self)
  inherit SugarTraversals.predicate as super

  val no_aliases = true
  val rec_vars = Types.TypeVarSet.empty
  method satisfied = no_aliases

  method rowe (fsm, rv) =
    let self =
      Utility.StringMap.fold
        (fun _ t self -> match t with
           | `Present t -> self#datatype t
           | `Absent    -> self) fsm self
    in 
    let self = 
      match Unionfind.find rv with
        | `Closed     -> self
        | `Flexible _ -> self
        | `Rigid _    -> self
        | `Recursive (t, _) when Types.TypeVarSet.mem t rec_vars -> self
        | `Recursive (t, body) ->
            let self = {< rec_vars = Types.TypeVarSet.add t rec_vars >} in
              self#rowe body in
      self

  method datatype = function
    | `Not_typed -> self
    | `Primitive _ -> self
    | `Function (f,m,t) ->
        let self = self#datatype f in
        let self = self#datatype m in
        let self = self#datatype t in
          self
    | `Record r ->
        self#rowe r
    | `Variant r ->
        self#rowe r
    | `Table (t1, t2) ->
        let self = self#datatype t1 in
        let self = self#datatype t2 in
          self
    | `Alias (_, t) -> self#datatype t
    | `Application (t, _) when SEnv.has aliases t -> {< no_aliases = false >}
    | `Application (_, ts) ->
        let self = self#list (fun o -> o#ts) ts in
          self
    | `MetaTypeVar mtv ->
        match Unionfind.find mtv with
          | `Rigid _ -> self
          | `Flexible _ -> self
          | `Recursive (t, _) when TypeVarSet.mem t rec_vars ->
              self
          | `Recursive (t, body) ->
              let self = {< rec_vars = TypeVarSet.add t rec_vars >} in
                self#datatype body
          | `Body t -> self#datatype t
    | `ForAll (_, t) -> self#datatype t
end

*)
