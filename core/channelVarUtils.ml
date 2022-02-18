open Value
open Utility

(* We can't use the visitor in ir.ml, since we don't have access to the
 * typing environment at this point. It would be good to replace this with
 * a visitor (or better, an implementation using the built tables) but that
 * will have to come later. *)
let variables_in_computation comp =
  Debug.print ("Getting variables in computation: " ^ (Ir.show_computation comp));
  let open Ir in
  let variable_set = ref IntSet.empty in
  let add_variable var =
    variable_set := (IntSet.add var !variable_set) in
  let rec traverse_stringmap : 'a . ('a -> unit) -> 'a stringmap -> unit =
    fun proj_fn smap -> (* (proj_fn: 'a . 'a -> 'b) (smap: 'a stringmap) : unit = *)
      StringMap.fold (fun _ v _ -> proj_fn v) smap ()
  and traverse_value = function
    | Variable v -> add_variable v
    | Closure (_, _, value)
    | Project (_, value)
    | Inject (_, value, _)
    | TAbs (_, value)
    | TApp (value, _)
    | Coerce (value, _)
    | Erase (_, value) -> traverse_value value
    | XmlNode (_, v_map, vs) ->
        traverse_stringmap (traverse_value) v_map;
        List.iter traverse_value vs
    | ApplyPure (v, vs) ->
        traverse_value v;
        List.iter traverse_value vs
    | Extend (v_map, v_opt) ->
        traverse_stringmap (traverse_value) v_map;
        begin match v_opt with | Some v -> traverse_value v | None -> () end
    | Constant _ -> ()
  and traverse_tail_computation = function
    | Return value -> traverse_value value
    | Apply (value, values) ->
        traverse_value value; List.iter traverse_value values
    | Special s -> traverse_special s
    | If (v, c1, c2) ->
        traverse_value v; List.iter traverse_computation [c1 ; c2]
    | Case (scrutinee, cases, case_opt) ->
        traverse_value scrutinee;
        traverse_stringmap (fun (_, c) -> traverse_computation c) cases;
        OptionUtils.opt_iter (fun (_, c) -> traverse_computation c) case_opt
  and traverse_fundef {fn_binder = bnd; _} =
    let fun_var = Var.var_of_binder bnd in
    match Tables.(lookup fun_defs fun_var) with
      | Some (_, _, (Some fvs_var), _) ->
          Debug.print ("fvs_var: " ^ (string_of_int fvs_var));
          add_variable fvs_var
      | Some _fd -> Debug.print ("fundef without fvs var attached")
      | _ -> ()
    (* traverse_computation c *)
  and traverse_binding = function
    | Let (_, (_, tc)) -> traverse_tail_computation tc
    | Fun fd ->
        Debug.print "traversing fundef";
        traverse_fundef fd
    | Rec fds -> List.iter traverse_fundef fds
    | Module (_, (Some bs)) -> List.iter traverse_binding bs
    | Module _
    | Alien _ -> ()
  and traverse_special = function
    | Database value
    | CallCC value
    | Select (_, value) -> traverse_value value
    | Wrong _ -> ()
    | Table  { database; table; keys; _ } ->
        List.iter (traverse_value) [database; table; keys]
    | Query (vs_opt, _, comp, _) ->
        OptionUtils.opt_iter
          (fun (v1, v2) -> List.iter (traverse_value) [v1; v2]) vs_opt;
        traverse_computation comp
    | InsertRows (_, v, r) ->
        traverse_value v;
        traverse_value r
    | InsertReturning (_, v, r, s) ->
        traverse_value v;
        traverse_value r;
        traverse_value s
    | Update (upd, (_, v), c_opt, c) ->
        begin
          match upd with
            | Some (Ir.ValidTimeUpdate (SequencedUpdate { validity_from; validity_to })) ->
                traverse_value validity_from;
                traverse_value validity_to
            | Some (Ir.ValidTimeUpdate (NonsequencedUpdate { from_time; to_time })) ->
                OptionUtils.opt_iter (traverse_computation) from_time;
                OptionUtils.opt_iter (traverse_computation) to_time
            | _ -> ()
        end;
        traverse_value v;
        OptionUtils.opt_iter (traverse_computation) c_opt;
        traverse_computation c
    | Delete (del, (_, v), c_opt) ->
        begin
          match del with
            | Some (Ir.ValidTimeDeletion (SequencedDeletion { validity_from; validity_to })) ->
                traverse_value validity_from;
                traverse_value validity_to
            | _ -> ()
        end;
        traverse_value v;
        OptionUtils.opt_iter (traverse_computation) c_opt
    | TemporalJoin (_, c, _) -> traverse_computation c
    | Handle h -> traverse_handler h
    | DoOperation (_, vs, _) -> List.iter (traverse_value) vs
    | Choice (v, clauses) ->
        traverse_value v;
        traverse_stringmap (fun (_, c) ->
          traverse_computation c) clauses
    | Lens (value, _)
    | LensSerial { lens = value; _ }
    | LensSelect { lens = value; _ }
    | LensCheck (value, _)
    | LensGet (value, _) -> traverse_value value
    | LensDrop { lens = v1; default = v2; _ }
    | LensJoin { left = v1; right = v2; _ }
    | LensPut (v1, v2, _) -> List.iter (traverse_value) [v1; v2]
  and traverse_computation (bnds, tc) =
    List.iter traverse_binding bnds; traverse_tail_computation tc
  and traverse_clause (_, _, c) = traverse_computation c
  and traverse_handler (h: Ir.handler) =
    traverse_computation (h.ih_comp);
    traverse_stringmap (traverse_clause) h.ih_cases;
    traverse_computation (snd h.ih_return)
  in
  traverse_computation comp;
  IntSet.elements (!variable_set)

(* Key point: IR variables are unique (HURRAH!) -- so no need to
 * worry about shadowing. All we need to do is:
   * 1) traverse each frame's computation to gather the referenced variables
   * 2) for each variable, check whether it's in the environment upon handler
   *    installation. If so, and it's a channel, add to the variable set
   * 3) fold over variable set in order to resolve to channels *)


let sessions_in_value v =
  let values = ref [] in
  let add_value v = values := (v :: (!values)) in

  let rec go = function
    | `List vs -> List.iter (go) vs
    | `Record r -> List.iter (fun (_, v) -> go v) r
    | `Variant (_, v) -> go v
    | `FunctionPtr (_, (Some fvs)) -> go fvs
    | (`SessionChannel _) as c ->
        Debug.print ("affected value: " ^ (Value.string_of_value c));
        add_value c
    | v ->
        Debug.print ("not-affected value: " ^ (Value.string_of_value v)) in
  go v; (!values)


let affected_in_context (raise_env: Value.env) comp =
  let show_values xs =
    String.concat "," (List.map (string_of_value) xs) in
  let affected_values =
    List.fold_left (fun acc v ->
      match Value.Env.lookup v raise_env with
        | Some v -> v :: acc
        | None -> acc
    ) [] (variables_in_computation comp) in
  let res = List.map (sessions_in_value) affected_values |> List.concat in
  Debug.print ("Final affected values: " ^ (show_values res));
  res

(*
let show_frames =
  List.iter (fun (sc, v, env, comp) ->
    Printf.printf "FRAME: \n scope: %s \n var: %s \n env: %s \n comp: %s \n \n"
      (Ir.show_scope sc)
      (string_of_int v)
      (Value.show_env env)
      (Ir.show_computation comp))
*)

let affected_channels raise_env frames =
  let affected_context_chans =
    List.fold_left (
      fun acc c ->
        (affected_in_context raise_env c) @ acc) [] frames in
  unduplicate (fun v1 v2 -> v1 = v2) (affected_context_chans)
