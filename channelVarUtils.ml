open Value
open Utility
open Pervasives (* PIPES *)
  (** Subtraction **)
  (* e1 - e2:
    *   e1'[x |-> V] - e2'[x |-> V] = e1' - e2'
    *   e1'[x |-> V] - e2'[x |-> W], where V =/= W = e1'[x |-> V] - e2'
    *   e1' - e2'[x |-> V], where x not in dom(e1') = e1' - e2'
    *   e1' - empty = e1'
  *)

let subtract_env (e1: Value.env) (e2: Value.env) =
  Env.fold (fun var (v, scope) (acc_env: Value.env) ->
    match Env.lookup var e2 with
      | Some v2 when v = v2 -> acc_env
      | Some v2 -> Env.bind var (v2, scope) acc_env
      | None -> Env.bind var (v, scope) acc_env) e1 Env.empty

(* Bound channels in an environment *)
let channels_in_env e =
  Env.fold (fun _var (v, _scope) acc ->
    match v with
      | (`SessionChannel _c) as c -> c :: acc
      | _ -> acc) e []


(* TODO: Maybe it would be nice to have some kind of visitors for the IR?
 * Or is it just me that's crazy enough to have to traverse it? *)
let variables_in_computation comp =
  let open Ir in
  let variable_set = ref IntSet.empty in
  let add_variable var =
    variable_set := (IntSet.add var !variable_set) in
  let rec traverse_stringmap : 'a . ('a -> unit) -> 'a stringmap -> unit =
    fun proj_fn smap -> (* (proj_fn: 'a . 'a -> 'b) (smap: 'a stringmap) : unit = *)
      StringMap.fold (fun _ v _ -> proj_fn v) smap ()
  and traverse_value = function
    | `Variable v -> add_variable v
    | `Closure (_, value)
    | `Project (_, value)
    | `Inject (_, value, _)
    | `TAbs (_, value)
    | `TApp (value, _)
    | `Coerce (value, _)
    | `Erase (_, value) -> traverse_value value
    | `XmlNode (_, v_map, vs) ->
        traverse_stringmap (traverse_value) v_map;
        List.iter traverse_value vs
    | `ApplyPure (v, vs) ->
        traverse_value v;
        List.iter traverse_value vs
    | `Extend (v_map, v_opt) ->
        traverse_stringmap (traverse_value) v_map;
        begin match v_opt with | Some v -> traverse_value v | None -> () end
    | `Constant _ -> ()
  and traverse_tail_computation = function
    | `Return value -> traverse_value value
    | `Apply (value, values) ->
        traverse_value value; List.iter traverse_value values
    | `Special s -> traverse_special s
    | `If (v, c1, c2) ->
        traverse_value v; List.iter traverse_computation [c1 ; c2]
    | `Case (scrutinee, cases, case_opt) ->
        traverse_value scrutinee;
        traverse_stringmap (fun (_, c) -> traverse_computation c) cases;
        OptionUtils.opt_iter (fun (_, c) -> traverse_computation c) case_opt
  and traverse_fundef (_, (_, _, c), _, _) = traverse_computation c
  and traverse_binding = function
    | `Let (_, (_, tc)) -> traverse_tail_computation tc
    | `Fun fd -> traverse_fundef fd
    | `Rec fds -> List.iter traverse_fundef fds
    | `Module (_, (Some bs)) -> List.iter traverse_binding bs
    | `Module _
    | `Alien _ -> ()
  and traverse_special = function
    | `Database value
    | `CallCC value
    | `Select (_, value) -> traverse_value value
    | `Wrong _ -> ()
    | `Table (v1, v2, v3, _) -> List.iter (traverse_value) [v1; v2; v3]
    | `Query (vs_opt, comp, _) ->
        OptionUtils.opt_iter
          (fun (v1, v2) -> List.iter (traverse_value) [v1; v2]) vs_opt;
        traverse_computation comp
    | `Update ((_, v), c_opt, c) ->
        traverse_value v;
        OptionUtils.opt_iter (traverse_computation) c_opt;
        traverse_computation c
    | `Delete ((_, v), c_opt) ->
        traverse_value v;
        OptionUtils.opt_iter (traverse_computation) c_opt
    | `Handle h -> traverse_handler h
    | `DoOperation (_, vs, _) -> List.iter (traverse_value) vs
    | `Choice (v, clauses) ->
        traverse_value v;
        traverse_stringmap (fun (_, c) ->
          traverse_computation c) clauses
  and traverse_computation (bnds, tc) =
    List.iter traverse_binding bnds; traverse_tail_computation tc
  and traverse_clause (_, _, c) = traverse_computation c
  and traverse_handler (h: Ir.handler) =
    traverse_computation (h.ih_comp);
    traverse_stringmap (traverse_clause) h.ih_clauses in
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
    | #primitive_value -> ()
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
  let open Pervasives in
  let show_values xs =
    String.concat "," (List.map (string_of_value) xs) in
  (* Excuse the eta, OCaml whines otherwise *)
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
      (Ir.Show_scope.show sc)
      (string_of_int v)
      (Value.Show_env.show env)
      (Ir.Show_computation.show comp))
*)

let affected_channels raise_env install_env frames =
  let added_before_raise =
    subtract_env raise_env install_env |> channels_in_env in
  (* show_frames frames; *)
  let affected_context_chans =
    List.fold_left (
      fun acc c ->
        (affected_in_context raise_env c) @ acc) [] frames in
  unduplicate (fun v1 v2 -> v1 = v2) (added_before_raise @ affected_context_chans)

