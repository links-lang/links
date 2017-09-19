open Value

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

let affected_channels raise_env install_env _frames =
  let open Pervasives in
  subtract_env raise_env install_env
  |> channels_in_env
