open CommonTypes
open Var
open Utility
open Lens.Utility

module LP = Lens.Phrase

module I = Ir


let internal_error message =
  Errors.internal_error ~filename:"lens_ir_conv.ml" ~message

(** This code is mostly taken and adapted from query/query.ml *)

module IrValue = struct
  type t =
    | Closure of closure
    | Phrase of Lens.Phrase.t
    | Primitive of string
    | Record
  and closure = (Ir.var list * Ir.computation) * env
  and env = Value.env * t Env.Int.t
  [@@deriving show]
end

let env_of_value_env value_env = (value_env, Env.Int.empty)
let (++) (venv, eenv) (venv', eenv') =
  Value.Env.shadow venv ~by:venv', Env.Int.extend eenv eenv'

let lookup_fun (f, fvs) =
  match Tables.lookup Tables.fun_defs f with
  | Some (finfo, (xs, body), z, location) ->
    let fn = match location with
    | Location.Server | Location.Unknown ->
      let env =
        match z, fvs with
        | None, None -> Value.Env.empty
        | Some z, Some fvs -> Value.Env.bind z (fvs, Scope.Local) Value.Env.empty
        | _, _ -> assert false in
      IrValue.Closure ((xs, body), env_of_value_env env)
    | Location.Client ->
      raise (Errors.runtime_error (Js.var_name_binder (f, finfo) |> Format.asprintf "Attempt to use client function: %s in query"))
    | Location.Native ->
      raise (Errors.runtime_error (Var.show_binder (f, finfo) |> Format.asprintf "Attempt to use native function: %s in query")) in
    Some fn
  | None -> None

let expression_of_value value =
  match value with
  | `PrimitiveFunction (f, _) -> IrValue.Primitive f
  | _ ->
    let pv = Lens_value_conv.lens_phrase_value_of_value value in
    let p = Lens.Phrase.Constant.of_value pv in
    IrValue.Phrase p

let lookup (val_env, exp_env) var =
  match lookup_fun (var, None) with
  | Some v -> v
  | None ->
    begin
      match Value.Env.lookup var val_env, Env.Int.find exp_env var with
      | None, Some v -> v
      | Some v, None -> expression_of_value v
      | Some _, Some v -> v (*eval_error "Variable %d bound twice" var*)
      | None, None ->
        begin
          try expression_of_value (Lib.primitive_stub (Lib.primitive_name var)) with
          | NotFound _ ->
            raise (internal_error (Format.sprintf "Variable %d not found" var));
        end
    end

module Of_ir_error = struct
  type t =
    | Operator_not_supported_binary of string
    | Operator_not_supported_unary of string
    | Unsupported_function_value of Value.t
    | Client_function
    | Internal_error
    | Recursive_function
    | Modules_unsupported
    | Application_of_nonfunction
    | Unsupported_tail_computation of Ir.tail_computation
    | Unexpected_closure of IrValue.closure
    | Unexpected_primitive of string
    | Unexpected_record
    | Unexpected_phrase of Lens.Phrase.t
    | Unsupported_arbitrary_if of Ir.tail_computation

  exception E of t

  let to_string v =
    match v with
    | Operator_not_supported_binary v ->
      Format.asprintf "Relational lenses do not support the primitive binary operator '%s'." v
    | Operator_not_supported_unary v ->
      Format.asprintf "Relational lenses do not support the primitive unary operator '%s'." v
    | Unsupported_function_value v ->
      Format.asprintf "Relational lenses does not support function value %a."
        Value.pp v
    | Client_function -> "Relational lenses do not support client functions."
    | Internal_error -> "Internal error."
    | Recursive_function -> "Relational lenses do not support recursive functions."
    | Modules_unsupported -> "Relational lenses do not support modules yet."
    | Application_of_nonfunction -> "Application of non-function."
    | Unexpected_phrase p -> Format.asprintf "Unexpected phrase %a." Lens.Phrase.pp p
    | Unexpected_closure c -> Format.asprintf "Unexpected closure %a. This should not happen." IrValue.pp_closure c
    | Unexpected_primitive f -> Format.asprintf "Unexpected primitive %s. This should not happen." f
    | Unexpected_record -> Format.asprintf "Unexpected record. Primary operations can only be applied to primitives."
    | Unsupported_tail_computation c -> Format.asprintf "Unsupported tail computation %a." Ir.pp_tail_computation c
    | Unsupported_arbitrary_if c -> Format.asprintf "Arbitrary if expressions are not support. %a" Ir.pp_tail_computation c

  let unpack_exn ~die v =
    match v with
    | Result.Ok v -> v
    | Result.Error e ->
      to_string e |> die

  let raise v =
    raise (E v)
end

module Primitives = struct
  let binary_of_string f =
    match f with
    | "+" -> Lens.Operators.Binary.Plus |> Result.return
    | "-" -> Lens.Operators.Binary.Minus |> Result.return
    | "*" -> Lens.Operators.Binary.Multiply |> Result.return
    | "/" -> Lens.Operators.Binary.Divide |> Result.return
    | "==" -> Lens.Operators.Binary.Equal |> Result.return
    | ">" -> Lens.Operators.Binary.Greater |> Result.return
    | ">=" -> Lens.Operators.Binary.GreaterEqual |> Result.return
    | "<" -> Lens.Operators.Binary.Less |> Result.return
    | "<=" -> Lens.Operators.Binary.LessEqual |> Result.return
    | _ -> Result.error (Of_ir_error.Operator_not_supported_binary f)

  let unary_of_string f =
    match f with
    | "not" -> Lens.Operators.Unary.Not |> Result.return
    | "negate" -> Lens.Operators.Unary.Minus |> Result.return
    | "negatef" -> Lens.Operators.Unary.Minus |> Result.return
    | _ -> Result.error (Of_ir_error.Operator_not_supported_unary f)
end

let lens_sugar_phrase_of_ir p env =
  let bind (val_env, exp_env) (x, v) =
    (val_env, Env.Int.bind exp_env (x, v))
  in
  let rec computation env (binders, tailcomp) =
    match binders with
    | [] -> tail_computation env tailcomp
    | b :: bs ->
      match b with
      | I.Let (xb, (_, tc)) ->
        let x = Var.var_of_binder xb in
        let v = tail_computation env tc in
        Result.bind ~f:(fun v -> computation (bind env (x, v)) (bs, tailcomp)) v
      | I.Fun (_, _, _, (Location.Client | Location.Native)) -> Result.error Of_ir_error.Client_function
      | I.Fun _ -> Result.error Of_ir_error.Internal_error
      | I.Rec _ -> Result.error Of_ir_error.Recursive_function
      | I.Alien _ -> computation env (bs, tailcomp)
      | I.Module _ -> Result.error Of_ir_error.Modules_unsupported
  and unpack_phrase v =
    match v with
    | IrValue.Phrase p -> Result.return p
    | IrValue.Closure c -> Result.error (Of_ir_error.Unexpected_closure c)
    | IrValue.Primitive f -> Result.error (Of_ir_error.Unexpected_primitive f)
    | IrValue.Record -> Result.error Of_ir_error.Unexpected_record
  and unpack_record v =
    match v with
    | IrValue.Phrase p -> Result.error (Of_ir_error.Unexpected_phrase p)
    | IrValue.Closure c -> Result.error (Of_ir_error.Unexpected_closure c)
    | IrValue.Primitive f -> Result.error (Of_ir_error.Unexpected_primitive f)
    | IrValue.Record -> Result.return ()
  and apply env (v, args) =
    let open Result.O in
    match v, args with
    | IrValue.Closure ((xs, body), closure_env), _ ->
      let env = env ++ closure_env in
      let env = List.fold_right2 (fun x arg env ->
           bind env (x, arg)) xs args env in
      computation env body
    | IrValue.Primitive f, [v1; v2] ->
        Primitives.binary_of_string f
        >>= fun op ->
        unpack_phrase v1
        >>= fun v1 ->
        unpack_phrase v2
        >>| fun v2 ->
        let p = Lens.Phrase.infix op v1 v2 in
        IrValue.Phrase p
    | IrValue.Primitive f, [v1] ->
      Primitives.unary_of_string f
      >>= fun op ->
      unpack_phrase v1
      >>| fun v1 ->
      let p = Lens.Phrase.UnaryAppl (op, v1) in
      IrValue.Phrase p
    | _ -> Result.error Of_ir_error.Application_of_nonfunction
  and tail_computation env comp =
    let open Result.O in
    match comp with
    | I.Return v -> value env v
    | I.If (v, ct, cf) ->
      value env v
      >>= fun v ->
      unpack_phrase v
      >>= fun v ->
      computation env ct
      >>= fun ct ->
      computation env cf
      >>= fun cf ->
      (match cf with
      | IrValue.Phrase (Lens.Phrase.Constant (Lens.Phrase.Value.Bool false))
        ->
        unpack_phrase ct >>| fun ct ->
        IrValue.Phrase (Lens.Phrase.and' v ct)
      | _ ->
        (match ct with
        | IrValue.Phrase (Lens.Phrase.Constant (Lens.Phrase.Value.Bool true
                                               )) ->
          unpack_phrase cf >>| fun cf ->
          IrValue.Phrase (Lens.Phrase.or' v cf)
        | _ -> Result.error (Of_ir_error.Unsupported_arbitrary_if comp))
      )
    | I.Apply (f, args) ->
      let f = value env f in
      let args = List.map_result ~f:(value env) args in
      Result.bind ~f:(fun f ->
        Result.bind ~f:(fun args -> apply env (f, args)) args) f
    | _ -> Result.error (Of_ir_error.Unsupported_tail_computation comp)
  and value env p =
    let open Result.O in
    match p with
    | I.Constant c ->
      let c = Lens_value_conv.lens_phrase_value_of_constant c in
      let p = Lens.Phrase.Constant.of_value c in
      IrValue.Phrase p |> Result.return
    | I.Variable var -> lookup env var |> Result.return
    | I.TAbs (_, v) -> value env v
    | I.TApp (v, _) -> value env v
    | I.Project (n, r) ->
      value env r
      >>= fun r ->
      unpack_record r
      >>| fun () ->
      let p = Lens.Phrase.var n in
      IrValue.Phrase p
    | I.ApplyPure (f, args) ->
      let f = value env f in
      let args = List.map_result ~f:(value env) args in
      Result.bind ~f:(fun f ->
          Result.bind ~f:(fun args -> apply env (f, args)) args) f
    | _ -> Format.asprintf "Could not convert value %a to lens sugar phrase." I.pp_value p |> failwith
  in
  let initial env p =
    match p with
    | I.Variable var -> (
        match lookup env var with
        | IrValue.Closure (([v], comp), closure_env) ->
          let env = env ++ closure_env in
          let env = bind env (v, IrValue.Record) in
          computation env comp
        | _ as v -> Format.asprintf "unsupported value %a." IrValue.pp v |> failwith
      )
    | _ -> Format.asprintf "unsupported initial %a" I.pp_value  p |> failwith in
  let open Result.O in
  let env = env, Env.Int.empty in
  initial env p >>= unpack_phrase
