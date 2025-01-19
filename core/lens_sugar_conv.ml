open Lens.Operators
open Operators
open SourceCode
open Lens.Utility
module LPS = Lens.Phrase.Sugar
module S = Sugartypes

let unary_of_sugartype_op v =
  let open Unary in
  match v with
  | UnaryOp.Minus -> Some Minus
  | UnaryOp.FloatMinus -> Some Minus
  | _ -> None

let binary_of_sugartype_op v =
  let open Binary in
  match v with
  | BinaryOp.Minus -> Some Minus
  | BinaryOp.FloatMinus -> Some Minus
  | BinaryOp.And -> Some LogicalAnd
  | BinaryOp.Or -> Some LogicalOr
  | BinaryOp.Name "+" -> Some Plus
  | BinaryOp.Name "*" -> Some Multiply
  | BinaryOp.Name "/" -> Some Divide
  | BinaryOp.Name ">" -> Some Greater
  | BinaryOp.Name "<" -> Some Less
  | BinaryOp.Name ">=" -> Some GreaterEqual
  | BinaryOp.Name "<=" -> Some LessEqual
  | BinaryOp.Name "==" -> Some Equal
  | _ -> None

let cols_of_phrase key : string list =
  let open Sugartypes in
  let var_name (var : phrase) =
    match WithPos.node var with
    | Var name -> name
    | _ -> failwith "Expected a `Var type"
  in
  match WithPos.node key with
  | TupleLit keys -> List.map ~f:var_name keys
  | Var name -> [ name ]
  | _ -> failwith "Expected a tuple or a variable."

module Error = struct
  type t = Internal_error of string [@@deriving show]

  let internal_error_res msg = Result.error (Internal_error msg)
end

let is_static _typ p =
  let rec no_ext_deps v p =
    match p |> WithPos.node with
    | S.Block ([], body) -> no_ext_deps v body
    | S.InfixAppl (_, left, right) -> no_ext_deps v left && no_ext_deps v right
    | S.Var v' -> S.Binder.to_name v = v'
    | S.Projection (body, _) -> no_ext_deps v body
    | S.Constant _ -> true
    | _ -> false
  in
  (* If the given argument is a function of the form `fun (x) { body }`, check
       if body contains any external references. If it does, then it is dynamic,
       otherwise it is static. *)
  match WithPos.node p with
  | S.FunLit (_, _, Sugartypes.NormalFunlit ([ [ var ] ], body), _) -> (
      let var = WithPos.node var in
      match var with
      | S.Pattern.Variable x -> no_ext_deps x body
      | _ -> false)
  | _ -> false

let rec lens_sugar_phrase_of_body v p =
  let open Result.O in
  let conv p = lens_sugar_phrase_of_body v p in
  let pos = WithPos.pos p in
  (match WithPos.node p with
  | S.InfixAppl ((_, op), p, q) ->
      let op = binary_of_sugartype_op op |> fun v -> Option.value_exn v in
      conv p >>= fun p ->
      conv q >>| fun q -> LPS.InfixAppl (op, p, q)
  | S.UnaryAppl ((_, op), p) ->
      let op = unary_of_sugartype_op op |> fun v -> Option.value_exn v in
      conv p >>| fun p -> LPS.UnaryAppl (op, p)
  | S.Constant c ->
      LPS.Constant (Lens_value_conv.lens_phrase_value_of_constant c)
      |> Result.return
  | S.Block ([], body) -> conv body >>| LPS.node
  | S.Projection (var, field) ->
      (match var |> WithPos.node with
      | S.Var v' ->
          if v = v' then Result.return ()
          else
            Format.asprintf "Unexpected external variable: %s" v'
            |> Error.internal_error_res
      | _ ->
          Format.asprintf "Unexpected expression to project on: %a" S.pp_phrase
            var
          |> Error.internal_error_res)
      >>| fun () -> LPS.Var field
  | _ ->
      Format.asprintf
        "Unsupported sugar phrase in lens sugar phrase of body: %a" S.pp_phrase
        p
      |> Error.internal_error_res)
  >>| fun v -> (pos, v)

let lens_sugar_phrase_of_sugar p =
  (match WithPos.node p with
  | S.FunLit (_, _, Sugartypes.NormalFunlit ([ [ var ] ], body), _) -> (
      let var = WithPos.node var in
      match var with
      | S.Pattern.Variable x ->
          lens_sugar_phrase_of_body (S.Binder.to_name x) body
      | _ ->
          Format.asprintf "Unsupported binder: %a" S.pp_phrase p
          |> Error.internal_error_res)
  | S.FunLit (_, _, Sugartypes.SwitchFunlit (_, _), _) -> assert false
  | _ -> lens_sugar_phrase_of_body "" p)
  |> Result.ok_exn
