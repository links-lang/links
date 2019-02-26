open Utility
open Types
open SourceCode
open Lens_operators
open Lens_utility

module Alias = Lens_alias

type t = lens_phrase
type node = lens_phrasenode

let source_position phrase = WithPos.pos phrase

let node phrase = WithPos.node phrase

let with_pos ~pos phrase = WithPos.make ~pos phrase

let dummy_pos = Position.dummy

let var ?(pos = dummy_pos) n = Var n |> with_pos ~pos

let infix ?(pos = dummy_pos) op v1 v2 = InfixAppl (op, v1, v2) |> with_pos ~pos

let and' ?(pos = dummy_pos) v1 v2 = InfixAppl (Binary.Logical Logical_binop.And, v1, v2) |> with_pos ~pos

let or' ?(pos = dummy_pos) v1 v2 = InfixAppl (Binary.Logical Logical_binop.Or, v1, v2) |> with_pos ~pos

let equal ?(pos = dummy_pos) v1 v2 = InfixAppl (Binary.Equal, v1, v2) |> with_pos ~pos

let not' ?(pos = dummy_pos) v1 = UnaryAppl (Unary.Not, v1) |> with_pos ~pos

let tuple ?(pos = dummy_pos) v = TupleLit v |> with_pos ~pos

let tuple_singleton ?(pos = dummy_pos) v = tuple ~pos [v]

let name_of_var expr =
  match WithPos.node expr with
  | Sugartypes.Var n -> n
  | _ -> failwith "Expected var."

let of_phrase p =
  let rec f p =
    let phrase =
      match WithPos.node p with
      | Sugartypes.Constant c -> Constant c
      | Sugartypes.Var v -> Var v
      | Sugartypes.UnaryAppl ((_, op), phrase) -> UnaryAppl (Unary.from_links op, f phrase)
      | Sugartypes.InfixAppl ((_, op), phrase1, phrase2) -> InfixAppl (Binary.of_supertype_operator op, f phrase1, f phrase2)
      | Sugartypes.TupleLit l -> TupleLit (List.map ~f l)
      | Sugartypes.FnAppl (fn, arg) ->
        begin
          match name_of_var fn with
          | "not" -> UnaryAppl ((Unary.Name "!"), f (List.hd arg))
          | _ -> failwith "Unsupported function"
        end
      | _ -> failwith "Unknown phrasenode for lens_phrase to phrase." in
    WithPos.map ~f:(fun _ -> phrase) p
  in
  f p

let rec traverse expr ~f =
  let fn expr' = traverse expr' ~f in
  let pos = source_position expr in
  let expr = node expr in
  let expr =
    match expr with
    | Constant _ -> expr
    | Var _ -> expr
    | UnaryAppl (a, arg) ->
      let arg = fn arg in
      UnaryAppl (a, arg)
    | InfixAppl (a, a1, a2) ->
      let a1 = fn a1 in
      let a2 = fn a2 in
      InfixAppl (a, a1, a2)
    | TupleLit (x :: []) ->
      let x = fn x in
      TupleLit ([x])
    | Case (phr, cases, otherwise) ->
      let phr = OptionUtils.opt_map fn phr in
      let cases = List.map ~f:(fun (inp, lst) -> fn inp, fn lst) cases in
      let otherwise = fn otherwise in
      Case (phr, cases, otherwise)
    | _ -> failwith "Unknown operation" in
  f expr |> with_pos ~pos

let get_vars expr =
  let cols = ref Alias.Set.empty in
  let _ = traverse expr ~f:(fun expr ->
      match expr with
      | Var n -> cols := Alias.Set.add n (!cols); expr
      | _ -> expr) in
  !cols

let rename_var expr ~replace =
  traverse expr ~f:(fun expr ->
      match expr with
      | Var key ->
        Alias.Map.find ~key replace
        |> Option.map ~f:(fun v -> Var v)
        |> Option.value ~default:expr
      | _ -> expr
    )

module Constant = struct
  open Lens_constant

  let bool ?(pos = Position.dummy) v = Constant (bool v) |> with_pos ~pos

  let int ?(pos = Position.dummy) v = Constant (int v) |> with_pos ~pos

  let of_value ?(pos = Position.dummy) v = Constant (of_value v) |> with_pos ~pos
end

let replace_var expr ~replace =
  traverse expr ~f:(fun expr ->
      match expr with
      | Var key ->
        Alias.Map.find ~key replace
        |> Option.map ~f:(Constant.of_value ~pos:Position.dummy ->- node)
        |> Option.value ~default:expr
      | _ -> expr
    )

let rec eval expr get_val =
  let open Value in
  match node expr with
  | Constant c -> Lens_constant.to_value c
  | Var v ->
    begin
      try get_val v
      with NotFound _ -> failwith ("Could not find column " ^ v ^ ".")
    end
  | InfixAppl (op, a1, a2) ->
    let a1 = eval a1 get_val in
    let a2 = eval a2 get_val in
    begin
      match op with
      | Binary.Equal -> box_bool (
          match a1 with
          | `Bool b -> b == unbox_bool a2
          | `Int i -> i == unbox_int a2
          | `String s -> s = unbox_string a2
          | _ -> failwith "Unsupported equality constant.")
      | Binary.Logical Logical_binop.And -> box_bool (unbox_bool a1 && unbox_bool a2)
      | Binary.Name "+" -> box_int ((unbox_int a1) + (unbox_int a2))
      | Binary.Name "*" -> box_int (unbox_int a1 * unbox_int a2)
      | Binary.Minus -> box_int (unbox_int a1 - unbox_int a2)
      | Binary.Name ">" -> box_bool (unbox_int a1 > unbox_int a2)
      | Binary.Name "<" -> box_bool (unbox_int a1 < unbox_int a2)
      | _ as v -> failwith ("Unknown binary operation " ^ Binary.to_string v ^ ".")
    end
  | TupleLit l -> eval (List.hd l) get_val
  | UnaryAppl (op, arg) ->
    begin
      match op with
      | Unary.Not ->
        let res = eval arg get_val in
        let res = not (unbox_bool res) in
        box_bool res
      | Unary.Minus ->
        let res = eval arg get_val in
        (match res with
         | `Float v -> -. v |> box_float
         | `Int v -> - v |> box_int
         | _ -> failwith (
             Format.asprintf "Value '%a' does not support the unary minus operator." Value.pp res))
      | op -> failwith ("Unsupported unary operation " ^ Unary.to_string op)
    end
  | In (names, vals) ->
    let find = List.map ~f:get_val names in
    let vals = List.map ~f:(List.map ~f:Lens_constant.to_value) vals in
    let res = List.mem find vals in
    box_bool res
  | Case (inp, cases, otherwise) ->
    let inp = match inp with
      | None -> `Bool true
      | Some inp -> eval inp get_val in
    try
      let (_k,v) = List.find (fun (k,_v) -> eval k get_val = inp) cases in
      eval v get_val
    with
      NotFound _ -> eval otherwise get_val
    | _ -> failwith "Unknown phrasenode for calculate_predicate."


module Option = struct
  type elt = t
  type t = elt option

  let combine_and phrase1 phrase2 =
    let tup_or x =
      match node x with
      | InfixAppl (Binary.Logical Logical_binop.Or, _, _) -> tuple_singleton x
      | _ -> x in
    Option.combine ~f:(fun v1 v2 -> and' (tup_or v1) (tup_or v2)) phrase1 phrase2

  let combine_or phrase1 phrase2 =
    Option.combine ~f:(or' ~pos:Position.dummy) phrase1 phrase2

  let in_expr names vals =
    if names = [] then
      None
    else if vals = [] then
      Some (Constant.bool false)
    else
      let val_of_rec r = List.map ~f:Lens_constant.of_value r in
      let vals = List.map ~f:val_of_rec vals in
      Some (In (names, vals) |> with_pos ~pos:Position.dummy)

end

module Record = struct
  type record = Value.t

  module Record = Lens_record

  let eval t r =
    let get_val = fun key -> Record.get ~key r in
    eval t get_val

  let matching_cols_simp on row =
    let phrase = List.fold_left (fun phrase (on,v) ->
        let term = Some (equal (var on) (Constant.of_value v)) in
        Option.combine_and phrase term
      ) None (List.combine on row) in
    phrase

  let matching_cols on row =
    let phrase = List.fold_left (fun phrase on ->
        let term = Some (equal (var on) (Record.get row ~key:on |> Constant.of_value ~pos:Position.dummy)) in
        Option.combine_and phrase term
      ) None (Alias.Set.elements on) in
    phrase
end

module List = struct
  type elt = t
  type t = elt list

  let fold_and l =
    List.fold_left (fun phrase term -> Option.combine_and phrase (Some term)) None l

  let fold_and_opt l =
    List.filter_opt l |> fold_and

  let fold_or phrases =
    let ored = List.fold_left (fun phrase term -> Option.combine_or phrase (Some term)) None phrases in
    match ored with
    | None -> Some (Constant.bool false)
    | _ -> ored

  let fold_or_opt l =
    List.filter_opt l |> fold_or
end

module O = struct
  let (>) a b = infix (Lens_operators.Binary.of_string ">") a b

  let (<) a b = infix (Lens_operators.Binary.of_string "<") a b

  let (=) a b = infix (Lens_operators.Binary.of_string "=") a b

  let (&&) a b = infix (Lens_operators.Binary.Logical Lens_operators.Logical_binop.And) a b

  let (||) a b = infix (Lens_operators.Binary.Logical Lens_operators.Logical_binop.Or) a b

  let v a = var a

  let i v = Constant.int v

  let b b = Constant.bool b
end

module Grouped_variables = struct
  module Inner = Alias.Set

  include Set.Make (Inner)

  let times s1 s2 =
    fold (fun e acc -> map (Inner.union e) acc) s1 s2

  let of_lists l =
    Lens_list.map ~f:Inner.of_list l |> of_list

  let rec gtv p =
    match node p with
    | Var v -> Inner.singleton v |> singleton
    | Constant _ -> singleton Inner.empty
    | InfixAppl (Lens_operators.Binary.Logical Logical_binop.And, p1, p2) ->
      let s1 = gtv p1 in
      let s2 = gtv p2 in
      union s1 s2
    | InfixAppl (_, p1, p2) ->
      let s1 = gtv p1 in
      let s2 = gtv p2 in
      times s1 s2
    | _ -> failwith "Grouped type variables does not support this operator."

  let has_partial_overlaps t ~cols =
    exists (fun gr ->
        let int_not_empty =
          Inner.inter gr cols
          |> Inner.is_empty
          |> not in
        let diff_not_empty =
          Inner.diff gr cols
          |> Inner.is_empty
          |> not in
        int_not_empty && diff_not_empty) t
end
