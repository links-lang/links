open CommonTypes
open Lens.Utility
module V = Value
module LPV = Lens.Phrase.Value

let lens_phrase_value_of_constant c =
  match c with
  | Constant.Bool b -> LPV.Bool b
  | Constant.Int i -> LPV.Int i
  | Constant.Char c -> LPV.Char c
  | Constant.Float f -> LPV.Float f
  | Constant.String s -> LPV.String s

let rec lens_phrase_value_of_value t =
  match t with
  | `Bool b -> LPV.Bool b
  | `Int i -> LPV.Int i
  | `Char c -> LPV.Char c
  | `String s -> LPV.String s
  | `Record l ->
      let l =
        List.map ~f:(fun (n, v) -> (n, lens_phrase_value_of_value v)) l
      in
      LPV.Record l
  | _ ->
      failwith
      @@ Format.asprintf "Unsupported value %a in lens_phrase_value_of_value."
           Value.pp t

let rec value_of_lens_phrase_value t =
  match t with
  | LPV.Bool b -> V.box_bool b
  | LPV.Int i -> V.box_int i
  | LPV.Float f -> V.box_float f
  | LPV.String s -> V.box_string s
  | LPV.Char c -> V.box_char c
  | LPV.Record r ->
      let r =
        List.map ~f:(fun (n, v) -> (n, value_of_lens_phrase_value v)) r
      in
      V.box_record r
  | _ ->
      failwith
      @@ Format.asprintf "Unsupported value %a in lens_phrase_value_of_value."
           LPV.pp t
