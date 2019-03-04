open Lens_utility
module T = Types
module PT = CommonTypes.Primitive
module LPT = Lens_phrase_type

let primitive t = `Primitive t

let rec type_of_lens_phrase_type t =
  match t with
  | LPT.Bool -> PT.Bool |> primitive
  | LPT.Int -> PT.Int |> primitive
  | LPT.Char -> PT.Char |> primitive
  | LPT.Float -> PT.Float |> primitive
  | LPT.String -> PT.String |> primitive
  | LPT.Tuple ts ->
      let _ts = List.map ~f:type_of_lens_phrase_type ts in
      failwith "Tuple type not yet supported."
  | LPT.Record r ->
      let ts = String.Map.map type_of_lens_phrase_type r in
      T.make_record_type ts

let lens_phrase_type_of_type t =
  match t with
  | `Primitive p -> (
    match p with
    | PT.Bool -> LPT.Bool
    | PT.Int -> LPT.Int
    | PT.Char -> LPT.Char
    | PT.Float -> LPT.Float
    | PT.String -> LPT.String
    | _ ->
        failwith
        @@ Format.asprintf
             "Unsupported primitive type %a in lens_phrase_type_of_type."
             Types.pp_typ t )
  | _ ->
      failwith
      @@ Format.asprintf "Unsupported type %a in lens_phrase-type_of_type."
           Types.pp_typ t
