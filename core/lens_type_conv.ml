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

let lens_type_of_type t =
  match t with
  | `Lens l -> l
  | _ -> failwith "Expected a lens type."

let sort_cols_of_table t ~table =
  let record_fields rt =
    match rt with
    | LPT.Record fields -> fields
    | _ -> failwith "Expected a record type."
  in
  let sort_cols_of_record t =
    let fields = record_fields t in
    let cols =
      String.Map.to_list
        (fun name typ ->
          let alias = name in
          Lens_column.make ~table ~name ~alias ~typ ~present:true )
        fields
    in
    cols
  in
  (* get the underlying record type of either a table, a record or an application *)
  let extract_record_type t =
    match t with
    | `Record _ as r -> r
    | `Application (_, [`Type (`Record _ as r)]) -> r
    | `Table (r, _, _) -> r
    | _ -> failwith "LensTypes does not type."
  in
  let rt = extract_record_type t |> lens_phrase_type_of_type in
  sort_cols_of_record rt
