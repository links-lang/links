open Lens.Utility
module T = Types
module PT = CommonTypes.Primitive
module LPT = Lens.Phrase.Type

type 'a die = string -> 'a

let primitive t = `Primitive t

let to_links_map m =
  String.Map.fold
    (fun k v m -> Utility.StringMap.add k v m)
    m Utility.StringMap.empty

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
      T.make_record_type (to_links_map ts)

let rec lens_phrase_type_of_type t =
  match TypeUtils.concrete_type t with
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
  | `Record (r, _, _) ->
      let fields =
        Utility.StringMap.to_alist r
        |> String.Map.from_alist
        |> String.Map.map (fun v ->
               match v with
               | `Present t -> lens_phrase_type_of_type t
               | _ ->
                   failwith
                     "lens_phrase_type_of_type only works on records with \
                      present types.")
      in
      LPT.Record fields
  | _ ->
      failwith
      @@ Format.asprintf "Unsupported type %a in lens_phrase_type_of_type."
           Types.pp_typ t

let lens_type_of_type ~die t =
  match TypeUtils.concrete_type t with
  | `Lens l -> l
  | _ -> die "Expected a lens type."

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
          Lens.Column.make ~table ~name ~alias ~typ ~present:true)
        fields
    in
    cols
  in
  (* get the underlying record type of either a table, a record or an application *)
  let extract_record_type t =
    match TypeUtils.concrete_type t with
    | `Record _ as r -> r
    | `Application (_, [`Type (`Record _ as r)]) -> r
    | `Table (r, _, _) -> r
    | _ -> failwith "LensTypes does not type."
  in
  let rt = extract_record_type t |> lens_phrase_type_of_type in
  sort_cols_of_record rt
