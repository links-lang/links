open Lens.Utility

module T = Types
module PT = CommonTypes.Primitive
module LPT = Lens.Phrase.Type

type 'a die = string -> 'a

let to_links_map m =
  String.Map.fold
    (fun k v m -> Utility.StringMap.add k v m)
    m Utility.StringMap.empty

let lookup_alias context ~alias =
  match Env.String.find_opt alias context with
  | Some (`Alias (k, _, body)) ->
      let tycon = (alias, [], [], false) in
      T.Alias (k, tycon, body)
  | _ -> Errors.MissingBuiltinType alias |> raise

let rec type_of_lens_phrase_type ~context t =
  match t with
  | LPT.Bool -> T.bool_type
  | LPT.Int -> T.int_type
  | LPT.Serial -> lookup_alias context ~alias:"Serial"
  | LPT.Char -> T.char_type
  | LPT.Float -> T.float_type
  | LPT.String -> T.string_type
  | LPT.Tuple ts ->
      let ts = List.map ~f:(type_of_lens_phrase_type ~context) ts in
      T.make_tuple_type ts
  | LPT.Record r ->
      let ts = String.Map.map (type_of_lens_phrase_type ~context) r in
      T.make_record_type (to_links_map ts)

let rec lens_phrase_type_of_type t =
  match TypeUtils.concrete_type t with
  | T.Primitive p -> (
      match p with
      | PT.Bool -> LPT.Bool
      | PT.Int -> LPT.Int
      | PT.Char -> LPT.Char
      | PT.Float -> LPT.Float
      | PT.String -> LPT.String
      | _ ->
          failwith
          @@ Format.asprintf
               "Unsupported primitive type %a in lens_phrase_type_of_type." T.pp
               t)
  | T.Record r -> lens_phrase_type_of_type r
  | T.Row (fields, _, _) ->
      let fields =
        Utility.StringMap.to_alist fields
        |> String.Map.from_alist
        |> String.Map.map (fun v ->
               match v with
               | T.Present t -> lens_phrase_type_of_type t
               | _ ->
                   failwith
                     "lens_phrase_type_of_type only works on records with \
                      present types.")
      in
      LPT.Record fields
  | _ ->
      failwith
      @@ Format.asprintf "Unsupported type %a in lens_phrase_type_of_type." T.pp
           t

let lens_type_of_type ~die t =
  match TypeUtils.concrete_type t with
  | T.Lens l -> l
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
    | T.Application (_, (* args *) [ (_pk, r) ]) ->
        r (* get the first argument of a type application *)
    | T.Table (_, read, _, _) -> read (* use the read type of a table *)
    | T.Record r -> r (* Use the row of a record type. *)
    | _ -> failwith "LensTypes does not type."
  in
  let rt = extract_record_type t |> lens_phrase_type_of_type in
  sort_cols_of_record rt
