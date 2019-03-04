open Types
open Utility
open Lens_types
open SourceCode
open Sugartypes

module Phrase = Lens_phrase
module PT = Lens_phrase_type

let unpack_field_spec (typ : Types.field_spec) =
    match typ with
    | `Present t -> t
    | _ -> failwith "Expected `Present"

let extract_record_type (t : Types.typ) =
    match t with
    | `Record _ as r -> r
    | `Application (_, [`Type (`Record _ as r)]) -> r
    | `Table (r, _, _) -> r
    | _ -> failwith ("LensTypes does not type.") (* TODO: display incorrectly expected type **)

(** Returns the columns of a `Record type. **)
let record_fields (rt : Lens_phrase_type.t) =
    match rt with
    | PT.Record fields -> fields
    | _ -> failwith "Expected a record type." (* TODO: display incorrectly expected type **)

let sort_cols_of_record (table : string) (typ : Lens_phrase_type.t) =
    let fields = record_fields typ in
    let cols = StringMap.to_list (fun name typ ->
        let alias = name in
        Lens_column.make ~table ~name ~alias ~typ ~present:true) fields in
    cols

let cols_of_record (typ: Lens_phrase_type.t) =
    let fields = record_fields typ in
    StringMap.to_list (fun k _v -> k) fields

(** Generate a sort for a table with the given type **)
let sort_cols_of_table (tableName : string) (t : Types.typ) =
    let rt = extract_record_type t |> Lens_type_conv.lens_phrase_type_of_type in
    sort_cols_of_record tableName rt

let var_name (var : phrase) =
    match WithPos.node var with
    | Var name -> name
    | _ -> failwith "Expected a `Var type"

let cols_of_phrase (key : phrase) : string list =
    match WithPos.node key with
    | TupleLit keys -> List.map var_name keys
    | Var name -> [name]
    | _ -> failwith "Expected a tuple or a variable."

let select_lens_sort (sort : Lens_sort.t) (pred : Phrase.t) : Lens_sort.t =
    let oldPred = Lens_sort.predicate sort in
    let predicate = Phrase.Option.combine_and oldPred (Some pred) in
    Lens_sort.update_predicate sort ~predicate

let drop_lens_sort (sort : Lens_sort.t) (drop : Alias.Set.t) (cols : Alias.Set.t) =
    (* Verify that the functional dependencies contain X \to A *)
    if Alias.Set.subset drop (Fun_dep.Set.transitive_closure ~cols (Lens_sort.fds sort)) |> not then
        failwith "The dropped columns must be defined by the key";
    let fds = Fun_dep.Set.remove_defines (Sort.fds sort) ~cols:drop in
    let cols = List.map (fun c -> if Alias.Set.mem (Lens_column.alias c) drop then Lens_column.hide c else c) (Lens_sort.cols sort) in
    let predicate = Lens_sort.predicate sort in
    Lens_sort.make ~fds ~predicate cols
