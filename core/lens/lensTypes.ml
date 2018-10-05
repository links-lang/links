open Types
open Utility
open LensUtility
open LensQueryHelpers
open LensRecordHelpers

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
let record_fields (rt : Types.typ) =
    match rt with
    | `Record (fields, _row_var, _dual) -> fields
    | _ -> failwith "Expected a record type." (* TODO: display incorrectly expected type **)

let sort_cols_of_record (tableName : string) (typ : Types.typ) =
    let fields = record_fields typ in
    let cols = StringMap.to_list (fun k v -> {table = tableName; name = k; alias = k; typ = unpack_field_spec v; present = true;}) fields in
    cols

let cols_of_record (typ: Types.typ) =
    let fields = record_fields typ in
    StringMap.to_list (fun k _v -> k) fields

(** Generate a sort for a table with the given type **)
let sort_cols_of_table (tableName : string) (t : Types.typ) =
    let rt = extract_record_type t in
    sort_cols_of_record tableName rt

let var_name (var, _pos : Sugartypes.phrase) =
    match var with
    | `Var name -> name
    | _ -> failwith "Expected a `Var type"

let cols_of_phrase (key, _pos : Sugartypes.phrase) : string list =
    match key with
    | `TupleLit keys -> List.map var_name keys
    | `Var name -> [name]
    | _ -> failwith "Expected a tuple or a variable."

let select_lens_sort (sort : Types.lens_sort) (pred : lens_phrase) : Types.lens_sort =
    let oldPred = LensSort.predicate sort in
    let pred = Phrase.combine_and oldPred (Some pred) in
    (LensSort.fundeps sort, pred, LensSort.cols sort)

let drop_lens_sort (sort : Types.lens_sort) (drop : ColSet.t) (key : ColSet.t) =
    (* Verify that the functional dependencies contain X \to A *)
    if ColSet.subset drop (FunDepSet.transitive_closure key (LensSort.fundeps sort)) |> not then
        failwith "The dropped columns must be defined by the key";
    let fds = FunDepSet.remove_defines (LensSort.fundeps sort) drop in
    let domain = List.map (fun c -> if ColSet.mem (LensCol.alias c) drop then LensCol.hide c else c) (LensSort.cols sort) in
    LensSort.make fds (LensSort.predicate sort) domain
