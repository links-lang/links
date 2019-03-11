open Lens_utility

module Sort = Lens_sort
module Phrase = Lens_phrase
module Fun_dep = Lens_fun_dep

type t =
  | Lens of {table: Lens_database.Table.t; database: Lens_database.t; sort: Lens_sort.t}
  | LensMem of {records: Lens_phrase_value.t list; sort: Lens_sort.t}
  | LensSelect of {lens: t; predicate: Lens_phrase.t; sort: Lens_sort.t}
  | LensJoin of
      { left: t
      ; right: t
      ; on: (string * string * string) list
      ; del_left: Lens_phrase.t
      ; del_right: Lens_phrase.t
      ; sort: Lens_sort.t }
  | LensDrop of
      { lens: t
      ; drop: string
      ; key: string
      ; default: Lens_phrase_value.t
      ; sort: Lens_sort.t }

let show v =
  match v with
  | Lens _ -> "Lens"
  | LensMem _ -> "LensMem"
  | LensSelect _ -> "LensSelect"
  | LensJoin _ -> "LensJoin"
  | LensDrop _ -> "LensDrop"

let pp f v =
  Format.fprintf f "%s" @@ show v

let string_of_value v = show v

let sort v =
  match v with
  | Lens {sort; _} -> sort
  | LensMem { sort; _} -> sort
  | LensDrop { sort; _ } -> sort
  | LensSelect { sort; _ } -> sort
  | LensJoin {sort; _} -> sort

let rec is_memory_lens lens =
  match lens with
  | Lens _ -> false
  | LensMem _ -> true
  | LensDrop { lens; _ } -> is_memory_lens lens
  | LensSelect { lens; _ } -> is_memory_lens lens
  | LensJoin { left; right; _ } ->
      is_memory_lens left || is_memory_lens right

let columns lens =
  let sort = sort lens in
  let cols = Lens_sort.cols sort in
  cols

let cols_present_aliases (lens : t) =
  let cols = columns lens in
  Column.List.present_aliases cols

let colset (lens : t) =
  let sort = sort lens in
  Lens_sort.colset sort

let fundeps (lens : t) =
  let sort = sort lens in
  Lens_sort.fds sort

let predicate (lens : t) =
  let sort = sort lens in
  Lens_sort.predicate sort

let rec get_primary_key lens =
  match lens with
  | Lens { sort; _ } ->
      let fds = Lens_sort.fds sort in
      let fd = Lens_fun_dep.Set.min_elt fds in
      let left = Lens_fun_dep.left fd in
      left
  | LensMem { sort; _ } ->
      let fds = Lens_sort.fds sort in
      let fd = Lens_fun_dep.Set.min_elt fds in
      let left = Lens_fun_dep.left fd in
      left
  | LensDrop { lens; _ } -> get_primary_key lens
  | LensSelect { lens; _ } -> get_primary_key lens
  | LensJoin { left; _ } ->
      (* right table has to be defined by left table *) get_primary_key left

let rec database lens =
  match lens with
  | Lens { database; _ } -> database
  | LensMem _ -> failwith "Memory lens does not have a database."
  | LensDrop { lens; _ } -> database lens
  | LensSelect { lens; _ } -> database lens
  | LensJoin { left; _ } -> database left

let rec compute_memory_sorted lens =
  match lens with
  | Lens _ -> failwith "Non memory lenses not implemented."
  | LensMem { records; _ }  -> Lens_sorted_records.construct ~records
  | LensDrop { lens; drop; _ } ->
      let records = compute_memory_sorted lens in
      let columns =
        List.filter (fun c -> c <> drop) @@ Lens_sorted_records.columns records
      in
      Lens_sorted_records.project_onto records ~columns
  | LensSelect {lens; predicate; _ } ->
      let records = compute_memory_sorted lens in
      Lens_sorted_records.filter records ~predicate
  | LensJoin { left; right; on; _ } ->
      let records1 = compute_memory_sorted left in
      let records2 = compute_memory_sorted right in
      Lens_sorted_records.join records1 records2 ~on

let get_memory lens =
  compute_memory_sorted lens |> Lens_sorted_records.to_value

let rec generate_query lens =
  let open Lens_database.Select in
  match lens with
  | Lens { database = db; table; sort; } ->
      let cols = Lens_sort.cols sort in
      let open Lens_database.Table in
      let table = table.name in
      {tables= [(table, table)]; cols; predicate= None; db }
  | LensSelect { lens; predicate; _ } ->
      let query = generate_query lens in
      {query with predicate= Some predicate}
  (* get_lens_sort_row_type sort *)
  | LensDrop { lens; drop; _ } ->
      let query = generate_query lens in
      let cols = List.filter (fun c -> Column.alias c != drop) query.cols in
      { query with cols }
  | LensJoin { left; right; sort; _ } ->
      let q1 = generate_query left in
      let q2 = generate_query right in
      (* all table names must be unique, rename them *)
      let _tables2 =
        List.map
          ~f:(fun (n2, al2) ->
              match List.find ~f:(fun (n1, _al1) -> n1 = n2) q1.tables with
              | Some _ -> failwith "Cannot reuse a table twice in a join query!"
              | None -> (n2, al2) )
          q2.tables
      in
      let tables = List.append q1.tables q2.tables in
      let cols = Lens_sort.cols sort in
      if q1.db <> q2.db then
        failwith "Only single database expressions supported."
      else {tables; cols; predicate= Lens_sort.predicate sort; db= q1.db}
  | LensMem _ -> failwith "Memory lens unsupported for database query."

let get_query lens =
  let _ = Debug.print "getting tables" in
  let sort = sort lens in
  let database = database lens in
  let cols = Lens_sort.cols sort |> Column.List.present in
  let query = Lens_database.Select.of_sort database ~sort in
  let sql = Format.asprintf "%a" Lens_database.Select.fmt query in
  let field_types =
    List.map ~f:(fun c -> (Column.alias c, Column.typ c)) cols
  in
  let _ = Debug.print sql in
  let res =
    Lens_statistics.time_query (fun () ->
        Lens_database.Select.execute query ~field_types ~database )
  in
  res

let lens_get lens =
  if is_memory_lens lens then get_memory lens else get_query lens

let lens_select lens ~predicate =
  let sort = sort lens in
  let sort = Lens_sort.select_lens_sort sort ~predicate in
  LensSelect { lens; predicate; sort }

let lens_get_select lens ~predicate = lens_get (lens_select lens ~predicate)

let lens_get_select_opt lens ~predicate =
  match predicate with
  | None -> lens_get lens
  | Some predicate -> lens_get_select lens ~predicate

let query_exists lens predicate =
  let sort = sort lens in
  let sort = Lens_sort.select_lens_sort sort ~predicate in
  if is_memory_lens lens then
    let res = lens_get (LensSelect { lens; predicate; sort}) in
    res <> []
  else
    let database = database lens in
    let query = Lens_database.Select.of_sort database ~sort in
    let res =
      Lens_statistics.time_query (fun () ->
          Lens_database.Select.query_exists query ~database )
    in
    res
