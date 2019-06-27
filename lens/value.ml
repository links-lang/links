open Lens_utility

type t =
  | Lens of {table: Database.Table.t; database: Database.t; sort: Sort.t}
  | LensMem of {records: Phrase_value.t list; sort: Sort.t}
  | LensSelect of {lens: t; predicate: Phrase.t; sort: Sort.t}
  | LensJoin of
      { left: t
      ; right: t
      ; on: (string * string * string) list
      ; del_left: Phrase.t
      ; del_right: Phrase.t
      ; sort: Sort.t }
  | LensDrop of
      { lens: t
      ; drop: string
      ; key: string
      ; default: Phrase_value.t
      ; sort: Sort.t }

let show v =
  match v with
  | Lens _ -> "Lens"
  | LensMem _ -> "LensMem"
  | LensSelect _ -> "LensSelect"
  | LensJoin _ -> "LensJoin"
  | LensDrop _ -> "LensDrop"

let rec pp f v =
  match v with
  | LensSelect {lens; predicate; _} ->
      Format.fprintf f "lensselect from (%a) by (%a)" pp lens
        Database.fmt_phrase_dummy predicate
  | Lens {table; _} -> Format.fprintf f "table %s" @@ Database.Table.name table
  | LensMem _ -> Format.fprintf f "memory table"
  | LensJoin _ -> Format.fprintf f "LensJoin"
  | LensDrop _ -> Format.fprintf f "lensdrop"

let string_of_value v = show v

let sort v =
  match v with
  | Lens {sort; _} -> sort
  | LensMem {sort; _} -> sort
  | LensDrop {sort; _} -> sort
  | LensSelect {sort; _} -> sort
  | LensJoin {sort; _} -> sort

let rec is_memory_lens lens =
  match lens with
  | Lens _ -> false
  | LensMem _ -> true
  | LensDrop {lens; _} -> is_memory_lens lens
  | LensSelect {lens; _} -> is_memory_lens lens
  | LensJoin {left; right; _} -> is_memory_lens left || is_memory_lens right

let columns lens =
  let sort = sort lens in
  let cols = Sort.cols sort in
  cols

let cols_present_aliases (lens : t) =
  let cols = columns lens in
  Column.List.present_aliases cols

let colset (lens : t) =
  let sort = sort lens in
  Sort.colset sort

let fundeps (lens : t) =
  let sort = sort lens in
  Sort.fds sort

let predicate (lens : t) =
  let sort = sort lens in
  Sort.predicate sort

let rec get_primary_key lens =
  match lens with
  | Lens {sort; _} ->
      let fds = Sort.fds sort in
      let fd = Fun_dep.Set.min_elt fds in
      let left = Fun_dep.left fd in
      left
  | LensMem {sort; _} ->
      let fds = Sort.fds sort in
      let fd = Fun_dep.Set.min_elt fds in
      let left = Fun_dep.left fd in
      left
  | LensDrop {lens; _} -> get_primary_key lens
  | LensSelect {lens; _} -> get_primary_key lens
  | LensJoin {left; _} ->
      (* right table has to be defined by left table *) get_primary_key left

let rec database lens =
  match lens with
  | Lens {database; _} -> database
  | LensMem _ -> failwith "Memory lens does not have a database."
  | LensDrop {lens; _} -> database lens
  | LensSelect {lens; _} -> database lens
  | LensJoin {left; _} -> database left

let rec compute_memory_sorted lens =
  match lens with
  | Lens _ -> failwith "Non memory lenses not implemented."
  | LensMem {records; _} -> Sorted_records.construct ~records
  | LensDrop {lens; drop; _} ->
      let records = compute_memory_sorted lens in
      let columns =
        List.filter (fun c -> c <> drop) @@ Sorted_records.columns records
      in
      Sorted_records.project_onto records ~columns
  | LensSelect {lens; predicate; _} ->
      let records = compute_memory_sorted lens in
      Sorted_records.filter records ~predicate
  | LensJoin {left; right; on; _} ->
      let records1 = compute_memory_sorted left in
      let records2 = compute_memory_sorted right in
      Sorted_records.join records1 records2 ~on

let get_memory lens = compute_memory_sorted lens |> Sorted_records.to_value

let rec generate_query lens =
  let open Database.Select in
  match lens with
  | Lens {database= db; table; sort} ->
      let cols = Sort.cols sort in
      let open Database.Table in
      let table = table.name in
      {tables= [(table, table)]; cols; predicate= None; db}
  | LensSelect {lens; predicate; _} ->
      let query = generate_query lens in
      {query with predicate= Some predicate}
  (* get_lens_sort_row_type sort *)
  | LensDrop {lens; drop; _} ->
      let query = generate_query lens in
      let cols = List.filter (fun c -> Column.alias c != drop) query.cols in
      {query with cols}
  | LensJoin {left; right; sort; _} ->
      let q1 = generate_query left in
      let q2 = generate_query right in
      (* all table names must be unique, rename them *)
      let _tables2 =
        List.map
          ~f:(fun (n2, al2) ->
            match List.find ~f:(fun (n1, _al1) -> n1 = n2) q1.tables with
            | Some _ -> failwith "Cannot reuse a table twice in a join query!"
            | None -> (n2, al2))
          q2.tables
      in
      let tables = List.append q1.tables q2.tables in
      let cols = Sort.cols sort in
      if q1.db <> q2.db then
        failwith "Only single database expressions supported."
      else {tables; cols; predicate= Sort.predicate sort; db= q1.db}
  | LensMem _ -> failwith "Memory lens unsupported for database query."

let get_query lens =
  let _ = Debug.print "getting tables" in
  let sort = sort lens in
  let database = database lens in
  let cols = Sort.cols sort |> Column.List.present in
  let query = Database.Select.of_sort database ~sort in
  let sql = Format.asprintf "%a" Database.Select.fmt query in
  let field_types =
    List.map ~f:(fun c -> (Column.alias c, Column.typ c)) cols
  in
  let _ = Debug.print sql in
  let res =
    Statistics.time_query (fun () ->
        Database.Select.execute query ~field_types ~database)
  in
  res

let lens_get lens =
  if is_memory_lens lens then get_memory lens else get_query lens

(* HACK: This constructs a select lens bypassing sort checks. This is fine for the
   forward direction, but would produce an invalid lens for the put direction. *)
let lens_select_internal lens ~predicate =
  let sort = sort lens in
  let sort =
    let query =
      Some predicate |> Phrase.Option.combine_and @@ Sort.query sort
    in
    let predicate =
      Some predicate |> Phrase.Option.combine_and @@ Sort.predicate sort
    in
    Sort.update_predicate ~query ~predicate sort
  in
  LensSelect {lens; predicate; sort}

let lens_get_select lens ~predicate =
  lens_get (lens_select_internal lens ~predicate)

let lens_get_select_opt lens ~predicate =
  match predicate with
  | None -> lens_get lens
  | Some predicate -> lens_get_select lens ~predicate

let query_exists lens predicate =
  let sort = sort lens in
  let sort = Sort.select_lens_sort sort ~predicate |> Result.ok_exn in
  if is_memory_lens lens then
    let res = lens_get (LensSelect {lens; predicate; sort}) in
    res <> []
  else
    let database = database lens in
    let query = Database.Select.of_sort database ~sort in
    let res =
      Statistics.time_query (fun () ->
          Database.Select.query_exists query ~database)
    in
    res
