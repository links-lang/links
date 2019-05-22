open Lens.Utility

let lens_db_of_db (db : Value.database) =
  let driver_name = db#driver_name in
  let escape_string s = "'" ^ db#escape_string s ^ "'" in
  let quote_field s = db#quote_field s in
  let execute query =
    db#exec query
    |> fun result ->
    match result#status with
    | `QueryOk -> ()
    | `QueryError msg -> failwith @@ "Error executing database command: " ^ msg
  in
  let execute_select query ~field_types =
    let field_types =
      List.map
        ~f:(fun (n, v) -> (n, Lens_type_conv.type_of_lens_phrase_type v))
        field_types
    in
    let result, rs = Database.execute_select_result field_types query db in
    Database.build_result (result, rs)
    |> Value.unbox_list
    |> List.map ~f:Lens_value_conv.lens_phrase_value_of_value
  in
  { Lens.Database.driver_name
  ; escape_string
  ; quote_field
  ; execute
  ; execute_select }

let lens_table_of_table (table : Value.table) =
  let _, table, keys, _ = table in
  let open Lens.Database.Table in
  {name= table; keys}
