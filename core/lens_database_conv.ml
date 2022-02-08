open Lens.Utility

let lens_db_of_db cstr (db : Value.database) =
  let driver_name = db#driver_name in
  let serialize () = cstr in
  let escape_string s = "'" ^ db#escape_string s ^ "'" in
  let quote_field s = db#sql_printer#quote_field s in
  let execute query =
    db#exec query |> fun result ->
    match result#status with
    | `QueryOk -> ()
    | `QueryError msg -> failwith @@ "Error executing database command: " ^ msg
  in
  let execute_select query ~field_types =
    let field_types_links =
      List.map
        ~f:(fun (n, v) ->
          let context = Env.String.empty in
          let typ =
            match v with
            | Lens.Phrase.Type.Serial -> Types.int_type
            | _ -> Lens_type_conv.type_of_lens_phrase_type ~context v
          in
          (n, typ))
        field_types
    in
    let result, rs =
      Database.execute_select_result field_types_links query db
    in
    let scolumns =
      field_types
      |> List.filter (fun (_, t) -> t = Lens.Phrase.Type.Serial)
      |> List.map ~f:(fun (n, _) -> n)
      |> Lens.Alias.Set.of_list
    in
    Database.build_result (result, rs)
    |> Value.unbox_list
    (* convert all key values from type int into type serial *)
    |> List.map ~f:(fun row ->
           let vs =
             List.map
               ~f:(fun (k, v) ->
                 let v =
                   if Lens.Alias.Set.mem k scolumns then
                     Value.box_variant "Key" v
                   else v
                 in
                 (k, v))
               (Value.unbox_record row)
           in
           Value.box_record vs)
    |> List.map ~f:Lens_value_conv.lens_phrase_value_of_value
  in
  {
    Lens.Database.driver_name;
    escape_string;
    quote_field;
    execute;
    execute_select;
    serialize;
  }

let lens_table_of_table (table : Value.Table.t) =
  let open Value.Table in
  if table.temporality <> CommonTypes.Temporality.current then
    raise (Errors.runtime_error "Cannot use lenses with temporal tables.")
  else
    let { name = tbl_name; keys; _ } = table in
    let open Lens.Database.Table in
    { name = tbl_name; keys }
