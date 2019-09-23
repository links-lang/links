let connection_info
  = Settings.(option "database_args"
              |> synopsis "Database host, name, user, and password"
              |> to_string from_string_option
              |> convert Utility.some
              |> sync)

let shredding =
  Settings.(flag "shredding"
            |> synopsis "Enables database query shredding"
            |> convert parse_bool
            |> sync)


let relax_query_type_constraint =
  Settings.(flag "relax_query_type_constraint"
            |> convert parse_bool
            |> sync)
