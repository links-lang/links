open OUnit2
module Settings = Links_core.Settings

type 'a t = test_ctxt -> 'a

let display_table_query_opt =
  Conf.make_bool "display_table_query" false
    "Show queries to take and manipulate tables."

let leave_tables_opt =
  Conf.make_bool "leave_tables" false "Do not delete tables after run."

let database_args_opt =
  let module Database = Links_core.Database in
  let connection_args = Settings.get Database.connection_info in
  Conf.make_string "database_args"
    ("links:" ^ Links_core.Utility.from_option "" connection_args)
    "Database connection args."

let verbose_opt = Conf.make_bool "V" false "Print verbose information."

let classic_opt =
  Conf.make_bool "classic_lenses" false "Use non incremental relational lenses."

let benchmark_opt = Conf.make_bool "benchmark" false "Benchmark operations."

let set_n_opt = Conf.make_int "set_n" 0 "Override n."

let set_upto_opt = Conf.make_int "set_upto" 10 "Override upto."
