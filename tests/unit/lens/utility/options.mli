open OUnit2

type 'a t = test_ctxt -> 'a

val display_table_query_opt : bool t

val leave_tables_opt : bool t

val database_args_opt : string t

val verbose_opt : bool t

val classic_opt : bool t

val benchmark_opt : bool t

val set_n_opt : int t

val set_upto_opt : int t
