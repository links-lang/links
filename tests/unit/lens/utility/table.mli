open OUnit2

val create_db : test_ctxt -> Lens.Database.t -> (module Database_S.S)

val get_db : test_ctxt -> Lens.Database.t

val create : test_ctxt -> (module Database_S.S)
