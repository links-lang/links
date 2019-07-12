open OUnit2
open Links_core
open UnitTestsLensCommon

module Phrase = Lens.Phrase
open Lens.Phrase.Value
module Record = Lens.Phrase.Value.Record

(* let _ = Settings.set_value Debug.debugging_enabled true  *)

let performance_opt =
  Conf.make_bool "performance" false "Run performance tests."

let skip_long_perf_opt =
  Conf.make_bool "skip_long_perf" true "Skip long running performance tests."

let skip_performance test_ctx =
  let v = performance_opt test_ctx in
  skip_if (not v) "Not running performance tests"

let skip_long_performance test_ctx =
  let v = skip_long_perf_opt test_ctx in
  skip_if v "Not running long performance tests because of -skip-long-perf"

let test_join_five test_ctx n =
  skip_performance test_ctx ;
  skip_long_performance test_ctx ;
  let db = LensTestHelpers.get_db test_ctx in
  let l1 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c"
      "a b c"
      [`Seq; `RandTo (n / 15); `Rand]
      n
  in
  let l2 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d e"
      "b d e"
      [`Seq; `RandTo (n / 15 / 15); `Rand]
      (n / 15)
  in
  let l3 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t3" "d -> f g"
      "d f g"
      [`Seq; `RandTo (n / 15 / 15 / 15); `Rand]
      (n / 15 / 15)
  in
  let l4 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t4" "f -> h" "f h"
      [`Seq; `Rand]
      (n / 15 / 15 / 15)
  in
  let l5 = LensTestHelpers.join_lens_dl l1 l2 [("b", "b", "b")] in
  let l6 = LensTestHelpers.join_lens_dl l3 l4 [("f", "f", "f")] in
  let l7 = LensTestHelpers.join_lens_dl l5 l6 [("d", "d", "d")] in
  let l8 =
    LensTestHelpers.select_lens l7
      (Phrase.equal (Phrase.var "b") (Phrase.Constant.int 10))
  in
  (* run tests *)
  let res = Lens.Value.lens_get l8 in
  (* let r = LensTestHelpers.time_query_both (fun () -> lens_put l8 res None) in *)
  let _ =
    LensTestHelpers.print_verbose test_ctx (Lens.Phrase.Value.show_values res)
  in
  (* modify res *)
  let row = List.hd res in
  let row = Record.set row ~key:"a" ~value:(box_int (n + 1)) in
  let res = row :: res in
  (* et r = LensTestHelpers.time_query_both (fun () -> lens_put l8 res None) in *)
  let _ = LensTestHelpers.print_verbose test_ctx (show_values res) in
  (* cleanup *)
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t1" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t2" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t3" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t4" in
  ()

let create_join_n_lens test_ctx db n rows =
  let r = LensTestHelpers.range 1 n in
  let ls =
    List.map
      (fun i ->
        let stri = string_of_int i in
        let name = "t_" ^ stri in
        let fds =
          "p_" ^ stri ^ " -> p_" ^ string_of_int (i + 1) ^ " c_" ^ stri ^ "_2"
        in
        let cols =
          "p_" ^ stri ^ " p_" ^ string_of_int (i + 1) ^ " c_" ^ stri ^ "_2"
        in
        let colsA = [`Seq; `RandTo (rows / 15); `Rand] in
        LensTestHelpers.drop_create_populate_table test_ctx db name fds cols
          colsA rows)
      r
  in
  let _, l =
    List.fold_left
      (fun (i, l) r ->
        let col = "p_" ^ string_of_int i in
        (i + 1, LensTestHelpers.join_lens_dl l r [(col, col, col)]))
      (2, List.hd ls)
      (List.tl ls)
  in
  let l =
    LensTestHelpers.select_lens l
      (Phrase.equal (Phrase.var "p_2") (Phrase.Constant.int 10))
  in
  l

let cleanup_join_n_lens test_ctx db n _rows =
  let r = LensTestHelpers.range 1 n in
  let _ =
    List.map
      (fun i ->
        let name = "t_" ^ string_of_int i in
        LensTestHelpers.drop_if_cleanup test_ctx db name)
      r
  in
  ()

let benchmark_nr_of_lenses_remove test_ctx =
  skip_performance test_ctx ;
  let r = LensTestHelpers.range 1 10 in
  let db = LensTestHelpers.get_db test_ctx in
  let n = 2000 in
  let _ =
    List.map
      (fun i ->
        let l = create_join_n_lens test_ctx db i n in
        let res = Lens.Value.lens_get l in
        let _put = List.tl res in
        (* let r = LensTestHelpers.time_query false (fun () -> lens_put l put None) in
         let _ = LensTestHelpers.print_verbose test_ctx (string_of_value r) in *)
        cleanup_join_n_lens test_ctx db i)
      r
  in
  ()

let benchmark_nr_of_lenses_add test_ctx =
  skip_performance test_ctx ;
  let r = LensTestHelpers.range 1 10 in
  let db = LensTestHelpers.get_db test_ctx in
  let n = 2000 in
  let _ =
    List.map
      (fun i ->
        let l = create_join_n_lens test_ctx db i n in
        let res = Lens.Value.lens_get l in
        let r = List.hd res in
        let r = Record.set r ~key:"p_1" ~value:(box_int (n + 1)) in
        let _put = r :: List.tl res in
        (* let r = LensTestHelpers.time_query false (fun () -> lens_put l put None) in
         let _ = LensTestHelpers.print_verbose test_ctx (string_of_value r) in *)
        cleanup_join_n_lens test_ctx db i)
      r
  in
  ()

let create_join_five_lens test_ctx db n =
  let l1 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c"
      "a b c"
      [`Seq; `RandTo (n / 15); `Rand]
      n
  in
  let l2 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d e"
      "b d e"
      [`Seq; `RandTo (n / 15 / 15); `Rand]
      (n / 15)
  in
  let l3 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t3" "d -> f g"
      "d f g"
      [`Seq; `RandTo (n / 15 / 15 / 15); `Rand]
      (n / 15 / 15)
  in
  let l4 =
    LensTestHelpers.drop_create_populate_table test_ctx db "t4" "f -> h" "f h"
      [`Seq; `Rand]
      (n / 15 / 15 / 15)
  in
  let l5 = LensTestHelpers.join_lens_dl l1 l2 [("b", "b", "b")] in
  let l6 = LensTestHelpers.join_lens_dl l3 l4 [("f", "f", "f")] in
  let l7 = LensTestHelpers.join_lens_dl l5 l6 [("d", "d", "d")] in
  let l8 =
    LensTestHelpers.select_lens l7
      (Phrase.equal (Phrase.var "b") (Phrase.Constant.int 10))
  in
  l8

let cleanup_join_five_lens test_ctx db =
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t1" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t2" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t3" in
  let _ = LensTestHelpers.drop_if_cleanup test_ctx db "t4" in
  ()

let test_join_five_remove test_ctx n =
  skip_performance test_ctx ;
  let db = LensTestHelpers.get_db test_ctx in
  let l8 = create_join_five_lens test_ctx db n in
  (* modify res *)
  let res = Lens.Value.lens_get l8 in
  let rm =
    List.filter
      (fun r ->
        1
        = List.length
            (List.filter (fun r' -> Record.match_on r r' ~on:["c"]) res))
      res
  in
  (* let _ = print_endline (string_of_value (box_list rm)) in *)
  let rm = List.hd rm in
  let _res = List.filter (fun r -> not (Record.match_on r rm ~on:["a"])) res in
  (* let r = LensTestHelpers.time_query false (fun () -> lens_put l8 res None) in
     let _ = LensTestHelpers.print_verbose test_ctx (string_of_value r) in *)
  (* cleanup *)
  let _ = cleanup_join_five_lens test_ctx db in
  ()

let test_join_five_update test_ctx n =
  skip_performance test_ctx ;
  let db = LensTestHelpers.get_db test_ctx in
  let l8 = create_join_five_lens test_ctx db n in
  (* modify res *)
  let res = Lens.Value.lens_get l8 in
  let _res =
    Record.set (List.hd res) ~key:"h" ~value:(box_int 1) :: List.tl res
  in
  (* let r = LensTestHelpers.time_query false (fun () -> lens_put l8 res None) in
     let _ = LensTestHelpers.print_verbose test_ctx (string_of_value r) in *)
  (* cleanup *)
  let _ = cleanup_join_five_lens test_ctx db in
  ()

let test_join_five_10000 test_ctx = test_join_five test_ctx 10000

let test_join_five_remove_10000 test_ctx = test_join_five_remove test_ctx 10000

let test_join_five_update_10000 test_ctx = test_join_five_update test_ctx 10000

let suite =
  "lens_performance"
  >::: [ "join_five_update_10000" >:: test_join_five_update_10000
       ; "join_five_remove_10000" >:: test_join_five_remove_10000
       ; "join_five_10000" >:: test_join_five_10000
       ; "benchmark_nr_of_lenses_add" >:: benchmark_nr_of_lenses_add
       ; "benchmark_nr_of_lenses_remove" >:: benchmark_nr_of_lenses_remove ]
