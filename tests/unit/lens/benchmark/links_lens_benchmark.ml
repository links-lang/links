open OUnit2
open Links_unit_lens_utility

module Lens = Links_lens
module Prim = Links_lens_unit_tests.Lens_primitives_tests

open Lens.Utility

let csv_name str =
  let dir = Filename.dirname Sys.argv.(0) in
  Format.sprintf "%s/%s.csv" dir str

let print_csv ~file data =
  let channel = open_out (csv_name file) in
  Printf.fprintf channel "n, iqtime, ittime, cqtime, cttime\n";
  List.iter
    ~f:(fun (a, b, c, d, e) ->
      Printf.fprintf channel "%i, %f, %f, %f, %f\n" a b c d e)
    data;
  close_out channel

let test_put_benchmark_step ~behaviour ~db lens res =
  let step () =
    match behaviour with
    | Lens.Eval.Classic ->
        let cb _ _ = () in
        Lens.Eval.Classic.lens_put_step ~db lens res cb
    | Lens.Eval.Incremental ->
        let data = Lens.Eval.Incremental.lens_get_delta ~db lens res in
        let env = Int.Map.empty in
        let cb ~env:_ _ _ = env in
        Lens.Eval.Incremental.lens_put_set_step ~db ~env lens data cb |> ignore
  in
  List.init 20 (fun _i -> Timing.time step)

let avg_ints a b = float (a + b) /. 2.0

let skip_median l =
  let sorted = List.drop ~n:5 l |> List.sort Int.compare in
  let n = List.length sorted in
  if n mod 2 == 0 then
    avg_ints (List.nth sorted (n / 2)) (List.nth sorted ((n / 2) - 1))
  else List.nth sorted (n / 2) |> float

let benchmark_both ~n test_ctx benchmark =
  let timings = ref [] in
  let put ~db lens view =
    timings :=
      test_put_benchmark_step ~behaviour:Lens.Eval.Classic ~db lens view
  in
  benchmark ~n ~put test_ctx;
  let cqtime, cttime = List.split !timings in
  let put ~db lens view =
    timings :=
      test_put_benchmark_step ~behaviour:Lens.Eval.Incremental ~db lens view
  in
  benchmark ~n ~put test_ctx;
  let iqtime, ittime = List.split !timings in
  ( n,
    skip_median iqtime,
    skip_median ittime,
    skip_median cqtime,
    skip_median cttime )

let benchmark_select test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_select_lens_2)
      rowcounts
  in
  print_csv ~file:"select_benchmark" results

let benchmark_join test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_join_lens_1)
      rowcounts
  in
  print_csv ~file:"join_benchmark" results

let benchmark_drop test_ctx =
  let rowcounts =
    [ 500; 1000; 5000; 10000; 25000; 50000; 100000; 150000; 200000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_drop_lens_1)
      rowcounts
  in
  print_csv ~file:"drop_benchmark" results

let benchmark_select_delta_size test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_select_lens_2)
      rowcounts
  in
  print_csv ~file:"select_delta_size_benchmark" results

let suites =
  "benchmark"
  >::: [
         "select" >:: benchmark_select;
         "join" >:: benchmark_join;
         "drop" >:: benchmark_drop;
         "select_delta_size" >:: benchmark_select_delta_size;
       ]

let () = run_test_tt_main suites

let () = Format.printf "%s" (Filename.dirname Sys.argv.(0))
