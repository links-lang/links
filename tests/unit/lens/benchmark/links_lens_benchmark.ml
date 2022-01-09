open OUnit2
open Links_unit_lens_utility

module Lens = Links_lens
module Prim = Links_lens_unit_tests.Lens_primitives_tests

open Lens.Utility

let () = Links_core.Settings.set Links_core.Debug.enabled false

let csv_name str =
  let dir = Filename.dirname Sys.argv.(0) in
  Format.sprintf "%s/%s.csv" dir str

let print_csv_4 ~file data =
  let channel = open_out (csv_name file) in
  Printf.fprintf channel "n, iqtime, ittime, cqtime, cttime\n";
  List.iter
    ~f:(fun (a, b, c, d, e) ->
      Printf.fprintf channel "%i, %f, %f, %f, %f\n" a b c d e)
    data;
  close_out channel

let print_csv_2 ~file data =
  let channel = open_out (csv_name file) in
  Printf.fprintf channel "n, qtime, ttime\n";
  List.iter
    ~f:(fun (a, b, c) -> Printf.fprintf channel "%i, %f, %f\n" a b c)
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
    [ 500; 1000; 5000; 10000; 25000; 50000; 100000; 150000; 200000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_select_lens_2)
      rowcounts
  in
  print_csv_4 ~file:"select_benchmark" results

let benchmark_join test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_join_lens_1)
      rowcounts
  in
  print_csv_4 ~file:"join_benchmark" results

let benchmark_drop test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_drop_lens_1)
      rowcounts
  in
  print_csv_4 ~file:"drop_benchmark" results

let benchmark_select_delta_size test_ctx =
  let rowcounts = [ 10; 100; 200; 300; 400; 500; 600; 700; 800; 900; 1000 ] in
  let results =
    List.map
      ~f:(fun n ->
        benchmark_both ~n test_ctx (fun ~n ->
            Prim.template_select_lens_3 ~n:100000 ~upto:n))
      rowcounts
  in
  print_csv_4 ~file:"select_delta_size_benchmark" results

let test_get_delta_step ~db lens res =
  let step () = Lens.Eval.Incremental.lens_get_delta ~db lens res |> ignore in
  List.init 20 (fun _i -> Timing.time step)

let benchmark_get_delta test_ctx =
  let rowcounts =
    [
      100;
      20000;
      40000;
      60000;
      80000;
      100000;
      120000;
      140000;
      160000;
      180000;
      200000;
    ]
  in
  let results =
    List.map
      ~f:(fun n ->
        let timings = ref [] in
        let get_delta ~db lens view =
          timings := test_get_delta_step ~db lens view
        in
        Prim.template_get_delta test_ctx ~get_delta;
        let qtime, ttime = List.split !timings in
        (n, skip_median qtime, skip_median ttime))
      rowcounts
  in
  print_csv_2 ~file:"get_delta_benchmark" results

let put_delta_behaviour ~behaviour ~db l res =
  let table = Prim.prim_lens_table l in
  let delta = Lens.Eval.Incremental.lens_get_delta ~db l res in
  let cols = Lens.Value.cols_present_aliases l in
  let data = Lens.Sorted_records.construct_cols ~columns:cols ~records:res in
  let neg = Lens.Sorted_records.negate delta in
  let sort = Lens.Value.sort l in
  let env = Int.Map.empty in
  let step =
    match behaviour with
    | Lens.Eval.Incremental ->
        fun () ->
          Lens.Eval.Incremental.apply_delta ~db ~table ~sort ~env delta
          |> ignore
    | Lens.Eval.Classic ->
        fun () -> Lens.Eval.Classic.apply_table_data ~table ~db data
  in
  let revert () =
    Lens.Eval.Incremental.apply_delta ~db ~table ~sort ~env neg |> ignore
  in
  List.init 20 (fun _ ->
      let res = Timing.time step in
      revert ();
      res)

let benchmark_put_delta test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n ->
        let timings = ref [] in
        Prim.template_put_delta ~n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Incremental ~db l res)
          test_ctx;
        let iqtime, ittime = List.split !timings in
        Prim.template_put_delta ~n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Classic ~db l res)
          test_ctx;
        let cqtime, cttime = List.split !timings in
        ( n,
          skip_median iqtime,
          skip_median ittime,
          skip_median cqtime,
          skip_median cttime ))
      rowcounts
  in
  print_csv_4 ~file:"put_delta_benchmark" results

let benchmark_put_delta test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n ->
        let timings = ref [] in
        Prim.template_put_delta ~n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Incremental ~db l res)
          test_ctx;
        let iqtime, ittime = List.split !timings in
        Prim.template_put_delta ~n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Classic ~db l res)
          test_ctx;
        let cqtime, cttime = List.split !timings in
        ( n,
          skip_median iqtime,
          skip_median ittime,
          skip_median cqtime,
          skip_median cttime ))
      rowcounts
  in
  print_csv_4 ~file:"put_delta_benchmark" results

let suites =
  "benchmark"
  >::: [
         "select_delta_size" >:: benchmark_select_delta_size;
         "get_delta" >:: benchmark_select_delta_size;
         "put_delta" >:: benchmark_put_delta;
         "select" >:: benchmark_select;
         "join" >:: benchmark_join;
         "drop" >:: benchmark_drop;
       ]

let () = run_test_tt_main suites
