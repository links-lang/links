open OUnit2
open Links_unit_lens_utility

module Lens = Links_lens
module Prim = Links_lens_unit_tests.Lens_primitives_tests

open Lens.Utility

let () = Links_core.Settings.set Links_core.Debug.enabled false

let test_put_benchmark_step ~behaviour ~db lens res =
  let delta = Lens.Eval.Incremental.lens_get_delta ~db lens res in
  let step () =
    match behaviour with
    | Lens.Eval.Classic ->
        let cb _ _ = () in
        Lens.Eval.Classic.lens_put_step ~db lens res cb
    | Lens.Eval.Incremental ->
        let env = Int.Map.empty in
        let cb ~env:_ _ _ = env in
        Lens.Eval.Incremental.lens_put_set_step ~db ~env lens delta cb |> ignore
  in
  List.init 20 (fun _i -> Timing.time step)

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
    Tools.skip_median iqtime,
    Tools.skip_median ittime,
    Tools.skip_median cqtime,
    Tools.skip_median cttime )

let rowcounts_large =
  [ 500; 1000; 5000; 10000; 25000; 50000; 100000; 150000; 200000 ]
let rowcounts_small =
  [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]

let benchmark_select test_ctx =
  let rowcounts = rowcounts_large in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_select_lens_2)
      rowcounts
  in
  Tools.print_csv_4 ~file:"select_benchmark" results

let benchmark_join_template variant mk_join test_ctx =
  let rowcounts = rowcounts_large in
  let results =
    List.map
      ~f:(fun n ->
        benchmark_both ~n test_ctx (Prim.template_join_lens_1 ~mk_join))
      rowcounts
  in
  Tools.print_csv_4 ~file:(Format.sprintf "join_%s_benchmark" variant) results

let benchmark_join_dl test_ctx =
  benchmark_join_template "dl" Mk_lens.join_dl test_ctx

let benchmark_join_dr test_ctx =
  benchmark_join_template "dr" Mk_lens.join_dr test_ctx

let benchmark_join_db test_ctx =
  benchmark_join_template "db" Mk_lens.join_db test_ctx

let benchmark_drop test_ctx =
  let rowcounts = rowcounts_large in
  let results =
    List.map
      ~f:(fun n -> benchmark_both ~n test_ctx Prim.template_drop_lens_1)
      rowcounts
  in
  Tools.print_csv_4 ~file:"drop_benchmark" results

let benchmark_select_delta_size test_ctx =
  let rowcounts =
    [ 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000 ]
  in
  let results =
    List.map
      ~f:(fun n ->
        benchmark_both ~n test_ctx (fun ~n ->
            Prim.template_select_lens_3 ~n:50000 ~upto:n))
      (List.map ~f:(fun n -> n / 10) rowcounts)
  in
  Tools.print_csv_4 ~file:"select_delta_size_benchmark" results

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
        Prim.template_get_delta test_ctx ~n ~get_delta;
        let qtime, ttime = List.split !timings in
        (n, Tools.skip_median qtime, Tools.skip_median ttime))
      rowcounts
  in
  Tools.print_csv_2 ~file:"get_delta_benchmark" results

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
        Prim.template_put_delta ~n:10000 ~upto:n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Incremental ~db l res)
          test_ctx;
        let iqtime, ittime = List.split !timings in
        Prim.template_put_delta ~n:10000 ~upto:n
          ~put:(fun ~db l res ->
            timings :=
              put_delta_behaviour ~behaviour:Lens.Eval.Classic ~db l res)
          test_ctx;
        let cqtime, cttime = List.split !timings in
        ( n,
          Tools.skip_median iqtime,
          Tools.skip_median ittime,
          Tools.skip_median cqtime,
          Tools.skip_median cttime ))
      rowcounts
  in
  Tools.print_csv_4 ~file:"put_delta_benchmark" results

let benchmark_lens_for_table ~n test_ctx name benchmark =
  let timings = ref [] in
  let qcount = ref 0 in
  let put ~db lens view =
    timings :=
      test_put_benchmark_step ~behaviour:Lens.Eval.Incremental ~db lens view;
    qcount := Lens.Statistics.get_query_count ()
  in
  benchmark ~n ~put test_ctx;
  let qtime, ttime = List.split !timings in
  (name, !qcount, Tools.skip_median qtime, Tools.skip_median ttime)

let benchmark_table test_ctx =
  let n = 200000 in
  let results =
    [
      benchmark_lens_for_table ~n test_ctx "select" Prim.template_select_lens_2;
      benchmark_lens_for_table ~n test_ctx "drop" Prim.template_drop_lens_1;
      benchmark_lens_for_table ~n test_ctx "join delete left"
        (Prim.template_join_lens_1 ~mk_join:Mk_lens.join_dl);
      benchmark_lens_for_table ~n test_ctx "join delete both"
        (Prim.template_join_lens_1 ~mk_join:Mk_lens.join_db);
      benchmark_lens_for_table ~n test_ctx "join delete right"
        (Prim.template_join_lens_1 ~mk_join:Mk_lens.join_dr);
    ]
  in
  let pp_and_list = Format.pp_print_list ~pp_sep:(Format.pp_constant " & ") in
  let pp_math_string f s = Format.fprintf f "\\text{%s}" s in
  let pp_c f _ = Format.fprintf f "c" in
  let pp_math_int f i = Format.fprintf f "%i" i in
  let pp_math_float f fl =
    let i = int_of_float fl in
    if i < 1 then Format.fprintf f "< 1 ms" else Format.fprintf f "%d ms" i
  in
  let channel = open_out (Tools.tex_name "table") in
  let fch = Format.formatter_of_out_channel channel in
  Format.fprintf fch
    {|\begin{array}{c|%a}
    & %a \\
    \hline
    \text{query count} & %a \\
    \text{query } n = 200k & %a \\
    \text{total } n = 200k & %a
\end{array}|}
    (Format.pp_print_list ~pp_sep:(Format.pp_constant "") pp_c)
    results
    (Format.pp_map ~f:(fun (v, _, _, _) -> v) pp_math_string |> pp_and_list)
    results
    (Format.pp_map ~f:(fun (_, v, _, _) -> v) pp_math_int |> pp_and_list)
    results
    (Format.pp_map ~f:(fun (_, _, v, _) -> v) pp_math_float |> pp_and_list)
    results
    (Format.pp_map ~f:(fun (_, _, _, v) -> v) pp_math_float |> pp_and_list)
    results;
  close_out channel

let suites =
  "benchmark"
  >::: [
         "table" >:: benchmark_table;
         "select_delta_size" >:: benchmark_select_delta_size;
         "get_delta" >:: benchmark_get_delta;
         "put_delta" >:: benchmark_put_delta;
         "select" >:: benchmark_select;
         "join_dl" >:: benchmark_join_dl;
         "join_db" >:: benchmark_join_db;
         "join_dr" >:: benchmark_join_dr;
         "drop" >:: benchmark_drop;
         Dblp_example.suite;
       ]

let () = run_test_tt_main suites
