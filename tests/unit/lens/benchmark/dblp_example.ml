open OUnit2
open Links_unit_lens_utility

open Lens.Utility
open Lens.Utility.O

let _ =
  let prelude_path = "./_build/default/bin/prelude.links" in
  if not (Sys.file_exists prelude_path) then
    Unix.link "./prelude.links" prelude_path

let setup_dblp_database ~n test_ctx =
  let module DB = (val Table.create test_ctx) in
  let setup_tables =
    Format.asprintf
      {|
  drop table if exists inproceedings_subset;
  create table inproceedings_subset AS
  (
    (
      select t1.inproceedings, t4.title, t4.year, t1.proceedings from inproceedings_crossref as t1
        join inproceedings_author as t2 on t1.inproceedings = t2.inproceedings
        join inproceedings as t4 on t1.inproceedings = t4.inproceedings
      where t4.year = 2016 and t1.proceedings != 'conf/pods/2016'
      Limit (%d-108)
    )
    UNION
    (
      select t1.inproceedings, t1.title, t1.year, t2.proceedings from inproceedings as t1
        join inproceedings_crossref as t2 on t1.inproceedings = t2.inproceedings
        where t2.proceedings = 'conf/pods/2006'
    )
  );
  alter table inproceedings_subset add primary key (inproceedings);
  drop table if exists inproceedings_author_subset;
  create table inproceedings_author_subset AS
  (
    select distinct t1.inproceedings, t1.author from inproceedings_author as t1
      join inproceedings_subset as t2 on t1.inproceedings = t2.inproceedings
  );
  --alter table inproceedings_author_subset add primary key (inproceedings, author);
  drop table if exists inproceedings_crossref_subset;
  create table inproceedings_crossref_subset as
  (
    select distinct t1.inproceedings, t1.proceedings from inproceedings_crossref as t1
      join inproceedings_subset as t2 on t1.inproceedings = t2.inproceedings
  );
  --alter table inproceedings_crossref_subset add primary key (inproceedings, proceedings);
  drop table if exists proceedings_subset;
  create table proceedings_subset as
  (
    select distinct t1.proceedings, t1.proceedings_name, t1.proceedings_year from proceedings as t1
      join inproceedings_crossref_subset as t2 on t1.proceedings = t2.proceedings
  );
  --alter table proceedings_subset add primary key (proceedings);
|}
      n
  in
  DB.print_and_execute setup_tables;
  ()

let run_links ~classic test_ctx =
  let db_args =
    Options.database_args_opt test_ctx
    |> String.split_on_char ':'
    |> List.tl
    |> String.concat ":"
  in
  let process =
    Format.asprintf
      "./_build/default/bin/links.exe tests/unit/lens/benchmark/dblp.links \
       --config=tests/unit/lens/benchmark/links.config --set=database_args=%s \
       --set=relational_lenses_classic=%b"
      db_args classic
  in
  Printf.printf "%s\n%!" process;
  let ch = Unix.open_process_in process in
  let l = input_line ch in
  let times =
    String.index l ')' - 1
    |> String.sub l 1
    |> String.split_on_char ','
    |> List.map ~f:(String.trim >> int_of_string)
  in
  Format.printf "%a\n%!" (Format.pp_comma_list Format.pp_print_int) times;
  times

let benchmark_links_with ~n ~classic test_ctx =
  setup_dblp_database ~n test_ctx;
  let qtime, ttime =
    List.init 20 (fun _ ->
        let res = run_links ~classic test_ctx in
        (List.nth res 1, List.nth res 2))
    |> List.split
  in
  (Tools.skip_median qtime, Tools.skip_median ttime)

let rowcounts_large =
  [ 500; 1000; 5000; 10000; 25000; 50000; 100000; 150000; 200000 ]

let benchmark_links test_ctx =
  let counts = rowcounts_large in
  let dat =
    List.map
      ~f:(fun n ->
        setup_dblp_database ~n test_ctx;
        let iqtime, ittime = benchmark_links_with ~n ~classic:false test_ctx in
        let cqtime, cttime = benchmark_links_with ~n ~classic:true test_ctx in
        (n, iqtime, ittime, cqtime, cttime))
      counts
  in
  Tools.print_csv_4 ~file:"dblp_example_local" dat

let benchmark_links_incremental test_ctx =
  let counts = rowcounts_large in
  let dat =
    List.map
      ~f:(fun n ->
        setup_dblp_database ~n test_ctx;
        let iqtime, ittime = benchmark_links_with ~n ~classic:false test_ctx in
        (n, iqtime, ittime))
      counts
  in
  Tools.print_csv_2 ~file:"dblp_example_incr_local" dat

let suite =
  "dblp"
  >::: [
         "setup_5000" >:: setup_dblp_database ~n:200;
         "run" >:: benchmark_links;
         "run_incr" >:: benchmark_links_incremental;
       ]
