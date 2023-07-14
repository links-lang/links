open OUnitTest
open UnitTestsLensCommon
open Links_core
module H = LensTestHelpers
module Sorted = Lens.Sorted_records
module Phrase = Lens.Phrase
open Lens.Utility
open Lens.Utility.O
open Lens.Phrase.Value

open TestUtility

module R = Lens.Phrase.Value.Record

(* define composition operator *)
let ( <| ) f x = f x

module Query = struct
  module Value = Lens.Phrase.Value

  let lookup (col : string) (r : Value.t) =
    let rec find l =
      let c, _ = List.hd l in
      if c = col then 0 else 1 + find (List.tl l)
    in
    find (unbox_record r)

  let skip f a _b = f a

  let set (col : string) (fn : Value.t -> Value.t -> Value.t) (r : Value.t) =
    let r' = unbox_record r in
    let r' =
      List.map ~f:(fun (c, a) -> if col = c then (c, fn r a) else (c, a)) r'
    in
    box_record r'

  let ifcol (col : string) (bcond : Value.t -> bool) (i : Value.t) (r : Value.t)
      (oth : Value.t) =
    let _, v = List.find_exn ~f:(fun (c, _) -> c = col) (unbox_record r) in
    if bcond v then i else oth

  let ifelse (bcond : Value.t -> bool) i el (v : Value.t) =
    if bcond v then i v else el v

  let band (fn1 : Value.t -> bool) (fn2 : Value.t -> bool) (v : Value.t) =
    fn1 v && fn2 v

  let gt (i : int) (v : Value.t) = i < unbox_int v

  let ge (i : int) (v : Value.t) = i <= unbox_int v

  let lt (i : int) (v : Value.t) = unbox_int v < i

  let le (i : int) (v : Value.t) = unbox_int v <= i

  let id (v : Value.t) = v

  let col (col : string) (v : Value.t) =
    let _, v = List.find_exn ~f:(fun (c, _) -> c = col) (unbox_record v) in
    v

  let cst i (_v : Value.t) = i

  let iadd (i : int) (v : Value.t) = box_int (i + unbox_int v)

  let setcol (col : string) v (r : Value.t) =
    let r' = unbox_record r in
    let r' =
      List.map ~f:(fun (c, a) -> if col = c then (c, v r) else (c, a)) r'
    in
    box_record r'

  let setcolcst (col : string) (i : int) (r : Value.t) =
    setcol col (cst (box_int i)) r

  let map_records f recs = List.map ~f recs

  let map = map_records

  let append rs1 rs2 = List.append rs1 rs2

  let filter fn recs = List.filter fn recs
end

let get_int r c = R.get_exn ~key:c r |> unbox_int
let set_int r c v = R.set ~key:c ~value:(box_int v) r

let table_template_1 ~n db =
  let module DB = (val db : Database_S.S) in
  let upto = n / 10 in
  DB.drop_create_easy_populate ~table:"t1" ~fd:"a -> b c" ~n
    [ `Seq; `RandTo upto; `RandTo 100 ]

let table_template_2 ~n db =
  let module DB = (val db : Database_S.S) in
  let upto = n / 10 in
  DB.drop_create_easy_populate ~table:"t2" ~fd:"b -> d" ~n:upto
    [ `Seq; `RandTo upto ]

let test_put test_ctx ~db ~behaviour lens view =
  let module V = (val Debug.verbose_printer test_ctx) in
  Lens.Eval.put ~behaviour ~db lens view |> Result.ok_exn;
  (* double check results *)
  let upd = Lens.Value.lens_get ~db lens in
  V.printf "%a" Phrase.Value.pp_values upd;
  V.printf "%a" Phrase.Value.pp_values view;
  H.assert_rec_list_eq upd view

let test_put_benchmark_stats test_ctx ~db ~behaviour lens res =
  let module V = (val Debug.verbose_printer test_ctx) in
  let step cb =
    match behaviour with
    | Lens.Eval.Classic -> Lens.Eval.Classic.lens_put_step ~db lens res cb
    | Lens.Eval.Incremental ->
        let data = Lens.Eval.Incremental.lens_get_delta ~db lens res in
        V.printf "Delta size: %i" (Lens.Sorted_records.total_size data);
        let env = Int.Map.empty in
        let cb ~env:_ lens data =
          cb lens data;
          env
        in
        Lens.Eval.Incremental.lens_put_set_step ~db ~env lens data cb |> ignore
  in
  let run () =
    step (fun _ res ->
        V.printf "Delta Size (output): %i" (Sorted.total_size res);
        V.printf "%a" Sorted.pp_tabular res)
  in
  let runs = List.init 20 (fun _i -> H.time_op run) in
  let qts, tts = List.split runs in
  H.print_query_time ();
  print_endline "query times";
  let prlist =
    List.map ~f:Phrase.Value.box_int
    >> Phrase.Value.show_values
    >> print_endline
  in
  prlist qts;
  prlist tts

let test_put_benchmark_step test_ctx ~db ~behaviour lens res =
  let module V = (val Debug.verbose_printer test_ctx) in
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
  List.init 20 (fun _i -> H.time_op step)

let override_n n test_ctx =
  let v = UnitTestsLensCommon.set_n_opt test_ctx in
  if v = 0 then n else v

let behaviour test_ctx =
  if classic_opt test_ctx then Lens.Eval.Classic else Lens.Eval.Incremental

let test_select_lens_1 n test_ctx =
  let n = override_n n test_ctx in
  let behaviour = behaviour test_ctx in
  let module DB = (val Table.create test_ctx) in
  let module T1 =
    (val DB.drop_create_easy_populate ~table:"t1" ~fd:"a -> b c" ~n
           [ `Seq; `RandTo (n / 15); `Rand ])
  in
  let db = DB.db in
  let l1 = T1.lens () in
  let l2 =
    Mk_lens.select
      ~predicate:(Phrase.equal (Phrase.var "b") (Phrase.Constant.int 5))
      l1
  in
  let res = Lens.Value.lens_get ~db l2 in
  let res =
    List.map
      ~f:(fun r ->
        let a = get_int r "a" in
        if a > 60 && a <= 80 then set_int r "c" 5 else r)
      res
  in
  test_put ~db ~behaviour test_ctx l2 res;
  T1.drop_if_cleanup ()

let template_drop_lens_1 ~n ~put test_ctx =
  let n = override_n n test_ctx in
  let module DB = (val Table.create test_ctx) in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let db = DB.db in
  let l1 = T1.lens () in
  let l2 = Mk_lens.drop l1 "c" "a" (box_int 1) in
  let res = Lens.Value.lens_get ~db l2 in
  let res =
    List.map
      ~f:(fun r ->
        let a = get_int r "a" in
        if a > 60 && a <= 80 then set_int r "b" 5 else r)
      res
  in
  put ~db l2 res;
  T1.drop_if_cleanup ()

let test_drop_lens_1 n test_ctx =
  let behaviour = behaviour test_ctx in
  template_drop_lens_1 ~n ~put:(test_put test_ctx ~behaviour) test_ctx

let template_select_lens_2 ~n ~put test_ctx =
  let n = override_n n test_ctx in
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let module T2 = (val table_template_2 ~n (module DB)) in
  let l1 = T1.lens () in
  let l2 = T2.lens () in
  let l3 = Mk_lens.join_dl l1 l2 in
  let l4 =
    Mk_lens.select
      ~predicate:(Phrase.equal (Phrase.var "c") (Phrase.Constant.int 3))
      l3
  in
  let res =
    List.map
      ~f:(fun r ->
        let b = get_int r "b" in
        if b > 0 && b <= 100 then set_int r "d" 5 else r)
      (Lens.Value.lens_get ~db l4)
  in
  put ~db l4 res;
  T1.drop_if_cleanup ();
  T2.drop_if_cleanup ()

let test_select_lens_2 n test_ctx =
  let behaviour = behaviour test_ctx in
  template_select_lens_2 ~n ~put:(test_put test_ctx ~behaviour) test_ctx

let template_select_lens_3 ~n ~upto ~put test_ctx =
  let n = override_n n test_ctx in
  let upto_max = n / 10 in
  let module V = (val Debug.verbose_printer test_ctx) in
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let module T2 = (val table_template_2 ~n (module DB)) in
  let l1 = T1.lens () in
  let l2 = T2.lens () in
  let l3 = Mk_lens.join_dl l1 l2 in
  let l4 =
    Mk_lens.select
      ~predicate:(Phrase.equal (Phrase.var "c") (Phrase.Constant.int 3))
      l3
  in
  let res = ref (Lens.Value.lens_get ~db l4) in
  let n = ref 0 in
  let changed () =
    let del = Lens.Eval.Incremental.lens_get_delta ~db l4 !res in
    Sorted.total_size del
  in
  while changed () < upto && !n < upto_max do
    n := !n + 100;
    res :=
      List.map
        ~f:(fun r ->
          let b = get_int r "b" in
          if 0 < b && b <= !n then set_int r "d" 5 else r)
        !res
  done;
  V.printf "changed size %i for n=%i\n%!" (changed ()) !n;
  put ~db l4 !res;
  T1.drop_if_cleanup ();
  T2.drop_if_cleanup ()

let test_select_lens_3 n test_ctx =
  let behaviour = behaviour test_ctx in
  let upto = UnitTestsLensCommon.set_upto_opt test_ctx in
  template_select_lens_3 ~n ~upto ~put:(test_put test_ctx ~behaviour) test_ctx

let template_get_delta ~n ~get_delta test_ctx =
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let module T2 = (val table_template_2 ~n (module DB)) in
  let l1 = T1.lens () in
  let l2 = T2.lens () in
  let l3 = Mk_lens.join_dl l1 l2 in
  let res =
    List.map
      ~f:(fun r ->
        let b = get_int r "b" in
        if b > 0 && b <= 10 then set_int r "d" 5 else r)
      (Lens.Value.lens_get ~db l3)
  in
  get_delta ~db l3 res |> ignore;
  T1.drop_if_cleanup ();
  T2.drop_if_cleanup ()

let test_get_delta test_ctx =
  let n = set_upto_opt test_ctx in
  template_get_delta ~n ~get_delta:Lens.Eval.Incremental.lens_get_delta test_ctx

(* let () = Lens.Debug.set_debug true *)

let template_put_delta ~n ~upto ~put test_ctx =
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 =
    (val DB.drop_create_easy_populate ~table:"t1" ~fd:"a -> b c" ~n
           [ `Seq; `RandTo (n / 10); `RandTo 100 ])
  in
  let l1 = T1.lens () in
  let res = Lens.Value.lens_get ~db l1 in
  (* updates count twice, deletes once, inserts once *)
  let upto = upto / 4 in
  (* remove first columns *)
  let res = List.filter (fun r -> get_int r "a" > upto) res in
  (* make updates to next set *)
  let res =
    List.map
      ~f:(fun r ->
        let a = get_int r "a" in
        if a > upto && a <= upto * 2 then set_int r "c" 5 else r)
      res
  in
  (* insert new rows copied from rest of table *)
  let res =
    List.filter
      (fun r ->
        let a = get_int r "a" in
        a > upto * 2 && a <= upto * 3)
      res
    |> List.map ~f:(fun r -> set_int r "a" (get_int r "a" + n))
    |> List.append res
  in
  put ~db l1 res;
  T1.drop_if_cleanup ()

let prim_lens_table l =
  match l with
  | Lens.Value.Lens { table; _ } -> table
  | _ -> assert false

let put_delta_classic ~db l res =
  let table = prim_lens_table l in
  let cols = Lens.Value.cols_present_aliases l in
  let data = Sorted.construct_cols ~columns:cols ~records:res in
  Lens.Eval.Classic.apply_table_data ~table ~db data

(*
  let run () = Lens.Eval.Classic.apply_table_data ~table ~db data in
  (run, fun () -> ()) *)

let put_delta_incremental test_ctx ~db l res =
  let module V = (val TestUtility.Debug.verbose_printer test_ctx) in
  let table = prim_lens_table l in
  let delta = Lens.Eval.Incremental.lens_get_delta ~db l res in
  V.printf "Delta Size: %i" (Sorted.total_size delta);
  let sort = Lens.Value.sort l in
  let env = Int.Map.empty in
  Lens.Eval.Incremental.apply_delta ~db ~table ~sort ~env delta |> ignore

(*
   old code: used to have revert function
  let run () =
    Lens.Eval.Incremental.apply_delta ~db ~table ~sort ~env delta |> ignore
  in
  let neg = Lens.Sorted_records.negate delta in
  let revert () =
    Lens.Eval.Incremental.apply_delta ~db ~table ~sort ~env neg |> ignore
  in
  (run, revert) *)

let put_delta_behaviour test_ctx ~behaviour =
  match behaviour with
  | Lens.Eval.Incremental -> put_delta_incremental test_ctx
  | Lens.Eval.Classic -> put_delta_classic

let test_put_delta test_ctx =
  let n = override_n 10000 test_ctx in
  let upto = UnitTestsLensCommon.set_upto_opt test_ctx in
  let behaviour = behaviour test_ctx in
  template_put_delta ~n ~upto
    ~put:(put_delta_behaviour test_ctx ~behaviour)
    test_ctx

let template_join_lens_1 ?(mk_join = Mk_lens.join_dl) ~n ~put test_ctx =
  let n = override_n n test_ctx in
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let module T2 = (val table_template_2 ~n (module DB)) in
  let l1 = T1.lens () in
  let l2 = T2.lens () in
  let l3 = mk_join l1 l2 in
  let res =
    List.map
      ~f:(fun r ->
        let b = get_int r "b" in
        if b > 40 && b <= 50 then set_int r "c" 5 else r)
      (Lens.Value.lens_get ~db l3)
  in
  put ~db l3 res;
  T1.drop_if_cleanup ();
  T2.drop_if_cleanup ()

let test_join_lens_1 n test_ctx =
  let behaviour = behaviour test_ctx in
  template_join_lens_1 ~n ~put:(test_put test_ctx ~behaviour) test_ctx

let test_join_lens_2 n test_ctx =
  let module V = (val Debug.verbose_printer test_ctx) in
  let module DB = (val Table.create test_ctx) in
  let db = DB.db in
  let module T1 = (val table_template_1 ~n (module DB)) in
  let module T2 = (val table_template_2 ~n (module DB)) in
  let l1 = T1.lens () in
  let l2 = T2.lens () in
  let l3 = H.join_lens_dl l1 l2 [ ("b", "b", "b") ] in
  let res =
    List.filter
      (fun r ->
        let b = get_int r "b" in
        b < 40)
      (Lens.Value.lens_get ~db l3)
  in
  V.printf "%a" Phrase.Value.pp_values (Lens.Value.lens_get ~db l3);
  V.printf "%a" Phrase.Value.pp_values res;
  let env = Int.Map.empty in
  Lens.Eval.Incremental.lens_put_step l3 res ~env (fun ~env _ res ->
      V.printf "%a" Sorted.pp_tabular res;
      env)
  |> ignore;
  Lens.Eval.Incremental.lens_put ~db l3 res;
  let upd = Lens.Value.lens_get ~db l3 in
  V.printf "%a" Phrase.Value.pp_values upd;
  V.printf "%a" Phrase.Value.pp_values res;
  H.assert_rec_list_eq upd res;
  T1.drop_if_cleanup ();
  T2.drop_if_cleanup ()

let test_join_lens_dr_2 n test_ctx =
  let db = H.get_db test_ctx in
  let l1 =
    H.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c"
      [ `Seq; `RandTo 200; `RandTo 30 ]
      n
  in
  let l2 =
    H.drop_create_populate_table test_ctx db "t2" "b -> d" "b d"
      [ `Seq; `RandTo 40 ]
      50
  in
  let l3 = H.join_lens_dr l1 l2 [ ("b", "b", "b") ] in
  let res =
    Query.filter (Query.lt 20 << Query.col "c") (Lens.Value.lens_get ~db l3)
  in
  H.print_verbose test_ctx
    (Phrase.Value.show_values (Lens.Value.lens_get ~db l3));
  H.print_verbose test_ctx (Phrase.Value.show_values res);
  let env = Int.Map.empty in
  Lens.Eval.Incremental.lens_put_step l3 res ~env (fun ~env _ res ->
      H.print_verbose test_ctx (Format.asprintf "%a" Sorted.pp_tabular res);
      env)
  |> ignore;
  Lens.Eval.Incremental.lens_put ~db l3 res;
  let upd = Lens.Value.lens_get ~db l3 in
  H.print_verbose test_ctx (Phrase.Value.show_values upd);
  H.print_verbose test_ctx (Phrase.Value.show_values res);
  H.assert_rec_list_eq upd res;
  H.drop_if_cleanup test_ctx db "t1";
  H.drop_if_cleanup test_ctx db "t2";
  ()

(* override the >:: so that it increases the timeout
 * normally it is 60s and not enough for benchmarking *)
let ( >:: ) name testfn = TestLabel (name, TestCase (Huge, testfn))

let suite =
  "lens_primitives"
  >::: [
         "select_1_0" >:: test_select_lens_1 0;
         "select_1_50" >:: test_select_lens_1 50;
         "select_2_500" >:: test_select_lens_2 500;
         "select_2_10000" >:: test_select_lens_2 10000;
         "select_3_10000" >:: test_select_lens_3 10000;
         "drop_1_100" >:: test_drop_lens_1 100;
         "join_1_100" >:: test_join_lens_1 100;
         "join_1_10000" >:: test_join_lens_1 10000;
         "join_2_100" >:: test_join_lens_2 100;
         "join_2_dr_100" >:: test_join_lens_dr_2 100;
         "get_delta" >:: test_get_delta;
         "put_delta" >:: test_put_delta;
       ]
