open OUnitTest

open Links_core
open LensHelpers
open LensQueryHelpers
open LensSetOperations
open Value

open UnitTestsLensCommon

(* define composition operator *)
let (<<) f g x = f (g x)
let (<|) f x = f x

module Query = struct
    let lookup (col : string) (r : Value.t) =
        let rec fndi l =
            let c,_ = List.hd l in
            if c = col then 0 else 1 + fndi (List.tl l) in
        fndi (unbox_record r)

    let skip f a _b = f a

    let set (col : string) (fn : Value.t -> Value.t -> Value.t) (r : Value.t) =
        let r' = unbox_record r in
        let r' = List.map (fun (c,a) -> if col = c then (c,fn r a) else (c,a)) r' in
        box_record r'

    let ifcol (col : string) (bcond : Value.t -> bool) (i : Value.t) (r : Value.t) (oth : Value.t) =
        let (_,v) = List.find (fun (c,_) -> c = col) (unbox_record r) in
        if bcond v then i else oth

    let ifelse (bcond : Value.t -> bool) i el (v : Value.t) =
        if bcond v then
            i v
        else
            el v

    let band (fn1 : Value.t -> bool) (fn2 : Value.t -> bool) (v : Value.t) =
        (fn1 v) && (fn2 v)

    let gt (i : int) (v : Value.t) =
        i < (unbox_int v)

    let ge (i : int) (v : Value.t) =
        i <= (unbox_int v)

    let lt (i : int) (v : Value.t) =
        (unbox_int v) < i

    let le (i : int) (v : Value.t) =
        (unbox_int v) <= i

    let id (v : Value.t) = v

    let col (col : string) (v : Value.t) =
        let (_,v) = List.find (fun (c,_) -> c = col) (unbox_record v) in
        v

    let cst i (_v : Value.t) = i

    let iadd (i : int) (v : Value.t) =
        box_int (i + unbox_int v)

    let setcol (col : string) v (r : Value.t) =
        let r' = unbox_record r in
        let r' = List.map (fun (c,a) -> if col = c then (c,v r) else (c,a)) r' in
        box_record r'

    let setcolcst (col : string) (i : int) (r : Value.t) =
        setcol col (cst (box_int i)) r

    let map_records fn recs =
        let recs = unbox_list recs in
        let recs = List.map fn recs in
        box_list recs

    let map = map_records

    let append rs1 rs2 =
        let rs = List.append (unbox_list rs1) (unbox_list rs2) in
        box_list rs

    let filter fn recs =
        let recs = unbox_list recs in
        let recs = List.filter fn recs in
        box_list recs

end

let _ = Settings.set_value Debug.debugging_enabled true

(* can be replaced with List.init after 4.06 *)
let initlist n fn =
    let rec f i =
        fn i :: (if i+1 < n then f (i+1) else []) in
    f 0


let test_put test_ctx lens res =
    let classic_opt = UnitTestsLensCommon.classic_opt test_ctx in
    let benchmark_opt = UnitTestsLensCommon.benchmark_opt test_ctx in

    let step = if classic_opt then
        LensHelpersClassic.lens_put_step lens res
    else
        let data = LensHelpersIncremental.lens_get_delta lens res in
        LensTestHelpers.print_verbose test_ctx ("Delta Size: " ^ string_of_int (SortedRecords.total_size data));
        LensHelpersIncremental.lens_put_set_step lens data in

    let run () = step (fun _ res ->
                 LensTestHelpers.print_verbose test_ctx ("Delta Size (output): " ^ string_of_int (SortedRecords.total_size res));
                LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular res)
            ) in
    if benchmark_opt then
        let runs = initlist 20 (fun _i -> LensTestHelpers.time_op run) in
        let (qts, tts) = List.split runs in
        LensTestHelpers.print_query_time ();
        print_endline "query times";
        let prlist = print_endline << string_of_value << box_list << List.map box_int in
        prlist qts; prlist tts
    else
        (* calculate what the first step does *)
        let _res = LensTestHelpers.time_query false run in
        ();
    ;

    (* perform full update *)
    let put = if classic_opt then
        LensHelpersClassic.lens_put
    else
        LensHelpersIncremental.lens_put in
    put lens res;

    (* double check results *)
    let upd = lens_get lens None in
    LensTestHelpers.print_verbose test_ctx (string_of_value upd);
    LensTestHelpers.print_verbose test_ctx (string_of_value res);
    LensTestHelpers.assert_rec_list_eq upd res;
    ()

let override_n n test_ctx =
    let v = UnitTestsLensCommon.set_n_opt test_ctx in
    if v = 0 then
        n
    else
        v

let test_select_lens_1 n test_ctx =
    let n = override_n n test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (n / 15); `Rand] n in
    let l2 = LensTestHelpers.select_lens l1 (Phrase.equal (Phrase.var "b") (Phrase.constant_int 5)) in
    let res = lens_get l2 None in
    let _res = Query.map_records (Query.set "c" (Query.ifcol "a" (Query.band (Query.gt 60) (Query.lt 80)) (box_int 5))) (lens_get l2 None) in
    test_put test_ctx l2 res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1"

let test_drop_lens_1 n test_ctx =
    let n = override_n n test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (n / (UnitTestsLensCommon.set_upto_opt test_ctx)); `Rand] n in
    let l2 = LensTestHelpers.drop_lens l1 "c" "a" (box_int 1) in
    let _res = lens_get l2 None in
    let res = Query.map_records (Query.set "b" (Query.ifcol "a" (Query.band (Query.gt 60) (Query.lt 80)) (box_int 5))) (lens_get l2 None) in
    test_put test_ctx l2 res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1"

let test_select_lens_2 n test_ctx =
    let n = override_n n test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let upto = n / 10 in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (upto); `RandTo (100)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (upto)] upto in
    let l3 = LensTestHelpers.join_lens_dl l1 l2 ["b"] in
    let l4 = LensTestHelpers.select_lens l3 (
        Phrase.equal (Phrase.var "c") (Phrase.constant_int 3)) in
    let res = Query.map_records (Query.set "d" (Query.ifcol "b" (Query.band (Query.gt 0) (Query.lt 100)) (box_int 5))) (lens_get l4 ()) in
    test_put test_ctx l4 res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1";
    LensTestHelpers.drop_if_cleanup test_ctx db "t2";
    ()

let test_select_lens_3 n test_ctx =
    let n = override_n n test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let upto = n / 10 in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (upto); `RandTo (100)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (upto)] upto in
    let l3 = LensTestHelpers.join_lens_dl l1 l2 ["b"] in
    let l4 = LensTestHelpers.select_lens l3 (
        Phrase.equal (Phrase.var "c") (Phrase.constant_int 3)) in
    let res = ref (lens_get l4 ()) in
    let n = ref 0 in
    let changed () =
        let del = LensHelpersIncremental.lens_get_delta l4 !res in
        SortedRecords.total_size del in
    while changed () < UnitTestsLensCommon.set_upto_opt test_ctx && !n < upto do
        n := !n + 100;
        res := Query.map_records (Query.set "d" (Query.ifcol "b" (Query.band (Query.gt 0) (Query.lt !n)) (box_int 5))) !res
    done;
    test_put test_ctx l4 !res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1";
    LensTestHelpers.drop_if_cleanup test_ctx db "t2";
    ()


let test_get_delta test_ctx =
    let n = set_upto_opt test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let upto = n / 10 in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (upto); `RandTo (100)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (upto)] upto in
    let l3 = LensTestHelpers.join_lens_dl l1 l2 ["b"] in
    let res = Query.map_records (Query.set "d" (Query.ifcol "b" (Query.band (Query.gt 0) (Query.lt 10)) (box_int 5))) (lens_get l3 ()) in
    let run () =
        let _data = LensHelpersIncremental.lens_get_delta l3 res in
        () in
    let runs = initlist 20 (fun _i -> LensTestHelpers.time_op run) in
    let (qts, tts) = List.split runs in
    print_endline "query times";
    let prlist = print_endline << string_of_value << box_list << List.map box_int in
    prlist qts; prlist tts;
    ()

let test_put_delta test_ctx =
    let n = override_n 10000 test_ctx in
    let classic_opt = UnitTestsLensCommon.classic_opt test_ctx in
    let upto = UnitTestsLensCommon.set_upto_opt test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (n / 10); `RandTo (100)] n in
    let res = lens_get l1 () in
    (* updates count twice, deletes once, inserts once *)
    let upto = upto / 4 in
    (* remove first columns *)
    let res = Query.filter (Query.gt upto << Query.col "a") res in
    (* make updates to next set *)
    let res = Query.map_records (Query.ifelse
        (Query.band (Query.gt upto) (Query.le (upto * 2)) << Query.col "a")
        (Query.setcolcst "c" 5)
        (Query.id)) res in
    (* insert new rows copied from rest of table *)
    let res = Query.append res (
        Query.map
            (Query.setcol "a" (Query.iadd n << Query.col "a"))
            (Query.filter (
                Query.band (Query.gt (upto * 2)) (Query.le (upto * 3))
                << Query.col "a"
            ) res
        )
    ) in
    let table = match l1 with `Lens (t,_) -> t | _ -> assert false in
    let run = if classic_opt then
        let cols = Lens.Helpers.LensValue.cols_present_aliases l1 in
        let data = SortedRecords.construct_cols cols res in
        let run () = LensHelpersClassic.apply_table_data table data in
        run
    else
        let delta = LensHelpersIncremental.lens_get_delta l1 res in
        LensTestHelpers.print_verbose test_ctx ("Delta Size: " ^ string_of_int (SortedRecords.total_size delta));
        let run () = LensHelpersIncremental.apply_delta table delta in
        run in
    let runs = initlist 20 (fun _i -> LensTestHelpers.time_op run) in
    let (qts, tts) = List.split runs in
    print_endline "query times";
    let prlist = print_endline << string_of_value << box_list << List.map box_int in
    prlist qts; prlist tts;
    ()

let test_join_lens_1 n test_ctx =
    let n = override_n n test_ctx in
    let db = LensTestHelpers.get_db test_ctx in
    let upto = n / 10 in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (upto); `RandTo (100)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (upto)] upto in
    let l3 = LensTestHelpers.join_lens_dl l1 l2 ["b"] in
    let res = Query.map_records (Query.set "c" (Query.ifcol "b" (Query.band (Query.gt 40) (Query.lt 50)) (box_int 5))) (lens_get l3 ()) in
    test_put test_ctx l3 res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1";
    LensTestHelpers.drop_if_cleanup test_ctx db "t2";
    () (*
    assert (res.plus_rows = Array.of_list [
        [box_int 2; box_string "12"; box_string "data";];
        [box_int 3; box_string "this"; box_string "in";];
        [box_int 5; box_string "abc"; box_string "other";];
        [box_int 5; box_string "abc"; box_string "table";];
        [box_int 5; box_string "abc"; box_string "this";];
        [box_int 14; box_string "9012"; box_string "exists";]
    ]) *)

let test_join_lens_2 n test_ctx =
    let db = LensTestHelpers.get_db test_ctx in
    let upto = n / 10 in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (upto); `RandTo (30)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (40)] upto in
    let l3 = LensTestHelpers.join_lens_dl l1 l2 ["b"] in
    let res = Query.filter (Query.lt 40 << Query.col "b") (lens_get l3 ()) in
    LensTestHelpers.print_verbose test_ctx (string_of_value (lens_get l3 ()));
    LensTestHelpers.print_verbose test_ctx (string_of_value res);
    LensHelpersIncremental.lens_put_step l3 res (fun _ res ->
        LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular res)
    );
    LensHelpersIncremental.lens_put l3 res;
    let upd = lens_get l3 None in
    LensTestHelpers.print_verbose test_ctx (string_of_value upd);
    LensTestHelpers.print_verbose test_ctx (string_of_value res);
    LensTestHelpers.assert_rec_list_eq upd res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1";
    LensTestHelpers.drop_if_cleanup test_ctx db "t2";
    ()

let test_join_lens_dr_2 n test_ctx =
    let db = LensTestHelpers.get_db test_ctx in
    let l1 = LensTestHelpers.drop_create_populate_table test_ctx db "t1" "a -> b c" "a b c" [`Seq; `RandTo (200); `RandTo (30)] n in
    let l2 = LensTestHelpers.drop_create_populate_table test_ctx db "t2" "b -> d" "b d" [`Seq; `RandTo (40)] 50 in
    let l3 = LensTestHelpers.join_lens_dr l1 l2 ["b"] in
    let res = Query.filter (Query.lt 20 << Query.col "c") (lens_get l3 ()) in
    LensTestHelpers.print_verbose test_ctx (string_of_value (lens_get l3 ()));
    LensTestHelpers.print_verbose test_ctx (string_of_value res);
    LensHelpersIncremental.lens_put_step l3 res (fun _ res ->
        LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular res)
    );
    LensHelpersIncremental.lens_put l3 res;
    let upd = lens_get l3 None in
    LensTestHelpers.print_verbose test_ctx (string_of_value upd);
    LensTestHelpers.print_verbose test_ctx (string_of_value res);
    LensTestHelpers.assert_rec_list_eq upd res;
    LensTestHelpers.drop_if_cleanup test_ctx db "t1";
    LensTestHelpers.drop_if_cleanup test_ctx db "t2";
    ()

(* override the >:: so that it increases the timeout
 * normally it is 60s and not enough for benchmarking *)
let (>::) name testfn = TestLabel (name, TestCase (Huge, testfn))

let suite =
    "lens_primitives">:::
        [
            "select_1_0">:: test_select_lens_1 0;
            "select_1_50">:: test_select_lens_1 50;
            "select_2_500">:: test_select_lens_2 500;
            "select_2_10000">:: test_select_lens_2 10000;
            "select_3_10000">:: test_select_lens_3 10000;
            "drop_1_100">:: test_drop_lens_1 100;
            "join_1_100">:: test_join_lens_1 100;
            "join_1_10000">:: test_join_lens_1 10000;
            "join_2_100">:: test_join_lens_2 100;
            "join_2_dr_100">:: test_join_lens_dr_2 100;
            "get_delta">:: test_get_delta;
            "put_delta">:: test_put_delta;
        ];;
