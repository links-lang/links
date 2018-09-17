open LensSetOperations
open SortedRecords
open UnitTestsLensCommon
open OUnit2
open Value

let test_data_1 = { SortedRecords.columns = ["a"; "b"; "str";]; plus_rows = Array.of_list [
        [box_bool true; box_int 5; box_string "abc"];
        [box_bool true; box_int 3; box_string "this"];
        [box_bool false; box_int 9; box_string "0123"];
    ]; neg_rows = Array.of_list [
        [box_bool false; box_int 10; box_string "0123"];
        [box_bool false; box_int 11; box_string "0123"];
    ]};;


let test_data_2 = { SortedRecords.columns = ["a"; "b"; "str";]; plus_rows = Array.of_list [
        [box_bool true; box_int 5; box_string "abc"];
        [box_bool false; box_int 10; box_string "0123"];
    ]; neg_rows = Array.of_list [
        [box_bool false; box_int 9; box_string "0123"];
    ]}


let test_data_3 = { SortedRecords.columns = ["C"; "B"; "A";]; plus_rows = Array.of_list [
        [box_bool true; box_int 5; box_string "abc"];
        [box_bool true; box_int 3; box_string "this"];
        [box_bool false; box_int 9; box_string "0123"];
        [box_bool false; box_int 9; box_string "01234"];
    ]; neg_rows = Array.of_list [
        [box_bool false; box_int 10; box_string "0123"];
        [box_bool false; box_int 11; box_string "0123"];
    ]};;


let test_data_4 = { SortedRecords.columns = ["B"; "A";]; plus_rows = Array.of_list [
        [box_int 5; box_string "abc"];
        [box_int 3; box_string "this"];
        [box_int 9; box_string "0123"];
        [box_int 9; box_string "01234"];
        [box_int 12; box_string "012"];
        [box_int 9; box_string "9012"];
        [box_int 14; box_string "9012"];
        [box_int 2; box_string "12"];
    ]; neg_rows = Array.of_list [
    ]};;

let test_data_5 = { SortedRecords.columns = ["B"; "D";]; plus_rows = Array.of_list [
        [box_int 1; box_string "some"];
        [box_int 2; box_string "data"];
        [box_int 3; box_string "in"];
        [box_int 5; box_string "this"];
        [box_int 5; box_string "other"];
        [box_int 5; box_string "table"];
        [box_int 14; box_string "exists"];
        [box_int 8; box_string "here"];
    ]; neg_rows = Array.of_list [
    ]};;

let test_data_6 = { SortedRecords.columns = ["B"; "A";]; plus_rows = Array.of_list [
        [box_int 2; box_string "12"];
        [box_int 3; box_string "this"];
    ]; neg_rows = Array.of_list [
        [box_int 5; box_string "abc"];
        [box_int 14; box_string "9012"];
    ]};;

let test_data_7 = { SortedRecords.columns = ["B"; "D";]; plus_rows = Array.of_list [
        [box_int 2; box_string "data"];
        [box_int 14; box_string "exists"];
    ]; neg_rows = Array.of_list [
        [box_int 3; box_string "in"];
        [box_int 5; box_string "other"];
    ]};;

SortedRecords.sort test_data_1;;
SortedRecords.sort test_data_2;;
SortedRecords.sort test_data_3;;
SortedRecords.sort test_data_4

let test_construct_set _test_ctx =
    let recs = test_data_1 in
    assert_equal recs.columns ["a"; "b"; "str"];
    assert_equal recs.plus_rows (Array.of_list [
        [box_bool false; box_int 9; box_string "0123"];
        [box_bool true; box_int 3; box_string "this"];
        [box_bool true; box_int 5; box_string "abc"];
    ];);
    ()

let test_merge _test_ctx =
    let merge = SortedRecords.merge test_data_1 test_data_2 in
    let str = SortedRecords.to_string_tabular merge in
    print_endline str;
    ()


let test_minus _test_ctx = 
    let set = box_list [
        box_record ["a", box_bool true; "b", box_int 5; "str", box_string "abc"];
        box_record ["a", box_bool true; "b", box_int 3; "str", box_string "this"];
        box_record ["a", box_bool false; "b", box_int 9; "str", box_string "0123"];
    ] in
    let recs1 = SortedRecords.construct set in
    let set2 = box_list [
        box_record ["str", box_string "abc"; "b", box_int 5; ];
        box_record ["str", box_string "0123"; "b", box_int 9; ];
    ] in
    let recs2 = SortedRecords.construct set2 in
    let res = SortedRecords.minus recs1 recs2 in
    let _str = SortedRecords.to_string_tabular res in
    (* print_endline ("\n" ^ _str); *)
    ()

let test_project _test_ctx = 
    let set = box_list [
        box_record ["a", box_bool true; "b", box_int 5; "str", box_string "abc"];
        box_record ["a", box_bool true; "b", box_int 3; "str", box_string "this"];
        box_record ["a", box_bool false; "b", box_int 9; "str", box_string "0123"];
    ] in
    let recs = SortedRecords.construct set in
    (* the project onto should drop 'nonexistant' *)
    let recs = SortedRecords.project_onto recs ["str"; "b"; "nonexistant"] in
    (* let str = SortedRecords.to_string_tabular recs in
    print_endline ("\n" ^ str); *)
    assert_equal recs {columns = ["str"; "b"]; plus_rows = Array.of_list [
        [box_string "0123"; box_int 9];
        [box_string "abc"; box_int 5];
        [box_string "this"; box_int 3];
    ]; neg_rows = Array.of_list []; }

let test_compare _test_ctx = 
    let set = box_list [
        box_record ["a", box_bool true; "b", box_int 5; "str", box_string "abc"];
        box_record ["a", box_bool true; "b", box_int 3; "str", box_string "this"];
        box_record ["a", box_bool false; "b", box_int 9; "str", box_string "0123"];
    ] in
    let recs = SortedRecords.construct set in
    let find = [box_bool true; box_int 3; box_string "this"] in
    let cmp i = (SortedRecords.compare (Array.get (recs.plus_rows) i) find) in
    assert ((cmp 1) = 0);
    assert ((cmp 0) < 0);
    assert ((cmp 2) > 0)


let test_find_set _test_ctx =
    let set1 = box_list [
        box_record ["a", box_bool true; "b", box_int 5; "str", box_string "abc"];
        box_record ["a", box_bool true; "b", box_int 3; "str", box_string "this"];
        box_record ["a", box_bool false; "b", box_int 9; "str", box_string "0123"];
    ] in
    let set2 = box_list [
        box_record ["a", box_bool true; "b", box_int 3; "str", box_string "this"];
    ] in
    let set3 = box_list [
        box_record ["a", box_bool false; "b", box_int 9; "str", box_string "0123"];
    ] in
    let _set4 = box_list [
    ] in
    let find = [box_bool true; box_int 3; box_string "this"] in
    let find2 = [box_bool true; box_int 5; box_string "abc"] in
    let find3 = [box_bool false; box_int 9; box_string "0123"] in
    let test set find = SortedRecords.find (SortedRecords.construct set) find in
    assert (test set1 find);
    assert (test set2 find);
    assert (not (test set3 find));
    assert (not (test set3 find));
    assert (test set1 find2);
    assert (test set1 find3);
    ()

let test_reorder_cols _test_ctx =
    let cols1 = ["a"; "b"; "c"; "test"] in
    let reorder = ["test"; "b"] in
    let res = SortedRecords.reorder_cols cols1 reorder in
    assert (res = ["test"; "b"; "a"; "c"])

let test_find_all_ind_1 test_ctx =
    let data = test_data_4 in
    let find = [box_int 9] in
    let (b,e) = SortedRecords.find_all_ind data.plus_rows find in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data);
    LensTestHelpers.print_verbose test_ctx (string_of_int b ^ " to " ^ string_of_int e);
    assert (b = 3);
    assert (e = 5)

let test_find_all_ind_2 test_ctx =
    let data = test_data_1 in
    let find = [box_bool true] in
    let (b,e) = SortedRecords.find_all_ind data.plus_rows find in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data);
    LensTestHelpers.print_verbose test_ctx (string_of_int b ^ " to " ^ string_of_int e);
    assert (b = 1);
    assert (e = 2)

let test_find_all_ind_3 test_ctx =
    let data = test_data_1 in
    let find = [box_bool false] in
    let (b,e) = SortedRecords.find_all_ind data.plus_rows find in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data);
    LensTestHelpers.print_verbose test_ctx (string_of_int b ^ " to " ^ string_of_int e);
    assert (b = 0);
    assert (e = 0)

let test_find_all_1 test_ctx =
    let data = test_data_4 in
    let find = [box_int 9] in
    let rows = SortedRecords.find_all data.plus_rows find in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data);
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular {
        SortedRecords.columns = data.columns;
        plus_rows = rows;
        neg_rows = Array.of_list [];
    });
    assert (rows = Array.of_list [
        [box_int 9; box_string "0123";];
        [box_int 9; box_string "01234";];
        [box_int 9; box_string "9012";];
    ])

let test_join_1 test_ctx =
    let data_l = test_data_4 in
    let data_r = test_data_5 in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data_l);
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data_r);
    let joined = SortedRecords.join data_l data_r ["B"] in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular joined);
    assert (joined.plus_rows = Array.of_list [
        [box_int 2; box_string "12"; box_string "data"];
        [box_int 3; box_string "this"; box_string "in"];
        [box_int 5; box_string "abc"; box_string "other"];
        [box_int 5; box_string "abc"; box_string "table"];
        [box_int 5; box_string "abc"; box_string "this"];
        [box_int 14; box_string "9012"; box_string "exists"];
    ])
 
let test_join_neg_1 test_ctx =
    let data_l = test_data_6 in
    let data_r = test_data_7 in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data_l);
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular data_r);
    let joined = SortedRecords.join data_l data_r ["B"] in
    LensTestHelpers.print_verbose test_ctx (SortedRecords.to_string_tabular joined);
    assert (joined.plus_rows = Array.of_list [
        [box_int 2; box_string "12"; box_string "data"];
    ]);
    assert (joined.neg_rows = Array.of_list [
        [box_int 3; box_string "this"; box_string "in"];
        [box_int 5; box_string "abc"; box_string "other"];
        [box_int 14; box_string "9012"; box_string "exists"];
    ])

let suite =
    "lens_set_operations">:::
        [
            "construct_set">:: test_construct_set;
            "compare">:: test_compare;
            "find_set">:: test_find_set;
            "project">:: test_project;
            "minus">:: test_minus;
            "merge">:: test_merge;
            "reorder_cols">:: test_reorder_cols;
            "find_all_ind_1">:: test_find_all_ind_1;
            "find_all_ind_2">:: test_find_all_ind_2;
            "find_all_ind_3">:: test_find_all_ind_3;
            "find_all_1">:: test_find_all_1;
            "join_1">:: test_join_1;
            "join_neg_1">:: test_join_neg_1;
        ];;
