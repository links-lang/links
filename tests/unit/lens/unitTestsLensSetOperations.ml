open OUnit2
open Links_core
open UnitTestsLensCommon
open Lens.Phrase.Value
module Sorted = Lens.Sorted_records

let print_verbose_sorted test_ctx sorted =
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular sorted)

let test_data_1 =
  Sorted.construct_full ~columns:[ "a"; "b"; "str" ]
    ~plus:
      [
        [ box_bool true; box_int 5; box_string "abc" ];
        [ box_bool true; box_int 3; box_string "this" ];
        [ box_bool false; box_int 9; box_string "0123" ];
      ]
    ~neg:
      [
        [ box_bool false; box_int 10; box_string "0123" ];
        [ box_bool false; box_int 11; box_string "0123" ];
      ]

let test_data_2 =
  Sorted.construct_full ~columns:[ "a"; "b"; "str" ]
    ~plus:
      [
        [ box_bool true; box_int 5; box_string "abc" ];
        [ box_bool false; box_int 10; box_string "0123" ];
      ]
    ~neg:[ [ box_bool false; box_int 9; box_string "0123" ] ]

let test_data_3 =
  Sorted.construct_full ~columns:[ "C"; "B"; "A" ]
    ~plus:
      [
        [ box_bool true; box_int 5; box_string "abc" ];
        [ box_bool true; box_int 3; box_string "this" ];
        [ box_bool false; box_int 9; box_string "0123" ];
        [ box_bool false; box_int 9; box_string "01234" ];
      ]
    ~neg:
      [
        [ box_bool false; box_int 10; box_string "0123" ];
        [ box_bool false; box_int 11; box_string "0123" ];
      ]

let test_data_4 =
  Sorted.construct_full ~columns:[ "B"; "A" ]
    ~plus:
      [
        [ box_int 5; box_string "abc" ];
        [ box_int 3; box_string "this" ];
        [ box_int 9; box_string "0123" ];
        [ box_int 9; box_string "01234" ];
        [ box_int 12; box_string "012" ];
        [ box_int 9; box_string "9012" ];
        [ box_int 14; box_string "9012" ];
        [ box_int 2; box_string "12" ];
      ]
    ~neg:[]

let test_data_5 =
  Sorted.construct_full ~columns:[ "B"; "D" ]
    ~plus:
      [
        [ box_int 1; box_string "some" ];
        [ box_int 2; box_string "data" ];
        [ box_int 3; box_string "in" ];
        [ box_int 5; box_string "this" ];
        [ box_int 5; box_string "other" ];
        [ box_int 5; box_string "table" ];
        [ box_int 14; box_string "exists" ];
        [ box_int 8; box_string "here" ];
      ]
    ~neg:[]

let test_data_6 =
  Sorted.construct_full ~columns:[ "B"; "A" ]
    ~plus:[ [ box_int 2; box_string "12" ]; [ box_int 3; box_string "this" ] ]
    ~neg:[ [ box_int 5; box_string "abc" ]; [ box_int 14; box_string "9012" ] ]

let test_data_7 =
  Sorted.construct_full ~columns:[ "B"; "D" ]
    ~plus:
      [ [ box_int 2; box_string "data" ]; [ box_int 14; box_string "exists" ] ]
    ~neg:[ [ box_int 3; box_string "in" ]; [ box_int 5; box_string "other" ] ]

let test_data_8 =
  Sorted.construct_full ~columns:[ "a"; "b"; "c"; "test" ] ~plus:[] ~neg:[]

let test_data_9 =
  Sorted.construct_full ~columns:[ "B"; "A" ]
    ~plus:
      [
        [ box_int 2; box_string "24" ];
        [ box_int 3; box_string "that" ];
        [ box_int 99; box_string "luftballons" ];
      ]
    ~neg:[]
;;
Sorted.sort test_data_1;;
Sorted.sort test_data_2;;
Sorted.sort test_data_3;;
Sorted.sort test_data_4

let test_construct_set _test_ctx =
  let recs = test_data_1 in
  assert_equal (Sorted.columns recs) [ "a"; "b"; "str" ];
  assert_equal (Sorted.plus_rows recs)
    (Array.of_list
       [
         [ box_bool false; box_int 9; box_string "0123" ];
         [ box_bool true; box_int 3; box_string "this" ];
         [ box_bool true; box_int 5; box_string "abc" ];
       ]);
  ()

let test_merge _test_ctx =
  let merge = Sorted.merge test_data_1 test_data_2 in
  let str = Format.asprintf "%a" Sorted.pp_tabular merge in
  print_endline str;
  ()

let test_minus _test_ctx =
  let set =
    [
      box_record
        [ ("a", box_bool true); ("b", box_int 5); ("str", box_string "abc") ];
      box_record
        [ ("a", box_bool true); ("b", box_int 3); ("str", box_string "this") ];
      box_record
        [ ("a", box_bool false); ("b", box_int 9); ("str", box_string "0123") ];
    ]
  in
  let recs1 = Sorted.construct ~records:set in
  let set2 =
    [
      box_record [ ("str", box_string "abc"); ("b", box_int 5) ];
      box_record [ ("str", box_string "0123"); ("b", box_int 9) ];
    ]
  in
  let recs2 = Sorted.construct ~records:set2 in
  let res = Sorted.minus recs1 recs2 in
  let _str = Format.asprintf "%a" Sorted.pp_tabular res in
  ()

let test_project _test_ctx =
  let set =
    [
      box_record
        [ ("a", box_bool true); ("b", box_int 5); ("str", box_string "abc") ];
      box_record
        [ ("a", box_bool true); ("b", box_int 3); ("str", box_string "this") ];
      box_record
        [ ("a", box_bool false); ("b", box_int 9); ("str", box_string "0123") ];
    ]
  in
  let recs = Sorted.construct ~records:set in
  (* the project onto should drop 'nonexistant' *)
  let recs = Sorted.project_onto recs ~columns:[ "str"; "b"; "nonexistant" ] in
  (* let str = SortedRecords.to_string_tabular recs in
     print_endline ("\n" ^ str); *)
  assert_equal recs
    (Sorted.construct_full ~columns:[ "str"; "b" ]
       ~plus:
         [
           [ box_string "0123"; box_int 9 ];
           [ box_string "abc"; box_int 5 ];
           [ box_string "this"; box_int 3 ];
         ]
       ~neg:[])

let test_compare _test_ctx =
  let set =
    [
      box_record
        [ ("a", box_bool true); ("b", box_int 5); ("str", box_string "abc") ];
      box_record
        [ ("a", box_bool true); ("b", box_int 3); ("str", box_string "this") ];
      box_record
        [ ("a", box_bool false); ("b", box_int 9); ("str", box_string "0123") ];
    ]
  in
  let recs = Sorted.construct ~records:set in
  let find = [ box_bool true; box_int 3; box_string "this" ] in
  let cmp i = Sorted.Simple_record.compare (Sorted.plus_rows recs).(i) find in
  assert (cmp 1 = 0);
  assert (cmp 0 < 0);
  assert (cmp 2 > 0)

let test_find_set _test_ctx =
  let set1 =
    [
      box_record
        [ ("a", box_bool true); ("b", box_int 5); ("str", box_string "abc") ];
      box_record
        [ ("a", box_bool true); ("b", box_int 3); ("str", box_string "this") ];
      box_record
        [ ("a", box_bool false); ("b", box_int 9); ("str", box_string "0123") ];
    ]
  in
  let set2 =
    [
      box_record
        [ ("a", box_bool true); ("b", box_int 3); ("str", box_string "this") ];
    ]
  in
  let set3 =
    [
      box_record
        [ ("a", box_bool false); ("b", box_int 9); ("str", box_string "0123") ];
    ]
  in
  let _set4 = [] in
  let find = [ box_bool true; box_int 3; box_string "this" ] in
  let find2 = [ box_bool true; box_int 5; box_string "abc" ] in
  let find3 = [ box_bool false; box_int 9; box_string "0123" ] in
  let test set find =
    Sorted.find (Sorted.construct ~records:set) ~record:find
  in
  assert (test set1 find);
  assert (test set2 find);
  assert (not (test set3 find));
  assert (not (test set3 find));
  assert (test set1 find2);
  assert (test set1 find3);
  ()

let test_reorder_cols _test_ctx =
  let reorder = [ "test"; "b" ] in
  let res = Sorted.reorder_exn test_data_8 ~first:reorder in
  assert (Sorted.columns res = [ "test"; "b"; "a"; "c" ])

let test_find_all_ind_1 test_ctx =
  let data = test_data_4 in
  let find = [ box_int 9 ] in
  let b, e =
    Sorted.Simple_record.find_all_index (Sorted.plus_rows data) ~record:find
  in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data);
  LensTestHelpers.print_verbose test_ctx
    (string_of_int b ^ " to " ^ string_of_int e);
  assert (b = 3);
  assert (e = 5)

let test_find_all_ind_2 test_ctx =
  let data = test_data_1 in
  let find = [ box_bool true ] in
  let b, e =
    Sorted.Simple_record.find_all_index (Sorted.plus_rows data) ~record:find
  in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data);
  LensTestHelpers.print_verbose test_ctx
    (string_of_int b ^ " to " ^ string_of_int e);
  assert (b = 1);
  assert (e = 2)

let test_find_all_ind_3 test_ctx =
  let data = test_data_1 in
  let find = [ box_bool false ] in
  let b, e =
    Sorted.Simple_record.find_all_index (Sorted.plus_rows data) ~record:find
  in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data);
  LensTestHelpers.print_verbose test_ctx
    (string_of_int b ^ " to " ^ string_of_int e);
  assert (b = 0);
  assert (e = 0)

let test_find_all_1 test_ctx =
  let data = test_data_4 in
  let find = [ box_int 9 ] in
  let rows =
    Sorted.Simple_record.find_all_record (Sorted.plus_rows data) ~record:find
  in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data);
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular
       (Sorted.construct_full ~columns:(Sorted.columns data)
          ~plus:(Array.to_list rows) ~neg:[]));
  assert (
    rows
    = Array.of_list
        [
          [ box_int 9; box_string "0123" ];
          [ box_int 9; box_string "01234" ];
          [ box_int 9; box_string "9012" ];
        ])

let test_join_1 test_ctx =
  let data_l = test_data_4 in
  let data_r = test_data_5 in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data_l);
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data_r);
  let joined = Sorted.join_exn data_l data_r ~on:[ ("B", "B", "B") ] in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular joined);
  assert (
    Sorted.plus_rows joined
    = Array.of_list
        [
          [ box_int 2; box_string "12"; box_string "data" ];
          [ box_int 3; box_string "this"; box_string "in" ];
          [ box_int 5; box_string "abc"; box_string "other" ];
          [ box_int 5; box_string "abc"; box_string "table" ];
          [ box_int 5; box_string "abc"; box_string "this" ];
          [ box_int 14; box_string "9012"; box_string "exists" ];
        ])

let test_join_neg_1 test_ctx =
  let data_l = test_data_6 in
  let data_r = test_data_7 in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data_l);
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular data_r);
  let joined = Sorted.join_exn data_l data_r ~on:[ ("B", "B", "B") ] in
  LensTestHelpers.print_verbose test_ctx
    (Format.asprintf "%a" Sorted.pp_tabular joined);
  assert (
    Sorted.plus_rows joined
    = Array.of_list [ [ box_int 2; box_string "12"; box_string "data" ] ]);
  assert (
    Sorted.neg_rows joined
    = Array.of_list
        [
          [ box_int 3; box_string "this"; box_string "in" ];
          [ box_int 5; box_string "abc"; box_string "other" ];
          [ box_int 14; box_string "9012"; box_string "exists" ];
        ])

let test_relational_update_1 test_ctx =
  let data = test_data_4 in
  let update_with = test_data_6 in
  let fun_deps = LensTestHelpers.fundepset_of_string "B -> A" in
  let updated = Sorted.relational_update data ~update_with ~fun_deps in
  print_verbose_sorted test_ctx updated

let test_relational_merge_1 test_ctx =
  let data = test_data_4 in
  let update_with = test_data_6 in
  let fun_deps = LensTestHelpers.fundepset_of_string "B -> A" in
  let updated = Sorted.relational_merge data ~update_with ~fun_deps in
  print_verbose_sorted test_ctx updated

let suite =
  "lens_set_operations"
  >::: [
         "construct_set" >:: test_construct_set;
         "compare" >:: test_compare;
         "find_set" >:: test_find_set;
         "project" >:: test_project;
         "minus" >:: test_minus;
         "merge" >:: test_merge;
         "reorder_cols" >:: test_reorder_cols;
         "find_all_ind_1" >:: test_find_all_ind_1;
         "find_all_ind_2" >:: test_find_all_ind_2;
         "find_all_ind_3" >:: test_find_all_ind_3;
         "find_all_1" >:: test_find_all_1;
         "join_1" >:: test_join_1;
         "join_neg_1" >:: test_join_neg_1;
         "relational_update_1" >:: test_relational_update_1;
         "relational_merge_1" >:: test_relational_merge_1;
       ]
