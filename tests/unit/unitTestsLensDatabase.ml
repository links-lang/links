open OUnit2
open UnitTestsLensCommon
module H = LensTestHelpers

let test_create_table test_ctx =
  let db = H.get_db test_ctx in
  H.drop_if_exists test_ctx db "MyTable" ;
  H.create_table_easy test_ctx db "MyTable" "key1 key2 -> value1 value2 value3" ;
  let testData = [[1; 1; 1; 1; 1]; [1; 2; 3; 1; 2]; [3; 3; 3; 3; 3]] in
  let testData =
    H.box_int_record_list
      (H.colslist_of_string "key1 key2 value1 value2 value3")
      testData
  in
  H.insert_rows db "MyTable" testData ;
  H.drop_if_cleanup test_ctx db "MyTable"

let test_create_table_rand test_ctx =
  let db = H.get_db test_ctx in
  H.drop_if_exists test_ctx db "MyTableRand" ;
  H.create_table_easy test_ctx db "MyTableRand"
    "key1 key2 -> value1 value2 value3" ;
  let testData =
    H.gen_data [`Seq; `Constant 1; `RandTo 15; `Rand; `Rand] 100
  in
  let testData =
    H.box_int_record_list
      (H.colslist_of_string "key1 key2 value1 value2 value3")
      testData
  in
  let _ = H.insert_rows db "MyTableRand" testData in
  let _ = H.drop_if_cleanup test_ctx db "MyTableRand" in
  ()

let test_drop_create_and_populate test_ctx =
  let db = H.get_db test_ctx in
  let _lens =
    H.drop_create_populate_table test_ctx db "table_test" "a -> b c" "a b c"
      [`Seq; `RandTo 15; `Rand] 500
  in
  let _ = H.drop_if_cleanup test_ctx db "table1" in
  ()

let suite =
  "lens_database"
  >::: [ (* disable these tests due to adventure works dependency *)
         "create_table" >:: test_create_table
       ; "create_table_rand" >:: test_create_table_rand
       ; "drop_create_and_populate" >:: test_drop_create_and_populate ]
