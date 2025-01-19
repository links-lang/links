open OUnit2
open UnitTestsLensCommon
module H = LensTestHelpers

let test_create_table test_ctx =
  let module DB = (val TestUtility.Table.create test_ctx) in
  let module MyTable =
    (val DB.drop_create_easy ~table:"MyTable"
           "key1 key2 -> value1 value2 value3")
  in
  let testData = [ [ 1; 1; 1; 1; 1 ]; [ 1; 2; 3; 1; 2 ]; [ 3; 3; 3; 3; 3 ] ] in
  MyTable.insert_ints testData;
  assert_equal 3 (MyTable.count ());
  MyTable.drop ()

let test_create_table_rand test_ctx =
  let module DB = (val TestUtility.Table.create test_ctx) in
  let module MyTableRand =
    (val DB.drop_create_easy ~table:"MyTableRand"
           "key1 key2 -> value1 value2 value3")
  in
  let testData =
    H.gen_data [ `Seq; `Constant 1; `RandTo 15; `Rand; `Rand ] 100
  in
  MyTableRand.insert_ints testData;
  assert_equal 100 (MyTableRand.count ());
  MyTableRand.drop ()

let test_drop_create_and_populate test_ctx =
  let module DB = (val TestUtility.Table.create test_ctx) in
  let module TestTable =
    (val DB.drop_create_easy_populate ~table:"test_table" ~fd:"a -> b c" ~n:500
           [ `Seq; `RandTo 15; `Rand ])
  in
  assert_equal 500 (TestTable.count ());
  TestTable.drop ()

let suite =
  "database"
  >::: [
         (* disable these tests due to adventure works dependency *)
         "create_table" >:: test_create_table;
         "create_table_rand" >:: test_create_table_rand;
         "drop_create_and_populate" >:: test_drop_create_and_populate;
       ]
