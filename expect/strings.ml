open Test_common
open Expect_test_common.File.Location


let%expect_test "head and tail" =
  run_expr {|(charAt("string", 0), strsub("string", 1, 5)) == ('s', "tring")|}

let%expect_test "octal literals" =
  run_expr {|"L\151nks"|}

let%expect_test "hex literals" =
  run_expr {|"L\x69nks"|}

let%expect_test "concatenation" =
  run_expr {|"abc" ^^ "def"|}

