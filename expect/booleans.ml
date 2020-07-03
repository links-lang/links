open Test_common
open Expect_test_common.File.Location


let%expect_test "Boolean not(true)" =
  run_expr {|not(true)|}

let%expect_test "Boolean not(false)" =
  run_expr {|not(false)|}

