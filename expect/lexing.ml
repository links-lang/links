open Test_common
open Expect_test_common.File.Location


let%expect_test "Incomplete expression" =
  run_expr {|1 +|}

let%expect_test "Incomplete start element" =
  run_expr {|<a|}

let%expect_test "Incomplete attribute" =
  run_expr {|<a b="|}

let%expect_test "Incomplete regexp (1)" =
  run_expr {|"abc" =~ /ab|}

