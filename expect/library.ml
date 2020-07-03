open Test_common
open Expect_test_common.File.Location


let%expect_test "Test that prelude is loaded" =
  run_expr {|map|}

let%expect_test "Test sysexit [0]" =
  run_expr {|fun () {print("before"); sysexit(0); print("after")}()|}

let%expect_test "Test sysexit [1]" =
  run_expr {|fun() {print("before"); sysexit(1); print("after")}()|}

