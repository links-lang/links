open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Test that prelude is loaded" =
  run_expr {|map|};
  [%expect {|
    fun : ((a) -b-> c, [a]) -b-> [c]
    exit: 0 |}]

let%expect_test "Test sysexit [0]" =
  run_expr {|fun () {print("before"); sysexit(0); print("after")}()|};
  [%expect {| beforeexit: 0 |}]

let%expect_test "Test sysexit [1]" =
  run_expr {|fun() {print("before"); sysexit(1); print("after")}()|};
  [%expect {| beforeexit: 1 |}]

