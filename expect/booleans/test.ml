open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Boolean not(true)" =
  run_expr {|not(true)|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "Boolean not(false)" =
  run_expr {|not(false)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Predefined \"javascript\" value" =
  run_expr {|javascript|};
  [%expect {|
    false : Bool
    exit: 0 |}]

