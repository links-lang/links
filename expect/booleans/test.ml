open Links_expect.Test_common
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

