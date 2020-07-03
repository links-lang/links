open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "exp is the inverse of log" =
  run_expr {|exp(log(2.0))|};
  [%expect {|
    2.0 : Float

    exit: 0 |}]

let%expect_test "log is the inverse of exp" =
  run_expr {|log(exp(2.0))|};
  [%expect {|
    2.0 : Float

    exit: 0 |}]

