open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Crypt accepts correct passwords" =
  run_expr {|verify("correcthorsebatterystaple", crypt("correcthorsebatterystaple"))|};
  [%expect {|
    true : Bool

    exit: 0 |}]

let%expect_test "Crypt rejects incorrect passwords" =
  run_expr {|verify("password", crypt("correcthorsebatterystaple"))|};
  [%expect {|
    false : Bool

    exit: 0 |}]

