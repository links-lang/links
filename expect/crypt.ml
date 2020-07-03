open Test_common
open Expect_test_common.File.Location


let%expect_test "Crypt accepts correct passwords" =
  run_expr {|verify("correcthorsebatterystaple", crypt("correcthorsebatterystaple"))|}

let%expect_test "Crypt rejects incorrect passwords" =
  run_expr {|verify("password", crypt("correcthorsebatterystaple"))|}

