open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Test that DOS newlines are handled" =
  run_file {|./tests/dos-newlines.links|};
  [%expect {|
    43 : Int
    exit: 0 |}]

let%expect_test "Test precedence parsing (float plus times)" =
  run_expr {|42.0 +. 1.0 *. 42.0 == 42.0 +. (1.0 *. 42.0)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

