open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Test that DOS newlines are handled" =
  run_file {|./tests/dos-newlines.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/dos-newlines.links |}]

let%expect_test "Test precedence parsing (float plus times)" =
  run_expr {|42.0 +. 1.0 *. 42.0 == 42.0 +. (1.0 *. 42.0)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

