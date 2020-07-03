open Test_common
open Expect_test_common.File.Location


let%expect_test "Conditional expressions" =
  run_expr {|if (true) "yes" else "no"|}

let%expect_test "Typing of the test" =
  run_expr {|if (3) "yes" else "no"|}

let%expect_test "Typing of both branches" =
  run_expr {|if (true) "yes" else 41|}

let%expect_test "Conditionals in polymorphic functions" =
  run_expr {|(fun (a, b, c) { if (a) [b] else [c] })(true, "three", "four")|}

let%expect_test "Logical operators" =
  run_expr {|true && (false || true)|}

