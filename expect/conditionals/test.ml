open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Conditional expressions" =
  run_expr {|if (true) "yes" else "no"|};
  [%expect {|
    "yes" : String
    exit: 0 |}]

let%expect_test "Typing of the test" =
  run_expr {|if (3) "yes" else "no"|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The condition of an `if (...) ... else ...' expression must have type `Bool', but the expression
        `3'
    has type
        `Int'
    In expression: if (3) "yes" else "no". |}]

let%expect_test "Typing of both branches" =
  run_expr {|if (true) "yes" else 41|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Both branches of an `if (...) ... else ...' expression should have the same type, but the expression
        `"yes"'
    has type
        `String'
    while the expression
        `41'
    has type
        `Int'
    In expression: if (true) "yes" else 41. |}]

let%expect_test "Conditionals in polymorphic functions" =
  run_expr {|(fun (a, b, c) { if (a) [b] else [c] })(true, "three", "four")|};
  [%expect {|
    ["three"] : [String]
    exit: 0 |}]

let%expect_test "Logical operators" =
  run_expr {|true && (false || true)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

