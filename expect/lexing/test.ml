open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Incomplete expression" =
  run_expr {|1 +|};
  [%expect {|
    <string>:1: Type error: The unary operator
        `+'
    has type
        `(Int, Int) -a-> Int'
    while the argument passed to it has type
        `Int'
    and the currently allowed effects are
        `wild'
    In expression: 1 +.

    exit: 1 |}]

let%expect_test "Incomplete start element" =
  run_expr {|<a|};
  [%expect {|
    ***: Parse error: <string>:1

      <a
        ^
    exit: 1 |}]

let%expect_test "Incomplete attribute" =
  run_expr {|<a b="|};
  [%expect {|
    ***: Parse error: <string>:1

      <a b="
            ^
    exit: 1 |}]

let%expect_test "Incomplete regexp (1)" =
  run_expr {|"abc" =~ /ab|};
  [%expect {|
    ***: Parse error: <string>:1

      "abc" =~ /ab
                  ^
    exit: 1 |}]

let%expect_test "Incomplete regexp (2)" =
  run_expr {|"abc" =~ s/ab/de|};
  [%expect {|
    ***: Parse error: <string>:1

      "abc" =~ s/ab/de
                      ^
    exit: 1 |}]

