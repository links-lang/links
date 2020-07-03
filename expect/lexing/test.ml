open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Incomplete expression" =
  run_expr {|1 +|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The unary operator
        `+'
    has type
        `(Int, Int) -a-> Int'
    while the argument passed to it has type
        `Int'
    and the currently allowed effects are
        `wild'
    In expression: 1 +. |}]

let%expect_test "Incomplete start element" =
  run_expr {|<a|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      <a
        ^ |}]

let%expect_test "Incomplete attribute" =
  run_expr {|<a b="|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      <a b="
            ^ |}]

let%expect_test "Incomplete regexp (1)" =
  run_expr {|"abc" =~ /ab|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      "abc" =~ /ab
                  ^ |}]

