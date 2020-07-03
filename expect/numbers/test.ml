open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Truncating integer division" =
  run_expr {|2/3 + 1/3|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Incompatibility of float and int types" =
  run_expr {|1 + 2.0|};
  [%expect {|
    <string>:1: Type error: The infix operator
        `+'
    has type
        `(Int, Int) -a-> Int'
    while the arguments passed to it have types
        `Int'
    and
        `Float'
    and the currently allowed effects are
        `wild'
    In expression: 1 + 2.0.

    exit: 1 |}]

let%expect_test "Incompatibility with everything else" =
  run_expr {|1 == "two"|};
  [%expect {|
    <string>:1: Type error: The infix operator
        `=='
    has type
        `(Int, Int) -a-> Bool'
    while the arguments passed to it have types
        `Int'
    and
        `String'
    and the currently allowed effects are
        `wild'
    In expression: 1 == "two".

    exit: 1 |}]

let%expect_test "Conversions between float and int types" =
  run_expr {|intToFloat(3)|};
  [%expect {|
    3.0 : Float
    exit: 0 |}]

let%expect_test "Integer literals" =
  run_expr {|2003004005006007002|};
  [%expect {|
    2003004005006007002 : Int
    exit: 0 |}]

let%expect_test "Floating point literals" =
  run_expr {|3.14|};
  [%expect {|
    3.14 : Float
    exit: 0 |}]

let%expect_test "Unary negation [1]" =
  run_expr {|{var x = 3; -x}|};
  [%expect {|
    -3 : Int
    exit: 0 |}]

let%expect_test "Unary negation [2]" =
  run_expr {|{var x = 21342; x == - -x}|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Prefix arithmetic operators" =
  run_expr {|{var plus = (+); plus(1, (+)(2,3))}|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Operator precedence [1]" =
  run_expr {|(2 + 3 * 4 == 14) && (2 * 3 + 4 == 10)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Error on division by zero" =
  run_expr {|{var x = 10; x / (x - 10)}|};
  [%expect {|
    ***: Error: Division_by_zero
    exit: 1 |}]

let%expect_test "Equality and comparisons [integer comparison]" =
  run_expr {|({var x = 100000230 * 102300000; x < x + 1}) && ({var x = 1032452430 * 102300234234; x > x - 1})|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Equality and comparisons [integer equality with 64-bit overflow]" =
  run_expr {|({var x = 100000230 * 102300000; x == 10230023529000000}) && ({var x = 1032452430 * 102300234234; x == 4163033019059954732})|};
  [%expect {|
    true : Bool
    exit: 0 |}]

