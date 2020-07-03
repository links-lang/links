open Test_common
open Expect_test_common.File.Location


let%expect_test "Truncating integer division" =
  run_expr {|2/3 + 1/3|}

let%expect_test "Incompatibility of float and int types" =
  run_expr {|1 + 2.0|}

let%expect_test "Incompatibility with everything else" =
  run_expr {|1 == "two"|}

let%expect_test "Conversions between float and int types" =
  run_expr {|intToFloat(3)|}

let%expect_test "Integer literals" =
  run_expr {|2003004005006007002|}

let%expect_test "Floating point literals" =
  run_expr {|3.14|}

let%expect_test "Unary negation [1]" =
  run_expr {|{var x = 3; -x}|}

let%expect_test "Unary negation [2]" =
  run_expr {|{var x = 21342; x == - -x}|}

let%expect_test "Prefix arithmetic operators" =
  run_expr {|{var plus = (+); plus(1, (+)(2,3))}|}

let%expect_test "Operator precedence [1]" =
  run_expr {|(2 + 3 * 4 == 14) && (2 * 3 + 4 == 10)|}

let%expect_test "Error on division by zero" =
  run_expr {|{var x = 10; x / (x - 10)}|}

let%expect_test "Equality and comparisons [integer comparison]" =
  run_expr {|({var x = 100000230 * 102300000; x < x + 1}) && ({var x = 1032452430 * 102300234234; x > x - 1})|}

let%expect_test "Equality and comparisons [integer equality with 64-bit overflow]" =
  run_expr {|({var x = 100000230 * 102300000; x == 10230023529000000}) && ({var x = 1032452430 * 102300234234; x == 4163033019059954732})|}

