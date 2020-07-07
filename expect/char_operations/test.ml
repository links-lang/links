open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "isBlank [1]" =
  run_expr {|isBlank(' ')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isBlank [2]" =
  run_expr {|isBlank('\t')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isBlank [3]" =
  run_expr {|isBlank('\n')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isBlank [4]" =
  run_expr {|isBlank('$')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isBlank [5]" =
  run_expr {|isBlank('M')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isLower [1]" =
  run_expr {|isLower('a')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isLower [2]" =
  run_expr {|isLower('z')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isLower [3]" =
  run_expr {|isLower('j')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isLower [4]" =
  run_expr {|isLower('A')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isLower [5]" =
  run_expr {|isLower('{')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isLower [7]" =
  run_expr {|isLower('`')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isUpper [1]" =
  run_expr {|isUpper('A')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isUpper [2]" =
  run_expr {|isUpper('Z')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isUpper [3]" =
  run_expr {|isUpper('J')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isUpper [4]" =
  run_expr {|isUpper('a')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isUpper [5]" =
  run_expr {|isUpper('[')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isUpper [6]" =
  run_expr {|isUpper('@')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isAlnum [1]" =
  run_expr {|isAlnum('a') && isAlnum('z') && isAlnum('A') && isAlnum('Z') && isAlnum('0') && isAlnum('1') && isAlnum('2') && isAlnum('3') && isAlnum('4') && isAlnum('5') && isAlnum('6') && isAlnum('7') && isAlnum('8') && isAlnum('9')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isAlnum [2]" =
  run_expr {|isAlnum('/') || isAlnum('$') || isAlnum('*') || isAlnum('{')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "isXDigit [1]" =
  run_expr {|isXDigit('A') && isXDigit('B') && isXDigit('C') && isXDigit('D') && isXDigit('E') && isXDigit('F') && isXDigit('a') && isXDigit('b') && isXDigit('c') && isXDigit('d') && isXDigit('e') && isXDigit('f') && isXDigit('0') && isXDigit('1') && isXDigit('2') && isXDigit('3') && isXDigit('4') && isXDigit('5') && isXDigit('6') && isXDigit('7') && isXDigit('8') && isXDigit('9')|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "isXDigit [2]" =
  run_expr {|isXDigit('/') || isXDigit('[')|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "toUpper [1]" =
  run_expr {|toUpper('a')|};
  [%expect {|
    'A' : Char
    exit: 0 |}]

let%expect_test "toUpper [2]" =
  run_expr {|toUpper('z')|};
  [%expect {|
    'Z' : Char
    exit: 0 |}]

let%expect_test "toUpper [3]" =
  run_expr {|toUpper('k')|};
  [%expect {|
    'K' : Char
    exit: 0 |}]

let%expect_test "toUpper [4]" =
  run_expr {|toUpper('$')|};
  [%expect {|
    '$' : Char
    exit: 0 |}]

let%expect_test "toUpper [5]" =
  run_expr {|toUpper('[')|};
  [%expect {|
    '[' : Char
    exit: 0 |}]

let%expect_test "toUpper [6]" =
  run_expr {|toUpper('7')|};
  [%expect {|
    '7' : Char
    exit: 0 |}]

let%expect_test "toLower [1]" =
  run_expr {|toLower('A')|};
  [%expect {|
    'a' : Char
    exit: 0 |}]

let%expect_test "toLower [2]" =
  run_expr {|toLower('Z')|};
  [%expect {|
    'z' : Char
    exit: 0 |}]

let%expect_test "toLower [3]" =
  run_expr {|toLower('O')|};
  [%expect {|
    'o' : Char
    exit: 0 |}]

let%expect_test "toLower [4]" =
  run_expr {|toLower('$')|};
  [%expect {|
    '$' : Char
    exit: 0 |}]

let%expect_test "toLower [5]" =
  run_expr {|toLower('{')|};
  [%expect {|
    '{' : Char
    exit: 0 |}]

let%expect_test "toLower [6]" =
  run_expr {|toLower('5')|};
  [%expect {|
    '5' : Char
    exit: 0 |}]

