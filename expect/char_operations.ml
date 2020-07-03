open Test_common
open Expect_test_common.File.Location


let%expect_test "isBlank [1]" =
  run_expr {|isBlank(' ')|}

let%expect_test "isBlank [2]" =
  run_expr {|isBlank('\t')|}

let%expect_test "isBlank [3]" =
  run_expr {|isBlank('\n')|}

let%expect_test "isBlank [4]" =
  run_expr {|isBlank('$')|}

let%expect_test "isBlank [5]" =
  run_expr {|isBlank('M')|}

let%expect_test "isLower [1]" =
  run_expr {|isLower('a')|}

let%expect_test "isLower [2]" =
  run_expr {|isLower('z')|}

let%expect_test "isLower [3]" =
  run_expr {|isLower('j')|}

let%expect_test "isLower [4]" =
  run_expr {|isLower('A')|}

let%expect_test "isLower [5]" =
  run_expr {|isLower('{')|}

let%expect_test "isLower [7]" =
  run_expr {|isLower('`')|}

let%expect_test "isUpper [1]" =
  run_expr {|isUpper('A')|}

let%expect_test "isUpper [2]" =
  run_expr {|isUpper('Z')|}

let%expect_test "isUpper [3]" =
  run_expr {|isUpper('J')|}

let%expect_test "isUpper [4]" =
  run_expr {|isUpper('a')|}

let%expect_test "isUpper [5]" =
  run_expr {|isUpper('[')|}

let%expect_test "isUpper [6]" =
  run_expr {|isUpper('@')|}

let%expect_test "isAlnum [1]" =
  run_expr {|isAlnum('a') && isAlnum('z') && isAlnum('A') && isAlnum('Z') && isAlnum('0') && isAlnum('1') && isAlnum('2') && isAlnum('3') && isAlnum('4') && isAlnum('5') && isAlnum('6') && isAlnum('7') && isAlnum('8') && isAlnum('9')|}

let%expect_test "isAlnum [2]" =
  run_expr {|isAlnum('/') || isAlnum('$') || isAlnum('*') || isAlnum('{')|}

let%expect_test "isXDigit [1]" =
  run_expr {|isXDigit('A') && isXDigit('B') && isXDigit('C') && isXDigit('D') && isXDigit('E') && isXDigit('F') && isXDigit('a') && isXDigit('b') && isXDigit('c') && isXDigit('d') && isXDigit('e') && isXDigit('f') && isXDigit('0') && isXDigit('1') && isXDigit('2') && isXDigit('3') && isXDigit('4') && isXDigit('5') && isXDigit('6') && isXDigit('7') && isXDigit('8') && isXDigit('9')|}

let%expect_test "isXDigit [2]" =
  run_expr {|isXDigit('/') || isXDigit('[')|}

let%expect_test "toUpper [1]" =
  run_expr {|toUpper('a')|}

let%expect_test "toUpper [2]" =
  run_expr {|toUpper('z')|}

let%expect_test "toUpper [3]" =
  run_expr {|toUpper('k')|}

let%expect_test "toUpper [4]" =
  run_expr {|toUpper('$')|}

let%expect_test "toUpper [5]" =
  run_expr {|toUpper('[')|}

let%expect_test "toUpper [6]" =
  run_expr {|toUpper('7')|}

let%expect_test "toLower [1]" =
  run_expr {|toLower('A')|}

let%expect_test "toLower [2]" =
  run_expr {|toLower('Z')|}

let%expect_test "toLower [3]" =
  run_expr {|toLower('O')|}

let%expect_test "toLower [4]" =
  run_expr {|toLower('$')|}

let%expect_test "toLower [5]" =
  run_expr {|toLower('{')|}

