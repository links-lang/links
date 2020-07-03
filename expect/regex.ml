open Test_common
open Expect_test_common.File.Location


let%expect_test "Range [1]" =
  run_expr {|"3" =~ /[0-9]/|}

let%expect_test "Range [2]" =
  run_expr {|"0" =~ /[0-9]/|}

let%expect_test "Range [3]" =
  run_expr {|"9" =~ /[0-9]/|}

let%expect_test "Range [4]" =
  run_expr {|"." =~ /[0-9]/|}

let%expect_test "Range [5]" =
  run_expr {|"p" =~ /[a-z]/|}

let%expect_test "Range [6]" =
  run_expr {|"p" =~ /[A-Z]/|}

let%expect_test "Escaping metacharacter" =
  run_expr {|"some .*string$\" ++?" =~ /some\ \.\*string\$\"\ \+\+\?/|}

let%expect_test "Star [1]" =
  run_expr {|"23r2r3" =~ /.*/|}

let%expect_test "Star [2]" =
  run_expr {|"" =~ /.*/|}

let%expect_test "Star [3]" =
  run_expr {|"abc" =~ /(abc)*/|}

let%expect_test "Star [4]" =
  run_expr {|"abcabc" =~ /(abc)*/|}

let%expect_test "Star [5]" =
  run_expr {|"" =~ /(abc)*/|}

let%expect_test "Star [6]" =
  run_expr {|"a" =~ /(abc)*/|}

let%expect_test "Star [7]" =
  run_expr {|"abca" =~ /(abc)*/|}

let%expect_test "Plus [1]" =
  run_expr {|"23r2r3" =~ /.+/|}

let%expect_test "Plus [2]" =
  run_expr {|"" =~ /.+/|}

let%expect_test "Plus [3]" =
  run_expr {|"abc" =~ /(abc)+/|}

let%expect_test "Plus [4]" =
  run_expr {|"abcabc" =~ /(abc)+/|}

let%expect_test "Plus [5]" =
  run_expr {|"" =~ /(abc)+/|}

let%expect_test "Plus [6]" =
  run_expr {|"a" =~ /(abc)+/|}

let%expect_test "Plus [7]" =
  run_expr {|"abca" =~ /(abc)+/|}

let%expect_test "Plus/grouping [1]" =
  run_expr {|"ABBB" =~ /AB+/|}

let%expect_test "Plus/grouping [2]" =
  run_expr {|"ABAB" =~ /AB+/|}

let%expect_test "Plus/grouping [3]" =
  run_expr {|"ABAB" =~ /((A)(B))+/|}

let%expect_test "Plus/grouping [4]" =
  run_expr {|"ABBB" =~ /((A)(B))+/|}

let%expect_test "Interpolation [1]" =
  run_expr {|var x = "a"; "aaa" =~ /{x}*/|}

let%expect_test "Interpolation [2]" =
  run_expr {|var x = "a"; "abc" =~ /{x}*/|}

