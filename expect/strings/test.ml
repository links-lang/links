open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "head and tail" =
  run_expr {|(charAt("string", 0), strsub("string", 1, 5)) == ('s', "tring")|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "octal literals" =
  run_expr {|"L\151nks"|};
  [%expect {|
    "Links" : String
    exit: 0 |}]

let%expect_test "hex literals" =
  run_expr {|"L\x69nks"|};
  [%expect {|
    "Links" : String
    exit: 0 |}]

let%expect_test "concatenation" =
  run_expr {|"abc" ^^ "def"|};
  [%expect {|
    "abcdef" : String
    exit: 0 |}]

