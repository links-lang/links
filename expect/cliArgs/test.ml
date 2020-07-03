open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Empty argument list [1]" =
  run_expr {|getArgs()|};
  [%expect {|
    [] : [String]
    exit: 0 |}]

let%expect_test "Empty argument list [2]" =
  run_expr {|getArgs()|};
  [%expect {|
    [] : [String]
    exit: 0 |}]

let%expect_test "Singleton argument list [1]" =
  run_expr {|getArgs()|} ~pargs:["42"];
  [%expect {|
    ["42"] : [String]
    exit: 0 |}]

let%expect_test "Singleton argument list [2]" =
  run_expr {|getArgs()|} ~pargs:["\"Hello"; "World!\""];
  [%expect {|
    [""Hello", "World!""] : [String]
    exit: 0 |}]

let%expect_test "Multiple arguments [1]" =
  run_expr {|getArgs()|} ~pargs:["foo"; "bar"; "42"];
  [%expect {|
    ["foo", "bar", "42"] : [String]
    exit: 0 |}]

let%expect_test "Multiple arguments [2]" =
  run_expr {|getArgs()|} ~pargs:["\"foo"; "bar\""; "42"; "\"quux"; "quasi\""];
  [%expect {|
    [""foo", "bar"", "42", ""quux", "quasi""] : [String]
    exit: 0 |}]

