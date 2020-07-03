open Test_common
open Expect_test_common.File.Location


let%expect_test "Empty argument list [1]" =
  run_expr {|getArgs()|}

let%expect_test "Empty argument list [2]" =
  run_expr ~args:["--"] {|getArgs()|}

let%expect_test "Singleton argument list [1]" =
  run_expr ~pargs:["42"] {|getArgs()|}

let%expect_test "Singleton argument list [2]" =
  run_expr ~pargs:["\"Hello"; "World!\""] {|getArgs()|}

let%expect_test "Multiple arguments [1]" =
  run_expr ~pargs:["foo"; "bar"; "42"] {|getArgs()|}

let%expect_test "Multiple arguments [2]" =
  run_expr ~pargs:["\"foo"; "bar\""; "42"; "\"quux"; "quasi\""] {|getArgs()|}

