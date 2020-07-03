open Test_common
open Expect_test_common.File.Location


let%expect_test "Empty page" =
  run_expr {|addRoute("/", fun(_) { page <#></#> })|}

let%expect_test "Unhandled operation" =
  run_expr ~args:["--enable-handlers"] {|addRoute("/", fun(_) {do Fail})|}

