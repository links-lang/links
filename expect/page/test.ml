open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Empty page" =
  run_expr {|addRoute("/", fun(_) { page <#></#> })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Unhandled operation" =
  run_expr ~args:["--enable-handlers"] {|addRoute("/", fun(_) {do Fail})|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `addRoute'
    has type
        `(String, (String) {hear{a}}~> Page) ~b~> ()'
    while the arguments passed to it have types
        `String'
    and
        `(String) {|Fail:() {}-> c|d}-> c'
    and the currently allowed effects are
        `wild'
    In expression: addRoute("/", fun(_) {do Fail}). |}]

