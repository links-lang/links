open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Empty page" =
  run_expr {|addRoute("/", fun(_) { page <#></#> })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Unhandled operation" =
  run_expr ~args:["--enable-handlers"] {|addRoute("/", fun(_) {do Fail})|};
  [%expect {|
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
    In expression: addRoute("/", fun(_) {do Fail}).

    exit: 1 |}]

let%expect_test "Handled operation" =
  run_expr ~args:["--enable-handlers"] {|addRoute("/", fun(_) { handle({do Fail}) { case Fail -> page <#>{stringToXml("Caught")}</#> case Return(_) -> page <#>{stringToXml("Success")}</#> } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

