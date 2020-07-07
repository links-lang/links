open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Correct message type sent to a process" =
  run_expr {|spawn { recv() + 1 } ! 1|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Incorrect message type sent to a process is a type error" =
  run_expr {|spawn { { recv() + 1} } ! "two"|};
  [%expect {|
    <string>:1: Type error: The infix operator
        `!'
    has type
        `(Process ({ hear:Int|a }), Int) ~b~> ()'
    while the arguments passed to it have types
        `Process ({ |hear:Int|a })'
    and
        `String'
    and the currently allowed effects are
        `wild'
    In expression: spawn { { recv() + 1} } ! "two".

    exit: 1 |}]

let%expect_test "Receive types must unify (correct, closed rows)" =
  run_expr {|fun f() { receive { case Bar -> () }} fun g() { receive { case Bar -> () }} fun () { f(); g() }|};
  [%expect {|
    fun : () {:[|Bar|]|_}~> ()
    exit: 0 |}]

let%expect_test "Receive types must unify (incorrect, closed rows)" =
  run_expr {|fun f() { receive { case Bar -> () }} fun g() { receive { case Foo -> () }} fun () { f(); g() }|};
  [%expect {|
    <string>:1: Type error: The function
        `g'
    has type
        `() {:[|Foo|]|a}~> ()'
    while the arguments passed to it have types

    and the currently allowed effects are
        `|hear:[|Bar|],wild|b'
    In expression: g().

    exit: 1 |}]

let%expect_test "Receive types must unify (correct, open rows)" =
  run_expr {|fun f() { receive { case Bar -> () case x -> () }} fun g() { receive { case Foo -> () case x -> () }} fun () { f(); g() }|};
  [%expect {|
    fun : () {:[|Bar|Foo|_|]|_}~> ()
    exit: 0 |}]

let%expect_test "Receive types must unify (incorrect, open rows)" =
  run_expr {|fun f() { receive { case Bar (x) -> x+1 case x -> 0 }} fun g() { receive { case Bar (s) -> s+.1.0 case x -> 0.0 }} fun () { f(); g() }|};
  [%expect {|
    <string>:1: Type error: Side-effect expressions must have type `()', but the expression
        `f()'
    has type
        `Int'
    In expression: f();.

    exit: 1 |}]

let%expect_test "Basic send/receive test." =
  run_expr {|fun main() { spawnWait { var p = spawn { recv() ! "The end" } ! self(); recv() } } main()|};
  [%expect {|
    "The end" : String
    exit: 0 |}]

let%expect_test "Mailboxes are not polymorphic [1]" =
  run_expr {|fun main() { spawnWait { var p = spawn { recv() ! "The end" } ! self(); recv() } } main()|};
  [%expect {|
    "The end" : String
    exit: 0 |}]

let%expect_test "Mailboxes are not polymorphic [2]" =
  run_expr {|var pid = spawn { recv() ++ [] }; { pid ! "one"; pid ! [2] }|};
  [%expect {|
    <string>:1: Type error: The infix operator
        `!'
    has type
        `(Process ({ hear:[a::(Unl,Mono)]|b::(Unl,Mono) }), [a::(Unl,Mono)]) ~c~> ()'
    while the arguments passed to it have types
        `Process ({ hear:[a::(Unl,Mono)]|b::(Unl,Mono) })'
    and
        `String'
    and the currently allowed effects are
        `wild'
    In expression: pid ! "one".

    exit: 1 |}]

let%expect_test "Built-in functions are polymorphic in their mailbox types" =
  run_expr {|fun f() {var x = recv(); intToString(x)} fun g(x) {var () = recv(); intToString(x)} (f, g)|};
  [%expect {|
    (fun, fun) : (() {:Int|_}~> String, (Int) {:()|_}~> String)
    exit: 0 |}]

let%expect_test "SpawnWait works on the server" =
  run_expr {|fun go() { var p1 = spawn { receive { case Hi(pid) -> pid ! 100 } }; spawnWait { p1 ! Hi(self()) ; receive { case n -> n } } } go()|};
  [%expect {|
    100 : Int
    exit: 0 |}]

