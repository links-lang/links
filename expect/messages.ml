open Test_common
open Expect_test_common.File.Location


let%expect_test "Correct message type sent to a process" =
  run_expr {|spawn { recv() + 1 } ! 1|}

let%expect_test "Incorrect message type sent to a process is a type error" =
  run_expr {|spawn { { recv() + 1} } ! "two"|}

let%expect_test "Receive types must unify (correct, closed rows)" =
  run_expr {|fun f() { receive { case Bar -> () }} fun g() { receive { case Bar -> () }} fun () { f(); g() }|}

let%expect_test "Receive types must unify (incorrect, closed rows)" =
  run_expr {|fun f() { receive { case Bar -> () }} fun g() { receive { case Foo -> () }} fun () { f(); g() }|}

let%expect_test "Receive types must unify (correct, open rows)" =
  run_expr {|fun f() { receive { case Bar -> () case x -> () }} fun g() { receive { case Foo -> () case x -> () }} fun () { f(); g() }|}

let%expect_test "Receive types must unify (incorrect, open rows)" =
  run_expr {|fun f() { receive { case Bar (x) -> x+1 case x -> 0 }} fun g() { receive { case Bar (s) -> s+.1.0 case x -> 0.0 }} fun () { f(); g() }|}

let%expect_test "Basic send/receive test." =
  run_expr {|fun main() { spawnWait { var p = spawn { recv() ! "The end" } ! self(); recv() } } main()|}

let%expect_test "Mailboxes are not polymorphic [1]" =
  run_expr {|fun main() { spawnWait { var p = spawn { recv() ! "The end" } ! self(); recv() } } main()|}

let%expect_test "Mailboxes are not polymorphic [2]" =
  run_expr {|var pid = spawn { recv() ++ [] }; { pid ! "one"; pid ! [2] }|}

let%expect_test "Built-in functions are polymorphic in their mailbox types" =
  run_expr {|fun f() {var x = recv(); intToString(x)} fun g(x) {var () = recv(); intToString(x)} (f, g)|}

