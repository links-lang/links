open Test_common
open Expect_test_common.File.Location


let%expect_test "Access points" =
  run_expr {|fun f(ap) {send(42, request(ap))} f|}

let%expect_test "Linear function annotation" =
  run_expr {|sig h : ((a::Any) -e-@ a::Any, a::Any) -e-> a::Any fun h(f, x) {f(x)}|}

let%expect_test "Non-linear use of linear function" =
  run_expr {|sig h : ((a::Any) -e-@ a::Any, a::Any) -e-> a::Any fun h(f, x) {f(f(x))}|}

let%expect_test "Linear identity" =
  run_expr {|fun (x) {x}|}

let%expect_test "Non-linear dup" =
  run_expr {|fun (x) {(x, x)}|}

let%expect_test "Receive value" =
  run_expr {|fun (c) {receive(c).1}|}

let%expect_test "Receive channel" =
  run_expr {|fun (c) {receive(c).2}|}

let%expect_test "Ignore send" =
  run_expr {|fun (c) {ignore(send(42, c))}|}

let%expect_test "Linear end" =
  run_expr {|ignore(request((new(): AP(End))))|}

let%expect_test "Non-linear generalisation (1)" =
  run_expr {|{var x = A; ()}|}

