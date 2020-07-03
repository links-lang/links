open Test_common
open Expect_test_common.File.Location


let%expect_test "Function typing bug (see jdy's blog, 2005-10-24)" =
  run_expr {|(fun (x,y) { [x] ++ [y] }) ([1],"a")|}

let%expect_test "Type annotations on functions" =
  run_expr {|fun (x) { x } : (String) -> String|}

let%expect_test "Incorrect type annotations rejected" =
  run_expr {|fun (x) { x + 1 } : (Float) -> String|}

let%expect_test "Loose type annotations on functions" =
  run_expr {|fun (x) { x } : (b) -> b|}

let%expect_test "Trailing semicolon means \"ignore the final value\" [1]" =
  run_expr {|{ 2 }|}

let%expect_test "Trailing semicolon means \"ignore the final value\" [2]" =
  run_expr {|{ 2; }|}

let%expect_test "Trailing semicolon means \"ignore the final value\" [3]" =
  run_expr {|fun () { 2 }|}

let%expect_test "Trailing semicolon means \"ignore the final value\" [4]" =
  run_expr {|fun () { 2; }|}

let%expect_test "Type annotations" =
  run_expr {|fun (x:Int) {x:Int}|}

let%expect_test "Identity annotation" =
  run_expr {|fun (x:a) {x:a}|}

let%expect_test "Type annotation scope" =
  run_expr {|fun (x:a, y:a) {(x, y)}|}

let%expect_test "Negative recursive type" =
  run_expr {|fun (x) {x:a} : a|}

let%expect_test "Infers effect variables" =
  run_expr {|fun (f, x: Int) { f(x) + 0 }|}

let%expect_test "Typename [1]" =
  run_expr {|typename Foo = Int; fun f(x:Foo) {x} f(1)|}

let%expect_test "Typename [2]" =
  run_expr {|typename Bar(a,b,c) = (a,b,c); fun f(x:Bar(a,b,c)) {x} f((false, 1, "two"))|}

let%expect_test "Typename [3]" =
  run_expr {|typename F(a,b) = (a) {:b}~> a; sig f : F(Int,Int) fun f(x) {recv() + x} sig g : F(Int,String) fun g(x) {stringToInt(recv()) + x} g|}

let%expect_test "Nested closures" =
  run_file {|./tests/functions/nested-closures.links|}

let%expect_test "Quantification of alien functions (#280)" =
  run_file {|./tests/functions/alien-quantification.links|}

let%expect_test "Type annotation on inner function (correct, basic)" =
  run_file {|./tests/functions/innerfun1.links|}

let%expect_test "Type annotation on inner function (incorrect, basic)" =
  run_file {|./tests/functions/innerfun2.links|}

let%expect_test "Type annotation on inner function (correct, recursive)" =
  run_file {|./tests/functions/innerfun3.links|}

let%expect_test "Closure conversion: Test generalization of free type variables during hoisting" =
  run_file {|./tests/functions/nested-functions-polymorphic.links|}

let%expect_test "Closure conversion: Test function with free type variables, but no free term variables" =
  run_file {|./tests/functions/closure-conv-type-abstr-only.links|}

let%expect_test "Quantifiers should not escape their scopes (#687)" =
  run_file {|./tests/functions/escaped-quantifier.links|}

let%expect_test "Linearity (1)" =
  run_expr {|fun (x, y) {(x, y)}|}

let%expect_test "Linearity (2)" =
  run_expr {|fun (x) {fun (y) {(x, y)}}|}

let%expect_test "Linearity (3) (#795)" =
  run_expr {|fun (x)(y) {(x, y)}|}

let%expect_test "Linearity (4)" =
  run_expr {|linfun (x){ linfun (y) {(x, y)}}|}

let%expect_test "Linearity (5)" =
  run_expr {|linfun (x)(y) {(x, y)}|}

let%expect_test "Linearity (6) (#797)" =
  run_expr {|fun f(x)(y) {(x, y)} f|}

let%expect_test "Linearity (7) (#797)" =
  run_expr {|fun f(x)(y) {f(x)(y)} f|}

let%expect_test "Linearity (8) (#797)" =
  run_expr {|linfun f(x)(y) {(x, y)} f|}

