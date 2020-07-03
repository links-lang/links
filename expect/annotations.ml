open Test_common
open Expect_test_common.File.Location


let%expect_test "Type annotation that matches inference" =
  run_expr {|fun (x) {x} : (a) -> a|}

let%expect_test "More-specific type annotation with typevars" =
  run_expr {|fun (x) {x} : ((a) -b-> a) -> ((a) -b-> a)|}

let%expect_test "Too-general type annotation" =
  run_expr {|fun (x) {x+1} : (a) -> a|}

let%expect_test "Annotations inside functions [1]" =
  run_expr {|fun (x:a) { x:a } : (a) -> a|}

let%expect_test "Annotations inside functions [2]" =
  run_expr {|fun (x:a) { error("boo") } : (a) ~> b|}

let%expect_test "Inferred kind" =
  run_expr {|fun (x : a) {x : a :: Base}|}

let%expect_test "Kind mismatch [1]" =
  run_expr {|sig f(x) : (a) ~a~> a fun f(x) {f(x)}|}

let%expect_test "Kind mismatch [2]" =
  run_expr {|fun (x : a :: Any) {x : a :: Base}|}

let%expect_test "Close recursive patterns (issue #360)" =
  run_expr {|switch (Var(0)) { case (_ : (mu a . [|Lam:(Int, a)|Var:Int|])) -> 42 }|}

let%expect_test "Unsafe type annotations on non-recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { print(x) } f|}

let%expect_test "Unsafe type annotations on recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { f(x) } f|}

let%expect_test "Unsafe type annotations on mutually recursive functions" =
  run_expr {|mutual { unsafe sig even : (Int) -> Bool fun even(x) { x == 0 || odd(x -1) } unsafe sig odd : (Int) -> Bool fun odd(x) { x <> 0 && even(x - 1) } } even|}

let%expect_test "Invalid unsafe type annotations on non-recursive functions" =
  run_expr {|unsafe sig f : (Int) -> () fun f(x) { print(x) } f|}

let%expect_test "Invalid unsafe type annotations on recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { f(x + 1) } f|}

let%expect_test "Invalid unsafe type annotations on mutually recursive functions" =
  run_expr {|mutual { unsafe sig even : (String) -> Bool fun even(x) { x == 0 || odd(x -1) } unsafe sig odd : (Int) -> Bool fun odd(x) { x <> 0 && even(x - 1) } } even|}

