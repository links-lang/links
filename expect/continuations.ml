open Test_common
open Expect_test_common.File.Location


let%expect_test "Escape expressions" =
  run_expr {|escape e in {2+2}|}

let%expect_test "Nested escapes" =
  run_expr {|escape return in {1+(escape e in {2+2})}|}

let%expect_test "Invoking escapes" =
  run_expr {|escape return in {1+(escape e in {return(2+2)})}|}

let%expect_test "Continuation typing [1]" =
  run_expr {|escape e in {if (false) 1+e else 2}|}

let%expect_test "Continuation typing [2]" =
  run_expr {|escape e in { e(1) }|}

let%expect_test "continuation typing [3]" =
  run_expr {|{ escape y in { ("" == y(1), true == y(1)); 2 } }|}

