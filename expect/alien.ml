open Test_common
open Expect_test_common.File.Location


let%expect_test "Alien declaration below toplevel" =
  run_expr {|if (true) { alien javascript "foo.js" foo : () ~> (); foo() } else { () }|}

let%expect_test "Alien blocks below toplevel" =
  run_expr {|if (true) { alien javascript "foo.js" { foo : () ~> (); } foo() } else { () }|}

let%expect_test "Alien functions may be evaluated in the interpreter" =
  run_expr {|alien javascript "fun.js" f : () ~> (); id(f)|}

let%expect_test "Alien values may be evaluated in the interpreter" =
  run_expr {|alien javascript "val.js" x : (); id(x)|}

let%expect_test "Alien functions may not be applied in the interpreter" =
  run_expr {|alien javascript "fun.js" f : () ~> (); f()|}

let%expect_test "Alien binders cannot contain primes" =
  run_expr {|alien javascript "" f' : () -> ();|}

