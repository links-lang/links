open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Alien declaration below toplevel" =
  run_expr {|if (true) { alien javascript "foo.js" foo : () ~> (); foo() } else { () }|};
  [%expect {|
    ***: Parse error: <string>:1

      if (true) { alien javascript "foo.js" foo : () ~> (); foo() } else { () }
                       ^
    exit: 1 |}]

let%expect_test "Alien blocks below toplevel" =
  run_expr {|if (true) { alien javascript "foo.js" { foo : () ~> (); } foo() } else { () }|};
  [%expect {|
    ***: Parse error: <string>:1

      if (true) { alien javascript "foo.js" { foo : () ~> (); } foo() } else { () }
                       ^
    exit: 1 |}]

let%expect_test "Alien functions may be evaluated in the interpreter" =
  run_expr {|alien javascript "fun.js" f : () ~> (); id(f)|};
  [%expect {|
    alien : () ~> ()
    exit: 0 |}]

let%expect_test "Alien values may be evaluated in the interpreter" =
  run_expr {|alien javascript "val.js" x : (); id(x)|};
  [%expect {|
    alien : ()
    exit: 0 |}]

let%expect_test "Alien functions may not be applied in the interpreter" =
  run_expr {|alien javascript "fun.js" f : () ~> (); f()|};
  [%expect {|
    ***: Error: Links_core.Evalir.Exceptions.EvaluationError("Cannot make alien call on the server.")
    exit: 1 |}]

let%expect_test "Alien binders cannot contain primes" =
  run_expr {|alien javascript "" f' : () -> ();|};
  [%expect {|
    <string>:1: Syntax error: Foreign binders cannot contain single quotes `'`.
    In expression: alien javascript "" f' : () -> ();.
    exit: 1 |}]

let%expect_test "Alien type variables can contain primes" =
  run_expr {|alien javascript "" f : (a', b') -> c';|};
  [%expect {|
    () : ()
    exit: 0 |}]

