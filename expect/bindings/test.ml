open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Lexical scoping at top-level" =
  run_expr {|var x = 3; fun y(a) { x + a } var x = 4; y(10)|};
  [%expect {|
    13 : Int
    exit: 0 |}]

let%expect_test "Nested scopes" =
  run_expr {|{ var x = 3; ({ var x = 4; x }, x)}|};
  [%expect {|
    (4, 3) : (Int, Int)
    exit: 0 |}]

let%expect_test "Non-recursive top-level functions:" =
  run_expr {|var f = fun(x) { f(x) }|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      var f = fun(x) { f(x) }
                             ^ |}]

let%expect_test "Non-recursive block-scope functions:" =
  run_expr {|{ var f = fun(x) { f(x) }; () }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unknown variable f.
    In expression: f. |}]

let%expect_test "Mutually recursive top-level functions" =
  run_expr {|mutual { fun evn(n) { n == 0 || od(n - 1) } fun od(n) { evn(n) == false } } evn(20)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Mutually recursive nested functions" =
  run_expr {|{ mutual{ fun even(n) { n == 0 || odd(n - 1) } fun odd(n) { even(n) == false } } even(20) }|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Mutually recursive nested functions returned outside their scope" =
  run_expr {|{fun f() { mutual { fun even(n) { n == 0 || odd(n - 1) } fun odd(n) { even(n) == false } } even } f()(7) }|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "Closures using anonymous functions" =
  run_expr {|fun addn(n) { fun(x) { x + n } } addn(3)(4)|};
  [%expect {|
    7 : Int
    exit: 0 |}]

let%expect_test "Closures using named functions" =
  run_expr {|fun addn(n) { fun f(x) { x + n } f } addn(3)(4)|};
  [%expect {|
    7 : Int
    exit: 0 |}]

let%expect_test "Closures where the environment contains a closure from a different scope" =
  run_expr {|fun add(x,y){x+y} fun baz(z, w) {z + w} fun foo(f, x) { fun bar(y) { f(3, y) } bar(x) } foo(add,4)|};
  [%expect {|
    7 : Int
    exit: 0 |}]

let%expect_test "No value recursion" =
  run_expr {|fun f() { g() } var x = f(); fun g() { x }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unknown variable g.
    In expression: g. |}]

let%expect_test "as patterns" =
  run_expr {|{var x::xs as y = [1,2,3]; y}|};
  [%expect {|
    [1, 2, 3] : [Int]
    exit: 0 |}]

let%expect_test "Reject multiple occurrences of a name in a pattern [1]" =
  run_expr {|fun (x,x) { x }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Duplicate names are not allowed in patterns.
    In expression: fun (x,x) { x }. |}]

let%expect_test "Reject multiple occurrences of a name in a pattern [2]" =
  run_expr {|fun () { var (x,x) = (1,2); x }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Duplicate names are not allowed in patterns.
    In expression: (x,x). |}]

let%expect_test "Reject multiple occurrences of a name in a pattern [3]" =
  run_expr {|fun () { var (a=x,b=x) = (a=1,b=2); x }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Duplicate names are not allowed in patterns.
    In expression: (a=x,b=x). |}]

let%expect_test "Check that recursive bindings don't destroy the local environments of values in the local environment (see bug report from Thierry, 2006-09-17 on links-users)" =
  run_expr {|fun (z) { fun s() {} z()}(fun (aa)() { aa(()) }(fun (x){x}))|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Bug in interaction between pattern-matching and anonymous functions" =
  run_expr {|(fun (l) { switch (l) { case x::xs -> fun (x) { x } } })([1])(2)|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Variable names may contain primes (1)" =
  run_expr {|var x' = 42; x'|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Variable names may contain primes (2)" =
  run_expr {|var x'''y = 128; x'''y|};
  [%expect {|
    128 : Int
    exit: 0 |}]

let%expect_test "Variable names may contain primes (3)" =
  run_expr {|var 'x = 42;|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : '
      var 'x = 42;
           ^ |}]

let%expect_test "Function names may contain primes (1)" =
  run_expr {|fun f'() { 42 } f'()|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Function names may contain primes (2)" =
  run_expr {|sig f'''y : (a') -> a' fun f'''y(x) { x } f'''y(128)|};
  [%expect {|
    128 : Int
    exit: 0 |}]

let%expect_test "Function names may contain primes (3)" =
  run_expr {|fun 'f() { () }|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : '
      fun 'f() { () }
           ^ |}]

let%expect_test "Function names may contain primes (4)" =
  run_expr {|fun f'(x) { x } f'("f'")|};
  [%expect {|
    "f'" : String
    exit: 0 |}]

