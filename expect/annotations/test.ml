open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Type annotation that matches inference" =
  run_expr {|fun (x) {x} : (a) -> a|};
  [%expect {|
    fun : (a) -> a
    exit: 0 |}]

let%expect_test "More-specific type annotation with typevars" =
  run_expr {|fun (x) {x} : ((a) -b-> a) -> ((a) -b-> a)|};
  [%expect {|
    fun : ((a) -b-> a) -> (a) -b-> a
    exit: 0 |}]

let%expect_test "Too-general type annotation" =
  run_expr {|fun (x) {x+1} : (a) -> a|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The inferred type of the expression
        `fun (x) {x+1}'
    is
        `(Int) -a-> Int'
    but it is annotated with type
        `(b) -c-> b'
    In expression: fun (x) {x+1} : (a) -> a. |}]

let%expect_test "Annotations inside functions [1]" =
  run_expr {|fun (x:a) { x:a } : (a) -> a|};
  [%expect {|
    fun : (a) -> a
    exit: 0 |}]

let%expect_test "Annotations inside functions [2]" =
  run_expr {|fun (x:a) { error("boo") } : (a) ~> b|};
  [%expect {|
    fun : (_) ~> _
    exit: 0 |}]

let%expect_test "Inferred kind" =
  run_expr {|fun (x : a) {x : a :: Base}|};
  [%expect {|
    fun : (a::Base) -> a::Base
    exit: 0 |}]

let%expect_test "Kind mismatch [1]" =
  run_expr {|sig f(x) : (a) ~a~> a fun f(x) {f(x)}|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      sig f(x) : (a) ~a~> a fun f(x) {f(x)}
            ^ |}]

let%expect_test "Kind mismatch [2]" =
  run_expr {|fun (x : a :: Any) {x : a :: Base}|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Type(Unl,Base)' and `a::Type(Any,Any)'.
    In expression: a :: Base. |}]

let%expect_test "Close recursive patterns (issue #360)" =
  run_expr {|switch (Var(0)) { case (_ : (mu a . [|Lam:(Int, a)|Var:Int|])) -> 42 }|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Unsafe type annotations on non-recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { print(x) } f|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `print'
    has type
        `(String) ~a~> ()'
    while the arguments passed to it have types
        `String'
    and the currently allowed effects are
        `|b'
    In expression: print(x). |}]

let%expect_test "Unsafe type annotations on recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { f(x) } f|};
  [%expect {|
    fun : (String) -> ()
    exit: 0 |}]

let%expect_test "Unsafe type annotations on mutually recursive functions" =
  run_expr {|mutual { unsafe sig even : (Int) -> Bool fun even(x) { x == 0 || odd(x -1) } unsafe sig odd : (Int) -> Bool fun odd(x) { x <> 0 && even(x - 1) } } even|};
  [%expect {|
    fun : (Int) -> Bool
    exit: 0 |}]

let%expect_test "Invalid unsafe type annotations on non-recursive functions" =
  run_expr {|unsafe sig f : (Int) -> () fun f(x) { print(x) } f|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `print'
    has type
        `(String) ~a~> ()'
    while the arguments passed to it have types
        `Int'
    and the currently allowed effects are
        `|b'
    In expression: print(x). |}]

let%expect_test "Invalid unsafe type annotations on recursive functions" =
  run_expr {|unsafe sig f : (String) -> () fun f(x) { f(x + 1) } f|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The infix operator
        `+'
    has type
        `(Int, Int) -a-> Int'
    while the arguments passed to it have types
        `String'
    and
        `Int'
    and the currently allowed effects are
        `|b'
    In expression: x + 1. |}]

let%expect_test "Invalid unsafe type annotations on mutually recursive functions" =
  run_expr {|mutual { unsafe sig even : (String) -> Bool fun even(x) { x == 0 || odd(x -1) } unsafe sig odd : (Int) -> Bool fun odd(x) { x <> 0 && even(x - 1) } } even|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The infix operator
        `=='
    has type
        `(String, String) -a-> Bool'
    while the arguments passed to it have types
        `String'
    and
        `Int'
    and the currently allowed effects are
        `|b'
    In expression: x == 0. |}]

