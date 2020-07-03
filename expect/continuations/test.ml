open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Escape expressions" =
  run_expr {|escape e in {2+2}|};
  [%expect {|
    4 : Int

    exit: 0 |}]

let%expect_test "Nested escapes" =
  run_expr {|escape return in {1+(escape e in {2+2})}|};
  [%expect {|
    5 : Int

    exit: 0 |}]

let%expect_test "Invoking escapes" =
  run_expr {|escape return in {1+(escape e in {return(2+2)})}|};
  [%expect {|
    4 : Int

    exit: 0 |}]

let%expect_test "Continuation typing [1]" =
  run_expr {|escape e in {if (false) 1+e else 2}|};
  [%expect {|
    exit: 1<string>:1: Type error: The infix operator
        `+'
    has type
        `(Int, Int) -a-> Int'
    while the arguments passed to it have types
        `Int'
    and
        `(b::Any) ~c~> d::Any'
    and the currently allowed effects are
        `wild'
    In expression: 1+e. |}]

let%expect_test "Continuation typing [2]" =
  run_expr {|escape e in { e(1) }|};
  [%expect {|
    1 : Int

    exit: 0 |}]

let%expect_test "continuation typing [3]" =
  run_expr {|{ escape y in { ("" == y(1), true == y(1)); 2 } }|};
  [%expect {|
    exit: 1<string>:1: Type error: The infix operator
        `=='
    has type
        `(Bool, Bool) -a-> Bool'
    while the arguments passed to it have types
        `Bool'
    and
        `String'
    and the currently allowed effects are
        `wild'
    In expression: true == y(1). |}]

