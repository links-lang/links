open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Lists (Correct)" =
  run_file {|./tests/mutual/list.links|};
  [%expect {|
    Cons(1, Cons(2, Cons(3, Nil))) : List (Int)
    exit: 0 |}]

let%expect_test "Lists (Type argument mismatch)" =
  run_file {|./tests/mutual/listWrong.links|};
  [%expect {|
    ./tests/mutual/listWrong.links:4: Type error: The non-recursive function definition has return type
        `[|Cons:(Int, [|Cons:(Int, [|Cons:(Int, [|Nil|a::Any|])|b::Any|])|c::Any|])|d::Any|]'
    but its annotation has return type
        `List (String)'
    In expression: fun example() {
        Cons(1, Cons(2, Cons(3, Nil)))
    }.

    exit: 1 |}]

let%expect_test "Lists (Map)" =
  run_file {|./tests/mutual/listMap.links|};
  [%expect {|
    Cons(2, Cons(3, Cons(4, Nil))) : List (Int)
    exit: 0 |}]

let%expect_test "Odd and even numbers (1)" =
  run_file {|./tests/mutual/oddEven.links|};
  [%expect {|
    SuccO(SuccE(SuccO(Z))) : Odd
    exit: 0 |}]

let%expect_test "Odd and even numbers (2)" =
  run_file {|./tests/mutual/oddOrEven.links|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "Only functions and typenames in mutual blocks" =
  run_file {|./tests/mutual/badMutualBinding.links|};
  [%expect {|
    ***: Parse error: ./tests/mutual/badMutualBinding.links:4
    Only `fun` and `typename` bindings are allowed in a `mutual` block.
        var bobsleigh = 5;
                          ^
    exit: 1 |}]

let%expect_test "Unguarded recursive applications disallowed (1)" =
  run_file {|./tests/mutual/unguarded1.links|};
  [%expect {|
    ./tests/mutual/unguarded1.links:4: Type error: The value has type
        `Int'
    but it is annotated with type
        `Unguarded'
    In expression: var bar = 5.

    exit: 1 |}]

let%expect_test "Unguarded recursive applications disallowed (2)" =
  run_file {|./tests/mutual/unguarded2.links|};
  [%expect {|
    ./tests/mutual/unguarded2.links:9: Type error: The value has type
        `Int'
    but it is annotated with type
        `Unguarded'
    In expression: var bar = 5.

    exit: 1 |}]

let%expect_test "Type variables not shared in a mutual block" =
  run_file {|./tests/mutual/tyvarSharingDisallowed.links|};
  [%expect {|
    ./tests/mutual/tyvarSharingDisallowed.links:3: Type error: Unbound type variable `a' in position where
            no free type variables are allowed
    In expression: a.

    exit: 1 |}]

let%expect_test "Linearity (1)" =
  run_file {|./tests/mutual/linearity1.links|};
  [%expect {|
    ./tests/mutual/linearity1.links:9: Type error: The function
        `ignore'
    has type
        `(a) -b-> ()'
    while the arguments passed to it have types
        `Bar'
    and the currently allowed effects are
        `wild'
    In expression: ignore(bobsleigh).

    exit: 1 |}]

let%expect_test "Linearity (2)" =
  run_file {|./tests/mutual/linearity2.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Linearity (3)" =
  run_file {|./tests/mutual/linearity3.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Duplicate type bindings disallowed" =
  run_file {|./tests/mutual/duplicateType.links|};
  [%expect {|
    ***: Error: Duplicate mutually-defined bindings
     Foo:
      File ./tests/mutual/duplicateType.links, line 3, columns 33 to 54
      File ./tests/mutual/duplicateType.links, line 2, columns 11 to 29
    exit: 1 |}]

let%expect_test "Duplicate function bindings disallowed" =
  run_file {|./tests/mutual/duplicateFun.links|};
  [%expect {|
    ***: Error: Duplicate mutually-defined bindings
     foo:
      File ./tests/mutual/duplicateFun.links, line 3, columns 30 to 46
      File ./tests/mutual/duplicateFun.links, line 2, columns 11 to 27
    exit: 1 |}]

let%expect_test "Use structural unification if nominal unification fails" =
  run_file {|./tests/mutual/structural.links|};
  [%expect {|
    123 : Int
    exit: 0 |}]

let%expect_test "Use structural unification if nominal unification fails (type error)" =
  run_file {|./tests/mutual/structural2.links|};
  [%expect {|
    ./tests/mutual/structural2.links:16: Type error: The function
        `test'
    has type
        `(T2 (Int)) -a-> Int'
    while the arguments passed to it have types
        `T1 (Int)'
    and the currently allowed effects are
        `wild'
    In expression: test(nil).

    exit: 1 |}]

let%expect_test "Top level functions within a mutal do not share type variables" =
  run_file {|./tests/mutual/sharedTyvar.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

