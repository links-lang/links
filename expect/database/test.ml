open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Orderby clause (not a semantic test, just syntax)" =
  run_expr {|for (x <- [1, 2, 3]) orderby (x) [x]|};
  [%expect {|
    [1, 2, 3] : [Int]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (1)" =
  run_expr {|query { for (x <- [(<a>asdf</a>)]) [(b=2)] }|};
  [%expect {|
    [(b = 2)] : [(b:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (2)" =
  run_expr {|query { for (x <- (<a>asdf</a>)) [(b=2)] }|};
  [%expect {|
    [(b = 2)] : [(b:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (3)" =
  run_expr {|query { for (x <- (<a>asdf</a>)) [(b=2)] }|};
  [%expect {|
    [(b = 2)] : [(b:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (4)" =
  run_expr {|query {var x = <a>asdf</a>; [(a=1)]}|};
  [%expect {|
    [(a = 1)] : [(a:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (5)" =
  run_expr {|query {var x = <a href="foo.com">asdf</a>; [(a=1)]}|};
  [%expect {|
    [(a = 1)] : [(a:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (6)" =
  run_expr {|query {var x = <a href="foo.com">{stringToXml("asdf")}</a>; [(a=1)]}|};
  [%expect {|
    [(a = 1)] : [(a:Int)]
    exit: 0 |}]

let%expect_test "XML literals in query blocks (6)" =
  run_expr {|query {var x = <a {[("href", "foo.com")]}>{stringToXml("asdf")}</a>; [(a=1)]}|};
  [%expect {|
    [(a = 1)] : [(a:Int)]
    exit: 0 |}]

let%expect_test "Explicit query evaluator annotation (1)" =
  run_expr {|query nested { for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }|};
  [%expect {|
    [(x = 1, y = [4, 5, 6]), (x = 2, y = [4, 5, 6]), (x = 3, y = [4, 5, 6])] : [(x:Int,y:[Int])]
    exit: 0 |}]

let%expect_test "Explicit query evaluator annotation (2)" =
  run_expr ~args:["--config=tests/shredding/config.sample"] {|query flat { for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }|};
  [%expect {|
    <string>:1: Type error: Flat query blocks must return a list of records of base type, but the expression
        `{ for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }'
    has type
        `[(x:Int,y:[Int])]'
    In expression: query flat { for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }.

    exit: 1 |}]

let%expect_test "Explicit query evaluator annotation (3)" =
  run_expr ~args:["--config=tests/shredding/config.sample"] {|query [4] flat { for (x <- [1, 2, 3, 4, 5, 6]) [(x = x)] }|};
  [%expect {|
    [(x = 1), (x = 2), (x = 3), (x = 4), (x = 5), (x = 6)] : [(x:Int)]
    exit: 0 |}]

let%expect_test "Nested query annotations (1)" =
  run_expr {|query nested { query nested { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    [(x = 1), (x = 2), (x = 3)] : [(x:Int)]
    exit: 0 |}]

let%expect_test "Nested query annotations (2)" =
  run_expr {|query nested { query { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    [(x = 1), (x = 2), (x = 3)] : [(x:Int)]
    exit: 0 |}]

let%expect_test "Nested query annotations (3)" =
  run_expr {|query { query nested { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    ***: Runtime error: Incompatible query evaluation annotations. Expected CommonTypes.QueryPolicy.Flat, got CommonTypes.QueryPolicy.Nested.
    exit: 1 |}]

let%expect_test "Nested query annotations (4)" =
  run_expr {|query flat { query flat { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    [(x = 1), (x = 2), (x = 3)] : [(x:Int)]
    exit: 0 |}]

let%expect_test "Nested query annotations (5)" =
  run_expr {|query nested { query flat { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    ***: Runtime error: Incompatible query evaluation annotations. Expected CommonTypes.QueryPolicy.Nested, got CommonTypes.QueryPolicy.Flat.
    exit: 1 |}]

let%expect_test "Nested query annotations (5)" =
  run_expr {|query flat { query nested { for (i <- [1,2,3]) [(x = i)] } }|};
  [%expect {|
    ***: Runtime error: Incompatible query evaluation annotations. Expected CommonTypes.QueryPolicy.Flat, got CommonTypes.QueryPolicy.Nested.
    exit: 1 |}]

let%expect_test "Ranges are wild (1)" =
  run_expr {|query {for (x <- [1..3] ) [(num=x)]}|};
  [%expect {|
    <string>:1: Type error: Ranges are wild
        `(wild|a)'
    but the currently allowed effects are
        `()'
    In expression: [1..3].

    exit: 1 |}]

let%expect_test "Ranges are wild, but can appear outside of query blocks (2)" =
  run_expr {|for (x <- [1..3] ) [(num=x)]|};
  [%expect {|
    [(num = 1), (num = 2), (num = 3)] : [(num:Int)]
    exit: 0 |}]

