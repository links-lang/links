open Test_common
open Expect_test_common.File.Location


let%expect_test "Orderby clause (not a semantic test, just syntax)" =
  run_expr {|for (x <- [1, 2, 3]) orderby (x) [x]|}

let%expect_test "XML literals in query blocks (1)" =
  run_expr {|query { for (x <- [(<a>asdf</a>)]) [(b=2)] }|}

let%expect_test "XML literals in query blocks (2)" =
  run_expr {|query { for (x <- (<a>asdf</a>)) [(b=2)] }|}

let%expect_test "XML literals in query blocks (3)" =
  run_expr {|query { for (x <- (<a>asdf</a>)) [(b=2)] }|}

let%expect_test "XML literals in query blocks (4)" =
  run_expr {|query {var x = <a>asdf</a>; [(a=1)]}|}

let%expect_test "XML literals in query blocks (5)" =
  run_expr {|query {var x = <a href="foo.com">asdf</a>; [(a=1)]}|}

let%expect_test "XML literals in query blocks (6)" =
  run_expr {|query {var x = <a href="foo.com">{stringToXml("asdf")}</a>; [(a=1)]}|}

let%expect_test "XML literals in query blocks (6)" =
  run_expr {|query {var x = <a {[("href", "foo.com")]}>{stringToXml("asdf")}</a>; [(a=1)]}|}

let%expect_test "Explicit query evaluator annotation (1)" =
  run_expr {|query nested { for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }|}

let%expect_test "Explicit query evaluator annotation (2)" =
  run_expr ~args:["--config=tests/shredding/config.sample"] {|query flat { for (x <- [1,2,3]) [(x = x, y = (for (y <- [4,5,6]) [y]))] }|}

let%expect_test "Explicit query evaluator annotation (3)" =
  run_expr ~args:["--config=tests/shredding/config.sample"] {|query [4] flat { for (x <- [1, 2, 3, 4, 5, 6]) [(x = x)] }|}

let%expect_test "Nested query annotations (1)" =
  run_expr {|query nested { query nested { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Nested query annotations (2)" =
  run_expr {|query nested { query { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Nested query annotations (3)" =
  run_expr {|query { query nested { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Nested query annotations (4)" =
  run_expr {|query flat { query flat { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Nested query annotations (5)" =
  run_expr {|query nested { query flat { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Nested query annotations (5)" =
  run_expr {|query flat { query nested { for (i <- [1,2,3]) [(x = i)] } }|}

let%expect_test "Ranges are wild (1)" =
  run_expr {|query {for (x <- [1..3] ) [(num=x)]}|}

