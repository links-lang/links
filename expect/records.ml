open Test_common
open Expect_test_common.File.Location


let%expect_test "Record printing" =
  run_expr {|(x=1, y="two")|}

let%expect_test "Quote record labels that are also keywords" =
  run_expr {|("client"=5, "fun"=7)|}

let%expect_test "Record comparisons" =
  run_expr {|(x=1, y="two") == (y="two", x=1)|}

let%expect_test "Record extension" =
  run_expr {|{var z = (y="three"); (x=4|z) }|}

let%expect_test "Let pattern matching" =
  run_expr {|{var (x=a,y=(z=b)) = (y=(z=3),x="four"); a}|}

let%expect_test "Lambda pattern matching" =
  run_expr {|(fun((x=a,y=(z=b))) { a }) ((y=(z=3),x="four"))|}

let%expect_test "Projection of absent field" =
  run_expr {|(x="1").y;;|}

let%expect_test "Projections" =
  run_expr {|(y=(z=3),x="four").y.z|}

let%expect_test "Projection sections" =
  run_expr {|(.x)((y=(z=3),x="four"))|}

let%expect_test "Passing two different closed rows to an open-row function arg" =
  run_expr {|fun foo(x) { x.a } (foo((a="a", b=2)), foo((a=1, c=3)))|}

let%expect_test "Passing two different list types to a polymorphic function" =
  run_expr {|fun foo(x) { hd(x) } (foo([1,2]), foo(['a', 'b', 'c']))|}

let%expect_test "Row types preserved across functions" =
  run_expr {|(fun (x) { var (r=r|s) = x; (r=3|s) })((r=3,s=4)).s|}

let%expect_test "With syntax (same type)" =
  run_expr {|((x = 3) with x = 4)|}

let%expect_test "With syntax (different type)" =
  run_expr {|((x = 3) with x = "four")|}

let%expect_test "With syntax: multiple labels (a)" =
  run_expr {|((x=3,y=4) with y="four")|}

let%expect_test "With syntax: multiple labels (b)" =
  run_expr {|((z='a',x=3,y=4) with x="four",y=3)|}

let%expect_test "With syntax (missing label)" =
  run_expr {|((x = 3) with y=4)|}

let%expect_test "Tables must have table type." =
  run_expr {|fun (t) { for (x <-- t) [(a=x.y)] }|}

let%expect_test "Duplicate fields" =
  run_expr {|(x=3,x=3)|}

let%expect_test "Uninhabited recursive rows (questionable)" =
  run_expr {|(fun (x : (|(mu b . b))) {x})(())|}

let%expect_test "Missing absent label (1)" =
  run_expr {|fun (r : (|a)) {(l=42|r)}|}

let%expect_test "Missing absent label (2)" =
  run_expr {|fun (r : (|a)) {(l=42|(r : (l-|a)))}|}

let%expect_test "Possibly absent label in a closed row" =
  run_expr {|fun (x : (a{%b}|%c)) {x : ()}|}

let%expect_test "Record field punning (1)" =
  run_expr {|var hello = "hellooo"; (=hello)|}

let%expect_test "Record field punning (2)" =
  run_expr {|var hello = "hellooo"; (=hello, world = 5)|}

let%expect_test "Record field punning (3)" =
  run_expr {|switch((hello=5, world=10)) { case (=hello, world=y) -> hello + y }|}

let%expect_test "Record field punning (4)" =
  run_expr {|var y = 2; (x=1, =y)|}

let%expect_test "Record field punning (5)" =
  run_expr {|var y = 2; (x=1, =y, z=3)|}

let%expect_test "Record field punning (6)" =
  run_expr {|var z = 3; (x=1, y=2, =z)|}

