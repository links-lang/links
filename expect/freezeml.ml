open Test_common
open Expect_test_common.File.Location


let config = Some ("tests/freezeml.tests.config")

let%expect_test "Polymorphic Instantiation (1)" =
  run_expr ~config {|fun(x)(y) { y }|}

let%expect_test "Polymorphic Instantiation (1•)" =
  run_expr ~config {|$(fun(x)(y) { y })|}

let%expect_test "Polymorphic Instantiation (2)" =
  run_expr ~config {|choose(id)|}

let%expect_test "Polymorphic Instantiation (2•)" =
  run_expr ~config {|choose(~id)|}

let%expect_test "Polymorphic Instantiation (3)" =
  run_expr ~config {|choose([])(ids)|}

let%expect_test "Polymorphic Instantiation (4)" =
  run_expr ~config {|fun(x : (forall a,e::Row. (a) -e-> a)) { x(x) }|}

let%expect_test "Polymorphic Instantiation (4•)" =
  run_expr ~config {|fun(x : (forall a,e::Row. (a) -e-> a)) { x(~x) }|}

let%expect_test "Polymorphic Instantiation (5)" =
  run_expr ~config {|id(auto)|}

let%expect_test "Polymorphic Instantiation (6)" =
  run_expr ~config {|id(auto')|}

let%expect_test "Polymorphic Instantiation (6•)" =
  run_expr ~config {|id(~auto')|}

let%expect_test "Polymorphic Instantiation (7)" =
  run_expr ~config {|choose(id)(auto)|}

let%expect_test "Polymorphic Instantiation (8)" =
  run_expr ~config {|choose(id)(auto')|}

let%expect_test "Polymorphic Instantiation (9)" =
  run_file ~config {|tests/freezeml/a9.links|}

let%expect_test "Polymorphic Instantiation (10)" =
  run_expr ~config {|poly(~id)|}

let%expect_test "Polymorphic Instantiation (11)" =
  run_expr ~config {|poly($(fun(x:_::(Unl,Any)) { x }))|}

let%expect_test "Polymorphic Instantiation (12)" =
  run_expr ~config {|id(poly)($(fun(x:_::(Unl,Any)) { x }))|}

let%expect_test "Inference of Polymorphic Arguments (1)" =
  run_expr ~config {|fun(f : (forall a,e::Row. (a) -e-> a)) { (f(1), f(true)) }|}

let%expect_test "Inference of Polymorphic Arguments (2)" =
  run_expr ~config {|fun(xs : [forall a,e::Row. (a) -e-> a]) { poly(head(xs)) }|}

let%expect_test "Functions on Polymorphic Lists (1)" =
  run_expr ~config {|length(ids)|}

let%expect_test "Functions on Polymorphic Lists (2)" =
  run_expr ~config {|tail(ids)|}

let%expect_test "Functions on Polymorphic Lists (3)" =
  run_expr ~config {|head(ids)|}

let%expect_test "Functions on Polymorphic Lists (4)" =
  run_expr ~config {|single(id)|}

let%expect_test "Functions on Polymorphic Lists (4•)" =
  run_expr ~config {|single(~id)|}

let%expect_test "Functions on Polymorphic Lists (5)" =
  run_expr ~config {|~id :: ids|}

let%expect_test "Functions on Polymorphic Lists (6)" =
  run_expr ~config {|$(fun(x:_::(Unl,Any)) { x }) :: ids|}

let%expect_test "Functions on Polymorphic Lists (7)" =
  run_expr ~config {|single(inc) ++ single(id)|}

let%expect_test "Functions on Polymorphic Lists (8)" =
  run_file ~config {|tests/freezeml/c8.links|}

let%expect_test "Functions on Polymorphic Lists (9)" =
  run_expr ~config {|map(poly)(single(~id))|}

let%expect_test "Functions on Polymorphic Lists (10)" =
  run_expr ~config {|map(head)(single(ids))|}

let%expect_test "Application functions (1)" =
  run_expr ~config {|app(poly)(~id)|}

let%expect_test "Application functions (2)" =
  run_expr ~config {|revapp(~id)(poly)|}

let%expect_test "Application functions (3)" =
  run_expr ~config {|runST(~argST)|}

let%expect_test "Application functions (4)" =
  run_expr ~config {|revapp(~argST)(runST)|}

let%expect_test "η-expansion (1)" =
  run_file ~config {|tests/freezeml/e1.links|}

let%expect_test "η-expansion (2)" =
  run_file ~config {|tests/freezeml/e2.links|}

let%expect_test "η-expansion (3)" =
  run_file ~config {|tests/freezeml/e3.links|}

let%expect_test "η-expansion (4)" =
  run_file ~config {|tests/freezeml/e4.links|}

let%expect_test "FreezeML programs (1)" =
  run_expr ~config {|fun id(x) { x } ~id|}

let%expect_test "FreezeML programs (2)" =
  run_expr ~config {|var ids = [~id]; ~ids|}

let%expect_test "FreezeML programs (5)" =
  run_expr ~config {|auto(~id)|}

let%expect_test "FreezeML programs (6)" =
  run_expr ~config {|head(ids) :: ids|}

let%expect_test "FreezeML programs (7)" =
  run_expr ~config {|head(ids)@(3)|}

let%expect_test "FreezeML programs (8)" =
  run_expr ~config {|choose (head(ids))|}

let%expect_test "FreezeML programs (8•)" =
  run_expr ~config {|choose (head(ids)@)|}

let%expect_test "FreezeML programs (9)" =
  run_expr ~config {|var f = revapp(~id); f(poly)|}

let%expect_test "Do not generalise non-values" =
  run_expr ~config {|$(id(2))|}

let%expect_test "Do not infer polymorphic arguments (1)" =
  run_expr ~config {|fun(f) { (poly(~f), f(42) + 1) }|}

let%expect_test "Do not infer polymorphic arguments (2)" =
  run_expr ~config {|fun(f) { (f(42) + 1, poly(~f)) }|}

let%expect_test "Do not infer polymorphic arguments (3)" =
  run_expr ~config {|tests/freezeml/session1.links|}

let%expect_test "Do not infer polymorphic arguments (4)" =
  run_expr ~config {|tests/freezeml/session2.links|}

let%expect_test "Do not infer polymorphic values (1)" =
  run_expr ~config {|fun(bot: (forall a. a)) { var f = bot(bot); (poly(~f), f(42) + 1) }|}

