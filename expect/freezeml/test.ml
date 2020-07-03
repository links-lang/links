open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let config = Some ("tests/freezeml.tests.config")

let%expect_test "Polymorphic Instantiation (1)" =
  run_expr ~config {|fun(x)(y) { y }|};
  [%expect {|
    fun : forall a,b::Row,c::(Any,Any),d::Row.(a) -b-> (c::Any) -d-> c::Any
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (1•)" =
  run_expr ~config {|$(fun(x)(y) { y })|};
  [%expect {|
    fun : forall a,b::Row,c::(Any,Any),d::Row.(a) -b-> (c::Any) -d-> c::Any
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (2)" =
  run_expr ~config {|choose(id)|};
  [%expect {|
    fun : ((a) -b-> a) -c-> (a) -b-> a
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (2•)" =
  run_expr ~config {|choose(~id)|};
  [%expect {|
    fun : (forall a,b::Row.(a) -b-> a) -c-> forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (3)" =
  run_expr ~config {|choose([])(ids)|};
  [%expect {|
    [] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (4)" =
  run_expr ~config {|fun(x : (forall a,e::Row. (a) -e-> a)) { x(x) }|};
  [%expect {|
    fun : forall a::Row,b,c::Row.(forall d,e::Row.(d) -e-> d) -a-> (b) -c-> b
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (4•)" =
  run_expr ~config {|fun(x : (forall a,e::Row. (a) -e-> a)) { x(~x) }|};
  [%expect {|
    fun : forall a::Row.(forall b,c::Row.(b) -c-> b) -a-> forall b,c::Row.(b) -c-> b
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (5)" =
  run_expr ~config {|id(auto)|};
  [%expect {|
    fun : (forall a,b::Row.(a) -b-> a) -c-> forall d,e::Row.(d) -e-> d
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (6)" =
  run_expr ~config {|id(auto')|};
  [%expect {|
    fun : (forall a,b::Row.(a) -b-> a) -c-> (d) -e-> d
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (6•)" =
  run_expr ~config {|id(~auto')|};
  [%expect {|
    fun : forall a,b::Row,c::Row.(forall d,e::Row.(d) -e-> d) -b-> (a) -c-> a
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (7)" =
  run_expr ~config {|choose(id)(auto)|};
  [%expect {|
    fun : (forall a,b::Row.(a) -b-> a) -c-> forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (8)" =
  run_expr ~config {|choose(id)(auto')|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `choose(id)'
    has type
        `((forall a,b::Row.(a) -b-> a) -c-> forall a,b::Row.(a) -b-> a) -d-> (forall a,b::Row.(a) -b-> a) -c-> forall a,b::Row.(a) -b-> a'
    while the arguments passed to it have types
        `(forall a,b::Row.(a) -b-> a) -c-> (e) -f-> e'
    and the currently allowed effects are
        `wild'
    In expression: choose(id)(auto'). |}]

let%expect_test "Polymorphic Instantiation (9)" =
  run_file ~config {|tests/freezeml/a9.links|};
  [%expect {|
    fun : forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (10)" =
  run_expr ~config {|poly(~id)|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (11)" =
  run_expr ~config {|poly($(fun(x:_::(Unl,Any)) { x }))|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Polymorphic Instantiation (12)" =
  run_expr ~config {|id(poly)($(fun(x:_::(Unl,Any)) { x }))|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Inference of Polymorphic Arguments (1)" =
  run_expr ~config {|fun(f : (forall a,e::Row. (a) -e-> a)) { (f(1), f(true)) }|};
  [%expect {|
    fun : forall a::Row.(forall b,c::Row.(b) -c-> b) -a-> (Int, Bool)
    exit: 0 |}]

let%expect_test "Inference of Polymorphic Arguments (2)" =
  run_expr ~config {|fun(xs : [forall a,e::Row. (a) -e-> a]) { poly(head(xs)) }|};
  [%expect {|
    fun : forall a::Row.([forall b,c::Row.(b) -c-> b]) -a-> (Int, Bool)
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (1)" =
  run_expr ~config {|length(ids)|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (2)" =
  run_expr ~config {|tail(ids)|};
  [%expect {|
    [] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (3)" =
  run_expr ~config {|head(ids)|};
  [%expect {|
    fun : forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (4)" =
  run_expr ~config {|single(id)|};
  [%expect {|
    [fun] : [(a) -b-> a]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (4•)" =
  run_expr ~config {|single(~id)|};
  [%expect {|
    [fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (5)" =
  run_expr ~config {|~id :: ids|};
  [%expect {|
    [fun, fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (6)" =
  run_expr ~config {|$(fun(x:_::(Unl,Any)) { x }) :: ids|};
  [%expect {|
    [fun, fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (7)" =
  run_expr ~config {|single(inc) ++ single(id)|};
  [%expect {|
    [fun, fun] : [(Int) -a-> Int]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (8)" =
  run_file ~config {|tests/freezeml/c8.links|};
  [%expect {|
    fun : forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (9)" =
  run_expr ~config {|map(poly)(single(~id))|};
  [%expect {|
    [(0, true)] : [(Int, Bool)]
    exit: 0 |}]

let%expect_test "Functions on Polymorphic Lists (10)" =
  run_expr ~config {|map(head)(single(ids))|};
  [%expect {|
    [fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "Application functions (1)" =
  run_expr ~config {|app(poly)(~id)|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Application functions (2)" =
  run_expr ~config {|revapp(~id)(poly)|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Application functions (3)" =
  run_expr ~config {|runST(~argST)|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Application functions (4)" =
  run_expr ~config {|revapp(~argST)(runST)|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "η-expansion (1)" =
  run_file ~config {|tests/freezeml/e1.links|};
  [%expect {|
    exit: 1
    tests/freezeml/e1.links:15: Type error: The function
        `k(h)'
    has type
        `([(Int) -a-> forall b,c::Row.(b) -c-> b]) -d-> (Int) -a-> forall b,c::Row.(b) -c-> b'
    while the arguments passed to it have types
        `[forall e::Row,f,g::Row.(Int) -e-> (f) -g-> f]'
    and the currently allowed effects are
        `wild'
    In expression: k(h)(lst). |}]

let%expect_test "η-expansion (2)" =
  run_file ~config {|tests/freezeml/e2.links|};
  [%expect {|
    fun : forall a::Row,b,c::Row.(Int) -a-> (b) -c-> b
    exit: 0 |}]

let%expect_test "η-expansion (3)" =
  run_file ~config {|tests/freezeml/e3.links|};
  [%expect {|
    exit: 1
    tests/freezeml/e3.links:4: Type error: The function
        `r'
    has type
        `(forall a,b::Row.(a) -b-> forall c,d::Row.(c) -d-> c) -e-> Int'
    while the arguments passed to it have types
        `(f) -g-> (h) -i-> h'
    and the currently allowed effects are
        `wild'
    In expression: r(fun(x:a::(Unl,Any))(y:b::(Unl,Any)) { y }). |}]

let%expect_test "η-expansion (4)" =
  run_file ~config {|tests/freezeml/e4.links|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "FreezeML programs (1)" =
  run_expr ~config {|fun id(x) { x } ~id|};
  [%expect {|
    fun : forall a::(Any,Any),b::Row.(a::Any) -b-> a::Any
    exit: 0 |}]

let%expect_test "FreezeML programs (2)" =
  run_expr ~config {|var ids = [~id]; ~ids|};
  [%expect {|
    [fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "FreezeML programs (5)" =
  run_expr ~config {|auto(~id)|};
  [%expect {|
    fun : forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "FreezeML programs (6)" =
  run_expr ~config {|head(ids) :: ids|};
  [%expect {|
    [fun, fun] : [forall a,b::Row.(a) -b-> a]
    exit: 0 |}]

let%expect_test "FreezeML programs (7)" =
  run_expr ~config {|head(ids)@(3)|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "FreezeML programs (8)" =
  run_expr ~config {|choose (head(ids))|};
  [%expect {|
    fun : (forall a,b::Row.(a) -b-> a) -c-> forall a,b::Row.(a) -b-> a
    exit: 0 |}]

let%expect_test "FreezeML programs (8•)" =
  run_expr ~config {|choose (head(ids)@)|};
  [%expect {|
    fun : ((a) -b-> a) -c-> (a) -b-> a
    exit: 0 |}]

let%expect_test "FreezeML programs (9)" =
  run_expr ~config {|var f = revapp(~id); f(poly)|};
  [%expect {|
    (0, true) : (Int, Bool)
    exit: 0 |}]

let%expect_test "Do not generalise non-values" =
  run_expr ~config {|$(id(2))|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Because of the value restriction, the expression
        `(id(2))'
    cannot be generalised.
    In expression: $(id(2)). |}]

let%expect_test "Do not infer polymorphic arguments (1)" =
  run_expr ~config {|fun(f) { (poly(~f), f(42) + 1) }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `poly'
    has type
        `(forall a,b::Row.(a) -b-> a) -c-> (Int, Bool)'
    while the arguments passed to it have types
        `d::(Any,Mono)'
    and the currently allowed effects are
        `|e'
    In expression: poly(~f). |}]

let%expect_test "Do not infer polymorphic arguments (2)" =
  run_expr ~config {|fun(f) { (f(42) + 1, poly(~f)) }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `poly'
    has type
        `(forall a,b::Row.(a) -b-> a) -c-> (Int, Bool)'
    while the arguments passed to it have types
        `(Int) -d-> Int'
    and the currently allowed effects are
        `|d::(Unl,Mono)'
    In expression: poly(~f). |}]

let%expect_test "Do not infer polymorphic arguments (3)" =
  run_expr ~config {|tests/freezeml/session1.links|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unknown variable tests.
    In expression: tests. |}]

let%expect_test "Do not infer polymorphic arguments (4)" =
  run_expr ~config {|tests/freezeml/session2.links|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unknown variable tests.
    In expression: tests. |}]

let%expect_test "Do not infer polymorphic values (1)" =
  run_expr ~config {|fun(bot: (forall a. a)) { var f = bot(bot); (poly(~f), f(42) + 1) }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `poly'
    has type
        `(forall a,b::Row.(a) -b-> a) -c-> (Int, Bool)'
    while the arguments passed to it have types
        `d::(Any,Mono)'
    and the currently allowed effects are
        `|e'
    In expression: poly(~f). |}]

