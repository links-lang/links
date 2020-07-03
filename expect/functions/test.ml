open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Function typing bug (see jdy's blog, 2005-10-24)" =
  run_expr {|(fun (x,y) { [x] ++ [y] }) ([1],"a")|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The function
        `(fun (x,y) { [x] ++ [y] })'
    has type
        `([Int], [Int]) -a-> [[Int]]'
    while the arguments passed to it have types
        `[Int]'
    and
        `String'
    and the currently allowed effects are
        `wild'
    In expression: (fun (x,y) { [x] ++ [y] }) ([1],"a"). |}]

let%expect_test "Type annotations on functions" =
  run_expr {|fun (x) { x } : (String) -> String|};
  [%expect {|
    fun : (String) -> String
    exit: 0 |}]

let%expect_test "Incorrect type annotations rejected" =
  run_expr {|fun (x) { x + 1 } : (Float) -> String|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The inferred type of the expression
        `fun (x) { x + 1 }'
    is
        `(Int) -a-> Int'
    but it is annotated with type
        `(Float) -b-> String'
    In expression: fun (x) { x + 1 } : (Float) -> String. |}]

let%expect_test "Loose type annotations on functions" =
  run_expr {|fun (x) { x } : (b) -> b|};
  [%expect {|
    fun : (a) -> a
    exit: 0 |}]

let%expect_test "Trailing semicolon means \"ignore the final value\" [1]" =
  run_expr {|{ 2 }|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Trailing semicolon means \"ignore the final value\" [2]" =
  run_expr {|{ 2; }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Side-effect expressions must have type `()', but the expression
        `2'
    has type
        `Int'
    In expression: 2. |}]

let%expect_test "Trailing semicolon means \"ignore the final value\" [3]" =
  run_expr {|fun () { 2 }|};
  [%expect {|
    fun : () -> Int
    exit: 0 |}]

let%expect_test "Trailing semicolon means \"ignore the final value\" [4]" =
  run_expr {|fun () { 2; }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Side-effect expressions must have type `()', but the expression
        `2'
    has type
        `Int'
    In expression: 2. |}]

let%expect_test "Type annotations" =
  run_expr {|fun (x:Int) {x:Int}|};
  [%expect {|
    fun : (Int) -> Int
    exit: 0 |}]

let%expect_test "Identity annotation" =
  run_expr {|fun (x:a) {x:a}|};
  [%expect {|
    fun : (a) -> a
    exit: 0 |}]

let%expect_test "Type annotation scope" =
  run_expr {|fun (x:a, y:a) {(x, y)}|};
  [%expect {|
    fun : (a, a) -> (a, a)
    exit: 0 |}]

let%expect_test "Negative recursive type" =
  run_expr {|fun (x) {x:a} : a|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The inferred type of the expression
        `fun (x) {x:a}'
    is
        `(a) -b-> a'
    but it is annotated with type
        `a'
    In expression: fun (x) {x:a} : a. |}]

let%expect_test "Infers effect variables" =
  run_expr {|fun (f, x: Int) { f(x) + 0 }|};
  [%expect {|
    fun : ((Int) -a-> Int, Int) -a-> Int
    exit: 0 |}]

let%expect_test "Typename [1]" =
  run_expr {|typename Foo = Int; fun f(x:Foo) {x} f(1)|};
  [%expect {|
    1 : Foo
    exit: 0 |}]

let%expect_test "Typename [2]" =
  run_expr {|typename Bar(a,b,c) = (a,b,c); fun f(x:Bar(a,b,c)) {x} f((false, 1, "two"))|};
  [%expect {|
    (false, 1, "two") : Bar (Bool,Int,String)
    exit: 0 |}]

let%expect_test "Typename [3]" =
  run_expr {|typename F(a,b) = (a) {:b}~> a; sig f : F(Int,Int) fun f(x) {recv() + x} sig g : F(Int,String) fun g(x) {stringToInt(recv()) + x} g|};
  [%expect {|
    fun : F (Int,String)
    exit: 0 |}]

let%expect_test "Nested closures" =
  run_file {|./tests/functions/nested-closures.links|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Quantification of alien functions (#280)" =
  run_file {|./tests/functions/alien-quantification.links|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.EvaluationError("Cannot make alien call on the server.") |}]

let%expect_test "Type annotation on inner function (correct, basic)" =
  run_file {|./tests/functions/innerfun1.links|};
  [%expect {|
    "Hello!" : String
    exit: 0 |}]

let%expect_test "Type annotation on inner function (incorrect, basic)" =
  run_file {|./tests/functions/innerfun2.links|};
  [%expect {|
    exit: 1
    ./tests/functions/innerfun2.links:2: Type error: The non-recursive function definition has return type
        `String'
    but its annotation has return type
        `Int'
    In expression: sig bar : () -> Int
      fun bar() {
        "Hello!"
      }. |}]

let%expect_test "Type annotation on inner function (correct, recursive)" =
  run_file {|./tests/functions/innerfun3.links|};
  [%expect {|
    "Hello!" : String
    exit: 0 |}]

let%expect_test "Closure conversion: Test generalization of free type variables during hoisting" =
  run_file {|./tests/functions/nested-functions-polymorphic.links|};
  [%expect {|
    123 : Int
    exit: 0 |}]

let%expect_test "Closure conversion: Test function with free type variables, but no free term variables" =
  run_file {|./tests/functions/closure-conv-type-abstr-only.links|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Quantifiers should not escape their scopes (#687)" =
  run_file {|./tests/functions/escaped-quantifier.links|};
  [%expect {|
    exit: 1
    ./tests/functions/escaped-quantifier.links:4: Type error: The quantifiers in the type of function
        g: forall a.(a) {}-> a
    escape their scope, as they are present in the types:
        f: (a) {}-> a
    In expression: fun g(x) { f(x) }. |}]

let%expect_test "Linearity (1)" =
  run_expr {|fun (x, y) {(x, y)}|};
  [%expect {|
    fun : (a::Any, b::Any) -> (a::Any, b::Any)
    exit: 0 |}]

let%expect_test "Linearity (2)" =
  run_expr {|fun (x) {fun (y) {(x, y)}}|};
  [%expect {|
    fun : (a) -> (c::Any) -> (a, c::Any)
    exit: 0 |}]

let%expect_test "Linearity (3) (#795)" =
  run_expr {|fun (x)(y) {(x, y)}|};
  [%expect {|
    fun : (a) -> (c::Any) -> (a, c::Any)
    exit: 0 |}]

let%expect_test "Linearity (4)" =
  run_expr {|linfun (x){ linfun (y) {(x, y)}}|};
  [%expect {|
    fun : (a::Any) -@ (c::Any) -@ (a::Any, c::Any)
    exit: 0 |}]

let%expect_test "Linearity (5)" =
  run_expr {|linfun (x)(y) {(x, y)}|};
  [%expect {|
    fun : (a::Any) -@ (c::Any) -@ (a::Any, c::Any)
    exit: 0 |}]

let%expect_test "Linearity (6) (#797)" =
  run_expr {|fun f(x)(y) {(x, y)} f|};
  [%expect {|
    fun : (a) -> (c::Any) -> (a, c::Any)
    exit: 0 |}]

let%expect_test "Linearity (7) (#797)" =
  run_expr {|fun f(x)(y) {f(x)(y)} f|};
  [%expect {|
    fun : (_) -> (_::Any) ~> _::Any
    exit: 0 |}]

let%expect_test "Linearity (8) (#797)" =
  run_expr {|linfun f(x)(y) {(x, y)} f|};
  [%expect {|
    fun : (a::Any) -@ (c::Any) -@ (a::Any, c::Any)
    exit: 0 |}]

