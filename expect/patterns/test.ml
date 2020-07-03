open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Nested variant matching [1]" =
  run_expr {|switch (A (A)) { case A (A) -> 0 case A (B) -> 1 }|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Nested variant matching [2]" =
  run_expr {|switch (A (A)) { case A (B) -> 0 case A (A) -> 1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Constant patterns [1]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case A (1) -> 1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Constant patterns [2]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case _ -> 1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Constant patterns [3]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case A (x) -> 1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Default pattern" =
  run_expr {|switch (A) { case _ -> 0 }|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Integer pattern" =
  run_expr {|switch (0) { case x -> x+1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "List pattern" =
  run_expr {|switch (A ([])) { case A (_::_) -> 0 case A ([]) -> 1 }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Open record pattern" =
  run_expr {|fun (r) {switch (r) {case (l=l|s) -> s}}|};
  [%expect {|
    fun : ((l:_|b::Any)) -> (l-|b::Any)
    exit: 0 |}]

let%expect_test "HasType pattern [1]" =
  run_expr {|switch (1) {case (0:Int) -> 0 case (1:Int) -> 1}|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "HasType pattern [2]" =
  run_expr {|switch (1) {case (1:Int) -> 1 case (x:String) -> 0}|};
  [%expect {|
    exit: 1
    <string>:1: Type error: All the cases of a switch should have compatible patterns, but the pattern
        `1:Int'
    has type
        `Int'
    while the subsequent patterns have type
        `String'
    In expression: switch (1) {case (1:Int) -> 1 case (x:String) -> 0}. |}]

let%expect_test "HasType pattern [3]" =
  run_expr {|switch (A) {case A -> 0 case -A:[|B:[|C:String|]|] -> 1}|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "HasType pattern [4]" =
  run_expr {|switch (A) {case A -> 0 case B(C(x:String)) -> 1}|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "As pattern" =
  run_expr {|switch (1) {case 1 as x -> x}|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Absence typing in variant patterns" =
  run_expr {|fun f(x) {switch (x) {case A(B) -> B case A(-B as y) -> A(f(y))}} f|};
  [%expect {|
    fun : ([||(mu a . A:[|B|a|])|]) ~> mu c . [|A:c|B|_::Any|]
    exit: 0 |}]

let%expect_test "Redundant pattern [1]" =
  run_expr {|fun (x) { switch (x) { case x -> x case A -> A }}|};
  [%expect {|
    fun : ([|A|a::Any|]) -> [|A|a::Any|]
    exit: 0 |}]

let%expect_test "Redundant pattern [2]" =
  run_expr {|fun (x) { switch (x) { case x -> A(1) case A -> A('2') } }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: All the cases of a switch should have the same type, but the expression
        `A(1)'
    has type
        `[|A:Int|a::Any|]'
    while the subsequent expressions have type
        `[|A:Char|b::Any|]'
    In expression: switch (x) { case x -> A(1) case A -> A('2') }. |}]

let%expect_test "Redundant pattern [3]" =
  run_expr {|fun (x) {switch (x) { case(A(B(C(1,2,3)))) -> 0 case(A(B(C(1,2,3)))) -> 1}}|};
  [%expect {|
    fun : ([|A:[|B:[|C:(Int, Int, Int)|]|]|]) -> Int
    exit: 0 |}]

let%expect_test "Type-based redundant pattern" =
  run_expr {|fun (x) {switch (x) { case(true) -> 0 case(false) -> 1 case(y) -> 2}}|};
  [%expect {|
    fun : (Bool) -> Int
    exit: 0 |}]

let%expect_test "Pattern matching twice against the same expression" =
  run_expr {|fun (x) {(switch (x) {case A -> 0 case _ -> 1}) + (switch (x) {case A -> 0 case _ -> 1})}|};
  [%expect {|
    fun : ([|A|_|]) -> Int
    exit: 0 |}]

let%expect_test "Negative pattern [1]" =
  run_expr {|fun (-Foo) { () }|};
  [%expect {|
    fun : ([|Foo:_::Any|_::Any|]) -> ()
    exit: 0 |}]

let%expect_test "Negative pattern [2]" =
  run_expr {|(fun (-Foo) { () })(Bar)|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Negative pattern [3]" =
  run_expr {|(fun (-Foo) { () })(Foo)|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [4]" =
  run_expr {|fun (-(Foo,Bar,Baz)) { () }|};
  [%expect {|
    fun : ([|Bar:_::Any|Baz:_::Any|Foo:_::Any|_::Any|]) -> ()
    exit: 0 |}]

let%expect_test "Negative pattern [5]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Bar)|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [6]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Baz)|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [5]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Foo)|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [6]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { 42 })(Quux)|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Negative pattern [7]" =
  run_expr {|(fun() { var -Foo = Foo; () })()|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [8]" =
  run_expr {|(fun() { var -(Foo,Bar,Baz) = Bar; () })()|};
  [%expect {|
    exit: 1
    ***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Negative pattern [9]" =
  run_expr {|(fun() { var -(Foo,Bar,Baz) = Quux; 42 })()|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Negative pattern [10]" =
  run_expr {|fun(-Foo as r) { r }|};
  [%expect {|
    fun : ([|Foo:_::Any|b::Any|]) -> [|Foo-|b::Any|]
    exit: 0 |}]

let%expect_test "Negative pattern [11]" =
  run_expr {|(fun(x) { switch(x) { case (-(Foo, Bar, Baz) as x) -> x case _ -> Quux }})(Foo)|};
  [%expect {|
    Quux : [|Bar-|Baz-|Foo-|Quux|_|]
    exit: 0 |}]

