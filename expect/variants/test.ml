open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Construction" =
  run_expr {|Foo|};
  [%expect {|
    Foo : [|Foo|_::Any|]
    exit: 0 |}]

let%expect_test "Nested Construction" =
  run_expr {|Foo (Bar)|};
  [%expect {|
    Foo(Bar) : [|Foo:[|Bar|_::Any|]|_::Any|]
    exit: 0 |}]

let%expect_test "Nested Construction + argument (nary)" =
  run_expr {|This(Is(A(Valid(Links(Program(42))))))|};
  [%expect {|
    This(Is(A(Valid(Links(Program(42)))))) : [|This:[|Is:[|A:[|Valid:[|Links:[|Program:Int|_::Any|]|_::Any|]|_::Any|]|_::Any|]|_::Any|]|_::Any|]
    exit: 0 |}]

let%expect_test "Trivial closed case" =
  run_expr {|fun (x) { switch (x) { case A(b) -> b } }|};
  [%expect {|
    fun : ([|A:a::Any|]) -> a::Any
    exit: 0 |}]

let%expect_test "Variant matching - Closed case, immediate value [1]" =
  run_expr {|switch (A(3)) { case A(a) -> a case B(b) -> b }|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Variant matching - Closed case, immediate value [2]" =
  run_expr {|switch (B(3)) { case A(a) -> a case B(b) -> b }|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Variant matching - Closed case, immediate value [3]" =
  run_expr {|switch (L(3)) { case L(x) -> x case M(y) -> y }|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Variant matching - Closed case in function [1]" =
  run_expr {|fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> b } }|};
  [%expect {|
    fun : ([|A:Bool|B:Bool|]) -> Bool
    exit: 0 |}]

let%expect_test "Variant matching - Closed case in function [2]" =
  run_expr {|fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> true } }|};
  [%expect {|
    fun : ([|A:Bool|B:_|]) -> Bool
    exit: 0 |}]

let%expect_test "Variant matching - Closed case in function [3]" =
  run_expr {|fun (f) { switch (f) { case B(a) -> not(a) case A(b) -> b } }|};
  [%expect {|
    fun : ([|A:Bool|B:Bool|]) -> Bool
    exit: 0 |}]

let%expect_test "Variant matching : Closed case type error" =
  run_expr {|fun () { switch (C (3)) { case A (a) -> a case B (b) -> b } }|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The type of an input to a switch should match the type of its patterns, but the expression
        `C (3)'
    has type
        `[|C:Int|a::Any|]'
    while the patterns have type
        `[|A:b::Any|B:b::Any|]'
    In expression: switch (C (3)) { case A (a) -> a case B (b) -> b }. |}]

let%expect_test "Variant matching - Open case \"immediate value\" [1]" =
  run_expr {|switch (A(true)) { case A(a) -> a case B(b) -> b case c -> false }|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Variant matching - Open case \"immediate value\" [2]" =
  run_expr {|switch (C(true)) { case A(a) -> a case B(b) -> b case c -> false }|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "Variant matching - Open case in function" =
  run_expr {|fun (f) { switch (f) { case A (a) -> a case B (b) -> b case c -> false } }|};
  [%expect {|
    fun : ([|A:Bool|B:Bool|_|]) -> Bool
    exit: 0 |}]

let%expect_test "Recursive variant types [1]" =
  run_expr {|fun (x) { switch (x) { case A(a) -> a case y -> x } }|};
  [%expect {|
    fun : (mu a . [|A:a|b|]) -> mu a . [|A:a|b|]
    exit: 0 |}]

let%expect_test "Recursive variant types [2]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ ((increment(n))) }} increment|};
  [%expect {|
    fun : (mu a . [|Succ:a|Zero|]) ~> [|Zero|(mu c . Succ:[|Zero|c|]|_::Any)|]
    exit: 0 |}]

let%expect_test "Recursive variant types [3]" =
  run_expr {|fun rev(x, r) { switch (x) { case Empty -> r case Cons(a, b) -> rev(b, Cons(a, r)) }} rev|};
  [%expect {|
    fun : (mu a . [|Cons:(b::Any, a)|Empty|], mu c . [|Cons:(b::Any, c)|d::Any|]) ~> [|Cons:(b::Any, mu f . [|Cons:(b::Any, f)|d::Any|])|d::Any|]
    exit: 0 |}]

let%expect_test "Recursive variant types [4]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ (increment(n))}} fun (x) {switch (increment(x)) { case Foo -> 0 case Zero -> 1 case Succ (n) -> 2 }}|};
  [%expect {|
    fun : (mu a . [|Succ:a|Zero|]) ~> Int
    exit: 0 |}]

let%expect_test "Recursive variant types [5]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ (increment(n))}} increment(increment(Zero))|};
  [%expect {|
    Succ(Succ(Zero)) : [|Zero|(mu a . Succ:[|Zero|a|]|_::Any)|]
    exit: 0 |}]

let%expect_test "Rows preserved across functions" =
  run_expr {|fun f(x) { switch (x) { case Foo -> Bar case s -> s } } f|};
  [%expect {|
    fun : ([|Bar|Foo|a::Any|]) -> [|Bar|Foo|a::Any|]
    exit: 0 |}]

let%expect_test "Nullary variants with cases" =
  run_expr {|switch (None) { case None -> None case Some (x) -> Some (x) }|};
  [%expect {|
    None : [|None|Some:_::Any|_::Any|]
    exit: 0 |}]

let%expect_test "Nested variant unification" =
  run_expr {|[C (A), C (B)]|};
  [%expect {|
    [C(A), C(B)] : [[|C:[|A|B|_::Any|]|_::Any|]]
    exit: 0 |}]

let%expect_test "Type annotations" =
  run_expr {|fun increment(x) {(Succ (x)):([|Succ:(mu a . [|Zero | Succ:a|])|])} increment|};
  [%expect {|
    fun : (mu a . [|Succ:a|Zero|]) -> [|Succ:mu c . [|Succ:c|Zero|]|]
    exit: 0 |}]

let%expect_test "Closure at top-level (issue #422) [1]" =
  run_expr {|(switch(Foo(id)) { case Foo(id) -> fun(x) { id(x) } })(2)|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Closure at top-level (issue #422) [2]" =
  run_expr {|switch(Foo(fun(x) { x })) { case Foo(id) -> fun(x) { id(x) } }|};
  [%expect {|
    fun : (a::Any) -> a::Any
    exit: 0 |}]

let%expect_test "Constructor names with primes (1)" =
  run_expr {|Foo'|};
  [%expect {|
    Foo' : [|Foo'|_::Any|]
    exit: 0 |}]

let%expect_test "Constructor names with primes (2)" =
  run_expr {|Foo'''''bar|};
  [%expect {|
    Foo'''''bar : [|Foo'''''bar|_::Any|]
    exit: 0 |}]

