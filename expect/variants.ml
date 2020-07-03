open Test_common
open Expect_test_common.File.Location


let%expect_test "Construction" =
  run_expr {|Foo|}

let%expect_test "Nested Construction" =
  run_expr {|Foo (Bar)|}

let%expect_test "Nested Construction + argument (nary)" =
  run_expr {|This(Is(A(Valid(Links(Program(42))))))|}

let%expect_test "Trivial closed case" =
  run_expr {|fun (x) { switch (x) { case A(b) -> b } }|}

let%expect_test "Variant matching - Closed case, immediate value [1]" =
  run_expr {|switch (A(3)) { case A(a) -> a case B(b) -> b }|}

let%expect_test "Variant matching - Closed case, immediate value [2]" =
  run_expr {|switch (B(3)) { case A(a) -> a case B(b) -> b }|}

let%expect_test "Variant matching - Closed case, immediate value [3]" =
  run_expr {|switch (L(3)) { case L(x) -> x case M(y) -> y }|}

let%expect_test "Variant matching - Closed case in function [1]" =
  run_expr {|fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> b } }|}

let%expect_test "Variant matching - Closed case in function [2]" =
  run_expr {|fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> true } }|}

let%expect_test "Variant matching - Closed case in function [3]" =
  run_expr {|fun (f) { switch (f) { case B(a) -> not(a) case A(b) -> b } }|}

let%expect_test "Variant matching : Closed case type error" =
  run_expr {|fun () { switch (C (3)) { case A (a) -> a case B (b) -> b } }|}

let%expect_test "Variant matching - Open case \"immediate value\" [1]" =
  run_expr {|switch (A(true)) { case A(a) -> a case B(b) -> b case c -> false }|}

let%expect_test "Variant matching - Open case \"immediate value\" [2]" =
  run_expr {|switch (C(true)) { case A(a) -> a case B(b) -> b case c -> false }|}

let%expect_test "Variant matching - Open case in function" =
  run_expr {|fun (f) { switch (f) { case A (a) -> a case B (b) -> b case c -> false } }|}

let%expect_test "Recursive variant types [1]" =
  run_expr {|fun (x) { switch (x) { case A(a) -> a case y -> x } }|}

let%expect_test "Recursive variant types [2]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ ((increment(n))) }} increment|}

let%expect_test "Recursive variant types [3]" =
  run_expr {|fun rev(x, r) { switch (x) { case Empty -> r case Cons(a, b) -> rev(b, Cons(a, r)) }} rev|}

let%expect_test "Recursive variant types [4]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ (increment(n))}} fun (x) {switch (increment(x)) { case Foo -> 0 case Zero -> 1 case Succ (n) -> 2 }}|}

let%expect_test "Recursive variant types [5]" =
  run_expr {|fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ (increment(n))}} increment(increment(Zero))|}

let%expect_test "Rows preserved across functions" =
  run_expr {|fun f(x) { switch (x) { case Foo -> Bar case s -> s } } f|}

let%expect_test "Nullary variants with cases" =
  run_expr {|switch (None) { case None -> None case Some (x) -> Some (x) }|}

let%expect_test "Nested variant unification" =
  run_expr {|[C (A), C (B)]|}

let%expect_test "Type annotations" =
  run_expr {|fun increment(x) {(Succ (x)):([|Succ:(mu a . [|Zero | Succ:a|])|])} increment|}

let%expect_test "Closure at top-level (issue #422) [1]" =
  run_expr {|(switch(Foo(id)) { case Foo(id) -> fun(x) { id(x) } })(2)|}

let%expect_test "Closure at top-level (issue #422) [2]" =
  run_expr {|switch(Foo(fun(x) { x })) { case Foo(id) -> fun(x) { id(x) } }|}

let%expect_test "Constructor names with primes (1)" =
  run_expr {|Foo'|}

let%expect_test "Constructor names with primes (2)" =
  run_expr {|Foo'''''bar|}

