open Test_common
open Expect_test_common.File.Location


let%expect_test "Nested variant matching [1]" =
  run_expr {|switch (A (A)) { case A (A) -> 0 case A (B) -> 1 }|}

let%expect_test "Nested variant matching [2]" =
  run_expr {|switch (A (A)) { case A (B) -> 0 case A (A) -> 1 }|}

let%expect_test "Constant patterns [1]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case A (1) -> 1 }|}

let%expect_test "Constant patterns [2]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case _ -> 1 }|}

let%expect_test "Constant patterns [3]" =
  run_expr {|switch (A (1)) { case A (0) -> 0 case A (x) -> 1 }|}

let%expect_test "Default pattern" =
  run_expr {|switch (A) { case _ -> 0 }|}

let%expect_test "Integer pattern" =
  run_expr {|switch (0) { case x -> x+1 }|}

let%expect_test "List pattern" =
  run_expr {|switch (A ([])) { case A (_::_) -> 0 case A ([]) -> 1 }|}

let%expect_test "Open record pattern" =
  run_expr {|fun (r) {switch (r) {case (l=l|s) -> s}}|}

let%expect_test "HasType pattern [1]" =
  run_expr {|switch (1) {case (0:Int) -> 0 case (1:Int) -> 1}|}

let%expect_test "HasType pattern [2]" =
  run_expr {|switch (1) {case (1:Int) -> 1 case (x:String) -> 0}|}

let%expect_test "HasType pattern [3]" =
  run_expr {|switch (A) {case A -> 0 case -A:[|B:[|C:String|]|] -> 1}|}

let%expect_test "HasType pattern [4]" =
  run_expr {|switch (A) {case A -> 0 case B(C(x:String)) -> 1}|}

let%expect_test "As pattern" =
  run_expr {|switch (1) {case 1 as x -> x}|}

let%expect_test "Absence typing in variant patterns" =
  run_expr {|fun f(x) {switch (x) {case A(B) -> B case A(-B as y) -> A(f(y))}} f|}

let%expect_test "Redundant pattern [1]" =
  run_expr {|fun (x) { switch (x) { case x -> x case A -> A }}|}

let%expect_test "Redundant pattern [2]" =
  run_expr {|fun (x) { switch (x) { case x -> A(1) case A -> A('2') } }|}

let%expect_test "Redundant pattern [3]" =
  run_expr {|fun (x) {switch (x) { case(A(B(C(1,2,3)))) -> 0 case(A(B(C(1,2,3)))) -> 1}}|}

let%expect_test "Type-based redundant pattern" =
  run_expr {|fun (x) {switch (x) { case(true) -> 0 case(false) -> 1 case(y) -> 2}}|}

let%expect_test "Pattern matching twice against the same expression" =
  run_expr {|fun (x) {(switch (x) {case A -> 0 case _ -> 1}) + (switch (x) {case A -> 0 case _ -> 1})}|}

let%expect_test "Negative pattern [1]" =
  run_expr {|fun (-Foo) { () }|}

let%expect_test "Negative pattern [2]" =
  run_expr {|(fun (-Foo) { () })(Bar)|}

let%expect_test "Negative pattern [3]" =
  run_expr {|(fun (-Foo) { () })(Foo)|}

let%expect_test "Negative pattern [4]" =
  run_expr {|fun (-(Foo,Bar,Baz)) { () }|}

let%expect_test "Negative pattern [5]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Bar)|}

let%expect_test "Negative pattern [6]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Baz)|}

let%expect_test "Negative pattern [5]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { () })(Foo)|}

let%expect_test "Negative pattern [6]" =
  run_expr {|(fun (-(Foo,Bar,Baz)) { 42 })(Quux)|}

let%expect_test "Negative pattern [7]" =
  run_expr {|(fun() { var -Foo = Foo; () })()|}

let%expect_test "Negative pattern [8]" =
  run_expr {|(fun() { var -(Foo,Bar,Baz) = Bar; () })()|}

let%expect_test "Negative pattern [9]" =
  run_expr {|(fun() { var -(Foo,Bar,Baz) = Quux; 42 })()|}

let%expect_test "Negative pattern [10]" =
  run_expr {|fun(-Foo as r) { r }|}

let%expect_test "Negative pattern [11]" =
  run_expr {|(fun(x) { switch(x) { case (-(Foo, Bar, Baz) as x) -> x case _ -> Quux }})(Foo)|}

