open Test_common
open Expect_test_common.File.Location


let%expect_test "Type variables are correctly scoped [1]" =
  run_expr {|typename A = forall a::Eff. forall a::Type. () ~a~> ();|}

let%expect_test "Type variables are correctly scoped [2]" =
  run_file {|./tests/desugar_datatypes/shadowKinds.links|}

let%expect_test "Subkind declaration mismatches are reported" =
  run_expr {|() : (( | a :: Any)) { | a :: Eff }~> ()|}

let%expect_test "Subkind declaration mismatches are reported [forall]" =
  run_expr {|() : forall a::Base. a::Any|}

let%expect_test "Subkind declaration mismatches are reported [typename]" =
  run_expr {|typename A(a::Base) = a::Any;|}

let%expect_test "Kind declaration mismatches are reported" =
  run_expr {|() : forall a::Row. (a) ~a~> ()|}

let%expect_test "Kind usage mismatches are reported" =
  run_expr {|() : (a) ~a~> ()|}

let%expect_test "Quantifiers within nested definitions are allowed [1]" =
  run_file {|./tests/desugar_datatypes/nestedQuantifiers1.links|}

let%expect_test "Quantifiers within nested definitions are allowed [2]" =
  run_file {|./tests/desugar_datatypes/nestedQuantifiers2.links|}

let%expect_test "Qualified type variables default to `type' [typename]" =
  run_expr {|typename Arrow (a) = () -a-> ();|}

let%expect_test "Qualified type variables default to `type' [forall]" =
  run_expr {|sig f : forall a. () -a-> () fun f() {}|}

let%expect_test "Qualified type variables can infer their kind if enabled [typename]" =
  run_expr ~args:["--config=./tests/desugar_datatypes.tests.infer_kinds.config"] {|typename Arrow (a) = () -a-> ();|}

let%expect_test "Qualified type variables can infer their kind if enabled [forall]" =
  run_expr ~args:["--config=./tests/desugar_datatypes.tests.infer_kinds.config"] {|sig f : forall a. () -a-> () fun f() {}|}

let%expect_test "Free type variables are detected" =
  run_expr {|typename A = a;|}

let%expect_test "Implicit type variables are detected" =
  run_expr {|typename A = () -> ();|}

let%expect_test "Free type variables are detected (in nested definitions)" =
  run_file {|./tests/desugar_datatypes/nestedTypename.links|}

let%expect_test "Type aliases cannot repeat variables" =
  run_expr {|typename T(a::Type, a::Type) = a;|}

let%expect_test "foralls cannot repeat variables" =
  run_expr {|() : forall a::Type, a::Type. a|}

let%expect_test "Sugar for shared effect variables in functions" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun (f, x: Int) { f(x) + 0 }|}

let%expect_test "Sugar for shared effect variables in type applications (1)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|sig forever : (Comp(a)) ~> b fun forever(f) { ignore(f()); forever(f) } forever|}

let%expect_test "Sugar for shared effect variables in type applications (2)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|sig f : Comp(()) fun f() { } f|}

let%expect_test "Implicit effect variables are shared across different functions" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|(map, id) : (((a) -> b, [a]) -> [b], (c) -> c)|}

let%expect_test "Sugar for implicit effect variables in typenames" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|typename Comp(a) = () ~> a; fun() {} : Comp((), { | e})|}

let%expect_test "Sugar for implicit effect variables in typenames (propagates through types)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|mutual { typename Either(a) = [|A:() ~> a|B:Indirect(a)|]; typename Indirect(a) = Either(a); } A(fun() {}) : Either((), { | e})|}

