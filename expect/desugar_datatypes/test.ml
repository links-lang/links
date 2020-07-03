open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Type variables are correctly scoped [1]" =
  run_expr {|typename A = forall a::Eff. forall a::Type. () ~a~> ();|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Row' and `a::Type'.
    In expression: () ~a~> (). |}]

let%expect_test "Type variables are correctly scoped [2]" =
  run_file {|./tests/desugar_datatypes/shadowKinds.links|};
  [%expect {|
    exit: 1
    ./tests/desugar_datatypes/shadowKinds.links:2: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Type' and `a::Row'.
    In expression: (a). |}]

let%expect_test "Subkind declaration mismatches are reported" =
  run_expr {|() : (( | a :: Any)) { | a :: Eff }~> ()|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Row(Unl,Eff)' and `a::Row(Any,Any)'.
    In expression: (( | a :: Any)) { | a :: Eff }~> (). |}]

let%expect_test "Subkind declaration mismatches are reported [forall]" =
  run_expr {|() : forall a::Base. a::Any|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Type(Any,Any)' and `a::Type(Unl,Base)'.
    In expression: a::Any. |}]

let%expect_test "Subkind declaration mismatches are reported [typename]" =
  run_expr {|typename A(a::Base) = a::Any;|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Type(Any,Any)' and `a::Type(Unl,Base)'.
    In expression: a::Any. |}]

let%expect_test "Kind declaration mismatches are reported" =
  run_expr {|() : forall a::Row. (a) ~a~> ()|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Type' and `a::Row'.
    In expression: a. |}]

let%expect_test "Kind usage mismatches are reported" =
  run_expr {|() : (a) ~a~> ()|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Row' and `a::Type'.
    In expression: (a) ~a~> (). |}]

let%expect_test "Quantifiers within nested definitions are allowed [1]" =
  run_file {|./tests/desugar_datatypes/nestedQuantifiers1.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Quantifiers within nested definitions are allowed [2]" =
  run_file {|./tests/desugar_datatypes/nestedQuantifiers2.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Qualified type variables default to `type' [typename]" =
  run_expr {|typename Arrow (a) = () -a-> ();|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Row' and `a::Type'.
    In expression: () -a-> (). |}]

let%expect_test "Qualified type variables default to `type' [forall]" =
  run_expr {|sig f : forall a. () -a-> () fun f() {}|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Mismatch in kind for type variable `a'.
      Declared as `a::Row' and `a::Type'.
    In expression: () -a-> (). |}]

let%expect_test "Qualified type variables can infer their kind if enabled [typename]" =
  run_expr ~args:["--config=./tests/desugar_datatypes.tests.infer_kinds.config"] {|typename Arrow (a) = () -a-> ();|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Qualified type variables can infer their kind if enabled [forall]" =
  run_expr ~args:["--config=./tests/desugar_datatypes.tests.infer_kinds.config"] {|sig f : forall a. () -a-> () fun f() {}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Free type variables are detected" =
  run_expr {|typename A = a;|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unbound type variable `a' in position where
            no free type variables are allowed
    In expression: a. |}]

let%expect_test "Implicit type variables are detected" =
  run_expr {|typename A = () -> ();|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unbound anonymous type variable in position where
            no free type variables are allowed
    In expression: () -> (). |}]

let%expect_test "Free type variables are detected (in nested definitions)" =
  run_file {|./tests/desugar_datatypes/nestedTypename.links|};
  [%expect {|
    exit: 1
    ./tests/desugar_datatypes/nestedTypename.links:3: Type error: Unbound type variable `a' in position where
            no free type variables are allowed
    In expression: a. |}]

let%expect_test "Type aliases cannot repeat variables" =
  run_expr {|typename T(a::Type, a::Type) = a;|};
  [%expect {|
    exit: 1
    :0: Type error: Multiple definitions of type variable `a'.
    In expression: <dummy>. |}]

let%expect_test "foralls cannot repeat variables" =
  run_expr {|() : forall a::Type, a::Type. a|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Multiple definitions of type variable `a'.
    In expression: forall a::Type, a::Type. a. |}]

let%expect_test "Sugar for shared effect variables in functions" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun (f, x: Int) { f(x) + 0 }|};
  [%expect {|
    fun : ((Int) -> Int, Int) -> Int
    exit: 0 |}]

let%expect_test "Sugar for shared effect variables in type applications (1)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|sig forever : (Comp(a)) ~> b fun forever(f) { ignore(f()); forever(f) } forever|};
  [%expect {|
    fun : (Comp (_)) ~> _
    exit: 0 |}]

let%expect_test "Sugar for shared effect variables in type applications (2)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|sig f : Comp(()) fun f() { } f|};
  [%expect {|
    fun : Comp (())
    exit: 0 |}]

let%expect_test "Implicit effect variables are shared across different functions" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|(map, id) : (((a) -> b, [a]) -> [b], (c) -> c)|};
  [%expect {|
    (fun, fun) : (((a) -> c, [a]) -> [c], (d) -> d)
    exit: 0 |}]

let%expect_test "Sugar for implicit effect variables in typenames" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|typename Comp(a) = () ~> a; fun() {} : Comp((), { | e})|};
  [%expect {|
    fun : Comp (())
    exit: 0 |}]

let%expect_test "Sugar for implicit effect variables in typenames (propagates through types)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|mutual { typename Either(a) = [|A:() ~> a|B:Indirect(a)|]; typename Indirect(a) = Either(a); } A(fun() {}) : Either((), { | e})|};
  [%expect {|
    A(fun) : Either (())
    exit: 0 |}]

