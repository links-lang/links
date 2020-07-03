open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Effect sugar: Implicit effect variables (1)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun (f, x: Int) { f(x) + 0 } : ((Int) -e-> Int, Int) -e-> Int|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Implicit effect variables (2)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun (f)(x: Int) { f(x) + 0 } : ((Int) -e-> Int) -f-> (Int) -e-> Int|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Implicit effect variables (3)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|(map, id) : (((a) -e-> b, [a]) -e-> [b], (c) -e-> c)|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Implicit effect variables (4)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun(_) {} : (() {F:()|_}-> ()) {F:()|_} -> ()|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Single-use variables" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun(_) { } : (() -e-> ()) -f-> ()|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Shared variables" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun()() { } : () -a-> () -a-> ()|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Distinct variables" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|(fun(){}, fun(){}) : (() -a-> (), () -b-> ())|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Type aliases (1)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun() {} : Comp((), { | e })|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

let%expect_test "Effect sugar: Type aliases (2)" =
  run_expr ~args:["--config=tests/effect_sugar.config"] {|fun()() {} : () -e-> Comp((), { | f })|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/effect_sugar.config: No such file or directory") |}]

