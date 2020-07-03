open Links_expect.Test_common
open Expect_test_common.File.Location


let config = Some ("tests/typed_ir.tests.config")

let%expect_test "Anonymous constant function (#693)" =
  run_expr ~config {|fun (x) {1}|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Anonymous identity function (#693)" =
  run_expr ~config {|fun (x) {x}|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Anonymous composition function (#693)" =
  run_expr ~config {|fun (f,g) {fun (x){f (g(x))}}|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Anonymous application function (#693)" =
  run_expr ~config {|fun (f) {fun (x){f (x)}}|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "XML typing (#693)" =
  run_expr ~config {|fun (x) {<a>{x}</a>}|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "ConcatMap (#368)" =
  run_file ~config {|tests/typed_ir/T368.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Quantifiers on let-bound values (#620)" =
  run_file ~config {|tests/typed_ir/T620.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Bound quantifiers in `and` and `all` (#692)" =
  run_file ~config {|tests/typed_ir/T692.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Bound quantifiers in a query (#694)" =
  run_file ~config {|tests/typed_ir/T694.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Curry (#574)" =
  run_file ~config {|tests/typed_ir/T574.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Unsafe type signatures #1 (#691)" =
  run_file ~config {|tests/typed_ir/T691a.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Unsafe type signatures #2 (#691)" =
  run_file ~config {|tests/typed_ir/T691b.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Wild effect compatibility #1 (#697)" =
  run_file ~config {|tests/typed_ir/T697a.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "Wild effect compatibility #2 (#697)" =
  run_file ~config {|tests/typed_ir/T697b.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "asList (#698)" =
  run_file ~config {|tests/typed_ir/T698.links|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

let%expect_test "isInt (#575)" =
  run_expr ~config {|sig isInt : (String) -> Bool fun isInt (x) { x =~ /-?[0-9]+$/ }|};
  [%expect {|
    exit: 2
    Fatal error: exception (Sys_error "tests/typed_ir.tests.config: No such file or directory") |}]

