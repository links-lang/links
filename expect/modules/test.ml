open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Modules hide inner definitions" =
  run_file {|./tests/modules/basic-hide.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-hide.links |}]

let%expect_test "Basic qualified binding resolution" =
  run_file {|./tests/modules/basic-qual-resolution.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-qual-resolution.links |}]

let%expect_test "Inner module qualified binding resolution" =
  run_file {|./tests/modules/basic-inner-qual-resolution.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-inner-qual-resolution.links |}]

let%expect_test "Open allows basic unqualified binding access" =
  run_file {|./tests/modules/basic-open.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-open.links |}]

let%expect_test "Module nesting allows partially-qualified names to be used for resolution" =
  run_file {|./tests/modules/basic-partial-qualification.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-partial-qualification.links |}]

let%expect_test "Open allows partially-qualified names to be used for resolution" =
  run_file {|./tests/modules/basic-partial-qualification-open.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-partial-qualification-open.links |}]

let%expect_test "Open still allows fully-qualified names to be used" =
  run_file {|./tests/modules/basic-open-fully-qual.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-open-fully-qual.links |}]

let%expect_test "Opened module does not shadow bindings after opening" =
  run_file {|./tests/modules/basic-open-shadow.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-open-shadow.links |}]

let%expect_test "Opened module shadows previous bindings after opening" =
  run_file {|./tests/modules/basic-open-no-shadow.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/basic-open-no-shadow.links |}]

let%expect_test "Cyclic dependencies outlawed" =
  run_file {|./tests/modules/runmulti cyclicA.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/runmulti cyclicA.links |}]

let%expect_test "Module chasing" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/moduleA.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/moduleA.links |}]

let%expect_test "Basic types in modules" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-sig.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-sig.links |}]

let%expect_test "Basic type in session type" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/session-type.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/session-type.links |}]

let%expect_test "Mutually-recursive functions (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/mutualfn.links |}]

let%expect_test "Mutually-recursive functions (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn2.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/mutualfn2.links |}]

let%expect_test "Mutually-recursive functions (3)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn3.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/mutualfn3.links |}]

let%expect_test "Mutually-recursive types" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualtys.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/mutualtys.links |}]

let%expect_test "Type signatures in inner modules" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/typesigInner.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/typesigInner.links |}]

let%expect_test "Infix operators" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/infix.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/infix.links |}]

let%expect_test "Lots of modules" =
  run_file ~args:["--path=tests/modules/overflow-test"] {|./tests/modules/overflow-test/test.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/overflow-test/test.links |}]

let%expect_test "Constructor in module" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/constructor-test-good.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/constructor-test-good.links |}]

let%expect_test "Constructor out of scope" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/constructor-test-bad.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/constructor-test-bad.links |}]

let%expect_test "Qualified names allowed without parentheses" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/qualified-type-names.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/qualified-type-names.links |}]

let%expect_test "Scoping/Shadowing of type names (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping1.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping1.links |}]

let%expect_test "Scoping/Shadowing of type names (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping2.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping2.links |}]

let%expect_test "Scoping/Shadowing of type names (3)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping3.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping3.links |}]

let%expect_test "Scoping/Shadowing of type names (4)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping4.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping4.links |}]

let%expect_test "Scoping/Shadowing of type names (5)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping5.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping5.links |}]

let%expect_test "Scoping/Shadowing of type names (6)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping6.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping6.links |}]

let%expect_test "Broken scoping of type names (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping-bad1.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/type-scoping-bad1.links |}]

let%expect_test "Import alien functions" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/alien_blocks_importer.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/alien_blocks_importer.links |}]

let%expect_test "Structural labels (1)" =
  run_file {|./tests/modules/labels0.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/labels0.links |}]

let%expect_test "Structural labels (2)" =
  run_file {|./tests/modules/labels1.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/labels1.links |}]

let%expect_test "Boundary peek" =
  run_file {|./tests/modules/boundary_peek.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/boundary_peek.links |}]

let%expect_test "Import (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import1.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/import1.links |}]

let%expect_test "Import (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import2.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/import2.links |}]

let%expect_test "Import via open" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import_via_open0.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/import_via_open0.links |}]

let%expect_test "Open is not include" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/open_is_not_include2.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/open_is_not_include2.links |}]

let%expect_test "Record accesses" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/records.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/records.links |}]

let%expect_test "Switch expression" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/switch-test.links|};
  [%expect {| exit: 1***: Module Error: Could not find file ./tests/modules/switch-test.links |}]

