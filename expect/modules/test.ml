open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Modules hide inner definitions" =
  run_file {|./tests/modules/basic-hide.links|};
  [%expect {|
    ./tests/modules/basic-hide.links:7: Type error: Unknown variable foo.
    In expression: foo.

    exit: 1 |}]

let%expect_test "Basic qualified binding resolution" =
  run_file {|./tests/modules/basic-qual-resolution.links|};
  [%expect {|
    "hello" : String
    exit: 0 |}]

let%expect_test "Inner module qualified binding resolution" =
  run_file {|./tests/modules/basic-inner-qual-resolution.links|};
  [%expect {|
    "hi" : String
    exit: 0 |}]

let%expect_test "Open allows basic unqualified binding access" =
  run_file {|./tests/modules/basic-open.links|};
  [%expect {|
    "hello!" : String
    exit: 0 |}]

let%expect_test "Module nesting allows partially-qualified names to be used for resolution" =
  run_file {|./tests/modules/basic-partial-qualification.links|};
  [%expect {|
    "hello" : String
    exit: 0 |}]

let%expect_test "Open allows partially-qualified names to be used for resolution" =
  run_file {|./tests/modules/basic-partial-qualification-open.links|};
  [%expect {|
    "hello" : String
    exit: 0 |}]

let%expect_test "Open still allows fully-qualified names to be used" =
  run_file {|./tests/modules/basic-open-fully-qual.links|};
  [%expect {|
    "hello" : String
    exit: 0 |}]

let%expect_test "Opened module does not shadow bindings after opening" =
  run_file {|./tests/modules/basic-open-shadow.links|};
  [%expect {|
    "hi" : String
    exit: 0 |}]

let%expect_test "Opened module shadows previous bindings after opening" =
  run_file {|./tests/modules/basic-open-no-shadow.links|};
  [%expect {|
    "greetings" : String
    exit: 0 |}]

let%expect_test "Cyclic dependencies outlawed" =
  run_file {|./tests/modules/runmulti cyclicA.links|};
  [%expect {|
    ***: Module Error: Could not find file ./tests/modules/runmulti cyclicA.links
    exit: 1 |}]

let%expect_test "Module chasing" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/moduleA.links|};
  [%expect {|
    "hello from c!" : String
    exit: 0 |}]

let%expect_test "Basic types in modules" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-sig.links|};
  [%expect {|
    5 : A.AInt
    exit: 0 |}]

let%expect_test "Basic type in session type" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/session-type.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Mutually-recursive functions (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn.links|};
  [%expect {|
    "Hello!Hello!" : String
    exit: 0 |}]

let%expect_test "Mutually-recursive functions (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn2.links|};
  [%expect {|
    "Hello!" : String
    exit: 0 |}]

let%expect_test "Mutually-recursive functions (3)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualfn3.links|};
  [%expect {|
    "Hello!" : String
    exit: 0 |}]

let%expect_test "Mutually-recursive types" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/mutualtys.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type signatures in inner modules" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/typesigInner.links|};
  [%expect {|
    11 : A.B.Foo
    exit: 0 |}]

let%expect_test "Infix operators" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/infix.links|};
  [%expect {|
    150 : Int
    exit: 0 |}]

let%expect_test "Lots of modules" =
  run_file ~args:["--path=tests/modules/overflow-test"] {|./tests/modules/overflow-test/test.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Constructor in module" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/constructor-test-good.links|};
  [%expect {|
    15 : Int
    exit: 0 |}]

let%expect_test "Constructor out of scope" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/constructor-test-bad.links|};
  [%expect {|
    ./tests/modules/constructor-test-bad.links:5: Unbound type constructor DT

    exit: 1 |}]

let%expect_test "Qualified names allowed without parentheses" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/qualified-type-names.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping1.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping2.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (3)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping3.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (4)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping4.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (5)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping5.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Scoping/Shadowing of type names (6)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping6.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Broken scoping of type names (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/type-scoping-bad1.links|};
  [%expect {|
    ./tests/modules/type-scoping-bad1.links:6: Unbound type constructor Foo

    exit: 1 |}]

let%expect_test "Import alien functions" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/alien_blocks_importer.links|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Structural labels (1)" =
  run_file {|./tests/modules/labels0.links|};
  [%expect {|
    "Hello!" : String
    exit: 0 |}]

let%expect_test "Structural labels (2)" =
  run_file {|./tests/modules/labels1.links|};
  [%expect {|
    ./tests/modules/labels1.links:8: Type error: The non-recursive function definition has return type
        `Int'
    but its annotation has return type
        `String'
    In expression: fun f(x) {
      switch(x) {
        case Bar(y) -> y
      }
    }.

    exit: 1 |}]

let%expect_test "Boundary peek" =
  run_file {|./tests/modules/boundary_peek.links|};
  [%expect {|
    "A1" : String
    exit: 0 |}]

let%expect_test "Import (1)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import1.links|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Import (2)" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import2.links|};
  [%expect {|
    ./tests/modules/import2.links:2: Type error: Unknown variable x.
    In expression: x.

    exit: 1 |}]

let%expect_test "Import via open" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/import_via_open0.links|};
  [%expect {|
    ./tests/modules/import_via_open0.links:2: Module Error: Unbound module Import_via_open1
    exit: 1 |}]

let%expect_test "Open is not include" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/open_is_not_include2.links|};
  [%expect {|
    [1, 2] : [Int]
    exit: 0 |}]

let%expect_test "Record accesses" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/records.links|};
  [%expect {|
    "LinksModules" : String
    exit: 0 |}]

let%expect_test "Switch expression" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/switch-test.links|};
  [%expect {|
    "hello!" : String
    exit: 0 |}]

let%expect_test "Qualified module reference" =
  run_file ~args:["--path=tests/modules"] {|./tests/modules/varRefA.links|};
  [%expect {|
    "hello from b!" : String
    exit: 0 |}]

