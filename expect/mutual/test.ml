open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Lists (Correct)" =
  run_file {|./tests/mutual/list.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/list.links |}]

let%expect_test "Lists (Type argument mismatch)" =
  run_file {|./tests/mutual/listWrong.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/listWrong.links |}]

let%expect_test "Lists (Map)" =
  run_file {|./tests/mutual/listMap.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/listMap.links |}]

let%expect_test "Odd and even numbers (1)" =
  run_file {|./tests/mutual/oddEven.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/oddEven.links |}]

let%expect_test "Odd and even numbers (2)" =
  run_file {|./tests/mutual/oddOrEven.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/oddOrEven.links |}]

let%expect_test "Only functions and typenames in mutual blocks" =
  run_file {|./tests/mutual/badMutualBinding.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/badMutualBinding.links |}]

let%expect_test "Unguarded recursive applications disallowed (1)" =
  run_file {|./tests/mutual/unguarded1.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/unguarded1.links |}]

let%expect_test "Unguarded recursive applications disallowed (2)" =
  run_file {|./tests/mutual/unguarded2.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/unguarded2.links |}]

let%expect_test "Type variables not shared in a mutual block" =
  run_file {|./tests/mutual/tyvarSharingDisallowed.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/tyvarSharingDisallowed.links |}]

let%expect_test "Linearity (1)" =
  run_file {|./tests/mutual/linearity1.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/linearity1.links |}]

let%expect_test "Linearity (2)" =
  run_file {|./tests/mutual/linearity2.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/linearity2.links |}]

let%expect_test "Linearity (3)" =
  run_file {|./tests/mutual/linearity3.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/linearity3.links |}]

let%expect_test "Duplicate type bindings disallowed" =
  run_file {|./tests/mutual/duplicateType.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/duplicateType.links |}]

let%expect_test "Duplicate function bindings disallowed" =
  run_file {|./tests/mutual/duplicateFun.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/duplicateFun.links |}]

let%expect_test "Use structural unification if nominal unification fails" =
  run_file {|./tests/mutual/structural.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/structural.links |}]

let%expect_test "Use structural unification if nominal unification fails (type error)" =
  run_file {|./tests/mutual/structural2.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file ./tests/mutual/structural2.links |}]

