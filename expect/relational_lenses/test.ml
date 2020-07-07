open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Relational lenses bad put" =
  run_file {|./tests/relational-lenses/bad-put.links|};
  [%expect {|
    ./tests/relational-lenses/bad-put.links:9: Relational lenses are not enabled. To enable relational lenses set the `relational_lenses' setting to `true'.
    exit: 1 |}]

