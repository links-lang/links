open Test_common
open Expect_test_common.File.Location


let%expect_test "Tuple printing and typing" =
  run_expr {|(1,"boo")|}

let%expect_test "Tuple equality" =
  run_expr {|(1,"foo") == (1, "foo")|}

let%expect_test "Tuple inequality" =
  run_expr {|(1,"foo") == (2, "foo")|}

let%expect_test "Tuple comparisons [exhaustive]" =
  run_expr {|((1,"foo") < (2, "foo")) || ((1,"foo") > (2, "foo")) || ((1,"foo") == (2, "foo"))|}

let%expect_test "Tuple comparisons [exclusive]" =
  run_expr {|(((1,"foo") < (2, "foo")) == false) || (((1,"foo") > (2, "foo")) == false)|}

let%expect_test "Tuple patterns [match]" =
  run_expr {|{var ((x,y,(z,a,b)), c) = (("string",3.21,(15,[1,2,3],(3,2,1))), ());  z}|}

let%expect_test "Tuple patterns [no match]" =
  run_expr {|{var ((x,y,(z,a,b)), c) = (("string",3.21,(15,(3,2,1))), ());  z}|}

let%expect_test "Tuple/record interchangeability" =
  run_expr {|(1,"two",3) == (2="two", 3=3,1=1)|}

let%expect_test "Tuple extension" =
  run_expr {|{var x = (1,"two"); (3='3'|x) }|}

let%expect_test "Tuple projection" =
  run_expr {|((1,"two",3).2 == "two")|}

let%expect_test "1-tuples" =
  run_expr {|(1="one")|}

