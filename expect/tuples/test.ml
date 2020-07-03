open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Tuple printing and typing" =
  run_expr {|(1,"boo")|};
  [%expect {|
    (1, "boo") : (Int, String)
    exit: 0 |}]

let%expect_test "Tuple equality" =
  run_expr {|(1,"foo") == (1, "foo")|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Tuple inequality" =
  run_expr {|(1,"foo") == (2, "foo")|};
  [%expect {|
    false : Bool
    exit: 0 |}]

let%expect_test "Tuple comparisons [exhaustive]" =
  run_expr {|((1,"foo") < (2, "foo")) || ((1,"foo") > (2, "foo")) || ((1,"foo") == (2, "foo"))|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Tuple comparisons [exclusive]" =
  run_expr {|(((1,"foo") < (2, "foo")) == false) || (((1,"foo") > (2, "foo")) == false)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Tuple patterns [match]" =
  run_expr {|{var ((x,y,(z,a,b)), c) = (("string",3.21,(15,[1,2,3],(3,2,1))), ());  z}|};
  [%expect {|
    15 : Int
    exit: 0 |}]

let%expect_test "Tuple patterns [no match]" =
  run_expr {|{var ((x,y,(z,a,b)), c) = (("string",3.21,(15,(3,2,1))), ());  z}|};
  [%expect {|
    exit: 1
    <string>:1: Type error: The binder must match the type of the body in a value binding, but the pattern
        `((x,y,(z,a,b)), c)'
    has type
        `((String, Float, (a::Any, b::Any, c::Any)), d::Any)'
    while the expression
        `(("string",3.21,(15,(3,2,1))), ())'
    has type
        `((String, Float, (Int, (Int, Int, Int))), ())'
    In expression: var ((x,y,(z,a,b)), c) = (("string",3.21,(15,(3,2,1))), ());. |}]

let%expect_test "Tuple/record interchangeability" =
  run_expr {|(1,"two",3) == (2="two", 3=3,1=1)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Tuple extension" =
  run_expr {|{var x = (1,"two"); (3='3'|x) }|};
  [%expect {|
    (1, "two", '3') : (Int, String, Char)
    exit: 0 |}]

let%expect_test "Tuple projection" =
  run_expr {|((1,"two",3).2 == "two")|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "1-tuples" =
  run_expr {|(1="one")|};
  [%expect {|
    (1 = "one") : (1:String)
    exit: 0 |}]

