open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Addition and multiplication [1]" =
  run_expr {|10 + 20 * 30|};
  [%expect {|
    610 : Int
    exit: 0 |}]

let%expect_test "Addition and multiplication [2]" =
  run_expr {|20 * 30 + 10|};
  [%expect {|
    610 : Int
    exit: 0 |}]

let%expect_test "Plus and multiplication [3]" =
  run_expr {|(10 + 20) * 30|};
  [%expect {|
    900 : Int
    exit: 0 |}]

let%expect_test "Subtraction is left associative [1]" =
  run_expr {|3 - 2 - 1|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Subtraction is left associative [1]" =
  run_expr {|(3 - 2) - 1|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Subtraction is left associative [3]" =
  run_expr {|3 - (2 - 1)|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Unary minus" =
  run_expr {|(-1)|};
  [%expect {|
    -1 : Int
    exit: 0 |}]

let%expect_test "Unary float minus" =
  run_expr {|(-.1.0)|};
  [%expect {|
    -1.0 : Float
    exit: 0 |}]

let%expect_test "Addition, division, subtraction, and multiplication" =
  run_expr {|100 + 200 / 10 - 3 * 10|};
  [%expect {|
    90 : Int
    exit: 0 |}]

let%expect_test "Relational, logical, and arithmetic [1]" =
  run_expr {|88 > 32 && 42 <= 100 || 100 < 88 + 32 && 30 * 3 + 1 > 90|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Relational, logical, and arithmetic [1]" =
  run_expr {|((88 > 32) && (42 <= 100)) || ((100 < 88 + 32) && (30 * 3 + 1 > 90))|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Exponentiation and unary minus (TODO: result may be surprising. Decide whether to change precedence)" =
  run_expr {|-2^4|};
  [%expect {|
    16 : Int
    exit: 0 |}]

let%expect_test "Arithmetic [1]" =
  run_expr {|3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Arithmetic [2]" =
  run_expr {|3 + ((4 * 2) / (( 1 - 5 ) ^ (2 ^ 3)))|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Arithmetic [3]" =
  run_expr {|2 / ( 1 - 5 ) ^ 2 ^ 3 * 4 + 3|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Arithmetic [4]" =
  run_expr {|3 + 2 / ( 1 - 5 ) ^ 2 ^ 3 * 4|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Arithmetic and function calls." =
  run_expr {|sin((switch (max([2.0,3.0])) { case Some(f) -> f case None -> 0.0 }) /. 3.0 *. 3.14)|};
  [%expect {|
    0.00159265291649 : Float
    exit: 0 |}]

let%expect_test "Infix application" =
  run_expr {|5 `elem` [1,2,3,4] || 3 `elem` [3] && 2 `elem` [1,2,3,4]|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Cons (TODO: surprising that parentheses are required. Decide whether to change precedence)" =
  run_expr {|(2 ^ 0) :: (2 ^ 1) :: (2 ^ 2) :: (2 ^ 3) :: []|};
  [%expect {|
    [1, 2, 4, 8] : [Int]
    exit: 0 |}]

let%expect_test "List concatenation" =
  run_expr {|1 :: [] ++ [] ++ 2 :: 3 :: 4 :: [] ++ 5 :: []|};
  [%expect {|
    [1, 2, 3, 4, 5] : [Int]
    exit: 0 |}]

