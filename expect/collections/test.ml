open Links_expect.Test_common
open Expect_test_common.File.Location


let%expect_test "Iteration" =
  run_expr {|for (i <- [1,2,3,4,5]) if (i == 3) [] else [i, i]|};
  [%expect {|
    [1, 1, 2, 2, 4, 4, 5, 5] : [Int]

    exit: 0 |}]

let%expect_test "Where clause" =
  run_expr {|for (i <- [1,2,3,4,5]) where (i <> 3) [i, i]|};
  [%expect {|
    [1, 1, 2, 2, 4, 4, 5, 5] : [Int]

    exit: 0 |}]

let%expect_test "Concatenation/union" =
  run_expr {|[1,2,3] ++ [4,5]|};
  [%expect {|
    [1, 2, 3, 4, 5] : [Int]

    exit: 0 |}]

let%expect_test "Head and tail" =
  run_expr {|hd(['a','b','c']) == 'a'|};
  [%expect {|
    true : Bool

    exit: 0 |}]

let%expect_test "Equality" =
  run_expr {|[1,2,3] == [1,2,3] && [1,2,3] <> [2,1,3]|};
  [%expect {|
    true : Bool

    exit: 0 |}]

let%expect_test "Cons" =
  run_expr {|1 :: 2 :: 3 :: 4 :: []|};
  [%expect {|
    [1, 2, 3, 4] : [Int]

    exit: 0 |}]

let%expect_test "Let-patterns [1]" =
  run_expr {|{var x :: xs = [1,2,3] ; x}|};
  [%expect {|
    1 : Int

    exit: 0 |}]

let%expect_test "Let-patterns [2]" =
  run_expr {|{var (x :: y :: xs, z) = ([1,2,3],4) ; (x,y)}|};
  [%expect {|
    (1, 2) : (Int, Int)

    exit: 0 |}]

let%expect_test "Let-patterns [3]" =
  run_expr {|{var ((v,w) :: (x,y) :: z) = [(1,2),(3,4)] ; (w,x)}|};
  [%expect {|
    (2, 3) : (Int, Int)

    exit: 0 |}]

let%expect_test "Let-patterns [4]" =
  run_expr {|{var (x,y) :: [] = [(1,2),(3,4),(4,5)] ; (x,y)}|};
  [%expect {| exit: 1***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Let-patterns [5]" =
  run_expr {|{var [(x,y)] = [(1,2),(3,4),(4,5)] ; (x,y)}|};
  [%expect {| exit: 1***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Let-patterns [6]" =
  run_expr {|{var [] = [1,2,3] ; ()}|};
  [%expect {| exit: 1***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Let-patterns [7]" =
  run_expr {|{var [x] = [] ; ()}|};
  [%expect {| exit: 1***: Runtime error: hd() of empty list |}]

let%expect_test "Let-patterns [8]" =
  run_expr {|{var x::y = [] ; ()}|};
  [%expect {| exit: 1***: Runtime error: hd() of empty list |}]

let%expect_test "Case-patterns [1]" =
  run_expr {|switch ([]) { case [] -> 1 }|};
  [%expect {|
    1 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [2]" =
  run_expr {|switch ([]) { case x::xs -> 1 }|};
  [%expect {| exit: 1***: Error: Links_core.Evalir.Exceptions.Wrong |}]

let%expect_test "Case-patterns [3]" =
  run_expr {|switch ([]) { case [] -> 2 case x -> 1 }|};
  [%expect {|
    2 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [4]" =
  run_expr {|switch ([]) { case x::xs -> 1 case x -> 2 }|};
  [%expect {|
    2 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [5]" =
  run_expr {|switch ([1]) { case x::xs -> 1  case x -> 2 }|};
  [%expect {|
    1 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [6]" =
  run_expr {|switch ([1,3]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }|};
  [%expect {|
    3 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [7]" =
  run_expr {|switch ([1]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }|};
  [%expect {|
    2 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [8]" =
  run_expr {|switch ([1,3]) { case x::y::[] -> 3 case x::y -> 2 case x -> 1 }|};
  [%expect {|
    3 : Int

    exit: 0 |}]

let%expect_test "Case-patterns [9]" =
  run_expr {|switch ([1]) { case x::[] -> 2 case x -> 1 }|};
  [%expect {|
    2 : Int

    exit: 0 |}]

let%expect_test "Case-patterns (redundancy) [10]" =
  run_expr {|fun (x) { switch (x) { case ([], []) -> 1 case (x::xs, y::ys) -> 2 case ([], _) -> 3 case (_, []) -> 4 }}|};
  [%expect {|
    fun : (([_], [_])) -> Int

    exit: 0 |}]

let%expect_test "Case-patterns (singleton list pattern)" =
  run_expr {|switch ([1]) { case [x] -> 2 case x -> 1 }|};
  [%expect {|
    2 : Int

    exit: 0 |}]

let%expect_test "Case patterns (with redefined hd)" =
  run_expr {|{ fun hd(_) { 1 } switch (['a']) { case [y] -> y }}|};
  [%expect {|
    'a' : Char

    exit: 0 |}]

let%expect_test "With parentheses:" =
  run_expr {|switch ([1]) { case (x::xs) -> 1  case x -> 2 }|};
  [%expect {|
    1 : Int

    exit: 0 |}]

let%expect_test "Length function" =
  run_expr {|fun len (l) { switch (l) { case [] -> 0 case x::xs -> 1 + len(xs) } } len ([1,2,3])|};
  [%expect {|
    3 : Int

    exit: 0 |}]

let%expect_test "Map function" =
  run_expr {|map(curry((+))(1), [1,2,3])|};
  [%expect {|
    [2, 3, 4] : [Int]

    exit: 0 |}]

let%expect_test "Sorting:" =
  run_expr {|for (i <- [2,1,3]) orderby (i) [i]|};
  [%expect {|
    [1, 2, 3] : [Int]

    exit: 0 |}]

let%expect_test "Sorting:" =
  run_expr {|for (i <- [2,1,3]) orderby (-i) [i]|};
  [%expect {|
    [3, 2, 1] : [Int]

    exit: 0 |}]

let%expect_test "Empty-list comparison (1)" =
  run_expr {|[] < []|};
  [%expect {|
    false : Bool

    exit: 0 |}]

let%expect_test "Empty-list comparison (2)" =
  run_expr {|[] > []|};
  [%expect {|
    false : Bool

    exit: 0 |}]

let%expect_test "List comparison (1)" =
  run_expr {|[1] < []|};
  [%expect {|
    false : Bool

    exit: 0 |}]

let%expect_test "List comparison (2)" =
  run_expr {|[1] > []|};
  [%expect {|
    true : Bool

    exit: 0 |}]

let%expect_test "Nullary for comprehensions" =
  run_expr {|for () [1]|};
  [%expect {|
    [1] : [Int]

    exit: 0 |}]

