open Test_common
open Expect_test_common.File.Location


let%expect_test "Iteration" =
  run_expr {|for (i <- [1,2,3,4,5]) if (i == 3) [] else [i, i]|}

let%expect_test "Where clause" =
  run_expr {|for (i <- [1,2,3,4,5]) where (i <> 3) [i, i]|}

let%expect_test "Concatenation/union" =
  run_expr {|[1,2,3] ++ [4,5]|}

let%expect_test "Head and tail" =
  run_expr {|hd(['a','b','c']) == 'a'|}

let%expect_test "Equality" =
  run_expr {|[1,2,3] == [1,2,3] && [1,2,3] <> [2,1,3]|}

let%expect_test "Cons" =
  run_expr {|1 :: 2 :: 3 :: 4 :: []|}

let%expect_test "Let-patterns [1]" =
  run_expr {|{var x :: xs = [1,2,3] ; x}|}

let%expect_test "Let-patterns [2]" =
  run_expr {|{var (x :: y :: xs, z) = ([1,2,3],4) ; (x,y)}|}

let%expect_test "Let-patterns [3]" =
  run_expr {|{var ((v,w) :: (x,y) :: z) = [(1,2),(3,4)] ; (w,x)}|}

let%expect_test "Let-patterns [4]" =
  run_expr {|{var (x,y) :: [] = [(1,2),(3,4),(4,5)] ; (x,y)}|}

let%expect_test "Let-patterns [5]" =
  run_expr {|{var [(x,y)] = [(1,2),(3,4),(4,5)] ; (x,y)}|}

let%expect_test "Let-patterns [6]" =
  run_expr {|{var [] = [1,2,3] ; ()}|}

let%expect_test "Let-patterns [7]" =
  run_expr {|{var [x] = [] ; ()}|}

let%expect_test "Let-patterns [8]" =
  run_expr {|{var x::y = [] ; ()}|}

let%expect_test "Case-patterns [1]" =
  run_expr {|switch ([]) { case [] -> 1 }|}

let%expect_test "Case-patterns [2]" =
  run_expr {|switch ([]) { case x::xs -> 1 }|}

let%expect_test "Case-patterns [3]" =
  run_expr {|switch ([]) { case [] -> 2 case x -> 1 }|}

let%expect_test "Case-patterns [4]" =
  run_expr {|switch ([]) { case x::xs -> 1 case x -> 2 }|}

let%expect_test "Case-patterns [5]" =
  run_expr {|switch ([1]) { case x::xs -> 1  case x -> 2 }|}

let%expect_test "Case-patterns [6]" =
  run_expr {|switch ([1,3]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }|}

let%expect_test "Case-patterns [7]" =
  run_expr {|switch ([1]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }|}

let%expect_test "Case-patterns [8]" =
  run_expr {|switch ([1,3]) { case x::y::[] -> 3 case x::y -> 2 case x -> 1 }|}

let%expect_test "Case-patterns [9]" =
  run_expr {|switch ([1]) { case x::[] -> 2 case x -> 1 }|}

let%expect_test "Case-patterns (redundancy) [10]" =
  run_expr {|fun (x) { switch (x) { case ([], []) -> 1 case (x::xs, y::ys) -> 2 case ([], _) -> 3 case (_, []) -> 4 }}|}

let%expect_test "Case-patterns (singleton list pattern)" =
  run_expr {|switch ([1]) { case [x] -> 2 case x -> 1 }|}

let%expect_test "Case patterns (with redefined hd)" =
  run_expr {|{ fun hd(_) { 1 } switch (['a']) { case [y] -> y }}|}

let%expect_test "With parentheses:" =
  run_expr {|switch ([1]) { case (x::xs) -> 1  case x -> 2 }|}

let%expect_test "Length function" =
  run_expr {|fun len (l) { switch (l) { case [] -> 0 case x::xs -> 1 + len(xs) } } len ([1,2,3])|}

let%expect_test "Map function" =
  run_expr {|map(curry((+))(1), [1,2,3])|}

let%expect_test "Sorting:" =
  run_expr {|for (i <- [2,1,3]) orderby (i) [i]|}

let%expect_test "Sorting:" =
  run_expr {|for (i <- [2,1,3]) orderby (-i) [i]|}

let%expect_test "Empty-list comparison (1)" =
  run_expr {|[] < []|}

let%expect_test "Empty-list comparison (2)" =
  run_expr {|[] > []|}

let%expect_test "List comparison (1)" =
  run_expr {|[1] < []|}

let%expect_test "List comparison (2)" =
  run_expr {|[1] > []|}

let%expect_test "Nullary for comprehensions" =
  run_expr {|for () [1]|}

