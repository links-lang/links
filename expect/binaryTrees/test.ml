open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Tree construction and postorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/postorder.links|};
  [%expect {|
    "[1,4,3,2]" : String
    exit: 0 |}]

let%expect_test "Tree construction and inorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/inorder.links|};
  [%expect {|
    "[1,2,3,4]" : String
    exit: 0 |}]

let%expect_test "Tree construction and preorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/preorder.links|};
  [%expect {|
    "[2,1,3,4]" : String
    exit: 0 |}]

let%expect_test "Deletion (1 node)" =
  run_file {|./tests/stdlibtests/binaryTree/remove1.links|};
  [%expect {|
    Node(Node(Leaf, 1, Leaf), 3, Node(Leaf, 4, Leaf)) : BinaryTree.BinaryTree (Int)
    exit: 0 |}]

let%expect_test "Deletion (2 nodes)" =
  run_file {|./tests/stdlibtests/binaryTree/remove2.links|};
  [%expect {|
    Node(Node(Node(Leaf, 1, Leaf), 2, Leaf), 4, Node(Node(Leaf, 1, Leaf), 2, Leaf)) : BinaryTree.BinaryTree (Int)
    exit: 0 |}]

