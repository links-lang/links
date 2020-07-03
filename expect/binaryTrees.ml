open Test_common
open Expect_test_common.File.Location


let%expect_test "Tree construction and postorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/postorder.links|}

let%expect_test "Tree construction and inorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/inorder.links|}

let%expect_test "Tree construction and preorder traversal" =
  run_file {|./tests/stdlibtests/binaryTree/preorder.links|}

let%expect_test "Deletion (1 node)" =
  run_file {|./tests/stdlibtests/binaryTree/remove1.links|}

