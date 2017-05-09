(*pp deriving *)

open Debug
open Json
open OUnit2
open Types
open Value
open Utility

let _ = Settings.set_value Debug.debugging_enabled true


let test_jsonize_value_list_empty test_ctx = 
   assert_equal "[]" (jsonize_value (`List []))

let test_jsonize_value_list_int test_ctx = 
   assert_equal "[5]" (jsonize_value (`List [`Int 5]))

let suite =
   "json" >:::
      [
         "jsonize_value_list_empty" >:: test_jsonize_value_list_empty;
         "jsonize_value_list_int" >:: test_jsonize_value_list_int;
      ];;

let () =
    run_test_tt_main suite;;
