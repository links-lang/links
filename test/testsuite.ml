open OUnit2

let tests = "Migrated" >::: [
  "Hello World" >::
    (fun _ ->
      assert_equal "hello" "hello"
    );
]

let run_tests = run_test_tt_main tests
