open OUnit2

let suites = 
   "All" >::: [
      UnitTestsJson.suite;
   ];;

let () =
   run_test_tt_main suites;;
