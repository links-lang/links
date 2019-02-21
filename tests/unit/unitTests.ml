open OUnit2

let suites =
   "All" >::: [
       "Lenses" >::: [
            UnitTestsLensCommon.suite;
            UnitTestsLensFDHelpers.suite;
            UnitTestsLensDatabase.suite;
            UnitTestsLensSetOperations.suite;
            UnitTestsLensPerformance.suite;
            UnitTestsLensPrimitives.suite;
        ];
   ];;

let () =
   run_test_tt_main suites;;

