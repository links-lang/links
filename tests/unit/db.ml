open OUnit2

let suites =
  "all"
  >::: [
         ( "lenses"
         >:::
         let open Links_lens_unit_tests in
         [
           UnitTestsLensLang.suite;
           UnitTestsLensCommon.suite;
           UnitTestsLensFDHelpers.suite;
           UnitTestsLensDatabase.suite;
           UnitTestsLensSetOperations.suite;
           UnitTestsLensPerformance.suite;
           UnitTestsLensPrimitives.suite;
           UnitTestsLensLang.suite;
           UnitTestsLensSort.suite;
           UnitTestsLensAlias.suite;
         ] );
       ]

let () = run_test_tt_main suites
