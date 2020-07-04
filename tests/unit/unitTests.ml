open OUnit2

open Links_lens_unit_tests

let suites =
  "all"
  >::: [
         "lenses"
         >::: [
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
              ];
       ]

let () = run_test_tt_main suites
