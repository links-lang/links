open OUnit2

let suites =
  "all"
  >::: [
         ("lenses"
         >:::
         let open Links_lens_unit_tests in
         [
           UnitTestsLensLang.suite;
           UnitTestsLensCommon.suite;
           Fun_dep_tests.suite;
           Database_utility_test.suite;
           UnitTestsLensSetOperations.suite;
           UnitTestsLensPerformance.suite;
           Lens_primitives_tests.suite;
           UnitTestsLensLang.suite;
           UnitTestsLensSort.suite;
           Alias_tests.suite;
         ]);
       ]

let () = run_test_tt_main suites
