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
         ( "ir"
         >:::
         let open Links_ir_unit_tests in
         [
           Binding.suite;
           Functions.suite;
           Effects.suite;
           Quantifiers.suite;
           Closures.suite;
         ] );
       ]

let () = run_test_tt_main suites
