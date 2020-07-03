open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/calc.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/reccalc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/reccalc.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/two-factor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/two-factor.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem1.links|};
  [%expect {|
    exit: 1
    examples/AIPL14/SMTP/Problem1.links:58: Type error: Variable c has linear type
        `~SMTPServer'
    but is used 0 times.
    In expression: fun mailClient(c) {
      # To implement!
    }. |}]

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem2.links|};
  [%expect {|
    exit: 1
    examples/AIPL14/SMTP/Problem2.links:61: Type error: Variable c has linear type
        `~SMTPServer'
    but is used 0 times.
    In expression: fun mailClient(c) {
      # To implement!
    }. |}]

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem3.links|};
  [%expect {|
    exit: 1
    examples/AIPL14/SMTP/Problem3.links:71: Type error: Variable c has linear type
        `~SMTPServer'
    but is used 0 times.
    In expression: fun mailClient(c) {
      # To implement.
    }. |}]

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem4.links|};
  [%expect {|
    exit: 1
    examples/AIPL14/SMTP/Problem4.links:94: Type error: Variable c has linear type
        `~SMTPServer'
    but is used 0 times.
    In expression: fun mailClient(c) {
      # To implement!
    }. |}]

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem1.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem3.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem4.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/buttons.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/buttons.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/channels.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/channels.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/church.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/church.links|};
  [%expect {|
    exit: 1
    examples/church.links:13: Type error: The function
        `succ'
    has type
        `(Nat) {}-> Nat'
    while the arguments passed to it have types
        `Nat'
    and the currently allowed effects are
        `wild'
    In expression: succ(zero). |}]

let%expect_test "Typecheck example file examples/citations.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/citations.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/client-server-prims.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/client-server-prims.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/crop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/crop.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/date.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/date.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/dictionary/dictSuggest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggest.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/dictionary/dictSuggestLite.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggestLite.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/dictionary/dictSuggestUpdate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggestUpdate.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientProcessOnServer.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientProcessOnServer.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientProcessOnServer2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientProcessOnServer2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientToClient.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClient.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientToClientDeleg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClientDeleg.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientToClientViaServerAP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClientViaServerAP.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/clientToServerDeleg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToServerDeleg.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/serverToClient.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/serverToClient.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/serverToClient2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/serverToClient2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/distribution/simpleServerAP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/simpleServerAP.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/document-replace.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/document-replace.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draggable.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/draggableDb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draggableDb.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/draw.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draw.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/extensible-case.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/extensible-case.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/factorial.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/factorial.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/ffi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/ffi.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/filter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/filter.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/formsTest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/formsTest.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/games/breakout.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/breakout.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/games/pacman.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/pacman.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/games/tetris.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/tetris.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/games/twentyfortyeight.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/twentyfortyeight.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/alert.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/alert.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/aop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/aop.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/aop2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/aop2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/coins_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/coins_web.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/choose.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/choose.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/handlers/choose.links:18

      var positive = handler(m) {
                                  ^ |}]

let%expect_test "Typecheck example file examples/handlers/coins.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/coins.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/concurrency.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/concurrency.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/handlers/concurrency.links:75

      handler scheduleBreadthFirst {
              ^ |}]

let%expect_test "Typecheck example file examples/handlers/count_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/count_web.links|};
  [%expect {|
    exit: 1
    :0: Kind mismatch: Type argument 1 for type constructor Comp has kind Row, but an argument of kind Type was expected.
    In:
    <dummy> |}]

let%expect_test "Typecheck example file examples/handlers/deep_state.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/deep_state.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/exceptions.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/exceptions.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/fringe.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/fringe.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/identity.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/identity.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/lambda.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/lambda.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/light_switch.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/light_switch.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/monadic_reflection.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/monadic_reflection.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/nim.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/nim.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/handlers/nim.links:76

      handler naive(m) {
              ^ |}]

let%expect_test "Typecheck example file examples/handlers/nim-webversion.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/nim-webversion.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/number_games.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/number_games.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/pi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/pi.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/pipes.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/pipes.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/racing-lines.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/racing-lines.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/sat.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sat.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/shiftreset.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/shiftreset.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/shallow_state.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/shallow_state.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/sierpinski-triangle.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sierpinski-triangle.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/sudoku.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sudoku.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/tests.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/tests.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file deep_state.links |}]

let%expect_test "Typecheck example file examples/handlers/toggle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/toggle.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/handlers/toggle.links:44

          case otherwise -> error("Input '" ^^ s ^^ "' was not recognised as a boolean value.")
                         ^ |}]

let%expect_test "Typecheck example file examples/handlers/toggle_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/toggle_web.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/handlers/toggle_web.links:44

          case otherwise -> error("Input '" ^^ s ^^ "' was not recognised as a boolean value.")
                         ^ |}]

let%expect_test "Typecheck example file examples/handlers/transaction.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/transaction.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/unhandled.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/unhandled.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/handlers/u2_puzzle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/u2_puzzle.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/href-test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/href-test.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/initialise-list.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/initialise-list.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/insert-factorials.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/insert-factorials.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/loginFlow.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/loginFlow.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mandelbrot.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelbrot.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mandelcolor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelcolor.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mandelopt.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelopt.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/multiple.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/multiple.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/page-test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/page-test.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/paginate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/paginate.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/progress.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/progressiveQuery.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/progressiveQuery.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/relational_lenses/cds.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/relational_lenses/cds.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/serverClientProcess.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/serverClientProcess.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/serverHandler.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/serverHandler.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/ap-multi-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/ap-multi-client.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/atm.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/atm.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/bookshop-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshop-cp.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/bookshop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshop.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/bookshopAppServer.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshopAppServer.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc-client-server.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-client-server.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-client.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc-cp-endbang.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-cp-endbang.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-cp.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc-endbang.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-endbang.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/chatserver/chatClient.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/chatserver"] {|examples/sessions/chatserver/chatClient.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/chatserver/chatServer.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/chatserver"] {|examples/sessions/chatserver/chatServer.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/chatserver/chatSessions.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/sessions/chatserver/chatSessions.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/chatserver/linearList.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/chatserver/linearList.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/choice.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/choice.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/counter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/counter.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/draggable-boring.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/draggable-boring.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/sessions/draggable-boring.links:51

            manage(<|case s {
                          ^ |}]

let%expect_test "Typecheck example file examples/sessions/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/draggable.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_1.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_3.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_CP.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Draggable_Lists_1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Draggable_Lists_1.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Draggable_Lists_2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Draggable_Lists_2.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/Extended_Calculator.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Extended_Calculator.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/IRC.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/IRC.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/IRC_Test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/IRC_Test.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Real_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Real_CP.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Real_GV.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Real_GV.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Web_Model_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Web_Model_CP.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Web_Model_GV.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Web_Model_GV.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/end.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/end.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/forkbomb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/forkbomb.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/fuse-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fuse-cp.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/fuse.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fuse.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/fusegg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fusegg.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/givengrab.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/givengrab.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/gng-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/gng-client.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/gng-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/gng-cp.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/linear_if.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/linear_if.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/pi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/pi.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/reccalc-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/reccalc-client.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/reccalc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/reccalc.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/sim-calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/sim-calc.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp-dummy.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-dummy.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp-end.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-end.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp-factored-more.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-factored-more.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp-factored.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-factored.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp-formlet.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-formlet.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/smtp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/state.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/state.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/two-factor-cp.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/sessions/two-factor-cp.links|};
  [%expect {|
    exit: 1
    examples/sessions/two-factor-cp.links:103: Type error: The function
        `runSync'
    has type
        `((!(Data).EndBang) {SessionFail:() {}-> [||]}~> EndBang) ~a~> Data'
    while the arguments passed to it have types
        `(!(Data).EndBang) ~b~> EndBang'
    and the currently allowed effects are
        `|wild|b'
    In expression: runSync (fun (return) {<| nu x.({serve(x, "secret data")} | {user(x, return)}) |>}). |}]

let%expect_test "Typecheck example file examples/sessions/two-factor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/two-factor.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file  examples/Shopping-Cart.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/Shopping-Cart.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/shredding.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding.links|};
  [%expect {|
    exit: 1
    examples/shredding.links:26: Type error: Iterations over tables are only allowed in tame contexts.
        This iteration has ambient effect
        `{}'
    but the currently allowed effects are
        `{|a}'
    In expression: for (x <-- departments)
        [(name=x.dept,
          employees=
            for (y <-- employees) where (x.dept == y.dept)
              [(name=y.employee,
                tasks=
                  for (z <-- tasks)
                  where (y.employee == z.employee)
                    [z.task],
                salary=y.salary)],
          contacts=
           for (y <-- contacts) where (x.dept == y.dept)
             [(name=y.contact, "client"=y."client")])]. |}]

let%expect_test "Typecheck example file examples/shredding2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding2.links|};
  [%expect {|
    exit: 1
    examples/shredding2.links:26: Type error: Iterations over tables are only allowed in tame contexts.
        This iteration has ambient effect
        `{}'
    but the currently allowed effects are
        `{|a}'
    In expression: for (x <-- departments)
        [(name=x.dept,
          employees=
            for (y <-- employees) where (x.dept == y.dept)
              [(name=y.employee,
                tasks=
                  for (z <-- tasks)
                  where (y.employee == z.employee)
                    [z.task],
                salary=y.salary)],
          contacts=
           for (y <-- contacts) where (x.dept == y.dept)
             [(name=y.contact, "client"=y."client")])]. |}]

let%expect_test "Typecheck example file examples/shredding3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding3.links|};
  [%expect {|
    exit: 1
    examples/shredding3.links:20: Type error: Iterations over tables are only allowed in tame contexts.
        This iteration has ambient effect
        `{}'
    but the currently allowed effects are
        `{|a}'
    In expression: for (x <-- departments)
        [(name=x.dept,
          employees=
            for (y <-- employees) where (x.dept == y.dept)
              [(name=y.employee,
                tasks=
                  for (z <-- tasks)
                  where (y.employee == z.employee)
                    [z.task],
                salary=y.salary)],
          contacts=
           for (y <-- contacts) where (x.dept == y.dept)
             [(name=y.contact, "client"=y."client")])]. |}]

let%expect_test "Typecheck example file examples/shredding4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding4.links|};
  [%expect {|
    exit: 1
    examples/shredding4.links:20: Type error: Iterations over tables are only allowed in tame contexts.
        This iteration has ambient effect
        `{}'
    but the currently allowed effects are
        `{|a}'
    In expression: for (x <-- departments)
        [(name=x.dept,
          employees=
            for (y <-- employees) where (x.dept == y.dept)
              [(name=y.employee,
                tasks=
                  for (z <-- tasks)
                  where (y.employee == z.employee)
                    [z.task],
                salary=y.salary)],
          contacts=
           for (y <-- contacts) where (x.dept == y.dept)
             [(name=y.contact, "client"=y."client")])]. |}]

let%expect_test "Typecheck example file examples/silly-progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/silly-progress.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/test-prims.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/test-prims.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/todo.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/todo.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/todoDb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/todoDb.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/TicTacToe-Console.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/TicTacToe-Console.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/ToDo-List-Styles.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/ToDo-List-Styles.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/toggle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/toggle.links|};
  [%expect {|
    exit: 1
    ***: Parse error: examples/toggle.links:21

          case otherwise -> error("Input '" ^^ s ^^ "' was not recognised as a boolean value.")
                         ^ |}]

let%expect_test "Typecheck example file examples/validate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/validate.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/buttons.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/buttons.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/counter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/counter.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/draggable-sessions.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/draggable-sessions.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/draggable.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/examples-nodb.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples"] {|examples/webserver/examples-nodb.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file breakout.links |}]

let%expect_test "Typecheck example file examples/webserver/examples.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples"] {|examples/webserver/examples.links|};
  [%expect {|
    exit: 1
    ***: Module Error: Could not find file breakout.links |}]

let%expect_test "Typecheck example file examples/webserver/progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/progress.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/webserver/wsocket_conn.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/webserver/wsocket_conn.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/wine.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/wine.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/mind/mindServer.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindServer.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/mind/linearList.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/mind/linearList.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/mind/mindClient.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindClient.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/sessions/mind/mindSessions.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindSessions.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mvu/mousetest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/mousetest.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mvu/time.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/time.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mvu/pong.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/mvu/todomvc"] {|examples/mvu/pong.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mvu/todomvc/todoMVC.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/mvu/todomvc"] {|examples/mvu/todomvc/todoMVC.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/mvu/keypress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/keypress.links|};
  [%expect {| exit: 0 |}]

let%expect_test "Typecheck example file examples/examplesPage.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/examplesPage.links|};
  [%expect {| exit: 0 |}]

