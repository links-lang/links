open Test_common
open Expect_test_common.File.Location


let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/calc.links|}

let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/reccalc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/reccalc.links|}

let%expect_test "Typecheck example file examples/AIPL14/LectureExamples/two-factor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/LectureExamples/two-factor.links|}

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem1.links|}

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem2.links|}

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem3.links|}

let%expect_test "Typecheck example file examples/AIPL14/SMTP/Problem4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/SMTP/Problem4.links|}

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem1.links|}

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem2.links|}

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem3.links|}

let%expect_test "Typecheck example file examples/AIPL14/Solutions/Problem4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/AIPL14/Solutions/Problem4.links|}

let%expect_test "Typecheck example file examples/buttons.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/buttons.links|}

let%expect_test "Typecheck example file examples/channels.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/channels.links|}

let%expect_test "Typecheck example file examples/church.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/church.links|}

let%expect_test "Typecheck example file examples/citations.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/citations.links|}

let%expect_test "Typecheck example file examples/client-server-prims.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/client-server-prims.links|}

let%expect_test "Typecheck example file examples/crop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/crop.links|}

let%expect_test "Typecheck example file examples/date.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/date.links|}

let%expect_test "Typecheck example file examples/dictionary/dictSuggest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggest.links|}

let%expect_test "Typecheck example file examples/dictionary/dictSuggestLite.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggestLite.links|}

let%expect_test "Typecheck example file examples/dictionary/dictSuggestUpdate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/dictionary/dictSuggestUpdate.links|}

let%expect_test "Typecheck example file examples/distribution/clientProcessOnServer.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientProcessOnServer.links|}

let%expect_test "Typecheck example file examples/distribution/clientProcessOnServer2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientProcessOnServer2.links|}

let%expect_test "Typecheck example file examples/distribution/clientToClient.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClient.links|}

let%expect_test "Typecheck example file examples/distribution/clientToClientDeleg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClientDeleg.links|}

let%expect_test "Typecheck example file examples/distribution/clientToClientViaServerAP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToClientViaServerAP.links|}

let%expect_test "Typecheck example file examples/distribution/clientToServerDeleg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/clientToServerDeleg.links|}

let%expect_test "Typecheck example file examples/distribution/serverToClient.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/serverToClient.links|}

let%expect_test "Typecheck example file examples/distribution/serverToClient2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/serverToClient2.links|}

let%expect_test "Typecheck example file examples/distribution/simpleServerAP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/distribution/simpleServerAP.links|}

let%expect_test "Typecheck example file examples/document-replace.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/document-replace.links|}

let%expect_test "Typecheck example file examples/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draggable.links|}

let%expect_test "Typecheck example file examples/draggableDb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draggableDb.links|}

let%expect_test "Typecheck example file examples/draw.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/draw.links|}

let%expect_test "Typecheck example file examples/extensible-case.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/extensible-case.links|}

let%expect_test "Typecheck example file examples/factorial.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/factorial.links|}

let%expect_test "Typecheck example file examples/ffi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/ffi.links|}

let%expect_test "Typecheck example file examples/filter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/filter.links|}

let%expect_test "Typecheck example file examples/formsTest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/formsTest.links|}

let%expect_test "Typecheck example file examples/games/breakout.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/breakout.links|}

let%expect_test "Typecheck example file examples/games/pacman.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/pacman.links|}

let%expect_test "Typecheck example file examples/games/tetris.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/tetris.links|}

let%expect_test "Typecheck example file examples/games/twentyfortyeight.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/games/twentyfortyeight.links|}

let%expect_test "Typecheck example file examples/handlers/alert.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/alert.links|}

let%expect_test "Typecheck example file examples/handlers/aop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/aop.links|}

let%expect_test "Typecheck example file examples/handlers/aop2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/aop2.links|}

let%expect_test "Typecheck example file examples/handlers/coins_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/coins_web.links|}

let%expect_test "Typecheck example file examples/handlers/choose.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/choose.links|}

let%expect_test "Typecheck example file examples/handlers/coins.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/coins.links|}

let%expect_test "Typecheck example file examples/handlers/concurrency.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/concurrency.links|}

let%expect_test "Typecheck example file examples/handlers/count_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/count_web.links|}

let%expect_test "Typecheck example file examples/handlers/deep_state.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/deep_state.links|}

let%expect_test "Typecheck example file examples/handlers/exceptions.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/exceptions.links|}

let%expect_test "Typecheck example file examples/handlers/fringe.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/fringe.links|}

let%expect_test "Typecheck example file examples/handlers/identity.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/identity.links|}

let%expect_test "Typecheck example file examples/handlers/lambda.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/lambda.links|}

let%expect_test "Typecheck example file examples/handlers/light_switch.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/light_switch.links|}

let%expect_test "Typecheck example file examples/handlers/monadic_reflection.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/monadic_reflection.links|}

let%expect_test "Typecheck example file examples/handlers/nim.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/nim.links|}

let%expect_test "Typecheck example file examples/handlers/nim-webversion.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/nim-webversion.links|}

let%expect_test "Typecheck example file examples/handlers/number_games.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/number_games.links|}

let%expect_test "Typecheck example file examples/handlers/pi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/pi.links|}

let%expect_test "Typecheck example file examples/handlers/pipes.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/pipes.links|}

let%expect_test "Typecheck example file examples/handlers/racing-lines.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/racing-lines.links|}

let%expect_test "Typecheck example file examples/handlers/sat.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sat.links|}

let%expect_test "Typecheck example file examples/handlers/shiftreset.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/shiftreset.links|}

let%expect_test "Typecheck example file examples/handlers/shallow_state.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/shallow_state.links|}

let%expect_test "Typecheck example file examples/handlers/sierpinski-triangle.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sierpinski-triangle.links|}

let%expect_test "Typecheck example file examples/handlers/sudoku.links" =
  run_file ~args:["--config=tests/examples_effect_sugar.config"] {|examples/handlers/sudoku.links|}

let%expect_test "Typecheck example file examples/handlers/tests.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/tests.links|}

let%expect_test "Typecheck example file examples/handlers/toggle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/toggle.links|}

let%expect_test "Typecheck example file examples/handlers/toggle_web.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/toggle_web.links|}

let%expect_test "Typecheck example file examples/handlers/transaction.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/transaction.links|}

let%expect_test "Typecheck example file examples/handlers/unhandled.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/unhandled.links|}

let%expect_test "Typecheck example file examples/handlers/u2_puzzle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/handlers/u2_puzzle.links|}

let%expect_test "Typecheck example file examples/href-test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/href-test.links|}

let%expect_test "Typecheck example file examples/initialise-list.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/initialise-list.links|}

let%expect_test "Typecheck example file examples/insert-factorials.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/insert-factorials.links|}

let%expect_test "Typecheck example file examples/loginFlow.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/loginFlow.links|}

let%expect_test "Typecheck example file examples/mandelbrot.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelbrot.links|}

let%expect_test "Typecheck example file examples/mandelcolor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelcolor.links|}

let%expect_test "Typecheck example file examples/mandelopt.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mandelopt.links|}

let%expect_test "Typecheck example file examples/multiple.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/multiple.links|}

let%expect_test "Typecheck example file examples/page-test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/page-test.links|}

let%expect_test "Typecheck example file examples/paginate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/paginate.links|}

let%expect_test "Typecheck example file examples/progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/progress.links|}

let%expect_test "Typecheck example file examples/progressiveQuery.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/progressiveQuery.links|}

let%expect_test "Typecheck example file examples/relational_lenses/cds.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/relational_lenses/cds.links|}

let%expect_test "Typecheck example file examples/serverClientProcess.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/serverClientProcess.links|}

let%expect_test "Typecheck example file examples/serverHandler.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/serverHandler.links|}

let%expect_test "Typecheck example file examples/sessions/ap-multi-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/ap-multi-client.links|}

let%expect_test "Typecheck example file examples/sessions/atm.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/atm.links|}

let%expect_test "Typecheck example file examples/sessions/bookshop-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshop-cp.links|}

let%expect_test "Typecheck example file examples/sessions/bookshop.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshop.links|}

let%expect_test "Typecheck example file examples/sessions/bookshopAppServer.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/bookshopAppServer.links|}

let%expect_test "Typecheck example file examples/sessions/calc-client-server.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-client-server.links|}

let%expect_test "Typecheck example file examples/sessions/calc-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-client.links|}

let%expect_test "Typecheck example file examples/sessions/calc-cp-endbang.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-cp-endbang.links|}

let%expect_test "Typecheck example file examples/sessions/calc-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-cp.links|}

let%expect_test "Typecheck example file examples/sessions/calc-endbang.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc-endbang.links|}

let%expect_test "Typecheck example file examples/sessions/calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/calc.links|}

let%expect_test "Typecheck example file examples/sessions/chatserver/chatClient.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/chatserver"] {|examples/sessions/chatserver/chatClient.links|}

let%expect_test "Typecheck example file examples/sessions/chatserver/chatServer.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/chatserver"] {|examples/sessions/chatserver/chatServer.links|}

let%expect_test "Typecheck example file examples/sessions/chatserver/chatSessions.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/sessions/chatserver/chatSessions.links|}

let%expect_test "Typecheck example file examples/sessions/chatserver/linearList.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/chatserver/linearList.links|}

let%expect_test "Typecheck example file examples/sessions/choice.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/choice.links|}

let%expect_test "Typecheck example file examples/sessions/counter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/counter.links|}

let%expect_test "Typecheck example file examples/sessions/draggable-boring.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/draggable-boring.links|}

let%expect_test "Typecheck example file examples/sessions/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/draggable.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_1.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_2.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_3.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Bookshop_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Bookshop_CP.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Draggable_Lists_1.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Draggable_Lists_1.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Draggable_Lists_2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Draggable_Lists_2.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/Extended_Calculator.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/Extended_Calculator.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/IRC.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/IRC.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/IRC_Test.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/IRC_Test.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Real_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Real_CP.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Real_GV.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Real_GV.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Web_Model_CP.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Web_Model_CP.links|}

let%expect_test "Typecheck example file examples/sessions/Elvina/SMTP_Web_Model_GV.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/Elvina/SMTP_Web_Model_GV.links|}

let%expect_test "Typecheck example file examples/sessions/end.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/end.links|}

let%expect_test "Typecheck example file examples/sessions/forkbomb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/forkbomb.links|}

let%expect_test "Typecheck example file examples/sessions/fuse-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fuse-cp.links|}

let%expect_test "Typecheck example file examples/sessions/fuse.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fuse.links|}

let%expect_test "Typecheck example file examples/sessions/fusegg.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/fusegg.links|}

let%expect_test "Typecheck example file examples/sessions/givengrab.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/givengrab.links|}

let%expect_test "Typecheck example file examples/sessions/gng-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/gng-client.links|}

let%expect_test "Typecheck example file examples/sessions/gng-cp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/gng-cp.links|}

let%expect_test "Typecheck example file examples/sessions/linear_if.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/linear_if.links|}

let%expect_test "Typecheck example file examples/sessions/pi.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/pi.links|}

let%expect_test "Typecheck example file examples/sessions/reccalc-client.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/reccalc-client.links|}

let%expect_test "Typecheck example file examples/sessions/reccalc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/reccalc.links|}

let%expect_test "Typecheck example file examples/sessions/sim-calc.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/sim-calc.links|}

let%expect_test "Typecheck example file examples/sessions/smtp-dummy.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-dummy.links|}

let%expect_test "Typecheck example file examples/sessions/smtp-end.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-end.links|}

let%expect_test "Typecheck example file examples/sessions/smtp-factored-more.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-factored-more.links|}

let%expect_test "Typecheck example file examples/sessions/smtp-factored.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-factored.links|}

let%expect_test "Typecheck example file examples/sessions/smtp-formlet.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp-formlet.links|}

let%expect_test "Typecheck example file examples/sessions/smtp.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/smtp.links|}

let%expect_test "Typecheck example file examples/sessions/state.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/state.links|}

let%expect_test "Typecheck example file examples/sessions/two-factor-cp.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/sessions/two-factor-cp.links|}

let%expect_test "Typecheck example file examples/sessions/two-factor.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/two-factor.links|}

let%expect_test "Typecheck example file  examples/Shopping-Cart.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/Shopping-Cart.links|}

let%expect_test "Typecheck example file examples/shredding.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding.links|}

let%expect_test "Typecheck example file examples/shredding2.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding2.links|}

let%expect_test "Typecheck example file examples/shredding3.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding3.links|}

let%expect_test "Typecheck example file examples/shredding4.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/shredding4.links|}

let%expect_test "Typecheck example file examples/silly-progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/silly-progress.links|}

let%expect_test "Typecheck example file examples/test-prims.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/test-prims.links|}

let%expect_test "Typecheck example file examples/todo.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/todo.links|}

let%expect_test "Typecheck example file examples/todoDb.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/todoDb.links|}

let%expect_test "Typecheck example file examples/TicTacToe-Console.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/TicTacToe-Console.links|}

let%expect_test "Typecheck example file examples/ToDo-List-Styles.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/ToDo-List-Styles.links|}

let%expect_test "Typecheck example file examples/toggle.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/toggle.links|}

let%expect_test "Typecheck example file examples/validate.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/validate.links|}

let%expect_test "Typecheck example file examples/webserver/buttons.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/buttons.links|}

let%expect_test "Typecheck example file examples/webserver/counter.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/counter.links|}

let%expect_test "Typecheck example file examples/webserver/draggable-sessions.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/draggable-sessions.links|}

let%expect_test "Typecheck example file examples/webserver/draggable.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/draggable.links|}

let%expect_test "Typecheck example file examples/webserver/examples-nodb.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples"] {|examples/webserver/examples-nodb.links|}

let%expect_test "Typecheck example file examples/webserver/examples.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples"] {|examples/webserver/examples.links|}

let%expect_test "Typecheck example file examples/webserver/progress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/webserver/progress.links|}

let%expect_test "Typecheck example file examples/webserver/wsocket_conn.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"] {|examples/webserver/wsocket_conn.links|}

let%expect_test "Typecheck example file examples/wine.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/wine.links|}

let%expect_test "Typecheck example file examples/sessions/mind/mindServer.links" =
  run_file ~args:["--config=tests/examples_session_exceptions.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindServer.links|}

let%expect_test "Typecheck example file examples/sessions/mind/linearList.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/sessions/mind/linearList.links|}

let%expect_test "Typecheck example file examples/sessions/mind/mindClient.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindClient.links|}

let%expect_test "Typecheck example file examples/sessions/mind/mindSessions.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/sessions/mind"] {|examples/sessions/mind/mindSessions.links|}

let%expect_test "Typecheck example file examples/mvu/mousetest.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/mousetest.links|}

let%expect_test "Typecheck example file examples/mvu/time.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/time.links|}

let%expect_test "Typecheck example file examples/mvu/pong.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/mvu/todomvc"] {|examples/mvu/pong.links|}

let%expect_test "Typecheck example file examples/mvu/todomvc/todoMVC.links" =
  run_file ~args:["--config=tests/examples_default.config"; "--path=examples/mvu/todomvc"] {|examples/mvu/todomvc/todoMVC.links|}

let%expect_test "Typecheck example file examples/mvu/keypress.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/mvu/keypress.links|}

let%expect_test "Typecheck example file examples/examplesPage.links" =
  run_file ~args:["--config=tests/examples_default.config"] {|examples/examplesPage.links|}

