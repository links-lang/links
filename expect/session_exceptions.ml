open Test_common
open Expect_test_common.File.Location


let%expect_test "Receiving from cancelled peer endpoint" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel1.links|}

let%expect_test "Sending to cancelled peer endpoint" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel2.links|}

let%expect_test "Receiving from cancelled carried endpoint" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel3.links|}

let%expect_test "Nested exceptions" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel4.links|}

let%expect_test "Operation guarded by failing operation" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel5.links|}

let%expect_test "Operation guarded by failing operation (cp style)" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel6.links|}

let%expect_test "Cancellation in closure (1)" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel6.links|}

let%expect_test "Cancellation in closure (2)" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel7.links|}

let%expect_test "Cancellation in closure (3)" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel8.links|}

let%expect_test "Cancellation in closure (4)" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel9.links|}

let%expect_test "Non-empty continuation environments" =
  run_file ~args:["--session-exceptions"; "--enable-handlers"] {|./tests/session-exceptions/cancel10.links|}

