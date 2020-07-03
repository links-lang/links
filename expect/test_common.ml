open Core
include Expect_test_common.File.Location
include Expect_test_common.Expectation
include Expect_test_common.Expectation.Body

let () = Unix.chdir "../../../../"

let process_result proc =
  let open Core.Unix.Process_info in
  let res =
    Core.Unix.waitpid proc.pid in
  let res = match res with
    | Result.Ok () -> 0
    | Result.Error (`Exit_non_zero i) -> i
    | Result.Error (`Signal _) -> -1 in
  Core.Unix.in_channel_of_descr proc.stdout |> Stdio.In_channel.input_all |> Format.printf "%s";
  Core.Unix.in_channel_of_descr proc.stderr |> Stdio.In_channel.input_all |> Format.eprintf "%s";
  Format.printf "exit: %d\n" res

let run_expr ?(config=None) ?(args=[]) ?(pargs=[]) code =
  let pargs = if List.is_empty pargs then [] else "--" :: pargs in
  let config = Option.map ~f:(fun v -> "--config=" ^ v) config |> Option.to_list in
  let args = List.concat [config; args; ["-e"; code]; pargs] in
  let proc = Core.Unix.create_process ~prog:"./links" ~args in
  process_result proc

let run_file ?(config=None) ?(args=[]) ?(pargs=[]) code =
  let pargs = if List.is_empty pargs then [] else "--" :: pargs in
  let config = Option.map ~f:(fun v -> "--config=" ^ v) config |> Option.to_list in
  let args = List.concat [config; args; [code]; pargs] in
  let proc = Core.Unix.create_process ~prog:"./links" ~args in
  process_result proc
