open OUnit2
open Links_core

let with_setting bool_setting value body =
  let prev = Settings.get bool_setting in
  Settings.set bool_setting value;
  let result = Lazy.force body in
  Settings.set bool_setting prev;
  result

(** Run program and return first error or None if no errors *)
let check_program_first_error ir_prog =
  (* For now, we run the IR type-checker with a completely
     empty environment, with no access to Lib or Prelude *)
  let dummy_context = Context.empty in
  let dummy_state =
    {
      (* The IR type-checker only uses the context field, the rest is irrelevant *)
      IrTransform.context = dummy_context;
      IrTransform.primitive_vars = Utility.IntSet.empty;
      IrTransform.datatype = Types.Not_typed;
    }
  in

  (* We tell the IR type-checker to fail with an exception on IR errors *)
  with_setting IrCheck.fail_on_ir_type_error true
    (lazy
      (try
         ignore (IrCheck.Typecheck.program dummy_state ir_prog);
         None
       with
      | e -> Some e))

let assert_failure msg ir_program =
  with_setting Types.print_types_pretty false
    (lazy
      (OUnit2.assert_failure
         (Printf.sprintf "%s\nIR program:\n%s" msg
            (Ir.show_computation ir_program))))

let assert_bool msg ir_program bool =
  with_setting Types.print_types_pretty false
    (lazy
      (OUnit2.assert_bool
         (Printf.sprintf "%s\nIR program:\n%s" msg
            (Ir.show_computation ir_program))
         bool))

let assemble_ir_program schinks_prog =
  try Schinks.reify schinks_prog with
  | Schinks_repr.SchinksError m ->
      OUnit2.assert_failure ("Assembling IR program failed with message:\n" ^ m)
  | e ->
      OUnit2.assert_failure
        ("Unhandled expection while assembing IR program :\n"
        ^ Printexc.to_string e)

let extract_ir_error_message exn_opt ir_program =
  match exn_opt with
  | None -> None
  | Some (Errors.IRTypeError msg) -> Some msg
  | Some exn ->
      assert_failure
        (Printf.sprintf
           "Got non-IR error while type-checking:\n%s\nBacktrace:\n%s"
           (Printexc.to_string exn)
           (Printexc.get_backtrace ()))
        ir_program

(* External interface *)

let expect_error ~name ~(error_regex : string) prog : OUnit2.test =
  let the_test _test_ctxt =
    let ir_prog = assemble_ir_program prog in
    let error_opt = check_program_first_error ir_prog in
    match extract_ir_error_message error_opt ir_prog with
    | None ->
        assert_failure
          "IR type-checker didn't report errors, although we were expecting \
           some"
          ir_prog
    | Some msg ->
        let regexp = Str.regexp error_regex in
        (* Unlike any other regex library in the world, OCaml doesn't offer a
           flag to make . match newlines, too. This would make stating the
           expected error regexes very cumbersome. Instead, we replace all \r and
           \n by spaces before matching. *)
        let non_newline_msg =
          Str.global_replace (Str.regexp "[\n\r]") " " msg
        in
        assert_bool
          ("Got the following IR error message, which did not match the \
            expected one:\n"
          ^ msg)
          ir_prog
          (Str.string_match regexp non_newline_msg 0)
  in
  name >:: the_test

let expect_no_errors ~name prog =
  let the_test _test_ctxt =
    let ir_prog = assemble_ir_program prog in
    let error_opt = check_program_first_error ir_prog in
    match extract_ir_error_message error_opt ir_prog with
    | None -> ()
    | Some msg ->
        assert_failure ("Excepted no IR errors, but got:\n" ^ msg) ir_prog
  in

  name >:: the_test

let mk_suite ~name test_cases = name >::: test_cases
