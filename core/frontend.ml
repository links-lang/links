open Utility

(* Shall we re-run the frontend type-checker after each TransformSugar transformation? *)
let check_frontend_transformations =
  Settings.(flag "recheck_frontend_transformations"
            |> synopsis "Toggles whether to re-run the type checker after each (front end) transformation pass"
            |> convert parse_bool
            |> sync)

let check_frontend_transformations_dump =
  Settings.(flag "recheck_frontend_transformations_dump"
            |> synopsis "Toggles whether to dump the AST after each transformation pass"
            |> convert parse_bool
            |> sync)

let check_frontend_transformations_filter =
  Settings.(option ~default:(Some "all") "recheck_frontend_transformations_filter"
            |> convert Utility.some
            |> sync)

(* Print Sugar AST before frontend processing? *)
let show_pre_frontend_ast
  = Settings.(flag "show_pre_frontend_ast"
              |> synopsis "Dumps the undecorated front-end AST"
              |> convert parse_bool
              |> sync)

(* Print Sugar AST after frontend processing? *)
let show_post_frontend_ast
  = Settings.(flag "show_post_frontend_ast"
              |> synopsis "Dumps the decorated front-end AST"
              |> convert parse_bool
              |> sync)

let verify_transformation transformer =
  Settings.get check_frontend_transformations &&
    match Settings.get check_frontend_transformations_filter with
    | None | Some "" | Some "none" -> false
    | Some "all" -> true
    | Some filter -> String.split_on_char '\n' filter |> List.mem transformer


let trace_type_check_error name print before after stacktrace exn =
  Debug.f "Error during desugaring pass '%s'\n%!" name;
  let print x =
    let buffer = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buffer in
    Format.pp_set_margin formatter 120;
    print formatter x;
    Format.pp_print_flush formatter ();
    Buffer.contents buffer
  in
  Debug.if_set check_frontend_transformations_dump
    (fun () -> Printf.sprintf "Before %s:\n%s\n\n" name (print before));
  Debug.if_set check_frontend_transformations_dump
    (fun () -> Printf.sprintf "After %s:\n%s\n\n" name (print after));
  Printexc.raise_with_backtrace exn stacktrace


(* Alternative pipeline *)
type 'a result =
  { program: 'a;
    datatype: Types.datatype;
    context: Context.t }

module Pipeline': sig
  val program : Context.t ->
                Sugartypes.program ->
                Sugartypes.program result

  val interactive : Context.t ->
                    Sugartypes.sentence ->
                    Sugartypes.sentence result
end = struct
  let _show s program =
    Debug.print (s ^ ": " ^ Sugartypes.show_program program);
    program

  let _show_sentence s sentence =
    Debug.print (s ^ ": " ^ Sugartypes.show_sentence sentence);
    sentence

  open Transform

  module Untyped = struct
    type transformer = (module Untyped.S)

    (* This functor constructs a conditional transformer from a given
       transformer and a (side-effecting) condition. *)
    module Conditional(T : sig
                 include Untyped.S
                 val condition : unit -> bool
               end) = struct

      module Untyped = struct
        let program state program =
          if T.condition ()
          then T.Untyped.program state program
          else Identity.Untyped.program state program

        let sentence state sentence =
          if T.condition ()
          then T.Untyped.sentence state sentence
          else Identity.Untyped.sentence state sentence
      end
    end

    let only_if : bool Settings.setting -> (module Untyped.S) -> transformer
      = fun setting (module T) ->
      (module Conditional(struct include T let condition () = Settings.get setting end))

    (* Collection of transformers. *)
    let transformers : transformer array
      = [| (module ResolvePositions)
         ; (module CheckXmlQuasiquotes)
         ; (module DesugarModules)
         ; only_if Basicsettings.Sessions.exceptions_enabled (module DesugarSessionExceptions)
         ; (module DesugarLAttributes)
         ; (module LiftRecursive)
         ; (module DesugarDatatypes)
         |]

    let run : Context.t -> ((module Untyped.S) -> Context.t -> 'a -> 'a Transform.Untyped.result) -> 'a -> 'a result
      = fun context' select program ->
      let module TU = Transform.Untyped in
      let apply : 'a TU.result -> transformer -> 'a TU.result
        = fun (TU.Result { program; state }) (module T) ->
        select (module T) state program
      in
      let (TU.Result { state; program }) =
        Array.fold_left apply TU.(return context' program) transformers
      in
      { context = state; program;
        datatype = `Not_typed (* Slight abuse! *) }

    let run_sentence : Context.t ->
                       Sugartypes.sentence ->
                       Sugartypes.sentence result
      = fun context sentence ->
      run context (fun (module T) -> T.Untyped.sentence) sentence

    let run_program : Context.t ->
                      Sugartypes.program ->
                      Sugartypes.program result
      = fun context program ->
      run context (fun (module T) -> T.Untyped.program) program
  end

  module Typeability_preserving = struct
    type transformer = (module Typeable.S)

    (* This functor constructs a conditional transformer from a given
       transformer and a (side-effecting) condition. *)
    module Conditional(T : sig
                 include Typeable.S
                 val condition : unit -> bool
               end) = struct

      module Typeable = struct
        let program state program =
          if T.condition ()
          then T.Typeable.program state program
          else Identity.Typeable.program state program

        let sentence state sentence =
          if T.condition ()
          then T.Typeable.sentence state sentence
          else Identity.Typeable.sentence state sentence
      end
    end

    let only_if : bool Settings.setting -> (module Typeable.S) -> transformer
      = fun setting (module T) ->
      (module Conditional(struct include T let condition () = Settings.get setting end))

    (* let only_interactive : (module Typeable.S) -> transformer
     *   = fun (module T) -> only_if Basicsettings.interactive_mode (module T) *)

    (* Collection of transformers. *)
    let transformers : (string * transformer) array
      = [| "cp", (module DesugarCP)
         ; "inners", (module DesugarInners)
         ; "session_execeptions", only_if
                                    Basicsettings.Sessions.exceptions_enabled
                                    (module DesugarSessionExceptions)
         ; "processes", (module DesugarProcesses)
         ; "fors", (module DesugarFors)
         ; "regexes", (module DesugarRegexes)
         ; "formlets", (module DesugarFormlets)
         ; "pages", (module DesugarPages)
         ; "funs", (module DesugarFuns) |]

    (* Run program transformers. *)
    let run_program : Context.t ->
                      Types.datatype ->
                      Sugartypes.program ->
                      Sugartypes.program result
      = fun context' datatype program ->
      let apply : Sugartypes.program Transform.Typeable.result -> (string * transformer) -> Sugartypes.program Transform.Typeable.result
        = fun (Transform.Typeable.Result { state; program }) (name, (module T)) ->
        let (Transform.Typeable.Result payload) as result =
          T.Typeable.program state program
        in
        (if verify_transformation name then
           let tyenv =
             Context.typing_environment payload.state.Transform.Typeable.context
           in
           try
             ignore (TypeSugar.Check.program
                       { tyenv with Types.desugared = true }
                       payload.program)
           with _exn -> failwith "TODO");
        result
      in
      let (Transform.Typeable.Result { state; program }) =
        let initial_state = Transform.Typeable.{ datatype; context = context' } in
        Array.fold_left apply Transform.Typeable.(return initial_state program) transformers
      in
      { datatype = state.Transform.Typeable.datatype;
        context  = state.Transform.Typeable.context;
        program }

    (* Run sentence transformers. *)
    let run_sentence : Context.t ->
                       Types.datatype ->
                       Sugartypes.sentence ->
                       Sugartypes.sentence result
      = fun context' datatype program ->
      let apply : Sugartypes.sentence Transform.Typeable.result -> (string * transformer) -> Sugartypes.sentence Transform.Typeable.result
        = fun (Transform.Typeable.Result { state; program }) (name, (module T)) ->
        let (Transform.Typeable.Result payload) as result =
          T.Typeable.sentence state program
        in
        (if verify_transformation name then
           let tyenv =
             Context.typing_environment payload.state.Transform.Typeable.context
           in
           try
             ignore (TypeSugar.Check.sentence
                       { tyenv with Types.desugared = true }
                       payload.program)
           with _exn -> failwith "TODO");
        result
      in
      let (Transform.Typeable.Result { state; program }) =
        let initial_state = Transform.Typeable.{ datatype; context = context' } in
        Array.fold_left apply Transform.Typeable.(return initial_state program) transformers
      in
      { datatype = state.Transform.Typeable.datatype;
        context  = state.Transform.Typeable.context;
        program }
  end

  let program context program =
    (* Untyped transformations. *)
    let { program; context; _ } =
      Untyped.run_program context program
    in
    (* Typechecking. *)
    let (program, datatype, tyenv) =
      TypeSugar.Check.program Context.(typing_environment context) program
    in
    (* Typeability preserving transformations. *)
    let result =
      let open Typeability_preserving in
      let result = run_program context datatype program in
      { result with context = Context.{ context with typing_environment = tyenv } }
    in
    result

  let interactive context program =
    (* Untyped transformations. *)
    let { program; context; _ } =
      Untyped.run_sentence context program
    in
    (* Typechecking. *)
    let (program, datatype, tyenv) =
      TypeSugar.Check.sentence Context.(typing_environment context) program
    in
    (* Typeability preserving transformations. *)
    let result =
      let open Typeability_preserving in
      let result = run_sentence context datatype program in
      { result with context = Context.{ context with typing_environment = tyenv } }
    in
    result
end

(* TODO: Replace this 'Pipeline' by 'Pipeline''. *)
module Pipeline :
sig
  val program :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.program ->
    ((Sugartypes.program * Types.datatype * Types.typing_environment) * string list)
  val interactive :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.sentence ->
    Sugartypes.sentence * Types.datatype * Types.typing_environment
end
=
struct
  [@ocaml.warning "-23"]
  let program prev_tyenv pos_context program =
    if Settings.get show_pre_frontend_ast then
      Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_program program);

    let { program; datatype; context }  =
      Pipeline'.program Context.({ empty with
                                   typing_environment = prev_tyenv;
                                   source_code = pos_context})
        program
    in

    let ffi_files = ModuleUtils.get_ffi_files program in (* TODO associate FFI dependencies with their compilation units. *)

    if Settings.get show_post_frontend_ast then
      Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_program program);

    (program, datatype, (* typing_environment *) Context.(typing_environment context)), ffi_files


  let interactive prev_tyenv pos_context sentence =
    if Settings.get show_pre_frontend_ast then
      Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_sentence sentence);

    let { program = sentence; datatype; context } =
      Pipeline'.interactive Context.({ empty with
                                       typing_environment = prev_tyenv;
                                       source_code = pos_context })
        sentence
    in

    if Settings.get show_post_frontend_ast then
      Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_sentence sentence);

    (sentence, datatype, Context.(typing_environment context))


end
