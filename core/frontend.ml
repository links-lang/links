open Utility
open Transform

let _show s program =
  Debug.print (s ^ ": " ^ Sugartypes.show_program program);
  program

let _show_sentence s sentence =
  Debug.print (s ^ ": " ^ Sugartypes.show_sentence sentence);
  sentence

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


let trace_type_error transform_name print program program' stacktrace exn =
  Debug.f "error: transformation pass '%s' produced an ill-typed program.\n%!" transform_name;
  let print x =
    let buffer = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buffer in
    Format.pp_set_margin formatter 120;
    print formatter x;
    Format.pp_print_flush formatter ();
    Buffer.contents buffer
  in
  Debug.if_set check_frontend_transformations_dump
    (fun () ->
      Printf.sprintf "Program before transformation:\n%s\n\nProgram after transformation:\n%s\n\n"
        (print program) (print program'));
  Printexc.raise_with_backtrace exn stacktrace


(* Pipeline infrastructure. *)
type 'a result =
  { program: 'a;
    datatype: Types.datatype;
    context: Context.t }



(* The [Untyped] module contains the infrastructure for untyped
   transform passes. *)
module Untyped = struct
  type transformer = (module Untyped.S)

  (* This functor constructs a conditional transformer from a given
     transformer and a (side-effecting) condition. *)
  module Conditional(T : sig
               include Untyped.S
               val condition : unit -> bool
             end) = struct

    module Untyped = struct
      let name = Printf.sprintf "Conditional(%s)" T.Untyped.name

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

  module Collect_FFI_Files : Untyped.S = struct
    open Untyped
    module Untyped = struct
      let name = "collect_ffi_files"

      let program state program =
        let ffi_files' = ModuleUtils.get_ffi_files program in
        let context'  = context state in
        let context'' = Context.({ context' with ffi_files = ffi_files' }) in
        return context'' program

      let sentence state sentence =
        return state sentence (* TODO FIXME bug. A sentence can contain an alien declaration. *)
    end
  end

  (* Collection of transformers. *)
  let transformers : transformer array
    = [| (module ResolvePositions)
       ; (module CheckXmlQuasiquotes)
       ; (module DesugarSwitchFuns)
       ; (module DesugarModules)
       ; (module Shunting)
       ; (module Collect_FFI_Files)
       ; only_if Basicsettings.Sessions.exceptions_enabled (module DesugarSessionExceptions)
       ; (module DesugarLAttributes)
       ; (module LiftRecursive)
       ; (module DesugarTypeVariables)
       ; (module DesugarEffects)
       ; (module DesugarDatatypes)
      |]

  let run : Context.t -> ((module Untyped.S) -> Context.t -> 'a -> 'a Transform.Untyped.result) -> 'a -> 'a result
    = fun context' select program ->
    let module TU = Transform.Untyped in
    let apply : 'a TU.result -> transformer -> 'a TU.result
      = fun (TU.Result { program; state }) (module T) ->
        Debug.if_set Basicsettings.show_stages (fun () -> T.Untyped.name ^"...");
        select (module T) state program
    in
    let (TU.Result { state; program }) =
      Array.fold_left apply TU.(return context' program) transformers
    in
    { context = state; program;
      datatype = Types.Not_typed (* Slight abuse! *) }

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

(* The [Typeability_preserving] module runs transformation passes
   which _must_ preserve typeability, i.e. the preimage and image of
   the transform must be typeable. *)
module Typeability_preserving = struct
  type transformer = (module Typeable.S)

  (* This functor constructs a conditional transformer from a given
       transformer and a (side-effecting) condition. *)
  module Conditional(T : sig
               include Typeable.S
               val condition : unit -> bool
             end) = struct

    module Typeable = struct
      let name = Printf.sprintf "Conditional(%s)" T.Typeable.name

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
    (module Conditional(struct
                include T
                let condition () = Settings.get setting end))

  (* Collection of transformers. *)
  let transformers : transformer array
    = [| (module DesugarInners) (* this must always be done first after type inference *)
       ; (module DesugarCP)
       ; only_if
           Basicsettings.Sessions.exceptions_enabled
           (module DesugarSessionExceptions)
       ; (module DesugarProcesses)
       ; (module DesugarFors)
       ; (module DesugarRegexes)
       ; (module DesugarFormlets)
       ; (module DesugarPages)
       ; (module DesugarFuns) |]

  (* Run program transformers. *)
  let run_program : Context.t ->
                    Types.datatype ->
                    Sugartypes.program ->
                    Sugartypes.program result
    = fun context' datatype program ->
    let apply : Sugartypes.program Transform.Typeable.result -> transformer -> Sugartypes.program Transform.Typeable.result
      = fun (Transform.Typeable.Result { state; program }) (module T) ->
      let (Transform.Typeable.Result payload) as result =
        Debug.if_set Basicsettings.show_stages (fun () -> T.Typeable.name ^"...");
        T.Typeable.program state program
      in
      if verify_transformation T.Typeable.name then
         let tyenv =
           Context.typing_environment payload.state.Transform.Typeable.context
         in
         (* TODO(dhil): Ultimately we may want to move from
            typeability preserving transformations to type-preserving
            transformations in which case the type checker should
            check *against* the datatype in transformation state
            [payload]. *)
         try
           let (program, datatype, tyenv') =
             TypeSugar.Check.program
                     { tyenv with Types.desugared = true }
                     payload.program in
                  (* TODO(dhil): Verify post-transformation invariants. *)
           let context = { payload.state.Transform.Typeable.context with
                           Context.typing_environment = Types.extend_typing_environment tyenv tyenv' } in
           let state   = { Typeable.datatype = datatype
                         ; Typeable.context  = context }
           in (Transform.Typeable.Result { state; program })
         with exn ->
           let stacktrace = Printexc.get_raw_backtrace () in
           trace_type_error T.Typeable.name Sugartypes.pp_program program payload.program stacktrace exn
      else result
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
    let apply : Sugartypes.sentence Transform.Typeable.result -> transformer -> Sugartypes.sentence Transform.Typeable.result
      = fun (Transform.Typeable.Result { state; program }) (module T) ->
      let (Transform.Typeable.Result payload) as result =
        T.Typeable.sentence state program
      in
      if verify_transformation T.Typeable.name then
         let tyenv =
           Context.typing_environment payload.state.Transform.Typeable.context
         in
         (* TODO(dhil): Ultimately we may want to move from
            typeability preserving transformations to type-preserving
            transformations in which case the type checker should
            check *against* the datatype in transformation state
            [payload]. *)
         try
           let (program, datatype, tyenv') =
             TypeSugar.Check.sentence
                     { tyenv with Types.desugared = true }
                     payload.program in
                  (* TODO(dhil): Verify post-transformation invariants. *)
           let context = { payload.state.Transform.Typeable.context with
                           Context.typing_environment = Types.extend_typing_environment tyenv tyenv' } in
           let state   = { Typeable.datatype = datatype
                         ; Typeable.context  = context }
           in (Transform.Typeable.Result { state; program })
         with exn ->
           let stacktrace = Printexc.get_raw_backtrace () in
           trace_type_error T.Typeable.name Sugartypes.pp_sentence program payload.program stacktrace exn
      else result
    in
    let (Transform.Typeable.Result { state; program }) =
      let initial_state = Transform.Typeable.{ datatype; context = context' } in
      Array.fold_left apply Transform.Typeable.(return initial_state program) transformers
    in
    { datatype = state.Transform.Typeable.datatype;
      context  = state.Transform.Typeable.context;
      program }
end

(* Parametric transformation runner. *)
let transform show untyped_run typeable_run typechecker_run context program =
  (* Dump the undecorated AST. *)
  Debug.if_set show_pre_frontend_ast
    (fun () -> Printf.sprintf "AST before frontend transformations:\n%s\n\n" (show program));
  (* Untyped transformations. *)
  let { program; context; _ } =
    untyped_run context program
  in
  (* Typechecking. *)
  let (program, datatype, tenv) =
    typechecker_run Context.(typing_environment context) program
  in
  (* Typeability preserving transformations. *)
  let result =
    let result = typeable_run context datatype program in
    let tenv' = Context.(typing_environment result.context) in
    { result with context = Context.{ context with typing_environment = Types.extend_typing_environment tenv' tenv } }
  in
  (* Dump the decorated AST. *)
  Debug.if_set show_post_frontend_ast
    (fun () -> Printf.sprintf "AST after frontend transformations:\n%s\n\n" (show result.program)) ;
  result

let program =
  transform Sugartypes.show_program Untyped.run_program Typeability_preserving.run_program TypeSugar.Check.program

let interactive =
  transform Sugartypes.show_sentence Untyped.run_sentence Typeability_preserving.run_sentence TypeSugar.Check.sentence
