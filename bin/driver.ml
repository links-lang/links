open Links_core
open Webserver
open Performance
open Utility

(** Name of the file containing the prelude code. *)
let prelude_file =
  Settings.(option ~default: (Some Linkspath.prelude) "prelude"
            |> synopsis "The Links prelude source file"
            |> to_string from_string_option
            |> convert (Sys.expand ->- some)
            |> sync)

let typecheck_only
  = Settings.(flag "typecheck_only"
              |> synopsis "Only type check Links modules (development)"
              |> convert parse_bool
              |> sync)

module Phases = struct
  module Parse = struct
    let run : Context.t -> string -> Sugartypes.program Loader.result
      = Loader.load

    let string : Context.t -> string -> Sugartypes.program Loader.result
      = fun context source_code ->
      let program, pos_context =
        Parse.parse_string
          ~pp:(from_option "" (Settings.get Parse.pp))
          Parse.program
          source_code
      in
      let context' = Context.({ context with source_code = pos_context }) in
      Loader.({ program_ = program; context = context' })

    let interactive : string -> Context.t -> Sugartypes.sentence Loader.result
      = fun ps1 context ->
      let program, pos_context =
        Parse.Readline.parse ps1
      in
      let context' = Context.({ context with source_code = pos_context }) in
      Loader.({ program_ = program; context = context' })
  end

  module Desugar = struct
    let run : Sugartypes.program Loader.result -> Sugartypes.program Frontend.result
      = fun Loader.({ context; program_ }) ->
      Frontend.program context program_

    let interactive : Sugartypes.sentence Loader.result -> Sugartypes.sentence Frontend.result
      = fun Loader.({ context; program_ }) ->
      Frontend.interactive context program_
  end

  module Compile = struct
    module IR = struct
      let run : Sugartypes.program Frontend.result -> Sugartoir.result
        = fun Frontend.({ context; datatype; program }) ->
        Sugartoir.program context datatype program
    end
  end

  module Transform = struct
    let run : Sugartoir.result -> Backend.result
      = fun Sugartoir.({ context; datatype; globals; program }) ->
      let program' =
        Ir.with_bindings globals program (* TODO(dhil): This looks silly, and I think it may be wrong... *)
      in
      Backend.program context datatype program'
  end

  module Evaluate = struct
    module Eval = Evalir.Eval(Webserver)

    let run : Backend.result -> (Context.t * Types.datatype * Value.t)
      = fun Backend.({ context; program; datatype }) ->
      let valenv = Context.value_environment context in
      let (valenv', v) =
        lazy (Eval.run_program valenv program) |>measure_as<| "run_program"
      in
      (Context.({ context with value_environment = valenv' }), datatype, v)
  end

  module Interactive = struct
    type result =
      { context: Context.t;
        sentence: [ `Definitions of Ir.binding list
                  | `Expression of Types.datatype * Value.t
                  | `Directive of string * string list ] }

    let readline : string -> Context.t -> result
      = fun ps1 context ->
      let Frontend.({ context; program = sentence; datatype })
        = Parse.interactive ps1 context
          |> Desugar.interactive
      in
      let context, sentence =
        match sentence with
        | Sugartypes.Definitions defs ->
           let program' = (defs, None) in
           let Backend.({ program; _ }) as result =
             Compile.IR.run Frontend.({ context; datatype; program = program' })
             |> Transform.run
           in
           let (context', _, _) = Evaluate.run result in
           context', `Definitions (fst program)
        | Sugartypes.Expression expr ->
           let program' = ([], Some expr) in
           let result =
             Compile.IR.run Frontend.({ context; datatype; program = program' })
             |> Transform.run
           in
           let (context', datatype, value) = Evaluate.run result in
           context', `Expression (datatype, value)
        | Sugartypes.Directive d ->
           context, `Directive d
      in
      { context; sentence }
  end

  let dump_lib : out_channel -> unit
    = fun oc ->
    Printf.fprintf oc "lib.ml mappings:\n%!";
    Env.String.iter
      (fun name var ->
        let (datatype : string) =
          Types.string_of_datatype
            (Env.String.find name Lib.typing_env.Types.var_env)
        in
        Printf.fprintf oc " %d -> %s : %s\n%!" var name datatype)
      Lib.nenv

  (* Loads the prelude, and returns the 'initial' compilation context. *)
  let initialise : unit -> Context.t
    = fun () ->
    let context = Context.({ empty with name_environment = Lib.nenv;
                                        typing_environment = Lib.typing_env })
    in
    let filename = val_of (Settings.get prelude_file) in
    let result =
      Parse.run context filename
      |> Desugar.run
      |> (fun result ->
        let context = result.Frontend.context in
        let venv =
          Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)
        in
        let context' = Context.({ context with variable_environment = venv }) in
        Compile.IR.run Frontend.({ result with context = context' }))
      |> Transform.run
    in
    let context', _, _ = Evaluate.run result in
    let nenv = Context.name_environment context' in
    let tenv = Context.typing_environment context' in
    let venv = Var.varify_env (nenv, tenv.Types.var_env) in
    (* Prepare the webserver. *)
    Webserver.set_prelude (fst result.Backend.program);
    (* Return the 'initial' compiler context. *)
    Context.({ context' with variable_environment = venv })

  let whole_program : Context.t -> string -> (Context.t * Types.datatype * Value.t)
    = fun initial_context filename ->
    (* Process source file (and its dependencies. *)
    let result =
      Parse.run initial_context filename
      |> Desugar.run
      |> (fun result -> if Settings.get typecheck_only then exit 0 else result)
      |> Compile.IR.run
      |> Transform.run
    in
    let context, (globals, _) = result.Backend.context, result.Backend.program in
    let valenv    = Context.value_environment context in
    let nenv      = Context.name_environment context in
    let tenv      = Context.typing_environment context in
    let ffi_files = Context.ffi_files context in
    Webserver.init (valenv, nenv, tenv) globals ffi_files;
    Evaluate.run result

  let evaluate_string : Context.t -> string -> (Context.t * Types.datatype * Value.t)
    = fun initial_context source_code ->
    Parse.string initial_context source_code
    |> Desugar.run
    |> (fun result -> if Settings.get typecheck_only then exit 0 else result)
    |> Compile.IR.run
    |> Transform.run
    |> Evaluate.run

  let compile_js_only : Context.t -> string -> string -> unit
    = fun initial_context source_file object_file ->
      (* Process source file (and its dependencies. *)
      let result =
        Parse.run initial_context source_file
        |> Desugar.run
        |> (fun result -> if Settings.get typecheck_only then exit 0 else result)
        |> Compile.IR.run
        |> Transform.run
      in
      let context, program = result.Backend.context, result.Backend.program in
      (* let valenv    = Context.value_environment context in *)
      let nenv  = Context.name_environment context in
      (* let tenv      = Context.typing_environment context in *)
      let venv =
        Env.String.fold
        (fun name v venv -> Env.Int.bind v name venv)
        nenv
        Env.Int.empty
      in
      (* let tenv = Var.varify_env (nenv, tenv.Types.var_env) in *)
      let ffi_files =
        Context.ffi_files context
      in
      let open Irtojs in
      let _venv', code =
        let program' =
          let (bs, tc) = program in
          (* TODO(dhil): This is a slight hack. We shouldn't need to
             use the webserver to retrieve the prelude, alas, the
             current infrastructure does not let us get hold of the
             prelude bindings by other means. *)
          (Webserver.get_prelude () @ bs, tc)
        in
        Compiler.generate_program venv program'
      in
      (* Prepare object file. *)
      let oc =
        try open_out object_file
        with Sys_error reason -> raise (Errors.cannot_open_file object_file reason)
      in
      try
        if Settings.get Basicsettings.System.link_js_runtime
        then begin
            Js_CodeGen.output oc Compiler.primitive_bindings;
            let runtime_files =
              match Settings.get Basicsettings.System.custom_js_runtime with
              | [] -> [Filename.concat Linkspath.jslib "jslib.js"]
              | files -> files
            in
            List.iter
              (fun runtime_file ->
                let ic =
                  try open_in runtime_file
                  with Sys_error reason -> raise (Errors.cannot_open_file runtime_file reason)
                in
                try
                  Utility.IO.Channel.cat ic oc;
                  close_in ic
                with e -> close_in ic; raise e)
              runtime_files;
          end;
        (* Copy contents of FFI files. *)
        List.iter
          (fun ffi_file ->
            let ic = open_in ffi_file in
            try
              Utility.IO.Channel.cat ic oc;
              close_in ic
            with e -> close_in ic; raise e)
          ffi_files;
        (* Emit the JavaScript code produced by irtojs. *)
        Js_CodeGen.output oc code;
        close_out oc
      with Sys_error reason ->
        close_out oc; raise (Errors.object_file_write_error object_file reason)
end
