open Utility

 (* TODO: optimisation *)

  (* We need to be careful here as, for instance, running ElimDeadDefs
     on the prelude would lead to lots of functions being deleted that
     might actually be used in the program itself.

     It does cause problems in the interactive loop
     as if you define a function it immediately gets optimised away as
     it isn't yet used!

     In order to resolve the problem we could either simply disable
     optimisation for the interactive loop, or we could more usefully
     disable optimisation of the top level definitions by returning a
     tuple containing all of them. That way we could also optimise the
     prelude.
  *)

let show_compiled_ir_after_backend_transformations
  = Settings.(flag "show_compiled_ir_after_backend_transformations"
              |> convert parse_bool
              |> sync)

let simplify_types
  = Settings.(flag "simplify_types"
              |> convert parse_bool
              |> sync)

(* Optimization pass? *)
let optimise
  = Settings.(flag "optimise"
              |> synopsis "Optimises the generated code"
              |> convert parse_bool
              |> CLI.(add (long "optimise"))
              |> sync)

(* Transformation infrastructure. *)
type result = { program: Ir.program;
                datatype: Types.t;
                context: Context.t }

type transformer = (module IrTransform.S)
type transforms = transformer array

(* This functor collapses an array of [transformers] into a single
   [transformer]. *)
module Collapse(T : sig
             val name : string
             val transforms : transforms
           end) : IrTransform.S = struct
  let name = T.name
  let program state program =
    let apply : IrTransform.result -> transformer -> IrTransform.result
      = fun (IrTransform.Result { state; program }) (module T) ->
      T.program state program
    in
    Array.fold_left
      apply (IrTransform.return state program) T.transforms
end

let collapse : string -> transforms -> transformer
  = fun name transforms ->
  (module Collapse(struct let name = name
                          let transforms = transforms end))

(* This functor constructs a transformer that is only run if
   [condition] evaluates to `true`. *)
module Conditional(T : sig
             include IrTransform.S
             val condition : unit -> bool
           end) : IrTransform.S = struct
  let name = Printf.sprintf "Conditional(%s)" T.name
  let program state program =
    if T.condition ()
    then T.program state program
    else IrTransform.return state program
end

let only_if : bool Settings.setting -> (module IrTransform.S) -> transformer
  = fun setting (module T) ->
  (module Conditional(struct include T let condition () = Settings.get setting end))

let only_if_any : bool Settings.setting list -> (module IrTransform.S) -> transformer
  = fun settings (module T) ->
  (module Conditional(struct include T let condition () = List.exists Settings.get settings end))

(* This functor constructs performs some effectful computation, but
   leaves its given [program] unaltered. *)
(* TODO(dhil): Maybe [perform] should return an updated state such
   that we can alter the transformation state. *)
module PerformEffect(T : sig
             val name : string
             val perform : IrTransform.state -> Ir.program -> unit end) : IrTransform.S
  = struct
  let name = T.name

  let program state program =
    T.perform state program;
    IrTransform.return state program
end

let debug_tell : string -> transformer
  = fun msg ->
  only_if
    Debug.enabled
    (module PerformEffect(struct
                let name = "debug"
                let perform _ _ = Debug.print msg end))

let print_program : transformer
  = (module PerformEffect(struct
                let name = "print_program"
                let perform _ program = Debug.print (Ir.string_of_program program) end))

(* This functor instruments a transformer with performance measuring
   capabilities. *)
module Measure(T : sig include IrTransform.S end) = struct
  let name = Printf.sprintf "Measure(%s)" T.name
  let program state program =
    Performance.measure_l T.name (lazy (T.program state program))
end

let measure : transformer -> transformer
  = fun (module T) -> (module Measure(T))

(* Pipelines. *)
let optimisations : transforms
  = [| debug_tell "optimising IR"
     ; (module IrTraversals.ElimDeadDefs)
     ; (module IrTraversals.Inline)
     ; debug_tell "optimised IR" |]

let typechecking : transforms
  = [| debug_tell "typechecking IR"
     ; (module IrCheck.Typecheck) (* TODO FIXME check against the carried datatype. *)
     ; debug_tell "typechecked IR" |]

let simplify_type_structure : transforms
  = [| debug_tell "simplifying types"
     ; (module IrTraversals.ElimBodiesFromMetaTypeVars)
     ; debug_tell "simplified types" |]

(* A collection of the above pipelines. *)
let pipeline : transformer array
  = [| only_if optimise (measure (collapse "optimisations" optimisations))
     ; only_if IrCheck.typecheck (collapse "typechecking" typechecking)
     ; (module Closures)
     ; (module PerformEffect(struct
                   let name = "build_tables"
                   let perform = BuildTables.program end))
     ; only_if_any [IrCheck.typecheck; simplify_types] (collapse "type_structure" simplify_type_structure)
     ; only_if IrCheck.typecheck (collapse "typechecking" typechecking)
     ; only_if show_compiled_ir_after_backend_transformations print_program |]

let program context' datatype program =
  let apply : IrTransform.result -> transformer -> IrTransform.result
    = fun (IrTransform.Result { program; state }) (module T) ->
    (* TODO run verification logic? *)
      Debug.if_set Basicsettings.show_stages (fun () -> T.name ^"...");
      T.program state program
  in
  let initial_state =
    IrTransform.{ datatype; context = context'; primitive_vars = Lib.primitive_vars }
  in
  let IrTransform.(Result { program; state = { context; datatype; _ } }) =
    Array.fold_left apply (IrTransform.return initial_state program) pipeline
  in
  { program; context; datatype }
