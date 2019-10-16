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

(* let print_program _ p =
 *   Debug.print (Ir.string_of_program p)w;p *)

(* let print_bindings _ bs =
 *   List.iter (Debug.print -<- Ir.string_of_binding) bs;bs *)


(* let run pipeline tyenv p =
 *   List.fold_left (fun p transformer -> transformer tyenv p) p (pipeline ()) *)

(* let measure name func tyenv p = Performance.measure name (uncurry func) (tyenv, p) *)

(* let perform_for_side_effects side_effecting_transformer tyenv p =
 *   side_effecting_transformer tyenv p;p *)

type result = { program: Ir.program;
                datatype: Types.datatype;
                context: Context.t }

module Pipeline: sig
  val program : Context.t -> Types.datatype -> Ir.program -> result
end = struct
  type transformer = (module IrTransform.S)
  type transforms = (string * transformer) array

  module Pipeline(T : sig
               val transforms : transforms
             end) : IrTransform.S = struct
    let program state program =
      let apply : IrTransform.result -> (string * transformer) -> IrTransform.result
        = fun (IrTransform.Result { state; program }) (_, (module T)) ->
        T.program state program
      in
      Array.fold_left
        apply (IrTransform.return state program) T.transforms
  end

  let lift : transforms -> transformer
    = fun transforms ->
    (module Pipeline(struct let transforms = transforms end))


  module Conditional(T : sig
               include IrTransform.S
               val condition : unit -> bool
             end) : IrTransform.S = struct
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

  module PerformEffect(T : sig val perform : IrTransform.state -> Ir.program -> unit end) : IrTransform.S = struct
    let program state program =
      T.perform state program;
      IrTransform.return state program
  end

  let debug_tell : string -> transformer
    = fun msg ->
    only_if
      Debug.enabled
      (module PerformEffect(struct let perform _ _ = Debug.print msg end))

  let print_program : transformer
    = (module PerformEffect(struct let perform _ program = Debug.print (Ir.string_of_program program) end))


  module Measure(T : sig include IrTransform.S val name : string end) = struct
    let program state program =
      Performance.measure_l T.name (lazy (T.program state program))
  end

  let measure : string -> transformer -> transformer
    = fun name (module T) ->
    (module Measure(struct include T let name = name end))

  let optimisations : transforms
    = [| "debug", debug_tell "optimising IR"
       ; "ElimDeadDefs", (module IrTraversals.ElimDeadDefs)
       ; "inline", (module IrTraversals.Inline)
       ; "debug", debug_tell "optimised IR" |]

  let typechecking : transforms
    = [| "debug", debug_tell "typechecking IR"
       ; "Typecheck", (module IrCheck.Typecheck) (* TODO FIXME check against the carried datatype. *)
       ; "debug", debug_tell "typechecked IR" |]

  let simplify_type_structure : transforms
    = [| "debug", debug_tell "simplifying types"
       ; "ElimBodiesFromMetaTypeVars", (module IrTraversals.ElimBodiesFromMetaTypeVars)
       ; "debug", debug_tell "simplified types" |]

  let pipeline : transformer array
    = [| only_if optimise (measure "optimise" (lift optimisations))
       ; (module Closures)
       ; (module PerformEffect(struct let perform = BuildTables.program end))
       ; only_if_any [IrCheck.typecheck; simplify_types] (lift simplify_type_structure)
       ; only_if IrCheck.typecheck (lift typechecking)
       ; only_if show_compiled_ir_after_backend_transformations print_program |]

  let program context' datatype program =
    let apply : IrTransform.result -> transformer -> IrTransform.result
      = fun (IrTransform.Result { program; state }) (module T) ->
        (* TODO run verification logic? *)
        T.program state program
    in
    let initial_state =
      IrTransform.{ datatype; context = context'; primitive_vars = Lib.primitive_vars }
    in
    let IrTransform.(Result { program; state = { context; datatype; _ } }) =
      Array.fold_left apply (IrTransform.return initial_state program) pipeline
    in
    { program; context; datatype }
end

(* module Pipelines =
 * struct
 * 
 *     let optimisation_pipeline () = [
 *         debug_tell "optimising IR";
 *         IrTraversals.ElimDeadDefs.program';
 *         IrTraversals.Inline.program;
 *         debug_tell "optimised IR"
 *       ]
 * 
 *     let simplify_type_structure_program () = [
 *         debug_tell "simplifying types";
 *         (\*IrTraversals.NormaliseTypes.program;
 *         IrTraversals.ElimRecursiveTypeCycles.program;*\)
 *         (\* IrTraversals.ElimTypeAliases.program; *\)
 *         IrTraversals.ElimBodiesFromMetaTypeVars.program;
 *         debug_tell "simplified types";
 *         (only_if_set show_compiled_ir_after_backend_transformations print_program)
 *       ]
 * 
 *     let simplify_type_structure_bindings () = [
 *         debug_tell "simplifying types";
 *         (\*IrTraversals.NormaliseTypes.bindings;
 *         IrTraversals.ElimRecursiveTypeCycles.bindings;*\)
 *         (\* IrTraversals.ElimTypeAliases.bindings; *\)
 *         IrTraversals.ElimBodiesFromMetaTypeVars.bindings;
 *         debug_tell "simplified types";
 *         (only_if_set show_compiled_ir_after_backend_transformations print_bindings);
 *       ]
 * 
 *     let typechecking_pipeline () = [
 *         debug_tell "typechecking IR";
 *         IrCheck.Typecheck.program;
 *         debug_tell "typechecked IR"
 *       ]
 * 
 *     let prelude_typechecking_pipeline () = [
 *         debug_tell "typechecking prelude IR";
 *         IrCheck.Typecheck.bindings;
 *         debug_tell "typechecked prelude IR";
 *       ]
 * 
 * 
 *     let main_pipeline perform_optimisations () = [
 *         only_if
 *           perform_optimisations
 *           (measure "optimise" (run optimisation_pipeline));
 *         Closures.program Lib.primitive_vars;
 *         perform_for_side_effects
 *           (BuildTables.program Lib.primitive_vars);
 *         only_if_any_set
 *           [IrCheck.typecheck; simplify_types]
 *           (run simplify_type_structure_program);
 *         only_if_set
 *           IrCheck.typecheck
 *           (run typechecking_pipeline);
 *       ]
 * 
 *     let prelude_pipeline () = [
 *         (\* May perform some optimisations here that are safe to do on the prelude *\)
 *         (fun tenv globals -> Closures.bindings tenv Lib.primitive_vars globals);
 *         (fun tenv globals -> BuildTables.bindings tenv Lib.primitive_vars globals; globals);
 *         only_if_any_set
 *           [IrCheck.typecheck; simplify_types]
 *           (run simplify_type_structure_bindings);
 *         only_if_set
 *           IrCheck.typecheck
 *           (run prelude_typechecking_pipeline);
 *       ]
 * 
 * end *)


let program : Context.t -> Types.datatype -> Ir.program -> result
  = fun context datatype program ->
  Pipeline.program context datatype program

let transform_program _perform_optimisations _tyenv _p = assert false
  (* run (Pipelines.main_pipeline perform_optimisations) tyenv p *)

let transform_prelude _tyenv _bindings = assert false
  (* run Pipelines.prelude_pipeline tyenv bindings *)
