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


let only_if predicate transformer =
              if predicate then transformer else (fun _ x -> x)
let only_if_set setting =
             only_if (Settings.get setting)

let only_if_any_set settings transformer =
  if Utility.any_true (List.map Settings.get settings)
  then transformer
  else (fun _ x -> x)

let debug_tell msg =
  only_if_set
    Debug.enabled
    (fun _tyenv prog ->
      Debug.print msg; prog)

let print_program _ p =
  Debug.print (Ir.string_of_program p);p

let print_bindings _ bs =
  List.iter (Debug.print -<- Ir.string_of_binding) bs;bs


let run pipeline tyenv p =
  List.fold_left (fun p transformer -> transformer tyenv p) p (pipeline ())

let measure name func tyenv p = Performance.measure name (uncurry func) (tyenv, p)

let perform_for_side_effects side_effecting_transformer tyenv p =
  side_effecting_transformer tyenv p;p

module Pipelines =
struct

    let optimisation_pipeline () = [
        debug_tell "optimising IR";
        IrTraversals.ElimDeadDefs.program;
        IrTraversals.Inline.program;
        debug_tell "optimised IR"
      ]

    let simplify_type_structure_program () = [
        debug_tell "simplifying types";
        (*IrTraversals.NormaliseTypes.program;
        IrTraversals.ElimRecursiveTypeCycles.program;*)
        (* IrTraversals.ElimTypeAliases.program; *)
        IrTraversals.ElimBodiesFromMetaTypeVars.program;
        debug_tell "simplified types";
        (only_if_set show_compiled_ir_after_backend_transformations print_program)
      ]

    let simplify_type_structure_bindings () = [
        debug_tell "simplifying types";
        (*IrTraversals.NormaliseTypes.bindings;
        IrTraversals.ElimRecursiveTypeCycles.bindings;*)
        (* IrTraversals.ElimTypeAliases.bindings; *)
        IrTraversals.ElimBodiesFromMetaTypeVars.bindings;
        debug_tell "simplified types";
        (only_if_set show_compiled_ir_after_backend_transformations print_bindings);
      ]

    let typechecking_pipeline () = [
        debug_tell "typechecking IR";
        IrCheck.Typecheck.program;
        debug_tell "typechecked IR"
      ]

    let prelude_typechecking_pipeline () = [
        debug_tell "typechecking prelude IR";
        IrCheck.Typecheck.bindings;
        debug_tell "typechecked prelude IR";
      ]


    let main_pipeline perform_optimisations () = [
        only_if
          perform_optimisations
          (measure "optimise" (run optimisation_pipeline));
        Closures.program Lib.primitive_vars;
        perform_for_side_effects
          (BuildTables.program Lib.primitive_vars);
        only_if_any_set
          [IrCheck.typecheck; simplify_types]
          (run simplify_type_structure_program);
        only_if_set
          IrCheck.typecheck
          (run typechecking_pipeline);
      ]

    let prelude_pipeline () = [
        (* May perform some optimisations here that are safe to do on the prelude *)
        (fun tenv globals -> Closures.bindings tenv Lib.primitive_vars globals);
        (fun tenv globals -> BuildTables.bindings tenv Lib.primitive_vars globals; globals);
        only_if_any_set
          [IrCheck.typecheck; simplify_types]
          (run simplify_type_structure_bindings);
        only_if_set
          IrCheck.typecheck
          (run prelude_typechecking_pipeline);
      ]

end


let transform_program perform_optimisations tyenv p =
  run (Pipelines.main_pipeline perform_optimisations) tyenv p

let transform_prelude tyenv bindings =
  run Pipelines.prelude_pipeline tyenv bindings
