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


let only_if predicate transformer =
              if predicate then transformer else (fun _ -> identity)
let only_if_set setting =
             only_if (Settings.get_value setting)

let is_interactive  = Settings.get_value Basicsettings.interacting
let perform_optimisations = not is_interactive && Settings.get_value Basicsettings.optimise

(*let print_program _ p = (Debug.print (Ir.string_of_program p));p*)

let run pipeline tyenv p =
  List.fold_left (fun p transformer -> transformer tyenv p) p pipeline

let measure name func tyenv p = Performance.measure name (uncurry func) (tyenv, p)

let perform_for_side_effects side_effecting_transformer tyenv p =
  side_effecting_transformer tyenv p;p

module Pipelines =
struct

    let optimisation_pipeline = [
        IrTraversals.ElimDeadDefs.program;
        IrTraversals.Inline.program;
      ]

    (*let typechecking_pipeline = [
        IrTraversals.NormaliseTypes.program;
        IrTraversals.ElimRecursiveTypeCycles.program;
        IrTraversals.ElimTypeAliases.program;
        IrCheck.Typecheck.program
      ]*)


    let main_pipeline = [
        only_if perform_optimisations (measure "optimise" (run optimisation_pipeline));
        Closures.program Lib.primitive_vars;
        perform_for_side_effects (BuildTables.program Lib.primitive_vars);
        (*only_if_set Basicsettings.Ir.show_compiled_ir_after_backend_transformations print_program;
        only_if_set Basicsettings.Ir.typecheck_ir (perform_pipeline typechecking_pipeline);*)
      ]

end


let transform_program tyenv p =
  run Pipelines.main_pipeline tyenv p
