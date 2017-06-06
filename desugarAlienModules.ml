(* Desugars "alien modules" into plain modules containing alien declarations.
 * alien javascript module "test.js" Test {
 *   setTitle : (String) ~> ()
 *   alertBox : (String) ~> ()
 * }
 *
 * --->
 *
 * module Test {
 *   alien javascript "test.js" setTitle : (String) ~> ()
 *   alien javascript "test.js" alertBox : (String) ~> ()
 * }
 *)

open Utility
open Sugartypes

let do_transformation =
object(_self)
  inherit SugarTraversals.map as super

  method! bindingnode : bindingnode -> bindingnode = function
    | `AlienModule (lang, lib, mod_name, decls) ->
        `Module (mod_name,
          (List.map (fun (b, dt) ->
            let (name, _, pos) = b in
            (`Foreign (b, name, lang, lib, dt), pos))) decls)
    | x -> super#bindingnode x
end

let transform_alien_modules = do_transformation#program

