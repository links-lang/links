open Sugartypes
open Utility

(* Desugars SugarFuns. *)
(* By this point, every binding within a SugarFuns block will have
 * been transformed into a Fun. This pass flattens each SugarFuns
 * block into either:
   * a) a Fun, if the block is a single Fun which is non-recursive, or
   * b) a Funs, if the block is a collection of mutually-recursive functions,
   *    or if the block is a single Fun which is recursive *)

(* Simple check to see whether a function is recursive: checks whether the
 * binder occurs within the free variables of the function body. *)
let is_recursive bnd fnlit =
  StringSet.mem (name_of_binder bnd) (Freevars.funlit fnlit)


(* `Fun bindings must be lifted into `Funs if they are recursive. *)
let lift_funs =
object ((self : 'self_type))
    inherit SugarTraversals.map as super
    method! binding = function
      |  {node=`Fun (bndr, lin, (tvs, fnlit), location, dt); pos} ->
          if is_recursive bndr fnlit then
            let fnlit = self#funlit fnlit in
            {node=`Funs [(bndr, lin, ((tvs, None), fnlit), location, dt, pos)]; pos}
          else
            let fnlit = self#funlit fnlit in
            {node=`Fun (bndr, lin, (tvs, fnlit), location, dt); pos}
      | b -> super#binding b
end

(* Desugars SugarFuns blocks *)
let desugar_sugarfuns =
object ((self : 'self_type))
  inherit SugarTraversals.map as super
  method! binding = function
    (* Empty or singleton SugarFuns should not have been constructed. *)
    | {node = `SugarFuns []; _} -> assert false
    | {node = `SugarFuns [_]; _} -> assert false
    | {node = `SugarFuns bs; pos } ->
        (* Recursively apply *)
        let bs = self#list (fun o -> o#binding) bs in
        (* All contained bindings must be of the form `Fun x. Extract these,
         * and turn them into the right form for Funs blocks. *)
        let fs =
          List.map (function
            | {node=`Fun (bnd, lin, (tvs, fnlit), loc, dt); pos} ->
                (bnd, lin, ((tvs, None), fnlit), loc, dt, pos)
            (* Due to parser construction and the fact we've desugared handlers
             * into `Funs already, something must have gone seriously wrong if
             * the block contains anything but `Funs. *)
            | _ -> assert false) bs in
        {node = (`Funs fs); pos}
    | b -> super#binding b
end
