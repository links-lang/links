open Sugartypes
open Utility
open SourceCode

(* A simple pass to lift (self)-recursive functions from a `Fun` node to a `Funs` node. *)

(* Simple check to see whether a function is recursive: checks whether the
 * binder occurs within the free variables of the function body. *)
(* Note that we do not need to handle mutually-recursive functions
 * here since they will be constructed explicitly during parsing. *)
let is_recursive bnd fnlit =
  StringSet.mem (Binder.to_name bnd) (Freevars.funlit fnlit)


(* Fun bindings must be lifted into `Funs if they are recursive. This is
 * performed after the first pass. *)
let lift_funs =
object ((self : 'self_type))
    inherit SugarTraversals.map as super
    method! binding = fun b ->
      let pos = WithPos.pos b in
      match WithPos.node b with
      |  Fun (bndr, lin, (tvs, fnlit), location, dt) ->
          let fnlit = self#funlit fnlit in
          let node =
            if is_recursive bndr fnlit then
              Funs [(bndr, lin, ((tvs, None), fnlit), location, dt, pos)]
            else
              Fun (bndr, lin, (tvs, fnlit), location, dt) in
          WithPos.make ~pos node
      | _ -> super#binding b
end
