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
      |  Fun ({ fun_definition = (tvs, fnlit); _ } as fn) ->
          let fnlit = self#funlit fnlit in
          let node =
            if is_recursive fn.fun_binder fnlit then
              Funs [WithPos.make ~pos
                      { rec_binder           = fn.fun_binder
                      ; rec_linearity        = fn.fun_linearity
                      ; rec_definition       = ((tvs, None), fnlit)
                      ; rec_location         = fn.fun_location
                      ; rec_signature        = fn.fun_signature
                      ; rec_unsafe_signature = fn.fun_unsafe_signature
                      ; rec_frozen           = fn.fun_frozen
                      } ]
            else
              Fun { fn with fun_definition = (tvs, fnlit) } in
          WithPos.make ~pos node
      | _ -> super#binding b
end

module Untyped
  = Transform.Untyped.Make.Transformer(struct
        let name = "lift_recursive"
        let obj = lift_funs
      end)
