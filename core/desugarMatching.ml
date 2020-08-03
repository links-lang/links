open Sugartypes
open Utility
open SourceCode

let with_pos = SourceCode.WithPos.make

let desugar_matching =
object ((self : 'self_type))
    inherit SugarTraversals.map as super
    method! binding = fun b ->
      let pos = WithPos.pos b in
      match WithPos.node b with
      |  Fun ({ fun_definition = (tvs, MatchFunlit (patterns, cases)); _ } as fn) ->
          let nameList = List.map (fun pats -> List.map (fun pat -> (pat, Utility.gensym())) pats) patterns in
          let switchTuple = List.map (fun (_,name) -> with_pos (Var name)) (List.flatten nameList) in
          let normalArgs = 
            List.map (fun pats -> List.map (fun (pat, name) -> 
                                              with_pos (Pattern.As (with_pos (Binder.make ~name ()), pat))) 
                                            pats) nameList in
          let switchBody = Switch (with_pos (TupleLit switchTuple), cases, None) in
          let normalFnlit = NormalFunlit (normalArgs, with_pos switchBody) in
          let normalFnlit = self#funlit normalFnlit in
          let node = Fun { fun_binder = fn.fun_binder;
                           fun_linearity = fn.fun_linearity;
                           fun_definition = (tvs, normalFnlit);
                           fun_location = fn.fun_location;
                           fun_signature = fn.fun_signature;
                           fun_unsafe_signature = fn.fun_unsafe_signature;
                           fun_frozen = fn.fun_frozen;
                           } in
          WithPos.make ~pos node
      | _ -> super#binding b
end

module Untyped
  = Transform.Untyped.Make.Transformer(struct
        let name = "desugar_pattern_matching"
        let obj = desugar_matching
      end)
