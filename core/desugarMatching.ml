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
          let name_list = List.map (fun pats -> List.map (fun pat -> (pat, Utility.gensym())) pats) patterns in
          let switch_tuple = List.map (fun (_, name) -> with_pos (Var name)) (List.flatten name_list) in
          let exhaustive_patterns = List.map (fun _ -> with_pos (Pattern.Any)) switch_tuple in
          let exhaustive_patterns =
            match exhaustive_patterns with
              | [] -> with_pos (Pattern.Any)
              | [single] -> single
              | _ -> with_pos (Pattern.Tuple exhaustive_patterns) in
          let exhaustive_position = Format.sprintf "non-exhaustive pattern matching at %s" (SourceCode.Position.show pos) in
          let exhaustive_case = FnAppl (with_pos (Var "error"), [with_pos (Constant (CommonTypes.Constant.String exhaustive_position))]) in
          let normal_args =
            List.map (fun pats -> List.map (fun (pat, name) ->
                                              with_pos (Pattern.As (with_pos (Binder.make ~name ()), pat)))
                                            pats) name_list in
          let cases = cases@[(exhaustive_patterns, with_pos exhaustive_case)] in
          let switch_body = Switch (with_pos (TupleLit switch_tuple), cases, None) in
          let normal_fnlit = NormalFunlit (normal_args, with_pos switch_body) in
          let normal_fnlit = self#funlit normal_fnlit in
          let node = Fun { fun_binder = fn.fun_binder;
                           fun_linearity = fn.fun_linearity;
                           fun_definition = (tvs, normal_fnlit);
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
