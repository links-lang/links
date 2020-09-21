open Sugartypes
open Utility
open SourceCode

(* This module desugars pattern-matching functions

  This transformation convert `switch` functions of the form:

  fun foo(a1, ..., an) switch {
    case (p1_1, ..., p1_n) -> b_1
    ...
    case (pm_1, pm_n) -> b_m
  }

  to standard functions of the form:

  fun foo(a1 as x1, ..., an as xn) {
    switch ((x1, ..., xn)) {
      case (p1_1, ..., p1_n) -> b_1
      ...
      case (pm_1, ..., pm_n) -> b_m
      case (_, ..., _) -> error("non-exhaustive")
  }

  The last non-exhaustive case with wild card pattern is always attached to the end of switch body.

*)

let with_pos = SourceCode.WithPos.make

let switch_functions =
  Settings.(
    flag ~default:false "switch_functions"
    |> synopsis "Toggles whether to enable the switch function syntax"
    |> convert parse_bool
    |> sync)

let pattern_matching_sugar_guard pos =
  let pattern_matching_sugar_disabled pos =
    Errors.disabled_extension ~pos ~setting:("switch_functions", true) "Switch functions"
  in
  if not (Settings.get switch_functions)
  then raise (pattern_matching_sugar_disabled pos)

let nullary_guard pss pos =
  let nullary_error pos =
    Errors.desugaring_error ~pos:pos ~stage:Errors.DesugarSwitchFuns ~message:"Can't match over nullary function"
  in
  match pss with
    | [] -> raise (nullary_error pos)
    | _ -> ()

let switch_fun_currying_guard pos args =
  match args with
  | [arg] -> arg
  | _ -> raise (Errors.Type_error (pos, "Curried switch functions are not yet supported."))

let construct_normal_funlit funlit_pos patterns cases =
  pattern_matching_sugar_guard funlit_pos;
  let patterns = switch_fun_currying_guard funlit_pos patterns in
  nullary_guard patterns funlit_pos;
  (* bind the arguments with unique var name *)
  let pat_first_pos = WithPos.pos (List.nth patterns 0) in
  let name_list = List.map (fun pat -> (pat, Utility.gensym())) patterns in
  let switch_tuple = List.map (fun (_, name) -> with_pos (Var name)) name_list in
  (* assemble exhaustive handler *)
  let exhaustive_patterns = with_pos (Pattern.Any) in
  let exhaustive_position = Format.sprintf "non-exhaustive pattern matching at %s" (SourceCode.Position.show funlit_pos) in
  let exhaustive_case = FnAppl (with_pos (Var "error"), [with_pos (Constant (CommonTypes.Constant.String exhaustive_position))]) in
  let normal_args =
    List.map
      (fun (pat, name) -> with_pos (Pattern.As (with_pos ~pos:funlit_pos (Binder.make ~name ()), pat)))
      name_list
  in
  let cases = cases@[(exhaustive_patterns, with_pos exhaustive_case)] in
  let switch_body = Switch (with_pos ~pos:pat_first_pos (TupleLit switch_tuple), cases, None) in
  let normal_fnlit = NormalFunlit ([normal_args], with_pos ~pos:funlit_pos switch_body) in
  normal_fnlit

let desugar_switching =
object ((self : 'self_type))
    inherit SugarTraversals.map as super
    method! binding = fun b ->
      let pos = WithPos.pos b in
      match WithPos.node b with
      | Fun ({ fun_definition = (tvs, SwitchFunlit (patterns, cases)); _ } as fn) ->
        let normal_fnlit = construct_normal_funlit pos patterns cases in
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

    method! phrase = fun p ->
      let pos = WithPos.pos p in
      match WithPos.node p with
      | FunLit (typing, linearity, SwitchFunlit (patterns, cases), loc) ->
        let normal_fnlit = construct_normal_funlit pos patterns cases in
        let normal_fnlit = self#funlit normal_fnlit in
        let node = FunLit (typing, linearity, normal_fnlit, loc) in
        WithPos.make ~pos node
      | _ -> super#phrase p
end

module Untyped
  = Transform.Untyped.Make.Transformer(struct
        let name = "desugar_switch_functions"
        let obj = desugar_switching
      end)
