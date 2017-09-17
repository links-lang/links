open Sugartypes

(*
 try { M } as (pat) in { N } otherwise { N' }

  --->

 handle M with {
   Return pat -> N
   _SessionFail _ _ -> N'
 }


SCRATCH:
  | `Handle           of handler


and clause = pattern * phrase
and funlit = pattern list list * phrase
and handlerlit = [`Deep | `Shallow] * pattern * clause list * pattern list list option (* computation arg, cases, parameters *)
and handler = {
    sh_expr: phrase;
    sh_clauses: clause list;
    sh_descr: handler_descriptor
  }
and handler_descriptor = {
    shd_depth: [`Deep | `Shallow];
    shd_types: Types.row * Types.datatype * Types.row * Types.datatype;
    shd_raw_row: Types.row;
  }
*)


let dp = Sugartypes.dummy_position

let desugar_session_exceptions =
object (self)
  inherit SugarTraversals.map as super


  method! phrasenode = function
    | `TryInOtherwise (try_phr, pat, as_phr, otherwise_phr, _dt_opt) ->
        let o = self in
        let try_phr = o#phrase try_phr in
        let (_, pos) = try_phr in
        let try_phr = (`DoOperation ("Return", [try_phr], None), pos) in
        let pat = o#pattern pat in
        let as_phr = o#phrase as_phr in
        let otherwise_phr = o#phrase otherwise_phr in
        (* Now, to create a handler... *)
        let types =
          (Types.make_empty_closed_row (), `Not_typed,
          Types.make_empty_closed_row (), `Not_typed) in

        let mk_var_pat name : pattern = (`Variable (name, None, dp), dp) in

        let return_pat = (`Variant ("Return", Some (pat)), dp) in
        let return_clause = (return_pat, as_phr) in
        (* Otherwise clause: Distinguished 'session failure' name. Since
         * we'll never use the continuation (and this is invoked after pattern
         * deanonymisation in desugarHandlers), generate a fresh name for the
         * continuation argument. *)
        let dummy_name = Utility.gensym ~prefix:"dsh" () in


        let otherwise_pat =
          (`Variant ("_SessionFail", Some (mk_var_pat dummy_name)), dp) in
        let otherwise_clause = (otherwise_pat, otherwise_phr) in

        let clauses = [return_clause ; otherwise_clause] in

        let hndl_desc = {
          shd_depth = `Shallow; (* I think? *)
          shd_types = types;
          shd_raw_row = Types.make_empty_closed_row ()
        } in

        let hndlr = {
          sh_expr = try_phr;
          sh_clauses = clauses;
          sh_descr = hndl_desc
        } in `Handle hndlr
    | e -> super#phrasenode e
end

(*
let desugar_session_exceptions env =
  ((new desugar_session_exceptions env) :
    desugar_session_exceptions :> TransformSugar.transform)
*)

let desugar_session_exceptions = desugar_session_exceptions
