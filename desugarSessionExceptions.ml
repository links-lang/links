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


module TyEnv = Env.String

let dp = Sugartypes.dummy_position

class desugar_session_exceptions env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super


  method! phrasenode = function
    | `TryInOtherwise (_, _, _, _, None) -> assert false
    | `TryInOtherwise (try_phr, pat, as_phr, otherwise_phr, (Some dt)) ->
        (* TODO: Typing is not worked out yet. Types are probably garbage. *)
        let (o, try_phr, _try_dt) = o#phrase try_phr in
        let envs = o#backup_envs in
        let (o, pat) = o#pattern pat in
        let (o, as_phr, _as_dt) = o#phrase as_phr in
        let o = o#restore_envs envs in
        let (o, otherwise_phr, otherwise_dt) = o#phrase otherwise_phr in
        (* Now, to create a handler... *)
        (* Again, this is probably junk... *)
        let types =
          (Types.make_empty_closed_row (), `Not_typed,
          Types.make_empty_closed_row (), otherwise_dt) in

        let mk_var_pat name : pattern = (`Variable (name, Some `Not_typed, dp), dp) in

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
          shd_depth = `Deep; (* I think? *)
          shd_types = types;
          shd_raw_row = Types.make_empty_closed_row ()
        } in

        let hndlr = {
          sh_expr = try_phr;
          sh_clauses = clauses;
          sh_descr = hndl_desc
        } in (o, `Handle hndlr, dt)
    | e -> super#phrasenode e
end

let desugar_session_exceptions env =
  ((new desugar_session_exceptions env) :
    desugar_session_exceptions :> TransformSugar.transform)
