open Sugartypes

(*
 try { M } as (pat) in { N } otherwise { N' }

  --->

 handle M with {
   Return pat -> N
   _SessionFail _ _ -> N'
 }

*)


module TyEnv = Env.String

let failure_op_name = Value.session_exception_operation

let dp = Sugartypes.dummy_position
let mk_var_pat name : pattern = (`Variable (name, Some `Not_typed, dp), dp)
let dummy_pat () = mk_var_pat @@ Utility.gensym ~prefix:"dsh" ()

class insert_toplevel_handlers env =
object (o: 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | (`Spawn (`Wait, _, _, _)) as sw ->
        super#phrasenode sw
    | `Spawn (k, spawn_loc, (body, body_loc), Some inner_effects) ->
        let as_var = Utility.gensym ~prefix:"spawn_aspat" () in
        let as_pat = mk_var_pat as_var in
        let unit_phr = (`RecordLit ([], None), dp) in

        let (o, spawn_loc) = o#given_spawn_location spawn_loc in
        let envs = o#backup_envs in
        let (o, inner_effects) = o#row inner_effects in
        let process_type = `Application (Types.process, [`Row inner_effects]) in
        let o = o#with_effects inner_effects in
        let (o, body, _) = o#phrasenode body in
        let body =
          `TryInOtherwise ((body, body_loc), as_pat, (`Var as_var, dp), unit_phr, (Some (Types.unit_type))) in
        let o = o#restore_envs envs in
        (o, (`Spawn (k, spawn_loc, (body, body_loc), Some inner_effects)), process_type)
    | e -> super#phrasenode e
end


class desugar_session_exceptions env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | `Raise ->
        (o, `DoOperation (failure_op_name, [], Some `Not_typed), `Not_typed)
    | `TryInOtherwise (_, _, _, _, None) -> assert false
    | `TryInOtherwise (try_phr, pat, as_phr, otherwise_phr, (Some dt)) ->
        let open Pervasives in (* Let me have those sweet, sweet pipes *)
        let (o, try_phr, try_dt) = o#phrase try_phr in
        let envs = o#backup_envs in
        let (o, pat) = o#pattern pat in
        let (o, as_phr, _as_dt) = o#phrase as_phr in
        let o = o#restore_envs envs in
        let (o, otherwise_phr, otherwise_dt) = o#phrase otherwise_phr in
        (* Now, to create a handler... *)

        let return_pat = (`Variant ("Return", Some (pat)), dp) in
        let return_clause = (return_pat, as_phr) in
        (* Otherwise clause: Distinguished 'session failure' name. Since
         * we'll never use the continuation (and this is invoked after pattern
         * deanonymisation in desugarHandlers), generate a fresh name for the
         * continuation argument. *)
        let cont_pat = dummy_pat () in

        let otherwise_pat : Sugartypes.pattern =
          ((`Variant (failure_op_name, Some (cont_pat))), dp) in

        let otherwise_clause = (otherwise_pat, otherwise_phr) in

        let clauses = [return_clause ; otherwise_clause] in

        (* Manually construct a row with the two hardwired handler cases. *)
        let raw_row =
          Types.make_empty_closed_row ()
            |> Types.row_with ("Return", (`Present try_dt))
            |> Types.row_with (failure_op_name, (`Present (otherwise_dt))) in

        (* Dummy types *)
        let types =
          (Types.make_empty_closed_row (), try_dt,
          Types.make_empty_closed_row (), otherwise_dt) in

        let hndl_desc = {
          shd_depth = `Shallow; (* Doesn't matter either way, since we don't invoke the resumption. *)
          shd_types = types;
          shd_raw_row = raw_row
        } in

        let hndlr = {
          sh_expr = try_phr;
          sh_clauses = clauses;
          sh_descr = hndl_desc
        } in (o, `Handle hndlr, dt)
    | e -> super#phrasenode e
end

let insert_toplevel_handlers env =
  ((new insert_toplevel_handlers env) :
    insert_toplevel_handlers :> TransformSugar.transform)

let desugar_session_exceptions env =
  ((new desugar_session_exceptions env) :
    desugar_session_exceptions :> TransformSugar.transform)

let show prog =
  Printf.printf "%s\n\n" (Sugartypes.Show_program.show prog);
  prog

