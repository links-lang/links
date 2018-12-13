open Sugartypes
open Utility

(* A position produced by Menhir parser *)
type ppos = SourceCode.lexpos * SourceCode.lexpos
let dummy_ppos = (Lexing.dummy_pos, Lexing.dummy_pos)

let pos (start_pos, end_pos) : Sugartypes.position = (start_pos, end_pos, None)

let with_pos p = Sugartypes.with_pos (pos p)

(* JSTOLAREK: refactor and rename *)
let annotate sigpos (signame, datatype) dpos : _ -> binding =
  let checksig {node=signame; _} name =
    if signame <> name then
      raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `"^ name ^"'.", pos sigpos)) in
    function
      | `Fun (name, lin, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos (`Fun ( make_untyped_binder name, lin, ([], phrase)
                              , location, Some datatype))
      | `Var (name, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos
            (`Val ( with_pos dpos (`Variable (make_untyped_binder name))
                  , ([], phrase), location, Some datatype))
      | `Handler (bndr, hnlit, _) ->
         let _ = checksig signame (name_of_binder bndr) in
         with_pos dpos (`Handler (bndr, hnlit, Some datatype))

(* Create a record with a given list of labels *)
let make_record pos lbls =
  with_pos pos (`RecordLit (lbls, None))

(* Create a list of labeled database expressions *)
let make_db_exps pos exps =
  with_pos pos (`ListLit ([make_record pos exps], None))

let is_empty_db_exps : phrase -> bool = function
  | {node=`ListLit ([{node=`RecordLit ([], _);_}], _);_} -> true
  | _                                                    -> false

(* Create a database insertion query.  Ensures that either the list of labeled
   expression is non-empty or the returning variable has been named.*)
let make_db_insert p ins_exp lbls exps var_pos var_opt =
  if is_empty_db_exps exps && var_opt == None then
    raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide" ^
      " a nonempty list of labeled expression or a return variable.", pos p));
  with_pos p (`DBInsert (ins_exp, lbls, exps, OptionUtils.opt_map
       (fun var -> with_pos var_pos (`Constant (`String var))) var_opt))
