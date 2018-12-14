open Sugartypes
open Utility

(* A position produced by Menhir parser *)
type ppos = SourceCode.lexpos * SourceCode.lexpos
let dummy_ppos = (Lexing.dummy_pos, Lexing.dummy_pos)

let pos (start_pos, end_pos) : Sugartypes.position = (start_pos, end_pos, None)

let with_pos p = Sugartypes.with_pos (pos p)

let checksig sigpos {node=signame; _} name =
  if signame <> name then
    raise (ConcreteSyntaxError
             ("Signature for `" ^ signame ^ "' should precede definition of `"
              ^ signame ^ "', not `"^ name ^"'.", sigpos))

(* JSTOLAREK: change signature production to contain location.  This will allow
   to avoid passing extra sig_loc to make_fun and whatever functions replaced
   annotate.  Moreover, this might allow merging of rules in binding
   production*)

(* JSTOLAREK: create specialized Sig/NoSig datatype *)
(* JSTOLAREK create specialized Lin and Unl versions *)
let make_fun_binding sig_opt fpos (linearity, bndr, args, location, block) =
  let datatype = match sig_opt with
    | Some {node=(signame, datatype); pos=sigpos} ->
       checksig sigpos signame bndr.node;
       Some datatype
    | None -> None in
  with_pos fpos (`Fun (make_untyped_binder bndr, linearity,
                       (* NOTE: position of the block is slightly inaccurate.
                          This is done to make parser code less verbose. *)
                       ([], (args, with_pos fpos (`Block block))),
                       location, datatype))

let make_handler_binding sig_opt hpos (binder, handlerlit) =
  let datatype = match sig_opt with
    | Some {node=(signame, datatype); pos=sigpos} ->
       checksig sigpos signame (name_of_binder binder);
       Some datatype
    | None -> None in
  with_pos hpos (`Handler (binder, handlerlit, datatype))

(* Used for passing an argument to make_val_binding *)
type name_or_pat = Name of name with_pos | Pat of pattern

(* Create a Val binding.  This function takes either a name for a variable
   pattern or an already constructed pattern.  In the latter case no signature
   should be passed.  *)
let make_val_binding sig_opt vpos (name_or_pat, phrase, location) =
  let pat, datatype = match name_or_pat with
    | Name name ->
       let pat = with_pos vpos (`Variable (make_untyped_binder name)) in
       let datatype = match sig_opt with
         | Some {node=(signame, datatype); pos=sigpos} ->
            checksig sigpos signame name.node;
            Some datatype
         | None -> None in
       (pat, datatype)
    | Pat pat ->
       assert (sig_opt = None);
       (pat, None) in
    with_pos vpos (`Val (pat, ([], phrase), location, datatype))

(* Create a record with a given list of labels *)
let make_record pos lbls =
  with_pos pos (`RecordLit (lbls, None))

(* Create a list of labeled database expressions *)
let make_db_exps pos exps =
  with_pos pos (`ListLit ([make_record pos exps], None))

let is_empty_db_exps : phrase -> bool = function
  | {node=`ListLit ([{node=`RecordLit ([], _);_}], _);_} -> true
  | _                                                    -> false

(* JSTOLAREK: try to collapse var_pos inot var_opt *)
(* Create a database insertion query.  Ensures that either the list of labeled
   expression is non-empty or the returning variable has been named.*)
let make_db_insert p ins_exp lbls exps var_pos var_opt =
  if is_empty_db_exps exps && var_opt == None then
    raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide" ^
      " a nonempty list of labeled expression or a return variable.", pos p));
  with_pos p (`DBInsert (ins_exp, lbls, exps, OptionUtils.opt_map
       (fun var -> with_pos var_pos (`Constant (`String var))) var_opt))
