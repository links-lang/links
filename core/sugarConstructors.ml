open Sugartypes
open Utility

(* ppos = parser position, ie. a position as produced by Menhir parser *)
type ppos = SourceCode.lexpos * SourceCode.lexpos
let dummy_ppos = (Lexing.dummy_pos, Lexing.dummy_pos)

(* Convert position produced by a parser to Sugartypes position*)
let pos (start_pos, end_pos) : Sugartypes.position = (start_pos, end_pos, None)
(* Wrapper around with_pos that accepts parser positions *)
let with_pos p = Sugartypes.with_pos (pos p)

type signature_opt = Sig of (name with_pos * datatype') with_pos | NoSig

(* Produces a datatype if a name is accompanied by a signature.  Raises an
   exception if name does not match a name in a signature. *)
let datatype_opt_from_sig_opt sig_opt name =
  match sig_opt with
  | Sig {node=({node=signame; _}, datatype); pos} ->
     (* Ensure that name in a signature matches name in a declaration *)
     if signame <> name then
       raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `"^ name ^"'.", pos));
     Some datatype
  | NoSig -> None

(* JSTOLAREK create specialized Lin and Unl versions *)
let make_fun_binding sig_opt fpos (linearity, bndr, args, location, block) =
  let datatype = datatype_opt_from_sig_opt sig_opt bndr.node in
  with_pos fpos (`Fun (make_untyped_binder bndr, linearity,
                       (* NOTE: position of the block is slightly inaccurate.
                          This is done to make parser code less verbose. *)
                       ([], (args, with_pos fpos (`Block block))),
                       location, datatype))

let make_handler_binding sig_opt hpos (binder, handlerlit) =
  let datatype = datatype_opt_from_sig_opt sig_opt (name_of_binder binder) in
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
       let datatype = datatype_opt_from_sig_opt sig_opt name.node in
       (pat, datatype)
    | Pat pat ->
       assert (sig_opt = NoSig);
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
(* Create a database insertion query.  Raises an exception when the list of
   labeled expression is empty and the returning variable has not been named.*)
let make_db_insert p ins_exp lbls exps var_pos var_opt =
  if is_empty_db_exps exps && var_opt == None then
    raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide" ^
      " a nonempty list of labeled expression or a return variable.", pos p));
  with_pos p (`DBInsert (ins_exp, lbls, exps, OptionUtils.opt_map
       (fun var -> with_pos var_pos (`Constant (`String var))) var_opt))
