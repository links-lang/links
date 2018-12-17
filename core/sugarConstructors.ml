open Sugartypes
open Utility

(* ppos = parser position, ie. a position as produced by Menhir *)
type ppos = SourceCode.lexpos * SourceCode.lexpos
let dummy_ppos = (Lexing.dummy_pos, Lexing.dummy_pos)

(* Convert position produced by a parser to Sugartypes position*)
let pos (start_pos, end_pos) : Sugartypes.position = (start_pos, end_pos, None)
(* Wrapper around with_pos that accepts parser positions *)
let with_pos p = Sugartypes.with_pos (pos p)

type signature = Sig of (name with_pos * datatype') with_pos | NoSig
let sig_of_opt = function
    | Some s -> Sig s
    | None   -> NoSig

(* Produces a datatype if a name is accompanied by a signature.  Raises an
   exception if name does not match a name in a signature. *)
let datatype_opt_from_sig_opt sig_opt name =
  match sig_opt with
  | Sig {node=({node=signame; _}, datatype); pos} ->
     (* Ensure that name in a signature matches name in a declaration *)
     if signame <> name then
       raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `" ^ name ^ "'.", pos));
     Some datatype
  | NoSig -> None

(* Create a record with a given list of labels *)
let make_record ppos lbls =
  with_pos ppos (`RecordLit (lbls, None))

let block ppos b = with_pos ppos (`Block b)

let datatype d = (d, None)

let cp_unit ppos = with_pos ppos (`Unquote ([], with_pos ppos (`TupleLit [])))

let present = `Present (with_dummy_pos `Unit)

(* this preserves 1-tuples *)
let make_tuple pos = function
  | [e] -> make_record pos [("1", e)]
  | es  -> with_pos pos (`TupleLit es)

let make_fun_binding sig_opt ppos (linearity, bndr, args, location, blk) =
  let datatype = datatype_opt_from_sig_opt sig_opt bndr.node in
  with_pos ppos (`Fun (make_untyped_binder bndr, linearity,
                       (* NOTE: position of the block is slightly inaccurate.
                          This is done to make parser code less verbose. *)
                       ([], (args, block ppos blk)), location, datatype))

(* Create a non-linear function binding with unknown location *)
let make_unl_fun_binding sig_opt ppos (bndr, args, block) =
  make_fun_binding sig_opt ppos (`Unl, bndr, args, `Unknown, block)

(* Create a linear function binding with unknown location *)
let make_lin_fun_binding sig_opt ppos (bndr, args, block) =
  make_fun_binding sig_opt ppos (`Lin, bndr, args, `Unknown, block)

let make_handler_binding sig_opt ppos (name, handlerlit) =
  let datatype = datatype_opt_from_sig_opt sig_opt name.node in
  with_pos ppos (`Handler (make_untyped_binder name, handlerlit, datatype))

let make_variable_pat ppos name =
  with_pos ppos (`Variable (make_untyped_binder name))

(* Used for passing an argument to make_val_binding *)
type name_or_pat = Name of name with_pos | Pat of pattern

(* Create a Val binding.  This function takes either a name for a variable
   pattern or an already constructed pattern.  In the latter case no signature
   should be passed.  *)
let make_val_binding sig_opt ppos (name_or_pat, phrase, location) =
  let pat, datatype = match name_or_pat with
    | Name name ->
       let pat      = make_variable_pat ppos name in
       let datatype = datatype_opt_from_sig_opt sig_opt name.node in
       (pat, datatype)
    | Pat pat ->
       assert (sig_opt = NoSig);
       (pat, None) in
    with_pos ppos (`Val (pat, ([], phrase), location, datatype))

let make_hnlit depth computation_param handler_param =
  (depth, computation_param, fst handler_param, snd handler_param)

(* Create a list of labeled database expressions *)
let make_db_exps ppos exps =
  with_pos ppos (`ListLit ([make_record ppos exps], None))

let is_empty_db_exps : phrase -> bool = function
  | {node=`ListLit ([{node=`RecordLit ([], _);_}], _);_} -> true
  | _                                                    -> false

(* Create a database insertion query.  Raises an exception when the list of
   labeled expression is empty and the returning variable has not been named.*)
let make_db_insert ppos ins_exp lbls exps var_opt =
  if is_empty_db_exps exps && var_opt == None then
    raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide a " ^
      "nonempty list of labeled expression or a return variable.", pos ppos));
  with_pos ppos (`DBInsert (ins_exp, lbls, exps, OptionUtils.opt_map
       (fun {node; pos} -> Sugartypes.with_pos pos (`Constant (`String node)))
       var_opt))

let make_spawn ppos spawn_kind location blk =
  with_pos ppos (`Spawn (spawn_kind, location, block ppos blk, None))

let make_infix_appl' ppos arg1 op arg2 =
  with_pos ppos (`InfixAppl (([], op), arg1, arg2))

let make_infix_appl ppos arg1 op arg2 =
  make_infix_appl' ppos arg1 (`Name op) arg2

let make_unary_appl ppos op arg =
  with_pos ppos (`UnaryAppl (([], op), arg))
