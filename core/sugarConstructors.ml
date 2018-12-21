open Sugartypes
open Operators
open Utility.OptionUtils

(* Import module signatures *)
module type Pos                  = SmartConstructorsIntf.Pos
module type SmartConstructorsSig = SmartConstructorsIntf.SmartConstructorsSig

(* Actual implementation of smart constructors as a functor on a Pos module *)
module SmartConstructors (Position : Pos)
       : (SmartConstructorsSig with type t := Position.t) = struct

  (** Convenient aliases for functions operating on positions *)
  let pos      = Position.pos
  let with_pos = Position.with_pos

  (** Generation of fresh type variables *)

  let type_variable_counter = ref 0

  let fresh_type_variable () : datatypenode =
    incr type_variable_counter;
    `TypeVar ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_type_variable () : datatypenode =
    incr type_variable_counter;
    `TypeVar ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)

  let fresh_row_variable () : row_var =
    incr type_variable_counter;
    `Open ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_row_variable () : row_var =
    incr type_variable_counter;
    `Open ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)

  let fresh_presence_variable () : fieldspec =
    incr type_variable_counter;
    `Var ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_presence_variable () : fieldspec =
    incr type_variable_counter;
    `Var ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)


  (** Helper data types and functions for passing arguments to smart
      constructors *)

  (* Stores either a name of variable to be used in a binding pattern or the
     pattern itself.  Used for passing an argument to val_binding *)
  type name_or_pat = Name of name with_pos | Pat of pattern

  (* Optionally stores a datatype signature.  Isomporphic to Option. *)
  type signature = Sig of (name with_pos * datatype') with_pos | NoSig
  let sig_of_opt = function
    | Some s -> Sig s
    | None   -> NoSig

  (* Produces a datatype if a name is accompanied by a signature.  Raises an
     exception if name does not match a name in a signature. *)
  let datatype_opt_of_sig_opt sig_opt name =
    match sig_opt with
    | Sig {node=({node=signame; _}, datatype); pos} ->
       (* Ensure that name in a signature matches name in a declaration *)
       if signame <> name then
         raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `" ^ name ^ "'.", pos));
       Some datatype
    | NoSig -> None


  (** Common stuff *)

  (* Create a Block from block_body *)
  let block {node;pos} = Sugartypes.with_pos pos (`Block node)
  let datatype d = (d, None)
  let cp_unit ppos = with_pos ppos (`Unquote ([], with_pos ppos (`TupleLit [])))

  (* Create a record with a given list of labels *)
  let record ppos lbls = with_pos ppos (`RecordLit (lbls, None))

  (* Creata a tuple.  Preserves 1-tuples *)
  let tuple pos = function
    | [e] -> record pos [("1", e)]
    | es  -> with_pos pos (`TupleLit es)

  (* Create a variable pattern with a given name *)
  let variable_pat ppos name =
    with_pos ppos (`Variable (make_untyped_binder name))


  (** Fieldspec *)

  let wild           = "wild"
  let hear           = "hear"
  let present        = `Present (with_dummy_pos `Unit)
  let wild_present   = (wild, present)
  let hear_present p = (hear, `Present p)


  (** Rows *)

  let fresh_row   unit                    = ([], fresh_rigid_row_variable unit)
  let row_with    field (fields, row_var) = (field::fields, row_var)
  let row_with_wp fields                  = row_with wild_present fields
  let hear_arrow_prefix presence fields =
    row_with wild_present (row_with (hear_present presence) fields)


  (** Various phrases *)

  (* Create a FunLit *)
  let fun_lit ppos linearity pats blk =
    with_pos ppos (`FunLit (None, linearity, (pats, block blk), `Unknown))

  (* Create an argument used by Handler and HandlerLit *)
  let hnlit_arg depth computation_param handler_param =
    (depth, computation_param, fst handler_param, snd handler_param)

  (* Create a HandlerLit *)
  let handler_lit ppos handlerlit = with_pos ppos (`HandlerLit handlerlit)

  (* Create a Spawn *)
  let spawn ppos spawn_kind location blk =
    with_pos ppos (`Spawn (spawn_kind, location, block blk, None))


  (** Bindings *)
  (* Create a function binding *)
  let fun_binding ppos sig_opt (linearity, bndr, args, location, blk) =
    let datatype = datatype_opt_of_sig_opt sig_opt bndr.node in
    with_pos ppos (`Fun (make_untyped_binder bndr, linearity,
                         ([], (args, block blk)), location, datatype))

  (* Create a handler binding *)
  let handler_binding ppos sig_opt (name, handlerlit) =
    let datatype = datatype_opt_of_sig_opt sig_opt name.node in
    with_pos ppos (`Handler (make_untyped_binder name, handlerlit, datatype))

  (* Create a Val binding.  This function takes either a name for a variable
     pattern or an already constructed pattern.  In the latter case no signature
     should be passed.  *)
  let val_binding ppos sig_opt (name_or_pat, phrase, location) =
    let pat, datatype = match name_or_pat with
      | Name name ->
         let pat      = variable_pat ppos name in
         let datatype = datatype_opt_of_sig_opt sig_opt name.node in
         (pat, datatype)
      | Pat pat ->
         assert (sig_opt = NoSig);
         (pat, None) in
    with_pos ppos (`Val (pat, ([], phrase), location, datatype))


  (** Database queries *)

  (* Create a list of labeled database expressions *)
  let db_exps ppos exps =
    with_pos ppos (`ListLit ([record ppos exps], None))

  (* Is the list of labeled database expressions empty? *)
  let is_empty_db_exps : phrase -> bool = function
    | {node=`ListLit ([{node=`RecordLit ([], _);_}], _);_} -> true
    | _                                                    -> false

  (* Create a database insertion query.  Raises an exception when the list of
   labeled expression is empty and the returning variable has not been named.*)
  let db_insert ppos ins_exp lbls exps var_opt =
    if is_empty_db_exps exps && var_opt == None then
      raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide" ^
          " a nonempty list of labeled expression or a return variable.",
           pos ppos));
    with_pos ppos (`DBInsert (ins_exp, lbls, exps, opt_map
       (fun {node; pos} -> Sugartypes.with_pos pos (`Constant (`String node)))
       var_opt))

  (* Create a query *)
  let query ppos phrases_opt blk =
    with_pos ppos (`Query (phrases_opt, block blk, None))


  (** Operator applications *)
  (* Apply a binary infix operator *)
  let infix_appl' ppos arg1 op arg2 =
    with_pos ppos (`InfixAppl (([], op), arg1, arg2))

  (* Apply a binary infix operator with a specified name *)
  let infix_appl ppos arg1 op arg2 =
    infix_appl' ppos arg1 (`Name op) arg2

  (* Apply an unary operator *)
  let unary_appl ppos op arg =
    with_pos ppos (`UnaryAppl (([], op), arg))

  (** XML *)
  (* Create an XML tree.  Raise an exception if opening and closing tags don't
   match. *)
  let xml ppos tags_opt name attr_list blk_opt contents =
    let () = match tags_opt with
      | Some (opening, closing) when opening = closing -> ()
      | Some (opening, closing) ->
         raise (ConcreteSyntaxError
                  ("Closing tag '" ^ closing ^ "' does not match start tag '"
                   ^ opening ^ "'.", pos ppos))
      | _ -> () in
    let blk = opt_map (fun blk -> block blk) blk_opt in
    with_pos ppos (`Xml (name, attr_list, blk, contents))

  (** Handlers *)
  let untyped_handler ?(val_cases = []) ?parameters expr eff_cases depth =
    { sh_expr         = expr;
      sh_effect_cases = eff_cases;
      sh_value_cases  = val_cases;
      sh_descr = {
          shd_depth = depth;
          shd_types = ( Types.make_empty_closed_row (), `Not_typed
                      , Types.make_empty_closed_row (), `Not_typed);
          shd_raw_row = Types.make_empty_closed_row ();
          shd_params = opt_map (fun pps -> {shp_bindings = pps; shp_types = []})
                               parameters
        };
    }

end

(* A default type of positions *)
module SugartypesPosition
       : Pos with type t = (SourceCode.lexpos * SourceCode.lexpos *
                            SourceCode.source_code option) = struct
  type t = SourceCode.lexpos * SourceCode.lexpos * SourceCode.source_code option
  let pos    p = p
  let with_pos = Sugartypes.with_pos
end

module Make = SmartConstructors(SugartypesPosition)

(* Dummy positions for nodes constructed during compilation (such nodes don't
   have a corresponding position in the original program source code). *)
module DummyPosition : Pos with type t = unit = struct
  type t          = unit
  let pos ()      = Sugartypes.dummy_position
  let with_pos () = Sugartypes.with_pos Sugartypes.dummy_position
end

module DummyMake = SmartConstructors(DummyPosition)
