open CommonTypes
open Operators
open SourceCode
open SourceCode.WithPos
open Sugartypes
open Utility.OptionUtils

(* Import module signatures. *)
module type Pos                  = SugarConstructorsIntf.Pos
module type SugarConstructorsSig = SugarConstructorsIntf.SugarConstructorsSig

(* Actual implementation of smart constructors as a functor on a Pos module. *)
module SugarConstructors (Position : Pos)
       : (SugarConstructorsSig with type t := Position.t) = struct

  (** Convenient aliases for functions operating on positions. *)
  let pos      = Position.pos
  let with_pos = Position.with_pos
  let dp       = Position.dp

  (** Attach a dummy position to a node. *)
  let with_dummy_pos node = with_pos dp node

  (** Generation of fresh type variables. *)

  let type_variable_counter = ref 0

  let fresh_type_variable () : Datatype.t =
    incr type_variable_counter;
    Datatype.TypeVar ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_type_variable () : Datatype.t =
    incr type_variable_counter;
    Datatype.TypeVar ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)

  let fresh_row_variable () : Datatype.row_var =
    incr type_variable_counter;
    Datatype.Open ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_row_variable () : Datatype.row_var =
    incr type_variable_counter;
    Datatype.Open ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)

  let fresh_presence_variable () : Datatype.fieldspec =
    incr type_variable_counter;
    Datatype.Var ("_" ^ string_of_int (!type_variable_counter), None, `Flexible)

  let fresh_rigid_presence_variable () : Datatype.fieldspec =
    incr type_variable_counter;
    Datatype.Var ("_" ^ string_of_int (!type_variable_counter), None, `Rigid)


  (** Helper data types and functions for passing arguments to smart
      constructors. *)

  (* Stores either a name of variable to be used in a binding pattern or the
     pattern itself.  Used for passing an argument to val_binding. *)
  type name_or_pat = PatName of name | Pat of Pattern.with_pos

  (* Optionally stores a datatype signature.  Isomporphic to Option. *)
  type signature = Sig of (name WithPos.t * datatype') WithPos.t | NoSig
  let sig_of_opt = function
    | Some s -> Sig s
    | None   -> NoSig

  (* Produces a datatype if a name is accompanied by a signature.  Raises an
     exception if name does not match a name in a signature. *)
  let datatype_opt_of_sig_opt sig_opt name =
    match sig_opt with
    | Sig {node=({node=signame; _}, datatype); pos} ->
       (* Ensure that name in a signature matches name in a declaration. *)
       if signame <> name then
         raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `" ^ name ^ "'.", pos));
       Some datatype
    | NoSig -> None


  (** Common stuff *)

  let var ?(ppos=dp) name = with_pos ppos (Var name)

  (* Create a Block from block_body. *)
  let block_node       block_contents = Block block_contents
  let block ?(ppos=dp) block_contents =
    with_pos ppos (block_node block_contents)

  let datatype d = (d, None)

  (* Create a record with a given list of labels. *)
  let record ?(ppos=dp) ?exp lbls = with_pos ppos (RecordLit (lbls, exp))

  (* Create a tuple.  Preserves 1-tuples. *)
  let tuple ?(ppos=dp) = function
    | [e] -> record ~ppos [("1", e)]
    | es  -> with_pos ppos (TupleLit es)

  let cp_unit ppos = with_pos ppos (CPUnquote ([], tuple ~ppos []))

  let list ?(ppos=dp) ?ty elems =
    with_pos ppos (ListLit (elems, ty))

  let constructor ?(ppos=dp) ?body ?ty name =
    with_pos ppos (ConstructorLit (name, body, ty))


  (** Constants **)

  let constant      ?(ppos=dp) c = with_pos ppos (Constant c)
  let constant_str  ?(ppos=dp) s = with_pos ppos (Constant (Constant.String s))
  let constant_char ?(ppos=dp) c = with_pos ppos (Constant (Constant.Char   c))


  (** Binders **)

  let binder ?(ppos=dp) ?ty name = with_pos ppos (name, ty)


  (** Patterns *)

  (* Create a variable pattern with a given name. *)
  let variable_pat ?(ppos=dp) ?ty name =
    with_pos ppos (Pattern.Variable (binder ~ppos ?ty name))

  (* Create a tuple pattern. *)
  let tuple_pat ?(ppos=dp) pats =
    with_pos ppos (Pattern.Tuple pats)

  let any_pat ppos = with_pos ppos Pattern.Any

  (** Fieldspec *)

  let present        = Datatype.Present (WithPos.dummy Datatype.Unit)
  let wild_present   = ("wild", present)
  let hear_present p = ("hear", Datatype.Present p)


  (** Rows *)

  let fresh_row   unit                    = ([], fresh_rigid_row_variable unit)
  let row_with    field (fields, row_var) = (field::fields, row_var)
  let row_with_wp fields                  = row_with wild_present fields
  let hear_arrow_prefix presence fields =
    row_with wild_present (row_with (hear_present presence) fields)


  (** Various phrases *)

  (* Create a FunLit. *)
  let fun_lit ?(ppos=dp) ?args ?(location=loc_unknown) linearity pats blk =
    with_pos ppos (FunLit (args, linearity, (pats, blk), location))

  (* Create an argument used by Handler and HandlerLit. *)
  let hnlit_arg depth computation_param handler_param =
    (depth, computation_param, fst handler_param, snd handler_param)

  (* Create a HandlerLit. *)
  let handler_lit ?(ppos=dp) handlerlit =
    with_pos ppos (HandlerLit handlerlit)

  (* Create a Spawn. *)
  let spawn ?(ppos=dp) ?row spawn_kind location blk =
    with_pos ppos (Spawn (spawn_kind, location, blk, row))

  let fn_appl_node ?(ppos=dp) name tyvars vars =
    FnAppl (with_pos ppos (tappl (Var name, tyvars)), vars)

  let fn_appl ?(ppos=dp) name tyvars vars =
    with_pos ppos (fn_appl_node ~ppos name tyvars vars)

  let fn_appl_var ?(ppos=dp) var1 var2 =
    fn_appl ~ppos var1 [] [var ~ppos var2]


  (** Bindings *)
  (* Create a function binding. *)
  let fun_binding ?(ppos=dp) sig_opt (linearity, bndr, args, location, blk) =
    let datatype = datatype_opt_of_sig_opt sig_opt bndr in
    with_pos ppos (Fun (binder bndr, linearity,
                         ([], (args, blk)), location, datatype))

  let fun_binding' ?(ppos=dp) ?(linearity=dl_unl) ?(tyvars=[])
        ?(location=loc_unknown) ?annotation bndr fnlit =
    with_pos ppos (Fun (bndr, linearity, (tyvars, fnlit), location, annotation))


  (* Create a handler binding. *)
  let handler_binding ?(ppos=dp) sig_opt (name, handlerlit) =
    let datatype = datatype_opt_of_sig_opt sig_opt name in
    with_pos ppos (Handler (binder name, handlerlit, datatype))

  (* Create a Val binding.  This function takes either a name for a variable
     pattern or an already constructed pattern.  In the latter case no signature
     should be passed. *)
  let val_binding' ?(ppos=dp) sig_opt (name_or_pat, phrase, location) =
    let pat, datatype = match name_or_pat with
      | PatName name ->
         let pat      = variable_pat ~ppos name in
         let datatype = datatype_opt_of_sig_opt sig_opt name in
         (pat, datatype)
      | Pat pat ->
         assert (sig_opt = NoSig);
         (pat, None) in
    with_pos ppos (Val (pat, ([], phrase), location, datatype))

  (* A commonly used wrapper around val_binding *)
  let val_binding ?(ppos=dp) pat phrase =
    val_binding' ~ppos NoSig (Pat pat, phrase, loc_unknown)


  (** Database queries *)

  (* Create a list of labeled database expressions. *)
  let db_exps ?(ppos=dp) exps =
    list ~ppos [record ~ppos exps]

  (* Is the list of labeled database expressions empty? *)
  let is_empty_db_exps : phrase -> bool = function
    | {node=ListLit ([{node=RecordLit ([], _);_}], _);_} -> true
    | _                                                  -> false

  (* Create a database insertion query.  Raises an exception when the list of
     labeled expression is empty and the returning variable has not been named.
     *)
  let db_insert ?(ppos=dp) ins_exp lbls exps var_opt =
    if is_empty_db_exps exps && var_opt == None then
      raise (ConcreteSyntaxError ("Invalid insert statement.  Either provide" ^
          " a nonempty list of labeled expression or a return variable.",
           pos ppos));
    with_pos ppos (DBInsert (ins_exp, lbls, exps,
       opt_map (fun name -> constant_str ~ppos name) var_opt))

  (* Create a query. *)
  let query ?(ppos=dp) phrases_opt blk =
    with_pos ppos (Query (phrases_opt, blk, None))


  (** Operator applications *)
  (* Apply a binary infix operator. *)
  let infix_appl' ?(ppos=dp) arg1 op arg2 =
    with_pos ppos (InfixAppl (([], op), arg1, arg2))

  (* Apply a binary infix operator with a specified name. *)
  let infix_appl ?(ppos=dp) arg1 op arg2 =
    infix_appl' ~ppos arg1 (BinaryOp.Name op) arg2

  (* Apply an unary operator. *)
  let unary_appl ?(ppos=dp) op arg =
    with_pos ppos (UnaryAppl (([], op), arg))

  (** XML *)
  (* Create an XML tree.  Raise an exception if opening and closing tags don't
     match. *)
  let xml ?(ppos=dp) ?tags name attr_list blk_opt contents =
    let () = match tags with
      | Some (opening, closing) when opening = closing -> ()
      | Some (opening, closing) ->
         raise (ConcreteSyntaxError
                  ("Closing tag '" ^ closing ^ "' does not match start tag '"
                   ^ opening ^ "'.", pos ppos))
      | _ -> () in
    with_pos ppos (Xml (name, attr_list, blk_opt, contents))

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

(* Positions module based on standard Sugartypes positions. *)
module SugartypesPos : Pos with type t = Position.t = struct
  (* Sugartypes position *)
  type t = Position.t
  (* Identity - positions in this module are alread Sugartypes positions *)
  let pos p = p
  (* Construct a node with position *)
  let with_pos pos node = WithPos.make ~pos node
  (* Default (dummy) position *)
  let dp = Position.dummy
end

(* Positions module based on dummy/unit positions.  Prevents attaching any
   kind of meaningful positions to nodes.  *)
module DummyPos : Pos with type t = unit = struct
  type t = unit
  let pos ()           = Position.dummy
  let with_pos () node = WithPos.dummy node
  let dp               = ()
end

module SugartypesPositions = SugarConstructors(SugartypesPos)
module DummyPositions      = SugarConstructors(DummyPos     )

(*

Note [Attaching positions to nodes]
===================================

Positions represent locations of tokens inside a source file (roughly: filename
+ line number + column offset) and are attached to most nodes in an AST -
c.f. datatype definitions in Sugartypes.  During various stages of the
compilation pipeline we change the structure of an AST, often creating new nodes
that don't really correspond to any tokens in the source files.  In theory these
nodes should have dummy positions attached to them but in practice we often
maintain positions to know from which source code location a given node was
derived.  This is mostly used when debugging the compiler.

All that being said, there are two modules providing concrete access to
SugarConstructors:

  * SugartypesPositions provides constructors that allow to attach normal
    Sugartypes positions to nodes.  Used to traverse the AST and to propagate
    positions to derived nodes.

  * DummyPositions provides constructors that allow only to attach dummy
    positions to nodes.  Used to erase positions in derived nodes.

Note that all smart constructors have an optional position argument that
defaults to a dummy position.  These default arguments are usually only provided
in the parser, so the main difference between using SugartypesPositions and
DummyPositions lies in the usage of with_pos function.

All of this is a bit inconsistent at the moment.  There are no strict rules when
to propagate positions to derived nodes and when to discard them, so use your
best judgement.

*)
