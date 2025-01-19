open CommonTypes
open Operators
open SourceCode
open SourceCode.WithPos
open Sugartypes
open Utility

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

  let fresh_type_variable freedom : SugarTypeVar.t =
    incr type_variable_counter;
    let name = "_" ^ string_of_int (!type_variable_counter) in
    SugarTypeVar.mk_unresolved name None freedom

  (** Helper data types and functions for passing arguments to smart
      constructors. *)

  (* Stores either a name of variable to be used in a binding pattern or the
     pattern itself.  Used for passing an argument to val_binding. *)
  type name_or_pat = PatName of Name.t | Pat of Pattern.with_pos

  (* Optionally stores a datatype signature.  Isomporphic to Option. *)
  type signature = (Name.t WithPos.t * datatype') WithPos.t option

  (* Produces a datatype if a name is accompanied by a signature.  Raises an
     exception if name does not match a name in a signature. *)
  let datatype_opt_of_sig_opt sig_opt name =
    match sig_opt with
    | Some {node=({node=signame; _}, datatype); pos} ->
       (* Ensure that name in a signature matches name in a declaration. *)
       if signame <> name then
         raise (ConcreteSyntaxError (pos,
               Printf.sprintf "Signature for `%s' should precede definition of `%s', not `%s'."
                  signame signame name));
       Some datatype
    | None -> None


  (** Common stuff *)

  let var ?(ppos=dp) name = with_pos ppos (Var name)
  let freeze_var ?(ppos=dp) name = with_pos ppos (FreezeVar name)

  (* Create a Block from block_body. *)
  let block_node       block_contents = Block block_contents
  let block ?(ppos=dp) block_contents =
    with_pos ppos (block_node block_contents)

  let datatype d = (d, None)

  (* Create a record with a given list of labels. *)
  let record ?(ppos=dp) ?exp lbls = with_pos ppos (RecordLit (lbls, exp))

  (* Create a tuple *)
  let tuple ?(ppos=dp) = function
    | es  -> with_pos ppos (TupleLit es)

  (* Create a tuple for orderby clauses (includes a hack to ensure
     that 1-tuples are preserved) *)
  let orderby_tuple ?(ppos=dp) = function
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

  let binder ?(ppos=dp) ?ty name = with_pos ppos (Binder.make ~name ?ty ())

  (** Imports **)

  let import ?(ppos=dp) ?(pollute=false) names = with_pos ppos (Import { path = names; pollute })

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
  let wild_present   = (Types.wild, present)
  let hear_present p = (Types.hear, Datatype.Present p)


  (** Rows *)

  let row_with    field (fields, row_var) = (field::fields, row_var)
  let row_with_wp fields                  = row_with wild_present fields
  let hear_arrow_prefix presence fields =
    row_with wild_present (row_with (hear_present presence) fields)


  (** Various phrases *)

  (* Create a Normal FunLit. *)
  let fun_lit ?(ppos=dp) ?args ?(location=loc_unknown) linearity pats blk =
    with_pos ppos (FunLit (args, linearity, NormalFunlit (pats, blk), location))

  let switch_fun_lit ?(ppos=dp) ?args ?(location=loc_unknown) linearity pats switch_funlit_body =
    with_pos ppos (FunLit (args, linearity, SwitchFunlit (pats, switch_funlit_body), location))

  (* Create a Spawn. *)
  let spawn ?(ppos=dp) ?row spawn_kind location blk =
    with_pos ppos (Spawn (spawn_kind, location, blk, row))

  let fn_appl_node ?(ppos=dp) name tyvars vars =
    FnAppl (with_pos ppos (tappl (FreezeVar name, tyvars)), vars)

  let fn_appl ?(ppos=dp) name tyvars vars =
    with_pos ppos (fn_appl_node ~ppos name tyvars vars)

  let fn_appl_var ?(ppos=dp) var1 var2 =
    fn_appl ~ppos var1 [] [var ~ppos var2]


  (** Bindings *)
  (* Create a function binding. *)
  let fun_binding ?(ppos=dp) sig_opt ?(unsafe_sig=false) ((linearity, frozen), bndr, args, location, blk) =
    let fun_signature = datatype_opt_of_sig_opt sig_opt bndr in
      with_pos ppos (Fun { fun_binder = binder bndr;
                           fun_linearity = linearity;
                           fun_definition = ([], NormalFunlit (args, blk));
                           fun_location = location;
                           fun_signature;
                           fun_frozen = frozen;
                           fun_unsafe_signature = unsafe_sig })

  let fun_binding' ?(ppos=dp) ?(linearity=dl_unl) ?(tyvars=[])
        ?(location=loc_unknown) ?annotation bndr fnlit =
    with_pos ppos (Fun { fun_binder = bndr;
                         fun_linearity = linearity;
                         fun_definition = (tyvars, fnlit);
                         fun_location = location;
                         fun_signature = annotation;
                         fun_frozen = false;
                         fun_unsafe_signature = false })

  let switch_fun_binding ?(ppos=dp) sig_opt ?(unsafe_sig=false) ((linearity, frozen), bndr, args, location, blk) =
    let fun_signature = datatype_opt_of_sig_opt sig_opt bndr in
      with_pos ppos (Fun { fun_binder = binder bndr;
                           fun_linearity = linearity;
                           fun_definition = ([], SwitchFunlit (args, blk));
                           fun_location = location;
                           fun_signature;
                           fun_frozen = frozen;
                           fun_unsafe_signature = unsafe_sig })

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
         assert (sig_opt = None);
         (pat, None) in
    with_pos ppos (Val (pat, ([], phrase), location, datatype))

  (* A commonly used wrapper around val_binding *)
  let val_binding ?(ppos=dp) pat phrase =
    val_binding' ~ppos None (Pat pat, phrase, loc_unknown)

  (* Create a module binding. *)
  let module_binding ?(ppos=dp) binder members =
    with_pos ppos (Module { binder; members })

  let type_abstraction ?(ppos=dp) tyvars phrase =
    with_pos ppos (TAbstr (tyvars, phrase))

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
  let db_insert ?(ppos=dp) tmp_ins ins_exp lbls exps var_opt =
    if is_empty_db_exps exps && var_opt == None then
      raise (ConcreteSyntaxError (pos ppos, "Invalid insert statement. " ^
          "Either provide a nonempty list of labeled expression or a return " ^
          "variable."));
    with_pos ppos (DBInsert (tmp_ins, ins_exp, lbls, exps,
       opt_map (fun name -> constant_str ~ppos name) var_opt))

  (* Create a query. *)
  let query ?(ppos=dp) phrases_opt policy blk =
    with_pos ppos (Query (phrases_opt, policy, blk, None))

  (* Create a temporal join block. *)
  let temporal_join ?(ppos=dp) mode blk =
    with_pos ppos (DBTemporalJoin (mode, blk, None))

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
  let validate_xml ?tags e = match e with
    | {node=Xml (name, attr_list, blk_opt, _); pos} ->
       (* check whether opening and closing tags match *)
       let () = match tags with
         | Some (opening, closing) when opening = closing -> ()
         | Some (opening, closing) ->
            raise (ConcreteSyntaxError (pos,
                Printf.sprintf "Closing tag '%s' does not match start tag '%s'."
                  closing opening))
         | _ -> () in
       (* Check uniqueness of attributes *)
       let xml_sugar_error pos message =
         let open Errors in
         raise (desugaring_error ~pos ~stage:CheckXML ~message) in

       let () =
         let attr_names = fst (List.split attr_list) in
         if ListUtils.has_duplicates attr_names then
           xml_sugar_error pos
            (Printf.sprintf "XML tag '%s' has duplicate attributes" name) in
       (* Check that XML forests don't have attributes *)
       let () =
         if name = "#" && (List.length attr_list != 0 || blk_opt <> None) then
           xml_sugar_error pos
            "XML forest literals cannot have attributes" in
       ()
    | _ -> assert false

  (* Create an XML tree.  Raise an exception if opening and closing tags don't
     match. *)
  let xml ?(ppos=dp) ?tags name attr_list blk_opt contents =
    let node = with_pos ppos (Xml (name, attr_list, blk_opt, contents)) in
    let ()   = validate_xml ?tags node in
    node

  (** Handlers *)
  let untyped_handler ?parameters expr (val_cases, eff_cases) depth =
    { sh_expr         = expr;
      sh_effect_cases = eff_cases;
      sh_value_cases  = val_cases;
      sh_descr = {
          shd_depth = depth;
          shd_types = ( Types.make_empty_closed_row (), Types.Not_typed
                      , Types.make_empty_closed_row (), Types.Not_typed);
          shd_raw_row = Types.make_empty_closed_row ();
          shd_params = opt_map (fun pps -> {shp_bindings = pps; shp_types = []})
                               parameters
        };
    }

  (** Tables *)
  let table ?(ppos=dp) ~tbl_keys tbl_name tbl_type
    tbl_field_constraints temporal tbl_database =
    let tbl_keys = OptionUtils.from_option (list ~ppos []) tbl_keys in
    let (tmp, tbl_temporal_fields) =
      match temporal with
        | None -> (Temporality.current, None)
        | Some (tmp, fields) ->
            (tmp, Some fields)
    in
    let tbl_type = (tmp, tbl_type, None) in
    with_pos ppos
    (TableLit {
        tbl_name;
        tbl_type;
        tbl_field_constraints;
        tbl_keys;
        tbl_temporal_fields;
        tbl_database
    })
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
