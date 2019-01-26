open Utility
open Types
open Sugartypes

(* let constrain_absence_types = Basicsettings.Typing.contrain_absence_types *)

let endbang_antiquotes = Basicsettings.TypeSugar.endbang_antiquotes

let check_top_level_purity = Basicsettings.TypeSugar.check_top_level_purity

let dodgey_type_isomorphism = Basicsettings.TypeSugar.dodgey_type_isomorphism

module Env = Env.String

module Utils : sig
  val dummy_source_name : unit -> name
  val unify : Types.datatype * Types.datatype -> unit
  val instantiate : Types.environment -> string ->
                    (Types.type_arg list * Types.datatype)
  val generalise : Types.environment -> Types.datatype ->
                   ((Types.quantifier list*Types.type_arg list) * Types.datatype)

  (* val is_pure : phrase -> bool *)
  val is_pure_binding : binding -> bool
  val is_generalisable : phrase -> bool
end =
struct
  let counter = ref 0
  let dummy_source_name () =
    counter := !counter + 1;
    "DUMMY(" ^ string_of_int !counter ^ ")"

  let unify = Unify.datatypes
  let instantiate = Instantiate.var
  let generalise = Generalise.generalise

  let rec opt_generalisable o = opt_app is_pure true o
  and is_pure p = match p.node with
    | `Constant _
    | `Var _
    | `QualifiedVar _
    | `FunLit _
    | `DatabaseLit _
    | `TableLit _
    | `TextNode _
    | `HandlerLit _
    | `Section _ -> true

    | `ListLit (ps, _)
    | `TupleLit ps -> List.for_all is_pure ps
    | `RangeLit (e1, e2) -> is_pure e1 && is_pure e2
    | `TAbstr (_, p)
    | `TAppl (p, _)
    | `Projection (p, _)
    | `TypeAnnotation (p, _)
    | `Upcast (p, _, _)
    | `Escape (_, p) -> is_pure p
    | `ConstructorLit (_, p, _) -> opt_generalisable p
    | `RecordLit (fields, p) ->
        List.for_all (snd ->- is_pure) fields && opt_generalisable p
    | `With (p, fields) ->
        List.for_all (snd ->- is_pure) fields && is_pure p
    | `Block (bindings, e) ->
        List.for_all is_pure_binding bindings && is_pure e
    | `Conditional (p1, p2, p3) ->
        is_pure p1
     && is_pure p2
     && is_pure p3
    | `Xml (_, attrs, attrexp, children) ->
        List.for_all (snd ->- List.for_all is_pure) attrs
     && opt_generalisable attrexp
     && List.for_all (is_pure) children
    | `Formlet (p1, p2) ->
        is_pure p1 && is_pure p2
    | `Regex r -> is_pure_regex r
    | `Iteration _ (* could do a little better in some of these cases *)
    | `Page _
    | `FormletPlacement _
    | `PagePlacement _
    | `UnaryAppl _
    | `FormBinding _
    | `InfixAppl _
    | `Spawn _
    | `Query _
    | `FnAppl _
    | `Handle _
    | `Switch _
    | `Receive _
    | `Select _
    | `Offer _
    | `CP _
    (* | `Fork _ *)
    | `LensLit _
    | `LensKeysLit _
    | `LensFunDepsLit _
    | `LensDropLit _
    | `LensSelectLit _
    | `LensJoinLit _
    | `LensGetLit _
    | `LensPutLit _
    | `DoOperation _
    | `DBDelete _
    | `DBInsert _
    | `TryInOtherwise _
    | `Raise
    | `DBUpdate _ -> false
  and is_pure_binding ({node ; _ }: binding) = match node with
      (* need to check that pattern matching cannot fail *)
    | `QualifiedImport _
    | `AlienBlock _
    | `Module _
    | `Fun _
    | `Funs _
    | `Infix
    | `Type _
    | `Handler _
    | `Foreign _ -> true
    | `Exp p -> is_pure p
    | `Val (pat, (_, rhs), _, _) ->
        is_safe_pattern pat && is_pure rhs
  and is_safe_pattern {node = pat; _} = match pat with
      (* safe patterns cannot fail *)
    | `Nil
    | `Cons _
    | `List _
    | `Constant _ -> false
    (* NOTE: variant assigment is typed such that it must always succeed *)
    | `Variant (_, None) -> true
    | `Variant (_, Some p) -> is_safe_pattern p
    | `Negative _ -> true
    | `Any
    | `Variable _ -> true
    | `Record (ps, None) -> List.for_all (snd ->- is_safe_pattern) ps
    | `Record (ps, Some p) -> List.for_all (snd ->- is_safe_pattern) ps && is_safe_pattern p
    | `Tuple ps -> List.for_all is_safe_pattern ps
    | `HasType (p, _)
    | `As (_, p) -> is_safe_pattern p
    | `Effect (_, ps, k) ->
       List.for_all is_safe_pattern ps && is_safe_pattern k
  and is_pure_regex = function
      (* don't check whether it can fail; just check whether it
         contains non-generilisable sub-expressions *)
    | `Range _
    | `Simply _
    | `Any
    | `StartAnchor
    | `EndAnchor -> true
    | `Group r
    | `Repeat (_, r)
    | `Quote r -> is_pure_regex r
    | `Seq rs -> List.for_all is_pure_regex rs
    | `Alternate (r1, r2) -> is_pure_regex r1 && is_pure_regex r2
    | `Splice p -> is_pure p
    | `Replace (r, `Literal _) -> is_pure_regex r
    | `Replace (r, `Splice p) -> is_pure_regex r && is_pure p

  let is_generalisable = is_pure
end

(*

Note [Variable names in error messages]
=======================================

When generating error message with a griper we take care to generate type
variable names that are consistent across a single error message.  This is
achieved with a hash table that maintains a list of variables appearing in types
in a given error message.  This hash table is shared between subsequent
invocations of show_type within a single griper.

To ensure that variable names are generated correctly each griper that generates
type variable names must call build_tyvar_names helper function, passing in a
list of types for which we want to generate names.  This resets the hash table
that stores the type variables, making sure that generated names will start from
initial letters of the alphabet.  In case of some grippers we must also generate
names for row variables - add_rowvar_names takes care of that.  Note that there
are several helper functions like but, but2things, or fixed_type, that are
called by grippers.  These must NOT call build_tyvar_names or add_rowvar_names.

See also Note [Refreshing type variable names] and #43

 *)

module Gripers :
sig
  type griper =
      pos:SourceCode.pos ->
  t1:(string * Types.datatype) ->
  t2:(string * Types.datatype) ->
  error:Unify.error ->
  unit

  val die : SourceCode.pos -> string -> 'a

  val if_condition : griper
  val if_branches  : griper

  val switch_pattern : griper
  val switch_patterns : griper
  val switch_branches : griper

  val handle_parameter_pattern : griper
  val handle_value_patterns : griper
  val handle_effect_patterns : griper
  val handle_branches : griper
  val type_resumption_with_annotation : griper
  val deep_resumption : griper
  val deep_resumption_effects : griper
  val shallow_resumption : griper
  val shallow_resumption_effects : griper
  val handle_return : griper
  val handle_comp_effects : griper
  val handle_unify_with_context : griper

  val do_operation : griper

  val extend_record : griper
  val record_with : griper

  val list_lit : griper

  val table_name : griper
  val table_db : griper
  val table_keys : griper

  val delete_table : griper
  val delete_pattern : griper
  val delete_where : griper
  val delete_outer : griper

  val insert_table : griper
  val insert_values : griper
  val insert_read : griper
  val insert_write : griper
  val insert_needed : griper
  val insert_outer : griper

  val insert_id : griper
  val update_table : griper
  val update_pattern : griper
  val update_where : griper
  val update_read : griper
  val update_write : griper
  val update_needed : griper
  val update_outer : griper

  val range_bound : griper

  val spawn_outer : griper
  val spawn_wait_outer : griper
  val spawn_location : griper

  val query_outer : griper
  val query_base_row : griper

  val receive_mailbox : griper
  val receive_patterns : griper

  val unary_apply : griper
  val infix_apply : griper
  val fun_apply : griper

  val xml_attribute : griper
  val xml_attributes : griper
  val xml_child : griper

  val formlet_body : griper
  val page_body : griper

  val render_formlet : griper
  val render_handler : griper
  val render_attributes : griper

  val page_placement : griper

  val form_binding_body : griper
  val form_binding_pattern : griper

  val iteration_list_body : griper
  val iteration_list_pattern : griper
  val iteration_table_body : griper
  val iteration_table_pattern : griper
  val iteration_body : griper
  val iteration_where : griper
  val iteration_base_order : griper
  val iteration_base_body : griper

  val escape : griper
  val escape_outer : griper

  val projection : griper

  val upcast_source : griper
  val upcast_subtype : SourceCode.pos -> Types.datatype -> Types.datatype -> 'a

  val value_restriction : SourceCode.pos -> Types.datatype -> 'a

  val toplevel_purity_restriction : SourceCode.pos -> Sugartypes.binding -> 'a

  val duplicate_names_in_pattern : SourceCode.pos -> 'a

  val type_annotation : griper

  val bind_val : griper
  val bind_val_annotation : griper

  val bind_fun_annotation : griper
  val bind_fun_return : griper

  val bind_rec_annotation : griper
  val bind_rec_rec : griper

  val bind_exp : griper

  val list_pattern : griper
  val cons_pattern : griper
  val record_pattern : griper
  val pattern_annotation : griper

  val splice_exp : griper

  (* val link_session : griper *)
  (* val link_dual : griper *)

  val offer_variant : griper
  val offer_patterns : griper

  val selection : griper

  val cp_unquote : griper
  val cp_grab : string -> griper
  val cp_give : string -> griper
  val cp_select : string -> griper
  val cp_offer_choice : string -> griper
  val cp_offer_branches : griper
  val cp_comp_left : griper
  val cp_link_session : griper
  val cp_link_dual : griper

  val non_linearity : SourceCode.pos -> int -> string -> Types.datatype -> unit

  val try_in_unless_pat : griper
  val try_in_unless_branches : griper
  val try_in_unless_linearity : SourceCode.pos -> string -> unit

end
  = struct
    type griper =
        pos:SourceCode.pos ->
      t1:(string * Types.datatype) ->
      t2:(string * Types.datatype) ->
      error:Unify.error ->
      unit

    let wm () = Settings.get_value Basicsettings.web_mode

    let code s =
      if wm () then
        "<code>" ^ s ^ "</code>"
      else
        "`"^ s ^ "'"

    let nl () =
      if wm () then
        "<br />\n"
      else
        "\n"

    let tab () =
      if wm () then
        "&nbsp;&nbsp;&nbsp;&nbsp;"
      else
        "    "

    (* New line with indentation *)
    let nli () = nl () ^ tab ()

    (* Always display fresh variables when printing error messages *)
    let error_policy () = { (Types.Print.default_policy ()) with Types.Print.hide_fresh = false }

    (* Do not automatically refresh type variable names when pretty-printing
       types in error messages.  This will be done manually by calling
       build_tyvar_names in the gripers.
       See Notes [Variable names in error messages] and [Refreshing type variable names] *)
    let show_type   = Types.string_of_datatype ~policy:error_policy ~refresh_tyvar_names:false
    let show_row    = Types.string_of_row      ~policy:error_policy ~refresh_tyvar_names:false
    let show_effectrow row = "{" ^ (Types.string_of_row ~policy:error_policy ~refresh_tyvar_names:false row) ^ "}"

    (* Wrappers for generating type variable names *)
    let build_tyvar_names =
      Types.build_tyvar_names (Types.free_bound_type_vars ~include_aliases:true)
    let add_rowvar_names =
      Types.add_tyvar_names (Types.free_bound_row_type_vars ~include_aliases:true)

    let die pos msg = raise (Errors.Type_error (pos, msg))

    (* See Note [Variable names in error messages] *)
    let but (expr, t) =
      let ppr_t = show_type t in
      ", but the expression" ^ nli () ^
       code expr             ^ nl  () ^
      "has type"             ^ nli () ^
       code ppr_t

    let but2things (lthing, (lexpr, lt)) (rthing, (rexpr, rt)) =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      ", but the " ^ lthing ^ nli () ^
       code lexpr           ^ nl  () ^
      "has type"            ^ nli () ^
       code ppr_lt          ^ nl  () ^
      "while the " ^ rthing ^ nli () ^
       code rexpr           ^ nl  () ^
      "has type"            ^ nli () ^
       code ppr_rt

    let but2 l r = but2things ("expression", l) ("expression", r)

    let with_but pos s et =
      die pos (s ^ but et)

    let with_but2 pos s l r =
      die pos (s ^ but2 l r)

    let with_but2things pos s l r =
      die pos (s ^ but2things l r)

    let fixed_type pos thing t l =
      let ppr_t = show_type t in
      with_but pos (thing ^ " must have type " ^ code ppr_t) l

    let if_condition ~pos ~t1:l ~t2:(_,t) ~error:_ =
      fixed_type pos ("The condition of an " ^ code "if (...) ... else ..." ^
                      " expression") t l

    let if_branches ~pos ~t1:l ~t2:r ~error:_ =
      with_but2 pos ("Both branches of an " ^ code "if (...) ... else ..." ^
                     " expression should have the same type") l r

    let switch_pattern ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The type of an input to a switch should match the type of " ^
                  "its patterns, but the expression" ^ nli () ^
                  code lexpr                        ^ nl  () ^
                  "has type"                         ^ nli () ^
                  code ppr_lt                       ^ nl  () ^
                  "while the patterns have type"     ^ nli () ^
                  code ppr_rt)

    let switch_patterns ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("All the cases of a switch should have compatible patterns, " ^
                  "but the pattern"                         ^ nli () ^
                  code lexpr                               ^ nl  () ^
                  "has type"                                ^ nli () ^
                  code ppr_lt                              ^ nl  () ^
                  "while the subsequent patterns have type" ^ nli () ^
                  code ppr_rt)

    let switch_branches ~pos ~t1:(lexpr, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("All the cases of a switch should have the same type, but " ^
                  "the expression"                             ^ nli () ^
                  code lexpr                                  ^ nl  () ^
                  "has type"                                   ^ nli () ^
                  code ppr_lt                                 ^ nl  () ^
                  "while the subsequent expressions have type" ^ nli () ^
                  code ppr_rt)

    let handle_parameter_pattern ~pos ~t1:l ~t2:(rexpr, rt) ~error:_ =
      build_tyvar_names [snd l; rt];
        with_but2things pos
          ("The parameter pattern must match the expression in a handle parameter binding")
          ("pattern", l) ("expression", (rexpr, rt))

    let handle_value_patterns ~pos ~t1:(lexpr,lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("\
		All the value cases of a handle should have compatible patterns, " ^
                  "but the pattern" ^ nli () ^
                  code lexpr ^ nl () ^
                  "has type" ^ nli () ^
		  code ppr_lt ^ nl () ^
                  "while the subsequent patterns have type" ^ nli () ^
                  code ppr_rt)

    let handle_effect_patterns ~pos ~t1:(lexpr,lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("\
		All the effect cases of a handle should have compatible patterns, " ^
                  "but the pattern" ^ nli () ^
                  code lexpr ^ nl () ^
                  "has type" ^ nli () ^
		  code ppr_lt ^ nl () ^
                  "while the subsequent patterns have type" ^ nli () ^
                  code ppr_rt)

    let handle_branches ~pos ~t1:(lexpr, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("\
		All handler clauses must have the same type, but
		the clause expression" ^ nl() ^
		  tab() ^ code lexpr ^ nl() ^
		  "has type" ^ nl() ^
		  tab() ^ code (show_type lt) ^ nl() ^
		  "while the subsequent clauses have type" ^ nl() ^
		  tab() ^ code (show_type rt))

    let handle_return ~pos ~t1:(hexpr, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("The type of an input to a handle should match the type of " ^
               "its value patterns, but the expression" ^ nl() ^
               tab() ^ code hexpr ^ nl() ^
               "has type" ^ nl() ^
               tab() ^ code (show_type lt) ^ nl () ^
               "while the value patterns have type" ^ nl() ^
               tab() ^ code (show_type rt))

    let handle_comp_effects ~pos ~t1:(hexpr, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("The effect type of an input to a handle should match the type of " ^
               "its computation patterns, but the expression" ^ nl() ^
               tab() ^ code hexpr ^ nl() ^
               "has effect type" ^ nl() ^
               tab() ^ code (show_effectrow (TypeUtils.extract_row lt)) ^ nl() ^
               "while the handler handles effects" ^ nl() ^
               tab() ^ code (show_effectrow (TypeUtils.extract_row rt)))


    let handle_unify_with_context ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names[lt;rt];
      die pos ("The handle has effect type " ^ nl() ^
                  tab() ^ code (show_effectrow (TypeUtils.extract_row lt)) ^ nl() ^
		  "but, the currently allowed effects are" ^ nl() ^
                  tab() ^ code (show_effectrow (TypeUtils.extract_row rt)))


    let type_resumption_with_annotation ~pos ~t1:(resume,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("\
                The resumption" ^ nl() ^
                  tab() ^ code resume ^ nl() ^
                  "has type" ^ nl() ^
                  tab() ^ code (show_type lt) ^ nl() ^
                  "but it is annotated with type" ^ nl() ^
                  tab() ^ code (show_type rt))

    let deep_resumption ~pos ~t1:(resume,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("\
                 The return type of a deep resumption must be the same as the body type of its handler, " ^ nl() ^
                 "but the deep resumption" ^ nl() ^
                  tab() ^ code resume ^ nl() ^
                  "has return type" ^ nl() ^
                  tab() ^ code (show_type lt) ^ nl() ^
                  "while the body type of its handler is" ^ nl() ^
                  tab() ^ code (show_type rt))

    let deep_resumption_effects ~pos ~t1:(resume,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("A deep resumption may only perform the same effects as its handler, " ^ nl() ^
                  "but the deep resumption" ^nl() ^
                  tab() ^ code resume ^ nl() ^
                  "can perform effects" ^ nl() ^
                  tab() ^ code (show_type lt) ^ nl() ^
                  "while its handler can perform effects" ^ nl() ^
                  tab() ^ code (show_type rt))

    let shallow_resumption ~pos ~t1:(resume,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("\
                 The return type of a shallow resumption must be the same as the return type of the computation being handled, " ^ nl() ^
                  "but the shallow resumption" ^ nl() ^
                  tab() ^ code resume ^ nl() ^
                  "has return type" ^ nl() ^
                  tab() ^ code (show_type lt) ^ nl() ^
                  "while the computation has return type" ^ nl() ^
                  tab() ^ code (show_type rt))

    let shallow_resumption_effects ~pos ~t1:(resume,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("A shallow resumption may only perform the same effects as the computation being handled, " ^ nl() ^
                  "but the shallow resumption" ^nl() ^
                  tab() ^ code resume ^ nl() ^
                  "can perform effects" ^ nl() ^
                  tab() ^ code (show_type lt) ^ nl() ^
                  "while the computation can perform effects" ^ nl() ^
                  tab() ^ code (show_type rt))

    let do_operation ~pos ~t1:(_,lt) ~t2:(rexpr,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let dropDoPrefix = Str.substitute_first (Str.regexp "do ") (fun _ -> "") in
      let operation = dropDoPrefix (code rexpr) in
      die pos ("Invocation of the operation " ^ nl() ^
		  tab() ^ operation ^ nl() ^
		  "requires an effect context " ^ nl() ^
		  tab() ^ code (show_effectrow (TypeUtils.extract_row rt)) ^ nl() ^
		  "but, the currently allowed effects are" ^ nl()
               ^ tab() ^ code ( show_effectrow (TypeUtils.extract_row lt)))

    (* BUG: This griper is a bit rubbish because it doesn't distinguish
    between two different errors. *)
    let extend_record ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~error:_ =
      build_tyvar_names [lt;t];
      let ppr_lt = show_type lt in
      let ppr_t  = show_type  t in
      die pos ("Only a record can be extended, and it must be extended with " ^
               "different fields, but the expression" ^ nli () ^
                code lexpr                            ^ nl  () ^
               "has type"                             ^ nli () ^
                code ppr_lt                           ^ nl  () ^
               "while the extension fields have type" ^ nli () ^
                code ppr_t)

    let record_with ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~error:_ =
      build_tyvar_names [lt;t];
      let ppr_lt = show_type lt in
      let ppr_t  = show_type  t in
      die pos ("A record can only be updated with compatible fields, " ^
               "but the expression"                ^ nli () ^
                code lexpr                         ^ nl  () ^
               "has type"                          ^ nli () ^
                code ppr_lt                        ^ nl  () ^
               "while the update fields have type" ^ nli () ^
                code ppr_t)

    let list_lit ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2 pos "All elements of a list literal must have the same type" l r

    let table_name ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Table names" t l

    let table_db ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Databases" t l

    let table_keys ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Database keys" t l

    let delete_table ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Tables" t l

    let delete_where ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Where clauses" t l

    let delete_pattern ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The binder must match the table in a delete generator, " ^
               "but the pattern"             ^ nli () ^
                code lexpr                   ^ nl  () ^
               "has type"                    ^ nli () ^
                code ppr_lt                  ^ nl  () ^
               "while the read row has type" ^ nli () ^
                code ppr_rt)

    let delete_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Database deletes are wild"             ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let insert_table ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Tables" t l

    let insert_values ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The values must match the table in an insert expression, " ^
               "but the values"               ^ nli () ^
                code lexpr                    ^ nl  () ^
               "have type"                    ^ nli () ^
                code ppr_lt                   ^ nl  () ^
               "while the write row has type" ^ nli () ^
                code ppr_rt)

    let insert_read ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The fields must match the table in an insert expression, " ^
               "but the fields have type"    ^ nli () ^
                code ppr_lt                  ^ nl  () ^
               "while the read row has type" ^ nli () ^
                code ppr_rt)

    let insert_write ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The fields must match the table in an insert expression, " ^
               "but the fields have type"     ^ nli () ^
                code ppr_lt                   ^ nl  () ^
               "while the write row has type" ^ nli () ^
                code ppr_rt)

    let insert_needed ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The fields must match the table in an insert expression, " ^
               "but the fields have type"      ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "while the needed row has type" ^ nli () ^
                code ppr_rt)

    let insert_id ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Identity variables" t l

    let insert_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Database inserts are wild"             ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let update_table ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Tables" t l

    let update_pattern ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2things pos
        "The binding must match the table in an update expression"
        ("pattern", l) ("row", r)

    let update_read ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The fields must match the table in an update expression, " ^
               "but the fields have type"    ^ nli () ^
                code ppr_lt                  ^ nl  () ^
               "while the read row has type" ^ nli () ^
                code ppr_rt)

    let update_write ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = code (show_type lt) in
      let ppr_rt = code (show_type rt) in
      die pos ("The fields must match the table in an update expression, " ^
               "but the fields have type"     ^ nli () ^
                code ppr_lt                   ^ nl  () ^
               "while the write row has type" ^ nli () ^
                ppr_rt)

    let update_needed ~pos ~t1:(_, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The fields must match the table in an update expression, " ^
               "but the fields have type"      ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "while the needed row has type" ^ nli () ^
                code ppr_rt)

    let update_where ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Where clauses" t l

    let update_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Database updates are wild"             ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let range_bound ~pos ~t1:_l ~t2:(_, _t) ~error:_ =
      die pos "Range bounds must be integers."

    let spawn_location ~pos ~t1:l ~t2:(_, t) ~error:_ =
      fixed_type pos "Spawn locations" t l

    let spawn_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Spawn blocks are wild"                 ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let spawn_wait_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Spawn wait blocks are wild"            ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let query_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("The query block has effects"           ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let query_base_row ~pos ~t1:(lexpr, lt) ~t2:_ ~error:_ =
      build_tyvar_names [lt];
      with_but pos ("Query blocks must have LOROB type") (lexpr, lt)

    let receive_mailbox ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("The current mailbox must always have a mailbox type" ^ nli () ^
                code ppr_rt                                          ^ nl  () ^
               "but the currently allowed effects are"               ^ nli () ^
                code ppr_lt)

    let receive_patterns ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The current mailbox type should match the type of the " ^
               "patterns in a receive, but the current mailbox takes "  ^
               "messages of type"             ^ nli () ^
                code ppr_lt                   ^ nl  () ^
               "while the patterns have type" ^ nli () ^
                code ppr_rt)

    let unary_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      let arg_hd = List.hd (TypeUtils.arg_types rt) in
      let eff    = TypeUtils.effect_row rt in
      build_tyvar_names [lt; arg_hd];
      add_rowvar_names  [eff];
      let ppr_type   = show_type lt in
      let ppr_arg_hd = show_type arg_hd in
      let ppr_eff    = show_row eff in
      die pos ("The unary operator"                       ^ nli () ^
                code lexpr                                ^ nl  () ^
               "has type"                                 ^ nli () ^
                code ppr_type                             ^ nl  () ^
               "while the argument passed to it has type" ^ nli () ^
                code ppr_arg_hd                           ^ nl  () ^
               "and the currently allowed effects are"    ^ nli () ^
                code ppr_eff)

    let infix_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      let arg_hd = List.hd (TypeUtils.arg_types rt) in
      let arg_tl = List.hd (List.tl (TypeUtils.arg_types rt)) in
      let eff    = TypeUtils.effect_row rt in
      build_tyvar_names [lt; arg_hd; arg_tl];
      add_rowvar_names  [eff];
      let ppr_type   = show_type lt in
      let ppr_arg_hd = show_type arg_hd in
      let ppr_arg_tl = show_type arg_tl in
      let ppr_eff    = show_row eff in
      die pos ("The infix operator"                          ^ nli () ^
                code lexpr                                   ^ nl  () ^
               "has type"                                    ^ nli () ^
                code ppr_type                                ^ nl  () ^
               "while the arguments passed to it have types" ^ nli () ^
                code ppr_arg_hd                              ^ nl  () ^
               "and"                                         ^ nli () ^
                code ppr_arg_tl                              ^ nl  () ^
               "and the currently allowed effects are"       ^ nli () ^
                code ppr_eff)

    let fun_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      let tys = TypeUtils.arg_types rt in
      let eff = TypeUtils.effect_row rt in
      build_tyvar_names (lt :: tys);
      add_rowvar_names  [eff];
      let ppr_type  = show_type lt in
      let ppr_types = List.map (fun t -> tab() ^ code (show_type t)) tys in
      let ppr_eff   = show_row eff in
      die pos ("The function"                                 ^ nli () ^
                code lexpr                                    ^ nl  () ^
               "has type"                                     ^ nli () ^
                code ppr_type                                 ^ nl  () ^
               "while the arguments passed to it have types"  ^ nl  () ^
                String.concat (nl() ^ "and" ^ nl()) ppr_types ^ nl  () ^
               "and the currently allowed effects are"        ^ nli () ^
                code ppr_eff)

    let xml_attribute ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "XML attributes" t l

    let xml_attributes ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "A list of XML attributes" t l

    let xml_child ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "XML child nodes" t l

    let formlet_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Formlet bodies" t l

    let page_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Page bodies" t l

    let render_formlet ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Formlets" t l

    let render_handler ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2 pos
        "The formlet must match its handler in a formlet placement" l r

    let render_attributes ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "A list of XML attributes" t l

    let page_placement ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Page antiquotes" t l

    let form_binding_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Formlets" t l

    let form_binding_pattern ~pos ~t1:l ~t2:(rexpr, rt) ~error:_ =
      build_tyvar_names [snd l; rt];
        with_but2things pos
          ("The binding must match the formlet in a formlet binding")
          ("pattern", l) ("expression", (rexpr, rt))

    let iteration_list_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "The body of a list generator" t l

    let iteration_list_pattern ~pos ~t1:l ~t2:(rexpr,rt) ~error:_ =
      build_tyvar_names [snd l; rt];
      let rt = Types.make_list_type rt in
        with_but2things pos
          ("The binding must match the list in a list generator")
          ("pattern", l) ("expression", (rexpr, rt))

    let iteration_table_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "The body of a table generator" t l

    let iteration_table_pattern ~pos ~t1:l ~t2:(rexpr,rt) ~error:_ =
      build_tyvar_names [snd l; rt];
      let rt = Types.make_table_type
                 (rt, Types.fresh_type_variable (`Any, `Any)
                    , Types.fresh_type_variable (`Any, `Any)) in
        with_but2things pos
          ("The binding must match the table in a table generator")
          ("pattern", l) ("expression", (rexpr, rt))

    let iteration_body ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "The body of a for comprehension" t l

    let iteration_where ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Where clauses" t l

    let iteration_base_order ~pos ~t1:(expr,t) ~t2:_ ~error:_ =
      build_tyvar_names [t];
      with_but pos
        ("An orderby clause must return a list of records of base type")
        (expr, t)

    let iteration_base_body ~pos ~t1:(expr,t) ~t2:_ ~error:_ =
      build_tyvar_names [t];
      with_but pos
        ("A database comprehension must return a list of records of base type")
        (expr, t)

    let escape ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "The argument to escape" t l

    let escape_outer ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Escape is wild"                        ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let projection ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~error:_ =
      build_tyvar_names [lt; t];
      let ppr_lt = show_type lt in
      let ppr_t  = show_type t  in
      die pos ("Only a field that is present in a record can be projected, " ^
               "but the expression"            ^ nli () ^
                code lexpr                     ^ nl  () ^
               "has type"                      ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "while the projection has type" ^ nli () ^
                code ppr_t)

    let upcast_source ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~error:_ =
      build_tyvar_names [lt; t];
      let ppr_lt = show_type lt in
      let ppr_t  = show_type t  in
      die pos ("The source expression must match the source type of " ^
               "an upcast, but the expression" ^ nli () ^
                code lexpr                     ^ nl  () ^
               "has type"                      ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "while the source type is"      ^ nli () ^
                code ppr_t)

    let upcast_subtype pos t1 t2 =
      build_tyvar_names [t1; t2];
      let ppr_t1 = show_type t1 in
      let ppr_t2 = show_type t2 in
      die pos ("An upcast must be of the form" ^ code ("e : t2 <- t1") ^
               "where " ^ code "t1" ^ " is a subtype of" ^ code "t2" ^ nl  () ^
               "but"                                                 ^ nli () ^
                code ppr_t1                                          ^ nl  () ^
               "is not a subtype of"                                 ^ nli () ^
                code ppr_t2)

    let value_restriction pos t =
      build_tyvar_names [t];
      let ppr_t  = show_type t in
      die pos ("Because of the value restriction there can be no"              ^ nl () ^
               "free rigid type variables at an ungeneralisable binding site," ^ nl () ^
               "but the type " ^ code ppr_t ^ " has free rigid type variables.")

    let toplevel_purity_restriction pos _b =
      die pos ("Side effects are not allowed outside of" ^ nl() ^
               "function definitions. This binding may have a side effect.")

    let duplicate_names_in_pattern pos =
      die pos ("Duplicate names are not allowed in patterns.")

    let type_annotation ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The inferred type of the expression" ^ nli () ^
                code lexpr                           ^ nl  () ^
               "is"                                  ^ nli () ^
                code ppr_lt                          ^ nl  () ^
               "but it is annotated with type"       ^ nli () ^
                code ppr_rt)

    let bind_val ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2things pos
        ("The binder must match the type of the body in a value binding")
        ("pattern", l) ("expression", r)

    let bind_val_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The value has type"            ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "but it is annotated with type" ^ nli () ^
                code ppr_rt)

    let bind_fun_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The non-recursive function definition has type" ^ nli () ^
                code ppr_lt                                     ^ nl  () ^
               "but it is annotated with type"                  ^ nli () ^
                code ppr_rt)

    let bind_fun_return ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The non-recursive function definition has return type"^ nli () ^
                code ppr_lt                                           ^ nl  () ^
               "but its annotation has return type"                   ^ nli () ^
                code ppr_rt)

    let bind_rec_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The recursive function definition has type" ^ nli () ^
                code ppr_lt                                 ^ nl  () ^
               "but it is annotated with type"              ^ nli () ^
                code ppr_rt)

    let bind_rec_rec ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The recursive function definition has type" ^ nli () ^
                code ppr_lt                                 ^ nl  () ^
               "but its previously inferred type is"        ^ nli () ^
                code ppr_rt)

    let bind_exp ~pos ~t1:l ~t2:(_,t) ~error:_ =
      build_tyvar_names [snd l; t];
      fixed_type pos "Side-effect expressions" t l

    (* patterns *)
    let list_pattern ~pos ~t1:(lexpr,lt) ~t2:(rexpr,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("All elements in a list pattern must have the same type, " ^
               "but the pattern"   ^ nli () ^
                code lexpr         ^ nl  () ^
               "has type"          ^ nli () ^
                code ppr_lt        ^ nl  () ^
               "while the pattern" ^ nli () ^
                code rexpr         ^ nl  () ^
               "has type"          ^ nli () ^
                code ppr_rt)

    let cons_pattern ~pos ~t1:(lexpr,lt) ~t2:(rexpr,rt) ~error:_ =
      let lt = TypeUtils.element_type lt in
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The two subpatterns of a cons pattern " ^ code "p1::p2 "    ^
               "must have compatible types: if " ^ code "p1" ^ " has type " ^
                code "t'" ^ " then " ^ code "p2" ^ " must have type "       ^
                code "[t]" ^ ". However, the pattern"              ^ nli () ^
                code lexpr                                         ^ nl  () ^
               "has type"                                          ^ nli () ^
                code ppr_lt                                        ^ nl  () ^
               "whereas the pattern"                               ^ nli () ^
                code rexpr                                         ^ nl  () ^
               "has type"                                          ^ nli () ^
                code ppr_rt)

    let record_pattern ~pos:pos ~t1:(_lexpr,_lt) ~t2:(_rexpr,_rt) ~error =
      match error with
        | `PresentAbsentClash (label, _, _) ->
            let (_, _, expr) = SourceCode.resolve_pos pos in
            (* NB: is it certain that this is what's happened? *)
            die pos ("Duplicate labels are not allowed in record patterns. "  ^
                     "However, the pattern"                          ^ nli () ^
                      code expr                                      ^ nl  () ^
                     "contains more than one binding for the label " ^ nli () ^
                      code label)
      | `Msg msg -> raise (Errors.Type_error (pos, msg))

    let pattern_annotation ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The inferred type of the pattern" ^ nli () ^
                code lexpr                        ^ nl  () ^
               "is"                               ^ nli () ^
                code ppr_lt                       ^ nl  () ^
               "but it is annotated with type"    ^ nli () ^
                code ppr_rt)

    let splice_exp ~pos:pos ~t1:(_,lt) ~t2:_ ~error:_ =
      build_tyvar_names [lt];
      let ppr_lt = show_type lt in
      die pos ("An expression enclosed in {} in a regex pattern must have " ^
               "type String, but the expression here has type " ^ code ppr_lt)


(* session stuff *)
(*     let link_session ~pos ~t1:(lexpr, lt) ~t2:_ ~error:_ = *)
(*       die pos ("\ *)
(* Only session types can be linkd, but \ *)
(* the expression" ^ nl() ^ *)
(* tab() ^ code lexpr ^ nl() ^ *)
(* "has type" ^ nl() ^ *)
(* tab() ^ code (show_type lt) ^ nl() ^ *)
(* "which is not a session type") *)

(*     let link_dual ~pos ~t1:(lexpr, lt) ~t2:(rexpr, rt) ~error:_ = *)
(*       die pos ("\ *)
(* Only dual session types can be linkd, but \ *)
(* the dual of the type of expression" ^ nl() ^ *)
(* tab() ^ code lexpr ^ nl() ^ *)
(* "is" ^ nl() ^ *)
(* tab() ^ code (show_type lt) ^ nl() ^ *)
(* "and the expression" ^ nl() ^ *)
(* tab() ^ code rexpr ^ nl() ^ *)
(* "has type" ^ nl() ^ *)
(* tab() ^ code (show_type rt)) *)

    let selection ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~error:_ =
      build_tyvar_names [lt; t];
      let ppr_lt = show_type lt in
      let ppr_t  = show_type  t in
      die pos ("Only a label that is present in a session selection can be " ^
               "selected, but the expression"                       ^ nli () ^
                code lexpr                                          ^ nl  () ^
               "has type"                                           ^ nli () ^
                code ppr_lt                                         ^ nl  () ^
               "while the selection has type"                       ^ nli () ^
                code ppr_t)

    let offer_variant ~pos ~t1:(_,lt) ~t2:(_,_) ~error:_ =
      build_tyvar_names [lt];
      let ppr_lt = show_type lt in
      die pos ("The cases of an offer should have choice type, " ^
               "but the type "                          ^ nli () ^
                code ppr_lt                             ^ nl  () ^
               "is not a choice type")

    let offer_patterns ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The cases of an offer should match the type of its patterns, " ^
               "but the pattern"                                      ^ nli () ^
                code lexpr                                            ^ nl  () ^
               "has type"                                             ^ nli () ^
                code ppr_lt                                           ^ nl  () ^
               "while the subsequent patterns have type"              ^ nli () ^
                code ppr_rt)

    let cp_unquote ~pos ~t1:(_, lt) ~t2:(_, _) ~error:_ =
      build_tyvar_names [lt];
      let ppr_lt = show_type lt in
      die pos ("Spliced expression should have EndBang type, " ^
               "but has type"                         ^ nli () ^
                code ppr_lt                           ^ nl  () ^
               "instead.")

    let cp_grab channel ~pos ~t1:(_, actual) ~t2:(_, expected) ~error:_ =
      build_tyvar_names [actual; expected];
      let ppr_actual   = show_type actual   in
      let ppr_expected = show_type expected in
      die pos ("Channel " ^ channel ^ " " ^
               "was expected to have input type" ^ nli () ^
                code ppr_expected                ^ nl  () ^
               "but has type"                    ^ nli () ^
                code ppr_actual                  ^ nl  () ^
               "instead.")

    let cp_give channel ~pos ~t1:(_, actual) ~t2:(_, expected) ~error:_ =
      build_tyvar_names [actual; expected];
      let ppr_actual   = show_type actual   in
      let ppr_expected = show_type expected in
      die pos ("Channel " ^ channel ^ " " ^
               "was expected to have output type" ^ nli () ^
                code ppr_expected                 ^ nl  () ^
               "but has type"                     ^ nli () ^
                code ppr_actual                   ^ nl  () ^
               "instead.")

    let cp_select channel ~pos ~t1:(_, actual) ~t2:(_, expected) ~error:_ =
      build_tyvar_names [actual; expected];
      let ppr_actual   = show_type actual   in
      let ppr_expected = show_type expected in
      die pos ("Channel " ^ channel ^ " " ^
               "was expected to have selection type" ^ nli () ^
                code ppr_expected                    ^ nl  () ^
               "but has type"                        ^ nli () ^
                code ppr_actual                      ^ nl  () ^
               "instead.")

    let cp_offer_choice channel ~pos ~t1:(_, actual) ~t2:(_, expected) ~error:_ =
      build_tyvar_names [actual; expected];
      let ppr_actual   = show_type actual   in
      let ppr_expected = show_type expected in
      die pos ("Channel " ^ channel ^ " " ^
               "was expected to have choice type" ^ nli () ^
                code ppr_expected                 ^ nl  () ^
               "but has type"                     ^ nli () ^
                code ppr_actual                   ^ nl  () ^
               "instead.")

    let cp_offer_branches ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("The branches of an offer expression have " ^
               "divergent types:"                 ^ nli () ^
                code ppr_lt                       ^ nl  () ^
               "and"                              ^ nli () ^
                code ppr_rt)

    let cp_link_session ~pos ~t1:(_, lt) ~t2:_ ~error:_ =
      build_tyvar_names [lt];
      let ppr_lt = show_type lt in
      die pos ("Only session types can be linked, " ^
               "but the type"             ^ nli () ^
                code ppr_lt               ^ nl  () ^
               "is not a session type")

    let cp_link_dual ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt;rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("Only dual session types can be linked, " ^
               "but the type"                  ^ nli () ^
                code ppr_lt                    ^ nl  () ^
               "is not the dual of the type"   ^ nli () ^
                code ppr_rt)

    let cp_comp_left ~pos ~t1:_ ~t2:(_, rt) ~error:_ =
      build_tyvar_names [rt];
      let ppr_rt = show_type rt in
      die pos ("The left-hand computation in a composition must have " ^
               "EndBang type, but has type"                  ^ nli () ^
                code ppr_rt                                  ^ nl  () ^
               "instead.")

    let non_linearity pos uses v t =
      build_tyvar_names [t];
      let ppr_t = show_type t in
      die pos ("Variable " ^ v ^ " has linear type " ^ nli () ^
                code ppr_t                           ^ nl  () ^
               "but is used " ^ string_of_int uses ^ " times.")

    (* Affine session exception handling *)
    let try_in_unless_pat ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2things pos
        ("The 'try' clause should match the pattern defined in the 'as' clause")
        ("pattern", l) ("expression", r)

    let try_in_unless_branches ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2 pos
        ("Both branches of a try-as-in-unless block should have the same type")
        l r

    let try_in_unless_linearity pos v =
      die pos ("All variables in the as- and unless- branches of an " ^
               "exception handler must be unrestricted, but " ^ nl () ^
               "variable " ^ v ^ " is linear.")
end

type context = Types.typing_environment = {
  (* mapping from variables to type schemes *)
  var_env   : Types.environment ;

  (* mapping from type alias names to the types they name.  We don't
     use this to resolve aliases in the code, which is done before
     type inference.  Instead, we use it to resolve references
     introduced here to aliases defined in the prelude such as "Page"
     and "Formlet". *)
  tycon_env : Types.tycon_environment ;

  (* the current effects *)
  effect_row : Types.row
}

let empty_context eff =
  { var_env   = Env.empty;
    tycon_env = Env.empty;
    effect_row = eff }

let bind_var context (v, t) = {context with var_env = Env.bind context.var_env (v,t)}
let unbind_var context v = {context with var_env = Env.unbind context.var_env v}
let bind_tycon context (v, t) = {context with tycon_env = Env.bind context.tycon_env (v,t)}
let bind_effects context r = {context with effect_row = r}

let type_section context (`Section s as s') =
  let env = context.var_env in
  let ((tyargs, t), usages) =
    match s with
      | `Minus         -> Utils.instantiate env "-", StringMap.empty
      | `FloatMinus    -> Utils.instantiate env "-.", StringMap.empty
      | `Project label ->
          let a = Types.fresh_type_variable (`Any, `Any) in
          let rho = Types.fresh_row_variable (`Any, `Any) in
          let effects = Types.make_empty_open_row (`Any, `Any) in (* projection is pure! *)
          let r = `Record (StringMap.add label (`Present a) StringMap.empty, rho, false) in
            ([`Type a; `Row (StringMap.empty, rho, false); `Row effects], `Function (Types.make_tuple_type [r], effects, a)), StringMap.empty
      | `Name var      -> Utils.instantiate env var, StringMap.singleton var 1
  in
    if Settings.get_value Instantiate.quantified_instantiation then
      let tyvars = Types.quantifiers_of_type_args tyargs in
        tabstr(tyvars, tappl (s', tyargs)), t, usages
    else
      tappl (s', tyargs), t, usages

let datatype aliases = Instantiate.typ -<- DesugarDatatypes.read ~aliases
let add_usages (p, t) m = (p, t, m)
let add_empty_usages (p, t) = (p, t, StringMap.empty)

let type_unary_op env =
  let datatype = datatype env.tycon_env in
  function
  | `Minus      -> add_empty_usages (datatype "(Int) -> Int")
  | `FloatMinus -> add_empty_usages (datatype "(Float) -> Float")
  | `Name n     -> add_usages (Utils.instantiate env.var_env n) (StringMap.singleton n 1)

let type_binary_op ctxt =
  let datatype = datatype ctxt.tycon_env in function
  | `Minus        -> add_empty_usages (Utils.instantiate ctxt.var_env "-")
  | `FloatMinus   -> add_empty_usages (Utils.instantiate ctxt.var_env "-.")
  | `RegexMatch flags ->
      let nativep  = List.exists ((=) `RegexNative)  flags
      and listp    = List.exists ((=) `RegexList)    flags
      and replacep = List.exists ((=) `RegexReplace) flags in
        begin
          match replacep, listp, nativep with
           | true,   _   , false -> (* stilde  *) add_empty_usages (datatype "(String, Regex) -> String")
           | false, true , false -> (* ltilde *)  add_empty_usages (datatype "(String, Regex) -> [String]")
           | false, false, false -> (* tilde *)   add_empty_usages (datatype "(String, Regex) -> Bool")
           | _    , _    , true  -> assert false
        end
  | `And
  | `Or           -> add_empty_usages (datatype "(Bool,Bool) -> Bool")
  | `Cons         -> add_empty_usages (Utils.instantiate ctxt.var_env "Cons")
  | `Name "++"    -> add_empty_usages (Utils.instantiate ctxt.var_env "Concat")
  | `Name ">"
  | `Name ">="
  | `Name "=="
  | `Name "<"
  | `Name "<="
  | `Name "<>"    ->
      let a = Types.fresh_type_variable (`Any, `Any) in
      let eff = (StringMap.empty, Types.fresh_row_variable (`Any, `Any), false) in
        ([`Type a; `Row eff],
         `Function (Types.make_tuple_type [a; a], eff, `Primitive `Bool),
         StringMap.empty)
  | `Name "!"     -> add_empty_usages (Utils.instantiate ctxt.var_env "Send")
  | `Name n       -> add_usages (Utils.instantiate ctxt.var_env n) (StringMap.singleton n 1)

(** close a pattern type relative to a list of patterns

   If there are no _ or variable patterns at a variant type, then that
   variant will be closed.
*)
let close_pattern_type : pattern list -> Types.datatype -> Types.datatype = fun pats t ->
  (* We use a table to keep track of encountered recursive variables
     in order to avert non-termination. *)
  let rec_vars_seen = Hashtbl.create 8 in
  let rec cpt : pattern list -> Types.datatype -> Types.datatype = fun pats t ->
    match t with
      | `Alias (alias, t) -> `Alias (alias, cpt pats t)
      | `Record row when Types.is_tuple row->
          let fields, row_var, dual = fst (Types.unwrap_row row) in
          let rec unwrap_at i p =
            match p.node with
              | `Variable _ | `Any | `Constant _ -> p
              | `As (_, p) | `HasType (p, _) -> unwrap_at i p
              | `Tuple ps ->
                  List.nth ps i
              | `Nil | `Cons _ | `List _ | `Record _ | `Variant _ | `Negative _ | `Effect _ -> assert false in
          let fields =
            StringMap.fold(* true if the row variable is dualised *)

              (fun name ->
                 function
                 | `Present t ->
                       let pats = List.map (unwrap_at ((int_of_string name) - 1)) pats in
                         StringMap.add name (`Present (cpt pats t))
                   | `Absent
                   | `Var _ ->
                       assert false) fields StringMap.empty in
            `Record (fields, row_var, dual)
      | `Record row ->
          let fields, row_var, lr = fst (Types.unwrap_row row) in
          assert (not lr);
          let rec unwrap_at name p =
            match p.node with
              | `Variable _ | `Any | `Constant _ -> p
              | `As (_, p) | `HasType (p, _) -> unwrap_at name p
              | `Record (ps, default) ->
                  if List.mem_assoc name ps then
                    List.assoc name ps
                  else
                    begin
                      match default with
                        | None -> assert false
                        | Some p -> unwrap_at name p
                    end
              | `Nil | `Cons _ | `List _ | `Tuple _ | `Variant _ | `Negative _ | `Effect _ -> assert false in
          let fields =
            StringMap.fold
              (fun name ->
                 function
                   | `Present t ->
                       let pats = List.map (unwrap_at name) pats in
                         StringMap.add name (`Present (cpt pats t))
                   | `Absent
                   | `Var _ ->
                        assert false) fields StringMap.empty in
            `Record (fields, row_var, false)
      | `Variant row ->
          let fields, row_var, lr = fst (Types.unwrap_row row) in
          assert (not lr);
          let end_pos p =
            let (_, end_pos, buf) = p.pos in
              (*
                QUESTION:

                This indicates the position immediately after the pattern.
                How can we indicate a 0-length position in an error message?
              *)
              (end_pos, end_pos, buf) in

          let rec unwrap_at : string -> pattern -> pattern list = fun name p ->
            match p.node with
              | `Variable _ | `Any -> [ with_pos (end_pos p) `Any ]
              | `As (_, p) | `HasType (p, _) -> unwrap_at name p
              | `Variant (name', None) when name=name' ->
                    [with_pos (end_pos p) (`Record ([], None))]
              | `Variant (name', Some p) when name=name' -> [p]
              | `Variant _ -> []
              | `Negative names when List.mem name names -> []
              | `Negative _ -> [ with_pos (end_pos p) `Any ]
              | `Nil | `Cons _ | `List _ | `Tuple _ | `Record _ | `Constant _ | `Effect _ -> assert false in
          let rec are_open : pattern list -> bool =
            function
              | [] -> false
              | {node = (`Variable _ | `Any | `Negative _); _} :: _ -> true
              | {node = (`As (_, p) | `HasType (p, _)); _} :: ps -> are_open (p :: ps)
              | {node = (`Variant _); _} :: ps -> are_open ps
              | {node = (`Nil | `Cons _ | `List _ | `Tuple _ | `Record _ | `Constant _ | `Effect _); _} :: _ -> assert false in
          let fields =
            StringMap.fold
              (fun name field_spec env ->
                 match field_spec with
                   | `Present t ->
                       let pats = concat_map (unwrap_at name) pats in
                       let t = cpt pats t in
                         (StringMap.add name (`Present t)) env
                   | `Absent
                   | `Var _ ->
                       assert false) fields StringMap.empty
          in
            if are_open pats then
              begin
                let row = (fields, row_var, false) in
                (* NOTE: type annotations can lead to a closed type even though
                   the patterns are open *)
                `Variant row
              end
            else
              begin
                match Unionfind.find row_var with
                  | `Var _ -> `Variant (fields, Unionfind.fresh `Closed, false)
                  | `Recursive _ | `Body _ | `Closed -> assert false
              end

      | `Effect row ->
         (* We don't really close effect rows, rather, we close the
            subpatterns contained within the effect row. *)
          let fields, row_var, lr = fst (Types.unwrap_row row) in
          assert (not lr);

          let unwrap_at : string -> pattern -> pattern list = fun name p ->
            match p.node with
              | `Effect (name', ps, _) when name=name' -> ps
              | `Effect _ -> []
              | `Variable _ | `Any | `As _ | `HasType _ | `Negative _
              | `Nil | `Cons _ | `List _ | `Tuple _ | `Record _ | `Variant _ | `Constant _ -> assert false in
          let fields =
            StringMap.fold
              (fun name field_spec env ->
                 match field_spec with
                 | `Present t ->
                    begin match TypeUtils.concrete_type t with
                    | `Function (_, effs, codomain) ->
                       (* Idea: For each operation `name' extract its
                          patterns `ps' from `Effect(name, ps, _)' and
                          arrange each such ps as a row in and n x p
                          matrix, where n is the number of cases for
                          `name' and p is |ps|. Afterwards, point-wise
                          close the patterns by recursively calling
                          close_pattern_type on each column. *)
                       let t =
                       (* Construct an p x n matrix (i.e. the
                          transposition of p x n matrix as it is easier
                          to map column-wise) *)
                         let pmat : pattern list list =
                           let non_empty ps = ps <> [] in
                           let rows =
                             map_filter
                               (unwrap_at name)
                               non_empty
                               pats
                           in
                           transpose rows
                         in
                         (* Annotate each pattern with its inferred type *)
                         let annot_pmat =
                           try
                             let annotate ps t = (ps, t) in
                             let types = TypeUtils.arg_types t in
                             List.map2 annotate pmat types
                           with
                             Invalid_argument _ -> failwith "Inconsistent pattern type"
                         in
                         (* Recursively close each subpattern. This
                            yields the domain type for the operation. *)
                         let domain : Types.datatype list =
                           List.map
                             (fun (ps, t) -> cpt ps t)
                             annot_pmat
                         in
                       (* Reconstruct the type for the whole pattern *)
                         Types.make_function_type domain effs codomain
                       in
                       (* Bind name |-> Pre(t) *)
                       StringMap.add name (`Present t) env
                    | _ ->
                       StringMap.add name (`Present t) env
                    end
                 | t -> StringMap.add name t env) fields StringMap.empty
          in
          let row = (fields, row_var, false) in
          (* NOTE: type annotations can lead to a closed type even though
                   the patterns are open *)
          `Effect row
      | `Application (l, [`Type t])
          when Types.Abstype.equal l Types.list ->
          let rec unwrap p : pattern list =
            match p.node with
              | `Variable _ | `Any -> [p]
              | `Constant _ | `Nil -> []
              | `Cons (p1, p2) -> p1 :: unwrap p2
              | `List ps -> ps
              | `As (_, p) | `HasType (p, _) -> unwrap p
              | `Variant _ | `Negative _ | `Record _ | `Tuple _ | `Effect _ -> assert false in
          let pats = concat_map unwrap pats in
            `Application (Types.list, [`Type (cpt pats t)])
      | `ForAll (qs, t) -> `ForAll (qs, cpt pats t)
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t -> cpt pats t
              | `Var _ -> t
              | `Recursive (i, t') when not (Hashtbl.mem rec_vars_seen i) ->
                 Hashtbl.add rec_vars_seen i ();
                 cpt pats t'
              | `Recursive _ -> t
          end
      | `Not_typed
      | `Primitive _
      | `Function _
      | `Lolli _
      | `Table _
      | `Lens _
      (* TODO: do we need to do something special for session types? *)
      | #Types.session_type
       (* TODO: expand applications? *)
      | `Application _ -> t
  in
  cpt pats t

type unify_result = UnifySuccess | UnifyFailure of (Unify.error * SourceCode.pos)

let raise_unify ~(handle:Gripers.griper) ~pos error t1 t2 =
  begin
    match error with
    | `Msg s -> Debug.print ("Unification error: "^s)
    | _ -> ()
  end;
  handle ~pos ~t1 ~t2 ~error

let unify ~pos unifyTys  =
  try Utils.unify unifyTys; UnifySuccess
  with Unify.Failure error -> UnifyFailure (error, pos)

let unify_or_raise ~(handle:Gripers.griper) ~pos (_, ltype1 as lt1, (_, rtype1 as rt1)) =
  begin
  match unify ~pos (ltype1, rtype1) with
  | UnifySuccess -> ()
  | UnifyFailure (err, pos) -> raise_unify ~handle ~pos err lt1 rt1
  end

(* Expects at least one pair of arguments to succesfully unify *)
let unify_or ~(handle:Gripers.griper) ~pos ((_, ltype1), (_, rtype1))
                                          ((_, ltype2) as lt2, ((_, rtype2) as rt2)) =
  begin
  match unify ~pos (ltype1, rtype1) with
  | UnifySuccess -> ()
  | UnifyFailure _ ->
     begin
       match unify ~pos (ltype2, rtype2) with
       | UnifySuccess -> ()
       | UnifyFailure (err, pos) -> raise_unify ~handle ~pos err lt2 rt2
     end
  end


(** check for duplicate names in a list of pattern *)
let check_for_duplicate_names : Sugartypes.position -> pattern list -> string list = fun pos ps ->
  let add name binder binderss =
    if StringMap.mem name binderss then
      let (count, binders) = StringMap.find name binderss in
        StringMap.add name (count+1, binder::binders) binderss
    else
      StringMap.add name (1, [binder]) binderss in

  let rec gather binderss { node = (p : patternnode); _} =
    match p with
      | `Any -> binderss
      | `Nil -> binderss
      | `Cons (p, q) ->
          let binderss = gather binderss p in gather binderss q
      | `List ps ->
          List.fold_right (fun p binderss -> gather binderss p) ps binderss
      | `Variant (_, p) ->
         opt_app (fun p -> gather binderss p) binderss p
      | `Effect (_, ps, k) ->
         let binderss' =
           List.fold_right (fun p binderss -> gather binderss p) ps binderss
         in
         gather binderss' k
      | `Negative _ -> binderss
      | `Record (ps, p) ->
          let binderss = List.fold_right (fun (_, p) binderss -> gather binderss p) ps binderss in
            opt_app (fun p -> gather binderss p) binderss p
      | `Tuple ps ->
          List.fold_right (fun p binderss -> gather binderss p) ps binderss
      | `Constant _ -> binderss
      | `Variable bndr ->
          add (name_of_binder bndr) bndr binderss
      | `As (bndr, p) ->
          let binderss = gather binderss p in
            add (name_of_binder bndr) bndr binderss
      | `HasType (p, _) -> gather binderss p in

  let binderss =
    List.fold_left gather StringMap.empty ps in
  let dups = StringMap.filterv (fun (i, _) -> i > 1) binderss in
    if not (StringMap.is_empty dups) then
      Gripers.duplicate_names_in_pattern pos
    else
      List.map fst (StringMap.bindings binderss)

let type_pattern closed : pattern -> pattern * Types.environment * Types.datatype =
  let make_singleton_row =
    match closed with
      | `Closed -> Types.make_singleton_closed_row
      | `Open -> (fun var -> Types.make_singleton_open_row var (`Any, `Any)) in

  (* type_pattern p types the pattern p returning a typed pattern, a
     type environment for the variables bound by the pattern and two
     types. The first type is the type of the pattern 'viewed from the
     outside' - in the case of variant patterns it must be open in
     order to allow cases to unify. The second type is the type of the
     pattern 'viewed from the inside'. Type annotations are only
     applied to the inner pattern. The type environment is constructed
     using types from the inner type.

  *)
  let rec type_pattern {node = pattern; pos = pos'} : pattern * Types.environment * (Types.datatype * Types.datatype) =
    let _UNKNOWN_POS_ = "<unknown>" in
    let tp = type_pattern in
    let unify (l, r) = unify_or_raise ~pos:pos' (l, r)
    and erase (p,_, _) = p
    and ot (_,_,(t,_)) = t
    and it (_,_,(_,t)) = t
    and env (_,e,_) = e
    and pos ({pos = p;_},_,_) = let (_,_,p) = SourceCode.resolve_pos p in p
    and (++) = Env.extend in
    let (p, env, (outer_type, inner_type)) :
      patternnode * Types.environment * (Types.datatype * Types.datatype) =
      match pattern with
      | `Any ->
        let t = Types.fresh_type_variable (`Unl, `Any) in
        `Any, Env.empty, (t, t)
      | `Nil ->
        let t = Types.make_list_type (Types.fresh_type_variable (`Any, `Any)) in
        `Nil, Env.empty, (t, t)
      | `Constant c as c' ->
        let t = Constant.constant_type c in
        c', Env.empty, (t, t)
      | `Variable bndr ->
        let xtype = Types.fresh_type_variable (`Any, `Any) in
        (`Variable (set_binder_type bndr xtype),
         Env.bind Env.empty (name_of_binder bndr, xtype),
         (xtype, xtype))
      | `Cons (p1, p2) ->
        let p1 = tp p1
        and p2 = tp p2 in
        let () = unify ~handle:Gripers.cons_pattern ((pos p1, Types.make_list_type (ot p1)),
                                                     (pos p2, ot p2)) in
        let () = unify ~handle:Gripers.cons_pattern ((pos p1, Types.make_list_type (it p1)),
                                                     (pos p2, it p2)) in
        `Cons (erase p1, erase p2), env p1 ++ env p2, (ot p2, it p2)
      | `List ps ->
        let ps' = List.map tp ps in
        let env' = List.fold_right (env ->- (++)) ps' Env.empty in
        let list_type p ps typ =
          let () = List.iter (fun p' -> unify ~handle:Gripers.list_pattern ((pos p, typ p),
                                                                            (pos p', typ p'))) ps in
          Types.make_list_type (typ p) in
        let ts =
          match ps' with
          | [] -> let t = Types.fresh_type_variable (`Any, `Any) in t, t
          | p::ps ->
            list_type p ps ot, list_type p ps it
        in
        `List (List.map erase ps'), env', ts
      | `Variant (name, None) ->
        let vtype () = `Variant (make_singleton_row (name, `Present Types.unit_type)) in
        `Variant (name, None), Env.empty, (vtype (), vtype ())
      | `Variant (name, Some p) ->
        let p = tp p in
        let vtype typ = `Variant (make_singleton_row (name, `Present (typ p))) in
        `Variant (name, Some (erase p)), env p, (vtype ot, vtype it)
      | `Effect (name, ps, k) ->
         (* Auxiliary machinery for typing effect patterns *)
         let rec type_resumption_pat (kpat : pattern) : pattern * Types.environment * (Types.datatype * Types.datatype) =
           let fresh_resumption_type () =
             let domain = Types.fresh_type_variable (`Unl, `Any) in
             let codomain = Types.fresh_type_variable (`Unl, `Any) in
             let effrow = Types.make_empty_open_row (`Unl, `Any) in
             Types.make_function_type [domain] effrow codomain
           in
           let pos' = kpat.pos in
           match kpat.node with
           | `Any ->
              let t = fresh_resumption_type () in
              with_pos pos' `Any, Env.empty, (t, t)
           | `Variable bndr ->
              let xtype = fresh_resumption_type () in
              ( with_pos pos' (`Variable (set_binder_type bndr xtype))
              , Env.bind Env.empty (name_of_binder bndr, xtype), (xtype, xtype))
           | `As (bndr, pat') ->
              let p = type_resumption_pat pat' in
              let env' = Env.bind (env p) (name_of_binder bndr, it p) in
              with_pos pos' (`As ((set_binder_type bndr (it p), erase p))), env', (ot p, it p)
           | `HasType (p, (_, Some t)) ->
              let p = type_resumption_pat p in
              let () = unify ~handle:Gripers.type_resumption_with_annotation ((pos p, it p), (_UNKNOWN_POS_, t)) in
              erase p, env p, (ot p, t)
           | _ -> Gripers.die pos' "Improper pattern matching on resumption"
         in
         (* Typing of effect patterns *)
         let ps = List.map tp ps in
         let k = type_resumption_pat k in
         let eff typ =
           let domain = List.map typ ps in
           let codomain = TypeUtils.arg_types (typ k) in
           let t =
             (* Construct operation type, i.e. op : A -> B or op : B *)
             match domain, codomain with
             | [], [] | _, [] -> assert false (* The continuation is at least unary *)
             | [], [t] -> `Function (Types.unit_type, Types.make_empty_closed_row (), t)
             | [], ts -> Types.make_tuple_type ts
             | ts, [t] ->
                Types.make_function_type ts (Types.make_empty_closed_row ()) t
             | ts, ts' ->
                (* parameterised continuation *)
                let t = ListUtils.last ts' in
                Types.make_function_type ts (Types.make_empty_closed_row ()) t
           in
           `Effect (make_singleton_row (name, `Present t))
         in
         let env =
           let penv =
             List.fold_right (env ->- (++)) ps Env.empty
           in
           let kenv = env k in
           penv ++ kenv
         in
         `Effect (name, List.map erase ps, erase k), env, (eff ot, eff it)
      | `Negative names ->
        let row_var = Types.fresh_row_variable (`Any, `Any) in

        let positive, negative =
          List.fold_right
            (fun name (positive, negative) ->
               let a = Types.fresh_type_variable (`Any, `Any) in
               (StringMap.add name (`Present a) positive,
                StringMap.add name `Absent negative))
            names (StringMap.empty, StringMap.empty) in

        let outer_type = `Variant (positive, row_var, false) in
        let inner_type = `Variant (negative, row_var, false) in
        `Negative names, Env.empty, (outer_type, inner_type)
      | `Record (ps, default) ->
        let ps = alistmap tp ps in
        let default = opt_map tp default in
        let initial_outer, initial_inner, denv =
          match default with
          | None ->
            let row = Types.make_empty_closed_row () in
            row, row, Env.empty
          | Some r ->
            let make_closed_row typ =
              let row =
                List.fold_right
                  (fun (label, _) ->
                     Types.row_with (label, `Absent))
                  ps (Types.make_empty_open_row (`Any, `Any)) in
              let () = unify ~handle:Gripers.record_pattern (("", `Record row),
                                                             (pos r, typ r))
              in
              row
            in
            make_closed_row ot, make_closed_row it, env r in
        let rtype typ initial =
          `Record (List.fold_right
                     (fun (l, f) -> Types.row_with (l, `Present (typ f)))
                     ps initial) in
        let penv =
          List.fold_right (snd ->- env ->- (++)) ps Env.empty
        in
        (`Record (alistmap erase ps, opt_map erase default),
         penv ++ denv,
         (rtype ot initial_outer, rtype it initial_inner))
      | `Tuple ps ->
        let ps' = List.map tp ps in
        let env' = List.fold_right (env ->- (++)) ps' Env.empty in
        let make_tuple typ = Types.make_tuple_type (List.map typ ps') in
        `Tuple (List.map erase ps'), env', (make_tuple ot, make_tuple it)
      | `As (bndr, p) ->
        let p = tp p in
        let env' = Env.bind (env p) (name_of_binder bndr, it p) in
        `As (set_binder_type bndr (it p), erase p), env', (ot p, it p)
      | `HasType (p, (_,Some t as t')) ->
        let p = tp p in
        let () = unify ~handle:Gripers.pattern_annotation ((pos p, it p), (_UNKNOWN_POS_, t)) in
        `HasType (erase p, t'), env p, (ot p, t)
      | `HasType _ -> assert false in
    with_pos pos' p, env, (outer_type, inner_type)
  in
  fun pattern ->
    let _ = check_for_duplicate_names pattern.pos [pattern] in
    let pos, env, (outer_type, _) = type_pattern pattern in
    pos, env, outer_type

let rec pattern_env : pattern -> Types.datatype Env.t =
  fun { node = p; _} -> match p with
    | `Any
    | `Nil
    | `Constant _ -> Env.empty

    | `HasType (p,_) -> pattern_env p
    | `Variant (_, Some p) -> pattern_env p
    | `Variant (_, None) -> Env.empty
    | `Effect (_, ps, k) ->
       let env = List.fold_right (pattern_env ->- Env.extend) ps Env.empty in
       Env.extend env (pattern_env k)
    | `Negative _ -> Env.empty
    | `Record (ps, Some p) ->
        List.fold_right (snd ->- pattern_env ->- Env.extend) ps (pattern_env p)
    | `Record (ps, None) ->
        List.fold_right (snd ->- pattern_env ->- Env.extend) ps Env.empty
    | `Cons (h,t) -> Env.extend (pattern_env h) (pattern_env t)
    | `List ps
    | `Tuple ps -> List.fold_right (pattern_env ->- Env.extend) ps Env.empty
    | `Variable {node=v, Some t; _} -> Env.bind Env.empty (v, t)
    | `Variable {node=_, None; _} -> assert false
    | `As       ({node=v, Some t; _}, p) -> Env.bind (pattern_env p) (v, t)
    | `As       ({node=_, None; _}, _) -> assert false


let update_pattern_vars env =
(object (self)
  inherit SugarTraversals.map as super

  method! patternnode : patternnode -> patternnode =
    fun n ->
      let update bndr =
        let ty = Env.lookup env (name_of_binder bndr) in
        set_binder_type bndr ty
      in match n with
         | `Variable b -> `Variable (update b)
         | `As (b, p) -> `As (update b, self#pattern p)
         | _ -> super#patternnode n
 end)#pattern

let rec extract_formlet_bindings : phrase -> Types.datatype Env.t = fun p ->
  match p.node with
  | `FormBinding (_, pattern) -> pattern_env pattern
  | `Xml (_, _, _, children) ->
      List.fold_right
        (fun child env ->
           Env.extend env (extract_formlet_bindings child))
        children Env.empty
  | _ -> Env.empty

(* let show_context : context -> context = *)
(*   fun context -> *)
(*     Printf.fprintf stderr "Types  : %s\n" (Env.Dom.show_t (Env.domain context.tycon_env)); *)
(*     Printf.fprintf stderr "Values : %s\n" (Env.Dom.show_t (Env.domain context.var_env)); *)
(*     flush stderr; *)
(*     context *)


(* given a list of argument patterns and a return type
   return the corresponding function type *)
let make_ft declared_linearity ps effects return_type =
  let pattern_typ (_, _, t) = t in
  let args =
    Types.make_tuple_type -<- List.map pattern_typ in
  let ftcon = fun p -> if declared_linearity=`Lin then `Lolli p else `Function p in
  let rec ft =
    function
      | [p] -> ftcon (args p, effects, return_type)
      | p::ps -> ftcon (args p, (StringMap.empty, Types.fresh_row_variable (`Any, `Any), false), ft ps)
      | [] -> assert false
  in
    ft ps

let make_ft_poly_curry declared_linearity ps effects return_type =
  let pattern_typ (_, _, t) = t in
  let args =
    Types.make_tuple_type -<- List.map pattern_typ in
  let ftcon = fun p -> if declared_linearity=`Lin then `Lolli p else `Function p in
  let rec ft =
    function
      | [p] -> [], ftcon (args p, effects, return_type)
      | p::ps ->
          let qs, t = ft ps in
          let q, eff = Types.fresh_row_quantifier (`Any, `Any) in
            q::qs, ftcon (args p, eff, t)
      | [] -> assert false
  in
    Types.for_all (ft ps)

type usagemap = int stringmap
let merge_usages (ms:usagemap list) : usagemap =
      match ms with
      | [] -> StringMap.empty
      | (m::ms) -> List.fold_right
                     (fun m n ->
                        StringMap.merge
                          (fun _ xo yo ->
                             match xo, yo with
                             | Some x, Some y -> Some (x + y)
                             | Some x, None   -> Some x
                             | None, Some y   -> Some y
                             | None, None     -> None
                          ) m n) ms m

let uses_of v us =
  try
    StringMap.find v us
  with
    _ -> 0

let usage_compat =
  function
  | [] ->
    (* HACK: for now we take the conservative choice of assuming that
       no linear variables are used in empty cases. We could keep
       track of all variables in scope so that we can treat them as
       linear if possible. This would require a further map recording
       all variables that have empty pattern matching 'sink'. *)
    StringMap.empty
  | (u::us) ->
    let same m n =
      let mvs = List.map fst (StringMap.bindings m) in
      let vs  = List.append (List.filter (fun v -> not (List.mem v mvs)) (List.map fst (StringMap.bindings n)))
          mvs in
      let f v resulting_usages =
        if StringMap.mem v m && StringMap.mem v n && StringMap.find v m = StringMap.find v n then
          StringMap.add v (StringMap.find v m) resulting_usages
        else
        (* We need to treat anything appearing in this case as unlimited; '2' assures that no
           matter whether the variable in question is used anywhere else or not, it must be
           unlimited. *)
          StringMap.add v 2 resulting_usages in
      List.fold_right f vs StringMap.empty in
    List.fold_right same us u

let usages_cases bs =
  usage_compat (List.map (fun (_, (_, _, m)) -> m) bs)

let rec type_check : context -> phrase -> phrase * Types.datatype * usagemap =
  fun context {node=expr; pos} ->
    let _UNKNOWN_POS_ = "<unknown>" in
    let no_pos t = (_UNKNOWN_POS_, t) in

    let unify (l, r) = unify_or_raise ~pos:pos (l, r)
    and (++) env env' = {env with var_env = Env.extend env.var_env env'} in

    let typ (_,t,_) : Types.datatype = t
    and erase (p, _, _) = p
    and usages (_, _, m) = m
    and update_usages (p, t, _) u = (p, t, u)
    and erase_pat (p, _, _) = p
    and pattern_typ (_, _, t) = t
    and pattern_env (_, e, _) = e in
    let pattern_pos ({pos=p; _},_,_) = let (_,_,p) = SourceCode.resolve_pos p in p in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos p = let (_,_,p) = SourceCode.resolve_pos p.pos in p in
    let exp_pos (p,_,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in
    let tpc p = type_pattern `Closed p
    and tpo p = type_pattern `Open p
    and tc : phrase -> phrase * Types.datatype * usagemap = type_check context
    and expr_string (p : Sugartypes.phrase) : string =
      let (_,_,e) = SourceCode.resolve_pos p.pos in e
    and erase_cases = List.map (fun ((p, _, _t), (e, _, _)) -> p, e) in
    let type_cases binders =
      let pt = Types.fresh_type_variable (`Any, `Any) in
      let bt = Types.fresh_type_variable (`Any, `Any) in
      let binders, pats =
        List.fold_right
          (fun (pat, body) (binders, pats) ->
             let pat = tpo pat in
             let () =
               unify ~handle:Gripers.switch_patterns
                 (ppos_and_typ pat, no_pos pt)
             in
               (pat, body)::binders, pat :: pats)
          binders ([], []) in
      let pt = close_pattern_type (List.map fst3 pats) pt in

      (* NOTE: it is important to type the patterns in isolation first in order
         to allow them to be closed before typing the bodies *)

      let binders =
        List.fold_right
          (fun (pat, body) binders ->
             let body = type_check (context ++ pattern_env pat) body in
             let () = unify ~handle:Gripers.switch_branches
               (pos_and_typ body, no_pos bt) in
             let () = Env.iter (fun v t -> let uses = uses_of v (usages body) in
                                           if uses <> 1 then
                                             if Types.type_can_be_unl t then
                                               Types.make_type_unl t
                                             else
                                               Gripers.non_linearity pos uses v t)
                               (pattern_env pat) in
             let vs = Env.domain (pattern_env pat) in
             let us = StringMap.filter (fun v _ -> not (StringSet.mem v vs)) (usages body) in
             (pat, update_usages body us)::binders)
          binders []
      in
        binders, pt, bt in

    let e, t, usages =
      match (expr : phrasenode) with
        | `Var v            ->
            (
              try
                let (tyargs, t) = Utils.instantiate context.var_env v in
                  if Settings.get_value Instantiate.quantified_instantiation then
                    let tyvars = Types.quantifiers_of_type_args tyargs in
                      tabstr(tyvars, tappl (`Var v, tyargs)), t, StringMap.singleton v 1
                  else
                    tappl (`Var v, tyargs), t, StringMap.singleton v 1
              with
                  Errors.UndefinedVariable _msg ->
                    Gripers.die pos ("Unknown variable " ^ v ^ ".")
            )
        | `Section _ as s   -> type_section context s
        (* literals *)
        | `Constant c as c' -> c', Constant.constant_type c, StringMap.empty
        | `TupleLit [p] ->
           let p = tc p in
              `TupleLit [erase p], typ p, usages p (* When is a tuple not a tuple? *)
        | `TupleLit ps ->
            let ps = List.map tc ps in
              `TupleLit (List.map erase ps), Types.make_tuple_type (List.map typ ps), merge_usages (List.map usages ps)
        | `RecordLit (fields, rest) ->
            let _ =
              (* check that each label only occurs once *)
              List.fold_left
                (fun labels (name, _) ->
                   if StringSet.mem name labels then
                     Gripers.die pos ("Duplicate labels (" ^ name ^ ") in record.")
                   else
                     StringSet.add name labels)
                StringSet.empty fields in
            let fields, field_env, absent_field_env, field_usages =
              List.fold_right
                (fun (label, e) (fields, field_env, absent_field_env, field_usages) ->
                   let e = tc e in
                   let t = typ e in
                     ((label, e)::fields,
                      StringMap.add label (`Present t) field_env,
                      StringMap.add label `Absent absent_field_env,
                      merge_usages [field_usages; usages e]))
                fields ([], StringMap.empty, StringMap.empty, StringMap.empty) in
              begin match rest with
                | None ->
                    `RecordLit (alistmap erase fields, None), `Record (field_env, Unionfind.fresh `Closed, false), field_usages
                | Some r ->
                    let r : phrase * Types.datatype * usagemap = tc r in

                    (* FIXME:

                       we need to explicitly instantiate quantifiers
                       like this *whenever* we do any kind of
                       elimination
                    *)

                    (* explicitly instantiate any quantifiers attached to r *)
                    let r =
                      let (tyargs, rtype) = Instantiate.typ (typ r) in
                      let r' = erase r in
                      let (rexp, rpos) = (r'.node, r'.pos) in
                        with_pos rpos (tappl (rexp, tyargs)), rtype, usages r in

                    let rtype = typ r in

                    (* make sure rtype is a record type that doesn't match any of the existing fields *)
                    let () = unify ~handle:Gripers.extend_record
                      (pos_and_typ r, no_pos (`Record (absent_field_env, Types.fresh_row_variable (`Any, `Any), false))) in

                    let (rfield_env, rrow_var, lr), _ = Types.unwrap_row (TypeUtils.extract_row rtype) in
                    assert (lr = false);
                      (* attempt to extend field_env with the labels from rfield_env
                         i.e. all the labels belonging to the record r
                      *)
                    let field_env' =
                      StringMap.fold (fun label f field_env' ->
                                        match f with
                                          | `Absent ->
                                              if StringMap.mem label field_env then
                                                field_env'
                                              else
                                                StringMap.add label `Absent field_env'
                                          | `Present t ->
                                              if StringMap.mem label field_env then
                                                failwith ("Could not extend record "^ expr_string (erase r)^" (of type "^
                                                            Types.string_of_datatype rtype^") with the label "^
                                                            label^
                                                            " (of type"^Types.string_of_datatype (`Record (field_env, Unionfind.fresh `Closed, false))^
                                                            ") because the labels overlap")
                                              else
                                                StringMap.add label (`Present t) field_env'
                                          | `Var _ -> assert false) rfield_env field_env in
                    let usages = merge_usages [field_usages; usages r] in
                      `RecordLit (alistmap erase fields, Some (erase r)), `Record (field_env', rrow_var, false), usages
              end
        | `ListLit (es, _) ->
            begin match List.map tc es with
              | [] ->
                  let t = Types.fresh_type_variable (`Any, `Any) in
                    `ListLit ([], Some t), `Application (Types.list, [`Type t]), StringMap.empty
              | e :: es ->
                  List.iter (fun e' -> unify ~handle:Gripers.list_lit (pos_and_typ e, pos_and_typ e')) es;
                  `ListLit (List.map erase (e::es), Some (typ e)), `Application (Types.list, [`Type (typ e)]), merge_usages (List.map usages (e::es))
            end
        | `HandlerLit _ -> assert false (* already desugared at this point *)
        | `FunLit (_, lin, (pats, body), location) ->
            let vs = check_for_duplicate_names pos (List.flatten pats) in
            let pats = List.map (List.map tpc) pats in
            let pat_env = List.fold_left (List.fold_left (fun env pat' -> Env.extend env (pattern_env pat'))) Env.empty pats in
            let env' = Env.extend context.var_env pat_env in

            (* type of the effects in the body of the lambda *)
            let effects = (StringMap.empty, Types.fresh_row_variable (`Any, `Any), false) in
            let body = type_check ({context with
                                      var_env = env';
                                      effect_row = effects}) body in

            let () =
              Env.iter (fun v t ->
                let uses = uses_of v (usages body) in
                  if uses <> 1 then
                    if Types.type_can_be_unl t then
                      Types.make_type_unl t
                    else
                      Gripers.non_linearity pos uses v t)
                pat_env in

            let () =
              if lin=`Unl then
                StringMap.iter (fun v _ ->
                                if not (List.mem v vs) then
                                  let t = Env.lookup env' v in
                                  if Types.type_can_be_unl t then
                                    Types.make_type_unl t
                                  else
                                    Gripers.die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^ " is used in a non-linear function literal."))
                               (usages body)
              else () in

            let ftype = make_ft lin pats effects (typ body) in
            let argss =
              let rec arg_types =
                function
                  | (`Function (args, effects, t)) -> (args, effects) :: arg_types t
                  | (`Lolli (args, effects, t)) -> (args, effects) :: arg_types t
                  | _ -> []
              in
                arg_types ftype in

            (*
              FIXME:

              fun (x:a) {x:a} : (b) -> b

              should probably give an error, but doesn't if quantified
              instantiation is switched on.

              Perhaps what we should be doing is filtering out all type
              variables that appear in patterns and type annotations
              from the quantifiers. We could do this by adding them
              to the environment passed to Utils.generalise.

              Needs more thought...
            *)

            let e = `FunLit (Some argss, lin, (List.map (List.map erase_pat) pats, erase body), location) in
              if Settings.get_value Instantiate.quantified_instantiation then
                let (qs, _tyargs), ftype = Utils.generalise context.var_env ftype in
                let _, ftype = Instantiate.typ ftype in
                  tabstr (qs, e), ftype, StringMap.filter (fun v _ -> not (List.mem v vs)) (usages body)
              else
                e, ftype, StringMap.filter (fun v _ -> not (List.mem v vs)) (usages body)

        | `ConstructorLit (c, None, _) ->
            let type' = `Variant (Types.make_singleton_open_row
                                    (c, `Present Types.unit_type)
                                    (`Any, `Any)) in
              `ConstructorLit (c, None, Some type'), type', StringMap.empty

        | `ConstructorLit (c, Some v, _) ->
            let v = tc v in
            let type' = `Variant (Types.make_singleton_open_row
                                    (c, `Present (typ v))
                                    (`Any, `Any)) in
              `ConstructorLit (c, Some (erase v), Some type'), type', usages v

        (* database *)
        | `DatabaseLit (name, (driver, args)) ->
            let driver = opt_map tc driver
            and args   = opt_map tc args
            and name   = tc name in
              `DatabaseLit (erase name, (opt_map erase driver, opt_map erase args)), `Primitive `DB,
              merge_usages [from_option StringMap.empty (opt_map usages driver); from_option StringMap.empty (opt_map usages args); usages name]
        | `TableLit (tname, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) ->
            let tname = tc tname
            and db = tc db
            and keys = tc keys in
            let () = unify ~handle:Gripers.table_name (pos_and_typ tname, no_pos Types.string_type)
            and () = unify ~handle:Gripers.table_db (pos_and_typ db, no_pos Types.database_type)
            and () = unify ~handle:Gripers.table_keys (pos_and_typ keys, no_pos Types.keys_type) in
              `TableLit (erase tname, (dtype, Some (read_row, write_row, needed_row)), constraints, erase keys, erase db),
              `Table (read_row, write_row, needed_row),
              merge_usages [usages tname; usages db]
        | `TableLit _ -> assert false
        | `LensLit (table, _) ->
           let table = tc table in
           let cols = Lens.Types.sort_cols_of_table "" (typ table) in
           let lens_sort = (Lens.Utility.FunDepSet.empty, None, cols) in
           `LensLit (erase table, Some (lens_sort)), `Lens (lens_sort), merge_usages [usages table]
        | `LensKeysLit (table, keys, _) ->
           let table = tc table in
           let cols = Lens.Types.sort_cols_of_table "" (typ table) in
           let keys = Lens.Types.cols_of_phrase keys in
           let fds =
             Lens.Utility.FunDepSet.key_fds
               keys
               (Lens.Helpers.Record.LensColList.present_aliases cols)
           in
           let lens_sort = (fds, None, cols) in
           `LensLit (erase table, Some (lens_sort)), `Lens (lens_sort), merge_usages [usages table]
        | `LensFunDepsLit (table, fds, _) ->
           let table = tc table in
           let cols = Lens.Types.sort_cols_of_table "" (typ table) in
           let fds = Lens.Helpers.Incremental.get_fds fds cols in
           let lens_sort = (fds, None, cols) in
           `LensLit (erase table, Some (lens_sort)), `Lens (lens_sort), merge_usages [usages table]
        | `LensDropLit (lens, drop, key, default, _) ->
           let lens = tc lens
           and default = tc default in
           let sort =
             Lens.Types.drop_lens_sort
               (Lens.Helpers.LensType.sort (typ lens))
               (Lens.Utility.ColSet.singleton drop)
               (Lens.Utility.ColSet.singleton key)
           in
           `LensDropLit (erase lens, drop, key, erase default, Some (sort)), `Lens (sort), merge_usages [usages lens; usages default]
        | `LensSelectLit (lens, predicate, _) ->
           let lens = tc lens in
           let lens_sort = Lens.Helpers.LensType.sort (typ lens) in
               `LensSelectLit(erase lens, predicate, Some (lens_sort)), `Lens(lens_sort), merge_usages [usages lens]
        | `LensJoinLit (lens1, lens2, on, left, right, _) ->
           let lens1 = tc lens1
           and lens2 = tc lens2 in
           let sort1 = Lens.Helpers.LensType.sort (typ lens1) in
           let sort2 = Lens.Helpers.LensType.sort (typ lens2) in
           let sort, _ =
             Lens.Helpers.join_lens_sort
               sort1
               sort2
               (Lens.Types.cols_of_phrase on)
           in
           `LensJoinLit (erase lens1, erase lens2, on, left, right, Some sort), `Lens(sort), merge_usages [usages lens1; usages lens2]
        | `LensGetLit (lens, _) ->
           let lens = tc lens in
           let sort = LensHelpers.LensType.sort (typ lens) in
           let trowtype = Lens.Helpers.Record.get_lens_sort_row_type sort in
           `LensGetLit (erase lens, Some trowtype), Types.make_list_type trowtype, merge_usages [usages lens]
        | `LensPutLit (lens, data, _) ->
           let lens = tc lens in
           let sort = Lens.Helpers.LensType.sort (typ lens) in
           let trowtype = Lens.Helpers.Record.get_lens_sort_row_type sort in
           let data = tc data in
           `LensPutLit (erase lens, erase data, Some trowtype), Types.make_tuple_type [], merge_usages [usages lens; usages data]
        | `DBDelete (pat, from, where) ->
            let pat  = tpc pat in
            let from = tc from in
            let read  = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let write = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let needed = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let () = unify ~handle:Gripers.delete_table
              (pos_and_typ from, no_pos (`Table (read, write, needed))) in
            let () = unify ~handle:Gripers.delete_pattern (ppos_and_typ pat, no_pos read) in

            let hide =
              let bs = Env.domain (pattern_env pat) in
              StringMap.filter (fun b _ -> not (StringSet.mem b bs)) in

            let inner_effects = Types.make_empty_closed_row () in
            let context' = bind_effects (context ++ pattern_env pat) inner_effects in

            let where = opt_map (type_check context') where in
            let () =
              opt_iter
                (fun e -> unify ~handle:Gripers.delete_where (pos_and_typ e, no_pos Types.bool_type)) where in

            (* delete is wild *)
            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.delete_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects))
            in
              `DBDelete (erase_pat pat, erase from, opt_map erase where), Types.unit_type,
              merge_usages [usages from; hide (from_option StringMap.empty (opt_map usages where))]
        | `DBInsert (into, labels, values, id) ->
            let into   = tc into in
            let values = tc values in
            let id = opt_map tc id in
            let read  = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let write = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let needed = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let () = unify ~handle:Gripers.insert_table
              (pos_and_typ into, no_pos (`Table (read, write, needed))) in

            let field_env =
              List.fold_right
                (fun name field_env ->
                   if StringMap.mem name field_env then
                     Gripers.die pos "Duplicate labels in insert expression."
                   else
                     StringMap.add name (`Present (Types.fresh_type_variable (`Any, `Base))) field_env)
                labels StringMap.empty in

            (* check that the fields in the type of values match the declared labels *)
            let () =
              unify ~handle:Gripers.insert_values
                (pos_and_typ values,
                 no_pos (Types.make_list_type (`Record (field_env, Unionfind.fresh `Closed, false)))) in

            let needed_env =
              StringMap.map
                (fun _f -> Types.fresh_presence_variable (`Any, `Base))
                field_env in

            (* all fields being inserted must be present in the read row *)
            let () = unify ~handle:Gripers.insert_read
              (no_pos read, no_pos (`Record (field_env, Types.fresh_row_variable (`Any, `Base), false))) in

            (* all fields being inserted must be present in the write row *)
            let () = unify ~handle:Gripers.insert_write
              (no_pos write, no_pos (`Record (field_env, Types.fresh_row_variable (`Any, `Base), false))) in

            (* all fields being inserted must be consistent with the needed row *)
            let () = unify ~handle:Gripers.insert_needed
              (no_pos needed, no_pos (`Record (needed_env, Unionfind.fresh `Closed, false))) in

            (* insert returning ... *)
            let return_type =
              match id with
                | None -> Types.unit_type
                | Some ({node=(id : phrasenode); _}, _, _) ->
                    begin
                      match id with
                        | `Constant (`String id) ->
                            (* HACK: The returned column is encoded as
                               a string.  We check here that it
                               appears as a column in the read type of
                               the table.
                            *)
                            unify
                              ~handle:Gripers.insert_id
                              (no_pos read,
                               no_pos (`Record (StringMap.singleton id (`Present Types.int_type), Types.fresh_row_variable (`Any, `Base), false)));
                            Types.int_type
                        | _ -> assert false
                    end in

            (* insert is wild *)
            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.insert_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects))
            in
              `DBInsert (erase into, labels, erase values, opt_map erase id), return_type,
              merge_usages [usages into; usages values; from_option StringMap.empty (opt_map usages id)]
        | `DBUpdate (pat, from, where, set) ->
            let pat  = tpc pat in
            let from = tc from in
            let read =  `Record (Types.make_empty_open_row (`Any, `Base)) in
            let write = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let needed = `Record (Types.make_empty_open_row (`Any, `Base)) in
            let () = unify ~handle:Gripers.update_table
              (pos_and_typ from, no_pos (`Table (read, write, needed))) in

            let hide =
              let bs = Env.domain (pattern_env pat) in
              StringMap.filter (fun b _ -> not (StringSet.mem b bs)) in

            (* the pattern should match the read type *)
            let () = unify ~handle:Gripers.update_pattern (ppos_and_typ pat, no_pos read) in

            let inner_effects = Types.make_empty_closed_row () in
            let context' = bind_effects (context ++ pattern_env pat) inner_effects in

            let where = opt_map (type_check context') where in

            (* check that the where clause is boolean *)
            let () =
              opt_iter
                (fun e -> unify ~handle:Gripers.update_where (pos_and_typ e, no_pos Types.bool_type)) where in

            let set, field_env =
              List.fold_right
                (fun (name, exp) (set, field_env) ->
                   let exp = type_check context' exp in
                     if StringMap.mem name field_env then
                       Gripers.die pos "Duplicate fields in update expression."
                     else
                       (name, exp)::set, StringMap.add name (`Present (typ exp)) field_env)
                set ([], StringMap.empty) in

            let needed_env =
              StringMap.map
                (fun _f -> Types.fresh_presence_variable (`Any, `Base))
                field_env in

            (* all fields being updated must be present in the read row *)
            let () = unify ~handle:Gripers.update_read
              (no_pos read, no_pos (`Record (field_env, Types.fresh_row_variable (`Any, `Base), false))) in

            (* all fields being updated must be present in the write row *)
            let () = unify ~handle:Gripers.update_write
              (no_pos write, no_pos (`Record (field_env, Types.fresh_row_variable (`Any, `Base), false))) in

            (* all fields being updated must be consistent with the needed row *)
            let () = unify ~handle:Gripers.update_needed
              (no_pos needed, no_pos (`Record (needed_env, Types.fresh_row_variable (`Any, `Base), false))) in

            (* update is wild *)
            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.update_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects))
            in
              `DBUpdate (erase_pat pat, erase from, opt_map erase where, List.map (fun (n,(p,_,_)) -> n, p) set),
              Types.unit_type,
              merge_usages (usages from :: hide (from_option StringMap.empty (opt_map usages where)) :: List.map hide (List.map (usages -<- snd) set))
        | `Query (range, p, _) ->
            let range, outer_effects, range_usages =
              match range with
                | None -> None, Types.make_empty_open_row (`Any, `Any), StringMap.empty
                | Some (limit, offset) ->
                    let limit = tc limit in
                    let () = unify ~handle:Gripers.range_bound (pos_and_typ limit, no_pos Types.int_type) in
                    let offset = tc offset in
                    let () = unify ~handle:Gripers.range_bound (pos_and_typ offset, no_pos Types.int_type) in
                    let outer_effects =
                      Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
                    in
                      Some (erase limit, erase offset), outer_effects, merge_usages [usages limit; usages offset] in
            let inner_effects = Types.make_empty_closed_row () in
            let () = unify ~handle:Gripers.query_outer
              (no_pos (`Record context.effect_row), no_pos (`Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in
            let () = if Settings.get_value Basicsettings.Shredding.relax_query_type_constraint then ()
                     else let shape = Types.make_list_type (`Record (StringMap.empty, Types.fresh_row_variable (`Any, `Base), false)) in
                          unify ~handle:Gripers.query_base_row (pos_and_typ p, no_pos shape) in
            `Query (range, erase p, Some (typ p)), typ p, merge_usages [range_usages; usages p]
        (* mailbox-based concurrency *)
        | `Spawn (`Wait, l, p, _) ->
            assert (l = `NoSpawnLocation);
            (* (() -{b}-> d) -> d *)
            let inner_effects = Types.make_empty_open_row (`Any, `Any) in
            (* TODO: check if pid_type is actually needed somewhere *)
            (* let pid_type = `Application (Types.process, [`Row inner_effects]) in *)
            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.spawn_wait_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in
            let return_type = typ p in
              `Spawn (`Wait, l, erase p, Some inner_effects), return_type, usages p
        | `Spawn (k, given_loc, p, _) ->
            (* Location -> (() -e-> _) -> Process (e) *)
            (match given_loc with
              | `ExplicitSpawnLocation loc_phr ->
                  let target_ty = `Application (Types.spawn_location, []) in
                  let t = tc loc_phr in
                  let _ = unify ~handle:Gripers.spawn_location (pos_and_typ t, no_pos target_ty) in ()
              | _ -> ());

            (* (() -e-> _) -> Process (e) *)
            let inner_effects = Types.make_empty_open_row (`Any, `Any) in
            let pid_type = `Application (Types.process, [`Row inner_effects]) in
            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.spawn_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in
            if not (Types.type_can_be_unl (typ p)) then
              Gripers.die pos ("Spawned processes cannot produce values of linear type (here " ^ Types.string_of_datatype (typ p) ^ ")");
            `Spawn (k, given_loc, erase p, Some inner_effects), pid_type, usages p
        | `Receive (binders, _) ->
            let mb_type = Types.fresh_type_variable (`Any, `Any) in
            let effects =
              Types.row_with ("wild", `Present Types.unit_type)
                (Types.make_singleton_open_row ("hear", `Present mb_type) (`Any, `Any)) in

            let () = unify ~handle:Gripers.receive_mailbox
              (no_pos (`Record context.effect_row), no_pos (`Record effects)) in

            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Gripers.receive_patterns
              (no_pos mb_type, no_pos pattern_type)
            in
              `Receive (erase_cases binders, Some body_type), body_type, usages_cases binders

        (* session-based concurrency *)
        (* | `Link (l, r) -> *)
        (*   let l = tc l in *)
        (*   let r = tc r in *)
        (*     unify ~handle:Gripers.cp_link_session *)
        (*       (pos_and_typ l, no_pos (Types.fresh_type_variable (`Any, `Session))); *)
        (*     unify ~handle:Gripers.cp_link_session *)
        (*       (pos_and_typ r, no_pos (Types.fresh_type_variable (`Any, `Session))); *)
        (*     unify ~handle:Gripers.cp_link_dual *)
        (*       ((exp_pos l, Types.dual_type (typ l)), pos_and_typ r); *)
        (*     `Link (erase l, erase r), Types.unit_type, merge_usages [usages l; usages r] *)
        | `Select (l, e) ->
           let e = tc e in
           let selected_session = Types.fresh_type_variable (`Any, `Session) in
           unify ~handle:Gripers.selection
                 (pos_and_typ e, no_pos (`Select (Types.make_singleton_open_row
                                                              (l, `Present selected_session)
                                                              (`Any, `Session))));
           `Select (l, erase e), selected_session, usages e
        | `Offer (e, branches, _) ->
           let e = tc e in
           let branches, pattern_type, body_type = type_cases branches in
           let r = Types.make_empty_open_row (`Any, `Session) in
             unify ~handle:Gripers.offer_variant (no_pos pattern_type, no_pos (`Variant r));
             unify ~handle:Gripers.offer_patterns (pos_and_typ e, no_pos (`Choice r));
             `Offer (erase e, erase_cases branches, Some body_type), body_type, merge_usages [usages e; usages_cases branches]

        (* No comment *)
        | `CP p ->
           let (p, t, u) = type_cp context p in
           `CP p, t, u

        (* applications of various sorts *)
        | `UnaryAppl ((_, op), p) ->
            let tyargs, opt, op_usage = type_unary_op context op
            and p = tc p
            and rettyp = Types.fresh_type_variable (`Any, `Any) in
              unify ~handle:Gripers.unary_apply
                ((Operators.string_of_unary_op op, opt),
                 no_pos (`Function (Types.make_tuple_type [typ p], context.effect_row, rettyp)));
              `UnaryAppl ((tyargs, op), erase p), rettyp, merge_usages [usages p; op_usage]
        | `InfixAppl ((_, op), l, r) ->
            let tyargs, opt, op_usages = type_binary_op context op in
            let l = tc l
            and r = tc r
            and rettyp = Types.fresh_type_variable (`Any, `Any) in
              unify ~handle:Gripers.infix_apply
                ((Operators.string_of_binop op, opt),
                 no_pos (`Function (Types.make_tuple_type [typ l; typ r],
                                    context.effect_row, rettyp)));
              `InfixAppl ((tyargs, op), erase l, erase r), rettyp, merge_usages [usages l; usages r; op_usages]
        | `RangeLit (l, r) ->
            let l, r = tc l, tc r in
            let () = unify ~handle:Gripers.range_bound  (pos_and_typ l,
                                                         no_pos Types.int_type)
            and () = unify ~handle:Gripers.range_bound  (pos_and_typ r,
                                                         no_pos Types.int_type)
            in `RangeLit (erase l, erase r),
               Types.make_list_type Types.int_type,
               merge_usages [usages l; usages r]
        | `FnAppl (f, ps) ->
            let f = tc f in
            let ps = List.map (tc) ps in

              (*
                SL: though superficially appealing, the following is unsound
                as it evidently violates the value restriction!
                Thus we disable it by default.

                I think the isomorphism for projections is still OK.

                We can take advantage of this type isomorphism:

                forall X.P -> Q == P -> forall X.Q
                where X is not free in P

                What we need to do for:

                forall XS.P -> Q

                - instantiate each quantifier in XS
                that is free in P

                - push the other quantifiers
                into the return type

                - generate an appropriate term to
                explicitly witness the isomorphism

                For f a we generate /\ZS.f XS a.
              *)
              begin
                match Types.concrete_type (typ f) with
                  | `ForAll (qs, `Function (fps, fe, _)) as t ->

                      (* the free type variables in the arguments (and effects) *)
                      let arg_vars =
                        Types.TypeVarSet.union (Types.free_type_vars fps) (Types.free_row_type_vars fe) in

                      (* return true if this quantifier appears free in the arguments (or effects) *)
                      let free_in_arg q = Types.TypeVarSet.mem (Types.var_of_quantifier q) arg_vars in

                      (*
                        since we've smashed through the quantifiers, we should
                        make any remaining quantifiers flexible
                      *)

                      (* xs is a list of tuples of the shape:
                         (original quantifier, (fresh quantifier, fresh type argument))
                      *)
                      let xs =
                        List.map
                          (fun q ->
                             q, Types.freshen_quantifier_flexible q)
                          (Types.unbox_quantifiers qs) in

                      (* quantifiers for the return type *)
                      let rqs =
                        if Settings.get_value dodgey_type_isomorphism then
                          (fst -<- List.split -<- snd -<- List.split)
                            (List.filter
                               (fun (q, _) -> not (free_in_arg q))
                               xs)
                        else
                          [] in

                      (* type arguments to apply f to *)
                      let tyargs = (snd -<- List.split -<- snd -<- List.split) xs in
                        begin
                          match Instantiate.apply_type t tyargs with
                            | `Function (fps, fe, rettyp) ->
                                let rettyp = Types.for_all (rqs, rettyp) in
                                let ft = `Function (fps, fe, rettyp) in
                                let f' = erase f in
                                let fn, fpos = f'.node, f'.pos in
                                let e = tabstr (rqs, `FnAppl (with_pos fpos (tappl (fn, tyargs)), List.map erase ps)) in
                                  unify ~handle:Gripers.fun_apply
                                    ((exp_pos f, ft), no_pos (`Function (Types.make_tuple_type (List.map typ ps),
                                                                         context.effect_row,
                                                                         rettyp)));
                                  e, rettyp, merge_usages (usages f :: List.map usages ps)
                            | `Lolli (fps, fe, rettyp) ->
                                let rettyp = Types.for_all (rqs, rettyp) in
                                let ft = `Function (fps, fe, rettyp) in
                                let f' = erase f in
                                let fn, fpos = f'.node, f'.pos in
                                let e = tabstr (rqs, `FnAppl (with_pos fpos (tappl (fn, tyargs)), List.map erase ps)) in
                                  unify ~handle:Gripers.fun_apply
                                    ((exp_pos f, ft), no_pos (`Lolli (Types.make_tuple_type (List.map typ ps),
                                                                      context.effect_row,
                                                                      rettyp)));
                                  e, rettyp, merge_usages (usages f :: List.map usages ps)
                            | _ ->
                                assert false
                        end

                  | ft ->
                      let rettyp = Types.fresh_type_variable (`Any, `Any) in
                      begin
                        unify_or ~handle:Gripers.fun_apply ~pos
                                ((exp_pos f, ft), no_pos (`Function (Types.make_tuple_type (List.map typ ps),
                                                                     context.effect_row, rettyp)))
                                ((exp_pos f, ft), no_pos (`Lolli (Types.make_tuple_type (List.map typ ps),
                                                                  context.effect_row, rettyp)))
                      end;
                      `FnAppl (erase f, List.map erase ps), rettyp, merge_usages (usages f :: List.map usages ps)
              end
        | `TAbstr (qs, e) ->
            let e, t, u = tc e in
            let qs = Types.unbox_quantifiers qs in
            let t = Types.for_all(qs, t) in
              tabstr (qs, e.node), t, u
        | `TAppl (e, _qs) ->
            let e, t, u = tc e in e.node, t, u

        (* xml *)
        | `Xml (tag, attrs, attrexp, children) ->
            let attrs = alistmap (List.map (tc)) attrs
            and attrexp = opt_map tc attrexp
            and children = List.map (tc) children in
            let () = List.iter
              (snd ->-
                 List.iter (fun attr -> unify ~handle:Gripers.xml_attribute
                              (pos_and_typ attr, no_pos Types.string_type))) attrs
            and () =
              opt_iter
                (fun e ->
                   unify ~handle:Gripers.xml_attributes
                     (pos_and_typ e, no_pos (
                        (Instantiate.alias "Attributes" [] context.tycon_env)))) attrexp
            and () =
              List.iter (fun child ->
                           unify ~handle:Gripers.xml_child (pos_and_typ child, no_pos Types.xml_type)) children in
              `Xml (tag,
                    List.map (fun (x,p) -> (x, List.map erase p)) attrs,
                    opt_map erase attrexp,
                    List.map erase children),
              Types.xml_type,
              merge_usages (List.concat [ List.concat (List.map snd (alistmap (List.map usages) attrs));
                                          [from_option StringMap.empty (opt_map usages attrexp)];
                                          List.map usages children ])
        | `TextNode _ as t -> t, Types.xml_type, StringMap.empty
        | `Formlet (body, yields) ->
            let body = tc body in
            let env = extract_formlet_bindings (erase body) in
            let vs = Env.domain env in
            let context' = context ++ env in
            let yields = type_check context' yields in
              unify ~handle:Gripers.formlet_body (pos_and_typ body, no_pos Types.xml_type);
              (`Formlet (erase body, erase yields),
               Instantiate.alias "Formlet" [`Type (typ yields)] context.tycon_env,
               merge_usages [usages body; StringMap.filter (fun v _ -> not (StringSet.mem v vs)) (usages yields)])
        | `Page e ->
            let e = tc e in
              unify ~handle:Gripers.page_body (pos_and_typ e, no_pos Types.xml_type);
              `Page (erase e), Instantiate.alias "Page" [] context.tycon_env, usages e
        | `FormletPlacement (f, h, attributes) ->
            let t = Types.fresh_type_variable (`Any, `Any) in

            let f = tc f
            and h = tc h
            and attributes = tc attributes in
            let () = unify ~handle:Gripers.render_formlet
              (pos_and_typ f, no_pos (Instantiate.alias "Formlet" [`Type t] context.tycon_env)) in
            let () = unify ~handle:Gripers.render_handler
              (pos_and_typ h, (exp_pos f,
                               Instantiate.alias "Handler" [`Type t] context.tycon_env)) in
            let () = unify ~handle:Gripers.render_attributes
              (pos_and_typ attributes, no_pos (Instantiate.alias "Attributes" [] context.tycon_env))
            in
              `FormletPlacement (erase f, erase h, erase attributes), Types.xml_type, merge_usages [usages f; usages h; usages attributes]
        | `PagePlacement e ->
            let e = tc e in
            let pt = Instantiate.alias "Page" [] context.tycon_env in
              unify ~handle:Gripers.page_placement (pos_and_typ e, no_pos pt);
              `PagePlacement (erase e), Types.xml_type, usages e
        | `FormBinding (e, pattern) ->
            let e = tc e
            and pattern = tpc pattern in
            let a = Types.fresh_type_variable (`Any, `Any) in
            let ft = Instantiate.alias "Formlet" [`Type a] context.tycon_env in
              unify ~handle:Gripers.form_binding_body (pos_and_typ e, no_pos ft);
              unify ~handle:Gripers.form_binding_pattern (ppos_and_typ pattern, (exp_pos e, a));
              `FormBinding (erase e, erase_pat pattern), Types.xml_type, usages e

        (* various expressions *)
        | `Iteration (generators, body, where, orderby) ->
            let is_query =
              List.exists (function
                             | `List _ -> false
                             | `Table _ -> true) generators in
            let context =
              if is_query then
                {context with effect_row = Types.make_empty_closed_row ()}
              else
                context in
            let generators, generator_usages, environments =
              List.fold_left
                (fun (generators, generator_usages, environments) ->
                   function
                     | `List (pattern, e) ->
                         let a = Types.fresh_type_variable (`Any, `Any) in
                         let lt = Types.make_list_type a in
                         let pattern = tpc pattern in
                         let e = tc e in
                         let () = unify ~handle:Gripers.iteration_list_body (pos_and_typ e, no_pos lt) in
                         let () = unify ~handle:Gripers.iteration_list_pattern (ppos_and_typ pattern, (exp_pos e, a))
                         in
                           (`List (erase_pat pattern, erase e) :: generators,
                            usages e :: generator_usages,
                            pattern_env pattern :: environments)
                     | `Table (pattern, e) ->
                         let a = Types.fresh_type_variable (`Any, `Any) in
                         let tt = Types.make_table_type (a, Types.fresh_type_variable (`Any, `Any), Types.fresh_type_variable (`Any, `Any)) in
                         let pattern = tpc pattern in
                         let e = tc e in
                         let () = unify ~handle:Gripers.iteration_table_body (pos_and_typ e, no_pos tt) in
                         let () = unify ~handle:Gripers.iteration_table_pattern (ppos_and_typ pattern, (exp_pos e, a)) in
                           (`Table (erase_pat pattern, erase e) :: generators,
                            usages e :: generator_usages,
                            pattern_env pattern:: environments))
                ([], [], []) generators in
            let generators = List.rev generators in
            let context = context ++ List.fold_left Env.extend Env.empty environments in
            let tc = type_check context in
            let body = tc body in
            let where = opt_map tc where in
            let orderby = opt_map tc orderby in
            let () =
              unify ~handle:Gripers.iteration_body
                (pos_and_typ body, no_pos (Types.make_list_type (Types.fresh_type_variable (`Any, `Any)))) in
            let () =
              opt_iter (fun where -> unify ~handle:Gripers.iteration_where
                          (pos_and_typ where, no_pos Types.bool_type)) where in

            let () =
              opt_iter
                (fun order ->
                   unify ~handle:Gripers.iteration_base_order
                     (pos_and_typ order, no_pos (`Record (Types.make_empty_open_row (`Any, `Base))))) orderby in
            let () =
              if is_query && not (Settings.get_value Basicsettings.Shredding.relax_query_type_constraint) then
                unify ~handle:Gripers.iteration_base_body
                  (pos_and_typ body, no_pos (Types.make_list_type (`Record (Types.make_empty_open_row (`Any, `Base))))) in
            let e = `Iteration (generators, erase body, opt_map erase where, opt_map erase orderby) in
            let vs = List.fold_left StringSet.union StringSet.empty (List.map Env.domain environments) in
            let us = merge_usages (List.append generator_usages
                                               (List.map (StringMap.filter (fun v _ -> not (StringSet.mem v vs)))
                                                         [usages body; from_option StringMap.empty (opt_map usages where); from_option StringMap.empty (opt_map usages orderby)])) in
              if is_query then
                `Query (None, with_pos pos e, Some (typ body)), typ body, us
              else
                e, typ body, us
        | `Escape (bndr, e) ->
            (* There's a question here whether to generalise the
               return type of continuations.  With `escape'
               continuations are let-bound, so generalising the return
               type is sound.  With `call/cc' continuations are
               lambda-bound so the return type cannot be generalised.
               If we do generalise here then we can accept more valid
               programs, since the continuation can then be used in
               any context, e.g.:

               escape y in {
               var _ = y(1) == "";
               var _ = y(1) == true;
               2
               }

               However, currently we desugar escape to call/cc, so
               generalising will mean accepting programs that have an
               invalid type in the IR (although they're guaranteed not
               to "go wrong".)

               (Also, should the mailbox type be generalised?)
            *)
            let name = name_of_binder bndr in
            let f = Types.fresh_type_variable (`Any, `Any) in
            let t = Types.fresh_type_variable (`Any, `Any) in

            let eff = Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any) in

            let cont_type = `Function (Types.make_tuple_type [f], eff, t) in
            let context' = {context
                            with var_env = Env.bind context.var_env (name, cont_type)} in
            let e = type_check context' e in

            let () =
              let outer_effects =
                Types.make_singleton_open_row ("wild", `Present Types.unit_type) (`Any, `Any)
              in
                unify ~handle:Gripers.escape_outer
                  (no_pos (`Record context.effect_row), no_pos (`Record outer_effects)) in

            let () = unify ~handle:Gripers.escape (pos_and_typ e, no_pos f) in
              `Escape (set_binder_type bndr cont_type, erase e), typ e, StringMap.filter (fun v _ -> v <> name) (usages e)
        | `Conditional (i,t,e) ->
            let i = tc i
            and t = tc t
            and e = tc e in
              unify ~handle:Gripers.if_condition
                (pos_and_typ i, no_pos (`Primitive `Bool));
              unify ~handle:Gripers.if_branches
                (pos_and_typ t, pos_and_typ e);
              `Conditional (erase i, erase t, erase e), (typ t), merge_usages [usages i; usage_compat [usages t; usages e]]
        | `Block (bindings, e) ->
            let context', bindings, usage_builder = type_bindings context bindings in
            let e = type_check (Types.extend_typing_environment context context') e in
            `Block (bindings, erase e), typ e, usage_builder (usages e)
        | `Regex r ->
            `Regex (type_regex context r),
            Instantiate.alias "Regex" [] context.tycon_env,
            StringMap.empty
        | `Projection (r,l) ->
            (*
              Take advantage of the type isomorphism:

              forall X.(P, Q) == (forall X.P, forall X.Q)

              Before doing the projection, grab any quantifiers
              and pass them through to the result type.

              If the label matches one of the existing ones
              in the field env then we unify against a singleton
              closed row.

              Otherwise, we instantiate the quantifiers and unify
              against a singleton open row.

            *)
            let r = tc r in
              begin
                match TypeUtils.concrete_type (typ r) with
                  | `ForAll (qs, `Record _row) as t ->
                      let xs =
                        List.map
                          (fun q ->
                             q, Types.freshen_quantifier_flexible q)
                          (Types.unbox_quantifiers qs) in

                      (* type arguments to apply r to *)
                      let tyargs = (snd -<- List.split -<- snd -<- List.split) xs in

                      let rt = Instantiate.apply_type t tyargs in

                      let field_env, _row_var, lr =
                        match rt with
                        | `Record row -> fst (Types.unwrap_row row)
                        | _ -> assert false in
                      assert (not lr);
                      begin
                        match StringMap.lookup l field_env with
                        | Some (`Present t) ->
                          (* the free type variables in the projected type *)
                          let vars = Types.free_type_vars t in

                          (* return true if this quantifier appears
                             free in the projected type *)
                          let free_in_body q = Types.TypeVarSet.mem (Types.var_of_quantifier q) vars in

                          (* quantifiers for the projected type *)
                          let pqs =
                            (fst -<- List.split -<- snd -<- List.split)
                              (List.filter
                                 (fun (q, _) -> free_in_body q)
                                 xs) in

                          let fieldtype = Types.for_all (pqs, t) in

                          (* really we just need to unify the presence
                             variable with `Presence, but our griper
                             interface doesn't currently support
                             that *)
                          let rt = `Record (StringMap.singleton l (`Present fieldtype), Types.closed_row_var, false) in
                          unify ~handle:Gripers.projection
                            ((exp_pos r, rt),
                             no_pos (`Record (Types.make_singleton_closed_row
                                                (l, `Present (Types.fresh_type_variable (`Any, `Any))))));
                          let r' = erase r in
                          let rn, rpos = r'.node, r'.pos in
                          let e = tabstr (pqs, `Projection (with_pos rpos (tappl (rn, tyargs)), l)) in
                          e, fieldtype, usages r
                        | Some (`Absent | `Var _)
                        | None ->
                          let fieldtype = Types.fresh_type_variable (`Any, `Any) in
                          unify ~handle:Gripers.projection
                            ((exp_pos r, rt),
                             no_pos (`Record (Types.make_singleton_open_row
                                                (l, `Present fieldtype)
                                                (`Unl, `Any))));
                          let r' = erase r in
                          let rn, rpos = r'.node, r'.pos in
                          let e = `Projection (with_pos rpos (tappl (rn, tyargs)), l) in
                          e, fieldtype, usages r
                      end
                  | _ ->
                      let fieldtype = Types.fresh_type_variable (`Any, `Any) in
                        unify ~handle:Gripers.projection
                          (pos_and_typ r, no_pos (`Record (Types.make_singleton_open_row
                                                             (l, `Present fieldtype)
                                                             (`Unl, `Any))));
                        `Projection (erase r, l), fieldtype, usages r
              end
        | `With (r, fields) ->
            let r = tc r in
            let fields = alistmap tc fields in

            let () =
              let fields_type =
                `Record (List.fold_right
                           (fun (lab, _) row ->
                              Types.row_with (lab, `Present (Types.fresh_type_variable (`Unl, `Any))) row)
                           fields (Types.make_empty_open_row (`Any, `Any))) in
                unify ~handle:Gripers.record_with (pos_and_typ r, no_pos fields_type) in
            let (rfields, row_var, lr), _ = Types.unwrap_row (TypeUtils.extract_row (typ r)) in
            assert (not lr);
            let rfields =
              StringMap.mapi
                (fun name t ->
                   if List.mem_assoc name fields then
                     `Present (snd3 (List.assoc name fields))
                   else t)
                rfields
            in
              `With (erase r, alistmap erase fields), `Record (rfields, row_var, false), merge_usages (usages r :: List.map usages (range fields))
        | `TypeAnnotation (e, (_, Some t as dt)) ->
            let e = tc e in
              unify ~handle:Gripers.type_annotation (pos_and_typ e, no_pos t);
              `TypeAnnotation (erase e, dt), t, usages e
        | `TypeAnnotation _ -> assert false
        | `Upcast (e, (_, Some t1 as t1'), (_, Some t2 as t2')) ->
            let e = tc e in
              if Types.is_sub_type (t2, t1) then
                begin
                  unify ~handle:Gripers.upcast_source (pos_and_typ e, no_pos t2);
                  `Upcast (erase e, t1', t2'), t1, usages e
                end
              else
                Gripers.upcast_subtype pos t2 t1
        | `Upcast _ -> assert false
        | `Handle { sh_expr = m; sh_value_cases = val_cases; sh_effect_cases = eff_cases; sh_descr = descr; } ->
           let rec pop_last = function
             | [] -> assert false
             | [x] -> x, []
             | x' :: xs ->
                let (x, xs') = pop_last xs in
                x, x' :: xs'
           in
           (** allow_wild adds wild : () to the given effect row *)
           let allow_wild : Types.row -> Types.row
	     = fun row ->
	       let fields = StringMap.add "wild" Types.unit_type StringMap.empty in
	       Types.extend_row fields row
           in
           (** returns a pair of lists whose first component is the
               value clauses, while the second component is the
               operation clauses *)
           let split_handler_cases : (pattern * phrase) list -> (pattern * phrase) list * (pattern * phrase) list
             = fun cases ->
             let ret, ops =
               List.fold_left
                 (fun (val_cases, eff_cases) (pat, body) ->
                   match pat.node with
                   | `Variant ("Return", None)     -> Gripers.die pat.pos "Improper pattern-matching on return value"
                   | `Variant ("Return", Some pat) -> (pat, body) :: val_cases, eff_cases
                   | _                             -> val_cases, (pat, body) :: eff_cases)
                 ([], []) cases
             in
             List.rev ret, List.rev ops
           in
           (* type parameters *)
           let henv = context in
           let (henv, params, descr) =
             match descr.shd_params with
             | Some { shp_bindings; _ } ->
                let _ =
                  check_for_duplicate_names pos (List.map snd shp_bindings)
                in
                let type_binding (body, pat) =
                  let body = tc body in
                  let pat = tpc pat in
                  unify ~handle:Gripers.handle_parameter_pattern (ppos_and_typ pat, (pos_and_typ body));
                  (body, pat)
                in
                let typed_bindings = List.map type_binding shp_bindings in
                let pat_types =
                  List.map (snd ->- pattern_typ) typed_bindings
                in
                let param_env =
                  List.fold_left
                    (fun env p ->
                      env ++ pattern_env p)
                    henv (List.map snd typed_bindings)
                in
                (param_env, typed_bindings, { descr with shd_params = Some { shp_bindings = List.map (fun (body, pat) -> erase body, erase_pat pat) typed_bindings;
                                                                             shp_types = pat_types } })
             | None -> (henv, [], descr)
           in
           let type_cases val_cases eff_cases =
             let wild_row () =
               let fresh_row = Types.make_empty_open_row (`Unl, `Any) in
               allow_wild fresh_row
             in
             let rt = Types.fresh_type_variable (`Unl, `Any) in
             let bt = Types.fresh_type_variable (`Unl, `Any) in
             let inner_eff = wild_row () in
             let outer_eff = wild_row () in
             (* Type value patterns *)
             let val_cases, val_pats =
               List.fold_right
                 (fun (pat, body) (cases, pats) ->
                   let pat = tpo pat in
                   let () =
                     unify ~handle:Gripers.handle_value_patterns
                       (ppos_and_typ pat, no_pos rt)
                   in
                   (pat, body)::cases, pat :: pats)
                 val_cases ([], [])
             in
             let rt = close_pattern_type (List.map fst3 val_pats) rt in
             (* Type effect patterns *)
             let eff_cases =
               List.fold_right
                 (fun (pat, body) cases ->
                   let pat =
                     match pat with
                     | { node = `Variant (opname, Some pat'); pos } ->
                        begin match pat'.node with
                        | `Tuple [] ->
                           with_pos pos (`Effect (opname, [], with_dummy_pos `Any))
                        | `Tuple ps ->
                           let kpat, pats = pop_last ps in
                           let eff = `Effect (opname, pats, kpat) in
                           with_pos pos eff
                        | _ -> with_pos pos (`Effect (opname, [], pat'))
                        end
                     | { node = `Variant (opname, None); pos } ->
                        with_pos pos (`Effect (opname, [], with_dummy_pos `Any))
                     | {pos;_} -> Gripers.die pos "Improper pattern matching"
                   in
                   let pat = tpo pat in
                   unify ~handle:Gripers.handle_effect_patterns
                         (ppos_and_typ pat, no_pos (`Effect inner_eff));
                   (* We may have to patch up the inferred resumption
                      type as `type_pattern' cannot infer the
                      principal type for a resumption in a
                      parameterised handler since it requires access
                      to information which is not conveyed by
                      pattern. TODO: perhaps augment the pattern with
                      arity information. *)
                   let (pat, env, effrow) = pat in
                   let effname, kpat =
                     match pat.node with
                     | `Effect (name, _, kpat) -> name, kpat
                     | _ -> assert false
                   in
                   let pat, kpat =
                     let rec find_effect_type eff = function
                       | (eff', t) :: _ when eff = eff' ->
                          begin match t with
                          | `Present t -> t
                          | _ -> assert false
                          end
                       | _ :: xs -> find_effect_type eff xs
                       | [] -> raise Not_found
                     in
                     match descr.shd_params with
                     | Some params when descr.shd_depth = `Deep ->
                        let handler_params = params.shp_types in
                        begin match kpat.node with
                        | `Any ->
                           let kt =
                             let domain =
                               (Types.fresh_type_variable (`Unl, `Any)) :: handler_params
                             in
                             let effects = Types.make_empty_open_row (`Unl, `Any) in
                             let codomain =  Types.fresh_type_variable (`Unl, `Any) in
                             Types.make_function_type domain effects codomain
                           in
                           (pat, env, effrow), (kpat, Env.empty, kt)
                        | `As (bndr,_)
                        | `Variable bndr ->
                           let kname = name_of_binder bndr in
                           let kt =
                             let (fields,_,_) = TypeUtils.extract_row effrow in
                             let kt = find_effect_type effname (StringMap.to_alist fields) in
                             let op_param = TypeUtils.return_type kt in
                             let typ = Env.lookup env kname in
                             let domain =
                               op_param :: handler_params
                             in
                             let effs, codomain =
                               TypeUtils.(effect_row typ, return_type typ)
                             in
                             Types.make_function_type domain effs codomain
                           in
                           let env = Env.bind env (kname, kt) in
                           let env' = Env.bind Env.empty (kname, kt) in
                           (pat, env, effrow), (kpat, env', kt)
                        | _ -> assert false
                        end
                     | _ ->
                        match kpat.node with
                        | `As (bndr,_)
                        | `Variable bndr ->
                           let kname = name_of_binder bndr in
                           let kt =
                             match Env.find env kname with
                             | Some t -> t
                             | None -> assert false
                           in
                           let env' = Env.bind Env.empty (kname, kt) in
                           (pat, env, effrow), (kpat, env', kt)
                        | `Any ->
                           let kt =
                             Types.make_function_type
                               [Types.fresh_type_variable (`Unl, `Any)]
                               (Types.make_empty_open_row (`Unl, `Any))
                               (Types.fresh_type_variable (`Unl, `Any))
                           in
                           (pat, env, effrow), (kpat, Env.empty, kt)
                        | _ -> assert false
                   in
                   (pat, kpat, body) :: cases)
                 eff_cases []
             in
             (* Closing of subpatterns in effect patterns *)
             let inner_eff = TypeUtils.extract_row (close_pattern_type (List.map (fst3 ->- fst3) eff_cases) (`Effect inner_eff)) in
             (* Type value clause bodies *)
             let val_cases =
               List.fold_right
                 (fun (pat, body) cases ->
                   let body = type_check (henv ++ pattern_env pat) body in
                   let () = unify ~handle:Gripers.handle_branches
	                      (pos_and_typ body, no_pos bt) in
                   let vs = Env.domain (pattern_env pat) in
                   let vs' = Env.domain henv.var_env in
                   let us = StringMap.filter (fun v _ -> not (StringSet.mem v vs || StringSet.mem v vs')) (usages body) in
                   (pat, update_usages body us) :: cases)
                 val_cases []
             in
             (* Type operation clause bodies and resumptions *)
             let eff_cases =
               List.fold_right
                 (fun (pat, (kpat : pattern * Types.datatype Env.t * Types.datatype), body) cases ->
                   let body = type_check (henv ++ pattern_env pat) body in
                   let () = unify ~handle:Gripers.handle_branches
                              (pos_and_typ body, no_pos bt)
                   in
                   let vs = Env.domain (pattern_env pat) in
                   let vs' = Env.domain henv.var_env in
                   let us = StringMap.filter (fun v _ -> not (StringSet.mem v vs || StringSet.mem v vs')) (usages body) in
                   let () =
                     let (_,_,pos') = SourceCode.resolve_pos @@ (fst3 kpat).pos in
                     let kt = TypeUtils.return_type (pattern_typ kpat) in
                     match descr.shd_depth with
                     | `Deep ->
                        let eff = context.effect_row in
                        unify ~handle:Gripers.deep_resumption
                          ((pos', kt), no_pos bt);
                        unify ~handle:Gripers.deep_resumption_effects
                          ((pos', `Effect eff), no_pos (`Effect outer_eff))
                     | `Shallow ->
                        let eff = TypeUtils.effect_row (pattern_typ kpat) in
                        unify ~handle:Gripers.shallow_resumption
                          ((pos', kt), no_pos rt);
                        unify ~handle:Gripers.shallow_resumption_effects
                          ((pos', `Effect eff), no_pos (`Effect inner_eff))
                   in
                   (pat, kpat, update_usages body us) :: cases)
                 eff_cases []
             in
             (val_cases, rt), eff_cases, bt, inner_eff, outer_eff
           in
           (** make_operations_presence_polymorphic makes the operations in the given row polymorphic in their presence *)
           let make_operations_presence_polymorphic : Types.row -> Types.row
	     = fun row ->
             let (operations, rho, dual) = row in
	     let operations' =
               StringMap.mapi
                 (fun name p ->
                   if TypeUtils.is_builtin_effect name
                   then p
                   else Types.fresh_presence_variable (`Unl, `Any)) (* It is questionable whether it is ever correct to
                                                                       make absent operations polymorphic in their presence. *)
                 operations
             in
	     (operations', rho, dual)
           in
           let m_context = { context with effect_row = Types.make_empty_open_row (`Unl, `Any) } in
           let m = type_check m_context m in (* Type-check the input computation m under current context *)
           let m_effects = `Effect m_context.effect_row in
           (** Most of the work is done by `type_cases'. *)
           let (val_cases, eff_cases) =
             (** The following is a slight hack until I get rid of the
                 `handler' sugar. It is necessary because of "old
                 fashioned" parameterised handlers. *)
             match val_cases with
             | [] -> split_handler_cases eff_cases
             | _  -> val_cases, eff_cases
           in
           let (val_cases, rt), eff_cases, body_type, inner_eff, outer_eff = type_cases val_cases eff_cases in
           (* Printf.printf "result: %s\ninner_eff: %s\nouter_eff: %s\n%!" (Types.string_of_datatype rt) (Types.string_of_row inner_eff) (Types.string_of_row outer_eff); *)
           (** Patch the result type of `m' *)
           let () =
              unify ~handle:Gripers.handle_return (pos_and_typ m, no_pos rt)
           in
           (** Finalise construction of the effect row of the input computation *)
           let inner_eff, outer_eff =
             let m_pos = exp_pos m in
             let () = unify ~handle:Gripers.handle_comp_effects ((m_pos, m_effects), no_pos (`Effect inner_eff)) in
             let inner_eff' = make_operations_presence_polymorphic inner_eff in
             (* Printf.printf "inner_eff': %s\n%!" (Types.string_of_row inner_eff'); *)
             let () = unify ~handle:Gripers.handle_unify_with_context (no_pos (`Effect inner_eff'), no_pos (`Effect outer_eff)) in
             let () = unify ~handle:Gripers.handle_unify_with_context (no_pos (`Effect outer_eff), no_pos (`Effect context.effect_row)) in
             inner_eff, outer_eff
           in
           let eff_cases =
             List.map (fun (p, _, body) -> (p, body)) eff_cases
           in
           (* Printf.printf "result: %s\ninner_eff: %s\nouter_eff: %s\n%!" (Types.string_of_datatype rt) (Types.string_of_row inner_eff) (Types.string_of_row outer_eff); *)
           (***)
           let descr = { descr with
                         shd_types = (Types.flatten_row inner_eff, typ m, Types.flatten_row outer_eff, body_type);
                         shd_raw_row = Types.make_empty_closed_row (); }
           in
           `Handle { sh_expr = erase m;
                     sh_effect_cases = erase_cases eff_cases;
                     sh_value_cases = erase_cases val_cases;
                     sh_descr = descr }, body_type, merge_usages [usage_compat (List.map (fun ((_, _, m),_) -> m) params); usages m; usages_cases eff_cases; usages_cases val_cases]
        | `DoOperation (opname, args, _) ->
           (* Strategy:
              1. List.map tc args
              2. Construct operation type
              3. Construct effect row where the operation name gets bound to the previously constructed operation type
              4. Unify with current effect context
           *)
           if String.compare opname "Return" = 0 then
	     Gripers.die pos "The implicit effect Return is not invocable"
           else
	     let (row, return_type, args) =
	       let ps     = List.map tc args in
	       let inp_t  = List.map typ ps in
	       let out_t  = Types.fresh_type_variable (`Unl, `Any) in
	       let optype = Types.make_pure_function_type inp_t out_t in
               let effrow = Types.make_singleton_open_row (opname, `Present optype) (`Unl, `Effect) in
	       (effrow, out_t, ps)
	     in
	     let (_,_,p) = SourceCode.resolve_pos pos in
	     let () = unify ~handle:Gripers.do_operation
	       (no_pos (`Effect context.effect_row), (p, `Effect row))
	     in
             (`DoOperation (opname, List.map erase args, Some return_type), return_type, StringMap.empty)
        | `Switch (e, binders, _) ->
            let e = tc e in
            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Gripers.switch_pattern (pos_and_typ e, no_pos pattern_type) in
              `Switch (erase e, erase_cases binders, Some body_type), body_type, merge_usages [usages e; usages_cases binders]
        | `TryInOtherwise (try_phrase, pat, in_phrase, unless_phrase, _) ->
            let try_phrase = tc try_phrase in

            (* Pattern type variable *)
            let pat = tpc pat in

            (* Check whether pattern corresponds to try_phrase *)
            let () =
              unify ~handle:Gripers.try_in_unless_pat
                (ppos_and_typ pat, (exp_pos try_phrase, (typ try_phrase))) in

            let in_context = context ++ (pattern_env pat) in
            let in_phrase = type_check in_context in_phrase in

            (* Pattern we have just bound should either be used, or should be
             * able to be made unrestricted *)
            let () =
              Env.iter (fun v t ->
                let uses = uses_of v (usages in_phrase) in
                if uses <> 1 then
                  if Types.type_can_be_unl t then
                    Types.make_type_unl t
                  else
                    Gripers.non_linearity pos uses v t) (pattern_env pat) in

            (* vs: variables bound in the pattern. *)
            let vs = Env.domain (pattern_env pat) in

            let unless_phrase = tc unless_phrase in
            unify ~handle:Gripers.try_in_unless_branches
                (pos_and_typ in_phrase, pos_and_typ unless_phrase);

            (* in_usages: usages in the in_phrase *not* bound in the pattern *)
            let in_usages = StringMap.filter (fun v _ -> not (StringSet.mem v vs)) (usages in_phrase) in

            (* Now, we need to ensure that all variables used in the in- and unless-
             * phrases are unrestricted (apaart from the pattern variables!) *)
            let () =
              StringMap.iter (fun v n ->
                if n == 0 then () else
                if Env.has (pattern_env pat) v then () else
                  let ty = Env.lookup context.var_env v in
                  if Types.type_can_be_unl ty then
                    Types.make_type_unl ty
                  else
                    Gripers.try_in_unless_linearity pos v
              ) (usages in_phrase) in

            let () =
              StringMap.iter (fun v n ->
                if n == 0 then () else
                let ty = Env.lookup context.var_env v in
                  if Types.type_can_be_unl ty then
                    Types.make_type_unl ty
                  else
                    Gripers.try_in_unless_linearity pos v
              ) (usages unless_phrase) in


            (* Calculate resulting usages *)
            let usages_res =
              merge_usages [usages try_phrase;
              (usage_compat [in_usages; (usages unless_phrase)])] in

            let return_type = typ in_phrase in

            `TryInOtherwise
              (erase try_phrase, erase_pat pat, erase in_phrase,
                erase unless_phrase, Some return_type), return_type, usages_res
        | `QualifiedVar _ -> assert false
        | `Raise -> (`Raise, Types.fresh_type_variable (`Any, `Any), StringMap.empty)
    in with_pos pos e, t, usages

(** [type_binding] takes XXX YYY (FIXME)
    The input context is the environment in which to type the bindings.

    The output context is the environment resulting from typing the
    bindings.  It does not include the input context.

    The result includes the introduced bindings, along with the variable
    usage map from the binder's body.
 *)
and type_binding : context -> binding -> binding * context * usagemap =
  fun context {node = def; pos} ->
    let type_check = type_check in
    let unify pos (l, r) = unify_or_raise ~pos:pos (l, r)
    and typ (_,t,_) = t
    and erase (e, _, _) = e
    and usages (_,_,u) = u
    and erase_pat (e, _, _) = e
    and pattern_typ (_, _, t) = t
    and tc = type_check context
    and tpc = type_pattern `Closed
    and pattern_env (_, e, _) = e
    and (++) ctxt env' = {ctxt with var_env = Env.extend ctxt.var_env env'} in
    let _UNKNOWN_POS_ = "<unknown>" in
    let no_pos t = (_UNKNOWN_POS_, t) in
    let pattern_pos ({pos=p;_},_,_) = let (_,_,p) = SourceCode.resolve_pos p in p in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos p = let (_,_,p) = SourceCode.resolve_pos p.pos in p in
    let exp_pos (p,_,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in

    let empty_context = empty_context (context.Types.effect_row) in

    let typed, ctxt, usage = match def with
      | `Val (pat, (_, body), location, datatype) ->
          let body = tc body in
          let pat = tpc pat in
          let penv = pattern_env pat in
          let bt =
            match datatype with
              | Some (_, Some t) ->
                  unify pos ~handle:Gripers.bind_val_annotation (no_pos (typ body), no_pos t);
                  t
              | _ -> typ body in
          let () = unify pos ~handle:Gripers.bind_val (ppos_and_typ pat, (exp_pos body, bt)) in
          let usage = usages body in
          let body = erase body in
          let tyvars, pat, penv =
            if Utils.is_generalisable body then
              let penv = Env.map (snd -<- Utils.generalise context.var_env) penv in
              let pat = update_pattern_vars penv (erase_pat pat) in
              let ((tyvars, _), _bt) = Utils.generalise context.var_env bt in
              tyvars, pat, penv
            else
              (* All rigid type variables in bt should appear in the
                 environment *)
              let tyvars = Generalise.get_quantifiers context.var_env bt in
              if List.exists Types.is_rigid_quantifier tyvars
              then
                Gripers.value_restriction pos bt
              else
                [], erase_pat pat, penv
          in
            `Val (pat, (tyvars, body), location, datatype),
            {empty_context with
              var_env = penv},
            usage
      | `Fun (bndr, lin, (_, (pats, body)), location, t) ->
          let name = name_of_binder bndr in
          let vs = name :: check_for_duplicate_names pos (List.flatten pats) in
          let pats = List.map (List.map tpc) pats in

          let effects = Types.make_empty_open_row (`Any, `Any) in
          let return_type = Types.fresh_type_variable (`Any, `Any) in

          (** Check that any annotation matches the shape of the function *)
          let context', ft =
            match t with
              | None ->
                  context, make_ft lin pats effects return_type
              | Some (_, Some ft) ->
                  (* Debug.print ("ft: " ^ Types.string_of_datatype ft); *)
                  (* make sure the annotation has the right shape *)
                  let shape = make_ft lin pats effects return_type in
                  let _, fti = Instantiate.typ_rigid ft in
                  (* Debug.print ("fti: " ^ Types.string_of_datatype fti); *)
                  let () = unify pos ~handle:Gripers.bind_fun_annotation (no_pos shape, no_pos fti) in
                    (* Debug.print ("return type: " ^Types.string_of_datatype (TypeUtils.concrete_type return_type)); *)
                  (* HACK: Place a dummy name in the environment in
                     order to ensure that the generalisation check
                     does the right thing (it would be unsound to use
                     the original name as the function is not
                     recursive) *)
                  let v = Utils.dummy_source_name () in
                  bind_var context (v, fti), ft
              | Some _ -> assert false in

          (* type check the body *)
          let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in
          let context' = List.fold_left fold_in_envs context' pats in

          let body = type_check (bind_effects context' effects) body in

          (* check that the body type matches the return type of any annotation *)
          let () = unify pos ~handle:Gripers.bind_fun_return (no_pos (typ body), no_pos return_type) in

          (* check that the usage of bound variables agrees with the
             linearity of their types *)
          let () = List.iter
                     (fun pat ->
                      Env.iter (fun v t ->
                        let uses = uses_of v (usages body) in
                        if uses <> 1 then
                          if Types.type_can_be_unl t then
                            Types.make_type_unl t
                          else
                            Gripers.non_linearity pos uses v t)
                               (pattern_env pat))
                     (List.flatten pats) in

          let () =
            if lin = `Unl then
              StringMap.iter (fun v _ ->
                              if not (List.mem v vs) then
                                let t = Env.lookup context'.var_env v in
                                if Types.type_can_be_unl t then
                                  Types.make_type_unl t
                                else
                                  Gripers.die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^
                                                     " was used in a non-linear function definition"))
                             (usages body)
            else () in

          (* generalise*)
          let (tyvars, _tyargs), ft = Utils.generalise context.var_env ft in
          let ft = Instantiate.freshen_quantifiers ft in
            (`Fun (set_binder_type bndr ft,
                   lin,
                   (tyvars, (List.map (List.map erase_pat) pats, erase body)),
                   location, t),
             {empty_context with
                var_env = Env.bind Env.empty (name, ft)},
             StringMap.filter (fun v _ -> not (List.mem v vs)) (usages body))
      | `Funs defs ->
          (*
            Compute initial types for the functions using
            - the patterns
            - an optional type annotation

            Note that the only way of getting polymorphic recursion is
            to provide a type annotation (generalisation would
            otherwise be unsound as the function types cannot be fully
            known until the definition bodies have been inspected).

            As well as the function types, the typed patterns are also
            returned here as a simple optimisation.  *)

          let fresh_wild () = Types.make_singleton_open_row ("wild", (`Present Types.unit_type)) (`Any, `Any) in

          let inner_env, patss =
            List.fold_left
              (fun (inner_env, patss) (bndr, lin, (_, (pats, _body)), _, t, pos) ->
                 let name = name_of_binder bndr in
                 let _ = check_for_duplicate_names pos (List.flatten pats) in
                 let pats = List.map (List.map tpc) pats in
                 let inner =
                   match t with
                     | None ->
                         (* Here we're taking advantage of the
                            following equivalence to make the curried
                            arrows polymorphic:

                            fun f(x1)...(xk) {e}
                            ==
                            fun f(x1)...(xk) {
                            fun f(x1)...(xk) {e}
                            f(x1)...(xk)
                            }
                         *)
                         make_ft_poly_curry lin pats (fresh_wild ()) (Types.fresh_type_variable (`Any, `Any))
                     | Some (_, Some t) ->
                         (* Debug.print ("t: " ^ Types.string_of_datatype t); *)
                         let shape = make_ft lin pats (fresh_wild ()) (Types.fresh_type_variable (`Any, `Any)) in
                         let (_, ft) = Generalise.generalise_rigid context.var_env t in
                         (* Debug.print ("ft: " ^ Types.string_of_datatype ft); *)
                           (* make sure the annotation has the right shape *)
                         let _, fti = Instantiate.typ ft in
                         let () = unify pos ~handle:Gripers.bind_rec_annotation (no_pos shape, no_pos fti) in
                           ft
                     | Some _ -> assert false
                 in
                   Env.bind inner_env (name, inner), pats::patss)
              (Env.empty, []) defs in
          let patss = List.rev patss in

          (*
             type check the function bodies using
             - monomorphic bindings for unannotated functions
             - potentially polymorphic bindings for annotated functions
          *)
          let (defs, used) =
            let body_env = Env.extend context.var_env inner_env in
            (* let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in *)
            List.split
              (List.rev
                (List.fold_left2
                   (fun defs_and_uses (bndr, lin, (_, (_, body)), location, t, pos) pats ->
                      let name = name_of_binder bndr in
                      let pat_env = List.fold_left (fun env pat -> Env.extend env (pattern_env pat)) Env.empty (List.flatten pats) in
                      let context' = {context with var_env = Env.extend body_env pat_env} in
                      let effects = fresh_wild () in
                      let body = type_check (bind_effects context' effects) body in
                      let () =
                        Env.iter
                          (fun v t ->
                            let uses = uses_of v (usages body) in
                              if uses <> 1 then
                                begin
                                  if Types.type_can_be_unl t then
                                    Types.make_type_unl t
                                  else
                                    Gripers.non_linearity pos uses v t
                                end)
                          pat_env in
                      let used =
                        let vs = StringSet.add name (Env.domain pat_env) in
                        if lin=`Unl then
                          StringMap.iter (fun v _ ->
                                          if not (StringSet.mem v vs) then
                                            let t = Env.lookup context'.var_env v in
                                            if Types.type_can_be_unl t then
                                              Types.make_type_unl t
                                            else
                                              Gripers.die pos ("Use of variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^ " in unlimited function binding.")
                                          else ())
                                         (usages body);
                        StringMap.filter (fun v _ -> not (StringSet.mem v vs)) (usages body) in
                      let shape =
                        make_ft lin pats effects (typ body) in
                        (* it is important to instantiate with rigid
                           type variables in order to ensure that the
                           inferred type is consistent with any
                           annotation *)
                      let ft =
                        let _, ft = Instantiate.rigid context'.var_env name in
                          if Settings.get_value Instantiate.quantified_instantiation then
                            match ft with
                              | `ForAll (_, ft) -> ft
                              | ft -> ft
                          else
                            ft in
                      let () = unify pos ~handle:Gripers.bind_rec_rec (no_pos shape, no_pos ft) in
                      ((erase_binder_type bndr, lin, (([], None), (pats, body)), location, t, pos), used) :: defs_and_uses) [] defs patss)) in

          (* Generalise to obtain the outer types *)
          let defs, outer_env =
            let defs, outer_env =
              List.fold_left2
                (fun (defs, outer_env) (bndr, lin, (_, (_, body)), location, t, pos) pats ->
                   let name = name_of_binder bndr in
                   let inner = Env.lookup inner_env name in
                   let inner, outer, tyvars =
                     match inner with
                       | `ForAll (inner_tyvars, inner_body) ->
                           let (tyvars, _tyargs), outer = Utils.generalise context.var_env inner_body in
                           let extras =
                             let has q =
                               let n = Types.type_var_number q in
                                 List.exists (fun q -> Types.type_var_number q = n) (Types.unbox_quantifiers inner_tyvars)
                             in
                               List.map (fun q ->
                                           if has q then None
                                           else Some q) tyvars in
                           let outer = Instantiate.freshen_quantifiers outer in
                           let inner = Instantiate.freshen_quantifiers inner in
                             (inner, extras), outer, tyvars
                       | _ ->
                           let (tyvars, _tyargs), outer = Utils.generalise context.var_env inner in
                           let extras = List.map (fun q -> Some q) tyvars in
                           let outer = Instantiate.freshen_quantifiers outer in
                             (inner, extras), outer, tyvars in

                   let pats = List.map (List.map erase_pat) pats in
                   let body = erase body in
                     ((set_binder_type bndr outer, lin, ((tyvars, Some inner), (pats, body)), location, t, pos)::defs,
                      Env.bind outer_env (name, outer)))
                ([], Env.empty) defs patss
            in
              List.rev defs, outer_env in

          let defined = List.map (fun (bndr, _, _, _, _, _) -> name_of_binder bndr) defs

          in
            `Funs defs, {empty_context with var_env = outer_env}, (StringMap.filter (fun v _ -> not (List.mem v defined)) (merge_usages used))

      | `Foreign (bndr, raw_name, language, file, (dt1, Some datatype)) ->
          (* Ensure that we quantify FTVs *)
          let (_tyvars, _args), datatype = Utils.generalise context.var_env datatype in
          let datatype = Instantiate.freshen_quantifiers datatype in
          (`Foreign (set_binder_type bndr datatype, raw_name, language, file, (dt1, Some datatype)),
           (bind_var empty_context (name_of_binder bndr, datatype)),
           StringMap.empty)
      | `Foreign _ -> assert false
      | `Type (name, vars, (_, Some dt)) as t ->
          t, bind_tycon empty_context (name, `Alias (List.map (snd ->- val_of) vars, dt)), StringMap.empty
      | `Type _ -> assert false
      | `Infix -> `Infix, empty_context, StringMap.empty
      | `Exp e ->
          let e = tc e in
          let () = unify pos ~handle:Gripers.bind_exp
            (pos_and_typ e, no_pos Types.unit_type) in
          `Exp (erase e), empty_context, usages e
      | `Handler _
      | `QualifiedImport _
      | `AlienBlock _
      | `Module _ -> assert false
    in
      {node = typed; pos}, ctxt, usage
and type_regex typing_env : regex -> regex =
  fun m ->
    let erase (e, _, _) = e in
    let typ (_, t, _) = t in
    let no_pos t = ("<unknown>", t) in
    let tr = type_regex typing_env in
      match m with
        | (`Range _ | `Simply _ | `Any  | `StartAnchor | `EndAnchor) as r -> r
        | `Quote r -> `Quote (tr r)
        | `Seq rs -> `Seq (List.map tr rs)
        | `Alternate (r1, r2) -> `Alternate (tr r1, tr r2)
        | `Group r -> `Group (tr r)
        | `Repeat (repeat, r) -> `Repeat (repeat, tr r)
        | `Splice e ->
            let pos = e.pos in
            let e = type_check typing_env e in
            let () = unify_or_raise ~pos:pos ~handle:Gripers.splice_exp
              (no_pos (typ e), no_pos Types.string_type)
            in
              `Splice (erase e)
        | `Replace (r, `Literal s) -> `Replace (tr r, `Literal s)
        | `Replace (r, `Splice e) -> `Replace (tr r, `Splice (erase (type_check typing_env e)))
and type_bindings (globals : context)  bindings =
  let tyenv, (bindings, uinf) =
    List.fold_left
      (fun (ctxt, (bindings, uinf)) (binding : binding) ->
         let binding, ctxt', usage = type_binding (Types.extend_typing_environment globals ctxt) binding in
         let result_ctxt = Types.extend_typing_environment ctxt ctxt' in
         result_ctxt, (binding::bindings, (binding.pos,ctxt'.var_env,usage)::uinf))
      (empty_context globals.Types.effect_row, ([], [])) bindings in
  let usage_builder body_usage =
    List.fold_left (fun usages (pos,env,usage) ->
                    let vs = Env.domain env in
                    Env.iter
                      (fun v t ->
                        let uses = uses_of v usages in
                          if uses <> 1 then
                            if Types.type_can_be_unl t then
                              Types.make_type_unl t
                            else
                              Gripers.non_linearity pos uses v t)
                      env;
                    merge_usages [usage; StringMap.filter (fun v _ -> not (StringSet.mem v vs)) usages])
                   body_usage uinf
  in
    tyenv, List.rev bindings, usage_builder
and type_cp (context : context) = fun {node = p; pos} ->
  let with_channel = fun c s (p, t, u) ->
    if uses_of c u <> 1 then
      if Types.type_can_be_unl s then
        Types.make_type_unl s
      else
        Gripers.non_linearity pos (uses_of c u) c s;
    (p, t, StringMap.remove c u) in

  let use s u = StringMap.add s 1 u in

  let unify ~pos ~handle (t, u) = unify_or_raise ~pos:pos ~handle:handle (("<unknown>", t), ("<unknown>", u)) in

  let (p, t, u) = match p with
    | `Unquote (bindings, e) ->
       let context', bindings, usage_builder = type_bindings context bindings in
       let (e, t, u) = type_check (Types.extend_typing_environment context context') e in
         if Settings.get_value endbang_antiquotes then
           unify ~pos:pos ~handle:Gripers.cp_unquote (t, Types.make_endbang_type);
         `Unquote (bindings, e), t, usage_builder u
    | `Grab ((c, _), None, p) ->
       let (_, t, _) = type_check context (with_pos pos (`Var c)) in
       let ctype = `Alias (("EndQuery", []), `Input (Types.unit_type, `End)) in
       unify ~pos:pos ~handle:(Gripers.cp_grab c) (t, ctype);
       let (p, pt, u) = type_cp (unbind_var context c) p in
       `Grab ((c, Some (ctype, [])), None, p), pt, use c u
    | `Grab ((c, _), Some bndr, p) ->
       let x = name_of_binder bndr in
       let (_, t, _) = type_check context (with_pos pos (`Var c)) in
       let a = Types.fresh_type_variable (`Any, `Any) in
       let s = Types.fresh_session_variable `Any in
       let ctype = `Input (a, s) in
       unify ~pos:pos ~handle:(Gripers.cp_grab c)
             (t, ctype);
       let (p, pt, u) = with_channel c s (type_cp (bind_var (bind_var context (c, s)) (x, a)) p) in
       let uses = uses_of x u in
       if uses <> 1 then
         if Types.type_can_be_unl a then
           Types.make_type_unl a
         else
           Gripers.non_linearity pos uses x a;
       let (_, grab_ty, _) = type_check context (with_pos pos (`Var "receive")) in
       let tyargs =
         match Types.concrete_type grab_ty with
         | `ForAll (qs, _t) ->
            let xs = List.map (fun q -> q, Types.freshen_quantifier_flexible q) (Types.unbox_quantifiers qs) in
            let tyargs = (snd -<- List.split -<- snd -<- List.split) xs in
            begin
              match Instantiate.apply_type grab_ty tyargs with
              | `Function (fps, _fe, _rettype) ->
                 unify ~pos:pos ~handle:(Gripers.cp_grab "") (Types.make_tuple_type [ctype], fps);
                 tyargs
              | _ -> assert false
            end
         | _ -> assert false in
       `Grab ((c, Some (ctype, tyargs)), Some (set_binder_type bndr a), p), pt, use c (StringMap.remove x u)
    | `Give ((c, _), None, p) ->
       let (_, t, _) = type_check context (with_pos pos (`Var c)) in
       let ctype = `Output (Types.unit_type, `End) in
       unify ~pos:pos ~handle:(Gripers.cp_give c) (t, ctype);
       let (p, t, u) = type_cp (unbind_var context c) p in
       `Give ((c, Some (ctype, [])), None, p), t, use c u
    | `Give ((c, _), Some e, p) ->
       let (_, t, _) = type_check context (with_pos pos (`Var c)) in
       let (e, t', u) = type_check context e in
       let s = Types.fresh_session_variable `Any in
       let ctype = `Output (t', s) in
       unify ~pos:pos ~handle:(Gripers.cp_give c)
             (t, ctype);
       let (p, t, u') = with_channel c s (type_cp (bind_var context (c, s)) p) in

       let (_, give_ty, _) = type_check context (with_pos pos (`Var "send")) in
       let tyargs =
         match Types.concrete_type give_ty with
         | `ForAll (qs, _t) ->
            let xs = List.map (fun q -> q, Types.freshen_quantifier_flexible q) (Types.unbox_quantifiers qs) in
            let tyargs = (snd -<- List.split -<- snd -<- List.split) xs in
            begin
              match Instantiate.apply_type give_ty tyargs with
              | `Function (fps, _fe, _rettpe) ->
                 unify ~pos:pos ~handle:(Gripers.cp_give "") (Types.make_tuple_type [t'; ctype], fps);
                 tyargs
              | _ -> assert false
            end
         | _ -> assert false in
       `Give ((c, Some (ctype, tyargs)), Some e, p), t, use c (merge_usages [u; u'])
    | `GiveNothing bndr ->
       let c = name_of_binder bndr in
       let binder_pos = bndr.pos in
       let _, t, _ = type_check context (with_pos binder_pos (`Var c)) in
       unify ~pos:pos ~handle:Gripers.(cp_give c) (t, Types.make_endbang_type);
       `GiveNothing (set_binder_type bndr t), t, StringMap.singleton c 1
    | `Select (bndr, label, p) ->
       let c = name_of_binder bndr in
       let (_, t, _) = type_check context (with_pos pos  (`Var c)) in
       let s = Types.fresh_session_variable `Any in
       let r = Types.make_singleton_open_row (label, `Present s) (`Any, `Session) in
       let ctype = `Select r in
       unify ~pos:pos ~handle:(Gripers.cp_select c)
             (t, ctype);
       let (p, t, u) = with_channel c s (type_cp (bind_var context (c, s)) p) in
       `Select (set_binder_type bndr ctype, label, p), t, use c u
    | `Offer (bndr, branches) ->
       let c = name_of_binder bndr in
       let (_, t, _) = type_check context (with_pos pos (`Var c)) in
       (*
       let crow = Types.make_empty_open_row (`Any, `Session) in
       let ctype = `Choice crow in
       unify ~pos:pos ~handle:(Gripers.cp_offer_choice c)
             (t, ctype);
        *)
       let check_branch (label, body) =
         let s = Types.fresh_type_variable (`Any, `Session) in
         let r = Types.make_singleton_open_row (label, `Present s) (`Any, `Session) in
         unify ~pos:pos ~handle:(Gripers.cp_offer_choice c) (t, `Choice r);
         let (p, t, u) = with_channel c s (type_cp (bind_var context (c, s)) body) in
         (label, p), t, u in
       let branches = List.map check_branch branches in
       let t' = Types.fresh_type_variable (`Any, `Any) in
       List.iter (fun (_, t, _) -> unify ~pos:pos ~handle:Gripers.cp_offer_branches (t, t')) branches;
       let u = usage_compat (List.map (fun (_, _, u) -> u) branches) in
       `Offer (set_binder_type bndr t, List.map (fun (x, _, _) -> x) branches), t', use c u
    | `Link (bndr1, bndr2) ->
      let c = name_of_binder bndr1 in
      let d = name_of_binder bndr2 in
      let (_, tc, uc) = type_check context (with_pos pos (`Var c)) in
      let (_, td, ud) = type_check context (with_pos pos (`Var d)) in
        unify ~pos:pos ~handle:Gripers.cp_link_session
          (tc, Types.fresh_type_variable (`Any, `Session));
        unify ~pos:pos ~handle:Gripers.cp_link_session
          (td, Types.fresh_type_variable (`Any, `Session));
        unify ~pos:pos ~handle:Gripers.cp_link_dual (Types.dual_type tc, td);
        `Link (set_binder_type bndr1 tc, set_binder_type bndr1 td), Types.make_endbang_type, merge_usages [uc; ud]
    | `Comp (bndr, left, right) ->
       let c = name_of_binder bndr in
       let s = Types.fresh_session_variable `Any in
       let left, t, u = with_channel c s (type_cp (bind_var context (c, s)) left) in
       let right, t', u' = with_channel c (`Dual s) (type_cp (bind_var context (c, `Dual s)) right) in
       unify ~pos:pos ~handle:Gripers.cp_comp_left (Types.make_endbang_type, t);
       `Comp (set_binder_type bndr s, left, right), t', merge_usages [u; u'] in
  {node = p; pos}, t, u

let show_pre_sugar_typing = Basicsettings.TypeSugar.show_pre_sugar_typing

let binding_purity_check bindings =
  List.iter (fun ({pos;_} as b) ->
               if not (Utils.is_pure_binding b) then
                 Gripers.toplevel_purity_restriction pos b)
    bindings

module Check =
struct
  let program tyenv (bindings, body) =
    try
      Debug.if_set show_pre_sugar_typing
        (fun () ->
           "before type checking: \n"^ show_program (bindings, body));
      let tyenv', bindings, _ = type_bindings tyenv bindings in
      let tyenv' = Types.normalise_typing_environment tyenv' in
        if Settings.get_value check_top_level_purity then
          binding_purity_check bindings; (* TBD: do this only in web mode? *)
        match body with
          | None -> (bindings, None), Types.unit_type, tyenv'
          | Some body ->
              let body, typ, _ = type_check (Types.extend_typing_environment tyenv tyenv') body in
              let typ = Types.normalise_datatype typ in
                (bindings, Some body), typ, tyenv'
    with
        Unify.Failure (`Msg msg) -> failwith msg

  let sentence tyenv sentence =
    Debug.if_set show_pre_sugar_typing
      (fun () ->
         "before type checking: \n"^ show_sentence sentence);
    match sentence with
      | `Definitions bindings ->
          let tyenv', bindings, _ = type_bindings tyenv bindings in
          let tyenv' = Types.normalise_typing_environment tyenv' in
            `Definitions bindings, Types.unit_type, tyenv'
      | `Expression body ->
          let body, t, _ = (type_check tyenv body) in
          let t = Types.normalise_datatype t in
            `Expression body, t, tyenv
      | `Directive d -> `Directive d, Types.unit_type, tyenv
end
