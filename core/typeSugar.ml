open CommonTypes
open Utility
open Operators
open Sugartypes
open SugarConstructors.SugartypesPositions
open SourceCode
open SourceCode.WithPos

let internal_error message =
  Errors.internal_error ~filename:"typeSugar.ml" ~message

let relational_lenses_guard pos =
  let relational_lenses_disabled pos =
    Errors.disabled_extension ~pos ~setting:("relational_lenses", true) "Relational lenses"
  in
  if not (Settings.get Lens.relational_lenses)
  then raise (relational_lenses_disabled pos)

let endbang_antiquotes
  = Settings.(flag "endbang_antiquotes"
              |> convert parse_bool
              |> sync)

let check_top_level_purity
  = Settings.(flag "check_top_level_purity"
              |> convert parse_bool
              |> sync)

let show_pre_sugar_typing
  = Settings.(flag "show_pre_sugar_typing"
              |> convert parse_bool
              |> sync)

let show_post_sugar_typing
  = Settings.(flag "show_post_sugar_typing"
              |> convert parse_bool
              |> sync)

let dodgey_type_isomorphism
  = Settings.(flag "dodgey_type_isomorphism"
              |> convert parse_bool
              |> sync)

let generalise_toplevel
  = Settings.(flag ~default:true "generalise_toplevel"
              |> convert parse_bool
              |> sync)

module Env = Env.String

module Utils : sig
  val dummy_source_name : unit -> Name.t
  val unify : Types.datatype * Types.datatype -> unit
  val instantiate : Types.environment -> string ->
                    (Types.type_arg list * Types.datatype)
  val generalise : ?unwrap:bool -> Types.environment -> Types.datatype ->
                   ((Quantifier.t list*Types.type_arg list) * Types.datatype)

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
    | QualifiedVar _
    | Constant _
    | Var _
    | FreezeVar _
    | FunLit _
    | DatabaseLit _
    | TableLit _
    | TextNode _
    | Section _
    | FreezeSection _ -> true

    | ListLit (ps, _)
    | TupleLit ps -> List.for_all is_pure ps
    | RangeLit (e1, e2) -> is_pure e1 && is_pure e2
    | TAbstr (_, p)
    | TAppl (p, _)
    | Projection (p, _)
    | TypeAnnotation (p, _)
    | Upcast (p, _, _)
    | Instantiate p
    | Generalise p
    | Linlet p
    | Unlet p
    | Escape (_, p) -> is_pure p
    | ConstructorLit (_, p, _) -> opt_generalisable p
    | RecordLit (fields, p) ->
       List.for_all (snd ->- is_pure) fields && opt_generalisable p
    | With (p, fields) ->
       List.for_all (snd ->- is_pure) fields && is_pure p
    | Block (bindings, e) ->
       List.for_all is_pure_binding bindings && is_pure e
    | Conditional (p1, p2, p3) ->
        is_pure p1
     && is_pure p2
     && is_pure p3
    | Xml (_, attrs, attrexp, children) ->
        List.for_all (snd ->- List.for_all is_pure) attrs
     && opt_generalisable attrexp
     && List.for_all (is_pure) children
    | Formlet (p1, p2) ->
        is_pure p1 && is_pure p2
    | Regex r -> is_pure_regex r
    | Iteration _ (* could do a little better in some of these cases *)
    | Page _
    | FormletPlacement _
    | PagePlacement _
    | UnaryAppl _
    | FormBinding _
    | InfixAppl _
    | Spawn _
    | Query _
    | FnAppl _
    | Handle _
    | Switch _
    | Receive _
    | Select _
    | Offer _
    | CP _
    | LensLit _
    | LensSerialLit _
    | LensKeysLit _
    | LensFunDepsLit _
    | LensDropLit _
    | LensSelectLit _
    | LensJoinLit _
    | LensCheckLit _
    | LensGetLit _
    | LensPutLit _
    | DoOperation _
    | Operation _
    | DBDelete _
    | DBInsert _
    | TryInOtherwise _
    | Raise
    | DBTemporalJoin _
    | DBUpdate _ -> false
  and is_pure_binding ({node ; _ }: binding) = match node with
      (* need to check that pattern matching cannot fail *)
    | Import _
    | Open _
    | AlienBlock _
    | Module _
    | Fun _
    | Funs _
    | Infix _
    | Aliases _
    | Foreign _ -> true
    | Exp p -> is_pure p
    | Val (pat, (_, rhs), _, _) ->
        is_safe_pattern pat && is_pure rhs
  and is_safe_pattern {node = pat; _} = let open Pattern in
    match pat with
      (* safe patterns cannot fail *)
    | Nil
    | Cons _
    | List _
    | Constant _ -> false
    (* NOTE: variant assigment is typed such that it must always succeed *)
    | Variant (_, None) -> true
    | Variant (_, Some p) -> is_safe_pattern p
    | Negative _ -> true
    | Any
    | Variable _ -> true
    | Record (ps, None) -> List.for_all (snd ->- is_safe_pattern) ps
    | Record (ps, Some p) -> List.for_all (snd ->- is_safe_pattern) ps && is_safe_pattern p
    | Tuple ps -> List.for_all is_safe_pattern ps
    | HasType (p, _)
    | As (_, p) -> is_safe_pattern p
    | Operation (_, ps, k, _) ->
       List.for_all is_safe_pattern ps && is_safe_pattern k
  and is_pure_regex = function
      (* don't check whether it can fail; just check whether it
         contains non-generilisable sub-expressions *)
    | Range _
    | Simply _
    | Any
    | StartAnchor
    | EndAnchor -> true
    | Group r
    | Repeat (_, r)
    | Quote r -> is_pure_regex r
    | Seq rs -> List.for_all is_pure_regex rs
    | Alternate (r1, r2) -> is_pure_regex r1 && is_pure_regex r2
    | Splice p -> is_pure p
    | Replace (r, Literal _) -> is_pure_regex r
    | Replace (r, SpliceExpr p) -> is_pure_regex r && is_pure p

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
      pos:Position.t ->
      t1:(string * Types.datatype) ->
      t2:(string * Types.datatype) ->
      error:Unify.error ->
      unit

  val die : Position.t -> string -> 'a

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

  val try_effect : griper

  val extend_record : griper
  val record_with : griper

  val lens_predicate : griper
  val lens_put_input : griper

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
  val sequenced_insert_values : griper
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
  val range_wild : griper

  val spawn_outer : griper
  val spawn_wait_outer : griper
  val spawn_location : griper
  val spawn_inconsistent : griper

  val query_outer : griper
  val query_base_row : griper

  val receive_mailbox : griper
  val receive_patterns : griper

  val unary_apply : griper
  val infix_apply : griper
  val fun_apply : griper
  val type_apply : Position.t -> string -> Types.datatype -> Types.type_arg list -> 'a

  val generalise_value_restriction : Position.t -> string -> 'a

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

  val iteration_unl_effect : griper
  val iteration_ambient_effect : griper
  val iteration_list_body : griper
  val iteration_list_pattern : griper
  val iteration_table_body : griper
  val iteration_table_pattern : Temporality.t -> griper
  val iteration_body : griper
  val iteration_where : griper
  val iteration_base_order : griper

  val escape : griper
  val escape_outer : griper

  val projection : griper

  val upcast_source : griper
  val upcast_subtype : Position.t -> Types.datatype -> Types.datatype -> 'a

  val value_restriction : Position.t -> Types.datatype -> 'a

  val toplevel_purity_restriction : Position.t -> Sugartypes.binding -> 'a

  val duplicate_names_in_pattern : Position.t -> 'a

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

  val cp_wild : griper
  val cp_unquote : griper
  val cp_grab : string -> griper
  val cp_give : string -> griper
  val cp_select : string -> griper
  val cp_offer_choice : string -> griper
  val cp_offer_branches : griper
  val cp_comp_left : griper
  val cp_link_session : griper
  val cp_link_dual : griper

  val non_linearity : Position.t -> int -> string -> Types.datatype -> unit
  val linear_recursive_function : Position.t -> string -> unit
  val linear_vars_in_deep_handler : Position.t -> string -> Types.datatype -> unit

  val try_in_unless_pat : griper
  val try_in_unless_branches : griper
  val try_in_unless_linearity : Position.t -> string -> unit

  val raise_effect : griper

  val recursive_usage : griper

  val inconsistent_quantifiers :
    pos:Position.t -> t1:Types.datatype -> t2:Types.datatype -> unit

  val tabstr_ambiguous_type : pos:Position.t -> Types.datatype -> unit

  val escaped_quantifier :
    pos:Position.t ->
    var:string ->
    annotation:Types.datatype ->
    escapees:((string * Types.datatype) list) ->
    unit

  val temporal_join_effects : griper
  val temporal_join_body : griper
  val valid_update_pattern : griper
  val sequenced_update_datetime : griper
  val nonsequenced_update_datetime : griper

end
  = struct
    type griper =
        pos:Position.t ->
      t1:(string * Types.datatype) ->
      t2:(string * Types.datatype) ->
      error:Unify.error ->
      unit

    let wm () = Settings.get  Webserver_types.webs_running

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
    let error_policy () = Types.Policy.set_hide_fresh false (Types.Policy.default_policy ())

    (* Do not automatically refresh type variable names when pretty-printing
       types in error messages.  This will be done manually by calling
       build_tyvar_names in the gripers.
       See Notes [Variable names in error messages] and [Refreshing type variable names] *)
    let show_type   = Types.string_of_datatype ~policy:error_policy ~refresh_tyvar_names:false
    let show_row    = Types.string_of_row      ~policy:error_policy ~refresh_tyvar_names:false
    let show_effectrow row = "{" ^ (Types.string_of_row ~policy:error_policy ~refresh_tyvar_names:false row) ^ "}"
    let show_type_arg = Types.string_of_type_arg ~policy:error_policy ~refresh_tyvar_names:false

    (* Wrappers for generating type variable names *)
    let build_tyvar_names =
      Types.build_tyvar_names ~refresh_tyvar_names:true Types.free_bound_type_vars
    let add_rowvar_names =
      Types.add_tyvar_names Types.free_bound_row_type_vars
    let add_typearg_names =
      Types.add_tyvar_names Types.free_bound_type_arg_type_vars

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

    let try_effect ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("Catching a session exception requires an effect context " ^ nli() ^
                code (show_effectrow (TypeUtils.extract_row rt)) ^ nl() ^
               "but, the currently allowed effects are" ^ nli() ^
                code ( show_effectrow (TypeUtils.extract_row lt)))

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

    let lens_predicate ~pos ~t1 ~t2 ~error:_ =
      build_tyvar_names [snd t1; snd t2];
      Format.asprintf
        "The predicate\n\
        \  %s\n
         has type\n\
        \  %s\n
         but the lens\n\
        \  %s\n
         expected a predicate of type\n\
        \  %s"
        (fst t1 |> code)
        (snd t1 |> show_type)
        (fst t2 |> code)
        (snd t2 |> show_type)
      |> die pos

    let lens_put_input ~pos ~t1 ~t2 ~error:_ =
      build_tyvar_names [snd t1; snd t2];
      Format.asprintf
        "The put value \n\
        \  %s\n
         has type\n\
        \  %s\n
         but the lens\n\
        \  %s\n
         expected an input type\n\
        \  %s"
        (fst t1 |> code)
        (snd t1 |> show_type)
        (fst t2 |> code)
        (snd t2 |> show_type)
      |> die pos


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


    let sequenced_insert_values ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_lt = show_type lt in
      let ppr_rt = show_type rt in
      die pos ("Valid time insertions require values matching the table " ^
               "in an insert expression, associated with validity periods," ^
               "but the values"               ^ nli () ^
                code lexpr                    ^ nl  () ^
               "have type"                    ^ nli () ^
                code ppr_lt                   ^ nl  () ^
               "while values of type"         ^ nli () ^
                code ppr_rt                   ^ nl  () ^
               "were expected."               ^ nl  () ^
                "Hint: try using"             ^ nli () ^
                "withValidity(x, from, to)")

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

    let range_wild ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Ranges are wild"                       ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

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

    let spawn_inconsistent  ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_pr = show_type rt in
      let ppr_ir = show_type lt in
      die pos ("Spawn blocks has inconsistent effects. Previously" ^ nli () ^
                code ppr_ir                             ^ nl  () ^
               "but the currently inferred effects are" ^ nli () ^
                code ppr_pr)

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
      with_but pos ("Flat query blocks must return a list of records of base type") (lexpr, lt)

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

    let generalise_value_restriction pos exp =
      die pos ("Because of the value restriction, the expression " ^ nli () ^
                code exp ^ nl () ^
               "cannot be generalised.")

    let type_apply pos lexpr ty tys =
      build_tyvar_names [ty];
      add_typearg_names tys;
      let quant_policy () = Types.Policy.set_quantifiers true (error_policy ()) in
      let ppr_type  = Types.string_of_datatype ~policy:quant_policy ~refresh_tyvar_names:false ty in
      let ppr_types = List.map (fun t -> tab() ^ code (show_type_arg t)) tys in
      die pos ("The term"                                     ^ nli () ^
                code lexpr                                    ^ nl  () ^
               "has type"                                     ^ nli () ^
                code ppr_type                                 ^ nl  () ^
               "while we're trying to instantiate it with"    ^ nl  () ^
                String.concat (nl() ^ "and" ^ nl()) ppr_types ^ nl  ())

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

    let iteration_unl_effect ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Iterations effects cannot be linear" ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let iteration_ambient_effect ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Iterations over tables are only allowed in tame contexts." ^ nli () ^
               "This iteration has ambient effect"     ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

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

    let iteration_table_pattern tmp ~pos ~t1:l ~t2:(rexpr,rt) ~error:_ =
      build_tyvar_names [snd l; rt];
      let rt = Types.make_table_type
                 (tmp, rt, Types.fresh_type_variable (lin_any, res_any),
                  Types.fresh_type_variable (lin_any, res_any)) in
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
          let expr = Position.Resolved.resolve pos |> Position.Resolved.source_expression in
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

    let cp_wild ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("CP clauses are wild"             ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

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

    let linear_recursive_function pos f =
      die pos ("Recursive function " ^ f ^ " cannot be linear.")

    let linear_vars_in_deep_handler pos v t =
      die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^ " is used in a deep handler.")

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

    let raise_effect ~pos ~t1:(_,lt) ~t2:(_,rt) ~error:_ =
      build_tyvar_names [lt;rt];
      die pos ("Raising a session exception requires an effect context " ^ nl() ^
          tab() ^ code (show_effectrow (TypeUtils.extract_row rt)) ^ nl() ^
          "but, the currently allowed effects are" ^ nl() ^
          tab() ^ code ( show_effectrow (TypeUtils.extract_row lt)))

   (* quantifier checks *)
   let inconsistent_quantifiers ~pos ~t1:l ~t2:r =
     let policy () = Types.Policy.set_quantifiers true (Types.Policy.default_policy ()) in
     let typ = Types.string_of_datatype ~policy in
       die pos ("Inconsistent quantifiers, expected: " ^ nli () ^
                typ l                                  ^ nl ()  ^
                "actual: "                             ^ nli () ^
                typ r                                  ^ nl ())

    let tabstr_ambiguous_type ~pos t =
      let policy () = Types.Policy.set_quantifiers true (Types.Policy.default_policy ()) in
      let typ = Types.string_of_datatype ~policy t in
      die pos ("The phrase under a type abstraction must have a unique type."     ^ nl ()  ^
               "We cannot guarantee this for the phrase under this /\\ with type" ^ nli () ^
                typ                                                      )

    let recursive_usage ~pos ~t1:(_, lt) ~t2:(v, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("Using the recursive variable"          ^ nli () ^
                code v                                 ^ nl  () ^
               "makes this term wild"                  ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let escaped_quantifier ~pos ~var ~annotation ~escapees =
      let escaped_tys = List.map snd escapees in
      build_tyvar_names (annotation :: escaped_tys);
      let policy () = Types.Policy.set_quantifiers true (error_policy ()) in
      let display_ty (var, ty) =
        Printf.sprintf "%s: %s" var
          (Types.string_of_datatype ~policy ~refresh_tyvar_names:false ty) in
      let displayed_tys =
        List.map display_ty escapees
        |> String.concat (nli ()) in
      die pos ("The quantifiers in the type of function" ^ nli () ^
               display_ty (var, annotation)              ^ nl () ^
               "escape their scope, as they are present in the types:" ^ nli () ^
               displayed_tys)

    let temporal_join_effects ~pos ~t1:(_, lt) ~t2:(_, rt) ~error:_ =
      build_tyvar_names [lt; rt];
      let ppr_rt = show_type rt in
      let ppr_lt = show_type lt in
      die pos ("The join block has effects"            ^ nli () ^
                code ppr_rt                            ^ nl  () ^
               "but the currently allowed effects are" ^ nli () ^
                code ppr_lt)

    let temporal_join_body ~pos ~t1:(expr,t) ~t2:_ ~error:_ =
      build_tyvar_names [t];
      with_but pos
        ("The body of a temporal join must return a list")
        (expr, t)

    let valid_update_pattern ~pos ~t1:l ~t2:r ~error:_ =
      build_tyvar_names [snd l; snd r];
      with_but2things pos
        "The binding must match the table in a valid-time update expression"
        ("pattern", l) ("row", r)

    let sequenced_update_datetime ~pos ~t1:(_, l) ~t2:_ ~error:_ =
        build_tyvar_names [l];
      with_but pos
        "The periods of validity for a valid-time sequenced update must have time DateTime"
        ("expression", l)

    let nonsequenced_update_datetime ~pos ~t1:(_, l) ~t2:_ ~error:_ =
        build_tyvar_names [l];
      with_but pos
        "The new validity fields for a valid-time nonsequenced update must have time DateTime"
        ("expression", l)

end

(* Tag: context begin *)
type context = Types.typing_environment = {
  (* mapping from variables to type schemes *)
  var_env   : Types.environment;

  (* variables which are recursive in the current scope *)
  rec_vars : StringSet.t;

  (* mapping from type alias names to the types they name.  We don't
     use this to resolve aliases in the code, which is done before
     type inference.  Instead, we use it to resolve references
     introduced here to aliases defined in the prelude such as "Page"
     and "Formlet". *)
  tycon_env : Types.tycon_environment;

  (* the current effects *)
  effect_row : Types.row;

  (* the current continuation linearity.
     It will be mapped to a pair of bools, where
     the first bool value indicates whether the current term is in a linear continuation currently
     the second bool value indicates whether the current term is bound by a linlet *)
  cont_lin : int;

  (* Whether this is runs on desugared code, and so some non-user
     facing constructs are permitted. *)
  desugared : bool;
}

let empty_context contlin eff desugared =
  { var_env    = Env.empty;
    rec_vars   = StringSet.empty;
    tycon_env  = Env.empty;
    effect_row = eff;
    cont_lin   = contlin;
    desugared }

let bind_var         context (v, t) = {context with var_env    = Env.bind v t context.var_env}
let unbind_var       context v      = {context with var_env    = Env.unbind v context.var_env}
let bind_alias       context (v, t) = {context with tycon_env  = Env.bind v t context.tycon_env}
let bind_effects     context r      = {context with effect_row = Types.flatten_row r}

let lookup_effect    context name   =
  match context.effect_row with
  | Types.Row (fields, _, _) ->
    begin match Utility.StringMap.find_opt name fields with
      | Some (Types.Present t) -> Some t
      | _ -> None
    end
  | _ -> raise (internal_error "Effect row in the context is not a row")


module LinCont = struct
  (* Some helper functions for control-flow linearity. *)

  let is_enabled = (Settings.get Basicsettings.CTLinearity.enabled)

  let enabled = fun f ->
    if is_enabled then f () else ()
  (*
    NOTE: The meaning of Any and Unl for effect row types is different from other types:
    - An effect row type with kind `Any` means it can be linear or unlimited.
    - An effect row type with kind `Unl` means it must be linear!
    Moreover, for effect signatures, `=>` means linear signature which must have a linear
    continuation, and `=@` means signature which may have any continuation.
    This is just an implementation trick to reuse the previous mechanism of unification.
  *)

  let make_operation_type : ?linear:bool -> Types.datatype list -> Types.datatype -> Types.datatype
    = fun ?(linear=false) args range ->
      let lin = if is_enabled then DeclaredLinearity.(if linear then Lin else Unl)
                else DeclaredLinearity.Unl
      in Types.Operation (Types.make_tuple_type args, range, lin)

  (* linear continuation(function) is still represented by `-@`, so we still use `islin` *)
  let make_continuation_type = fun islin inp eff out ->
    if is_enabled
    then (Types.make_function_type ~linear:(islin) inp eff out)
    else (Types.make_function_type ~linear:(false) inp eff out)


  (*
      `cont_lin` (continuation linearity) is represented by an
      integer, which is mapped to a pair of bools by the global
      `cont_lin_map`.
      - `cont_lin.first = true` : the current term is in a linear
        continuation. Nothing to do.
      - `cont_lin.first = false` : the current term is in an unlimited
        continuation. We need to guarantee it does not use linear
        variables bound outside.
      - `cont_lin.second = true` : the current term is bound by linlet
        (i.e. has a linear continuation). If the current term is not
        pure, we should guarantee that the current effect type
        `effect_row` is linear.
      - `cont_lin.second = false` : the current term is bound by let
        (i.e. has an unlimited continuation). Nothing to do.

      We implement `cont_lin = (false, false)` by default because it
      is more compatible with previous effect handlers.

      The reason to use a global map is that we want to make sure
      sequenced terms (terms in the "same scope") have the same
      `cont_lin`. The only places where `cont_lin` is updated to a new
      one is where `effect_row` is updated to a new one. (I think
      `effect_row` also uses some global mechanism for unification.)
  *)
  let count = ref 0

  let default = (false, false)

  (* `-1` is the `cont_lin` of `empty_typing_environment`. It is
    supposed to be used in the typing of default global bindings. *)
  let linmap = ref (IntMap.add (-1) default IntMap.empty)

  let getnew () =
    if is_enabled then (
      let newx = !count in
      let () = count := newx + 1 in
      let () = linmap := IntMap.add newx default !linmap in
      newx )
    else -1

  let is_in_linlet context =
    if is_enabled then fst <| IntMap.find context.cont_lin !linmap
    else false

  let is_bound_by_linlet context =
    if is_enabled then snd <| IntMap.find context.cont_lin !linmap
    else false

  let update_in_linlet context a =
    enabled (fun () ->
      let b = is_bound_by_linlet context in
      linmap := IntMap.add context.cont_lin (a, b) !linmap
    )

  let update_bound_by_linlet context b =
    enabled (fun () ->
      let a = is_in_linlet context in
      linmap := IntMap.add context.cont_lin (a, b) !linmap
    )
end


(* TODO(dhil): I have extracted the Usage abstraction from my name
   hygiene/compilation unit patch. The below module is a compatibility
   module which will make it easier for me to merge my other branch
   with master in the future. This module will be removed once I have
   land the aforementioned patch. *)
module Ident = struct
  type t = string

  module Map = Utility.StringMap
  module Set = Utility.StringSet
end

(* Track usages of identifiers. *)
module Usage: sig
  (* Disables unused warnings for members of this signature. *)
  [@ocaml.warning "-32"]
  type t

  (* Empty usage container. *)
  val empty : t
  (* Adds an entry to a given container. Any preexisting entry with
     the same identifier will be overwritten. *)
  val add : Ident.t -> int -> t -> t
  (* Removes an entry from a given container. *)
  val remove : Ident.t -> t -> t
  (* Tests whether an entry exists. *)
  val mem : Ident.t -> t -> bool
  (* Returns the uses of an identifier. Returns 0 if `mem` returns
     false. *)
  val uses_of : Ident.t -> t -> int
  (* Increments the usage of an identifier by a positive amount. *)
  val incr : ?by:int -> Ident.t -> t -> t
  (* Constructs a new container with a single usage mapping. *)
  val singleton : ?count:int -> Ident.t -> t
  (* Combines two containers. *)
  val combine : t -> t -> t
  (* Combines many containers. *)
  val combine_many : t list -> t
  (* Returns the contents of the container as an association list. *)
  val bindings : t -> (Ident.t * int) list
  (* Merges two containers. *)
  val merge : (Ident.t -> int option -> int option -> int option) -> t -> t -> t
  (* Filters entries in a given container. *)
  val filter : (Ident.t -> int -> bool) -> t -> t
  (* Iterates over entries in a given container. *)
  val iter : (Ident.t -> int -> unit) -> t -> unit
  (* Folds over entries in a given container. *)
  val fold : (Ident.t -> int -> 'b -> 'b) -> t -> 'b -> 'b
  (* Returns a usage container that does not contain any of the names
     in the given set. *)
  val restrict : t -> Ident.Set.t -> t
  (* Aligns disjunctive uses of variables. *)
  val align : t list -> t
end = struct
  type t = int Ident.Map.t

  let empty = Ident.Map.empty

  let add v c usages =
    assert (c >= 0);
    Ident.Map.add v c usages

  let remove v usages =
    Ident.Map.remove v usages

  let mem v usages =
    Ident.Map.mem v usages

  let uses_of v usages =
    try Ident.Map.find v usages
    with Notfound.NotFound _ -> 0

  let incr ?(by=1) v usages =
    assert (by > 0);
    let uses =
      uses_of v usages
    in
    Ident.Map.add v (uses + by) usages

  let singleton ?(count=1) v =
    incr ~by:count v empty

  let combine usages usages' =
    Ident.Map.merge
      (fun _ x y ->
        match x, y with
        | Some x, Some y -> Some (x + y)
        | Some _, None   -> x
        | None, Some _   -> y
        | None, None     -> None)
    usages usages'

  let combine_many usagess =
    List.fold_left combine empty usagess

  let bindings usages =
    Ident.Map.bindings usages

  let merge join usages usages' =
    Ident.Map.merge join usages usages'

  let filter f usages =
    Ident.Map.filter f usages

  let iter f usages =
    Ident.Map.iter f usages

  let fold f usages =
    Ident.Map.fold f usages

  let restrict usages idents =
    filter (fun v _ -> not (Ident.Set.mem v idents)) usages

  let align = function
    | [] ->
       (* HACK: for now we take the conservative choice of assuming
          that no linear variables are used in empty cases. We could
          keep track of all variables in scope so that we can treat
          them as linear if possible. This would require a further map
          recording all variables that have empty pattern matching
          'sink'. *)
       empty
    | usages :: usagess  ->
       let combine' : Ident.t -> int option -> int option -> int option
         = fun _ident x y ->
         let unlimited = max_int in
         match x, y with
         | None, None -> None
         | Some x', Some y' when x' = y' -> x
         (* We need to treat anything appearing in the below cases as
            unlimited; 'max_int' assures that no matter whether the
            variable in question is used anywhere else or not, it must
            be unlimited. *)
         | Some _, None | None, Some _ | Some _, Some _ -> Some unlimited
       in
       List.fold_left (merge combine') usages usagess
end

let type_section pos context s =
  let env = context.var_env in
  let ((tyargs, t), usages) =
    let open Section in
    let open Types in
    match s with
    | Minus         -> Utils.instantiate env "-", Usage.empty
    | FloatMinus    -> Utils.instantiate env "-.", Usage.empty
    | Project label ->
       let a = Types.fresh_type_variable (lin_unl, res_any) in
       let rho = Types.fresh_row_variable (lin_unl, res_any) in
       let effects = Types.make_empty_open_row default_effect_subkind in (* projection is pure! *)
       let r = Record (Row (StringMap.add label (Present a) StringMap.empty, rho, false)) in
         ([(PrimaryKind.Type, a); (PrimaryKind.Row, Row (StringMap.empty, rho, false)); (PrimaryKind.Row, effects)],
          Function (Types.make_tuple_type [r], effects, a)),
         Usage.empty
    | Name var      ->
       try Utils.instantiate env var, Usage.singleton var
       with Errors.UndefinedVariable _msg ->
         Gripers.die pos (Printf.sprintf "Unknown variable %s." var)
  in
  tappl (FreezeSection s, tyargs), t, usages

let type_frozen_section context s =
  let env = context.var_env in
  let t, usages =
    let open Section in
    let open Types in
    match s with
    | Minus         -> Env.find "-" env, Usage.empty
    | FloatMinus    -> Env.find "-." env, Usage.empty
    | Project label ->
       let a = Types.fresh_rigid_type_variable (lin_unl, res_any) in
       let rho = Types.fresh_rigid_row_variable (lin_unl, res_any) in
       let effects = StringMap.empty, Types.fresh_rigid_row_variable default_effect_subkind, false in
       let r = Record (Row (StringMap.add label (Present a) StringMap.empty, rho, false)) in
       Types.for_all
         (Types.quantifiers_of_type_args [(PrimaryKind.Type, a);
                                          (PrimaryKind.Row, Row (StringMap.empty, rho, false));
                                          (PrimaryKind.Row, Row effects)],
          Function (Types.make_tuple_type [r], Row effects, a)),
       Usage.empty
    | Name var      -> Env.find var env, Usage.singleton var in
  FreezeSection s, t, usages


let datatype aliases = Instantiate.typ -<- DesugarDatatypes.read ~aliases
let add_usages (p, t) m = (p, t, m)
let add_empty_usages (p, t) = (p, t, Usage.empty)

let type_unary_op pos env =
  let datatype = datatype env.tycon_env in
  function
  | UnaryOp.Minus      -> add_empty_usages (datatype "(Int) { |_::Any}-> Int")
  | UnaryOp.FloatMinus -> add_empty_usages (datatype "(Float) { |_::Any}-> Float")
  | UnaryOp.Name n     ->
     try
       add_usages (Utils.instantiate env.var_env n) (Usage.singleton n)
     with
       Errors.UndefinedVariable _msg ->
       Gripers.die pos (Printf.sprintf "Unknown variable %s." n)

let type_binary_op pos ctxt =
  let open BinaryOp in
  let open Types in
  let datatype = datatype ctxt.tycon_env in function
  | Minus        -> add_empty_usages (Utils.instantiate ctxt.var_env "-")
  | FloatMinus   -> add_empty_usages (Utils.instantiate ctxt.var_env "-.")
  | RegexMatch flags ->
      let nativep  = List.exists ((=) RegexNative)  flags
      and listp    = List.exists ((=) RegexList)    flags
      and replacep = List.exists ((=) RegexReplace) flags in
        begin
          match replacep, listp, nativep with
           | true,   _   , false -> (* stilde  *) add_empty_usages (datatype "(String, Regex) -> String")
           | false, true , false -> (* ltilde *)  add_empty_usages (datatype "(String, Regex) -> [String]")
           | false, false, false -> (* tilde *)   add_empty_usages (datatype "(String, Regex) -> Bool")
           | _    , _    , true  -> assert false
        end
  | And
  | Or           -> add_empty_usages (datatype "(Bool,Bool) -> Bool")
  | Cons         -> add_empty_usages (Utils.instantiate ctxt.var_env "Cons")
  | Name "++"    -> add_empty_usages (Utils.instantiate ctxt.var_env "Concat")
  | Name ">"
  | Name ">="
  | Name "=="
  | Name "<"
  | Name "<="
  | Name "<>"    ->
      let a = Types.fresh_type_variable (lin_any, res_any) in
      let eff = Types.make_empty_open_row default_effect_subkind in
        ([(PrimaryKind.Type, a); (PrimaryKind.Row, eff)],
         Function (Types.make_tuple_type [a; a], eff, Primitive Primitive.Bool),
         Usage.empty)
  | Name "!"     -> add_empty_usages (Utils.instantiate ctxt.var_env "Send")
  | Name n       ->
     try
       add_usages (Utils.instantiate ctxt.var_env n) (Usage.singleton n)
     with
       Errors.UndefinedVariable _msg ->
       Gripers.die pos (Printf.sprintf "Unknown variable %s." n)

(* close a pattern type relative to a list of patterns

   If there are no _ or variable patterns at a variant type, then that
   variant will be closed.
*)
let close_pattern_type : Pattern.with_pos list -> Types.datatype -> Types.datatype = fun pats t ->
  (* We use a table to keep track of encountered recursive variables
     in order to avert non-termination. *)
  let rec_vars_seen = Hashtbl.create 8 in
  let open Types in
  let rec cpt : Pattern.with_pos list -> Types.datatype -> Types.datatype = fun pats t ->
    match t with
      | Alias (k, alias, t) -> Alias (k, alias, cpt pats t)
      | Record row when Types.is_tuple row->
          let fields, row_var, dual = Types.unwrap_row row |> fst |> TypeUtils.extract_row_parts in
          let rec unwrap_at i p =
            let open Pattern in
            match p.node with
              | Variable _ | Any | Constant _ -> p
              | As (_, p) | HasType (p, _) -> unwrap_at i p
              | Tuple ps ->
                  List.nth ps i
              | Nil | Cons _ | List _ | Record _ | Variant _ | Negative _ | Operation _ -> assert false in
          let fields =
            StringMap.fold(* true if the row variable is dualised *)

              (fun name ->
                 function
                 | Present t ->
                       let pats = List.map (unwrap_at ((int_of_string name) - 1)) pats in
                         StringMap.add name (Present (cpt pats t))
                 | (Absent | Meta _) -> assert false
                 | _ -> raise Types.tag_expectation_mismatch) fields StringMap.empty in
            Record (Row (fields, row_var, dual))
      | Record row ->
          let fields, row_var, lr = (Types.unwrap_row row |> fst |> TypeUtils.extract_row_parts) in
          assert (not lr);
          let rec unwrap_at name p =
            let open Pattern in
            match p.node with
              | Variable _ | Any | Constant _ -> p
              | As (_, p) | HasType (p, _) -> unwrap_at name p
              | Record (ps, default) ->
                  if List.mem_assoc name ps then
                    List.assoc name ps
                  else
                    begin
                      match default with
                        | None -> assert false
                        | Some p -> unwrap_at name p
                    end
              | Nil | Cons _ | List _ | Tuple _ | Variant _ | Negative _ | Operation _ -> assert false in
          let fields =
            StringMap.fold
              (fun name ->
                 function
                   | Present t ->
                       let pats = List.map (unwrap_at name) pats in
                         StringMap.add name (Present (cpt pats t))
                   | (Absent | Meta _) -> assert false
                   | _ -> raise Types.tag_expectation_mismatch) fields StringMap.empty in
            Record (Row (fields, row_var, false))
      | Variant row ->
          let fields, row_var, lr = (Types.unwrap_row row |> fst |> TypeUtils.extract_row_parts) in
          assert (not lr);

          let rec unwrap_at : string -> Pattern.with_pos -> Pattern.with_pos list = fun name p ->
            let open Pattern in
            match p.node with
              | Variable _ | Any -> [ with_pos p.pos Pattern.Any ]
              | As (_, p) | HasType (p, _) -> unwrap_at name p
              | Variant (name', None) when name=name' ->
                    [with_pos p.pos (Pattern.Record ([], None))]
              | Variant (name', Some p) when name=name' -> [p]
              | Variant _ -> []
              | Negative names when List.mem name names -> []
              | Negative _ -> [ with_pos p.pos Pattern.Any ]
              | Nil | Cons _ | List _ | Tuple _ | Record _ | Constant _ | Operation _ -> assert false in
          let rec are_open : Pattern.with_pos list -> bool =
            let open Pattern in
            function
              | [] -> false
              | {node = (Variable _ | Any | Negative _); _} :: _ -> true
              | {node = (As (_, p) | HasType (p, _)); _} :: ps -> are_open (p :: ps)
              | {node = (Variant _); _} :: ps -> are_open ps
              | {node = (Nil | Cons _ | List _ | Tuple _ | Record _ | Constant _ | Operation _); _} :: _ -> assert false in
          let fields =
            StringMap.fold
              (fun name field_spec env ->
                 match field_spec with
                   | Present t ->
                       let pats = concat_map (unwrap_at name) pats in
                       let t = cpt pats t in
                         (StringMap.add name (Present t)) env
                   | (Absent | Meta _) -> assert false
                   | _ -> raise Types.tag_expectation_mismatch) fields StringMap.empty
          in
            if are_open pats then
              begin
                let row = Row (fields, row_var, false) in
                (* NOTE: type annotations can lead to a closed type even though
                   the patterns are open *)
                Variant row
              end
            else
              begin
                match Unionfind.find row_var with
                  | Var _ -> Variant (Row (fields, Unionfind.fresh Closed, false))
                  | _ -> assert false
              end

      | Effect row ->
         (* We don't really close effect rows, rather, we close the
            subpatterns contained within the effect row. *)
          let fields, row_var, lr = (Types.unwrap_row row |> fst |> TypeUtils.extract_row_parts) in
          assert (not lr);

          let unwrap_at : string -> Pattern.with_pos -> Pattern.with_pos list = fun name p ->
            let open Pattern in
            match p.node with
              | Operation (name', ps, _, _) when name=name' -> ps
              | Operation _ -> []
              | Variable _ | Any | As _ | HasType _ | Negative _
              | Nil | Cons _ | List _ | Tuple _ | Record _ | Variant _ | Constant _ -> assert false in
          let fields =
            StringMap.fold
              (fun name field_spec env ->
                 match field_spec with
                 | Present t ->
                    begin match TypeUtils.concrete_type t with
                    | Function (_, effs, codomain) ->
                       (* Idea: For each operation `name' extract its
                          patterns `ps' from Effect(name, ps, _)' and
                          arrange each such ps as a row in and n x p
                          matrix, where n is the number of cases for
                          `name' and p is |ps|. Afterwards, point-wise
                          close the patterns by recursively calling
                          close_pattern_type on each column. *)
                       let t =
                       (* Construct an p x n matrix (i.e. the
                          transposition of p x n matrix as it is easier
                          to map column-wise) *)
                         let pmat : Pattern.with_pos list list =
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
                       StringMap.add name (Present t) env
                    | _ ->
                       StringMap.add name (Present t) env
                    end
                 | t -> StringMap.add name t env) fields StringMap.empty
          in
          let row = Row (fields, row_var, false) in
          (* NOTE: type annotations can lead to a closed type even though
                   the patterns are open *)
          Effect row
      | Application (l, [t])
          when Types.Abstype.equal l Types.list ->
          let rec unwrap p : Pattern.with_pos list =
            let open Pattern in
            match p.node with
              | Variable _ | Any -> [p]
              | Constant _ | Nil -> []
              | Cons (p1, p2) -> p1 :: unwrap p2
              | List ps -> ps
              | As (_, p) | HasType (p, _) -> unwrap p
              | Variant _ | Negative _ | Record _ | Tuple _ | Operation _ -> assert false in
          let pats = concat_map unwrap pats in
            Application (Types.list, [cpta pats t])
      | ForAll (qs, t) -> ForAll (qs, cpt pats t)
      | Meta point ->
          begin
            match Unionfind.find point with
              | Var _ -> t
              | Recursive (i, _kind, t') when not (Hashtbl.mem rec_vars_seen i) ->
                 Hashtbl.add rec_vars_seen i ();
                 cpt pats t'
              | Recursive _ -> t
              | t -> cpt pats t
          end
      | Not_typed
      | Primitive _
      | Function _
      | Lolli _
      | Operation _
      | Table _
      | Lens _
      (* TODO: do we need to do something special for session types? *)
      | Input _
      | Output  _
      | Select _
      | Choice _
      | Dual _
      | End
      (* TODO: or anything special for recursive applications? *)
      | RecursiveApplication _
       (* TODO: expand applications? *)
      | Application _ -> t
       (* failing on constructors of non-type PK and plain Var/Recursive *)
      | Closed
      | Var _
      | Recursive _
      | Row _
      | Absent
      | Present _ -> raise Types.tag_expectation_mismatch
  and cpta : Pattern.with_pos list -> Types.type_arg -> Types.type_arg =
    fun pats (pk, t) -> (pk, cpt pats t) in
  cpt pats t

type unify_result = UnifySuccess | UnifyFailure of (Unify.error * Position.t)

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


(* check for duplicate names in a list of pattern *)
let check_for_duplicate_names : Position.t -> Pattern.with_pos list -> string list = fun pos ps ->
  let add name binder binderss =
    if StringMap.mem name binderss then
      let (count, binders) = StringMap.find name binderss in
        StringMap.add name (count+1, binder::binders) binderss
    else
      StringMap.add name (1, [binder]) binderss in

  let rec gather binderss {node; _} =
    let open Pattern in
    match node with
      | Nil -> binderss
      | Any -> binderss
      | Cons (p, q) ->
          let binderss = gather binderss p in gather binderss q
      | List ps ->
          List.fold_right (fun p binderss -> gather binderss p) ps binderss
      | Variant (_, p) ->
         opt_app (fun p -> gather binderss p) binderss p
      | Operation (_, ps, k, _) ->
         let binderss' =
           List.fold_right (fun p binderss -> gather binderss p) ps binderss
         in
         gather binderss' k
      | Negative _ -> binderss
      | Record (ps, p) ->
          let binderss = List.fold_right (fun (_, p) binderss -> gather binderss p) ps binderss in
            opt_app (fun p -> gather binderss p) binderss p
      | Tuple ps ->
          List.fold_right (fun p binderss -> gather binderss p) ps binderss
      | Constant _ -> binderss
      | Variable bndr ->
          add (Binder.to_name bndr) bndr binderss
      | As (bndr, p) ->
          let binderss = gather binderss p in
            add (Binder.to_name bndr) bndr binderss
      | HasType (p, _) -> gather binderss p in

  let binderss =
    List.fold_left gather StringMap.empty ps in
  let dups = StringMap.filterv (fun (i, _) -> i > 1) binderss in
    if not (StringMap.is_empty dups) then
      Gripers.duplicate_names_in_pattern pos
    else
      List.map fst (StringMap.bindings binderss)


let rec pattern_env : Pattern.with_pos -> Types.datatype Env.t =
  fun { node = p; _} -> let open Pattern in
  match p with
    | Any
    | Nil
    | Constant _ -> Env.empty

    | HasType (p,_) -> pattern_env p
    | Variant (_, Some p) -> pattern_env p
    | Variant (_, None) -> Env.empty
    | Operation (_, ps, k, _) ->
      let env = List.fold_right (pattern_env ->- Env.extend) ps Env.empty in
      Env.extend env (pattern_env k)
    | Negative _ -> Env.empty
    | Record (ps, Some p) ->
       List.fold_right (snd ->- pattern_env ->- Env.extend) ps (pattern_env p)
    | Record (ps, None) ->
       List.fold_right (snd ->- pattern_env ->- Env.extend) ps Env.empty
    | Cons (h,t) -> Env.extend (pattern_env h) (pattern_env t)
    | List ps
    | Tuple ps -> List.fold_right (pattern_env ->- Env.extend) ps Env.empty
    | Variable bndr ->
       Env.singleton (Binder.to_name bndr) (Binder.to_type bndr)
    | As (bndr, p) ->
       Env.bind (Binder.to_name bndr) (Binder.to_type bndr) (pattern_env p)

let type_pattern ?(linear_vars=true) closed
    : Pattern.with_pos -> Pattern.with_pos * Types.environment * Types.datatype =
  let make_singleton_row =
    match closed with
      | `Closed -> Types.make_singleton_closed_row
      | `Open -> (fun var -> Types.make_singleton_open_row var (lin_any, res_any)) in

  let fresh_var () =
    if linear_vars
    then Types.fresh_type_variable (lin_any, res_any)
    else Types.fresh_type_variable (lin_unl, res_any) in

  (* type_pattern p types the pattern p returning a typed pattern, a
     type environment for the variables bound by the pattern and two
     types. The first type is the type of the pattern 'viewed from the
     outside' - in the case of variant patterns it must be open in
     order to allow cases to unify. The second type is the type of the
     pattern 'viewed from the inside'. Type annotations are only
     applied to the inner pattern. The type environment is constructed
     using types from the inner type.

  *)
  let rec type_pattern ?ann {node = pattern; pos = pos'} : Pattern.with_pos * (Types.datatype * Types.datatype) =
    let _UNKNOWN_POS_ = "<unknown>" in
    let tp = type_pattern in
    let unify (l, r) = unify_or_raise ~pos:pos' (l, r)
    and erase (p, _) = p
    and ot (_,(t,_)) = t
    and it (_,(_,t)) = t
    and pos ({pos = p;_},_) = Position.Resolved.resolve p |> Position.Resolved.source_expression in
    let (p, (outer_type, inner_type)) :
          Pattern.t * (Types.datatype * Types.datatype) =
      let open Pattern in
      let open Types in
      match pattern with
      | Nil ->
        let t = Types.make_list_type (fresh_var ()) in
        Nil, (t, t)
      | Any ->
        let t = Types.fresh_type_variable (lin_unl, res_any) in
        Any, (t, t)
      | Constant c as c' ->
        let t = Primitive (Constant.type_of c) in
        c', (t, t)
      | Variable bndr ->
        let xtype = fresh_var () in
        Variable (Binder.set_type bndr xtype), (xtype, xtype)
      | Cons (p1, p2) ->
        let p1 = tp p1
        and p2 = tp p2 in
        let () = unify ~handle:Gripers.cons_pattern ((pos p1, Types.make_list_type (ot p1)),
                                                     (pos p2, ot p2)) in
        let () = unify ~handle:Gripers.cons_pattern ((pos p1, Types.make_list_type (it p1)),
                                                     (pos p2, it p2)) in
        Cons (erase p1, erase p2), (ot p2, it p2)
      | List ps ->
        let ps' = List.map tp ps in
        let list_type p ps typ =
          let () = List.iter (fun p' -> unify ~handle:Gripers.list_pattern ((pos p, typ p),
                                                                            (pos p', typ p'))) ps in
          Types.make_list_type (typ p) in
        let ts =
          match ps' with
          | [] -> let t = fresh_var () in t, t
          | p::ps ->
            list_type p ps ot, list_type p ps it
        in
        List (List.map erase ps'), ts
      | Pattern.Variant (name, None) ->
        let vtype () = Types.Variant (make_singleton_row (name, Present Types.unit_type)) in
        Pattern.Variant (name, None), (vtype (), vtype ())
      | Pattern.Variant (name, Some p) ->
        let p = tp p in
        let vtype typ = Types.Variant (make_singleton_row (name, Present (typ p))) in
        Pattern.Variant (name, Some (erase p)), (vtype ot, vtype it)
      | Pattern.Operation (name, ps, k, linearity) ->
         let is_lincase = linearity = DeclaredLinearity.Lin in
         (* Auxiliary machinery for typing effect patterns *)
         let rec type_resumption_pat (kpat : Pattern.with_pos) : Pattern.with_pos * (Types.datatype * Types.datatype) =
           let fresh_resumption_type () =
             let domain   = fresh_var () in
             let codomain = fresh_var () in
             let effrow   = Types.make_empty_open_row default_effect_subkind in
             LinCont.make_continuation_type is_lincase [domain] effrow codomain
           in
           let pos' = kpat.pos in
           let open Pattern in
           match kpat.node with
           | Any ->
              let t = fresh_resumption_type () in
              if is_lincase && LinCont.is_enabled then Gripers.die pos' ("The linear continuation is not bound.") else ();
              kpat, (t, t)
           | Variable bndr ->
              let xtype = fresh_resumption_type () in
              ( with_pos pos' (Variable (Binder.set_type bndr xtype))
              , (xtype, xtype))
           | As (bndr, pat') ->
              let p = type_resumption_pat pat' in
              with_pos pos' (As ((Binder.set_type bndr (it p), erase p))), (ot p, it p)
           | HasType (p, (_, Some t)) ->
              let p = type_resumption_pat p in
              let () = unify ~handle:Gripers.type_resumption_with_annotation ((pos p, it p), (_UNKNOWN_POS_, t)) in
              erase p, (ot p, t)
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
             | [], [t] -> Types.Operation (Types.unit_type, t, linearity)
             | [], ts -> Types.make_tuple_type ts  (* FIXME: WT: I don't understand this case *)
             | ts, [t] ->
                LinCont.make_operation_type ~linear:is_lincase ts t
             | ts, ts' ->
                (* parameterised continuation *)
                let t = ListUtils.last ts' in
                LinCont.make_operation_type ~linear:is_lincase ts t
           in
           t
         in
         let ot, it = match ann with
         | Some t ->
           let ot, _it = eff ot, eff it in
           let _, t_free = TypeUtils.split_quantified_type t in
           let () = unify ~handle:Gripers.pattern_annotation ((_UNKNOWN_POS_, ot), (_UNKNOWN_POS_, t_free)) in
           t, t
         | None ->
           (eff ot, eff it)
         in
         Pattern.Operation (name, List.map erase ps, erase k, linearity), (ot, it)
      | Negative names ->
        let row_var = Types.fresh_row_variable (lin_any, res_any) in

        let positive, negative =
          List.fold_right
            (fun name (positive, negative) ->
               let a = fresh_var () in
               (StringMap.add name (Present a) positive,
                StringMap.add name Absent negative))
            names (StringMap.empty, StringMap.empty) in

        let outer_type = Types.Variant (Row (positive, row_var, false)) in
        let inner_type = Types.Variant (Row (negative, row_var, false)) in
        Negative names, (outer_type, inner_type)
      | Pattern.Record (ps, default) ->
        let ps = alistmap tp ps in
        let default = opt_map tp default in
        let initial_outer, initial_inner =
          match default with
          | None ->
            let row = Types.make_empty_closed_row () in
            row, row
          | Some r ->
            let make_closed_row typ =
              let row =
                List.fold_right
                  (fun (label, _) ->
                     Types.row_with (label, Absent))
                  ps (Types.make_empty_open_row (lin_any, res_any)) in
              let () = unify ~handle:Gripers.record_pattern (("", Record row),
                                                             (pos r, typ r))
              in
              row
            in
            make_closed_row ot, make_closed_row it in
        let rtype typ initial =
          Types.Record (List.fold_right
                     (fun (l, f) -> Types.row_with (l, Present (typ f)))
                     ps initial) in
        (Pattern.Record (alistmap erase ps, opt_map erase default),
         (rtype ot initial_outer, rtype it initial_inner))
      | Tuple ps ->
        let ps' = List.map tp ps in
        let make_tuple typ = Types.make_tuple_type (List.map typ ps') in
        Tuple (List.map erase ps'), (make_tuple ot, make_tuple it)
      | As (bndr, p) ->
        let p = tp p in
        As (Binder.set_type bndr (it p), erase p), (ot p, it p)
      | HasType (p, (_,Some t as t')) ->
        let p = tp ~ann:t p in
        let () = unify ~handle:Gripers.pattern_annotation ((pos p, it p), (_UNKNOWN_POS_, t)) in
        HasType (erase p, t'), (ot p, t)
      | HasType _ -> assert false in
    with_pos pos' p, (outer_type, inner_type)
  in
  fun pattern ->
    let _ = check_for_duplicate_names pattern.pos [pattern] in
    let pattern', (outer_type, _) = type_pattern pattern in
    pattern', pattern_env pattern', outer_type

let update_pattern_vars env =
(object (self)
  inherit SugarTraversals.map as super

  method! patternnode : Pattern.t -> Pattern.t =
    fun n ->
      let open Pattern in
      let update bndr =
        let ty = Env.find (Binder.to_name bndr) env in
        Binder.set_type bndr ty
      in match n with
         | Variable b -> Variable (update b)
         | As (b, p)  -> As (update b, self#pattern p)
         | _ -> super#patternnode n
 end)#pattern

let rec extract_formlet_bindings : phrase -> Types.datatype Env.t = fun p ->
  match p.node with
  | FormBinding (_, pattern) -> pattern_env pattern
  | Xml (_, _, _, children) ->
      List.fold_right
        (fun child env ->
           Env.extend env (extract_formlet_bindings child))
        children Env.empty
  | _ -> Env.empty

(* make a function type constructor based on declared linearity *)
let make_ftcon declared_linearity p =
  if DeclaredLinearity.is_linear declared_linearity
  then Types.Lolli p
  else Types.Function p

(* given a declared linearity, list of argument patterns, effects, and a return
   type return the corresponding function type *)
let make_ft decl_lin ps effects return_type =
  let pattern_typ (_, _, t) = t in
  let args = Types.make_tuple_type -<- List.map pattern_typ in
  let rec ft =
    function
      | [p]   -> make_ftcon decl_lin (args p, effects, return_type)
      | p::ps -> make_ftcon decl_lin (args p, Types.make_empty_open_row default_effect_subkind, ft ps)
      | [] -> assert false
  in ft ps

let make_ft_poly_curry declared_linearity ps effects return_type =
  let pattern_typ (_, _, t) = t in
  let args = Types.make_tuple_type -<- List.map pattern_typ in
  let rec ft =
    function
      | [p] -> [], make_ftcon declared_linearity (args p, effects, return_type)
      | p::ps ->
          let qs, t = ft ps in
          let q, eff = Types.fresh_row_quantifier default_effect_subkind in
            q::qs, make_ftcon declared_linearity (args p, eff, t)
      | [] -> assert false in
  Types.for_all (ft ps)

(** Make any unannotated parameters monomorphic. *)
let make_mono pats = List.iter (List.iter (fun (_, _, t) -> Types.Mono.make_type t)) pats

let usages_cases bs =
  Usage.align (List.map (fun (_, (_, _, m)) -> m) bs)

(* if we've already inferred a type from a previous type inference
   pass then use that in place of any other annotation *)
let resolve_type_annotation : Binder.with_pos -> Sugartypes.datatype' option -> Types.datatype option =
  fun bndr t_ann' ->
  match Binder.to_type bndr with
  | Types.Not_typed ->
    begin
      match t_ann' with
      | None -> None
      | Some (_, t) -> t
    end
  | t -> Some t

(* NOTE: Unsafe signatures hack

   The syntax "unsafe sig f : ty" asserts that f has type t, *provided* that recursive
   calls to f are not treated as wild.  This is used to allow recursive definitions for
   specific operations such as map, concatMap, filter, sortByBase which previously required
   an even more awkward workaround.  "unsafe" should not be used outside of the prelude since it would
   enable constructing queries that typecheck but cannot be translated to SQL and
   perhaps we should check for this.

   The initial implementation of "unsafe" ran type inference as normal, and then performed
   surgery on the type.  Unfortunately this meant that other occurrences of the wild effect
   arising because of the recursive occurrence were left as is, and doing more surgery to fix
   these up seemed nontrivial.  See #691 and #864.

   The unsafe flag is currently propagated to the IR and has the same meaning there -
   calls involving function names are given the wild effect only if they are recursive
   and not declared with the unsafe flag.  It is desirable to move to a design where "unsafe"
   is not needed in the IR (because the staging distinction between queries and normal code
   has already been made explicit) or at all (because query syntax is desugared to an interface
   with sufficiently effect-polymorphic operations).
 *)

let rec type_check : context -> phrase -> phrase * Types.datatype * Usage.t =
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
    let pattern_pos ({pos=p; _},_,_) =
      Position.Resolved.resolve p |> Position.Resolved.source_expression in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos p =
      WithPos.pos p |>
      Position.Resolved.resolve |>
      Position.Resolved.source_expression in
    let exp_pos (p,_,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in
    let tpc p = type_pattern `Closed p in
    let tpcu p = type_pattern ~linear_vars:false `Closed p in
    let tpo p = type_pattern `Open p
    and tc : phrase -> phrase * Types.datatype * Usage.t = type_check context
    and expr_string (p : Sugartypes.phrase) : string =
      let pos = WithPos.pos p in
      Position.Resolved.resolve pos |> Position.Resolved.source_expression
    and erase_cases = List.map (fun ((p, _, _t), (e, _, _)) -> p, e) in
    let type_cases binders =
      let pt = Types.fresh_type_variable (lin_any, res_any) in
      let bt = Types.fresh_type_variable (lin_any, res_any) in
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
             let () = Env.iter (fun v t -> let uses = Usage.uses_of v (usages body) in
                                           if uses <> 1 then
                                             if Types.Unl.can_type_be t then
                                               Types.Unl.make_type t
                                             else
                                               Gripers.non_linearity pos uses v t)
                               (pattern_env pat) in
             let vs = Env.domain (pattern_env pat) in
             let us = Usage.restrict (usages body) vs in
             (pat, update_usages body us)::binders)
          binders []
      in
        binders, pt, bt in

    let check_recursive_usage e v context =
      (* register wildness if this is a recursive variable instance *)
      if StringSet.mem v context.rec_vars then
        begin
          let wild_open = Types.(open_row default_effect_subkind closed_wild_row) in
          unify ~handle:Gripers.recursive_usage (no_pos (Types.Effect wild_open), (uexp_pos e, Types.Effect context.effect_row));
        end;
    in

    (* Auxiliarty functions for control-flow linearity *)
    (* check if an effect row type can be made linear *)
    let _canlin_effrow = fun row ->
      (* trick: for effect row types, Unl means Lin, Any means Any *)
      Types.Unl.can_type_be row
    in
    let makelin_effrow = fun row ->
      (* trick: for effect row types, Unl means Lin, Any means Any *)
      if Types.Unl.can_type_be row then
        Types.Unl.make_type row
      else
        Gripers.die pos ("Effect row type " ^ Types.string_of_datatype row
          ^ " can not be made linear (represented by Unl).")
    in
    (* check if a term uses any linear variable *)
    let _haslin_term = fun usages ->
      (* trick: for effect row types, Unl means Lin, Any means Any *)
      let env = context.var_env in
      Usage.fold
        (fun v _ b ->
            let t = Env.find v env in
            b || not (Types.Unl.can_type_be t))
        usages false
    in
    (* make all variables in a term unlimited  *)
    let makeunl_term = fun usages ->
      let env = context.var_env in
      Usage.iter
        (fun v _ ->
            let t = Env.find v env in
            if Types.Unl.can_type_be t then
              Types.Unl.make_type t
            else
              Gripers.die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t
                ^ " is used in a non-linear continuation."))
        usages
    in
    (* update control-flow linearity without automatically inserting xlin *)
    let update_cflinearity p usages =
      (LinCont.enabled (fun () ->
        if (LinCont.is_bound_by_linlet context)
          (* make `context.effect_row` linear if the current term is bound by a linlet *)
          then makelin_effrow (context.effect_row)
          else ();
        if (not (LinCont.is_in_linlet context))
          (* make all vars in `p` unlimited if the current term is in the body of an unlet *)
          then makeunl_term usages
          else ();
        (match p with
          | Linlet _ -> LinCont.update_in_linlet context true
          | Unlet _ -> LinCont.update_in_linlet context false
          | DoOperation (_,_,_,lin) ->
              if lin = DeclaredLinearity.Unl then LinCont.update_in_linlet context false
          | _ -> ())
      ))
    in
    (* update control-flow linearity with automatically inserting xlin
       This function isn't very useful as usually we don't know the
       linearity of vars at the beginning until they are unified
       (e.g., applied to functions). *)
    (* let update_cflinearity_auto_xlin p usages =
      (LinCont.enabled (fun () ->
        if (LinCont.is_bound_by_linlet context)
          (* make `context.effect_row` linear if the current term is bound by a linlet *)
          then makelin_effrow (context.effect_row)
          else ();
        if (not (LinCont.is_in_linlet context))
          then
            if haslin_term usages then (
              (* automatically insert a xlin if the term uses linear variables *)
              LinCont.update_in_linlet context true;
              makelin_effrow (context.effect_row)
            )
            (* make all vars in `p` unlimited if the current term is in the body of an unlet *)
            else makeunl_term usages
          else ();
        (match p with
          | Linlet _ -> LinCont.update_in_linlet context true
          | Unlet _ -> LinCont.update_in_linlet context false
          | DoOperation (_,_,_,lin) ->
              if lin = DeclaredLinearity.Unl then LinCont.update_in_linlet context false
          | _ -> ())
      ))
    in *)
    let find_opname phrase =
      let o = object (o)
        inherit SugarTraversals.fold as super
        val mutable opname = None

        method opname = match opname with
          | Some name -> name
          | None -> failwith "Operation with no name"

        method! phrasenode = function
          | Operation name -> opname <- Some name ; o
          | p -> super#phrasenode p
      end in
      let o = o#phrase phrase in
      o#opname
    in

    let module T = Types in
    let e, t, usages =
      match (expr : phrasenode) with
        | Var v ->
           begin
           try
             check_recursive_usage (WithPos.make ~pos expr) v context;
             let (tyargs, t) = Utils.instantiate context.var_env v in
             let e = match tyargs with
               | [] -> Var v
               | _ -> tappl (FreezeVar v, tyargs)
             in
             e, t, Usage.singleton v
           with
             Errors.UndefinedVariable _msg ->
             Gripers.die pos ("Unknown variable " ^ v ^ ".")
           end
        | FreezeVar v ->
           begin
           try
             check_recursive_usage (WithPos.make ~pos expr) v context;
             let t = Env.find v context.var_env in
             FreezeVar v, t, Usage.singleton v
           with
             NotFound _ ->
             Gripers.die pos ("Unknown variable " ^ v ^ ".")
           end
        | Section s -> type_section pos context s
        | FreezeSection s -> type_frozen_section context s
        (* literals *)
        | Constant c as c' ->
           c', Types.Primitive (Constant.type_of c), Usage.empty
        | TupleLit [p] ->
           let p = tc p in
              TupleLit [erase p], typ p, usages p (* When is a tuple not a tuple? *)
        | TupleLit ps ->
            let ps = List.map tc ps in
              TupleLit (List.map erase ps), Types.make_tuple_type (List.map typ ps), Usage.combine_many (List.map usages ps)
        | RecordLit (fields, rest) ->
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
                      StringMap.add label (T.Present t) field_env,
                      StringMap.add label T.Absent absent_field_env,
                      Usage.combine field_usages (usages e)))
                fields ([], StringMap.empty, StringMap.empty, Usage.empty) in
              begin match rest with
                | None ->
                   let r = T.Row (field_env, Unionfind.fresh T.Closed, false) in
                    RecordLit (alistmap erase fields, None), T.Record r, field_usages
                | Some r ->
                    let r : phrase * Types.datatype * Usage.t = tc r in

                    (* FIXME:

                       we need to explicitly instantiate quantifiers
                       like this *whenever* we do any kind of
                       elimination
                    *)

                    (* explicitly instantiate any quantifiers attached to r *)
                    let r =
                      let (tyargs, rtype) = Instantiate.typ (typ r) in
                      let r' = erase r in
                      with_dummy_pos (tappl (r'.node, tyargs)), rtype, usages r in

                    let rtype = typ r in

                    (* make sure rtype is a record type that doesn't match any of the existing fields *)
                    let row = T.Record (T.Row (absent_field_env, Types.fresh_row_variable (lin_any, res_any), false)) in
                    let () = unify ~handle:Gripers.extend_record
                      (pos_and_typ r, no_pos row) in

                    let rfield_env, rrow_var, lr =
                      TypeUtils.extract_row rtype |> Types.unwrap_row |> fst |> TypeUtils.extract_row_parts in
                    assert (lr = false);
                      (* attempt to extend field_env with the labels from rfield_env
                         i.e. all the labels belonging to the record r
                      *)
                    let field_env' =
                      StringMap.fold (fun label f field_env' ->
                                        match f with
                                          | T.Absent ->
                                              if StringMap.mem label field_env then
                                                field_env'
                                              else
                                                StringMap.add label T.Absent field_env'
                                          | T.Present t ->
                                              if StringMap.mem label field_env then
                                                failwith ("Could not extend record "^ expr_string (erase r)^" (of type "^
                                                            Types.string_of_datatype rtype^") with the label "^
                                                            label^
                                                            " (of type"^Types.string_of_datatype (T.Record (T.Row (field_env, Unionfind.fresh T.Closed, false)))^
                                                            ") because the labels overlap")
                                              else
                                                StringMap.add label (T.Present t) field_env'
                                          | T.Meta _ -> assert false
                                          | _ -> raise Types.tag_expectation_mismatch)
                        rfield_env field_env in
                    let usages = Usage.combine field_usages (usages r) in
                      RecordLit (alistmap erase fields, Some (erase r)), T.Record (T.Row (field_env', rrow_var, false)), usages
              end
        | ListLit (es, _) ->
            begin match List.map tc es with
              | [] ->
                  let t = Types.fresh_type_variable (lin_any, res_any) in
                    ListLit ([], Some t), T.Application (Types.list, [PrimaryKind.Type, t]), Usage.empty
              | e :: es ->
                  List.iter (fun e' -> unify ~handle:Gripers.list_lit (pos_and_typ e, pos_and_typ e')) es;
                  ListLit (List.map erase (e::es), Some (typ e)), T.Application (Types.list, [PrimaryKind.Type, typ e]), Usage.combine_many (List.map usages (e::es))
            end
        | FunLit (argss_prev, lin, fnlit, location) ->
            let (pats, body) = Sugartypes.get_normal_funlit fnlit in
            (* vs: names of all variables in the parameter patterns *)
            let vs = check_for_duplicate_names pos (List.flatten pats) in
            let (pats_init, pats_tail) = from_option ([], []) (unsnoc_opt pats) in
            let tpc' = if DeclaredLinearity.is_linear lin then tpc else tpcu in
            let pats = List.append (List.map (List.map tpc') pats_init)
                                   [List.map tpc pats_tail] in
            let pat_env = List.fold_left (List.fold_left (fun env pat' -> Env.extend env (pattern_env pat'))) Env.empty pats in
            let env' = Env.extend context.var_env pat_env in

            make_mono pats;

            (* type of the effects in the body of the lambda *)
            let effects = Types.make_empty_open_row default_effect_subkind in
            let body = type_check ({context with
                                      var_env = env';
                                      effect_row = effects;
                                      cont_lin = LinCont.getnew ()}) body in

            (* make types of parameters unlimited if they are not used exactly once *)
            let () =
              Env.iter (fun v t ->
                let uses = Usage.uses_of v (usages body) in
                  if uses <> 1 then
                    if Types.Unl.can_type_be t then
                      Types.Unl.make_type t
                    else
                      Gripers.non_linearity pos uses v t)
                pat_env in

            (* make types of env vars unlimited if they are used in unlimited functions *)
            let () =
              if DeclaredLinearity.is_nonlinear lin then
                Usage.iter
                  (fun v _ ->
                    if not (List.mem v vs) then
                      let t = Env.find v env' in
                      if Types.Unl.can_type_be t then
                        Types.Unl.make_type t
                      else
                        Gripers.die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^ " is used in a non-linear function literal."))
                  (usages body)
              else ()
            in
            let ftype = make_ft lin pats effects (typ body) in

            (* Ensure the previously inferred arguments types match up with the
               current one. While we will detect this during {!DesugarFuns},
               it's good to catch this when it actually happens. *)
            (match argss_prev with
             | None -> ()
             | Some argss_prev ->
                let rec ft =
                  function
                  | [p, effects] -> make_ftcon lin (p, effects, typ body)
                  | (p, e)::ps -> make_ftcon lin (p, e, ft ps)
                  | [] -> raise (internal_error "Empty argument list")
                in
                let ftype_prev = ft argss_prev in
                unify ~handle:Gripers.bind_fun_annotation (no_pos ftype, no_pos ftype_prev));

            (* To correctly determine the arity of nested anonymous functions we
               need to make sure we only include arguments of the current FunLit
               and not the nested ones. *)
            let curried_argument_count = List.length pats in
            let argss =
              let rec arg_types : (Types.datatype * int)
                               -> (Types.datatype * Types.row) list =
                function
                  | _, 0 -> []
                  | T.Function (args, effects, t), c
                  | T.Lolli    (args, effects, t), c ->
                     (args, effects) :: arg_types (t, c-1)
                  | _, _ -> failwith "Error reconstructing FunLit Type"
              in
                arg_types (ftype, curried_argument_count) in

            let e = FunLit (Some argss, lin, NormalFunlit (List.map (List.map erase_pat) pats, erase body), location) in
            let vs' = List.fold_right Ident.Set.add vs Ident.Set.empty in
            e, ftype, Usage.restrict (usages body) vs'

        | ConstructorLit (c, None, _) ->
            let type' = T.Variant (Types.make_singleton_open_row
                                    (c, T.Present Types.unit_type)
                                    (lin_any, res_any)) in
              ConstructorLit (c, None, Some type'), type', Usage.empty

        | ConstructorLit (c, Some v, _) ->
            let v = tc v in
            let type' = T.Variant (Types.make_singleton_open_row
                                    (c, T.Present (typ v))
                                    (lin_any, res_any)) in
              ConstructorLit (c, Some (erase v), Some type'), type', usages v

        (* database *)
        | DatabaseLit (name, (driver, args)) ->
            let driver = opt_map tc driver
            and args   = opt_map tc args
            and name   = tc name in
              DatabaseLit (erase name, (opt_map erase driver, opt_map erase args)), T.Primitive Primitive.DB,
              Usage.combine_many [from_option Usage.empty (opt_map usages driver); from_option Usage.empty (opt_map usages args); usages name]
        | TableLit {
            tbl_name = tname;
            tbl_type = (tmp, dtype, Some (read_row, write_row, needed_row));
            tbl_field_constraints;
            tbl_keys;
            tbl_temporal_fields;
            tbl_database
        } ->
            let tname = tc tname
            and tbl_database = tc tbl_database
            and tbl_keys = tc tbl_keys in
            let () = unify ~handle:Gripers.table_name (pos_and_typ tname, no_pos Types.string_type)
            and () = unify ~handle:Gripers.table_db (pos_and_typ tbl_database, no_pos Types.database_type)
            and () = unify ~handle:Gripers.table_keys (pos_and_typ tbl_keys, no_pos Types.keys_type) in
            let tlit =
                TableLit {
                    tbl_name = erase tname;
                    tbl_type = (tmp, dtype, Some (read_row, write_row, needed_row));
                    tbl_field_constraints;
                    tbl_keys = erase tbl_keys;
                    tbl_temporal_fields;
                    tbl_database = erase tbl_database
                }
            in
            tlit,
            T.Table (tmp, read_row, write_row, needed_row),
            Usage.combine (usages tname) (usages tbl_database)
        | TableLit _ -> assert false
        | LensLit (table, _) ->
           relational_lenses_guard pos;
           let open Lens in
           let table = tc table in
           let cols = Lens_type_conv.sort_cols_of_table ~table:"" (typ table) in
           let lens_sort = Sort.make cols in
           let typ = Lens.Type.ConcreteLens lens_sort in
           LensLit (erase table, Some typ), T.Lens typ, usages table
        | LensSerialLit(lens, columns, _) ->
          let lens = tc lens in
          let typ =
            let columns = Lens.Alias.Set.of_list columns in
            let lens = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) |> Lens.Type.set_serial ~columns in
            lens in
          LensSerialLit (erase lens, columns, Some typ), T.Lens typ, usages lens
        | LensKeysLit (table, keys, _) ->
           relational_lenses_guard pos;
           let open Lens in
           let table = tc table in
           let columns = Lens_type_conv.sort_cols_of_table ~table:"" (typ table) in
           let keys = Lens_sugar_conv.cols_of_phrase keys in
           let fds = Fun_dep.Set.key_fds ~keys ~cols:(Column.List.present_aliases columns) in
           let lens_sort = Sort.make ~fds columns in
           let typ = Lens.Type.ConcreteLens lens_sort in
           LensLit (erase table, Some typ), T.Lens typ, usages table
        | LensFunDepsLit (table, fds, _) ->
           relational_lenses_guard pos;
           let table = tc table in
           let columns = Lens_type_conv.sort_cols_of_table ~table:"" (typ table) in
           let typ =
             Lens.Type.type_lens_fun_dep ~fds ~columns
             |> Lens_errors.unpack_type_lens_result ~die:(Gripers.die pos) in
           LensLit (erase table, Some typ), T.Lens typ, usages table
        | LensDropLit (lens, drop, key, default, _) ->
           relational_lenses_guard pos;
           let open Lens in
           let lens = tc lens in
           let default = tc default in
           let typ =
             let lens = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
             let default = typ default |> Lens_type_conv.lens_phrase_type_of_type |> fun a -> [a] in
             let drop = [drop] in
             let key = Alias.Set.singleton key in
             Type.type_drop_lens lens ~default ~drop ~key
             |> Lens_errors.unpack_type_drop_lens_result ~die:(Gripers.die pos) in
           LensDropLit (erase lens, drop, key, erase default, Some typ), T.Lens typ, Usage.combine (usages lens) (usages default)
        | LensSelectLit (lens, predicate, _) ->
           relational_lenses_guard pos;
           let lens = tc lens in
           let tpredicate = tc predicate in
           let typ =
             let tlens = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
             let trow = Lens.Type.sort tlens |> Lens.Sort.record_type in
             let {tycon_env = context;_} = context in
             let ltrow = Lens_type_conv.type_of_lens_phrase_type ~context trow in
             let tmatch = Types.make_pure_function_type [ltrow] Types.bool_type in
             unify (pos_and_typ tpredicate, (exp_pos lens, tmatch)) ~handle:Gripers.lens_predicate;
             if Lens_sugar_conv.is_static trow predicate
             then
               let predicate = Lens_sugar_conv.lens_sugar_phrase_of_sugar predicate in
               let typ = Lens.Type.type_select_lens tlens ~predicate
                         |> Lens_errors.unpack_type_select_lens_result ~die:(Gripers.die pos) in
               typ
             else
               let typ = Lens.Type.type_select_lens_dynamic tlens
                         |> Lens_errors.unpack_type_select_lens_result ~die:(Gripers.die pos) in
               typ
           in
           LensSelectLit(erase lens, erase tpredicate, Some typ), T.Lens typ, usages lens
        | LensJoinLit (lens1, lens2, on, left, right, _) ->
           relational_lenses_guard pos;
           let lens1 = tc lens1
           and lens2 = tc lens2 in
           let typ =
             let lens1 = typ lens1 |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
             let lens2 = typ lens2 |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
             let on = Lens_sugar_conv.cols_of_phrase on |> List.map (fun a -> a, a, a) in
             let del_left = Lens_sugar_conv.lens_sugar_phrase_of_sugar left in
             let del_right = Lens_sugar_conv.lens_sugar_phrase_of_sugar right in
             Lens.Type.type_join_lens lens1 lens2 ~on ~del_left ~del_right
           |> Lens_errors.unpack_type_join_lens_result ~die:(Gripers.die pos) in
           LensJoinLit (erase lens1, erase lens2, on, left, right, Some typ), T.Lens typ, Usage.combine (usages lens1) (usages lens2)
        | LensGetLit (lens, _) ->
           relational_lenses_guard pos;
           let lens = tc lens in
           let typ = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
           Lens.Type.ensure_checked typ |> Lens_errors.unpack_lens_checked_result ~die:(Gripers.die pos);
           let sort = Lens.Type.sort typ in
           let {tycon_env = context;_} = context in
           let trowtype = Lens.Sort.record_type sort |> Lens_type_conv.type_of_lens_phrase_type ~context in
           LensGetLit (erase lens, Some trowtype), Types.make_list_type trowtype, usages lens
        | LensCheckLit (lens, _) ->
          relational_lenses_guard pos;
           let lens = tc lens in
           let typ = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
           let typ = Lens.Type.make_checked typ in
           LensCheckLit (erase lens, Some typ), T.Lens typ, usages lens
        | LensPutLit (lens, data, _) ->
           relational_lenses_guard pos;
           let make_tuple_type = Types.make_tuple_type in
           let lens = tc lens in
           let typ = typ lens |> Lens_type_conv.lens_type_of_type ~die:(Gripers.die pos) in
           Lens.Type.ensure_checked typ |> Lens_errors.unpack_lens_checked_result ~die:(Gripers.die pos);
           let data = tc data in
           let trow = Lens.Type.sort typ |> Lens.Sort.record_type in
           let {tycon_env = context;_} = context in
           let ltrow = Lens_type_conv.type_of_lens_phrase_type ~context trow in
           unify (pos_and_typ data, (exp_pos lens, Types.make_list_type ltrow)) ~handle:Gripers.lens_put_input;
           LensPutLit (erase lens, erase data, Some Types.unit_type), make_tuple_type [], Usage.combine (usages lens) (usages data)
        | DBDelete (tdel, pat, from, where) ->
            (* Check the temporal_deletion to ascertain the temporality *)
            let (tmp, tdel, tmp_usages) =
                match tdel with
                    | None ->
                        (Temporality.current, tdel, Usage.empty)
                    | Some (ValidTimeDeletion (SequencedDeletion { validity_from; validity_to })) ->
                        let validity_from = tc validity_from in
                        let validity_to = tc validity_to in
                        (Temporality.valid,
                         Some (ValidTimeDeletion (SequencedDeletion {
                             validity_from = erase validity_from;
                             validity_to = erase validity_to
                         })),
                         Usage.combine (usages validity_from) (usages validity_to))
                    | Some (ValidTimeDeletion _) ->
                        (Temporality.valid, tdel, Usage.empty)
                    | Some TransactionTimeDeletion ->
                        (Temporality.transaction, tdel, Usage.empty)
            in
            let pat  = tpc pat in
            let from = tc from in
            let read  = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let write = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let needed = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let () = unify ~handle:Gripers.delete_table
              (pos_and_typ from, no_pos (T.Table (tmp, read, write, needed))) in

            let () =
              let expected =
                match tdel with
                  | Some (ValidTimeDeletion NonsequencedDeletion) ->
                      Types.make_valid_time_data_type read
                  | _ -> read in
              unify ~handle:Gripers.delete_pattern (ppos_and_typ pat, no_pos expected) in

            let hide =
              let bs = Env.domain (pattern_env pat) in
              (fun usages -> Usage.restrict usages bs) in

            let inner_effects = Types.make_empty_closed_row () in
            let context' = bind_effects (context ++ pattern_env pat) inner_effects in

            let where = opt_map (type_check context') where in
            let () =
              opt_iter
                (fun e -> unify ~handle:Gripers.delete_where (pos_and_typ e, no_pos Types.bool_type)) where in

            (* delete is wild *)
            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.delete_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects))
            in
              DBDelete (tdel, erase_pat pat, erase from, opt_map erase where), Types.unit_type,
              Usage.combine_many [
                  usages from;
                  hide (from_option Usage.empty (opt_map usages where));
                  hide tmp_usages
              ]
        | DBInsert (tmp_ins, into, labels, values, id) ->
            let temporality =
                match tmp_ins with
                  | None -> Temporality.current
                  | Some (ValidTimeInsertion _) -> Temporality.valid
                  | Some (TransactionTimeInsertion) -> Temporality.transaction
            in
            let into   = tc into in
            let values = tc values in
            let id = opt_map tc id in
            let read  = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let write = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let needed = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let () = unify ~handle:Gripers.insert_table
              (pos_and_typ into, no_pos (T.Table (temporality, read, write, needed))) in

            let field_env =
              List.fold_right
                (fun name field_env ->
                   if StringMap.mem name field_env then
                     Gripers.die pos "Duplicate labels in insert expression."
                   else
                     StringMap.add name (T.Present (Types.fresh_type_variable (lin_any, res_base))) field_env)
                labels StringMap.empty
            in

            (* Check that the fields in the type of values match the declared labels *)
            (* In the case of a valid-time sequenced insert, we need to be inserting valid-time metadata *)
            let () =
              match tmp_ins with
                | Some (ValidTimeInsertion SequencedInsertion) ->
                    let ty =
                      T.Record (T.Row (field_env, Unionfind.fresh T.Closed, false))
                        |> Types.make_valid_time_data_type
                        |> Types.make_list_type in
                    unify ~handle:Gripers.sequenced_insert_values (pos_and_typ values, no_pos ty)
                | _ ->
                    unify ~handle:Gripers.insert_values
                      (pos_and_typ values,
                       no_pos (Types.make_list_type (T.Record (T.Row (field_env, Unionfind.fresh T.Closed, false)))))
            in

            let needed_env =
              StringMap.map
                (fun _f -> Types.fresh_presence_variable (lin_any, res_base))
                field_env in

            (* all fields being inserted must be present in the read row *)
            let row = T.Row (field_env, Types.fresh_row_variable (lin_any, res_base), false) in
            let () = unify ~handle:Gripers.insert_read
              (no_pos read, no_pos (T.Record row)) in

            (* all fields being inserted must be present in the write row *)
            let row = T.Row (field_env, Types.fresh_row_variable (lin_any, res_base), false) in
            let () = unify ~handle:Gripers.insert_write
              (no_pos write, no_pos (T.Record row)) in

            (* all fields being inserted must be consistent with the needed row *)
            let row = T.Row (needed_env, Unionfind.fresh T.Closed, false) in
            let () = unify ~handle:Gripers.insert_needed
              (no_pos needed, no_pos (T.Record row)) in

            (* insert returning ... *)
            let return_type =
              match id with
                | None -> Types.unit_type
                | Some ({node=(id : phrasenode); _}, _, _) ->
                    begin
                      match id with
                        | Constant (Constant.String id) ->
                            (* HACK: The returned column is encoded as
                               a string.  We check here that it
                               appears as a column in the read type of
                               the table.
                            *)
                           let row =
                             T.Row (StringMap.singleton id (T.Present Types.int_type),
                               Types.fresh_row_variable (lin_any, res_base), false) in
                            unify
                              ~handle:Gripers.insert_id
                              (no_pos read,
                               no_pos (T.Record row));
                            Types.int_type
                        | _ -> assert false
                    end in

            (* insert is wild *)
            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.insert_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects))
            in
              DBInsert (tmp_ins, erase into, labels, erase values, opt_map erase id), return_type,
              Usage.combine_many [usages into; usages values; from_option Usage.empty (opt_map usages id)]
        | DBUpdate (tmp_upd, pat, from, where, set) ->
            (* Need temporality to be able to deduce pattern type *)
            let tmp =
                match tmp_upd with
                    | None -> Temporality.current
                    | Some (ValidTimeUpdate _) -> Temporality.valid
                    | Some TransactionTimeUpdate -> Temporality.transaction
            in
            let pat  = tpc pat in
            let from = tc from in
            let read =  T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let write = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let needed = T.Record (Types.make_empty_open_row (lin_any, res_base)) in
            let () = unify ~handle:Gripers.update_table
              (pos_and_typ from, no_pos (T.Table (tmp, read, write, needed))) in

            let hide =
              let bs = Env.domain (pattern_env pat) in
              (fun usages -> Usage.restrict usages bs)
            in

            (* the pattern should match the read type *)
            (* Nonsequenced valid-time queries get VT metadata *)
            let () =
                match tmp_upd with
                    | Some (ValidTimeUpdate (NonsequencedUpdate _)) ->
                        let ty = Types.make_valid_time_data_type read in
                        unify ~handle:Gripers.valid_update_pattern (ppos_and_typ pat, no_pos ty)
                    | _ ->
                        unify ~handle:Gripers.update_pattern (ppos_and_typ pat, no_pos read)
            in

            let inner_effects = Types.make_empty_closed_row () in
            let context' = bind_effects (context ++ pattern_env pat) inner_effects in
            let tc' = type_check context' in

            let where = opt_map tc' where in

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
                       (name, exp)::set, StringMap.add name (T.Present (typ exp)) field_env)
                set ([], StringMap.empty) in

            let needed_env =
              StringMap.map
                (fun _f -> Types.fresh_presence_variable (lin_any, res_base))
                field_env in

            (* all fields being updated must be present in the read row *)
            let () = unify ~handle:Gripers.update_read
              (no_pos read, no_pos (T.Record (T.Row (field_env, Types.fresh_row_variable (lin_any, res_base), false)))) in

            (* all fields being updated must be present in the write row *)
            let () = unify ~handle:Gripers.update_write
              (no_pos write, no_pos (T.Record (T.Row (field_env, Types.fresh_row_variable (lin_any, res_base), false)))) in

            (* all fields being updated must be consistent with the needed row *)
            let () = unify ~handle:Gripers.update_needed
              (no_pos needed, no_pos (T.Record (T.Row (needed_env, Types.fresh_row_variable (lin_any, res_base), false)))) in

            (* Typecheck any valid-time fields and calculate usages *)
            let (tmp_upd, tmp_usages) =
                match tmp_upd with
                    (* No extra info or TCing needed *)
                    | None
                    | Some TransactionTimeUpdate
                    | Some (ValidTimeUpdate (CurrentUpdate)) ->
                        (tmp_upd, Usage.empty)
                    | Some (ValidTimeUpdate (SequencedUpdate { validity_from; validity_to })) ->
                        let validity_from = tc' validity_from in
                        let validity_to = tc' validity_to in
                        (* Ensure validity from / validity to are DateTimes *)
                        let () = unify ~handle:Gripers.sequenced_update_datetime
                          (pos_and_typ validity_from, no_pos (T.Primitive Primitive.DateTime))
                        in
                        let () = unify ~handle:Gripers.sequenced_update_datetime
                          (pos_and_typ validity_to, no_pos (T.Primitive Primitive.DateTime))
                        in
                        let usages = Usage.combine (usages validity_from) (usages validity_to) in
                        let tmp_upd = Some (ValidTimeUpdate (SequencedUpdate {
                            validity_from = erase validity_from;
                            validity_to = erase validity_to
                        })) in
                        (tmp_upd, usages)
                    | Some (ValidTimeUpdate (NonsequencedUpdate { from_time; to_time })) ->
                        let tc_date x =
                            let x = tc' x in
                            let () =
                                unify ~handle:Gripers.nonsequenced_update_datetime
                                  (pos_and_typ x, no_pos (T.Primitive Primitive.DateTime))
                            in
                            x
                        in
                        let from_time = OptionUtils.opt_map tc_date from_time in
                        let to_time = OptionUtils.opt_map tc_date to_time in
                        let usages =
                            List.map (OptionUtils.opt_as_list) [from_time; to_time]
                            |> List.concat
                            |> List.map usages
                            |> Usage.combine_many
                        in
                        let from_time = OptionUtils.opt_map erase from_time in
                        let to_time = OptionUtils.opt_map erase to_time in
                        (Some (ValidTimeUpdate (NonsequencedUpdate { from_time; to_time })), usages)
            in

            (* update is wild *)
            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.update_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects))
            in
              DBUpdate (tmp_upd, erase_pat pat, erase from,
                opt_map erase where, List.map (fun (n,(p,_,_)) -> n, p) set),
              Types.unit_type,
              Usage.combine_many
                ([ usages from;
                   hide tmp_usages;
                   hide (from_option Usage.empty (opt_map usages where))]
                  @
                  (List.map hide (List.map (usages -<- snd) set)))
        | DBTemporalJoin (tmp, body, _) ->
            let outer_effects =
              Types.make_empty_open_row default_effect_subkind in
            let inner_effects = Types.make_empty_closed_row () in
            let () = unify ~handle:Gripers.temporal_join_effects
              (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects)) in
            let body = type_check (bind_effects context inner_effects) body in
            (* Given body type [A], tt_join should result in [TransactionTime(A)]
             * and likewise for valid time  *)
            let body_type = Types.fresh_type_variable (lin_any, res_any) in
            let () = unify ~handle:Gripers.temporal_join_body
              (pos_and_typ body, no_pos (Types.make_list_type body_type)) in

            let result_type =
              let open Temporality in
              match tmp with
                | Transaction ->
                    body_type
                    |> Types.make_transaction_time_data_type
                    |> Types.make_list_type
                | Valid ->
                    body_type
                    |> Types.make_valid_time_data_type
                    |> Types.make_list_type
                | _ -> assert false (* Impossible to construct *) in
            DBTemporalJoin (tmp, erase body, Some result_type), result_type,
            (usages body)
        | Query (range, policy, p, _) ->
            let open QueryPolicy in
            let range, outer_effects, range_usages =
              match range with
                | None -> None, Types.make_empty_open_row default_effect_subkind, Usage.empty
                | Some (limit, offset) ->
                    let limit = tc limit in
                    let () = unify ~handle:Gripers.range_bound (pos_and_typ limit, no_pos Types.int_type) in
                    let offset = tc offset in
                    let () = unify ~handle:Gripers.range_bound (pos_and_typ offset, no_pos Types.int_type) in
                    let outer_effects =
                      Types.(open_row default_effect_subkind closed_wild_row)
                    in
                    Some (erase limit, erase offset), outer_effects, Usage.combine (usages limit) (usages offset)
            in
            let inner_effects = Types.make_empty_closed_row () in
            let () = unify ~handle:Gripers.query_outer
              (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in
            let () =
              match policy with
                | Nested -> ()
                | Mixing
                | Delat
                | Flat  ->
                     let shape =
                       Types.make_list_type
                         (T.Record (T.Row (StringMap.empty,
                            Types.fresh_row_variable (lin_any, res_base), false))) in
                     unify ~handle:Gripers.query_base_row (pos_and_typ p, no_pos shape)
            in
            Query (range, policy, erase p, Some (typ p)), typ p, Usage.combine (range_usages) (usages p)
        (* mailbox-based concurrency *)
        | Spawn (Wait, l, p, old_inner) ->
            assert (l = NoSpawnLocation);
            (* (() -{b}-> d) -> d *)
            let inner_effects = Types.make_empty_open_row default_effect_subkind in
            (* TODO: check if pid_type is actually needed somewhere *)
            (* let pid_type = Application (Types.process, [Row inner_effects]) in *)
            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.spawn_wait_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in

            begin
              match old_inner with
              | None -> ()
              | Some old_inner ->
                 unify ~handle:Gripers.spawn_inconsistent (no_pos (T.Effect inner_effects), no_pos (T.Effect old_inner))
            end;

            let return_type = typ p in
              Spawn (Wait, l, erase p, Some inner_effects), return_type, usages p
        | Spawn (k, given_loc, p, old_inner) ->
            (* Location -> (() ~e~@ _) -> Process (e) *)
            (match given_loc with
              | ExplicitSpawnLocation loc_phr ->
                  let target_ty = T.Application (Types.spawn_location, []) in
                  let t = tc loc_phr in
                  let _ = unify ~handle:Gripers.spawn_location (pos_and_typ t, no_pos target_ty) in ()
              | _ -> ());

            let pid_effects = Types.make_empty_open_row default_effect_subkind in

            (* The various spawn methods have the type [(() ~e~> _) -> Process (e)]
               (unless session exceptions are enabled, then the spawned body may
               raise the SessionFail exception.

               We need to type the body of the spawn block with the effect type [{
               wild | e }] (and maybe SessionFail) (call this "inner_effects"), and
               then use e (pid_effects) as the type argument to the process.
             *)
            let inner_effects = Types.row_with Types.wild_present pid_effects in
            let inner_effects =
              if Settings.get Basicsettings.Sessions.exceptions_enabled &&
                 Settings.get Basicsettings.Sessions.expose_session_fail
              then
                let ty = LinCont.make_operation_type [] (Types.empty_type) in
                Types.row_with (Value.session_exception_operation, T.Present ty) inner_effects
              else
                inner_effects in
            let pid_type = T.Application (Types.process, [PrimaryKind.Row, pid_effects]) in
            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.spawn_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects)) in
            let p = type_check (bind_effects context inner_effects) p in
            if not (Types.Unl.can_type_be (typ p)) then
              Gripers.die pos ("Spawned processes cannot produce values of linear type (here " ^ Types.string_of_datatype (typ p) ^ ")");

            (* If we've previously typed this spawn block, ensure we've inferred the
               same types this time round.

               This is mostly used to ensure desugaring passes generate the correct
               type. *)
            begin match old_inner with
            | None -> ()
            | Some old_inner ->
               unify ~handle:Gripers.spawn_inconsistent (no_pos (T.Effect pid_effects), no_pos (T.Effect old_inner))
            end;

            Spawn (k, given_loc, erase p, Some pid_effects), pid_type, usages p
        | Receive (binders, _) ->
            let mb_type = Types.fresh_type_variable (lin_any, res_any) in
            let effects =
              Types.(open_row default_effect_subkind (row_with (hear_present mb_type) closed_wild_row))
            in
            let () = unify ~handle:Gripers.receive_mailbox
              (no_pos (T.Record context.effect_row), no_pos (T.Record effects)) in

            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Gripers.receive_patterns
              (no_pos mb_type, no_pos pattern_type)
            in
              Receive (erase_cases binders, Some body_type), body_type, usages_cases binders

        (* session-based concurrency *)
        | Select (l, e) ->
           let e = tc e in
           let selected_session = Types.fresh_type_variable (lin_any, res_session) in
           unify ~handle:Gripers.selection
                 (pos_and_typ e, no_pos (T.Select (Types.make_singleton_open_row
                                                              (l, T.Present selected_session)
                                                              (lin_any, res_session))));
           Select (l, erase e), selected_session, usages e
        | Offer (e, branches, _) ->
           let e = tc e in
           let branches, pattern_type, body_type = type_cases branches in
           let r = Types.make_empty_open_row (lin_any, res_session) in
             unify ~handle:Gripers.offer_variant (no_pos pattern_type, no_pos (T.Variant r));
             unify ~handle:Gripers.offer_patterns (pos_and_typ e, no_pos (T.Choice r));
             Offer (erase e, erase_cases branches, Some body_type), body_type, Usage.combine (usages e) (usages_cases branches)

        (* No comment *)
        | CP p ->
           let (p, t, u) = type_cp context p in
           CP p, t, u

        (* applications of various sorts *)
        | UnaryAppl ((_, op), p) ->
            let tyargs, opt, op_usage = type_unary_op pos context op
            and p = tc p
            and rettyp = Types.fresh_type_variable (lin_any, res_any) in
              unify ~handle:Gripers.unary_apply
                ((UnaryOp.to_string op, opt),
                 no_pos (T.Function (Types.make_tuple_type [typ p], context.effect_row, rettyp)));
              UnaryAppl ((tyargs, op), erase p), rettyp, Usage.combine (usages p) op_usage
        | InfixAppl ((_, op), l, r) ->
            let tyargs, opt, op_usages = type_binary_op pos context op in
            let l = tc l
            and r = tc r
            and rettyp = Types.fresh_type_variable (lin_any, res_any) in
              unify ~handle:Gripers.infix_apply
                ((BinaryOp.to_string op, opt),
                 no_pos (T.Function (Types.make_tuple_type [typ l; typ r],
                                    context.effect_row, rettyp)));
              InfixAppl ((tyargs, op), erase l, erase r), rettyp, Usage.combine_many [usages l; usages r; op_usages]
        | RangeLit (l, r) ->
            let l, r = tc l, tc r in
            let outer_effects =
              Types.(open_row default_effect_subkind closed_wild_row)
            in
            let () = unify ~handle:Gripers.range_bound  (pos_and_typ l,
                                                         no_pos Types.int_type)
            and () = unify ~handle:Gripers.range_bound  (pos_and_typ r,
                                                         no_pos Types.int_type)
            and () = unify ~handle:Gripers.range_wild   (no_pos (T.Record context.effect_row),
                                                         no_pos (T.Record outer_effects))
            in RangeLit (erase l, erase r),
               Types.make_list_type Types.int_type,
               Usage.combine (usages l) (usages r)

        (* Tag: FnAppl begin *)
        | FnAppl (f, ps) ->
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
                  | T.ForAll (_, (T.Function _ | T.Lolli _)) as t ->
                     begin
                       match Instantiate.typ t with
                       | tyargs, (T.Function (fps, fe, rettyp) | T.Lolli (fps, fe, rettyp) as t) ->
                          let mkft (a, e, r) = match t with
                            | T.Function _ -> T.Function (a, e, r)
                            | T.Lolli _ -> T.Lolli (a, e, r)
                            | _ -> assert false
                          in
                          (* the free type variables in the arguments (and effects) *)
                          let arg_vars = Types.TypeVarSet.union (Types.free_type_vars fps) (Types.free_row_type_vars fe) in
                          (* return true if this quantifier appears free in the arguments (or effects) *)
                          let free_in_arg q = Types.TypeVarSet.mem (Quantifier.to_var q) arg_vars in

                          (* quantifiers for the return type *)
                          let rqs =
                            if Settings.get  dodgey_type_isomorphism then
                              let rta, rqs =
                                List.map (fun q -> (q, Types.quantifier_of_type_arg q)) tyargs
                                |> List.filter (fun (_, q) -> free_in_arg q)
                                |> List.split
                              in
                              List.iter Generalise.rigidify_type_arg rta;
                              rqs
                            else
                              []
                          in

                          let rettyp = Types.for_all (rqs, rettyp) in
                          let ft = T.Function (fps, fe, rettyp) in
                          let f' = erase f in
                          let sugar_rqs = List.map SugarQuantifier.mk_resolved rqs in
                          let e = tabstr (sugar_rqs, FnAppl (with_dummy_pos (tappl (f'.node, tyargs)), List.map erase ps)) in
                          unify ~handle:Gripers.fun_apply
                            ((exp_pos f, ft), no_pos (mkft (Types.make_tuple_type (List.map typ ps),
                                                            context.effect_row,
                                                            rettyp)));
                          e, rettyp, Usage.combine_many (usages f :: List.map usages ps)
                       | _ -> assert false
                     end
                  | ft ->
                      let rettyp = Types.fresh_type_variable (lin_any, res_any) in
                      begin
                        let funt = no_pos (T.Function (Types.make_tuple_type (List.map typ ps),
                                                     context.effect_row, rettyp)) in
                        let lolt = no_pos (T.Lolli (Types.make_tuple_type (List.map typ ps),
                                                  context.effect_row, rettyp)) in
                        let term = (exp_pos f, ft) in
                        match Types.concrete_type ft with
                        | T.Function _ -> unify ~handle:Gripers.fun_apply (term, funt)
                        | T.Lolli _ -> unify ~handle:Gripers.fun_apply (term, lolt)
                        (* NOTE: non-linear function by default? why? *)
                        | _ -> unify_or ~handle:Gripers.fun_apply ~pos (term, funt) (term, lolt)
                      end;
                      FnAppl (erase f, List.map erase ps), rettyp, Usage.combine_many (usages f :: List.map usages ps)
              end
        | TAbstr (sugar_qs, e) ->
          if Utils.is_generalisable e then

            let qs = List.map SugarQuantifier.get_resolved_exn sugar_qs in

            (* Links being Links, the only way to ensure that we don't
               generalize any of the qs while type-checking e is to add
               a term variable to the environment that contains all variables
               from qs *)
            let dummy_var = Utils.dummy_source_name () in
            let type_of_quantifier q = Types.type_arg_of_quantifier q |> snd in
            let dummy_type = List.map type_of_quantifier qs |> Types. make_tuple_type in
            let context' = {context
                            with var_env = Env.bind dummy_var dummy_type context.var_env } in
            let e, t, u = type_check context' e in

            let free_flexible_vars = Types.free_flexible_type_vars t in
            if not (Types.TypeVarSet.is_empty free_flexible_vars) then
              Gripers.tabstr_ambiguous_type ~pos t;

            let t = Types.for_all(qs, t) in
            tabstr (sugar_qs, e.node), t, u
          else
            Gripers.generalise_value_restriction pos (uexp_pos e)
        | TAppl (e, tyargs) ->
           let e, t, u = tc e in

           (* Explicitly check that the arity and kinds match up. *)
           let args = List.map (snd ->- val_of) tyargs in
           let vars, _ = TypeUtils.split_quantified_type t in
           let rec quants_ok =
             let open PrimaryKind in
             function
             | _, [] -> ()
             | [], _ -> Gripers.type_apply pos (uexp_pos e) t args
             | (_, (Type, _)) :: qs, Type :: vs
             | (_, (Row, _)) :: qs, Row :: vs
             | (_, (Presence, _)) :: qs, Presence :: vs -> quants_ok (qs, vs)
             | _ -> Gripers.type_apply pos (uexp_pos e) t args in
           quants_ok (vars, List.map fst args);

           let t' = Instantiate.apply_type t args in
           TAppl (e, tyargs), t', u
        | Instantiate e ->
           let e, t, u = tc e in
           let (tyargs, t) = Instantiate.typ t in
           tappl' (e, tyargs), t, u
        | Generalise e ->
           let e, t, u = tc e in
           if Utils.is_generalisable e then
             let ((qs, _), t) = Utils.generalise ~unwrap:false context.var_env t in
             match qs with
             | [] -> WithPos.node e, t, u
             | _ ->
                let tyvars = List.map SugarQuantifier.mk_resolved qs in
                TAbstr (tyvars, e), t, u
           else
             Gripers.generalise_value_restriction pos (uexp_pos e)

        (* xml *)
        | Xml (tag, attrs, attrexp, children) ->
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
              Xml (tag,
                   List.map (fun (x,p) -> (x, List.map erase p)) attrs,
                   opt_map erase attrexp,
                   List.map erase children),
              Types.xml_type,
              Usage.combine_many (List.concat [ List.concat (List.map snd (alistmap (List.map usages) attrs))
                                              ; [ from_option Usage.empty (opt_map usages attrexp) ]
                                              ;  List.map usages children ] )
        | TextNode _ as t -> t, Types.xml_type, Usage.empty
        | Formlet (body, yields) ->
           let body = type_check context body in
           let env = extract_formlet_bindings (erase body) in
           let vs = Env.domain env in
           let context' = { context with effect_row = Types.closed_wild_row } ++ env in
           let yields = type_check context' yields in
           unify ~handle:Gripers.formlet_body (pos_and_typ body, no_pos Types.xml_type);
           (Formlet (erase body, erase yields),
            Instantiate.alias "Formlet" [PrimaryKind.Type, typ yields] context.tycon_env,
            Usage.combine (usages body) (Usage.restrict (usages yields) vs))
        | Page e ->
            let e = tc e in
              unify ~handle:Gripers.page_body (pos_and_typ e, no_pos Types.xml_type);
              Page (erase e), Instantiate.alias "Page" [] context.tycon_env, usages e
        | FormletPlacement (f, h, attributes) ->
            let t = Types.fresh_type_variable (lin_any, res_any) in

            let f = tc f
            and h = tc h
            and attributes = tc attributes in
            let () = unify ~handle:Gripers.render_formlet
              (pos_and_typ f, no_pos (Instantiate.alias "Formlet" [PrimaryKind.Type, t] context.tycon_env)) in
            let () = unify ~handle:Gripers.render_handler
              (pos_and_typ h, (exp_pos f,
                               Instantiate.alias "Handler" [PrimaryKind.Type, t] context.tycon_env)) in
            let () = unify ~handle:Gripers.render_attributes
              (pos_and_typ attributes, no_pos (Instantiate.alias "Attributes" [] context.tycon_env))
            in
              FormletPlacement (erase f, erase h, erase attributes), Types.xml_type, Usage.combine_many [usages f; usages h; usages attributes]
        | PagePlacement e ->
            let e = tc e in
            let pt = Instantiate.alias "Page" [] context.tycon_env in
              unify ~handle:Gripers.page_placement (pos_and_typ e, no_pos pt);
              PagePlacement (erase e), Types.xml_type, usages e
        | FormBinding (e, pattern) ->
            let e = tc e
            and pattern = tpc pattern in
            let a = Types.fresh_type_variable (lin_unl, res_any) in
            let ft = Instantiate.alias "Formlet" [PrimaryKind.Type, a] context.tycon_env in
              unify ~handle:Gripers.form_binding_body (pos_and_typ e, no_pos ft);
              unify ~handle:Gripers.form_binding_pattern (ppos_and_typ pattern, (exp_pos e, a));
              FormBinding (erase e, erase_pat pattern), Types.xml_type, usages e

        (* various expressions *)
        | Iteration (generators, body, where, orderby) ->
            begin
              unify ~handle:Gripers.iteration_unl_effect
                (no_pos (T.Effect context.effect_row),
                 no_pos (T.Effect (Types.make_empty_open_row default_effect_subkind)))
            end;
            let generators, generator_usages, environments =
              List.fold_left
                (fun (generators, generator_usages, environments) ->
                   function
                     | List (pattern, e) ->
                         let a = Types.fresh_type_variable (lin_unl, res_any) in
                         let lt = Types.make_list_type a in
                         let pattern = tpc pattern in
                         let e = tc e in
                         let () = unify ~handle:Gripers.iteration_list_body (pos_and_typ e, no_pos lt) in
                         let () = unify ~handle:Gripers.iteration_list_pattern (ppos_and_typ pattern, (exp_pos e, a))
                         in
                           (List (erase_pat pattern, erase e) :: generators,
                            usages e :: generator_usages,
                            pattern_env pattern :: environments)
                     | Table (tmp, pattern, e) ->
                         unify ~handle:Gripers.iteration_ambient_effect
                           (no_pos (T.Effect context.effect_row),
                            no_pos (T.Effect (Types.make_empty_closed_row ())));
                         let a = T.Record (Types.make_empty_open_row (lin_unl, res_base)) in
                         let b = T.Record (Types.make_empty_open_row (lin_unl, res_base)) in
                         let c = T.Record (Types.make_empty_open_row (lin_unl, res_base)) in
                         let pattern_type =
                             let open Temporality in
                             match tmp with
                                | Current     -> a
                                | Transaction ->
                                    Types.make_transaction_time_data_type a
                                | Valid       ->
                                    Types.make_valid_time_data_type a
                         in
                         let tt = Types.make_table_type (tmp, a, b, c) in
                         let pattern = tpc pattern in
                         let e = tc e in
                         let () = unify ~handle:Gripers.iteration_table_body (pos_and_typ e, no_pos tt) in
                         let () = unify ~handle:(Gripers.iteration_table_pattern tmp)
                            (ppos_and_typ pattern, (exp_pos e, pattern_type))
                         in
                           (Table (tmp, erase_pat pattern, erase e) :: generators,
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
                (pos_and_typ body, no_pos (Types.make_list_type (Types.fresh_type_variable (lin_unl, res_any)))) in
            let () =
              opt_iter (fun where -> unify ~handle:Gripers.iteration_where
                          (pos_and_typ where, no_pos Types.bool_type)) where in

            let () =
              opt_iter
                (fun order ->
                   unify ~handle:Gripers.iteration_base_order
                     (pos_and_typ order, no_pos (T.Record (Types.make_empty_open_row (lin_unl, res_base))))) orderby in
            let e = Iteration (generators, erase body, opt_map erase where, opt_map erase orderby) in
            let vs = List.fold_left StringSet.union StringSet.empty (List.map Env.domain environments) in
            let us = Usage.combine_many
                       (List.append generator_usages
                          (List.map (fun usages -> Usage.restrict usages vs)
                             [ usages body
                             ; from_option Usage.empty (opt_map usages where)
                             ; from_option Usage.empty (opt_map usages orderby) ]))
            in
            e, typ body, us
        | Escape (bndr, e) ->
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
            let name = Binder.to_name bndr in
            let f = Types.fresh_type_variable (lin_any, res_any) in
            let t = Types.fresh_type_variable (lin_any, res_any) in

            let eff = Types.(open_row default_effect_subkind closed_wild_row) in

            let cont_type = T.Function (Types.make_tuple_type [f], eff, t) in
            let context' = {context
                            with var_env = Env.bind name cont_type context.var_env } in
            let e = type_check context' e in

            let () =
              let outer_effects =
                Types.(open_row default_effect_subkind closed_wild_row)
              in
                unify ~handle:Gripers.escape_outer
                  (no_pos (T.Record context.effect_row), no_pos (T.Record outer_effects)) in

            let () = unify ~handle:Gripers.escape (pos_and_typ e, no_pos f) in
              Escape (Binder.set_type bndr cont_type, erase e), typ e, Usage.restrict (usages e) (StringSet.singleton name)
        | Conditional (i,t,e) ->
            let i = tc i
            and t = tc t
            and e = tc e in
              unify ~handle:Gripers.if_condition
                (pos_and_typ i, no_pos (T.Primitive Primitive.Bool));
              unify ~handle:Gripers.if_branches
                (pos_and_typ t, pos_and_typ e);
              Conditional (erase i, erase t, erase e), (typ t), Usage.combine (usages i) (Usage.align [usages t; usages e])
        | Block (bindings, e) ->
            let context', bindings, usage_builder = type_bindings context bindings in
            let cur_context = (Types.extend_typing_environment context context') in
            let e = type_check cur_context e in
            Block (bindings, erase e), typ e, usage_builder (usages e)
        | Regex r ->
            Regex (type_regex context r),
            Instantiate.alias "Regex" [] context.tycon_env,
            Usage.empty
        | Projection (r,l) ->
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
                  | T.ForAll (_, T.Record _) as t ->
                      let tyargs, rt = Instantiate.typ t in

                      let field_env, _row_var, lr =
                        match rt with
                        | T.Record row -> Types.unwrap_row row |> fst |> TypeUtils.extract_row_parts
                        | _ -> assert false
                      in
                      assert (not lr);
                      begin
                        match StringMap.lookup l field_env with
                        | Some (T.Present t) ->
                          (* the free type variables in the projected type *)
                          let vars = Types.free_type_vars t in

                          (* return true if this quantifier appears
                             free in the projected type *)
                          let free_in_body q = Types.TypeVarSet.mem (Quantifier.to_var q) vars in

                          (* quantifiers for the projected type *)
                          let pta, pqs =
                            List.map (fun q -> (q, Types.quantifier_of_type_arg q)) tyargs
                            |> List.filter (fun (_, q) -> free_in_body q)
                            |> List.split
                          in
                          List.iter Generalise.rigidify_type_arg pta;

                          let fieldtype = Types.for_all (pqs, t) in

                          let r' = erase r in
                          let sugar_pqs = List.map SugarQuantifier.mk_resolved pqs in
                          let e = tabstr (sugar_pqs, Projection (with_dummy_pos (tappl (r'.node, tyargs)), l)) in
                          e, fieldtype, usages r
                        | Some (T.Absent | T.Meta _)
                        | None ->
                          let fieldtype = Types.fresh_type_variable (lin_any, res_any) in
                          unify ~handle:Gripers.projection
                            ((exp_pos r, rt),
                             no_pos (T.Record (Types.make_singleton_open_row
                                                (l, T.Present fieldtype)
                                                (lin_unl, res_any))));
                          let r' = erase r in
                          let e = Projection (with_dummy_pos (tappl (r'.node, tyargs)), l) in
                          e, fieldtype, usages r
                        | Some _ -> raise Types.tag_expectation_mismatch
                      end
                  | _ ->
                      let fieldtype = Types.fresh_type_variable (lin_any, res_any) in
                        unify ~handle:Gripers.projection
                          (pos_and_typ r, no_pos (T.Record (Types.make_singleton_open_row
                                                             (l, T.Present fieldtype)
                                                             (lin_unl, res_any))));
                        Projection (erase r, l), fieldtype, usages r
              end
        | With (r, fields) ->
            let r = tc r in
            let fields = alistmap tc fields in

            let () =
              let fields_type =
                T.Record (List.fold_right
                           (fun (lab, _) row ->
                              Types.row_with (lab, T.Present (Types.fresh_type_variable (lin_unl, res_any))) row)
                           fields (Types.make_empty_open_row (lin_any, res_any))) in
                unify ~handle:Gripers.record_with (pos_and_typ r, no_pos fields_type) in
            let rfields, row_var, lr = (TypeUtils.extract_row (typ r)) |> Types.unwrap_row |> fst |> TypeUtils.extract_row_parts in
            assert (not lr);
            let rfields =
              StringMap.mapi
                (fun name t ->
                   if List.mem_assoc name fields then
                     T.Present (snd3 (List.assoc name fields))
                   else t)
                rfields
            in
              With (erase r, alistmap erase fields), T.Record (T.Row (rfields, row_var, false)), Usage.combine_many (usages r :: List.map usages (range fields))
        | TypeAnnotation (e, (_, Some t as dt)) ->
            let e = tc e in
              unify ~handle:Gripers.type_annotation (pos_and_typ e, no_pos t);
              TypeAnnotation (erase e, dt), t, usages e
        | TypeAnnotation _ -> assert false
        | Upcast (e, (_, Some t1 as t1'), (_, Some t2 as t2')) ->
            let e = tc e in
              if Types.is_sub_type (t2, t1) then
                begin
                  unify ~handle:Gripers.upcast_source (pos_and_typ e, no_pos t2);
                  Upcast (erase e, t1', t2'), t1, usages e
                end
              else
                Gripers.upcast_subtype pos t2 t1
        | Upcast _ -> assert false

        (* effect handlers *)
        | Handle { sh_expr = m; sh_value_cases = val_cases; sh_effect_cases = eff_cases; sh_descr = descr; } ->
           ignore
             (if not (Settings.get Basicsettings.Handlers.enabled)
              then raise (Errors.disabled_extension
                            ~pos ~setting:("enable_handlers", true)
                            ~flag:"--enable-handlers" "Handlers"));
           let allow_wild : Types.row -> Types.row
             = fun row ->
             Types.(row_with wild_present row)
           in
           (* type parameters *)
           let henv = context in
           (* deal with parameterised handlers *)
           let (henv, params, descr) =
             match descr.shd_params with
             | Some { shp_bindings; _ } ->
                let _ =
                  check_for_duplicate_names pos (List.map fst shp_bindings)
                in
                let type_binding (pat, body) =
                  let body = tc body in
                  let pat = tpc pat in
                  unify ~handle:Gripers.handle_parameter_pattern (ppos_and_typ pat, (pos_and_typ body));
                  (pat, body)
                in
                let typed_bindings = List.map type_binding shp_bindings in
                let pat_types =
                  List.map (fst ->- pattern_typ) typed_bindings
                in
                let param_env =
                  List.fold_left
                    (fun env p ->
                      env ++ pattern_env p)
                    henv (List.map fst typed_bindings)
                in
                (param_env, typed_bindings, { descr with shd_params =
                  Some { shp_bindings = List.map (fun (pat, body) -> erase_pat pat, erase body) typed_bindings;
                         shp_types = pat_types } })
             | None -> (henv, [], descr)
           in
           (* Tag: Handle type_cases begin *)
           let type_cases val_cases eff_cases =
             let wild_row () =
               let fresh_row = Types.make_empty_open_row default_effect_subkind in
               allow_wild fresh_row
             in
             let rt = Types.fresh_type_variable (lin_any, res_any) in
             let bt = Types.fresh_type_variable (lin_any, res_any) in
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
                     | { node = Pattern.Operation _; pos = _ }
                     | { node = Pattern.HasType _; pos = _ } -> pat
                     | { pos; _ } -> Gripers.die pos "Improper pattern matching" in
                   let pat = tpo pat in
                   (* We may have to patch up the inferred resumption
                      type as `type_pattern' cannot infer the
                      principal type for a resumption in a
                      parameterised handler since it requires access
                      to information which is not conveyed by
                      pattern. TODO: perhaps augment the pattern with
                      arity information. *)
                   let (pat, env, efftyp) = pat in
                   let pat =
                     let rec erase_ann pat = match pat.node with
                     | Pattern.Operation _ -> pat
                     | Pattern.HasType (p, _) -> erase_ann p
                     | _ -> assert false
                     in
                     erase_ann pat
                   in
                   let effname, kpat = match pat.node with
                     | Pattern.Operation (name, _, k, _) -> name, k
                     | _ -> assert false
                   in
                   let effrow =
                     if Settings.get Basicsettings.Sessions.exceptions_enabled &&
                        not (Settings.get Basicsettings.Sessions.expose_session_fail) &&
                        String.equal effname Value.session_exception_operation
                     then
                       Types.Effect (Types.make_empty_open_row default_effect_subkind)
                     else
                       Types.Effect (Types.make_singleton_open_row (effname, Types.Present efftyp) default_effect_subkind) in
                   unify ~handle:Gripers.handle_effect_patterns
                         ((uexp_pos pat, effrow),  no_pos (T.Effect inner_eff));
                   let pat, kpat =
                     let rec find_effect_type eff = function
                       | (eff', t) :: _ when eff = eff' ->
                          begin match t with
                          | T.Present t -> t
                          | _ -> assert false
                          end
                       | _ :: xs -> find_effect_type eff xs
                       | [] -> raise Not_found
                     in
                     match descr.shd_params with
                     | Some params when descr.shd_depth = Deep ->
                        let handler_params = params.shp_types in
                        let open Pattern in
                        begin match kpat.node with
                        | Any ->
                           let kt =
                             let domain =
                               (Types.fresh_type_variable (lin_unl, res_any)) :: handler_params
                             in
                             let effects = Types.make_empty_open_row default_effect_subkind in
                             let codomain = Types.fresh_type_variable (lin_unl, res_any) in
                             Types.make_function_type domain effects codomain
                           in
                           (pat, env, effrow), (kpat, Env.empty, kt)
                        | As (bndr,_)
                        | Variable bndr ->
                           let kname = Binder.to_name bndr in
                           let kt =
                             let (fields,_,_) = TypeUtils.extract_row_parts (TypeUtils.extract_row effrow) in
                             let kt = find_effect_type effname (StringMap.to_alist fields) in
                             let op_param = TypeUtils.return_type kt in
                             let typ = Env.find kname env in
                             let domain =
                               op_param :: handler_params
                             in
                             let effs, codomain =
                               TypeUtils.(effect_row typ, return_type typ)
                             in
                             Types.make_function_type domain effs codomain
                           in
                           let env = Env.bind kname kt env in
                           let env' = Env.singleton kname kt in
                           (pat, env, effrow), (kpat, env', kt)
                        | _ -> assert false
                        end
                     | _ ->
                        let open Pattern in
                        match kpat.node with
                        | As (bndr,_)
                        | Variable bndr ->
                           let kname = Binder.to_name bndr in
                           let kt =
                             match Env.find_opt kname env with
                             | Some t -> t
                             | None -> assert false
                           in
                           let env' = Env.singleton kname kt in
                           (pat, env, effrow), (kpat, env', kt)
                        | Any ->
                           let kt =
                             Types.make_function_type
                               [Types.fresh_type_variable (lin_unl, res_any)]
                               (Types.make_empty_open_row default_effect_subkind)
                               (Types.fresh_type_variable (lin_unl, res_any))
                           in
                           (pat, env, effrow), (kpat, Env.empty, kt)
                        | _ -> assert false
                   in
                   (pat, kpat, body) :: cases)
                 eff_cases []
             in
             (* Closing of subpatterns in effect patterns *)
             let inner_eff = TypeUtils.extract_row (close_pattern_type (List.map (fst3 ->- fst3) eff_cases) (T.Effect inner_eff)) in
             let check_linear_paras_in_clauses pat body =
              Env.iter (fun v t ->
                let uses = Usage.uses_of v (usages body) in
                  if uses <> 1 then
                    if Types.Unl.can_type_be t then
                      Types.Unl.make_type t
                    else
                      Gripers.non_linearity pos uses v t)
                (pattern_env pat) in
             let check_linear_vars_in_deep_handlers henv vs body =
              if descr.shd_depth = Deep then
                Usage.iter
                  (fun v _ ->
                    if not (StringSet.mem v vs) then
                      let t = Env.find v henv.var_env in
                      if Types.Unl.can_type_be t then
                        Types.Unl.make_type t
                      else
                        Gripers.linear_vars_in_deep_handler pos v t)
                  (usages body)
               else () in
             (* Type value clause bodies *)
             let val_cases =
               List.fold_right
                 (fun (pat, body) cases ->
                   let body = type_check (henv ++ pattern_env pat) body in
                   let () = unify ~handle:Gripers.handle_branches
                          (pos_and_typ body, no_pos bt) in
                   (* see the comments in eff_cases for the meaning of vs and vs' *)
                   let vs = Env.domain (pattern_env pat) in
                   let vs' = Env.domain <| List.fold_left (fun env p -> Env.extend env (pattern_env p))
                                           Env.empty (List.map fst params)
                   in
                   let us =
                     let vs'' = Ident.Set.union vs vs' in
                     Usage.restrict (usages body) vs''
                   in
                   (* check the usages of linear parameters in handler clauses *)
                   let () = check_linear_paras_in_clauses pat body in
                   (* check the usages of environment linear variables in deep handlers *)
                   let () = check_linear_vars_in_deep_handlers henv vs body in
                   (pat, update_usages body us) :: cases)
                 val_cases []
             in
             (* Type operation clause bodies and resumptions *)
             let eff_cases =
               List.fold_right
                 (fun (pat, (kpat : Pattern.with_pos * Types.datatype Env.t * Types.datatype), body) cases ->
                   let body = type_check (henv ++ pattern_env pat) body in
                   let () = unify ~handle:Gripers.handle_branches
                              (pos_and_typ body, no_pos bt)
                   in
                   (* vs is the variables in the pattern of eff-clauses *)
                   let vs = Env.domain (pattern_env pat) in
                   (* vs' is the variables in the params of parameterised handlers *)
                   let vs' = Env.domain <| List.fold_left (fun env p -> Env.extend env (pattern_env p))
                                           Env.empty (List.map fst params)
                   in
                   (* we need to remove vs ∪ vs' from the usages counting
                      because they are only bound in the handler *)
                   let us =
                     let vs'' = Ident.Set.union vs vs' in
                     Usage.restrict (usages body) vs''
                   in
                   (* check the usages of linear parameters in handler clauses *)
                   let () = check_linear_paras_in_clauses pat body in
                   (* check the usages of environment linear variables in deep handlers *)
                   let () = check_linear_vars_in_deep_handlers henv vs body in

                   let () =
                     let pos' = (fst3 kpat) |> WithPos.pos |> Position.resolve_expression in
                     let kt = TypeUtils.return_type (pattern_typ kpat) in
                     match descr.shd_depth with
                     | Deep ->
                        let eff = context.effect_row in
                        unify ~handle:Gripers.deep_resumption
                          ((pos', kt), no_pos bt);
                        unify ~handle:Gripers.deep_resumption_effects
                          ((pos', T.Effect eff), no_pos (T.Effect outer_eff))
                     | Shallow ->
                        let eff = TypeUtils.effect_row (pattern_typ kpat) in
                        unify ~handle:Gripers.shallow_resumption
                          ((pos', kt), no_pos rt);
                        unify ~handle:Gripers.shallow_resumption_effects
                          ((pos', T.Effect eff), no_pos (T.Effect inner_eff))
                   in
                   (pat, kpat, update_usages body us) :: cases)
                 eff_cases []
             in
             (val_cases, rt), eff_cases, bt, inner_eff, outer_eff
           in
           (* make_operations_presence_polymorphic makes the operations in the given row polymorphic in their presence *)
           let make_operations_presence_polymorphic : Types.row -> Types.row
            = fun row ->
             let (operations, rho, dual) = TypeUtils.extract_row_parts row in
             let operations' =
               StringMap.mapi
                 (fun name p ->
                   if TypeUtils.is_builtin_effect name
                   then p
                   else Types.fresh_presence_variable default_effect_subkind) (* It is questionable whether it is ever correct to
                                                                       make absent operations polymorphic in their presence. *)
                 operations
             in
             T.Row (operations', rho, dual)
           in
           let m_context = { context with
              effect_row = Types.make_empty_open_row default_effect_subkind;
              cont_lin   = LinCont.getnew () } in
           let m = type_check m_context m in (* Type-check the input computation m under current context *)
           let m_effects = T.Effect m_context.effect_row in
           (* Most of the work is done by `type_cases'. *)
           let val_cases = match val_cases with
             | [] -> (* insert a synthetic value case: x -> x. *)
                  let x = "x" in
                  let id = (variable_pat x, var x) in
                  [id]
             | _  -> val_cases
           in
           let (val_cases, rt), eff_cases, body_type, inner_eff, outer_eff = type_cases val_cases eff_cases in
           (* Printf.printf "result: %s\ninner_eff: %s\nouter_eff: %s\n%!" (Types.string_of_datatype rt) (Types.string_of_row inner_eff) (Types.string_of_row outer_eff); *)
           (* Patch the result type of `m' *)
           let () =
              unify ~handle:Gripers.handle_return (pos_and_typ m, no_pos rt)
           in
           (* Finalise construction of the effect row of the input computation *)
           let inner_eff, outer_eff =
             let m_pos = exp_pos m in
             let () = unify ~handle:Gripers.handle_comp_effects ((m_pos, m_effects), no_pos (T.Effect inner_eff)) in
             let inner_eff' = make_operations_presence_polymorphic inner_eff in
             (* Printf.printf "inner_eff': %s\n%!" (Types.string_of_row inner_eff'); *)
             let () = unify ~handle:Gripers.handle_unify_with_context (no_pos (T.Effect inner_eff'), no_pos (T.Effect outer_eff)) in
             let () = unify ~handle:Gripers.handle_unify_with_context (no_pos (T.Effect outer_eff), no_pos (T.Effect context.effect_row)) in
             inner_eff, outer_eff
           in
           let eff_cases =
             List.map (fun (p, _, body) -> (p, body)) eff_cases
           in
           (* Printf.printf "result: %s\ninner_eff: %s\nouter_eff: %s\n%!" (Types.string_of_datatype rt) (Types.string_of_row inner_eff) (Types.string_of_row outer_eff); *)
           let descr = { descr with
                         shd_types = (Types.flatten_row inner_eff, typ m, Types.flatten_row outer_eff, body_type);
                         shd_raw_row = Types.make_empty_closed_row (); }
           in
           (* Tag: combine all usages counting in handlers *)
           let usages =
             Usage.combine_many [ Usage.align (List.map (fun (_,(_, _, m)) -> m) params)
                                ; usages m
                                ; usages_cases eff_cases
                                ; usages_cases val_cases ]
           in
           Handle { sh_expr = erase m;
                    sh_effect_cases = erase_cases eff_cases;
                    sh_value_cases = erase_cases val_cases;
                    sh_descr = descr }, body_type, usages
        | DoOperation (op, ps, _, linearity) ->
          let is_lindo = if LinCont.is_enabled then linearity = DeclaredLinearity.Lin else false in
          let () = if is_lindo then ()
                               else LinCont.update_bound_by_linlet context false
                               (* do is implicitly bound by an unlet *)
          in
          let op_linearity = if is_lindo then lin_unl else lin_any
          (* lin_unl: linear operation; lin_any: unlimited operation *)
          in
          (* inline the type checking of Operation here to be able to
             use is_lindo *)
          let op = (match op.node with
            | Operation name ->
                if String.equal name Value.session_exception_operation then
                  if not context.desugared then
                    Gripers.die pos "The session failure effect SessionFail is not directly invocable (use `raise` instead)"
                  else
                    (Operation name, Types.empty_type, Usage.empty)
                else
                  let t =
                    match lookup_effect context name with
                    | Some t -> t
                    | None   -> Types.fresh_type_variable (op_linearity, res_any)
                  in
                  (Operation name, t, Usage.empty)
            | _ -> Gripers.die pos "Do should take an operation label.")
          in
          let op = (let a,b,c = op in with_pos pos a,b,c) in
          let ps = List.map (tc) ps in
          let doop, rettyp, usage, opt  =
            match Types.concrete_type (typ op) with
            | T.ForAll (_, (T.Operation _)) as t ->
              begin
                match Instantiate.typ t with
                | tyargs, T.Operation (pts, rettyp, _) ->
                  (* quantifiers for the return type *)
                  let rqs =
                    (* the free type variables in the arguments (and effects) *)
                    let arg_vars = Types.free_type_vars pts in
                    (* return true if this quantifier appears free in the arguments (or effects) *)
                    let free_in_arg q = Types.TypeVarSet.mem (Quantifier.to_var q) arg_vars in
                    if Settings.get  dodgey_type_isomorphism then
                      let rta, rqs =
                        List.map (fun q -> (q, Types.quantifier_of_type_arg q)) tyargs
                        |> List.filter (fun (_, q) -> free_in_arg q)
                        |> List.split
                      in
                      List.iter Generalise.rigidify_type_arg rta;
                      rqs
                    else
                      []
                  in

                  let rettyp = Types.for_all (rqs, rettyp) in
                  let opt = T.Operation (pts, rettyp, linearity) in
                  let op' = erase op in
                  let sugar_rqs = List.map SugarQuantifier.mk_resolved rqs in
                  let e = tabstr (sugar_rqs, DoOperation (with_dummy_pos (tappl (op'.node, tyargs)), List.map erase ps, Some rettyp, linearity)) in
                    e, rettyp, Usage.combine_many (usages op :: List.map usages ps), opt
                | _ -> assert false
              end
            | T.Operation (pts, rettyp, _) ->
                let opt = T.Operation (pts, rettyp, linearity) in
                DoOperation (erase op, List.map erase ps, Some rettyp, linearity), rettyp, Usage.combine_many (usages op :: List.map usages ps), opt
            | opt ->
              (* fresh variable case *)
              let rettyp = Types.fresh_type_variable (lin_unl, res_any) in
                DoOperation (erase op, List.map erase ps, Some rettyp, linearity), rettyp, Usage.combine_many (usages op :: List.map usages ps), opt
          in

          let opname = find_opname (erase op) in
          let infer_opt = no_pos (LinCont.make_operation_type ~linear:is_lindo (List.map typ ps) rettyp) in
          let term = (exp_pos op, opt) in
          let row =
            if Settings.get Basicsettings.Sessions.exceptions_enabled &&
               not (Settings.get Basicsettings.Sessions.expose_session_fail) &&
               String.equal opname Value.session_exception_operation
            then
               Types.make_empty_open_row default_effect_subkind
            else
              Types.make_singleton_open_row (opname, T.Present (typ op)) default_effect_subkind in
          let p = Position.resolve_expression pos in
          let () =
            unify ~handle:Gripers.do_operation
              (term, infer_opt) ;
            unify ~handle:Gripers.do_operation
              (no_pos (T.Effect context.effect_row), (p, T.Effect row))
          in
          (* postponed *)
          (* let () = if is_lindo then () *)
                   (* else LinCont.update_in_linlet context false *)
          (* in *)
            doop, rettyp, usage
        | Operation _ ->
          Gripers.die pos "The operation label is used in invalid positions."
        | Linlet p ->
          let () = LinCont.update_bound_by_linlet context true in
          let (p, t, usages) = type_check context p in
          (* let () = LinCont.update_in_linlet context true in *) (* postponed *)
          (WithPos.node p, t, usages)
        | Unlet p ->
          let () = LinCont.update_bound_by_linlet context false in
          let (p, t, usages) = type_check context p in
          (* let () = LinCont.update_in_linlet context false in *) (* postponed *)
          (WithPos.node p, t, usages)
        | Switch (e, binders, _) ->
            let e = tc e in
            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Gripers.switch_pattern (pos_and_typ e, no_pos pattern_type) in
              Switch (erase e, erase_cases binders, Some body_type), body_type, Usage.combine (usages e) (usages_cases binders)
        | TryInOtherwise (try_phrase, pat, in_phrase, unless_phrase, _) ->
           (* Ensure that the body of the try has the SessionFail
              effect and that the remaining effects agree with the
              outer effects *)
            let rho = Types.fresh_row_variable default_effect_subkind in
            let outer_effects =
              if Settings.get Basicsettings.Sessions.expose_session_fail then
                Types.row_with
                  (Value.session_exception_operation, Types.fresh_presence_variable default_subkind)
                  (T.Row (StringMap.empty, rho, false))
              else
                T.Row (StringMap.empty, rho, false)
            in
            let try_effects =
              if Settings.get Basicsettings.Sessions.expose_session_fail then
                Types.row_with
                  (Value.session_exception_operation, T.Present (LinCont.make_operation_type [] Types.empty_type))
                  (T.Row (StringMap.empty, rho, false))
              else
                T.Row (StringMap.empty, rho, false)
            in

            unify ~handle:Gripers.try_effect
              (no_pos (T.Effect context.effect_row), no_pos (T.Effect outer_effects));

            let try_phrase = type_check {context with effect_row = try_effects} try_phrase in

            (* Pattern.with_posype variable *)
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
                let uses = Usage.uses_of v (usages in_phrase) in
                if uses <> 1 then
                  if Types.Unl.can_type_be t then
                    Types.Unl.make_type t
                  else
                    Gripers.non_linearity pos uses v t) (pattern_env pat) in

            (* vs: variables bound in the pattern. *)
            let vs = Env.domain (pattern_env pat) in

            let unless_phrase = tc unless_phrase in
            unify ~handle:Gripers.try_in_unless_branches
                (pos_and_typ in_phrase, pos_and_typ unless_phrase);

            (* in_usages: usages in the in_phrase *not* bound in the pattern *)
            let in_usages = Usage.restrict (usages in_phrase) vs in

            (* Now, we need to ensure that all variables used in the in- and unless-
             * phrases are unrestricted (apart from the pattern variables!) *)
            let () =
              Usage.iter
                (fun v n ->
                  if n = 0 then () else
                    if Env.has v (pattern_env pat) then () else
                      let ty = Env.find v context.var_env in
                      if Types.Unl.can_type_be ty then
                        Types.Unl.make_type ty
                      else
                        Gripers.try_in_unless_linearity pos v
                ) (usages in_phrase)
            in
            let () =
              Usage.iter
                (fun v n ->
                  if n = 0 then () else
                    let ty = Env.find v context.var_env in
                    if Types.Unl.can_type_be ty then
                      Types.Unl.make_type ty
                    else
                      Gripers.try_in_unless_linearity pos v)
                (usages unless_phrase)
            in


            (* Calculate resulting usages *)
            let usages_res =
              Usage.combine (usages try_phrase) (Usage.align [in_usages; (usages unless_phrase)])
            in

            let return_type = typ in_phrase in

            TryInOtherwise
              (erase try_phrase, erase_pat pat, erase in_phrase,
                erase unless_phrase, Some return_type), return_type, usages_res
        | QualifiedVar _ -> assert false
        | Raise ->
            let effects =
              if Settings.get Basicsettings.Sessions.expose_session_fail then
                Types.make_singleton_open_row
                  (Value.session_exception_operation, T.Present (LinCont.make_operation_type [] Types.empty_type))
                  default_effect_subkind
              else
                Types.make_empty_open_row default_effect_subkind
            in
            unify ~handle:Gripers.raise_effect
              (no_pos (T.Effect context.effect_row), (Position.resolve_expression pos, T.Effect effects));
            (Raise, Types.fresh_type_variable (lin_any, res_any), Usage.empty)
    in
    let p = with_pos pos e in
    let () = update_cflinearity expr usages in
    p, t, usages

(* [type_binding] takes XXX YYY (FIXME)
    The input context is the environment in which to type the bindings.

    The output context is the environment resulting from typing the
    bindings.  It does not include the input context.

    The result includes the introduced bindings, along with the variable
    usage map from the binder's body.
 *)
and type_binding : context -> binding -> binding * context * Usage.t =
  fun context {node = def; pos} ->
    let _UNKNOWN_POS_ = "<unknown>" in
    let no_pos t = (_UNKNOWN_POS_, t) in
    let type_check = type_check in
    let unify pos (l, r) = unify_or_raise ~pos (l, r) in
    let typ (_,t,_) = t
    and erase (e, _, _) = e
    and usages (_,_,u) = u
    and erase_pat (e, _, _) = e
    and pattern_typ (_, _, t) = t
    and tc = type_check context
    and tpc = type_pattern `Closed
    and tpcu p = type_pattern ~linear_vars:false `Closed p
    and pattern_env (_, e, _) = e
    and (++) ctxt env' = {ctxt with var_env = Env.extend ctxt.var_env env'} in
    let pattern_pos ({pos=p;_},_,_) = Position.resolve_expression p in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos p = WithPos.pos p |> Position.resolve_expression in
    let exp_pos (p,_,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in

    let empty_context = empty_context context.cont_lin context.effect_row context.desugared in

    let module T = Types in
    let typed, ctxt, usage = match def with
      | Val (pat, (_, body), location, datatype) ->
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
          Types.Mono.make_type (pattern_typ pat);
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
              let tyvars = Generalise.get_quantifiers_rigid context.var_env bt in
              match tyvars with
              | [] -> [], erase_pat pat, penv
              | _ -> Gripers.value_restriction pos bt
          in
          let sugar_tyvars = List.map SugarQuantifier.mk_resolved tyvars in
            Val (pat, (sugar_tyvars, body), location, datatype),
            {empty_context with
              var_env = penv},
            usage
      | Fun def ->
         let { fun_binder = bndr;
               fun_linearity = lin;
               fun_definition = (_, fnlit);
               fun_location;
               fun_signature = t_ann';
               fun_frozen;
               fun_unsafe_signature = unsafe } =
           Renamer.rename_function_definition def in
          let (pats, body) = Sugartypes.get_normal_funlit fnlit in
          let name = Binder.to_name bndr in
          let vs = name :: check_for_duplicate_names pos (List.flatten pats) in
          let (pats_init, pats_tail) = from_option ([], []) (unsnoc_opt pats) in
          let tpc' = if DeclaredLinearity.is_linear lin then tpc else tpcu in
          let pats = List.append (List.map (List.map tpc') pats_init)
                       [List.map tpc pats_tail] in
          let effects = Types.make_empty_open_row default_effect_subkind in
          let return_type = Types.fresh_type_variable (lin_any, res_any) in

          let t_ann = resolve_type_annotation bndr t_ann' in

          (* Check that any annotation matches the shape of the function *)
          let context_body, ft, quantifiers =
            match t_ann with
              | None ->
                  context, make_ft lin pats effects return_type, []
              | Some ft ->
                  (* make sure the annotation has the right shape *)
                  let shape = make_ft lin pats effects return_type in
                  let quantifiers, ft_mono = TypeUtils.split_quantified_type ft in

                  (* Debug.print ("ft_mono: " ^ Types.string_of_datatype ft_mono); *)
                  let () = unify pos ~handle:Gripers.bind_fun_annotation (no_pos shape, no_pos ft_mono) in
                    (* Debug.print ("return type: " ^Types.string_of_datatype (TypeUtils.concrete_type return_type)); *)
                  (* HACK: Place a dummy name in the environment in
                     order to ensure that the generalisation check
                     does the right thing (it would be unsound to use
                     the original name as the function is not
                     recursive) *)
                  let v = Utils.dummy_source_name () in
                  bind_var context (v, ft_mono), ft, quantifiers in

          (* We make the patterns monomorphic after unifying with the signature. *)
          make_mono pats;

          (* type check the body *)
          let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in
          let context_body = List.fold_left fold_in_envs context_body pats in

          (* the effects are flattened and a new cont_lin is generated before type checking the body *)
          let new_body_context = {context_body with effect_row = Types.flatten_row effects;
                                               cont_lin = LinCont.getnew () } in
          let body = type_check new_body_context body in

          (* check that the body type matches the return type of any annotation *)
          let () = unify pos ~handle:Gripers.bind_fun_return (no_pos (typ body), no_pos return_type) in

          (* check that the usage of bound variables agrees with the
             linearity of their types *)
          let () = List.iter
                     (fun pat ->
                      Env.iter (fun v t ->
                        let uses = Usage.uses_of v (usages body) in
                        if uses <> 1 then
                          if Types.Unl.can_type_be t then
                            Types.Unl.make_type t
                          else
                            Gripers.non_linearity pos uses v t)
                               (pattern_env pat))
                     (List.flatten pats)
          in
          let () =
            if DeclaredLinearity.is_nonlinear lin then
              Usage.iter
                (fun v _ ->
                  if not (List.mem v vs) then
                    let t = Env.find v context_body.var_env in
                    if Types.Unl.can_type_be t then
                      Types.Unl.make_type t
                    else
                      Gripers.die pos ("Variable " ^ v ^ " of linear type " ^ Types.string_of_datatype t ^
                                         " was used in a non-linear function definition"))
                (usages body)
            else ()
          in
          (* Check that quantifiers have not escaped into the typing context *)
          let check_escaped_quantifiers quantifiers env =
            let quantifier_set = IntSet.of_list (List.map fst quantifiers) in
            (* Note that `type_predicate` returns true iff *all* child nodes of
             * the type satisfy the predicate. Thus the checker returns true if
             * *all* type variables are *not* in quantifier_set *)
            let checker = object(_self)
              inherit Types.type_predicate
              method! var_satisfies (i, _, _) = not (IntSet.mem i quantifier_set)
            end in
            let (is_safe, _) = checker#predicates in
            let escapees =
              Env.filter (fun _ dt -> not (is_safe dt)) env
              |> Env.bindings in
            if not (ListUtils.empty escapees) then
              Gripers.escaped_quantifier ~pos ~var:name ~annotation:ft ~escapees in

          let () =
            if not (quantifiers = []) then
              check_escaped_quantifiers quantifiers context.var_env in

          let (tyvars, _), ft =
            if fun_frozen then (TypeUtils.quantifiers ft, []), ft
            else Utils.generalise context.var_env ft
          in

          (* It could be handy to support a (different) syntax for
             specifying quantifiers sloppily without fixing an order
             or promising that they're complete - in order to denote
             their kinds if nothing else *)

          let tyvars, ft =
            (* generalise *)
            (* check that tyvars matches up those from any type
               annotation *)
            match t_ann with
            | None -> tyvars, ft
            | Some t ->
               begin
                 match TypeUtils.quantifiers t with
                 | [] -> tyvars, ft
                 | t_tyvars ->
                   if not (List.for_all
                             (fun q ->
                               List.exists (Quantifier.eq q) t_tyvars) tyvars)
                   then
                     Gripers.inconsistent_quantifiers ~pos ~t1:t ~t2:ft;
                   t_tyvars, t
               end in

          (* let ft = Instantiate.freshen_quantifiers ft in *)
          let vs' = List.fold_right Ident.Set.add vs Ident.Set.empty in
          let sugar_tyvars = List.map SugarQuantifier.mk_resolved tyvars in
          (Fun { fun_binder = Binder.set_type bndr ft;
                 fun_linearity = lin;
                 fun_definition = (sugar_tyvars, NormalFunlit (List.map (List.map erase_pat) pats, erase body));
                 fun_frozen = true;
                 fun_location; fun_signature = t_ann'; fun_unsafe_signature = unsafe },
             {empty_context with
                var_env = Env.singleton name ft},
             Usage.restrict (usages body) vs')
      | Funs defs ->
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

         (* we've changed the semantics such that wildness only
            manifests on encountering a recursive reference to a
            recursive function (which we track using the rec_vars
            component of context) *)
          let defs =
            List.map (WithPos.map ~f:Renamer.rename_recursive_functionnode) defs in

          let fresh_tame () = Types.make_empty_open_row default_effect_subkind in

          let inner_rec_vars, inner_env, patss =
            List.fold_left
              (fun (inner_rec_vars, inner_env, patss)
                   {node= { rec_binder = bndr; rec_linearity = lin;
                            rec_definition = ((_, def), fnlit);
                            rec_signature = t_ann';
                            rec_unsafe_signature = unsafe;
                            rec_frozen = frozen;
                            _ }; _ } ->
                 let name = Binder.to_name bndr in
                 let (pats, _) = Sugartypes.get_normal_funlit fnlit in
                 (* recursive functions can't be linear! *)
                 if DeclaredLinearity.is_linear lin then
                   Gripers.linear_recursive_function pos name;
                 let _ = check_for_duplicate_names pos (List.flatten pats) in
                 let (pats_init, pats_tail) = from_option ([], []) (unsnoc_opt pats) in
                 let pats = List.append (List.map (List.map tpcu) pats_init)
                              [List.map tpc pats_tail] in
                 let t_ann = match def with
                   | Some (ty, _) -> Some ty
                   | None -> resolve_type_annotation bndr t_ann'
                 in
                 let inner =
                   match t_ann with
                     | None ->
                         (* make_ft lin pats (fresh_wild ()) (Types.fresh_type_variable (lin_any, res_any)) *)
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
                         make_ft_poly_curry lin pats (fresh_tame ()) (Types.fresh_type_variable (lin_any, res_any))
                     | Some t ->
                         (* Debug.print ("t: " ^ Types.string_of_datatype t); *)
                         let shape = make_ft lin pats (fresh_tame ()) (Types.fresh_type_variable (lin_any, res_any)) in
                         let ft = match t with
                           | T.ForAll _ -> t
                           | _ when frozen -> t
                           | _ -> Generalise.generalise_rigid context.var_env t |> snd
                         in
                         (* Debug.print ("ft: " ^ Types.string_of_datatype ft); *)
                         (* make sure the annotation has the right shape *)
                         let _, ft_mono = TypeUtils.split_quantified_type ft in
                         let () = unify pos ~handle:Gripers.bind_rec_annotation (no_pos shape, no_pos ft_mono) in
                           ft in

                 (* We make the patterns monomorphic after unifying with the signature. *)
                 make_mono pats;
                 let inner_rec_vars =
                   if unsafe
                   then inner_rec_vars
                   else StringSet.add name inner_rec_vars
                 in
                 inner_rec_vars, Env.bind name inner inner_env, pats::patss)
              (StringSet.empty, Env.empty, []) defs in
          let patss = List.rev patss in

          (*
             type check the function bodies using
             - monomorphic bindings for unannotated functions
             - potentially polymorphic bindings for annotated functions
          *)
          let (defs, used) =
            let body_env = Env.extend context.var_env inner_env in
            let context = {context with rec_vars = StringSet.union context.rec_vars inner_rec_vars} in
            (* let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in *)
            List.split
              (List.rev
                (List.fold_left2
                   (fun defs_and_uses
                        {node={ rec_binder = bndr; rec_linearity = lin;
                                rec_definition = (_, fnlit); _ } as fn; pos }
                        pats ->
                      let (_, body) = Sugartypes.get_normal_funlit fnlit in
                      let name = Binder.to_name bndr in
                      let pat_env = List.fold_left (fun env pat -> Env.extend env (pattern_env pat)) Env.empty (List.flatten pats) in
                      let self_env =
                        let datatype =
                          Env.find name body_env
                          |> TypeUtils.split_quantified_type
                          |> snd
                        in
                        let name' = Utils.dummy_source_name () in
                        Env.bind name' datatype pat_env
                      in
                      let body_context = {context with var_env = Env.extend body_env self_env} in
                      let effects = fresh_tame () in
                      let new_body_context = {body_context with effect_row = effects;
                                                                cont_lin = LinCont.getnew () } in
                      let body = type_check new_body_context body in
                      let () =
                        Env.iter
                          (fun v t ->
                            let uses = Usage.uses_of v (usages body) in
                              if uses <> 1 then
                                begin
                                  if Types.Unl.can_type_be t then
                                    Types.Unl.make_type t
                                  else
                                    Gripers.non_linearity pos uses v t
                                end)
                          pat_env in
                      let used =
                        let vs = StringSet.add name (Env.domain pat_env) in
                        if DeclaredLinearity.is_nonlinear lin then
                          Usage.iter
                            (fun v _ ->
                              if not (Ident.Set.mem v vs) then
                                let t = Env.find v body_context.var_env in
                                if Types.Unl.can_type_be t then
                                  Types.Unl.make_type t
                                else
                                  Gripers.die pos ("Use of variable " ^ v ^ " of linear type " ^
                                                     Types.string_of_datatype t ^ " in unlimited function binding.")
                              else ())
                            (usages body);
                        Usage.restrict (usages body) vs
                      in

                      (* check that the inferred type doesn't contradict any type annotation *)
                      let shape = make_ft lin pats effects (typ body) in
                      let ft = Env.find name body_context.var_env in
                      let _, ft_mono = TypeUtils.split_quantified_type ft in
                      let () = unify pos ~handle:Gripers.bind_rec_rec (no_pos shape, no_pos ft_mono) in

                      ((make ~pos fn, Binder.erase_type bndr, (([], None), (pats, body))), used) :: defs_and_uses) [] defs patss)) in

          (* Generalise to obtain the outer types *)
          let defs, outer_env =
            let defs, outer_env =
              List.fold_left2
                (fun (defs, outer_env) ({node=fn;pos}, bndr, (_, (_, body))) pats ->
                   let name = Binder.to_name bndr in
                   let inner = Env.find name inner_env in
                   let t_ann = resolve_type_annotation bndr fn.rec_signature in

                   let inner, outer, tyvars =
                     match inner with
                       | T.ForAll (inner_tyvars, inner_body) ->
                           let (body_tyvars, _), gen =
                             if fn.rec_frozen then (inner_tyvars, []), inner
                             else Utils.generalise context.var_env inner_body
                           in

                           let outer_tyvars, outer =
                             match t_ann with
                             | Some (T.ForAll (outer_tyvars, _) as outer) ->
                                outer_tyvars, outer
                             | None -> body_tyvars, gen
                             | Some t -> body_tyvars, Types.for_all (body_tyvars, t) in

                           (* check that every member of body_tyvars is also in outer_tyvars *)
                           if not
                                (List.for_all
                                   (fun q ->
                                     List.exists (Quantifier.eq q) outer_tyvars) body_tyvars) then
                             Gripers.inconsistent_quantifiers ~pos ~t1:outer ~t2:gen;

                           (* We could check that inner_tyvars is
                              consistent with body_tyvars, but that
                              should always be the case as either
                              body_tyvars is outer_tyvars (if there's
                              an explicit type annotation) or it
                              contains all of the type variables in
                              inner_typvars. *)

                           (* We may be able to avoid the above check
                              altogether by doing a global check on
                              the input program to ensure that no free
                              type variables occur in quantified type
                              annotations. *)

                           (* compute mapping from outer_tyvars to inner_tyvars

                              None:    use the outer quantifier
                              Some i:  use the i-th type argument
                            *)
                           let extras =
                             let rec find p i =
                               function
                               | [] -> None
                               | q :: _ when Quantifier.eq p q -> Some i
                               | _ :: qs -> find p (i+1) qs in
                             let find p = find p 0 inner_tyvars in
                             List.map find outer_tyvars
                           in
                           (inner, extras), outer, outer_tyvars
                       | _ ->
                           let (body_tyvars, _), gen =
                             if fn.rec_frozen then ([], []), inner
                             else Utils.generalise context.var_env inner
                           in

                           let outer_tyvars, outer =
                             match t_ann with
                             | None -> body_tyvars, gen
                             | Some t -> body_tyvars, Types.for_all (body_tyvars, t) in

                           let extras = List.map (fun _q -> None) outer_tyvars in
                           (inner, extras), outer, outer_tyvars in

                   let pats = List.map (List.map erase_pat) pats in
                   let body = erase body in
                   let sugar_tyvars = List.map SugarQuantifier.mk_resolved tyvars in
                   (make ~pos { fn with
                      rec_binder = Binder.set_type bndr outer;
                      rec_definition = ((sugar_tyvars, Some inner), NormalFunlit (pats, body)) }::defs,
                      Env.bind name outer outer_env))
                ([], Env.empty) defs patss
            in
              List.rev defs, outer_env in

          let defined =
            let vs = List.map (fun x -> Binder.to_name x.node.rec_binder) defs in
            List.fold_right Ident.Set.add vs Ident.Set.empty
          in
          Funs defs, {empty_context with var_env = outer_env}, Usage.restrict (Usage.combine_many used) defined

      | Foreign alien ->
         let binder, dt, datatype =
           match Alien.declaration alien with
           | (b, (dt, Some datatype)) -> (b, dt, datatype)
           | _ -> assert false
         in
         ignore (if String.contains (Binder.to_name binder) '\''
                 then raise (Errors.prime_alien pos));
         (* Ensure that we quantify FTVs *)
         let (_tyvars, _args), datatype = Utils.generalise context.var_env datatype in
         let datatype = Instantiate.freshen_quantifiers datatype in
         let binder = Binder.set_type binder datatype in
         ( Foreign (Alien.modify ~declarations:[(binder, (dt, Some datatype))] alien)
         , bind_var empty_context (Binder.to_name binder, datatype)
         , Usage.empty )
      | Aliases ts ->
          let env = List.fold_left (fun env {node=(name, vars, b); _} ->
              match b with
                | Typename     (_, Some dt) ->
                    bind_alias env (name, `Alias (pk_type, List.map (SugarQuantifier.get_resolved_exn) vars, dt))
                | Effectname   (_, Some dt) ->
                    bind_alias env (name, `Alias (pk_row , List.map (SugarQuantifier.get_resolved_exn) vars, dt))
                | _ -> raise (internal_error "typeSugar.ml: unannotated type")
          ) empty_context ts in
          (Aliases ts, env, Usage.empty)
      | Infix def -> Infix def, empty_context, Usage.empty
      | Exp e ->
          let e = tc e in
          let () = unify pos ~handle:Gripers.bind_exp
            (pos_and_typ e, no_pos Types.unit_type) in
          Exp (erase e), empty_context, usages e
      | Import _
      | Open _
      | AlienBlock _
      | Module _ -> assert false
    in
      WithPos.make ~pos typed, ctxt, usage
and type_regex typing_env : regex -> regex =
  fun m ->
    let erase (e, _, _) = e in
    let typ (_, t, _) = t in
    let no_pos t = ("<unknown>", t) in
    let tr = type_regex typing_env in
      match m with
        | (Range _ | Simply _ | Any  | StartAnchor | EndAnchor) as r -> r
        | Quote r -> Quote (tr r)
        | Seq rs -> Seq (List.map tr rs)
        | Alternate (r1, r2) -> Alternate (tr r1, tr r2)
        | Group r -> Group (tr r)
        | Repeat (repeat, r) -> Repeat (repeat, tr r)
        | Splice e ->
           let pos = e.pos in
           let e = type_check typing_env e in
           let () = unify_or_raise ~pos:pos ~handle:Gripers.splice_exp
                      (no_pos (typ e), no_pos Types.string_type)
           in Splice (erase e)
        | Replace (r, Literal s) -> Replace (tr r, Literal s)
        | Replace (r, SpliceExpr e)  -> Replace (tr r, SpliceExpr (erase (type_check typing_env e)))
and type_bindings (globals : context) bindings =
  let tyenv, (bindings, uinf) =
    List.fold_left
      (fun (ctxt, (bindings, uinf)) (binding : binding) ->
         let cur_ctxt = (Types.extend_typing_environment globals ctxt) in
         let binding, ctxt', usage = type_binding cur_ctxt binding in
         let result_ctxt = Types.extend_typing_environment ctxt ctxt' in
         result_ctxt, (binding::bindings, (binding.pos,ctxt'.var_env,usage)::uinf))
      (empty_context globals.cont_lin globals.effect_row globals.desugared, ([], [])) bindings in
  (* usage_builder checks the usage of variables from the bindings *)
  let usage_builder body_usage =
    List.fold_left
      (fun usages (pos,env,usage) ->
        let vs = Env.domain env in
        Env.iter
          (fun v t ->
            let uses = Usage.uses_of v usages in
            if uses <> 1 then
              if Types.Unl.can_type_be t then
                Types.Unl.make_type t
              else
                Gripers.non_linearity pos uses v t)
          env;
        Usage.combine usage (Usage.restrict usages vs))
      body_usage uinf
  in
  tyenv, List.rev bindings, usage_builder
and type_cp (context : context) = fun {node = p; pos} ->
  let with_channel = fun c s (p, t, u) ->
    if Usage.uses_of c u <> 1 then
      if Types.Unl.can_type_be s then
        Types.Unl.make_type s
      else
        Gripers.non_linearity pos (Usage.uses_of c u) c s;
    (p, t, Usage.remove c u)
  in

  let use s u = Usage.incr ~by:1 s u in

  let unify ~pos ~handle (t, u) = unify_or_raise ~pos:pos ~handle:handle (("<unknown>", t), ("<unknown>", u)) in

  let wild_open = Types.(open_row default_effect_subkind closed_wild_row) in
  unify ~pos ~handle:Gripers.cp_wild (Types.Effect wild_open, Types.Effect context.effect_row);

  let module T = Types in
  let (p, t, u) = match p with
    | CPUnquote (bindings, e) ->
       let context', bindings, usage_builder = type_bindings context bindings in
       let (e, t, u) = type_check (Types.extend_typing_environment context context') e in
         if Settings.get endbang_antiquotes then
           unify ~pos:pos ~handle:Gripers.cp_unquote (t, Types.make_endbang_type);
         CPUnquote (bindings, e), t, usage_builder u
    | CPGrab ((c, _), None, p) ->
       let (_, t, _) = type_check context (var c) in
       let ctype = T.Alias (pk_type, ("EndQuery", [], [], false), T.Input (Types.unit_type, T.End)) in
       unify ~pos:pos ~handle:(Gripers.cp_grab c) (t, ctype);
       let (p, pt, u) = type_cp (unbind_var context c) p in
       CPGrab ((c, Some (ctype, [])), None, p), pt, use c u
    | CPGrab ((c, _), Some bndr, p) ->
       let x = Binder.to_name bndr in
       let (_, t, _) = type_check context (with_pos pos (Var c)) in
       let a = Types.fresh_type_variable (lin_any, res_any) in
       let s = Types.fresh_session_variable lin_any in
       let ctype = T.Input (a, s) in
       unify ~pos:pos ~handle:(Gripers.cp_grab c)
             (t, ctype);
       let (p, pt, u) = with_channel c s (type_cp (bind_var (bind_var context (c, s)) (x, a)) p) in
       let uses = Usage.uses_of x u in
       if uses <> 1 then
         if Types.Unl.can_type_be a then
           Types.Unl.make_type a
         else
           Gripers.non_linearity pos uses x a;
       let grab_ty = (Env.find "receive" context.var_env) in
       let tyargs =
         match Types.concrete_type grab_ty with
         | T.ForAll _ ->
            begin
              match Instantiate.typ grab_ty with
              | tyargs, T.Function (fps, _fe, _rettype) ->
                 unify ~pos:pos ~handle:(Gripers.cp_grab "") (Types.make_tuple_type [ctype], fps);
                 tyargs
              | _ -> assert false
            end
         | _ -> assert false in
       CPGrab ((c, Some (ctype, tyargs)), Some (Binder.set_type bndr a), p), pt, use c (Usage.remove x u)
    | CPGive ((c, _), None, p) ->
       let (_, t, _) = type_check context (with_pos pos (Var c)) in
       let ctype = T.Output (Types.unit_type, T.End) in
       unify ~pos:pos ~handle:(Gripers.cp_give c) (t, ctype);
       let (p, t, u) = type_cp (unbind_var context c) p in
       CPGive ((c, Some (ctype, [])), None, p), t, use c u
    | CPGive ((c, _), Some e, p) ->
       let (_, t, _) = type_check context (var c) in
       let (e, t', u) = type_check context e in
       let s = Types.fresh_session_variable lin_any in
       let ctype = T.Output (t', s) in
       unify ~pos:pos ~handle:(Gripers.cp_give c)
             (t, ctype);
       let (p, t, u') = with_channel c s (type_cp (bind_var context (c, s)) p) in

       let give_ty = (Env.find "send" context.var_env) in
       let tyargs =
         match Types.concrete_type give_ty with
         | T.ForAll _ ->
            begin
              match Instantiate.typ give_ty with
              | tyargs, T.Function (fps, _fe, _rettpe) ->
                 unify ~pos:pos ~handle:(Gripers.cp_give "") (Types.make_tuple_type [t'; ctype], fps);
                 tyargs
              | _ -> assert false
            end
         | _ -> assert false in
       CPGive ((c, Some (ctype, tyargs)), Some e, p), t, use c (Usage.combine u u')
    | CPGiveNothing bndr ->
       let c = Binder.to_name bndr in
       let _, t, _ = type_check context (var c) in
       unify ~pos:pos ~handle:Gripers.(cp_give c) (t, Types.make_endbang_type);
       CPGiveNothing (Binder.set_type bndr t), t, Usage.singleton c
    | CPSelect (bndr, label, p) ->
       let c = Binder.to_name bndr in
       let (_, t, _) = type_check context (var c) in
       let s = Types.fresh_session_variable lin_any in
       let r = Types.make_singleton_open_row (label, T.Present s) (lin_any, res_session) in
       let ctype = T.Select r in
       unify ~pos:pos ~handle:(Gripers.cp_select c)
             (t, ctype);
       let (p, t, u) = with_channel c s (type_cp (bind_var context (c, s)) p) in
       CPSelect (Binder.set_type bndr ctype, label, p), t, use c u
    | CPOffer (bndr, branches) ->
       let c = Binder.to_name bndr in
       let (_, t, _) = type_check context (var c) in
       (*
       let crow = Types.make_empty_open_row (lin_any, res_session) in
       let ctype = Choice crow in
       unify ~pos:pos ~handle:(Gripers.cp_offer_choice c)
             (t, ctype);
        *)
       let check_branch (label, body) =
         let s = Types.fresh_type_variable (lin_any, res_session) in
         let r = Types.make_singleton_open_row (label, T.Present s) (lin_any, res_session) in
         unify ~pos:pos ~handle:(Gripers.cp_offer_choice c) (t, T.Choice r);
         let (p, t, u) = with_channel c s (type_cp (bind_var context (c, s)) body) in
         (label, p), t, u
       in
       let branches = List.map check_branch branches in
       let t' = Types.fresh_type_variable (lin_any, res_any) in
       List.iter (fun (_, t, _) -> unify ~pos:pos ~handle:Gripers.cp_offer_branches (t, t')) branches;
       let u = Usage.align (List.map (fun (_, _, u) -> u) branches) in
       CPOffer (Binder.set_type bndr t, List.map (fun (x, _, _) -> x) branches), t', use c u
    | CPLink (bndr1, bndr2) ->
      let c = Binder.to_name bndr1 in
      let d = Binder.to_name bndr2 in
      let (_, tc, uc) = type_check context (var c) in
      let (_, td, ud) = type_check context (var d) in
        unify ~pos:pos ~handle:Gripers.cp_link_session
          (tc, Types.fresh_type_variable (lin_any, res_session));
        unify ~pos:pos ~handle:Gripers.cp_link_session
          (td, Types.fresh_type_variable (lin_any, res_session));
        unify ~pos:pos ~handle:Gripers.cp_link_dual (Types.dual_type tc, td);
        CPLink (Binder.set_type bndr1 tc, Binder.set_type bndr1 td), Types.make_endbang_type, Usage.combine uc ud
    | CPComp (bndr, left, right) ->
       let c = Binder.to_name bndr in
       let s = Types.fresh_session_variable lin_any in
       let left, t, u = with_channel c s (type_cp (bind_var context (c, s)) left) in
       let right, t', u' = with_channel c (T.Dual s) (type_cp (bind_var context (c, T.Dual s)) right) in
       unify ~pos:pos ~handle:Gripers.cp_comp_left (Types.make_endbang_type, t);
       CPComp (Binder.set_type bndr s, left, right), t', Usage.combine u u'
  in
  WithPos.make ~pos p, t, u

let type_check_general context body =
  let body, typ, _ = type_check context body in
  if Utils.is_generalisable body
     && Settings.get generalise_toplevel then
    match Utils.generalise ~unwrap:false context.var_env typ with
    | ([], _), typ -> body, typ
    | (qs, _), qtyp ->
       let ppos = WithPos.pos body in
       let sugar_qs = List.map SugarQuantifier.mk_resolved qs in
       let open SugarConstructors.SugartypesPositions in
       block ~ppos
         ([with_pos ppos (Val (variable_pat ~ppos ~ty:qtyp "it", (sugar_qs, body), loc_unknown, None))],
          freeze_var ~ppos "it"),
       qtyp
  else
    body, typ

let binding_purity_check bindings =
  List.iter (fun ({pos;_} as b) ->
               if not (Utils.is_pure_binding b) then
                 Gripers.toplevel_purity_restriction pos b)
    bindings

module Check =
struct
  let program tyenv (bindings, body) =
    try
      Debug.if_set Basicsettings.show_stages (fun () -> "Type checking...");
      Debug.if_set show_pre_sugar_typing
        (fun () ->
           "before type checking: \n"^ show_program (bindings, body));
      let tyenv', bindings, _ = type_bindings tyenv bindings in
      let tyenv' = Types.normalise_typing_environment tyenv' in
      if Settings.get check_top_level_purity then
        binding_purity_check bindings; (* TBD: do this only in web mode? *)
      let program, typ, tyenv' =
        match body with
        | None -> (bindings, None), Types.unit_type, tyenv'
        | Some body ->
          let context = (Types.extend_typing_environment tyenv tyenv') in
          (* create a new cont_lin before typing the body *)
          let body, typ = type_check_general {context with cont_lin = LinCont.getnew ()} body in
          let typ = Types.normalise_datatype typ in
          (bindings, Some body), typ, tyenv' in
      Debug.if_set show_post_sugar_typing
        (fun () ->
           ("after type checking: \n"^ show_program program));
      program, typ, tyenv'
    with
        Unify.Failure (`Msg msg) -> failwith msg

  let sentence tyenv sentence =
    Debug.if_set Basicsettings.show_stages (fun () -> "Type checking...");
    Debug.if_set show_pre_sugar_typing
      (fun () ->
         "before type checking: \n"^ show_sentence sentence);
    let sentence, t, tyenv =
      match sentence with
      | Definitions bindings ->
        let tyenv', bindings, _ = type_bindings tyenv bindings in
        let tyenv' = Types.normalise_typing_environment tyenv' in
        Definitions bindings, Types.unit_type, tyenv'
      | Expression body ->
        let body, t = type_check_general tyenv body in
        let t = Types.normalise_datatype t in
        Expression body, t, tyenv
      | Directive d -> Directive d, Types.unit_type, tyenv in
    Debug.if_set show_post_sugar_typing
      (fun () ->
         "after type checking: \n" ^ show_sentence sentence);
    sentence, t, tyenv
end
