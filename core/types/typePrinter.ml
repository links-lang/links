(** Type printers *)
(* Do not use this module directly, but its export via Types.Print *)

open TypesBase

open Utility
open CommonTypes

module FieldEnv = Utility.StringMap
module Vars = FreeTypeVars



let show_raw_type_vars = Basicsettings.Types.show_raw_type_vars

(* See Note [Variable names in error messages] *)
  (* We don't really care much about size of the hash table.  20 should be a
     reasonable default since error message is unlikely to have more type
     variables.  And even if it does we don't care about performance penalty
     because we're printing the error message and thus stopping the compilation
     anyway. *)
let tyvar_name_map = Hashtbl.create 20
let tyvar_name_counter = ref 0


let init (flavour, kind, scope) name =
    match scope with
      | `Free  -> (name, (flavour, kind, 1))
      | `Bound -> (name, (flavour, kind, 0))

let combine (name, (flavour, kind, count)) (flavour', kind', scope) =
  assert (flavour = flavour');
  assert (kind    = kind'   );
  match scope with
    | `Free  -> (name, (flavour, kind, count+1))
    | `Bound -> (name, (flavour, kind, count))

(* Generates next letter to use as a type variable name.  Uses side effects to
   increment variable counter.  This ensures that the next call generates next
   letter of the alphabet. *)
let next_letter : unit -> string = function _ ->
  let first_letter      = int_of_char 'a' in
  let last_letter       = int_of_char 'z' in
  let num_letters       = last_letter - first_letter + 1 in
  let string_of_ascii n = Char.escaped (char_of_int n) in
  let letter n = string_of_ascii (first_letter + (n mod num_letters)) in
  let rec num_to_letters n =
    letter n ^ (if n >= num_letters
                then (num_to_letters (n / num_letters))
                else "") in
  let n = !tyvar_name_counter in
  begin
    incr tyvar_name_counter;
    num_to_letters n
  end

(* Assigns names to type variables and adds them to a hash table storing type
   variables.  Both folds work by side-effecting on the hash table, which is
   then returned to be used freely outside of this module. *)
let make_names (vars:Vars.vars_list) =
  if Settings.get_value show_raw_type_vars then
    let _ = List.fold_left
      (fun _ (var, spec) ->
         match Hashtbl.lookup tyvar_name_map var with
           | None ->
              Hashtbl.add tyvar_name_map var (init spec (string_of_int var))
           | Some (name, spec') ->
              Hashtbl.add tyvar_name_map var (combine (name, spec') spec))
      () vars
      in tyvar_name_map
  else
    begin
      let _ = List.fold_left
        (fun _ (var, spec) ->
          match Hashtbl.lookup tyvar_name_map var with
          | None -> Hashtbl.add tyvar_name_map var (init spec (next_letter ()))
          | Some (name, spec') ->
             Hashtbl.add tyvar_name_map var (combine (name, spec') spec))
        () vars
      in tyvar_name_map
    end



module BS = Basicsettings
let show_quantifiers     = BS.Types.Print.show_quantifiers
let show_flavours        = BS.Types.Print.show_flavours
let show_kinds           = BS.Types.Print.show_kinds
let hide_fresh_type_vars = BS.Types.Print.hide_fresh_type_vars
let effect_sugar         = BS.Types.effect_sugar

(* Set the quantifiers to be true to display any outer quantifiers.
   Set flavours to be true to distinguish flexible type variables
   from rigid type variables. *)
type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}
type names  = (int, string * Vars.spec) Hashtbl.t
type context = { bound_vars: TypeVarSet.t; shared_effect: int option }

let default_policy () =
  {quantifiers=Settings.get_value show_quantifiers;
   flavours=Settings.get_value show_flavours;
   hide_fresh=Settings.get_value hide_fresh_type_vars;
   kinds=Settings.get_value show_kinds;
   effect_sugar=Settings.get_value effect_sugar}

let empty_context = { bound_vars = TypeVarSet.empty; shared_effect = None }

let has_kind =
  function
  | "" -> ""
  | s -> "::" ^ s

(** Checks that a field environment contains exactly the values passed in a
   list *)
let fields_present_in fields values =
  FieldEnv.size fields = List.length values &&
    List.for_all (fun v -> FieldEnv.mem v fields
                           && is_present (FieldEnv.find v fields))
      values

(** If this type may contain a shared effect. *)
let maybe_shared_effect = function
  | `Function _ | `Lolli _ -> true
  | `Alias ((_, qs, _), _) | `RecursiveApplication { r_quantifiers = qs; _ } ->
     begin match ListUtils.last_opt qs with
     | Some (PrimaryKind.Row, (_, Restriction.Effect)) -> true
     | _ -> false
     end
  | _ -> false

let context_with_shared_effect policy visit =
  let find_row_var r =
    let (_, r, _), _ = unwrap_row r in
    begin match Unionfind.find r with
    | `Var (var, _, _) -> Some var
    | _ -> None
    end
  in
  (* Find a shared effect variable from the right most arrow or type alias. *)
  let rec find_shared_var t =
    match t with
    | `Function (_, _, r) | `Lolli (_, _, r) when maybe_shared_effect r -> find_shared_var r
    | `Function (_, e, _) | `Lolli (_, e, _) -> find_row_var e
    | `Alias ((_, _, ts), _) | `RecursiveApplication { r_args = ts; _ } when maybe_shared_effect t ->
       begin match ListUtils.last ts with
       | `Row e -> find_row_var e
       | _ -> None
       end
    | _ -> None
  in
  let obj =
    object (self)
      inherit Transform.visitor as super

      val var = None
      method var = var

      method! typ typ =
        match self#var with
        | None ->
           begin match find_shared_var typ with
           | Some v -> typ, {<var = Some v>}
           | None -> super#typ typ
           end
        | Some _ -> typ, self
    end
  in
  if policy.effect_sugar then
    let (_, obj) = visit obj in
    { empty_context with shared_effect = obj#var }
  else
    empty_context

let subkind : (policy * names) -> subkind -> string =
  let full (l, r) = "(" ^ Linearity.to_string l ^ "," ^
                      Restriction.to_string r ^ ")" in

  fun (policy, _vars) ->
  if policy.kinds = "full" then
    full
  else if policy.kinds = "hide" then
    function (_, _) -> ""
  else
    function
    | (Linearity.Unl, Restriction.Any)     -> ""
    | (Linearity.Any, Restriction.Any)     -> "Any"
    | (Linearity.Unl, Restriction.Base)    -> Restriction.to_string res_base
    | (Linearity.Any, Restriction.Session) -> Restriction.to_string res_session
    | (Linearity.Unl, Restriction.Effect)  -> Restriction.to_string res_effect
    | (l, r) -> full (l, r)

let kind : (policy * names) -> kind -> string =
  let full (policy, _vars) (k, sk) =
    PrimaryKind.to_string k ^ subkind (policy, _vars) sk in
  fun (policy, _vars) (k, sk) ->
  if policy.kinds = "full" then
    full (policy, _vars) (k, sk)
  else if policy.kinds = "hide" then
    PrimaryKind.to_string k
  else
    match (k, sk) with
    | PrimaryKind.Type, (Linearity.Unl, Restriction.Any) -> ""
    | PrimaryKind.Type, (Linearity.Unl, Restriction.Base) ->
       Restriction.to_string res_base
    | PrimaryKind.Type, (Linearity.Any, Restriction.Session) ->
       Restriction.to_string res_session
    | PrimaryKind.Type, sk ->
       subkind ({policy with kinds="full"}, _vars) sk
    | PrimaryKind.Row, (Linearity.Unl, Restriction.Any) ->
       PrimaryKind.to_string pk_row
    | PrimaryKind.Row, (Linearity.Unl, Restriction.Effect) ->
       PrimaryKind.to_string pk_row
    | PrimaryKind.Presence, (Linearity.Unl, Restriction.Any) ->
       PrimaryKind.to_string pk_presence
    | PrimaryKind.Row, _ | PrimaryKind.Presence, _ ->
       full ({policy with kinds="full"}, _vars) (k, sk)

let quantifier : (policy * names) -> quantifier -> string =
  fun (policy, vars) q ->
    let k = kind_of_quantifier q in
    Vars.find (var_of_quantifier q) vars ^ has_kind (kind (policy, vars) k)

(** If type variable names are hidden return a generic name n1. Otherwise
   pass name of type variable to n2 so that it can construct a name. *)
let name_of_type_plain { bound_vars; _ } (policy, vars : policy * names) var n1 n2 =
  let name, (flavour, _, count) = Vars.find_spec var vars in
  if policy.hide_fresh && count = 1
     && ((flavour = `Flexible && not (policy.flavours)) || not (TypeVarSet.mem var bound_vars))
  then
    n1
  else
    n2 name

let name_of_type context p var k n1 n2 =
  name_of_type_plain context p var n1 n2 ^ has_kind (subkind p k)

let rec is_row_var known (_, rv, _) =
  match Unionfind.find rv with
  | `Var (var, _, _) when var = known -> true
  | `Body b -> is_row_var known b
  | _ -> false

let rec datatype : context -> policy * names -> datatype -> string =
  fun ({ bound_vars; _ } as context) ((policy, vars) as p) t ->
    let sd = datatype context p in

    let unwrap = fst -<- unwrap_row in
      (* precondition: the row is unwrapped *)
    let string_of_tuple context (field_env, _, _) =
      let tuple_env =
        FieldEnv.fold
          (fun i f tuple_env ->
             match f with
               | `Present t         -> IntMap.add (int_of_string i) t tuple_env
               | (`Absent | `Var _) -> assert false)
          field_env
          IntMap.empty in
      let ss = List.rev (IntMap.fold (fun _ t ss -> (datatype context p t) :: ss) tuple_env []) in
        "(" ^ String.concat ", " ss ^  ")" in

    let name_of_type = name_of_type context (policy, vars) in

    let name_of_eff_var ~allows_shared var _ nh nv =
      match context.shared_effect with
      | None -> name_of_type_plain context (policy, vars) var nh nv
      | Some v ->
         if allows_shared then
           (* If we're in a context with the shared variable, try to use it
              otherwise explicitly name it. *)
           if v = var then nh
             else
               let name, _ = Vars.find_spec var vars in
               nv name
         else
           (* Otherwise the shared effect variable must be explicitly referred to as "_". *)
           if v = var then nv "_" else name_of_type_plain context (policy, vars) var nh nv
    in

    (* Pretty-prints an arrow effect variable *)
    let ppr_eff_var ~args ~allows_shared to_match closed
          (flex_name_hidden, flex_name)
          (name_hidden, name) =
      match Unionfind.find to_match with
      | `Var (var, k, `Flexible) when policy.flavours ->
         name_of_eff_var ~allows_shared var k flex_name_hidden flex_name
      | `Var (var, k, _) ->
         name_of_eff_var ~allows_shared var k name_hidden name
      | `Closed      -> closed
      | `Body t'     -> datatype context p (`Function (args, t', t))
      | `Recursive _ -> assert false in

    (* Pretty-prints function spaces.
       `ah` argument stands for "arrow head", either ">" (for normal function
            space) or "@" (for linear types' space). *)
    let ppr_function_type args effects t ah ht =
     let (fields, row_var, dual) = unwrap effects in
     assert (not dual);

     let fields_present = fields_present_in fields in
     let allows_shared = not (maybe_shared_effect t) in

     let sd = datatype context p in

     let ppr_arrow () =
       if fields_present [] then
         ppr_eff_var ~args ~allows_shared row_var ("{}-" ^ ah)
             ("-%-" ^ ah, fun name -> "-%" ^ name ^ "-" ^ ah)
             ("-" ^ ah,   fun name -> "-"  ^ name ^ "-" ^ ah)
       else if fields_present ["wild"]
       then
         ppr_eff_var ~args ~allows_shared row_var ("{}~" ^ ah)
             ("~%~" ^ ah, fun name -> "~%" ^ name ^ "~" ^ ah)
             ("~" ^ ah,   fun name -> "~"  ^ name ^ "~" ^ ah)
       else if fields_present ["hear"; "wild"]
       then
         let ht' = ht fields in
         ppr_eff_var ~args ~allows_shared row_var ("{:" ^ ht' ^ "}~" ^ ah)
             ("{:" ^ ht' ^ "|%}~" ^ ah, fun name -> "{:" ^ ht' ^ "|%" ^ name ^ "}~" ^ ah)
             ("{:" ^ ht' ^ "|_}~" ^ ah, fun name -> "{:" ^ ht' ^ "|"  ^ name ^ "}~" ^ ah)
       else
           (* to guarantee termination it's crucial that we
              invoke row on the original wrapped version of
              the effect row *)
         let row = row ~name:(fun _ _ -> name_of_eff_var ~allows_shared) in
         if FieldEnv.mem "wild" fields &&
           is_present (FieldEnv.find "wild" fields) then
           "{" ^ row ~strip_wild:true "," context p effects ^ "}~" ^ ah
         else
           "{" ^ row "," context p effects ^ "}-" ^ ah
       in begin match concrete_type args with
          | `Record row when is_tuple ~allow_onetuples:true row ->
             (* Let bindings are needed here to ensure left-to-right
                generation of type variable names.
                See Note [Variable names in error messages] *)
             let row_str   = string_of_tuple context row in
             let arrow_str = ppr_arrow () in
             let sd_str    = sd t in
             row_str ^ " " ^ arrow_str ^ " " ^ sd_str
          | _ -> assert false
          end

    in match t with
        | `Not_typed       -> "not typed"
        | `Primitive p     -> Primitive.to_string p
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Var (var, k, `Flexible) when policy.flavours ->
                    (name_of_type var k "%" (fun name -> "%" ^ name))
                | `Var (var, k, _) ->
                    (name_of_type var k "_" (fun name -> name))
                | `Recursive (var, body) ->
                    if TypeVarSet.mem var bound_vars then
                      Vars.find var vars
                    else
                      "mu " ^ Vars.find var vars ^ " . " ^
                        datatype { context with bound_vars = TypeVarSet.add var bound_vars } p body
                | `Body t -> sd t
            end
        | `Function (args, effects, t) ->
           let ht fields =
           match FieldEnv.find "hear" fields with
           | `Present t -> sd t
           | _          -> assert false in
           ppr_function_type args effects t ">" ht
        | `Lolli    (args, effects, t) ->
           let ht fields =
           sd (match FieldEnv.find "hear" fields with
               | `Present t -> t
               | _          -> assert false)
           in ppr_function_type args effects t "@" ht
        | `Record r ->
            let ur = unwrap r in
              (if is_tuple ur then string_of_tuple context r
               else "(" ^ row "," context p r ^ ")")
        | `Variant r -> "[|" ^ row "|" context p r ^ "|]"
        | `Effect r -> "{" ^ row "," context p r ^ "}"
        | `ForAll (tyvars, body) ->
            let bound_vars =
              List.fold_left
                (fun bound_vars tyvar ->
                   TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
                bound_vars tyvars
            in
              if not (policy.flavours) then
                match tyvars with
                | [] -> datatype { context with bound_vars } p body
                | _ ->
                   "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
              else
                "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
        | `Input  (t, s) -> "?(" ^ sd t ^ ")." ^ sd s
        | `Output (t, s) -> "!(" ^ sd t ^ ")." ^ sd s
        | `Select bs -> "[+|" ^ row "," context p bs ^ "|+]"
        | `Choice bs -> "[&|" ^ row "," context p bs ^ "|&]"
        | `Dual s -> "~" ^ sd s
        | `End -> "End"
        | `Table (r, w, n)   ->
           (* TODO: pretty-print this using constraints? *)
           "TableHandle(" ^
             sd r ^ "," ^
             sd w ^ "," ^
             sd n ^ ")"
        | `Lens typ ->
          let open Lens in
          let sort = Type.sort typ in
          let cols = Sort.present_colset sort |> Column.Set.elements in
          let fds = Sort.fds sort in
          let predicate =
            Sort.predicate sort
            |> OptionUtils.from_option (Phrase.Constant.bool true) in
          let pp_col f col =
            Format.fprintf f "%s : %a"
              (Lens.Column.alias col)
              Lens.Phrase.Type.pp_pretty (Lens.Column.typ col) in
          if Lens.Type.is_abstract typ
          then
            if Lens.Type.is_checked typ
            then
              Format.asprintf "LensChecked((%a), { %a })"
                (Lens.Utility.Format.pp_comma_list pp_col) cols
                Lens.Fun_dep.Set.pp_pretty fds
            else
              Format.asprintf "LensUnchecked((%a), { %a })"
                (Lens.Utility.Format.pp_comma_list pp_col) cols
                Lens.Fun_dep.Set.pp_pretty fds
          else
            Format.asprintf "Lens((%a), %a, { %a })"
              (Lens.Utility.Format.pp_comma_list pp_col) cols
              Lens.Database.fmt_phrase_dummy predicate
              Lens.Fun_dep.Set.pp_pretty fds
        | `Alias ((s, _, ts), _) | `RecursiveApplication { r_name = s; r_args = ts; _ } ->
           let ts =
             match ListUtils.unsnoc_opt ts, context.shared_effect with
             | Some (ts, `Row r), Some v when maybe_shared_effect t && is_row_var v r ->
                let ts = List.map (type_arg context p) ts in
                let (fields, _, _), _ = unwrap_row r in
                if StringMap.is_empty fields then
                  ts
                else
                  let r = row ~name:(fun _ _ -> name_of_eff_var ~allows_shared:true) "," context p r in
                  ts @ ["{" ^ r ^ "}"]
             | _ -> List.map (type_arg context p) ts
           in
           begin match ts with
           | [] -> Module_hacks.Name.prettify s
           | _ ->
              Printf.sprintf "%s (%s)"
                (Module_hacks.Name.prettify s)
                (String.concat "," ts)
           end
        | `Application (l, [elems]) when Abstype.equal l list ->  "["^ (type_arg context p) elems ^"]"
        | `Application (s, []) -> Abstype.name s
        | `Application (s, ts) ->
            let vars = String.concat "," (List.map (type_arg context p) ts) in
            Printf.sprintf "%s (%s)" (Abstype.name s) vars
and presence ({ bound_vars; _ } as context) ((policy, vars) as p) =
  function
    | `Present t ->
      begin
        match concrete_type t with
        | `Record row when is_empty_row row -> ""
        | _                                 -> ":" ^ datatype context p t
      end
    | `Absent -> "-"
    | `Var point ->
        begin
          let name_of_type var n1 n2 =
            let name, (_, _, count) = Vars.find_spec var vars in
            if policy.hide_fresh && count = 1 && not (TypeVarSet.mem var bound_vars) then n1
            else (n2 name) in
          match Unionfind.find point with
            | `Var (var, _, `Flexible) when policy.flavours ->
               name_of_type var "{%}" (fun name -> "{%" ^ name ^ "}")
            | `Var (var, _, _) ->
               name_of_type var "{_}" (fun name -> "{" ^ name ^ "}")
            | `Body f ->
                presence context p f
        end

and row ?(name=name_of_type) ?(strip_wild=false) sep context p (field_env, rv, dual) =
  (* FIXME:

     should quote labels when necessary, i.e., when they
     contain non alpha-numeric characters
  *)
  let field_strings =
    FieldEnv.fold
      (fun label f field_strings ->
        if strip_wild && label = "wild" then
          field_strings
        else
          (label ^ presence context p f) :: field_strings)
      field_env [] in

  let row_var_string = row_var name sep context p rv in
    String.concat sep (List.rev (field_strings)) ^
      begin
        match row_var_string with
          | None -> ""
          | Some s -> "|"^ (if dual then "~" else "") ^ s
      end
and row_var name_of_type sep ({ bound_vars; _ } as context) ((policy, vars) as p) rv =
  match Unionfind.find rv with
    | `Closed -> None
    | `Var (var, k, `Flexible) when policy.flavours ->
       Some (name_of_type context (policy, vars) var k "%" (fun name -> "%" ^ name))
    | `Var (var, k, _) ->
       Some (name_of_type context (policy, vars) var k "_" (fun name -> name))
    | `Recursive (var, r) ->
        if TypeVarSet.mem var bound_vars then
          Some (Vars.find var vars)
        else
          Some ("(mu " ^ Vars.find var vars ^ " . " ^
                  row sep { context with bound_vars = TypeVarSet.add var bound_vars } p r ^ ")")
    | `Body r -> Some (row sep context p r)

and type_arg context p =
  function
    | `Type t -> datatype context p t
    | `Row r -> "{ " ^ row "," context p r ^ " }"
    | `Presence f -> "::Presence (" ^ presence context p f ^ ")"

let tycon_spec ({ bound_vars; _ } as context) p =
  let bound_vars tyvars =
    List.fold_left
      (fun bound_vars tyvar ->
         TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
      bound_vars tyvars in

  function
    | `Alias (tyvars, body) ->
        let ctx = { context with bound_vars = bound_vars tyvars } in
        begin
          match tyvars with
            | [] -> datatype ctx p body
            | _ -> mapstrcat "," (quantifier p) tyvars ^"."^ datatype ctx p body
        end
    | `Mutual _ -> "mutual"
    | `Abstract _ -> "abstract"

let strip_quantifiers =
  function
    | `ForAll (_, t)
    | t -> t




(** Generates new variable names for things in the list, adding them to already
    existing pool of type variable names.
 *)
let add_tyvar_names (f : 'a -> Vars.vars_list) (tys : 'a list) =
  List.iter (fun t -> let _ = make_names (f t) in ()) tys

(** Builds a fresh set of type variable names for a given list of things.  This
    function is called:

    * when pretty-printing a type.  It then builds type variable names for a
      single thing that is being printed.

    * when printing error messages.  It then builds a consistent set of variable
      names for several different types appearing in the error message.
 *)
let build_tyvar_names ~refresh_tyvar_names (f : 'a -> Vars.vars_list)
      (tys : 'a list) =
  if refresh_tyvar_names then
    begin tyvar_name_counter := 0; Hashtbl.reset tyvar_name_map; end;
  add_tyvar_names f tys

(*

Note [Refreshing type variable names]
=====================================

Optional argument refresh_tyvar_names passed to string_of_* pretty-printing
functions determines whether the set of variable names should be refreshed
(default) or re-used.  The latter is used for printing error messages, where we
want consistent type variable names across several calls to pretty-printing
functions.

See Note [Variable names in error messages].

 *)

module DecycleTypes  =
struct
  let elim_recursive_type_cycles_visitor = new ElimRecursiveTypeCyclesTransform.visitor

  let datatype t = fst (elim_recursive_type_cycles_visitor#typ t)
  let row r = fst (elim_recursive_type_cycles_visitor#row r)
  (*let field_spec p = fst (elim_recursive_type_cycles_visitor#field_spec p)
  let type_arg ta = fst (elim_recursive_type_cycles_visitor#type_arg ta)
  let row_var rv = fst (elim_recursive_type_cycles_visitor#row_var rv)
  let quantifier q = fst (elim_recursive_type_cycles_visitor#quantifier q) *)

end

(* string conversions *)


let string_of_datatype ?(policy=default_policy) ?(refresh_tyvar_names=true)
                       (t : datatype) =
  if Settings.get_value Basicsettings.print_types_pretty then
    let policy = policy () in
    let t = if policy.quantifiers then t
            else strip_quantifiers t in
    build_tyvar_names ~refresh_tyvar_names Vars.free_bound_type_vars [t];
    let context = context_with_shared_effect policy (fun o -> o#typ t) in
    datatype context (policy, tyvar_name_map) t
  else
    raw_show_datatype (DecycleTypes.datatype t)

let string_of_row ?(policy=default_policy) ?(refresh_tyvar_names=true) therow =
  if Settings.get_value Basicsettings.print_types_pretty then
    let policy = policy () in
    build_tyvar_names ~refresh_tyvar_names Vars.free_bound_row_type_vars [therow];
    let context = context_with_shared_effect policy (fun o -> o#row therow) in
    row "," context (policy, tyvar_name_map) therow
  else
    raw_show_row (DecycleTypes.row therow)

let string_of_presence ?(policy=default_policy) ?(refresh_tyvar_names=true)
                       (f : field_spec) =
  build_tyvar_names ~refresh_tyvar_names FreeTypeVars.free_bound_field_spec_type_vars [f];
  presence empty_context (policy (), tyvar_name_map) f

let string_of_type_arg ?(policy=default_policy) ?(refresh_tyvar_names=true)
                       (arg : type_arg) =
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names FreeTypeVars.free_bound_type_arg_type_vars [arg];
  let context = context_with_shared_effect policy (fun o -> o#type_arg arg) in
  type_arg context (policy, tyvar_name_map) arg

let string_of_row_var ?(policy=default_policy) ?(refresh_tyvar_names=true) the_row_var =
  build_tyvar_names ~refresh_tyvar_names FreeTypeVars.free_bound_row_var_vars [the_row_var];
  match row_var name_of_type "," empty_context (policy (), tyvar_name_map) the_row_var
  with | None -> ""
       | Some s -> s

let string_of_tycon_spec ?(policy=default_policy) ?(refresh_tyvar_names=true) (tycon : tycon_spec) =
  build_tyvar_names ~refresh_tyvar_names FreeTypeVars.free_bound_tycon_type_vars [tycon];
  tycon_spec empty_context (policy (), tyvar_name_map) tycon

let string_of_quantifier ?(policy=default_policy) ?(refresh_tyvar_names=true) (quant : quantifier) =
  build_tyvar_names ~refresh_tyvar_names FreeTypeVars.free_bound_quantifier_vars [quant];
  quantifier (policy (), tyvar_name_map) quant
