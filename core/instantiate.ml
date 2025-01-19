open CommonTypes
open Utility
open Types

exception ArityMismatch of (int * int)

let internal_error message =
  Errors.internal_error ~filename:"instantiate.ml" ~message

let unexpected_tag () =
  raise Types.tag_expectation_mismatch

let show_recursion = Basicsettings.Types.show_recursion

let show_instantiation
  = Settings.(flag "show_instantiation"
              |> depends Debug.enabled
              |> convert parse_bool
              |> sync)
(*
  instantiation environment:
    for stopping cycles during instantiation
*)
type inst_env = meta_type_var IntMap.t

(* The type of maps given to the actual instantiation functions *)
type instantiation_maps = Types.type_arg IntMap.t

(* TODO: rationalise instantiation
     - do we need all of instantiate_datatype, instantiate_typ, etc?
     - what should they be named?
     - is instantiation for first-class polymorphism correct?
*)

let instantiates : instantiation_maps -> (datatype -> datatype) * (row -> row) * (field_spec -> field_spec) =

  (* used for ordinary types and field_specs. Rows have their own special logic to perform flattening *)
  let rec inst_typ : instantiation_maps -> inst_env -> datatype -> datatype = fun inst_map rec_env datatype ->
    let inst = inst_typ inst_map rec_env in
    let instr = inst_row inst_map rec_env in
    let instta = inst_type_arg inst_map rec_env in
    (* let () = TypeUtils.check_type_wellformedness datatype in *)
    match datatype with
        | Not_typed -> raise (internal_error "Not_typed' passed to `instantiate'")
        | Primitive _  -> datatype
        | Meta point ->
            let t = Unionfind.find point in
              begin
                match t with
                  | Closed -> datatype
                  | Var (var, _, _) ->
                     (* TODO: kinding check here? *)
                      if IntMap.mem var inst_map then
                        snd (IntMap.find var inst_map)
                      else
                        datatype
                  | Recursive (var, kind, t) ->
                      Debug.if_set (show_recursion) (fun () -> "rec (instantiate)1: " ^(string_of_int var));

                      if IntMap.mem var rec_env then
                        (Meta (IntMap.find var rec_env))
                      else
                        begin
                          let var' = Types.fresh_raw_variable () in
                          let point' = Unionfind.fresh (Var (var', kind, `Flexible)) in
                          let t' = inst_typ inst_map (IntMap.add var point' rec_env) t in
                          let _ = Unionfind.change point' (Recursive (var', kind, t')) in
                            Meta point'
                        end
                  | _ ->
                     (* "Body" case *)
                     inst t
              end
        | Function (f, m, t) -> Function (inst f, instr m, inst t)
        | Lolli (f, m, t) -> Lolli (inst f, instr m, inst t)
        | Record row -> Record (instr row)
        | Variant row -> Variant (instr row)
        | Effect row -> Effect (instr row)
        | Operation (f, t, b) -> Operation (inst f, inst t, b)
        | Table (tmp, f, d, r) -> Table (tmp, inst f, inst d, inst r)
        | ForAll (qs, t) ->
           let remove_shadowed_quantifier tmap q =
             let var = Quantifier.to_var q in
             IntMap.remove var tmap
           in
           let updated_inst_map =
             List.fold_left remove_shadowed_quantifier inst_map qs
           in
            ForAll (qs, inst_typ updated_inst_map rec_env t)
        | Alias (k, (name, qs, ts, is_dual), d) ->
            Alias (k, (name, qs, List.map instta ts, is_dual), inst d)
        | Application (n, elem_type) ->
            Application (n, List.map instta elem_type)
        | RecursiveApplication app ->
            RecursiveApplication { app with r_args =
              List.map instta app.r_args }
        | Input (t, s) -> Input (inst t, inst s)
        | Output (t, s) -> Output (inst t, inst s)
        | Select fields -> Select (instr fields)
        | Choice fields -> Choice (instr fields)
        | Dual s -> Types.dual_type (inst s)
        | Lens s -> Lens s
        | End -> End
        (* presence stuff*)
        | Present t -> Present (inst t)
        | Absent -> Absent
        (* rows *)
        | Row _ as r -> instr r
        | Closed -> Closed
          (* bad *)
        | Var _
        | Recursive _
         -> raise tag_expectation_mismatch



  and inst_row : instantiation_maps -> inst_env -> row -> row = fun inst_map rec_env row ->
    let inst = inst_typ inst_map rec_env in
    (* BUG?

       If we change this to unwrap_row then it sometimes leads to
       divergence during type inference. Is this the correct
       behaviour? Why does this happen? *)
    let field_env, row_var, dual = flatten_row row |> TypeUtils.extract_row_parts in

    let is_closed =
      match Unionfind.find row_var with
        | Closed -> true
        | _ -> false in

    let field_env' = StringMap.fold
      (fun label f field_env' ->
         let rec add =
           function
             | Present t -> StringMap.add label (Present (inst t)) field_env'
             | Absent ->
                 if is_closed then field_env'
                 else StringMap.add label Absent field_env'
             | Meta point ->
                begin
                 match Unionfind.find point with
                   | Var (var, _, _) ->
                       let f =
                         if IntMap.mem var inst_map then
                           snd (IntMap.find var inst_map)
                         else
                           Meta point
                       in
                         StringMap.add label f field_env'
                   | f ->
                      add f
                end
             | Var _ -> Debug.print "oops!"; unexpected_tag ()
             | _t ->
                (* Debug.print ("t: "^Types.string_of_datatype t); *)
                unexpected_tag ()
         in
           add f)
      field_env
      StringMap.empty in
    let field_env'', row_var', dual' = inst_row_var inst_map rec_env row_var dual |> TypeUtils.extract_row_parts in
    Row (StringMap.fold StringMap.add field_env' field_env'', row_var', dual')
        (* precondition: row_var has been flattened *)
  and inst_row_var : instantiation_maps -> inst_env -> row_var -> bool -> row = fun inst_map rec_env row_var dual ->
    (* HACK: fix the ill-formed rows that are introduced in the
       instantiation maps *)
    (* TODO: ensure that this never happens or embrace it? *)
    (* We should at least track down *where* these Meta variables of row kind are being introduced *)
    let rowify t =
      match t with
      | Row _ -> t
      | Meta row_var -> Row (StringMap.empty, row_var, false)
      | Alias (PrimaryKind.Row, _,row) -> row
      | _ -> assert false in
    let instr = inst_row inst_map rec_env in
    let dual_if = if dual then dual_row else fun x -> x in
    match Unionfind.find row_var with
    | Closed -> Row (StringMap.empty, row_var, dual)
    | Var (var, _, _) ->
        if IntMap.mem var inst_map then
          dual_if (rowify (snd (IntMap.find var inst_map)))
        else
          Row (StringMap.empty, row_var, dual)
    | Recursive (var, kind, rec_row) ->
        if IntMap.mem var rec_env then
          Row (StringMap.empty, IntMap.find var rec_env, dual)
        else
          begin
            let var' = Types.fresh_raw_variable () in
            let point' = Unionfind.fresh (Var (var', kind, `Flexible)) in
            let rec_row' = inst_row inst_map (IntMap.add var point' rec_env) rec_row in
            let _ = Unionfind.change point' (Recursive (var', kind, rec_row')) in
              Row (StringMap.empty, point', dual)
          end
    | row ->
       dual_if (instr row)

  and inst_presence : instantiation_maps -> inst_env -> datatype -> datatype =
    fun inst_map rec_env p -> inst_typ inst_map rec_env p

  and inst_type_arg : instantiation_maps -> inst_env -> type_arg -> type_arg = fun inst_map rec_env ->
    let open PrimaryKind in
    fun (pk, t) ->
    match pk with
    | Type     -> Type, inst_typ inst_map rec_env t
    | Row      -> Row, inst_row inst_map rec_env t
    | Presence -> Presence, inst_presence inst_map rec_env t in

  let rec_env = IntMap.empty in
  fun inst_map ->
    inst_typ inst_map rec_env,
    inst_row inst_map rec_env,
    inst_typ inst_map rec_env

let instantiate_datatype = instantiates ->- fst3

(** instantiate_typ t

    remove any quantifiers and rename bound type vars accordingly
*)
let instantiate_typ : bool -> datatype -> (type_arg list * datatype) = fun rigid t ->
  match concrete_type t with
    | ForAll (quantifiers, t) as dtype ->
        let () =
          Debug.if_set (show_instantiation)
            (fun () -> "Instantiating datatype: " ^ string_of_datatype dtype) in

        let typ (var, kind) (inst_env, tys) =
          let var' = fresh_raw_variable () in
          let rigidity = if rigid then `Rigid else `Flexible in
          let new_var = Var (var', kind, rigidity) in
          let point = Unionfind.fresh new_var in
          let ty =
            let open PrimaryKind in
            match Kind.primary_kind kind with
            | (Type | Presence) as pk -> pk, Meta point
            | Row -> Row, Row (StringMap.empty, point, false) in
          IntMap.add var ty inst_env, ty :: tys in

        let inst_map, tys =
          List.fold_left
            (fun envs (var, kind) -> typ (var, kind) envs)
            (IntMap.empty, [])
            quantifiers in

        let tys = List.rev tys in
        (* let qs = List.rev qs in *)
        let body = instantiate_datatype inst_map t in
        Debug.if_set (show_instantiation)
          (fun () -> "...instantiated datatype with "^mapstrcat ", " (fun t -> Types.string_of_type_arg t) tys);
        tys, body
    | t -> [], t

(** instantiate_rigid t

    as instantiate_typ, but instantiates the bound type variables with fresh
    rigid type variables
*)
let instantiate_rigid : datatype -> (type_arg list * datatype) = instantiate_typ true

let instantiate_typ = instantiate_typ false

(*  fun t -> *)
(*   match concrete_type t with *)
(*     | `ForAll (quantifiers, t) as dtype -> *)
(*         let () = *)
(*           Debug.if_set (show_instantiation) *)
(*             (fun () -> "Instantiating datatype (rigidly): " ^ string_of_datatype dtype) in *)

(*         let typ (var, subkind) (tenv, renv, penv, tys) = *)
(*           let t = fresh_rigid_type_variable subkind in *)
(*             IntMap.add var t tenv, renv, penv, `Type t :: tys in *)

(*         let row (var, subkind) (tenv, renv, penv, tys) = *)
(*           let r = fresh_rigid_row_variable subkind in *)
(*             tenv, IntMap.add var (StringMap.empty, r) renv, penv, `Row (StringMap.empty, r) :: tys in *)

(*         let presence var (tenv, renv, penv, tys) = *)
(*           let t = fresh_rigid_presence_variable () in *)
(*             tenv, renv, IntMap.add var t penv, `Presence t :: tys in *)

(*         let tenv, renv, penv, tys = List.fold_left *)
(*           (fun env -> function *)
(*              | `TypeVar ((var, subkind), _) -> typ (var, subkind) env *)
(*              | `RowVar ((var, subkind), _) -> row (var, subkind) env *)
(*              | `PresenceVar (var, _) -> presence var env *)
(*           ) (IntMap.empty, IntMap.empty, IntMap.empty, []) quantifiers in *)

(*         let tys = List.rev tys in *)
(*           tys, instantiate_datatype (tenv, renv, penv) t *)
(*     | t -> [], t *)


(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.

    This returns the type arguments var is instantiated with
    and the instantiated type.
 *)
let instantiate : environment -> string -> type_arg list * datatype =
  fun env var ->
    let t =
      try
        Env.String.find var env
      with NotFound _ ->
        raise (Errors.UndefinedVariable ("Variable '"^ var ^ "' does not refer to a declaration"))
    in
(*       Debug.print ("t1: " ^ Types.string_of_datatype t); *)
      let t = instantiate_typ t in
(*       Debug.print ("t2: " ^ Types.string_of_datatype (snd t)); *)
        t

let rigid : environment -> string -> type_arg list * datatype =
  fun env var ->
    let t =
      try
        Env.String.find var env
      with NotFound _ ->
        raise (Errors.UndefinedVariable ("Variable '"^ var ^ "' does not refer to a declaration"))
    in
      instantiate_rigid t

let var = instantiate
let typ = instantiate_typ
let typ_rigid = instantiate_rigid
let datatype = instantiate_datatype
let row = instantiates ->- snd3
let presence = instantiates ->- thd3

module SEnv = Env.String

let populate_instantiation_map ~name qs tyargs =
  List.fold_right2
    (fun q ((pk, _) as tyarg) inst_map ->
      if pk <> Quantifier.to_primary_kind q then
        raise
          (internal_error
             ("Kind mismatch in type application: " ^
                name ^ " applied to type arguments: " ^
                  mapstrcat ", " (fun t -> Types.string_of_type_arg t) tyargs))
      else
        IntMap.add (Quantifier.to_var q) tyarg inst_map)
    qs tyargs IntMap.empty

let instantiation_maps_of_type_arguments :
      bool -> Types.datatype -> Types.type_arg list -> (datatype * instantiation_maps) =
  fun must_instantiate_all_quantifiers pt tyargs ->
    let vars, t = TypeUtils.split_quantified_type pt in
    let tyargs_length = List.length tyargs in
    let vars_length   = List.length vars   in
    let arities_okay =
      if must_instantiate_all_quantifiers
        then tyargs_length = vars_length
        else tyargs_length <= vars_length in

    if (not arities_okay) then
      (Debug.print (Printf.sprintf "# Type variables (total %d)" vars_length);
       let tyvars = String.concat "\n" @@ List.mapi (fun i t -> (string_of_int @@ i+1) ^ ". " ^ Quantifier.to_string t) vars in
       Debug.print tyvars;
       Debug.print (Printf.sprintf "\n# Type arguments (total %d)" tyargs_length);
       let tyargs' = String.concat "\n" @@ List.mapi (fun i arg -> (string_of_int @@ i+1) ^ ". " ^ Types.string_of_type_arg arg) tyargs in
       Debug.print tyargs';
       (* We don't have position information at this point. Any code invoking this
        * should either have done this check already, or does not have the position
        * information since a type is being introduced explicitly. *)
       raise (ArityMismatch (vars_length, tyargs_length)));

    let vars, remaining_quantifiers =
      if tyargs_length = vars_length then
        vars, []
      else
        (take tyargs_length vars, drop tyargs_length vars) in
    let inst_map = populate_instantiation_map ~name:(Types.string_of_datatype pt) vars tyargs in
    match remaining_quantifiers with
      | [] -> t, inst_map
      | _ -> ForAll (remaining_quantifiers, t),  inst_map


let apply_type : Types.datatype -> Types.type_arg list -> Types.datatype = fun pt tyargs ->
  let (t, instantiation_map) = instantiation_maps_of_type_arguments false pt tyargs in
  instantiate_datatype instantiation_map t

let build_fresh_quantifiers :
      Quantifier.t list -> Quantifier.t list * Types.type_arg list = fun qs ->
  List.split
    ((List.map
       (fun q ->
         let q, t = Types.fresh_quantifier (Quantifier.to_kind q) in
         (q, t)))
    qs)


(*
  ensure that t has fresh quantifiers
*)
let freshen_quantifiers t =
  match concrete_type t with
    | ForAll (qs, body) ->
        begin
          match qs with
            | [] -> body
            | qs ->
               let qs, tyargs = build_fresh_quantifiers qs in
               ForAll (qs, apply_type t tyargs)
        end
    | t -> t

(*
  replace the quantifiers in t with qs'
*)
let replace_quantifiers t qs' =
  match concrete_type t with
    | ForAll (qs, _) ->
        let tyargs =
          List.map2
            (fun q q' ->
              assert (Quantifier.to_primary_kind q = Quantifier.to_primary_kind q');
              type_arg_of_quantifier q')
            qs
            qs'
        in
          ForAll (qs', apply_type t tyargs)
    | t -> t

let recursive_application name qs tyargs body =
  let inst_map = populate_instantiation_map ~name qs tyargs in
  let (_, body) = typ (instantiate_datatype inst_map body) in
  body

let alias name tyargs env : Types.typ =
  (* This is just type application.

     (\Lambda x1 ... xn . t) (t1 ... tn) ~> t[ti/xi]
  *)
  let open Types in
  match (SEnv.find_opt name env : Types.tycon_spec option) with
    | None ->
        raise (internal_error (Printf.sprintf "Unrecognised type constructor: %s" name))
    | Some (`Abstract _)
    | Some (`Mutual _) ->
        raise (internal_error (Printf.sprintf "The type constructor: %s is not an alias" name))
    | Some (`Alias (_, vars, _)) when List.length vars <> List.length tyargs ->
        raise (internal_error
        (Printf.sprintf
          "Type alias %s applied with incorrect arity (%d instead of %d). This should have been checked prior to instantiation."
          name (List.length tyargs) (List.length vars)))
    | Some (`Alias (k, vars, body)) ->
        let inst_map = populate_instantiation_map ~name vars tyargs in
        (* instantiate the type variables bound by the alias
           definition with the type arguments *and* instantiate any
           top-level quantifiers *)
        let (_, body) = typ (instantiate_datatype inst_map body) in
          Alias (k, (name, List.map snd vars, tyargs, false), body)
