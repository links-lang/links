open CommonTypes
open Utility
open Types

exception ArityMismatch of (int * int)

let internal_error message =
  Errors.internal_error ~filename:"instantiate.ml" ~message

let show_recursion
  = Settings.(flag "show_recursion"
              |> depends Debug.enabled
              |> convert parse_bool
              |> sync)

let show_instantiation
  = Settings.(flag "show_instantiation"
              |> depends Debug.enabled
              |> convert parse_bool
              |> sync)
(*
  instantiation environment:
    for stopping cycles during instantiation
*)
type inst_type_env = meta_type_var IntMap.t
type inst_row_env = meta_row_var IntMap.t
type inst_env = inst_type_env * inst_row_env

(* The type of maps given to the actual instantiation functions *)
type instantiation_maps = (datatype IntMap.t * row IntMap.t * field_spec IntMap.t)

(* TODO: rationalise instantiation
     - do we need all of instantiate_datatype, instantiate_typ, etc?
     - what should they be named?
     - is instantiation for first-class polymorphism correct?
*)

let instantiates : instantiation_maps -> (datatype -> datatype) * (row -> row) * (field_spec -> field_spec) =
  let rec inst_typ : instantiation_maps -> inst_env -> datatype -> datatype = fun inst_maps rec_env datatype ->
    let inst = inst_typ inst_maps rec_env in
    let instr = inst_row inst_maps rec_env in
    let tenv, _, _ = inst_maps in
    let rec_type_env, rec_row_env = rec_env in
      match datatype with
        | `Not_typed -> raise (internal_error "`Not_typed' passed to `instantiate'")
        | `Primitive _  -> datatype
        | `MetaTypeVar point ->
            let t = Unionfind.find point in
              begin
                match t with
                  | `Var (var, _, _) ->
                      if IntMap.mem var tenv then
                        IntMap.find var tenv
                      else
                        datatype
                  | `Recursive (var, t) ->
                      Debug.if_set (show_recursion) (fun () -> "rec (instantiate)1: " ^(string_of_int var));

                      if IntMap.mem var rec_type_env then
                        (`MetaTypeVar (IntMap.find var rec_type_env))
                      else
                        begin
                          let var' = Types.fresh_raw_variable () in
                          let point' = Unionfind.fresh (`Var (var', (lin_any, res_any), `Flexible)) in
                          let t' = inst_typ inst_maps (IntMap.add var point' rec_type_env, rec_row_env) t in
                          let _ = Unionfind.change point' (`Recursive (var', t')) in
                            `MetaTypeVar point'
                        end
                  | `Body t -> inst t
              end
        | `Function (f, m, t) -> `Function (inst f, instr m, inst t)
        | `Lolli (f, m, t) -> `Lolli (inst f, instr m, inst t)
        | `Record row -> `Record (instr row)
        | `Variant row -> `Variant (instr row)
        | `Effect row -> `Effect (instr row)
        | `Table (f, d, r) -> `Table (inst f, inst d, inst r)
        | `ForAll (qs, t) ->
           let remove_shadowed_quantifier (tmap,rmap,pmap) q =
             let var = Quantifier.to_var q in
             let open CommonTypes.PrimaryKind in
             match Quantifier.to_primary_kind q with
               | Type ->
                  (IntMap.remove var tmap, rmap, pmap)
               | Row ->
                  (tmap, IntMap.remove var rmap, pmap)
               | Presence ->
                  (tmap, rmap, IntMap.remove var pmap)
           in
           let updated_inst_maps =
             List.fold_left remove_shadowed_quantifier inst_maps qs
           in
            `ForAll (qs, inst_typ updated_inst_maps rec_env t)
        | `Alias ((name, qs, ts), d) ->
            `Alias ((name, qs, List.map (inst_type_arg inst_maps rec_env) ts), inst d)
        | `Application (n, elem_type) ->
            `Application (n, List.map (inst_type_arg inst_maps rec_env) elem_type)
        | `RecursiveApplication app ->
            `RecursiveApplication { app with r_args =
              List.map (inst_type_arg inst_maps rec_env) app.r_args }
        | `Input (t, s) -> `Input (inst t, inst s)
        | `Output (t, s) -> `Output (inst t, inst s)
        | `Select fields -> `Select (instr fields)
        | `Choice fields -> `Choice (instr fields)
        | `Dual s -> Types.dual_type (inst s)
        | `Lens s -> `Lens s
        | `End -> `End

  and inst_presence : instantiation_maps -> inst_env -> field_spec -> field_spec = fun inst_maps rec_env ->
    let inst = inst_typ inst_maps rec_env in
    let (_, _, penv) = inst_maps in
    function
      | `Present t -> `Present (inst t)
      | `Absent -> `Absent
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, _) ->
                  if IntMap.mem var penv then
                    IntMap.find var penv
                  else
                    `Var point
              | `Body f ->
                  inst_presence  inst_maps rec_env f
          end
  and inst_row : instantiation_maps -> inst_env -> row -> row = fun inst_maps rec_env row ->
    let inst = inst_typ inst_maps rec_env in
    let (_, _, penv) = inst_maps in
    (* BUG?

       If we change this to unwrap_row then it sometimes leads to
       divergence during type inference. Is this the correct
       behaviour? Why does this happen? *)
    let field_env, row_var, dual = flatten_row row in

    let is_closed =
      match Unionfind.find row_var with
        | `Closed -> true
        | _ -> false in

    let field_env' = StringMap.fold
      (fun label f field_env' ->
         let rec add =
           function
             | `Present t -> StringMap.add label (`Present (inst t)) field_env'
             | `Absent ->
                 if is_closed then field_env'
                 else StringMap.add label `Absent field_env'
             | `Var point ->
                 match Unionfind.find point with
                   | `Var (var, _, _) ->
                       let f =
                         if IntMap.mem var penv then
                           IntMap.find var penv
                         else
                           `Var point
                       in
                         StringMap.add label f field_env'
                   | `Body f ->
                       add f
         in
           add f)
      field_env
      StringMap.empty in
    let field_env'', row_var', dual' = inst_row_var inst_maps rec_env row_var dual in
      StringMap.fold StringMap.add field_env' field_env'', row_var', dual'
        (* precondition: row_var has been flattened *)
  and inst_row_var : instantiation_maps -> inst_env -> row_var -> bool -> row = fun inst_maps rec_env row_var dual ->
    let instr = inst_row inst_maps rec_env in
    let (_, renv, _) = inst_maps in
    let (rec_type_env, rec_row_env) = rec_env in
    let dual_if = if dual then dual_row else fun x -> x in
    match row_var with
      | point ->
          begin
            match Unionfind.find point with
              | `Closed -> (StringMap.empty, row_var, dual)
              | `Var (var, _, _) ->
                  if IntMap.mem var renv then
                    dual_if (IntMap.find var renv)
                  else
                    (StringMap.empty, row_var, dual)
              | `Recursive (var, rec_row) ->
                  if IntMap.mem var rec_row_env then
                    (StringMap.empty, IntMap.find var rec_row_env, dual)
                  else
                    begin
                      let var' = Types.fresh_raw_variable () in
                      let point' = Unionfind.fresh (`Var (var', (lin_any, res_any), `Flexible)) in
                      let rec_row' = inst_row inst_maps (rec_type_env, IntMap.add var point' rec_row_env) rec_row in
                      let _ = Unionfind.change point' (`Recursive (var', rec_row')) in
                        (StringMap.empty, point', dual)
                    end
              | `Body row -> dual_if (instr row)
          end
  and inst_type_arg : instantiation_maps -> inst_env -> type_arg -> type_arg = fun inst_maps rec_env ->
    function
      | `Type t -> `Type (inst_typ inst_maps rec_env t)
      | `Row r -> `Row (inst_row inst_maps rec_env r)
      | `Presence f -> `Presence (inst_presence inst_maps rec_env f)
  in
  let env = (IntMap.empty, IntMap.empty) in
  fun inst_maps ->
    inst_typ inst_maps env,
    inst_row inst_maps env,
    inst_presence inst_maps env

let instantiate_datatype = instantiates ->- fst3

(** instantiate_typ t

    remove any quantifiers and rename bound type vars accordingly
*)
let instantiate_typ : bool -> datatype -> (type_arg list * datatype) = fun rigid t ->
  match concrete_type t with
    | `ForAll (quantifiers, t) as dtype ->
        let () =
          Debug.if_set (show_instantiation)
            (fun () -> "Instantiating datatype: " ^ string_of_datatype dtype) in

        let wrap var subkind =
          if rigid then `Var (var, subkind, `Rigid)
          else `Var (var, subkind, `Flexible) in

        let typ (var, subkind) (tenv, renv, penv, tys, qs) =
          let var' = fresh_raw_variable () in
          let point = Unionfind.fresh (wrap var' subkind) in
          let t = `MetaTypeVar point in
            IntMap.add var t tenv, renv, penv, `Type t :: tys, (var', subkind, `Type point) :: qs in

        let row (var, subkind) (tenv, renv, penv, tys, qs ) =
          let var' = fresh_raw_variable () in
          let r = Unionfind.fresh (wrap var' subkind) in
            tenv, IntMap.add var (StringMap.empty, r, false) renv, penv, `Row (StringMap.empty, r ,false) :: tys, (var', subkind, `Row r) :: qs in

        let presence (var, subkind) (tenv, renv, penv, tys, qs) =
          let var' = fresh_raw_variable () in
          let point = Unionfind.fresh (wrap var' subkind) in
          let t = `Var point in
            tenv, renv, IntMap.add var t penv, `Presence t :: tys, (var, subkind, `Presence point) :: qs in

        let open PrimaryKind in
        let tenv, renv, penv, tys, _qs =
          List.fold_left
            (fun env ->
               function
                 | (var, (Type, subkind))     -> typ (var, subkind) env
                 | (var, (Row, subkind))      -> row (var, subkind) env
                 | (var, (Presence, subkind)) -> presence (var, subkind) env)
            (IntMap.empty, IntMap.empty, IntMap.empty, [], []) quantifiers in

        let tys = List.rev tys in
        (* let qs = List.rev qs in *)
        let body = instantiate_datatype (tenv, renv, penv) t in
        Debug.if_set (show_instantiation) (fun () -> "...instantiated datatype with "^mapstrcat ", " (fun t -> Types.string_of_type_arg t) tys);
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

let populate_instantiation_maps ~name qs tyargs =
  let open PrimaryKind in
  List.fold_right2
    (fun var tyarg (tenv, renv, penv) ->
       match (var, tyarg) with
         | (var, (Type, _subkind)), `Type t ->
             (IntMap.add var t tenv, renv, penv)
         | (var, (Row, _subkind)), `Row row ->
             (tenv, IntMap.add var row renv, penv)
         | (var, (Presence, _subkind)), `Presence f ->
             (tenv, renv, IntMap.add var f penv)
         | _ ->
             raise (internal_error
               ("Kind mismatch in type application: " ^
                name ^ " applied to type arguments: " ^
                mapstrcat ", " (fun t -> Types.string_of_type_arg t) tyargs)))
    qs tyargs (IntMap.empty, IntMap.empty, IntMap.empty)

let instantiation_maps_of_type_arguments :
      bool -> Types.datatype -> Types.type_arg list -> (datatype * instantiation_maps) =
  fun must_instantiate_all_quantifiers pt tyargs ->
    (* Debug.print ("t: " ^ Types.string_of_datatype t); *)
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
    let tenv, renv, penv = populate_instantiation_maps ~name:(Types.string_of_datatype pt) vars tyargs in
    match remaining_quantifiers with
      | [] -> t, (tenv, renv, penv)
      | _ -> `ForAll (remaining_quantifiers, t),  (tenv, renv, penv)



let apply_type : Types.datatype -> Types.type_arg list -> Types.datatype = fun pt tyargs ->
  let (t, instantiation_maps) = instantiation_maps_of_type_arguments false pt tyargs in
  instantiate_datatype instantiation_maps t

let build_fresh_quantifiers :
      Quantifier.t list -> Quantifier.t list * Types.type_arg list = fun qs ->
  let open PrimaryKind in
  List.split (List.map (function
                  | (_, (Type, subkind)) ->
                     let q, t = Types.fresh_type_quantifier subkind in
                     q, `Type t
                  | (_, (Row, subkind)) ->
                     let q, r = Types.fresh_row_quantifier subkind in
                     q, `Row r
                  | (_, (Presence, subkind)) ->
                     let q, f = Types.fresh_presence_quantifier subkind in
                     q, `Presence f)
                qs)

(*
  ensure that t has fresh quantifiers
*)
let freshen_quantifiers t =
  match concrete_type t with
    | `ForAll (qs, body) ->
        begin
          match qs with
            | [] -> body
            | qs ->
               let qs, tyargs = build_fresh_quantifiers qs in
               `ForAll (qs, apply_type t tyargs)
        end
    | t -> t

(*
  replace the quantifiers in t with qs'
*)
let replace_quantifiers t qs' =
  match concrete_type t with
    | `ForAll (qs, _) ->
        let tyargs =
          List.map2
            (fun q q' ->
              assert (Quantifier.to_primary_kind q = Quantifier.to_primary_kind q');
              type_arg_of_quantifier q')
            qs
            qs'
        in
          `ForAll (qs', apply_type t tyargs)
    | t -> t

let recursive_application name qs tyargs body =
  let tenv, renv, penv = populate_instantiation_maps ~name qs tyargs in
  let (_, body) = typ (instantiate_datatype (tenv, renv, penv) body) in
  body

let alias name tyargs env : Types.typ =
  (* This is just type application.

     (\Lambda x1 ... xn . t) (t1 ... tn) ~> t[ti/xi]
  *)
  match (SEnv.find_opt name env : Types.tycon_spec option) with
    | None ->
        raise (internal_error (Printf.sprintf "Unrecognised type constructor: %s" name))
    | Some (`Abstract _)
    | Some (`Mutual _) ->
        raise (internal_error (Printf.sprintf "The type constructor: %s is not an alias" name))
    | Some (`Alias (vars, _)) when List.length vars <> List.length tyargs ->
        raise (internal_error
        (Printf.sprintf
          "Type alias %s applied with incorrect arity (%d instead of %d). This should have been checked prior to instantiation."
          name (List.length tyargs) (List.length vars)))
    | Some (`Alias (vars, body)) ->
        let tenv, renv, penv = populate_instantiation_maps ~name vars tyargs in
        (* instantiate the type variables bound by the alias
           definition with the type arguments *and* instantiate any
           top-level quantifiers *)
        let (_, body) = typ (instantiate_datatype (tenv, renv, penv) body) in
          `Alias ((name, List.map snd vars, tyargs), body)
