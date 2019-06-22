open CommonTypes
open Utility
open Ir

(* ERROR HANDLING*)

let fail_on_ir_type_error =
  Settings.get_value Basicsettings.Ir.fail_on_ir_type_error

let internal_error message =
  raise (Errors.internal_error ~filename:"irCheck.ml" ~message)

(* Artificial "supertype" of all IR types,
   so we can provide arbitrary IR fragments to the error handling functions *)
type ir_snippet =
  | STC    of tail_computation
  | SVal   of value
  | SSpec  of special
  | SBind  of binding
  | SBinds of binding list
  | SProg  of program
  | SNone

let string_of_occurrence : ir_snippet -> string =
  let nl = "\n" in function
  | STC tc ->
    "occurring in IR tail computation:" ^
    nl ^
    Ir.string_of_tail_computation tc
  | SVal v ->
    "occurring in IR value:" ^
    nl ^
    Ir.string_of_value v
  | SSpec s ->
    "occurring in IR special tail computation:" ^
    nl ^
    Ir.string_of_special s
  | SBind b ->
    "occurring in IR binding:" ^
    nl ^
    Ir.string_of_binding b
  | SBinds bs ->
    "occurring in IR binding list:" ^
    nl ^
    String.concat nl (List.map Ir.string_of_binding bs)
  | SProg p ->
    "occurring in IR program:" ^
    nl ^
    Ir.string_of_program p
  | SNone -> ""


let raise_ir_type_error msg occurrence =
  let nl = "\n" in
  let occurrence_string = string_of_occurrence occurrence in
  raise (Errors.IRTypeError (msg ^ nl ^ occurrence_string))


(* Evalutes a lazy value and handles exceptions raised while doing so.
   If we are supposed to fail on IR exceptions, we don't catch any of them.
   If we are supposed to continue after IR type errors, we print a debug
   message and return the alternative value in case of an exception. *)
let handle_ir_type_error lazy_val alternative occurrence =
  if fail_on_ir_type_error then
    (* All exceptions are left unhandled, leaving them for the error handling
       facilities outside of the IR type checker *)
    Lazy.force lazy_val
  else
    (* Catch all errors, print them to debug output, but continue by
       returning the alternative value *)
    try
      Lazy.force lazy_val
    with
      | Errors.IRTypeError msg  ->
         let nl = "\n" in
        (* We saw an IR error, but we are not supposed to abort.
           Print the error and return the alternative value *)
        Debug.print
          (nl ^
          "--------------------------------------------------------------------" ^
          nl ^
          "Continuing after IR Type Error:" ^
          nl ^
          msg ^
          nl ^
          "backtrace:" ^
          nl ^
          Printexc.get_backtrace () ^
          nl ^
          "-------------------------------------------------------------------" ^
          nl);
        alternative
      | exn ->
        let msg =
          "Encountered exception during IR type-checking, saying:\n" ^
          Printexc.to_string exn in
        let occurrence_string = string_of_occurrence occurrence in
        Debug.print (msg ^ occurrence_string);
        alternative

(* Some of the helper functions we use raise their own exceptions.
   We translate them to IR type exceptions here *)
let translate_helper_exns (f : 'a -> 'b) (x : 'a) occurrence : 'b =
  try
    f x
  with
    | TypeUtils.TypeDestructionError msg ->
      raise_ir_type_error msg occurrence
    | Instantiate.ArityMismatch (takes, given) ->
      let msg =
        Printf.sprintf "Arity mismatch during type application/instantiation. \
                        Providing %d type arguments to function that takes %d." given takes in
      raise_ir_type_error msg occurrence

let ensure condition msg occurrence =
  if condition then () else raise_ir_type_error msg occurrence

let _print_substmap subst =
  Debug.print ("Substmap\n:" ^ IntMap.show (fun fmt num -> Format.pp_print_int fmt num) subst)

(* TYPE EQUALITY *)

module Env = Env.Int

type type_eq_context = {
  typevar_subst : Var.var IntMap.t; (* equivalences of typevars *)
  tyenv: Types.kind Env.t (* track kinds of bound typevars *)
}

module RecursionDetector =
struct
  class visitor =
      object
        inherit Types.Transform.visitor as super

        val found_recursion = false
        method get_result () = found_recursion


        method! meta_type_var point = match Unionfind.find point with
          | `Recursive _ -> (point, {< found_recursion = true >})
          | _ -> super#meta_type_var point

        method! meta_row_var point = match Unionfind.find point with
          | `Recursive _ -> (point, {< found_recursion = true >})
          | _ -> super#meta_row_var point


      end

  let is_recursive t =
    let o = new visitor in
    let o' = snd (o#typ t) in
    o'#get_result ()

end



let eq_types occurrence : type_eq_context -> (Types.datatype * Types.datatype) -> bool =
  fun context (t1, t2) ->
    let lookupVar lvar map =
      match IntMap.find_opt lvar map with
      | Some rvar' -> rvar'
      | None       -> lvar in
    let handle_variable primary_kind (lid, lsk, lfd) (rid, rsk, rfd) ctx =
      let subst_map, kind_env = ctx.typevar_subst, ctx.tyenv in
      let rvar' = lookupVar lid subst_map in
      let is_equal = rid = rvar' && lsk = rsk && lfd = rfd in
      match is_equal, lfd with
        | true, `Flexible -> true
        | true, `Rigid ->
           begin match Env.find kind_env rid with
             | Some (primary_kind_env, subkind_env) ->
                ensure
                  (primary_kind = primary_kind_env &&  rsk = subkind_env)
                  "Mismatch between (sub) kind information in variable vs stored in kind environment"
                  SNone;
                true
             | None -> raise_ir_type_error ("Type variable "  ^ (string_of_int rid) ^ " is unbound") occurrence
           end
        | false, _ -> false in
    let rec collapse_toplevel_forall : Types.datatype -> Types.datatype = function
      | `ForAll (qs, t) ->
        begin match collapse_toplevel_forall t with
          | `ForAll (qs', t') ->
              `ForAll (qs @ qs', t')
          | t ->
              begin
                match qs with
                  | [] -> t
                  | _ -> `ForAll (qs, t)
              end
        end
      | t -> t in
    let remove_absent_fields_if_closed row =
      (* assumes that row is flattened already and ignores recursive rows *)
      let (field_env, row_var, dual) = row in
      if Types.is_closed_row row then
        let field_env' =
          Utility.StringMap.filter
            ( fun _ v -> match v with
              | `Absent -> false
              | _ -> true )
            field_env
        in (field_env', row_var, dual)
      else row in

    (* typevar_subst of ctx maps rigid typed/row/presence variables of t1 to corresponding ones of t2 *)
    let rec eqt ((context, t1, t2) : (type_eq_context * Types.datatype * Types.datatype)) =

      let rec unalias = function
        | `Alias (_, x) -> unalias x
        | x             -> x in

      let t1 = unalias (collapse_toplevel_forall t1) in
      let t2 = unalias (collapse_toplevel_forall t2) in

      (* If t2 is recursive at the top, we give up. t1 is checked for recursion later on *)
      if match t2 with
        | `MetaTypeVar mtv ->
          begin match Unionfind.find mtv with
            | `Recursive _ -> true
            | _ -> false
          end
        | `RecursiveApplication _ -> true
        | _ -> false
      then
        begin
        Debug.print "IR typechecker encountered recursive type";
        true
        end
      else
      begin
      match t1 with
      | `Not_typed ->
          begin match t2 with
              `Not_typed -> true
            | _          -> false
          end
      | `Primitive x ->
          begin match t2 with
              `Primitive y -> x = y
            | _            -> false
          end
      | `MetaTypeVar lpoint ->
          begin match Unionfind.find lpoint with
            | `Recursive _ -> Debug.print "IR typechecker encountered recursive type"; true
            | lpoint_cont ->
              begin match t2 with
                `MetaTypeVar rpoint ->
                begin match lpoint_cont, Unionfind.find rpoint with
                | `Var lv, `Var rv -> handle_variable pk_type lv rv context
                | `Body _, `Body _ -> raise (internal_error "Should have removed `Body by now")
                | _ -> false
                end
                | _                   -> false
              end
          end
      | `Function (lfrom, lm, lto) ->
          begin match t2 with
            | `Function (rfrom, rm, rto) ->
              eqt     (context, lfrom, rfrom) &&
              eqt     (context, lto  , rto  ) &&
              eq_rows (context, lm   , rm   )
            | _ -> false
          end
      | `Lolli (lfrom, lm, lto) ->
          begin match t2 with
            | `Function (rfrom, rm, rto) ->
              eqt     (context, lfrom, rfrom) &&
              eqt     (context, lto  , rto  ) &&
              eq_rows (context, lm   , rm   )
            | _  -> false
          end
      | `Record l ->
         begin match t2 with
         | `Record r -> eq_rows (context, l, r)
         | _         -> false
         end
      | `Variant l ->
         begin match t2 with
           `Variant r -> eq_rows (context, l, r)
         | _          -> false
         end
      | `Effect l ->
         begin match t2 with
         | `Effect r -> eq_rows (context, l, r)
         | _         -> false
         end
      | `Application (s, ts) ->
         begin match t2 with
         | `Application (s', ts') ->
            Types.Abstype.equal s s' &&
            List.length ts = List.length ts' &&
            List.for_all2 (fun larg rarg -> eq_type_args (context, larg, rarg)) ts ts'
         | _ -> false
         end
      | `RecursiveApplication _ ->
         Debug.print "IR typechecker encountered recursive type"; true
      | `ForAll (qs, t) ->
         begin match t2 with
         | `ForAll (qs', t') ->
            let (context', quantifiers_match) =
              List.fold_left2 (fun (context, prev_eq) lqvar rqvar ->
                  let lid, _ = lqvar in
                  let rid, _ = rqvar in
                  let l_kind = Types.kind_of_quantifier lqvar in
                  let r_kind = Types.kind_of_quantifier rqvar in
                  let ctx' = { typevar_subst = IntMap.add lid rid context.typevar_subst;
                               tyenv = Env.bind context.tyenv (rid, r_kind)
                             } in
                  (ctx', prev_eq && l_kind = r_kind)
                ) (context,true) qs qs' in
            if quantifiers_match then
              eqt (context', t, t')
            else false
         | _ -> false
         end
      | #Types.session_type as l ->
         begin match t2 with
         | #Types.session_type as r -> eq_sessions (context, l, r)
         | _          -> false
         end

      | `Alias (_, t1_inner) ->
        begin match t2 with
          | `Alias (_, t2_inner) -> eqt (context, t1_inner, t2_inner)
          | t2 -> eqt (context, t1_inner, t2)
        end

      | `Table (lt1, lt2, lt3) ->
         begin match t2 with
         | `Table (rt1, rt2, rt3) ->
            eqt (context, lt1, rt1) &&
            eqt (context, lt2, rt2) &&
            eqt (context, lt3, rt3)
         | _ -> false
         end
      | `Lens _ -> raise (internal_error "The IR type equality check does not support lenses (yet)")
      end

    and eq_sessions (context, l, r) =
      match (l,r) with
      | `Input (lt, _), `Input (rt, _)
        | `Output (lt, _), `Output (rt, _) ->
         eqt (context, lt, rt)
      | `Select l, `Select r
        | `Choice l, `Choice r ->
         eq_rows (context, l, r)
      | `Dual l, `Dual r ->
         eqt (context, l, r)
      | `End, `End -> true
      | _, _ -> false
    and eq_rows (context, r1, r2) =
      let (lfield_env, lrow_var, ldual) = remove_absent_fields_if_closed (Types.flatten_row r1) in
      let (rfield_env, rrow_var, rdual) = remove_absent_fields_if_closed (Types.flatten_row r2) in
      let r1 = eq_field_envs (context, lfield_env, rfield_env) in
      let r2 = eq_row_vars (context, lrow_var, rrow_var) in
        r1 && r2 && ldual=rdual
    and eq_presence (context, l, r) =
      match l, r with
      | `Absent, `Absent -> true
      | `Present lt, `Present rt -> eqt (context, lt, rt)
      | `Var lpoint, `Var rpoint ->
          begin
            match Unionfind.find lpoint, Unionfind.find rpoint with
              | `Body _, _
              | _, `Body _ ->
                  raise (internal_error "should have removed all `Body variants by now")
              | `Var lv, `Var rv -> handle_variable pk_presence lv rv context
              end
      | _, _ -> false
    and eq_field_envs (context, lfield_env, rfield_env) =
      let lfields_in_rfields =
        StringMap.for_all  (fun field lp ->
            match StringMap.find_opt field rfield_env with
              | Some rp -> eq_presence (context, lp, rp)
              | None -> false
          ) lfield_env in
      lfields_in_rfields  && StringMap.cardinal lfield_env = StringMap.cardinal rfield_env
    and eq_row_vars (context, lpoint, rpoint) =
      match Unionfind.find lpoint, Unionfind.find rpoint with
      | `Closed, `Closed ->  true
      | `Var lv, `Var rv ->   handle_variable pk_row lv rv context
      | `Recursive _, _
      | _, `Recursive _ -> Debug.print "IR typechecker encountered recursive type"; true
      | _ ->  false
    and eq_type_args  (context, l, r)  =
      match l,r with
      | `Type lt, `Type rt -> eqt (context, lt, rt)
      | `Row lr, `Row rr -> eq_rows (context, lr, rr)
      | `Presence lf, `Presence rf -> eq_presence (context, lf, rf)
      | _, _ -> false
    in
      eqt  (context, t1, t2)


let check_eq_types (ctx : type_eq_context) et at occurrence =
  if not (eq_types occurrence ctx (et, at)) then
    raise_ir_type_error
      ("Type mismatch:\n Expected:\n  " ^ Types.string_of_datatype et ^ "\n Actual:\n  " ^ Types.string_of_datatype at)
      occurrence

let check_eq_type_lists = fun (ctx : type_eq_context) exptl actl occurrence ->
    if List.length exptl <> List.length actl then
      raise_ir_type_error "Arity mismatch" occurrence
    else
      List.iter2 (fun  et at ->
          check_eq_types ctx et at occurrence
       )  exptl actl


let ensure_effect_present_in_row ctx allowed_effects required_effect_name required_effect_type occurrence =
  let (map, _, _) = fst (Types.unwrap_row allowed_effects) in
  match StringMap.find_opt required_effect_name map with
    | Some (`Present et) -> check_eq_types ctx et required_effect_type occurrence
    | _ -> raise_ir_type_error ("Required effect " ^ required_effect_name ^ " not present in effect row " ^ Types.string_of_row allowed_effects) occurrence



let ensure_effect_rows_compatible ctx allowed_effects imposed_effects_row occurrence =
  ensure
    (eq_types occurrence ctx (`Record allowed_effects, `Record imposed_effects_row))
    ("Incompatible effects; Allowed:\n" ^ (Types.string_of_row allowed_effects) ^ "\nactual effects:\n" ^  (Types.string_of_row imposed_effects_row))
    occurrence



(* TYPE CHECKING *)



module Typecheck =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.t

  let info_type (t, _, _) = t


  let checker tyenv =
  object (o)
    inherit IrTraversals.Transform.visitor(tyenv) as super

    (*val env = env*)
    val closure_def_env = Env.empty
    val type_var_env = Env.empty

    (* initialize to the default row of allowed toplevel effects*)
    val allowed_effects = Lib.typing_env.effect_row

    (* TODO: closure handling needs to be reworked properly *)
    method lookup_closure_def_for_fun fid = Env.find closure_def_env fid

    (* Creates a context for type equality checking *)
    method extract_type_equality_context () = { typevar_subst = IntMap.empty; tyenv = type_var_env }

    method set_allowed_effects eff_row = {< allowed_effects = eff_row >}, allowed_effects

    method impose_presence_of_effect effect_name effect_typ occurrence : unit =
      ensure_effect_present_in_row (o#extract_type_equality_context ()) allowed_effects effect_name effect_typ occurrence

    method add_typevar_to_context id kind = {< type_var_env = Env.bind type_var_env (id, kind)  >}
    method remove_typevar_to_context id  = {< type_var_env = Env.unbind type_var_env id >}
    method get_type_var_env = type_var_env

    method add_function_closure_binder f binder = {< closure_def_env = Env.bind closure_def_env (f, binder) >}
    method remove_function_closure_binder f = {< closure_def_env = Env.unbind closure_def_env f  >}

    method check_eq_types t1 t2 occurrence = check_eq_types (o#extract_type_equality_context ()) t1 t2 occurrence

    method! var : var -> (var * datatype * 'self_type) =
      fun var -> (var, o#lookup_type var, o)



    method! value : value -> (value * datatype * 'self_type) =
      let value_inner orig = match orig with
        | Ir.Constant c -> let (c, t, o) = o#constant c in Ir.Constant c, t, o
        | Variable x -> let (x, t, o) = o#var x in Variable x, t, o
        | Extend (fields, base) as orig ->
            let (fields, field_types, o) = o#name_map (fun o -> o#value) fields in
            let (base, base_type, o) = o#option (fun o -> o#value) base in

            let handle_extended_record = function
              | Some t -> `Record t
              | None -> raise_ir_type_error "Record already contains one of the extension fields" (SVal orig) in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match t with
                        | `Record row ->
                            handle_extended_record (extend_row_safe field_types row)
                        | _ -> raise_ir_type_error "Trying to extend non-record type" (SVal orig)
                    end
            in
              Extend (fields, base), t, o
        | Project (name, v) ->
            let (v, vt, o) = o#value v in
            Project (name, v), project_type ~overstep_quantifiers:false name vt, o

        | Erase (names, v) ->
            let (v, vt, o) = o#value v in
            let t = erase_type ~overstep_quantifiers:false names vt in
              Erase (names, v), t, o

        | Inject (name, v, t) ->
            let v, vt, o = o#value v in
            let _ = match t with
              | `Variant _ ->
                 o#check_eq_types  (variant_at ~overstep_quantifiers:false name t) vt (SVal orig)
              | _ -> raise_ir_type_error "trying to inject into non-variant type" (SVal orig) in
            Inject (name, v, t), t, o

        | TAbs (tyvars, v) ->
            let o = List.fold_left
              (fun o quant ->
                let var = var_of_quantifier quant in
                let kind = kind_of_quantifier quant in
                o#add_typevar_to_context var kind) o tyvars in
            let v, t, o = o#value v in
            let o = List.fold_left
              (fun o quant ->
                let var = var_of_quantifier quant in
                o#remove_typevar_to_context var) o tyvars in
            let t = Types.for_all (tyvars, t) in
              TAbs (tyvars, v), t, o

        | TApp (v, ts)  ->
            let v, t, o = o#value v in
            let t = Instantiate.apply_type t ts in
            TApp (v, ts), t, o
        | XmlNode (tag, attributes, children) ->
            let (attributes, attribute_types, o) = o#name_map (fun o -> o#value) attributes in
            let (children  , children_types, o) = o#list (fun o -> o#value) children in

            let _ = StringMap.iter (fun _ t -> o#check_eq_types  (`Primitive Primitive.String) t (SVal orig)) attribute_types in
            let _ = List.iter (fun t -> o#check_eq_types  Types.xml_type t (SVal orig)) children_types in
              XmlNode (tag, attributes, children), Types.xml_type, o

        | ApplyPure (f, args) ->
            let rec is_pure_function = function
              | TApp (v, _)
              | TAbs (_, v) -> is_pure_function v
              | Variable var when Lib.is_primitive_var var -> Lib.is_pure_primitive (Lib.primitive_name var)
              | _ -> false in

            let (f, ft, o) = o#value f in
            let (args, argument_types, o) = o#list (fun o -> o#value) args in

            let parameter_types = arg_types ~overstep_quantifiers:false ft in
            check_eq_type_lists (o#extract_type_equality_context ()) parameter_types argument_types (SVal orig);
            ensure (is_pure_function f) "ApplyPure used for non-pure function" (SVal orig);
            ApplyPure (f, args),  return_type ~overstep_quantifiers:false ft, o


        | Closure (f, tyargs, z) ->
          (* We must not use o#var here, because by design that function fails if it sees an identifier denoting a function needing a closure *)
          let ft = o#lookup_type f in
          let (z, zt, o) = o#value z in

          let ft_instantiated, instantiation_maps = if tyargs = []
            then
              (ft, (IntMap.empty, IntMap.empty, IntMap.empty))
            else
              let (remaining_type, instantiation_maps) = Instantiate.instantiation_maps_of_type_arguments false ft tyargs in
              Instantiate.datatype instantiation_maps remaining_type, instantiation_maps  in

          ensure (is_function_type (snd (TypeUtils.split_quantified_type ft_instantiated))) "Passing closure to non-function" (SVal orig);
          begin match o#lookup_closure_def_for_fun f with
          | Some optbinder ->
            begin match optbinder with
              | inner_quantifiers, Some  binder ->
                let outer_quantifiers = TypeUtils.quantifiers ft in

                let outer_to_inner_type_var_map  =
                  List.fold_left2 (fun map iq oq  ->
                      let iv = Types.var_of_quantifier iq in
                      let ov = Types.var_of_quantifier oq in
                      IntMap.add ov iv map
                    )  IntMap.empty inner_quantifiers outer_quantifiers  in

                (* Right now, the instantiation_maps map outer type variables to their substitutions. However,
                  the types in the closure record are expressed in terms of the function's inner type variables *)
                let tranform_outer_instantiation_map_to_inner map =
                  IntMap.fold (fun outer_var oldvalue resultmap ->
                      let inner_var = IntMap.find outer_var outer_to_inner_type_var_map in
                      IntMap.add inner_var oldvalue resultmap
                    ) map IntMap.empty in

                let inner_typemap = tranform_outer_instantiation_map_to_inner (fst3 instantiation_maps) in
                let inner_rowmap = tranform_outer_instantiation_map_to_inner (snd3 instantiation_maps) in
                let inner_presencemap = tranform_outer_instantiation_map_to_inner (thd3 instantiation_maps) in
                let inner_instantiation_maps = (inner_typemap, inner_rowmap, inner_presencemap) in

                let uninstantiated_type_of_environment = (Var.type_of_binder binder) in
                Debug.print (IntMap.show Types.pp_datatype (fst3 inner_instantiation_maps));
                let type_of_environment = Instantiate.datatype inner_instantiation_maps uninstantiated_type_of_environment in
                o#check_eq_types type_of_environment zt (SVal orig)
              | _, None -> raise_ir_type_error "Providing closure to a function that does not need one" (SVal orig)
            end
          | None -> raise_ir_type_error "Providing closure to untracked function" (SVal orig)
          end;
          Closure (f, tyargs, z), ft_instantiated, o

        | Coerce (v, t) ->
            let v, vt, o = o#value v in
            if RecursionDetector.is_recursive vt || RecursionDetector.is_recursive t then
              begin
              (* TODO: We may want to implement simple subtyping for recursive types *)
              Debug.print "IR Typechecker gave up on coercion involving recursive types";
              Coerce (v, t), t, o
              end
            else
              begin
              ensure (eq_types (SVal orig) (o#extract_type_equality_context ()) (vt, t) || is_sub_type (vt, t)) (Printf.sprintf "coercion error: %s is not a subtype of %s"
                                         (string_of_datatype vt) (string_of_datatype t)) (SVal orig);
              Coerce (v, t), t, o
              end
      in
      fun value -> translate_helper_exns value_inner value (SVal value)


    method! tail_computation :
      tail_computation -> (tail_computation * datatype * 'self_type) =
      let tail_computation_inner orig = match orig with
        | Return v ->
            let v, t, o = o#value v in
              Return v, t, o

        | Apply (f, args) ->
            let f, ft, o = o#value f in
            let args, argtypes, o = o#list (fun o -> o#value) args in
            let exp_argstype = arg_types ~overstep_quantifiers:false ft in
            let effects = effect_row ~overstep_quantifiers:false ft in
            ensure_effect_rows_compatible (o#extract_type_equality_context ()) allowed_effects effects (STC orig);
            check_eq_type_lists (o#extract_type_equality_context ()) exp_argstype argtypes (STC orig);
            Apply (f, args), return_type ~overstep_quantifiers:false ft, o

        | Special special ->
            let special, t, o = o#special special in
              Special special, t, o

        | Ir.Case (v, cases, default) ->
            let v, vt, o = o#value v in
            begin match vt with
            | `Variant row as variant ->
               let unwrapped_row = fst (unwrap_row row) in
               let present_fields, has_presence_polymorphism  =
                 StringMap.fold (fun field field_spec (fields, poly) -> match field_spec with
                                           | `Present _  -> (StringSet.add field fields), poly
                                           | `Var _ -> fields, true
                                           | `Absent -> fields, poly)
                   (fst3 unwrapped_row) (StringSet.empty, false) in
               let is_closed = is_closed_row row in
               let has_default = OptionUtils.is_some default in
               let case_fields = StringMap.fold (fun field _ fields -> StringSet.add field fields) cases StringSet.empty in

               if has_default then
                 ensure (StringSet.subset case_fields present_fields) "superfluous case" (STC orig)
               else
                 begin
                   ensure (not (StringSet.is_empty present_fields)) "Case with neither cases nor default" (STC orig);
                   ensure (is_closed) "case without default over open row"  (STC orig);
                   ensure (not has_presence_polymorphism)
                     "case without default over variant with presence polymorphism in some field"  (STC orig);
                   ensure (StringSet.equal case_fields present_fields)
                     "cases not identical to present fields in closed row, no default case" (STC orig)
                 end;

               let cases, types, o =
                 StringMap.fold
                   (fun name  (binder, comp) (cases, types, o) ->
                     let type_binder = Var.type_of_binder binder in
                     let type_variant = variant_at ~overstep_quantifiers:false name variant in
                     o#check_eq_types type_binder type_variant (STC orig);
                     let b, o = o#binder binder in
                     let c, t, o = o#computation comp in
                     let o = o#remove_binder binder in
                     StringMap.add name (b,c) cases, t :: types, o)
                   cases (StringMap.empty, [], o) in
               let default, default_type, o =
                 o#option (fun o (b, c) ->
                     let b, o = o#binder b in
                     let c, t, o = o#computation c in
                     let o = o#remove_binder b in
                     (b, c), t, o) default in
               let types = OptionUtils.opt_app (fun dt -> dt :: types) types default_type in
               let t = List.hd types in
               List.iter (fun ty -> o#check_eq_types t ty (STC orig)) (List.tl types);
               Ir.Case (v, cases, default), t, o
            | _ ->  raise_ir_type_error "Case over non-variant value" (STC orig)
            end
        | If (v, left, right) ->
            let v, vt, o = o#value v in
            let left, lt, o = o#computation left in
            let right, rt, o = o#computation right in
            o#check_eq_types vt (`Primitive Primitive.Bool) (STC orig);
            o#check_eq_types lt rt (STC orig);
            If (v, left, right), lt, o

      in
      fun tc -> translate_helper_exns tail_computation_inner tc (STC tc)




    method! special : special -> (special * datatype * 'self_type) =
      let special_inner special = match special with
        | Wrong t -> Wrong t, t, o
        | Database v ->
            let v, vt, o = o#value v in
            (* v must be a record containing string fields  name, args, and driver*)
            List.iter (fun field ->
                o#check_eq_types (project_type field vt) Types.string_type (SSpec special)
              ) ["name"; "args"; "driver"];
            Database v, `Primitive Primitive.DB, o

        | Table (db, table_name, keys, tt) ->
            let db, db_type, o = o#value db in
            o#check_eq_types db_type Types.database_type (SSpec special);
            let table_name, table_name_type, o = o#value table_name in
            o#check_eq_types table_name_type Types.string_type (SSpec special);
            let keys, keys_type, o = o#value keys in
            o#check_eq_types keys_type Types.keys_type (SSpec special);
            (* TODO: tt is a tuple of three records. Discussion pending about what kind of checks we should do here
               From an implementation perspective, we should check the consistency of the read, write, needed info here *)
              Table (db, table_name, keys, tt), `Table tt, o

        | Query (range, e, original_t) ->
            let range, o =
              o#optionu
                (fun o (limit, offset) ->
                   (* Query blocks themselves only have the wild effect when they have a range *)
                   o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);

                   let limit, ltype, o = o#value limit in
                   let offset, otype, o = o#value offset in
                      o#check_eq_types ltype Types.int_type (SSpec special);
                      o#check_eq_types otype Types.int_type (SSpec special);
                     (limit, offset), o)
                range in
            (* query body must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let e, t, o = o#computation e in
            let o, _ = o#set_allowed_effects outer_effects in

            (* The type of the body must match the type the query is annotated with *)
            o#check_eq_types original_t t (SSpec special);

            (if Settings.get_value Basicsettings.Shredding.relax_query_type_constraint then
              () (* Discussion pending about how to type-check here. Currently same as frontend *)
            else
              let list_content_type = TypeUtils.element_type ~overstep_quantifiers:false t in
              let row = TypeUtils.extract_row list_content_type in
              ensure (Types.is_base_row row) "Only base types allowed in query result record" (SSpec special));

              Query (range, e, t), t, o

        | InsertRows (source, rows)
	| InsertReturning (source, rows, _) ->
            (* Most logic is shared between InsertRow and InsetReturning.
               We disambiguate between the two later on. *)

            (* The insert itself is wild *)
            o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);
            let source, source_t, o = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let table_write = TypeUtils.table_write_type source_t in
            let table_needed = TypeUtils.table_needed_type source_t in
            let table_needed_r = TypeUtils.extract_row table_needed in


            let rows, rows_list_t, o = o#value rows in
            let rows_t = TypeUtils.element_type ~overstep_quantifiers:false rows_list_t in
            let rows_r = TypeUtils.extract_row rows_t in

            ensure (Types.is_closed_row rows_r) "Inserted record must have closed row" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                 match presence_spec with
                  | `Present actual_type_field ->
                    (* Ensure that the field we update is in the write row and the types match
                       As an invariant of Table types, it should then also be in the read row *)
                    let write_type = TypeUtils.project_type field table_write in
                    o#check_eq_types actual_type_field write_type (SSpec special);
                  | `Absent -> () (* This is a closed row, ignore Absent *)
                  | `Var _  -> raise_ir_type_error "Found presence polymorphism in inserted row" (SSpec special)
              ) rows_r;


            (* The following should be an invariant of Table types? *)
            ensure (Types.is_closed_row table_needed_r) "Needed row of table type must be closed" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                 match presence_spec with
                  | `Present needed_type ->
                    (* Ensure that all fields `Present in the needed row are being inserted *)
                    let inserted_type = TypeUtils.project_type field rows_t in
                    o#check_eq_types inserted_type needed_type (SSpec special)
                  | `Absent
                  | `Var _  -> ()
              ) table_needed_r;

            begin match special with
                | InsertRows (_, _) ->
                   InsertRows(source, rows), Types.unit_type, o
                | InsertReturning (_, _, (Constant (Constant.String id) as ret)) ->
                   (* The return value must be encoded as a string literal,
                      denoting a column *)
                   let ret_type = TypeUtils.project_type id table_read in
                   o#check_eq_types Types.int_type ret_type (SSpec special);
                   InsertReturning (source, rows, ret), Types.int_type, o
                | InsertReturning (_, _, _) ->
                   raise_ir_type_error "Return value in InsertReturning was not a string literal" (SSpec special)
                | _ -> assert false (* impossible at this point *)
            end

        | Update ((x, source), where, body) ->
            o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);
            let source, source_t, o = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let table_write = TypeUtils.table_write_type source_t in
            let x, o = o#binder x in
            o#check_eq_types (Var.type_of_binder x) table_read (SSpec special);
            (* where part must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let where, o = o#optionu (fun o where ->
                  let where, t, o = o#computation where in
                  o#check_eq_types t Types.bool_type (SSpec special);
                  where, o
                )
               where in
            let body, body_t, o = o#computation body in
            let body_record_row = (TypeUtils.extract_row body_t) in
            ensure (Types.is_closed_row body_record_row) "Open row as result of update" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                match presence_spec with
                  | `Present actual_type_field ->
                    (* Ensure that the field we update is in the write row and the types match *)
                    let expected_type_field = TypeUtils.project_type field table_write in
                    o#check_eq_types expected_type_field actual_type_field (SSpec special)
                  | `Absent -> () (* This is a closed row, ignore Absent *)
                  | `Var _  -> raise_ir_type_error "Found presence polymorphism in the result of an update" (SSpec special)
              ) body_record_row;
            let o = o#remove_binder x in
            let o, _ = o#set_allowed_effects outer_effects in
              Update ((x, source), where, body), Types.unit_type, o

        | Delete ((x, source), where) ->
            o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);
            let source, source_t, o = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let x, o = o#binder x in
            o#check_eq_types (Var.type_of_binder x) table_read (SSpec special);
            (* where part must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let where, o = o#optionu (fun o where ->
                  let where, t, o = o#computation where in
                  o#check_eq_types t Types.bool_type (SSpec special);
                  where, o
                )
               where in
            let o = o#remove_binder x in
            let o, _ = o#set_allowed_effects outer_effects in
              Delete ((x, source), where), Types.unit_type, o

        | CallCC v ->
            let v, t, o = o#value v in
            (* TODO: What is the correct argument type for v, since it expects a continuation? *)
              CallCC v, return_type ~overstep_quantifiers:false t, o
        | Select (l, v) -> (* TODO perform checks specific to this constructor *)
           o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);
           let v, t, o = o#value v in
           Select (l, v), t, o
        | Choice (v, bs) -> (* TODO perform checks specific to this constructor *)
           o#impose_presence_of_effect "wild" Types.unit_type (SSpec special);
           let v, _, o = o#value v in
           let bs, branch_types, o =
             o#name_map (fun o (b, c) ->
                         let b, o = o#binder b in
                         let c, t, o = o#computation c in
                         (b, c), t, o) bs in
           let t = (StringMap.to_alist ->- List.hd ->- snd) branch_types in
           Choice (v, bs), t, o
        | Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) ->
          (* outer effects is R_d in the IR formalization *)
          let outer_effects = Types.flatten_row allowed_effects in

          (* return_t is A_d in the IR formalization *)
          let (return, return_t, return_binder_type, o) =
            let return_binder, return_computation = ih_return in
             (* return_binder_type is A_c in the IR formalization *)
            let return_binder_type = Var.type_of_binder return_binder in
            let (b, o) = o#binder return_binder in
            let (comp, t, o) = o#computation return_computation in
            let o = o#remove_binder return_binder in
          (b, comp), t, return_binder_type, o in


          (* The effects and return type of all resumptions must be the same.
             instead of collecting them while processing the handler branches, we save the once we encounter first here.
             The subsequent cases are then compared against what's stored here *)
          let resumption_effects : Types.row option ref = ref None in
          let resumption_return_type : Types.datatype option ref = ref None in

          let (cases, branch_presence_spec_types, o) =
            o#name_map
              (fun o (x, resume, c) ->
                  let (x, o) = o#binder x in
                  let x_type = Var.type_of_binder x in
                  let (resume, o) = o#binder resume in
                  let resume_type = Var.type_of_binder resume in
                  let (cur_resume_args, cur_resume_effects, cur_resume_ret) = match TypeUtils.concrete_type resume_type with
                    | `Function (a, b, c) -> a, b, c
                    | _ -> raise_ir_type_error "Resumptions has non-function type" (SSpec special) in

                  let presence_spec_funtype = `Function (x_type, Types.make_empty_closed_row (), cur_resume_args) in

                  (match !resumption_effects, !resumption_return_type with
                    | (None, None) ->
                      resumption_effects := Some cur_resume_effects;
                      resumption_return_type := Some  cur_resume_ret
                    | (Some existing_resumption_effects, Some existing_resumption_rettype) ->
                      o#check_eq_types (`Effect existing_resumption_effects) (`Effect cur_resume_effects) (SSpec special);
                      o#check_eq_types existing_resumption_rettype cur_resume_ret (SSpec special)
                    | _ -> assert false);


                  (* ct is A_d in the IR formalization *)
                  let (c, ct, o) = o#computation c in
                  o#check_eq_types return_t ct (SSpec special);
                  let o = o#remove_binder x in
                  let o = o#remove_binder resume in
                  (x, resume, c), presence_spec_funtype, o)
              ih_cases in


          (* We now construct the inner effects from the outer effects and branch_presence_spec_types *)
          let (outer_effects_map, outer_effects_var, outer_effects_dualized) = outer_effects in
          (* For each case branch, the corresponding entry goes directly into the field spec map of the inner effect row *)
          let inner_effects_map_from_branches = StringMap.map (fun x -> `Present x) branch_presence_spec_types in
          (* We now add all entries from the outer effects that were not touched by the handler to the inner effects *)
          let inner_effects_map = StringMap.fold (fun effect outer_presence_spec map ->
              if StringMap.mem effect inner_effects_map_from_branches then
                map
              else
                StringMap.add effect outer_presence_spec map
            )  inner_effects_map_from_branches outer_effects_map in
          let inner_effects = (inner_effects_map, outer_effects_var, outer_effects_dualized) in

        (if not (Types.is_closed_row outer_effects) then
          let outer_effects_contain e = StringMap.mem e outer_effects_map in
          ensure (StringMap.for_all (fun e _ -> outer_effects_contain e) cases) "Outer effects are open but do not mention an effect handled by handler" (SSpec special));

          (* comp_t  is A_c in the IR formalization *)
          let o, _ = o#set_allowed_effects inner_effects in
          let (comp, comp_t, o) = o#computation ih_comp in
          let (depth, o) =
            match ih_depth with
            | Deep params ->
                (* TODO: Find out what these "params" are for *)
                let (o, bindings) =
                  List.fold_left
                    (fun (o, bvs) (b,v) ->
                      let (b, o) = o#binder b in
                      let (v, _, o) = o#value v in
                      let o = o#remove_binder b in
                      (o, (b,v) :: bvs))
                    (o, []) params
                in
                Deep (List.rev bindings), o
            | Shallow -> Shallow, o in
          let o, _ = o#set_allowed_effects outer_effects in

          o#check_eq_types return_binder_type comp_t (SSpec special);

        (match !resumption_effects, !resumption_return_type, depth with
          | Some re, Some rrt, (Deep _) ->
            o#check_eq_types (`Effect re) (`Effect outer_effects) (SSpec special);
            o#check_eq_types return_t rrt (SSpec special)
          | Some re, Some rrt, Shallow ->
            o#check_eq_types (`Effect re) (`Effect inner_effects) (SSpec special);
            o#check_eq_types comp_t rrt (SSpec special)
          | _ -> ());


          Handle { ih_comp = comp; ih_cases = cases; ih_return = return; ih_depth = depth}, return_t, o

        | DoOperation (name, vs, t) -> (* TODO perform checks specific to this constructor *)
          let (vs, vs_t, o) = o#list (fun o -> o#value) vs in
          let arg_type_actual =  make_tuple_type vs_t in

          (* Checks that "name" is Present in the current effect row *)
          let effect_type = fst (TypeUtils.split_row name allowed_effects) in
           (* contrary to normal functions, the argument type is not tuple-ified if there is only a single argument.
              Therefore, can't use return_type and arg_types from TypeUtils here, because these have those assumptions hard-coded *)
          let (arg_type_expected, effects, ret_type_expected) = match TypeUtils.concrete_type effect_type with
            | `Function (at, et, rt) -> (at, et, rt)
            | _ -> raise_ir_type_error "Non-function type associated with effect" (SSpec special) in

          ensure (Types.is_empty_row effects) "Effect case's function type has non-empty effect row" (SSpec special);
          o#check_eq_types arg_type_expected arg_type_actual (SSpec special);
          o#check_eq_types ret_type_expected t (SSpec special);

          (DoOperation (name, vs, t), t, o)

        | Lens _
        | LensDrop _
        | LensSelect _
        | LensGet _
        | LensJoin _
        | LensPut _ -> (* just do type reconstruction *) super#special special
      in
      fun special -> translate_helper_exns special_inner special (SSpec special)


   method! bindings : binding list -> (binding list * 'self_type) =
      fun bs ->
        let bs, o =
          List.fold_left
            (fun (bs, o) b ->
               let (b, o) = o#binding b in
                 (b::bs, o))
            ([], o)
            bs
        in
          List.rev bs, o

    method! computation : computation -> (computation * datatype * 'self_type) =
      fun (bs, tc) ->
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in
        let o = o#remove_bindings bs in
          (bs, tc), t, o


   (* The function parameters (and potentially other recursive functions), as well as the closure binder
      (if it exists), must be added to the environment before calling *)
    method handle_funbinding
             (expected_overall_funtype : datatype)
             (tyvars : Types.quantifier list)
             (parameter_types : datatype list)
             (body : computation)
             (is_recursive : bool)
             (occurrence : ir_snippet)
           : (computation * 'self_type) =

        (* Get all toplevel quantifiers, even if nested as in
           forall a. forall b. t1 -> t2. Additionally, get type with quantifiers stripped away *)
        let _, exp_unquant_t =
          TypeUtils.split_quantified_type expected_overall_funtype in

        ensure
          (TypeUtils.is_function_type exp_unquant_t)
          "Function annotated with non-function type"
          occurrence;

        (* In order to obtain the effects to type-check the body with,
           we take the expected effects and substitute the quantifiers from the type
           annotation with those in the tyvar list *)
        let quantifiers_as_type_args = List.map Types.type_arg_of_quantifier tyvars in
        let exp_t_substed = Instantiate.apply_type expected_overall_funtype quantifiers_as_type_args in
        let exp_effects_substed = TypeUtils.effect_row exp_t_substed in

        (if is_recursive then o#impose_presence_of_effect "wild" Types.unit_type occurrence);
        let o = List.fold_left
              (fun o quant ->
                let var = var_of_quantifier quant in
                let kind = kind_of_quantifier quant in
                o#add_typevar_to_context var kind) o tyvars in

        (* determine body type, using translated version of expected effects in context *)
        let o, previously_allowed_effects = o#set_allowed_effects exp_effects_substed in
        let body, body_type, o = o#computation body in

        let o = List.fold_left
              (fun o quant ->
                let var = var_of_quantifier quant in
                o#remove_typevar_to_context var) o tyvars in
        let o, _ = o#set_allowed_effects previously_allowed_effects in


        let is_linear = not (Types.is_unl_type exp_unquant_t) in
        let actual_ft_unquant =
          Types.make_function_type
            ~linear:is_linear
            parameter_types
            exp_effects_substed
            body_type in
        let actual_overall_funtype = Types.for_all (tyvars, actual_ft_unquant) in

        (* Compare the overall types. Note that the effects are actually unncessary here:
           We already checked them when checking the body. However, removing them
           from the overall types would be confusing *)
        o#check_eq_types expected_overall_funtype actual_overall_funtype occurrence;

        body, o

    method! binding : binding -> (binding * 'self_type) =
      let binding_inner = function
        | (Let (x, (tyvars, tc))) as orig ->
            let lazy_check =
            lazy (
              let o = List.fold_left
                (fun o quant ->
                  let var = var_of_quantifier quant in
                  let kind = kind_of_quantifier quant in
                  o#add_typevar_to_context var kind) o tyvars in
              let tc, act, o = o#tail_computation tc in
              let o = List.fold_left
                (fun o quant ->
                  let var = var_of_quantifier quant in
                  o#remove_typevar_to_context var) o tyvars in
              let exp = Var.type_of_binder x in
              let act_foralled = Types.for_all (tyvars, act) in
              o#check_eq_types exp act_foralled (SBind orig);
              tc, o
            ) in
            let (tc, o) = handle_ir_type_error lazy_check (tc, o) (SBind orig) in
            let x, o = o#binder x in
            Let (x, (tyvars, tc)), o

        | Fun (f, (tyvars, xs, body), z, location) as binding ->
           (* It is important that the type annotations of the parameters are
              expressed in terms of the type variables from tyvars (also for rec
              functions) *)
              let lazy_check =
              lazy(
                let (z, o) = o#optionu (fun o -> o#binder) z in
                let (xs, o) =
                  List.fold_right
                    (fun x (xs, o) ->
                      let x, o = o#binder x in
                        (x::xs, o))
                    xs
                    ([], o) in

                let whole_function_expected = Var.type_of_binder f in
                let actual_parameter_types = (List.map Var.type_of_binder xs) in
                let body, o = o#handle_funbinding
                                whole_function_expected
                                tyvars
                                actual_parameter_types
                                body
                                false
                                (SBind binding) in
                let o = o#add_function_closure_binder (Var.var_of_binder f) (tyvars, z) in
                (* Debug.print ("added " ^ string_of_int (Var.var_of_binder f) ^ " to closure env"); *)

                let o = OptionUtils.opt_app o#remove_binder o z in
                let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                f, tyvars, xs, body, z, location, o
              ) in
              let f, tyvars, xs, body, z, location, o =
               handle_ir_type_error lazy_check (f, tyvars, xs, body, z, location, o) (SBind binding) in
              let f, o = o#binder f in
              let o = o#add_function_closure_binder (Var.var_of_binder f) (tyvars, z) in
              Fun (f, (tyvars, xs, body), z, location), o

        | Rec defs  as binding ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let defs, o =
              List.fold_right
                (fun (f, (tyvars, xs, body), z, location) (fs, o) ->
                   let o = o#add_function_closure_binder (Var.var_of_binder f) (tyvars, z) in
                   let f, o = o#binder f in
                     ((f, (tyvars, xs, body), z, location)::fs, o))
                defs
                ([], o) in

            let lazy_check =
            lazy (
              let defs, o =
              List.fold_left
                (fun (defs, (o : 'self_type)) ((f, (tyvars, xs, body), z, location)) ->
                   let (z, o) = o#optionu (fun o -> o#binder) z in
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in

                  let whole_function_expected = Var.type_of_binder f in
                  let actual_parameter_types = (List.map Var.type_of_binder xs) in
                  let body, o = o#handle_funbinding
                                whole_function_expected
                                tyvars
                                actual_parameter_types
                                body
                                true
                                (SBind binding) in
                  let o = o#add_function_closure_binder (Var.var_of_binder f) (tyvars, z) in
                  (* Debug.print ("added " ^ string_of_int (Var.var_of_binder f) ^ " to closure env"); *)

                  let o = OptionUtils.opt_app o#remove_binder o z in
                  let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                    (f, (tyvars, xs, body), z, location)::defs, o)
                ([], o)
                defs in
              let defs = List.rev defs in
              defs, o
            ) in
            let defs, o = handle_ir_type_error lazy_check (defs, o) (SBind binding) in
            Rec defs, o


        | Alien (x, name, language) ->
            let x, o = o#binder x in
              Alien (x, name, language), o

        | Module (name, defs) ->
            let defs, o =
              match defs with
                | None -> None, o
                | Some defs ->
                    let defs, o = o#bindings defs
                    in
                      Some defs, o
            in
              Module (name, defs), o
      in
      fun b -> translate_helper_exns binding_inner b (SBind b)


    method! binder : binder -> (binder * 'self_type) =
      fun (var, info) ->
        let tyenv = Env.bind tyenv (var, info_type info) in
          (var, info), {< tyenv=tyenv >}

    method remove_binder : binder -> 'self_type = fun binder ->
      let tyenv = Env.unbind tyenv (Var.var_of_binder binder) in
      {< tyenv=tyenv >}

    method remove_binding : binding -> 'self_type = function
      | Let (x, _) -> o#remove_binder x
      | Fun  fundef ->
        let f = Var.var_of_binder (Ir.binder_of_fun_def fundef) in
        let o = o#remove_function_closure_binder f in
        o#remove_binder (Ir.binder_of_fun_def fundef)
      | Rec fundefs ->
        List.fold_left
                (fun o fundef  ->
                  let binder = Ir.binder_of_fun_def fundef in
                  let f = Var.var_of_binder binder in
                  let o = o#remove_binder binder in
                  o#remove_function_closure_binder f
                   )
                o
                fundefs
      | Alien (binder, _, _) -> o#remove_binder binder
      | Module _ -> o

    method remove_bindings : binding list -> 'self_type =
      List.fold_left (fun o b -> o#remove_binding b) o


    method! program : program -> (program * datatype * 'self_type) = o#computation

    method! get_type_environment : environment = tyenv
  end

  let program tyenv p =
    let lazy_check =
      lazy (let p, _, _ = (checker tyenv)#computation p in p) in
   handle_ir_type_error lazy_check p (SProg p)

  let bindings tyenv b =
    let lazy_check =
      lazy (let b, _ = (checker tyenv)#bindings b in b) in
    handle_ir_type_error lazy_check b (SBinds b)
end
