open Utility
open Ir

type subst_type = Var.var IntMap.t
type effects_type = Types.datatype StringMap.t
let print_substmap subst =
  Debug.print ("Substmap\n:" ^ IntMap.show (fun fmt num -> Format.pp_print_int fmt num) subst)

let eq_types_with_subst : subst_type -> (Types.datatype * Types.datatype) -> bool =
  fun  subst (t1, t2) ->
    let lookupVar lvar map  =
      match IntMap.find_opt lvar map with
      | Some rvar' -> (map, rvar')
      | None -> (map, lvar)  (* (IntMap.add lvar rvar map, rvar)*) in
    let handle_variable (lid, lsk, lfd)  (rid, rsk, rfd) map =
      let (map, rvar') = lookupVar lid map  in
      let is_equal = rid = rvar' && lsk = rsk && lfd = rfd in
      (if not is_equal then print_substmap map);
      (map, is_equal) in

    (* sbust maps rigid typed/row/presence variables of t1 to corresponding ones of t2 *)
    let rec eqt ((subst, t1, t2) : (subst_type* Types.datatype * Types.datatype)) =

      (* TODO: This also unwraps one level of `Recursive, without changing its body *)
      let t1 = Types.concrete_type t1 in
      let t2 = Types.concrete_type t2 in

      Debug.print ("Checking type equality\n " ^
                     (Types.string_of_datatype t1) ^ "\n vs \n" ^ (Types.string_of_datatype t2) (*^ IntMap.show Var.pp_var subst*));
      match t1 with
      | `Not_typed -> (* checked again *)
          begin match t2 with
              `Not_typed -> (subst,  true)
            | _          -> (subst, false)
          end
      | `Primitive x ->  (* checked again *)
          begin match t2 with
              `Primitive y -> (subst, x = y)
            | _            -> (subst, false)
          end
      | `MetaTypeVar lpoint ->  (* checked again *)
          begin match t2 with
            `MetaTypeVar rpoint ->
             begin match Unionfind.find lpoint, Unionfind.find rpoint with
             | `Var lv, `Var rv ->  handle_variable lv rv subst
             | `Recursive _, `Recursive _  -> failwith "FIXME" (* FIXME support recursion*)
             | `Body _, `Body _ -> failwith "Should have  removed `Body by now"
             | _ -> (subst, false)
             end
            | _                   -> (subst, false)
          end
      | `Function (lfrom, lm, lto) ->  (* checked again *)
          begin match t2 with
            `Function (rfrom, rm, rto) ->
             let (subst, r1) = eqt (subst, lfrom, rfrom) in
             let (subst, r2) =  eqt (subst, lto,   rto) in
             let (subst, r3) = eq_rows  (subst, lm, rm) in
             (subst, r1 && r2 && r3)
            | _                          -> (subst, false)
          end
      | `Lolli (lfrom, lm, lto) ->  (* checked again *)
          begin match t2 with
            `Function (rfrom, rm, rto) ->
             let (subst, r1) = eqt (subst, lfrom, rfrom) in
             let (subst, r2) = eqt (subst, lto,   rto) in
             let (subst, r3) = eq_rows  (subst, lm, rm) in
             (subst, r1 && r2 && r3)
            | _                          -> (subst, false)
          end
      | `Record l ->  (* checked again *)
         begin match t2 with
         | `Record r -> eq_rows (subst, l, r)
         | _         -> (subst, false)
         end
      | `Variant l ->  (* checked again *)
         begin match  t2 with
           `Variant r -> eq_rows (subst, l, r)
         | _          -> (subst, false)
         end
      | `Effect l ->  (* checked again *)
         begin match t2 with
         | `Effect r -> eq_rows (subst, l, r)
         | _         -> (subst, false)
         end
      | `Application (s, ts) ->  (* checked again *)
         begin match t2 with
         | `Application (s', ts') ->
            List.fold_left2 (fun (subst, prev_equal) larg rarg  ->
                let subst, eq = eq_type_args (subst, larg, rarg) in
                subst, prev_equal && eq)
              (subst, Types.Abstype.equal s  s') ts ts'
         | _ -> (subst, false)
         end
      | `ForAll (qs, t) -> (* checked again *)
         begin match t2 with
         | `ForAll (qs', t') ->
            let (subst', quantifiers_match) = List.fold_left2 (fun (subst, prev_eq) lqvar rqvar ->
                                                  let lid, lsk, lfd = lqvar in
                                                  let rid, rsk, rfd = rqvar in
                                                  (IntMap.add lid rid subst, prev_eq && lsk = rsk && lfd = rfd)
                                                ) (subst,true) (Types.unbox_quantifiers qs) (Types.unbox_quantifiers qs') in
            if quantifiers_match then
              (subst, snd (eqt (subst', t, t'))) (* TODO if we are to unify flexible variables, those should survive here *)
            else (subst, false)
         | _ -> (subst, false)
         end
      | #Types.session_type as l ->
         begin match  t2 with
         | #Types.session_type as r -> eq_sessions (subst, l, r)
         | _          -> (subst, false)
         end

      | `Alias  _ -> failwith "should have removed `Alias by now"
      | `Table (lt1, lt2, lt3)  -> (* checked again *)
         begin match t2 with
         | `Table (rt1, rt2, rt3) ->
            let (subst, r1) = eqt (subst, lt1, rt1) in
            let (subst, r2) = eqt (subst, lt2, rt2) in
            let (subst, r3) = eqt (subst, lt3, rt3) in
            (subst, r1 && r2 && r3)
         | _ -> (subst, false)
         end
    and eq_sessions (subst, l, r)  = (* checked again *)
      match (l,r) with
      | `Input (lt, _), `Input (rt, _)
        | `Output (lt, _), `Output (rt, _) ->
         eqt (subst, lt, rt)
      | `Select l, `Select r
        | `Choice l, `Choice r ->
         eq_rows (subst, l, r)
      | `Dual l, `Dual r ->
         eqt (subst, l, r)
      | `End, `End -> (subst, true)
      | _, _ -> (subst, false)
    and eq_rows  (subst, (lfield_env, lrow_var, ldual), (rfield_env, rrow_var, rdual))  = (* checked again *)
      let  (subst, r1) = eq_field_envs (subst, lfield_env, rfield_env) in
      let  (subst, r2) = eq_row_vars (subst, lrow_var, rrow_var) in
        (subst, r1 && r2 && ldual=rdual)
    and eq_presence (subst, l, r) = (* checked again *)
      match l, r with
      | `Absent, `Absent ->  (subst, true)
      | `Present lt, `Present rt -> eqt (subst, lt, rt)
      | `Var lpoint, `Var rpoint -> begin match Unionfind.find lpoint, Unionfind.find rpoint with
                                    | `Body _,  _
                                      | _, `Body _ -> failwith "should have removed all `Body variants by now"
                                    |  `Var lv, `Var rv -> handle_variable lv rv subst
                                    | _ , _ -> (subst, false)
                                    end
      | _, _ -> assert false
    and eq_field_envs  (subst, lfield_env, rfield_env) = (* checked again *)
      StringMap.fold (fun field lp (subst, prev_eq)  ->
                           match StringMap.find_opt field rfield_env with
                           | Some rp -> let (subst, eq) =
                                          eq_presence (subst, lp, rp) in
                                            (subst, eq && prev_eq)
                           | None -> (subst, false)
                          ) lfield_env  (subst, StringMap.cardinal lfield_env = StringMap.cardinal rfield_env)
    and eq_row_vars (subst, lpoint, rpoint) = (* checked again *)
      match Unionfind.find lpoint, Unionfind.find rpoint with
      | `Closed, `Closed ->  (subst, true)
      | `Var lv, `Var rv ->   handle_variable lv rv subst
      | `Recursive (_var, _), `Recursive (_var', _) -> assert false (* FIXME *)
      | _ ->  (subst, false)
    and eq_type_args  (subst, l, r)  =
      match l,r with
      | `Type lt, `Type rt -> eqt (subst, lt, rt)
      | `Row lr, `Row rr -> eq_rows (subst, lr, rr)
      | `Presence lf, `Presence rf -> eq_presence (subst, lf, rf)
      | _, _ -> (subst, false)
    in
      snd (eqt  (subst, t1, t2))

let eq_types = eq_types_with_subst IntMap.empty

let fail_on_ir_type_error = false
exception IRTypeError of string

let raise_ir_type_error msg _occurence =
  raise (IRTypeError msg)

let handle_ir_type_error error alternative =
  match error with
    | IRTypeError msg
    | TypeUtils.TypeDestructionError msg ->
      if fail_on_ir_type_error then
        failwith msg
      else
        Debug.print ("Continuing after IR Type Error:\n" ^ msg ); alternative
    |  e-> failwith ("Unknown exception" ^ Printexc.to_string e)





let check_eq_types :  Types.datatype -> Types.datatype -> unit = fun et at ->
  if not (eq_types (et, at)) then
    raise_ir_type_error
      ("Type mismatch:\n Expected:\n" ^ Types.string_of_datatype et ^ "\n Actual:\n " ^ Types.string_of_datatype at)
      `None

let check_eq_types_inital_substmap : subst_type -> Types.datatype -> Types.datatype -> unit = fun subst et at ->
  if not (eq_types_with_subst subst (et, at)) then
    raise_ir_type_error
      ("Type mismatch:\n Expected:\n" ^ Types.string_of_datatype et ^ "\n Actual:\n " ^ Types.string_of_datatype at)
      `None

let check_eq_type_lists = fun exptl actl ->
  (* if typecheck_ir then
   *   begin *)
    if List.length exptl <> List.length actl then
      raise_ir_type_error "Arity missmatch" `None
    else
      List.iter2 (fun  et at ->
          check_eq_types et at
       )  exptl actl
    (* end *)

let ensure condition msg occurence =
  if condition then () else raise_ir_type_error msg occurence


let ensure_effect_present_in_row allowed_effects required_effect_name required_effect_type occurence =
  let (map, _, _) = fst (Types.unwrap_row allowed_effects) in
  match StringMap.find_opt required_effect_name map with
    | Some (`Present et) -> check_eq_types et required_effect_type
    | _ -> raise_ir_type_error ("Required effect" ^ required_effect_name ^ " not present in effect row " ^ Types.string_of_row allowed_effects) `None



let ensure_effect_rows_compatible allowed_effects imposed_effects_row occurence =
  (* TODO: Shall we flatten the row first? *)
  (* FIXEM: need to flatten here and remove absent fields if closed row *)
  ensure (eq_types (`Record allowed_effects, `Record imposed_effects_row)) "Incompatible effects" occurence

module Typecheck =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.Int.t

  let info_type (t, _, _) = t

  (* FIXME remove this? *)
  let deconstruct f t = f t

  module Env = Env.Int

  let checker tyenv =
  object (o)
    inherit IrTraversals.Transform.visitor(tyenv) as super

    (*val env = env*)
    val closure_def_env = Env.empty
    (* initialize to the default row of allowed toplevel effects*)
    val allowed_effects = Lib.typing_env.effect_row

    method lookup_closure_def_for_fun fid = Env.lookup closure_def_env fid

    method set_allowed_effects eff_row = {< allowed_effects = eff_row >}

    method impose_presence_of_effect effect_name effect_typ =
      ensure_effect_present_in_row allowed_effects effect_name effect_typ

    method! var : var -> (var * datatype * 'self_type) =
      fun var -> (var, o#lookup_type var, o)


    (* method closure_var : var -> (var * datatype * 'self_type) = *)
    (*   fun var -> (var, o#lookup_closure_type var, o) *)

    method! value : value -> (value * datatype * 'self_type) = fun orig ->
      match orig with
        | `Constant c -> let (c, t, o) = o#constant c in `Constant c, t, o (* checked 19.06. *)
        | `Variable x -> let (x, t, o) = o#var x in `Variable x, t, o (* checked 19.06. *)
        | `Extend (fields, base) as orig -> (* checked 19.06. *)
            let (fields, field_types, o) = o#name_map (fun o -> o#value) fields in
            let (base, base_type, o) = o#option (fun o -> o#value) base in

            let handle_extended_record = function
              | Some t -> `Record t
              | None -> raise_ir_type_error "Record already contains one of the extension fields" (`Value orig) in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match t with
                        | `Record row ->
                            handle_extended_record (extend_row_safe field_types row)
                        | _ -> raise_ir_type_error "Trying to extend non-record type" (`Value orig)
                    end
            in
              `Extend (fields, base), t, o
        | `Project (name, v) ->
            let (v, vt, o) = o#value v in
            `Project (name, v), deconstruct (project_type ~overstep_quantifiers:false name) vt, o

        | `Erase (names, v) ->
            let (v, vt, o) = o#value v in
            let t = deconstruct (erase_type ~overstep_quantifiers:false names) vt in
              `Erase (names, v), t, o

        | `Inject (name, v, t) ->
            let v, vt, o = o#value v in
            let _ = match t with
              | `Variant _ ->
                 check_eq_types (project_type ~overstep_quantifiers:false name t) vt
              | _ -> raise_ir_type_error "trying to inject into non-variant type" (`Value orig) in
            `Inject (name, v, t), t, o

        | `TAbs (tyvars, v) ->
            let v, t, o = o#value v in
            let t = Types.for_all (tyvars, t) in
              `TAbs (tyvars, v), t, o

        | `TApp (v, ts)  ->
            let v, t, o = o#value v in
              begin try
                  (* FIXME this expects recursive types to be cyclic
                     (and breaks our convention about them not being so in the backend)
                   we cannot just run the de-cycling afterwards*)
                let t = Instantiate.apply_type t ts in
                  `TApp (v, ts), t, o
              with
                  Instantiate.ArityMismatch ->
                    let msg = ("Arity mismatch in type application (Ir.Transform)")
                    ^ ("expression: "^string_of_value (`TApp (v, ts)))
                    ^ ("type: "^Types.string_of_datatype t)
                    ^ ("tyargs: "^String.concat "," (List.map (fun t -> Types.string_of_type_arg t) ts)) in
                    raise_ir_type_error msg (`Value orig)
              end
        | `XmlNode (tag, attributes, children) ->
            let (attributes, attribute_types, o) = o#name_map (fun o -> o#value) attributes in
            let (children  , children_types, o) = o#list (fun o -> o#value) children in

            let _ = StringMap.iter (fun _ t -> check_eq_types (`Primitive `String) t ) attribute_types in
            let _ = List.iter (fun t -> check_eq_types (`Primitive `XmlItem) t ) children_types in
            (* FIXME xml_type denotes a list of xml items. should we just return xmlitem here? *)
              `XmlNode (tag, attributes, children), xml_type, o

        | `ApplyPure (f, args) ->
            let (f, ft, o) = o#value f in
            let (args, argument_types, o) = o#list (fun o -> o#value) args in

            let parameter_types = arg_types ~overstep_quantifiers:false ft in
            check_eq_type_lists parameter_types argument_types;
            let effects = effect_row ~overstep_quantifiers:false ft in
            ensure (is_empty_row effects) "ApplyPure used for function with effects" (`Value orig);
            `ApplyPure (f, args),  return_type ~overstep_quantifiers:false ft, o

        | `Closure (f, z) -> (* checked 19.06. *)
            let (f, ft, o) = o#var f in
            let (z, zt, o) = o#value z in

            ensure (is_function_type ft) "Passing closure to non-funcion" (`Value orig);
            begin match o#lookup_closure_def_for_fun f with
            | Some binder -> check_eq_types (Var.type_of_binder binder) zt
            | None -> raise_ir_type_error "Providing closure to a function that doesn't expect one" (`Value orig)
            end;
            `Closure (f, z), ft, o

        | `Coerce (v, t) -> (* checked 19.06. *)
            let v, vt, o = o#value v in
            ensure (is_sub_type (vt, t)) (Printf.sprintf "coercion error: %s is not a subtype of %s"
                                         (string_of_datatype vt) (string_of_datatype t)) (`Value orig);
            `Coerce (v, t), t, o

    method! tail_computation :
      tail_computation -> (tail_computation * datatype * 'self_type) = fun orig ->
      match orig with
        | `Return v -> (* checked 19.06. *)
            let v, t, o = o#value v in
              `Return v, t, o

        | `Apply (f, args) -> (* checked 19.06. *)
            let f, ft, o = o#value f in
            Debug.print ("Function type in appl:" ^ string_of_datatype ft);
            let args, argtypes, o = o#list (fun o -> o#value) args in
            let exp_argstype = arg_types ~overstep_quantifiers:false ft in
            let effects = effect_row ~overstep_quantifiers:false ft in
            ensure_effect_rows_compatible allowed_effects effects (`TC orig);
            check_eq_type_lists exp_argstype argtypes;
            `Apply (f, args), return_type ~overstep_quantifiers:false ft, o

        | `Special special ->
            let special, t, o = o#special special in
              `Special special, t, o

        | `Case (v, cases, default) ->
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
                 ensure (StringSet.subset case_fields present_fields) "superfluous case" (`TC orig)
               else
                 begin
                   ensure (not (StringSet.is_empty present_fields)) "Case with neither cases nor default" (`TC orig);
                   ensure (is_closed) "case without default over open row"  (`TC orig);
                   ensure (not has_presence_polymorphism)
                     "case without default over variant with presence polymorphism in some field"  (`TC orig);
                   ensure (StringSet.equal case_fields present_fields)
                     "cases not identical to present fields in closed row, no default case" (`TC orig)
                 end;

               let cases, types, o =
                 StringMap.fold
                   (fun name  (binder, comp) (cases, types, o) ->
                     let type_binder = Var.type_of_binder binder in
                     let type_variant = variant_at ~overstep_quantifiers:false name variant in
                     check_eq_types type_binder type_variant;
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
               List.iter (fun ty -> check_eq_types t ty) (List.tl types);
               `Case (v, cases, default), t, o
            | _ ->  raise_ir_type_error "Case over non-variant value" (`TC orig)
            end
        | `If (v, left, right) ->
            let v, vt, o = o#value v in
            let left, lt, o = o#computation left in
            let right, rt, o = o#computation right in
            check_eq_types vt (`Primitive `Bool);
            check_eq_types lt rt;
            `If (v, left, right), lt, o

    method! special : special -> (special * datatype * 'self_type) = (* FIXME no typechecking yet*)
      function
        | `Wrong t -> `Wrong t, t, o
        | `Database v ->
            let v, _, o = o#value v in
              `Database v, `Primitive `DB, o
        | `Table (db, table_name, keys, tt) ->
            let db, db_type, o = o#value db in
            check_eq_types db_type Types.database_type;
            let table_name, table_name_type, o = o#value table_name in
            check_eq_types table_name_type Types.string_type;
            let keys, keys_type, o = o#value keys in
            check_eq_types keys_type Types.keys_type;
            (* TODO: tt is a tuple of three records. Check it to contain base types only and all rows should be closed. *)
              `Table (db, table_name, keys, tt), `Table tt, o
        | `Query (range, e, _) -> (* TODO perform checks specific to this constructor *)
            o#impose_presence_of_effect "wild" Types.unit_type;
            let range, o =
              o#optionu
                (fun o (limit, offset) ->
                   let limit, _, o = o#value limit in
                   let offset, _, o = o#value offset in
                     (limit, offset), o)
                range in
            let e, t, o = o#computation e in
              `Query (range, e, t), t, o
        | `Update ((x, source), where, body) -> (* TODO perform checks specific to this constructor *)
            o#impose_presence_of_effect "wild" Types.unit_type;
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
            let body, _, o = o#computation body in
              `Update ((x, source), where, body), Types.unit_type, o
        | `Delete ((x, source), where) -> (* TODO perform checks specific to this constructor *)
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
              `Delete ((x, source), where), Types.unit_type, o
        | `CallCC v ->
            let v, t, o = o#value v in
            (* TODO: What is the correct argument type for v, since it expects a continuation? *)
              `CallCC v, return_type ~overstep_quantifiers:false t, o
        | `Select (l, v) ->
           o#impose_presence_of_effect "wild" Types.unit_type;
           let v, t, o = o#value v in
           `Select (l, v), t, o
        | `Choice (v, bs) ->
           o#impose_presence_of_effect "wild" Types.unit_type;
           let v, _, o = o#value v in
           let bs, branch_types, o =
             o#name_map (fun o (b, c) ->
                         let b, o = o#binder b in
                         let c, t, o = o#computation c in
                         (b, c), t, o) bs in
           let t = (StringMap.to_alist ->- List.hd ->- snd) branch_types in
           `Choice (v, bs), t, o
	| `Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) -> (* TODO perform checks specific to this constructor *)
	   let (comp, _, o) = o#computation ih_comp in
           (* TODO FIXME traverse parameters *)
           let (depth, o) =
             match ih_depth with
             | `Deep params ->
                let (o, bindings) =
                  List.fold_left
                    (fun (o, bvs) (b,v) ->
                      let (b, o) = o#binder b in
                      let (v, _, o) = o#value v in
                      (o, (b,v) :: bvs))
                    (o, []) params
                in
                `Deep (List.rev bindings), o
             | `Shallow -> `Shallow, o
           in
	   let (cases, _branch_types, o) =
	     o#name_map
               (fun o (x, resume, c) ->
                 let (x, o) = o#binder x in
		 let (resume, o) = o#binder resume in
		 let (c, t, o) = o#computation c in
		 (x, resume, c), t, o)
	       ih_cases
	   in
           let (return, t, o) =
             let (b, o) = o#binder (fst ih_return) in
             let (comp, t, o) = o#computation (snd ih_return) in
             (b, comp), t, o
           in
	   `Handle { ih_comp = comp; ih_cases = cases; ih_return = return; ih_depth = depth}, t, o
	| `DoOperation (name, vs, t) -> (* TODO perform checks specific to this constructor *)
	   let (vs, _, o) = o#list (fun o -> o#value) vs in
	   (`DoOperation (name, vs, t), t, o)

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

    method handle_funbinding : tyvar list -> datatype -> datatype -> effects_type -> binding -> unit =
      fun tyvars expected_overall_funtype body_actual _acutal_effects fundef ->
        let missmatch () = raise_ir_type_error "Quantifier missmatch in function def" (`Binding fundef) in
        let check_types subst expected_returntype _expected_effects  =
          check_eq_types_inital_substmap subst expected_returntype body_actual;
          (* TODO check expected effect row vs acutal effects *) in
        let rec handle_foralls subst funtype tyvars = match funtype with
          | `ForAll (qs_boxed, t)  -> handle_unboxed_foralls subst (unbox_quantifiers qs_boxed) t tyvars
          | `Function (at, eff, rt)
          | `Lolli (at, eff, rt) ->
            if tyvars = [] then
              check_types subst rt eff
            else
              missmatch ()
          | _ -> missmatch ()
        and handle_unboxed_foralls subst quantifier_list rest_expected_type tyvars = match quantifier_list, tyvars with
          | [], _ -> handle_foralls subst rest_expected_type tyvars
          | (lid, lsubkind, _lmtv) :: lqs, (rid, rsubkind, _rmtv) :: rqs ->
              (* TODO check mtv here *)
              if lid = rid && lsubkind = rsubkind then
                let subst' = IntMap.add lid rid subst in
                handle_unboxed_foralls subst' lqs rest_expected_type rqs
              else
                missmatch ()
          | _ -> missmatch () in
        handle_foralls IntMap.empty expected_overall_funtype tyvars

    method! binding : binding -> (binding * 'self_type) =
      function
        | `Let (x, (tyvars, tc)) ->
            let tc, o =
            try
              let tc, act, o = o#tail_computation tc in
              let exp = Var.type_of_binder x in
              check_eq_types exp act;
              tc, o
            with e -> handle_ir_type_error e (tc, o) in
            let x, o = o#binder x in
            `Let (x, (tyvars, tc)), o

        | `Fun (f, (tyvars, xs, body), z, location) as binding ->
              let f, tyvars, xs, body, z, location, o =
              try
                let (z, o) = o#optionu (fun o -> o#binder) z in
                let (xs, o) =
                  List.fold_right
                    (fun x (xs, o) ->
                      let x, o = o#binder x in
                        (x::xs, o))
                    xs
                    ([], o) in

                let whole_function_expected = Var.type_of_binder f in
                let quantifiers_as_type_args = List.map type_arg_of_quantifier tyvars in
                let fully_applied_function_expected = Instantiate.apply_type whole_function_expected quantifiers_as_type_args in
                let expected_function_effects = TypeUtils.effect_row fully_applied_function_expected in
                let previously_allowed_effects = allowed_effects in
                let o = o#set_allowed_effects expected_function_effects in
                let body, body_actual, o = o#computation body in
                let o = o#set_allowed_effects previously_allowed_effects in

                o#handle_funbinding tyvars whole_function_expected body_actual StringMap.empty binding;

                (* (match body_expected, body_actual with
                  | (`ForAll _, `Function _) -> failwith "forall-func"  | (`Function _, `ForAll _) -> failwith "func-forall" | _(`Function _, `Function _) -> () ); *)
                (* check_eq_types body_expected body_actual; *)
                let o = OptionUtils.opt_app o#remove_binder o z in
                let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                f, tyvars, xs, body, z, location, o
              with e -> handle_ir_type_error e (f, tyvars, xs, body, z, location, o) in
              let f, o = o#binder f in
              `Fun (f, (tyvars, xs, body), z, location), o

        | `Rec defs  as binding ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let defs, o =
              List.fold_right
                (fun (f, (tyvars, xs, body), z, location) (fs, o) ->
                   let f, o = o#binder f in
                     ((f, (tyvars, xs, body), z, location)::fs, o))
                defs
                ([], o) in

            let defs, o =
            try
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
                  let quantifiers_as_type_args = List.map type_arg_of_quantifier tyvars in
                  let fully_applied_function_expected = Instantiate.apply_type whole_function_expected quantifiers_as_type_args in
                  let expected_function_effects = TypeUtils.effect_row fully_applied_function_expected in
                  let previously_allowed_effects = allowed_effects in
                  let o = o#set_allowed_effects expected_function_effects in
                  o#impose_presence_of_effect "wild" Types.unit_type;
                  let body, body_actual, o = o#computation body in
                  let o = o#set_allowed_effects previously_allowed_effects in

                  o#handle_funbinding tyvars whole_function_expected body_actual StringMap.empty binding;

                  (* check_eq_types body_expected body_actual; *)
                  (* (match body_expected, body_actual  with
                    | (`ForAll _, `Function _) -> failwith "forall-func"  | (`Function _, `ForAll _) -> failwith "func-forall" | _(`Function _, `Function _) -> () ); *)
                  let o = OptionUtils.opt_app o#remove_binder o z in
                  let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                    (f, (tyvars, xs, body), z, location)::defs, o)
                ([], o)
                defs in
              let defs = List.rev defs in
              defs, o
            with e -> handle_ir_type_error e (defs, o) in
              `Rec defs, o


        | `Alien (x, name, language) ->
            let x, o = o#binder x in
              `Alien (x, name, language), o

        | `Module (name, defs) ->
            let defs, o =
              match defs with
                | None -> None, o
                | Some defs ->
                    let defs, o = o#bindings defs
                    in
                      Some defs, o
            in
              `Module (name, defs), o

    method! binder : binder -> (binder * 'self_type) =
      fun (var, info) ->
        Debug.print ("binding" ^ string_of_int var);
        let tyenv = Env.bind tyenv (var, info_type info) in
          (var, info), {< tyenv=tyenv >}

    method remove_binder : binder -> 'self_type = fun binder ->
      Debug.print ("unbinding" ^ string_of_int (Var.var_of_binder binder));
      let tyenv = Env.unbind tyenv (Var.var_of_binder binder) in
      {< tyenv=tyenv >}

    method remove_binding : binding -> 'self_type = function
      | `Let (x, _) -> o#remove_binder x
      | `Fun  fundef -> o#remove_binder (Ir.binder_of_fun_def fundef)
      | `Rec fundefs ->
        List.fold_left
                (fun o fundef  ->
                   o#remove_binder (Ir.binder_of_fun_def fundef))
                o
                fundefs
      | `Alien (binder, _, _) -> o#remove_binder binder
      | `Module _ -> o

    method remove_bindings : binding list -> 'self_type =
      List.fold_left (fun o b -> o#remove_binding b) o


    method! program : program -> (program * datatype * 'self_type) = o#computation

    method! get_type_environment : environment = tyenv
  end

  let program tyenv p =
    let p, _, _ = (checker tyenv)#computation p in
      p

  let bindings tyenv b =
    let b, _ = (checker tyenv)#bindings b in
      b
end
