open CommonTypes
open Utility
open Ir

module T = Types


let typecheck
  = Settings.(flag "typecheck_ir"
              |> synopsis "Type check the IR (development)"
              |> convert parse_bool
              |> sync)


(* ERROR HANDLING*)
let fail_on_ir_type_error
  = Settings.(flag "fail_on_ir_type_error"
              |> synopsis "Abort compilation if an IR type error occurs (experimental)"
              |> convert parse_bool
              |> sync)


let internal_error message =
  raise (Errors.internal_error ~filename:"irCheck.ml" ~message)

(* Artificial "supertype" of all IR types,
   so we can provide arbitrary IR fragments to the error handling functions *)
type ir_snippet =
  | STC    of tail_computation
  | SVal   of value
  | SSpec  of special
  | SBind  of binding
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
  if Settings.get fail_on_ir_type_error then
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
    | Errors.InternalError { message ; _ } when
        (Str.string_match (Str.regexp "Kind mismatch in type application" ) message 0) ->
      (* FIXME: The fact that this raises an internal error is horrible.
        Get rid of this once we kind-check all type applications within IrCheck. *)
      raise_ir_type_error message occurrence

let ensure condition msg occurrence =
  if condition then () else raise_ir_type_error msg occurrence

let _print_substmap subst =
  Debug.print ("Substmap\n:" ^ IntMap.show (fun fmt num -> Format.pp_print_int fmt num) subst)

(* TYPE EQUALITY *)

module Env = Env.Int

type type_eq_context = {
  typevar_subst : Var.var IntMap.t; (* equivalences of typevars *)
  tyenv: Kind.t Env.t (* track kinds of bound typevars *)
}


(* Detects recursion at any level *)
module RecursionDetector =
struct
  class visitor =
      object
        inherit Types.Transform.visitor as super

        val found_recursion = false
        method get_result () = found_recursion


        method! meta_type_var point = match Unionfind.find point with
          | T.Recursive _ -> ({< found_recursion = true >}, point)
          | _ -> super#meta_type_var point

        method! meta_row_var point = match Unionfind.find point with
          | T.Recursive _ -> ({< found_recursion = true >}, point)
          | _ -> super#meta_row_var point


      end

  let is_recursive t =
    let o = new visitor in
    let o' = fst (o#typ t) in
    o'#get_result ()

end



(* For both rows and types, detect recursion at top-level or immediately under
   a Body or Alias *)
let rec is_toplevel_rec_type = function
  | T.Meta mtv ->
     begin match Unionfind.find mtv with
       | T.Recursive _ -> true
       | _ -> false
     end
  | T.Alias (_, _, t') -> is_toplevel_rec_type t'
  | _ -> false

let is_toplevel_rec_row  row =
  let (_, row_var, _) = TypeUtils.extract_row_parts row in
  match Unionfind.find row_var with
       | T.Recursive _ -> true
       | _ -> false



let eq_types occurrence : type_eq_context -> (Types.datatype * Types.datatype) -> bool =
  let open Types in
  fun context (t1, t2) ->
    let lookupVar lvar map =
      match IntMap.find_opt lvar map with
      | Some rvar' -> rvar'
      | None       -> lvar in
    let handle_variable  (lid, (lpk, lsk), lfd) (rid, (rpk, rsk), rfd) ctx =
      let subst_map, kind_env = ctx.typevar_subst, ctx.tyenv in
      let rvar' = lookupVar lid subst_map in
      let is_equal = rid = rvar' && lpk = rpk && lsk = rsk && lfd = rfd in
      match is_equal, lfd with
        | true, `Flexible -> true
        | true, `Rigid ->
           begin match Env.find_opt rid kind_env with
             | Some (primary_kind_env, subkind_env) ->
                ensure
                  (lpk = primary_kind_env &&  rsk = subkind_env)
                  "Mismatch between (sub) kind information in variable vs stored in kind environment"
                  SNone;
                true
             | None -> raise_ir_type_error ("Type variable "  ^ (string_of_int rid) ^ " is unbound") occurrence
           end
        | false, _ -> false in

    let remove_absent_fields_if_closed row =
      (* assumes that row is flattened already and ignores recursive rows *)
      let (field_env, row_var, dual) as unpacked_row =
        row |> TypeUtils.extract_row_parts in
      if Types.is_closed_row row then
        let field_env' =
          Utility.StringMap.filter
            ( fun _ v -> match v with
              | T.Absent -> false
              | _ -> true )
            field_env
        in (field_env', row_var, dual)
      else unpacked_row in

    (* typevar_subst of ctx maps rigid typed/row/presence variables of t1 to corresponding ones of t2 *)
    let rec eqt ((context, t1, t2) : (type_eq_context * Types.datatype * Types.datatype)) =


      (* If t1 or t2 is recursive at the top, we give up. *)
      if is_toplevel_rec_type t1 || is_toplevel_rec_type t2 then
        begin
        Debug.print "IR typechecker encountered recursive type";
        true
        end
      else
      begin
      (* Collapses nested Foralls, unpacks Alias, Body *)
      let t1 = TypeUtils.concrete_type t1 in
      let t2 = TypeUtils.concrete_type t2 in
      match t1 with
      (* Unspecified kind *)
      | Not_typed ->
          begin match t2 with
              Not_typed -> true
            | _          -> false
          end
      | (Var _ | Recursive _ | Closed) ->
         raise Types.tag_expectation_mismatch
      | Alias _ -> assert false
      | Application (s, ts) ->
         begin match t2 with
         | Application (s', ts') ->
            Types.Abstype.equal s s' &&
            List.length ts = List.length ts' &&
            List.for_all2 (fun larg rarg -> eq_type_args (context, larg, rarg)) ts ts'
         | _ -> false
         end
      | RecursiveApplication _ ->
         Debug.print "IR typechecker encountered recursive type"; true
      | Meta lpoint ->
          begin match Unionfind.find lpoint with
            | Recursive _ -> assert false (* removed by now *)
            | lpoint_cont ->
               begin match t2 with
               | Meta rpoint ->
                  begin match lpoint_cont, Unionfind.find rpoint with
                  | Var lv, Var rv ->
                     handle_variable lv rv context
                  | _ ->
                     false
                  end
               | _ ->
                  (* This correponds to the old `Body cases,
                     which must have been lifted out of Meta by now*)
                  false
               end
          end
      (* Type *)
      | Primitive x ->
          begin match t2 with
              Primitive y -> x = y
            | _            -> false
          end
      | Function (lfrom, lm, lto) ->
          begin match t2 with
            | Function (rfrom, rm, rto) ->
              eqt     (context, lfrom, rfrom) &&
              eqt     (context, lto  , rto  ) &&
              eq_rows (context, lm   , rm   )
            | _ -> false
          end
      | Lolli (lfrom, lm, lto) ->
          begin match t2 with
            | Lolli (rfrom, rm, rto) ->
              eqt     (context, lfrom, rfrom) &&
              eqt     (context, lto  , rto  ) &&
              eq_rows (context, lm   , rm   )
            | _  -> false
          end
      | Record l ->
         begin match t2 with
         | Record r -> eq_rows (context, l, r)
         | _         -> false
         end
      | Variant l ->
         begin match t2 with
           Variant r -> eq_rows (context, l, r)
         | _          -> false
         end
      | Table (tmp1, lt1, lt2, lt3) ->
         begin match t2 with
         | Table (tmp2, rt1, rt2, rt3) ->
            tmp1 = tmp2 &&
            eqt (context, lt1, rt1) &&
            eqt (context, lt2, rt2) &&
            eqt (context, lt3, rt3)
         | _ -> false
         end
      | Lens _ -> raise (internal_error "The IR type equality check does not support lenses (yet)")
      | ForAll (qs, t) ->
         begin match t2 with
         | ForAll (qs', t') ->
            let (context', quantifiers_match) =
              List.fold_left2 (fun (context, prev_eq) lqvar rqvar ->
                  let lid, _ = lqvar in
                  let rid, _ = rqvar in
                  let l_kind = Quantifier.to_kind lqvar in
                  let r_kind = Quantifier.to_kind rqvar in
                  let ctx' = { typevar_subst = IntMap.add lid rid context.typevar_subst;
                               tyenv = Env.bind rid r_kind context.tyenv
                             } in
                  (ctx', prev_eq && l_kind = r_kind)
                ) (context,true) qs qs' in
            if quantifiers_match then
              eqt (context', t, t')
            else false
         | _ -> false
         end
      (* Effect *)
      | Effect l ->
         begin match t2 with
         | Effect r -> eq_rows (context, l, r)
         | _         -> false
         end
      | Operation (lfrom, lto, llin) ->
          begin match t2 with
            | Operation (rfrom, rto, rlin) ->
              eqt     (context, lfrom, rfrom) &&
              eqt     (context, lto  , rto  ) &&
              llin = rlin
            | _ -> false
          end
      (* Row *)
      | Row _ ->
         begin match t2 with
         |  Row _ -> eq_rows (context, t1, t2)
         | _ -> false
         end
      (* Presence *)
      | Absent ->
         begin match t2 with
         | Absent -> true
         | _ -> false
         end
      | Present lt ->
         begin match t2 with
         | Present rt -> eqt (context, lt, rt)
         | _ -> false
         end
      (* Session *)
      | Input (lt, _) ->
         begin match t2 with
         |  Input (rt, _) -> eqt (context, lt, rt)
         | _ -> false
         end
      | Output (lt, _) ->
         begin match t2 with
         |  Output (rt, _) -> eqt (context, lt, rt)
         | _ -> false
         end
      | Select lr ->
         begin match t2 with
         |  Select (rr) -> eq_rows (context, lr, rr)
         | _ -> false
         end
      | Choice lr ->
         begin match t2 with
         |  Choice rr -> eq_rows (context, lr, rr)
         | _ -> false
         end
      | Dual lt ->
         begin match t2 with
         |  Dual rt -> eqt (context, lt, rt)
         | _ -> false
         end
      | End ->
         begin match t2 with
         |  End -> true
         | _ -> false
         end
      end

    and eq_rows (context, r1, r2) =
      if is_toplevel_rec_row r1 || is_toplevel_rec_row r2 then
        (Debug.print "IR typechecker encountered recursive type";
        true)
      else
        let (lfield_env, lrow_var, ldual) = remove_absent_fields_if_closed (Types.flatten_row r1) in
        let (rfield_env, rrow_var, rdual) = remove_absent_fields_if_closed (Types.flatten_row r2) in
        let r1 = eq_field_envs (context, lfield_env, rfield_env) in
        let r2 = eq_row_vars (context, lrow_var, rrow_var) in
          r1 && r2 && ldual=rdual
    and eq_presence (context, l, r) =
      match Types.concrete_field_spec l, Types.concrete_field_spec r with
      | Absent, Absent -> true
      | Present lt, Present rt ->
         let b = eqt (context, lt, rt) in
         b
      | Meta lpoint, Meta rpoint ->
          begin
            match Unionfind.find lpoint, Unionfind.find rpoint with
            | Var lv, Var rv -> handle_variable lv rv context
            | _, _ ->
               raise (internal_error "should have removed all non-Vars in Meta of presence info by now")
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
      | Closed, Closed ->  true
      | Var lv, Var rv ->   handle_variable lv rv context
      | Recursive _, _
      | _, Recursive _ -> Debug.print "IR typechecker encountered recursive type"; true
      | _ ->  false
    and eq_type_args  (context, (lpk, l), (rpk, r))  =
      let open CommonTypes.PrimaryKind in
      match lpk, rpk with
      | Type,     Type     -> eqt (context, l, r)
      | Row,      Row      -> eq_rows (context, l, r)
      | Presence, Presence -> eq_presence (context, l, r)
      | _, _ -> false
    in
      eqt  (context, t1, t2)


let check_eq_types (ctx : type_eq_context) et at occurrence =
  if not (eq_types occurrence ctx (et, at)) then
    let exp_str = Types.string_of_datatype et in
    let act_str = Types.string_of_datatype ~refresh_tyvar_names:false at in
    raise_ir_type_error
      ("Type mismatch:\n Expected:\n  " ^ exp_str ^ "\n Actual:\n  " ^ act_str)
      occurrence

let check_eq_type_lists = fun (ctx : type_eq_context) exptl actl occurrence ->
    if List.length exptl <> List.length actl then
      raise_ir_type_error "Arity mismatch" occurrence
    else
      List.iter2 (fun  et at ->
          check_eq_types ctx et at occurrence
       )  exptl actl


let ensure_effect_present_in_row ctx allowed_effects required_effect_name required_effect_type occurrence =
  let (map, _, _) = fst (Types.unwrap_row allowed_effects) |> TypeUtils.extract_row_parts in
  match StringMap.find_opt required_effect_name map with
    | Some (T.Present et) -> check_eq_types ctx et required_effect_type occurrence
    | _ -> raise_ir_type_error ("Required effect " ^ required_effect_name ^ " not present in effect row " ^ Types.string_of_row allowed_effects) occurrence



let ensure_effect_rows_compatible ctx allowed_effects imposed_effects_row occurrence =
  ensure
    (eq_types occurrence ctx (T.Record allowed_effects, T.Record imposed_effects_row))
    (let allowed_str = Types.string_of_row allowed_effects in
     let actual_str  = Types.string_of_row ~refresh_tyvar_names:false
                                           imposed_effects_row in
     "Incompatible effects:\n Allowed:\n  " ^ allowed_str ^
     "\n Actual:\n  " ^ actual_str)
    occurrence


(** Compares lists of quantifiers for equal kinds.
  In particular, does not allow any kind of subkinding, the kinds must be idential.
  Assumes that you have checked that lengths are idential beforehand *)
let compare_quantifiers qs1 qs2 msg occurrence =
  List.iter2
  (fun q1 q2 ->
    ensure
      (Kind.equal (Quantifier.to_kind q1) (Quantifier.to_kind q2))
      msg
      occurrence)
  qs1
  qs2


(* TYPE CHECKING *)



module Typecheck =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.t

  let checker tyenv =
  object (o)
    inherit IrTraversals.Transform.visitor(tyenv) as super

    (*val env = env*)
    val closure_def_env = Env.empty
    val type_var_env = Env.empty

    (* initialize to the default row of allowed toplevel effects*)
    val allowed_effects = Lib.typing_env.effect_row

    (* TODO: closure handling needs to be reworked properly *)
    method lookup_closure_def_for_fun fid = Env.find_opt fid closure_def_env

    (* Creates a context for type equality checking *)
    method extract_type_equality_context () = { typevar_subst = IntMap.empty; tyenv = type_var_env }

    method set_allowed_effects eff_row = {< allowed_effects = eff_row >}, allowed_effects

    method impose_presence_of_effect (effect_name, effect_pre) occurrence : unit =
      let effect_typ = TypeUtils.from_present effect_pre in
      ensure_effect_present_in_row (o#extract_type_equality_context ()) allowed_effects effect_name effect_typ occurrence

    method add_typevar_to_context id kind = {< type_var_env = Env.bind id kind type_var_env  >}
    (* dangerous, unnecessary, and ungrammatical *)
    (* method remove_typevar_to_context id  = {< type_var_env = Env.unbind id type_var_env >} *)
    method get_type_var_env = type_var_env
    method set_type_var_env env = {< type_var_env = env >}

    method add_function_closure_binder f binder = {< closure_def_env = Env.bind f binder closure_def_env >}
    method remove_function_closure_binder f = {< closure_def_env = Env.unbind f closure_def_env  >}

    method check_eq_types t1 t2 occurrence = check_eq_types (o#extract_type_equality_context ()) t1 t2 occurrence

    method! lookup_type : var -> datatype = fun var ->
     match Env.find_opt var tyenv with
     | None -> raise_ir_type_error
                 (Printf.sprintf "Variable %d is unbound" var)
                 SNone
     | Some t -> t

    method! var : var -> ('self_type * var * datatype) =
      fun var -> (o, var, o#lookup_type var)



    method! value : value -> ('self_type * value * datatype) =
      let value_inner orig =
        let occurence = (SVal orig) in
        match orig with
        | Ir.Constant c -> let (o, c, t) = o#constant c in o, Ir.Constant c, t
        | Variable x -> let (o, x, t) = o#var x in o, Variable x, t
        | Extend (fields, base) as orig ->
            let (o, fields, field_types) = o#name_map (fun o -> o#value) fields in
            let (o, base, base_type) = o#option (fun o -> o#value) base in

            let handle_extended_record = function
              | Some t -> Record t
              | None -> raise_ir_type_error "Record already contains one of the extension fields" (SVal orig) in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.concrete_type t with
                        | Record row ->
                            handle_extended_record (extend_row_safe field_types row)
                        | _ -> raise_ir_type_error "Trying to extend non-record type" (SVal orig)
                    end
            in
              o, Extend (fields, base), t
        | Project (name, v) ->
            let (o, v, vt) = o#value v in
            o, Project (name, v), project_type ~overstep_quantifiers:false name vt

        | Erase (names, v) ->
            let (o, v, vt) = o#value v in
            let t = erase_type ~overstep_quantifiers:false names vt in
              o, Erase (names, v), t
        | Inject (name, v, t) ->
            let o, v, vt = o#value v in
            let _ = match TypeUtils.concrete_type t with
              | Variant _ ->
                 Debug.print "2" ;
                 let x = o#check_eq_types  (variant_at ~overstep_quantifiers:false name t) vt (SVal orig)
                 in Debug.print "22" ; x
              | _ -> raise_ir_type_error "trying to inject into non-variant type" (SVal orig) in
            o, Inject (name, v, t), t
        | TAbs (tyvars, v) ->
           let previous_tyenv = o#get_type_var_env in
           let o = List.fold_left
                     (fun o quant ->
                       let var  = Quantifier.to_var  quant in
                       let kind = Quantifier.to_kind quant in
                       o#add_typevar_to_context var kind) o tyvars in
           let o, v, t = o#value v in
           let o = o#set_type_var_env previous_tyenv in
           let t = Types.for_all (tyvars, t) in
           o, TAbs (tyvars, v), t
        | TApp (v, ts)  ->
           let o, v, t = o#value v in
           let t = Instantiate.apply_type t ts in
           o, TApp (v, ts), t
        | XmlNode (tag, attributes, children) ->
            let (o, attributes, attribute_types) = o#name_map (fun o -> o#value) attributes in
            let (o, children  , children_types) = o#list (fun o -> o#value) children in

            let _ = StringMap.iter (fun _ t -> o#check_eq_types  (Primitive Primitive.String) t (SVal orig)) attribute_types in
            let _ = List.iter (fun t -> o#check_eq_types  Types.xml_type t (SVal orig)) children_types in
              o, XmlNode (tag, attributes, children), Types.xml_type

        | ApplyPure (f, args) ->
            let rec is_pure_function = function
              | TApp (v, _)
              | TAbs (_, v) -> is_pure_function v
              | Variable var when Lib.is_primitive_var var -> Lib.is_pure_primitive (Lib.primitive_name var)
              | _ -> false in

            let (o, f, ft) = o#value f in
            let (o, args, argument_types) = o#list (fun o -> o#value) args in

            let parameter_types = arg_types ~overstep_quantifiers:false ft in
            check_eq_type_lists (o#extract_type_equality_context ()) parameter_types argument_types (SVal orig);
            ensure (is_pure_function f) "ApplyPure used for non-pure function" (SVal orig);
            o, ApplyPure (f, args),  return_type ~overstep_quantifiers:false ft


        | Closure (f, tyargs, z) ->
          (* We must not use o#var here, because by design that function fails if it sees an identifier denoting a function needing a closure *)
          let ft = o#lookup_type f in
          let (o, z, zt) = o#value z in

          let ft_instantiated = if tyargs = []
            then
              ft
            else
              let (remaining_type, instantiation_maps) = Instantiate.instantiation_maps_of_type_arguments false ft tyargs in
              Instantiate.datatype instantiation_maps remaining_type  in

          ensure (is_function_type (snd (TypeUtils.split_quantified_type ft_instantiated))) "Passing closure to non-function" (SVal orig);
          begin match o#lookup_closure_def_for_fun f with
          | Some optbinder ->
            begin match optbinder with
              | Some  binder ->

                let uninstantiated_closure_type_exp = (Var.type_of_binder binder) in
                let closure_quantifiers = TypeUtils.quantifiers uninstantiated_closure_type_exp in

                ensure
                  (List.length closure_quantifiers = List.length tyargs)
                  "Providing wrong number of closure type arguments"
                  occurence;

                let instantiated_closure_type_exp =
                  let (closure_remaining_type, closure_instantiation_maps) =
                    Instantiate.instantiation_maps_of_type_arguments true uninstantiated_closure_type_exp tyargs
                  in
                  Instantiate.datatype closure_instantiation_maps closure_remaining_type
                in
                o#check_eq_types instantiated_closure_type_exp zt (SVal orig)
              | None -> raise_ir_type_error "Providing closure to a function that does not need one" (SVal orig)
            end
          | None -> raise_ir_type_error "Providing closure to untracked function" (SVal orig)
          end;
          o, Closure (f, tyargs, z), ft_instantiated

        | Coerce (v, t) ->
            let o, v, vt = o#value v in
            if RecursionDetector.is_recursive vt || RecursionDetector.is_recursive t then
              begin
              (* TODO: We may want to implement simple subtyping for recursive types *)
              Debug.print "IR Typechecker gave up on coercion involving recursive types";
              o, Coerce (v, t), t
              end
            else
              begin
              ensure (eq_types (SVal orig) (o#extract_type_equality_context ())
                               (vt, t) || is_sub_type (vt, t))
                (let vt_str = string_of_datatype vt in
                 let t_str  = string_of_datatype ~refresh_tyvar_names:false t in
                 Printf.sprintf "coercion error: %s is not a subtype of %s"
                                vt_str t_str)
                (SVal orig);
              o, Coerce (v, t), t
              end
      in
      fun value -> translate_helper_exns value_inner value (SVal value)


    method! tail_computation :
      tail_computation -> ('self_type * tail_computation * datatype) =
      let tail_computation_inner orig = match orig with
        | Return v ->
            let o, v, t = o#value v in
              o, Return v, t

        | Apply (f, args) ->
            let o, f, ft = o#value f in
            let o, args, argtypes = o#list (fun o -> o#value) args in
            let exp_argstype = arg_types ~overstep_quantifiers:false ft in
            let effects = effect_row ~overstep_quantifiers:false ft in
            ensure_effect_rows_compatible (o#extract_type_equality_context ()) allowed_effects effects (STC orig);
            check_eq_type_lists (o#extract_type_equality_context ()) exp_argstype argtypes (STC orig);
            o, Apply (f, args), return_type ~overstep_quantifiers:false ft

        | Special special ->
            let o, special, t = o#special special in
              o, Special special, t

        | Ir.Case (v, cases, default) ->
            let o, v, vt = o#value v in
            begin match TypeUtils.concrete_type vt with
            | Variant row as variant ->
               let unwrapped_row = fst (unwrap_row row) |> TypeUtils.extract_row_parts in
               let present_fields, has_bad_presence_polymorphism  =
                 StringMap.fold (fun field field_spec (fields, poly) -> match field_spec with
                                           | Present _  -> (StringSet.add field fields), poly
                                           | Meta _ -> fields, StringMap.mem field cases
                                           | Absent -> fields, poly
                                           | _ -> raise Types.tag_expectation_mismatch)
                   (fst3 unwrapped_row) (StringSet.empty, false) in
               let is_closed = is_closed_row row in
               let has_default = OptionUtils.is_some default in
               let case_fields = StringMap.fold (fun field _ fields -> StringSet.add field fields) cases StringSet.empty in

               ensure (not has_bad_presence_polymorphism)
                 "row contains presence-polymorphic labels with corresponding \
                  match clauses. These can only be handled by a default case."  (STC orig);
               if has_default then
                 ensure (StringSet.subset case_fields present_fields) "superfluous case" (STC orig)
               else
                 begin
                   ensure (not (StringSet.is_empty present_fields)) "Case with neither cases nor default" (STC orig);
                   ensure (is_closed) "case without default over open row"  (STC orig);

                   ensure (StringSet.equal case_fields present_fields)
                     "cases not identical to present fields in closed row, no default case" (STC orig)
                 end;

               let o, cases, types =
                 StringMap.fold
                   (fun name  (binder, comp) (o, cases, types) ->
                     let type_binder = Var.type_of_binder binder in
                     Debug.print "1" ;
                     let type_variant = variant_at ~overstep_quantifiers:false name variant in
                     Debug.print "11" ;
                     o#check_eq_types type_binder type_variant (STC orig);
                     let o, b = o#binder binder in
                     let o, c, t = o#computation comp in
                     let o = o#remove_binder binder in
                     o, StringMap.add name (b,c) cases, t :: types)
                   cases (o, StringMap.empty, []) in
               let o, default, default_type =
                 o#option (fun o (b, c) ->
                     let o, b = o#binder b in
                     let actual_default_type = Var.type_of_binder b in
                     let expected_default_t =
                       StringMap.fold
                         (fun case _ v -> TypeUtils.split_variant_type case v |> snd)
                         cases
                         variant
                     in
                     o#check_eq_types expected_default_t actual_default_type (STC orig);
                     let o, c, t = o#computation c in
                     let o = o#remove_binder b in
                     o, (b, c), t) default in
               let types = OptionUtils.opt_app (fun dt -> dt :: types) types default_type in
               let t = List.hd types in
               List.iter (fun ty -> o#check_eq_types t ty (STC orig)) (List.tl types);
               o, Ir.Case (v, cases, default), t
            | _ ->  raise_ir_type_error "Case over non-variant value" (STC orig)
            end
        | If (v, left, right) ->
            let o, v, vt = o#value v in
            let o, left, lt = o#computation left in
            let o, right, rt = o#computation right in
            o#check_eq_types vt (Primitive Primitive.Bool) (STC orig);
            o#check_eq_types lt rt (STC orig);
            o, If (v, left, right), lt

      in
      fun tc -> translate_helper_exns tail_computation_inner tc (STC tc)




    method! special : special -> ('self_type * special * datatype) =
      let special_inner special = match special with
        | Wrong t -> o, Wrong t, t
        | Database v ->
            let o, v, vt = o#value v in
            (* v must be a record containing string fields  name, args, and driver*)
            List.iter (fun field ->
                o#check_eq_types (project_type field vt) Types.string_type (SSpec special)
              ) ["name"; "args"; "driver"];
            o, Database v, Primitive Primitive.DB

        | Table { database = db; table = table_name; keys; temporal_fields; table_type = tt } ->
            let o, db, db_type = o#value db in
            o#check_eq_types db_type Types.database_type (SSpec special);
            let o, table_name, table_name_type = o#value table_name in
            o#check_eq_types table_name_type Types.string_type (SSpec special);
            let o, keys, keys_type = o#value keys in
            o#check_eq_types keys_type Types.keys_type (SSpec special);
            (* TODO: tt is a tuple of three records. Discussion pending about what kind of checks we should do here
               From an implementation perspective, we should check the consistency of the read, write, needed info here *)
            o, Table { table = table_name; database = db; keys; temporal_fields; table_type = tt }, Table tt

        | TemporalJoin (tmp, comp, dt) ->
            (* Note: This is fairly bare-bones at the moment: checks that the type of the computation matches
               recorded type. May want to do more later. *)
            let o, comp, comp_dt = o#computation comp in
            o#check_eq_types dt comp_dt (SSpec special);
            o, TemporalJoin (tmp, comp, dt), dt
        | Query (range, policy, e, original_t) ->
            let o, range =
              o#optionu
                (fun o (limit, offset) ->
                   (* Query blocks themselves only have the wild effect when they have a range *)
                   o#impose_presence_of_effect Types.wild_present (SSpec special);

                   let o, limit, ltype = o#value limit in
                   let o, offset, otype = o#value offset in
                      o#check_eq_types ltype Types.int_type (SSpec special);
                      o#check_eq_types otype Types.int_type (SSpec special);
                     o, (limit, offset))
                range in
            (* query body must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let o, e, t = o#computation e in
            let o, _ = o#set_allowed_effects outer_effects in

            (* The type of the body must match the type the query is annotated with *)
            o#check_eq_types original_t t (SSpec special);

            (if not (policy = QueryPolicy.Flat) then
              () (* Discussion pending about how to type-check here. Currently same as frontend *)
            else
              let list_content_type = TypeUtils.element_type ~overstep_quantifiers:false t in
              let row = TypeUtils.extract_row list_content_type in
              ensure (Types.Base.row_satisfies row) "Only base types allowed in query result record" (SSpec special));

              o, Query (range, policy, e, t), t

        | InsertRows (_, source, rows)
        | InsertReturning (_, source, rows, _) ->
            (* Most logic is shared between InsertRow and InsetReturning.
               We disambiguate between the two later on. *)

            (* The insert itself is wild *)
            o#impose_presence_of_effect Types.wild_present (SSpec special);
            let o, source, source_t = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let table_write = TypeUtils.table_write_type source_t in
            let table_needed = TypeUtils.table_needed_type source_t in
            let table_needed_r = TypeUtils.extract_row table_needed in


            let o, rows, rows_list_t = o#value rows in
            let rows_t = TypeUtils.element_type ~overstep_quantifiers:false rows_list_t in
            let rows_r = TypeUtils.extract_row rows_t in

            ensure (Types.is_closed_row rows_r) "Inserted record must have closed row" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                 match presence_spec with
                  | Present actual_type_field ->
                    (* Ensure that the field we update is in the write row and the types match
                       As an invariant of Table types, it should then also be in the read row *)
                    let write_type = TypeUtils.project_type field table_write in
                    o#check_eq_types actual_type_field write_type (SSpec special);
                  | Absent -> () (* This is a closed row, ignore Absent *)
                  | Meta _  -> raise_ir_type_error "Found presence polymorphism in inserted row" (SSpec special)
                  | _ -> raise Types.tag_expectation_mismatch
              ) rows_r;


            (* The following should be an invariant of Table types? *)
            ensure (Types.is_closed_row table_needed_r) "Needed row of table type must be closed" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                 match presence_spec with
                  | Present needed_type ->
                    (* Ensure that all fields Present in the needed row are being inserted *)
                    let inserted_type = TypeUtils.project_type field rows_t in
                    o#check_eq_types inserted_type needed_type (SSpec special)
                  | Absent
                  | Meta _  -> ()
                  | _ -> raise Types.tag_expectation_mismatch
              ) table_needed_r;

            begin match special with
                | InsertRows (tmp, _, _) ->
                   o, InsertRows(tmp, source, rows), Types.unit_type
                | InsertReturning (tmp, _, _, (Constant (Constant.String id) as ret)) ->
                   (* The return value must be encoded as a string literal,
                      denoting a column *)
                   let ret_type = TypeUtils.project_type id table_read in
                   o#check_eq_types Types.int_type ret_type (SSpec special);
                   o, InsertReturning (tmp, source, rows, ret), Types.int_type
                | InsertReturning (_, _, _, _) ->
                   raise_ir_type_error "Return value in InsertReturning was not a string literal" (SSpec special)
                | _ -> assert false (* impossible at this point *)
            end

        | Update (upd, (x, source), where, body) ->
            o#impose_presence_of_effect Types.wild_present (SSpec special);
            let o, source, source_t = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let table_write = TypeUtils.table_write_type source_t in
            let o, x = o#binder x in
            o#check_eq_types (Var.type_of_binder x) table_read (SSpec special);
            (* where part must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let o, where = o#optionu (fun o where ->
                  let o, where, t = o#computation where in
                  o#check_eq_types t Types.bool_type (SSpec special);
                  o, where
                )
               where in
            let o, body, body_t = o#computation body in
            let body_record_row = (TypeUtils.extract_row body_t) in
            ensure (Types.is_closed_row body_record_row) "Open row as result of update" (SSpec special);
            TypeUtils.iter_row (fun field presence_spec ->
                match presence_spec with
                  | Present actual_type_field ->
                    (* Ensure that the field we update is in the write row and the types match *)
                    let expected_type_field = TypeUtils.project_type field table_write in
                    o#check_eq_types expected_type_field actual_type_field (SSpec special)
                  | Absent -> () (* This is a closed row, ignore Absent *)
                  | Meta _  -> raise_ir_type_error "Found presence polymorphism in the result of an update" (SSpec special)
                  | _ -> raise Types.tag_expectation_mismatch
              ) body_record_row;
            let o = o#remove_binder x in
            let o, _ = o#set_allowed_effects outer_effects in
              o, Update (upd, (x, source), where, body), Types.unit_type

        | Delete (del, (x, source), where) ->
            o#impose_presence_of_effect Types.wild_present (SSpec special);
            let o, source, source_t = o#value source in
            (* this implicitly checks that source is a table *)
            let table_read = TypeUtils.table_read_type source_t in
            let o, x = o#binder x in
            o#check_eq_types (Var.type_of_binder x) table_read (SSpec special);
            (* where part must not have effects *)
            let o, outer_effects = o#set_allowed_effects (Types.make_empty_closed_row ()) in
            let o, where = o#optionu (fun o where ->
                  let o, where, t = o#computation where in
                  o#check_eq_types t Types.bool_type (SSpec special);
                  o, where
                )
               where in
            let o = o#remove_binder x in
            let o, _ = o#set_allowed_effects outer_effects in
              o, Delete (del, (x, source), where), Types.unit_type

        | CallCC v ->
            let o, v, t = o#value v in
            (* TODO: What is the correct argument type for v, since it expects a continuation? *)
              o, CallCC v, return_type ~overstep_quantifiers:false t
        | Select (l, v) -> (* TODO perform checks specific to this constructor *)
           o#impose_presence_of_effect Types.wild_present (SSpec special);
           let o, v, t = o#value v in
           o, Select (l, v), t
        | Choice (v, bs) -> (* TODO perform checks specific to this constructor *)
           o#impose_presence_of_effect Types.wild_present (SSpec special);
           let o, v, _ = o#value v in
           let o, bs, branch_types =
             o#name_map (fun o (b, c) ->
                         let o, b = o#binder b in
                         let o, c, t = o#computation c in
                         o, (b, c), t) bs in
           let t = (StringMap.to_alist ->- List.hd ->- snd) branch_types in
           o, Choice (v, bs), t
        | Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) ->
          (* outer effects is R_d in the IR formalization *)
          let outer_effects = Types.flatten_row allowed_effects  in
          let outer_effects_parts = TypeUtils.extract_row_parts outer_effects in

          (* return_t is A_d in the IR formalization *)
          let (o, return, return_t, return_binder_type) =
            let return_binder, return_computation = ih_return in
             (* return_binder_type is A_c in the IR formalization *)
            let return_binder_type = Var.type_of_binder return_binder in
            let (o, b) = o#binder return_binder in
            let (o, comp, t) = o#computation return_computation in
            let o = o#remove_binder return_binder in
          o, (b, comp), t, return_binder_type in


          (* The effects and return type of all resumptions must be the same.
             instead of collecting them while processing the handler branches, we save the once we encounter first here.
             The subsequent cases are then compared against what's stored here *)
          let resumption_effects : Types.row option ref = ref None in
          let resumption_return_type : Types.datatype option ref = ref None in

          let (o, cases, branch_presence_spec_types) =
            o#name_map
              (fun o (x, resume, c) ->
                  let (o, x) = o#binder x in
                  let x_type = Var.type_of_binder x in
                  let (o, resume) = o#binder resume in
                  let resume_type = Var.type_of_binder resume in
                  let (cur_resume_args, cur_resume_effects, cur_resume_ret) = match TypeUtils.concrete_type resume_type with
                    | Function (a, b, c) -> a, b, c
                    | _ -> raise_ir_type_error "Resumptions has non-function type" (SSpec special) in

                  let presence_spec_funtype = Function (x_type, Types.make_empty_closed_row (), cur_resume_args) in

                  (match !resumption_effects, !resumption_return_type with
                    | (None, None) ->
                      resumption_effects := Some cur_resume_effects;
                      resumption_return_type := Some  cur_resume_ret
                    | (Some existing_resumption_effects, Some existing_resumption_rettype) ->
                      o#check_eq_types (Effect existing_resumption_effects) (Effect cur_resume_effects) (SSpec special);
                      o#check_eq_types existing_resumption_rettype cur_resume_ret (SSpec special)
                    | _ -> assert false);


                  (* ct is A_d in the IR formalization *)
                  let (o, c, ct) = o#computation c in
                  o#check_eq_types return_t ct (SSpec special);
                  let o = o#remove_binder x in
                  let o = o#remove_binder resume in
                  o, (x, resume, c), presence_spec_funtype)
              ih_cases in


          (* We now construct the inner effects from the outer effects and branch_presence_spec_types *)
          let (outer_effects_map, outer_effects_var, outer_effects_dualized) = outer_effects_parts in
          (* For each case branch, the corresponding entry goes directly into the field spec map of the inner effect row *)
          let inner_effects_map_from_branches = StringMap.map (fun x -> Present x) branch_presence_spec_types in
          (* We now add all entries from the outer effects that were not touched by the handler to the inner effects *)
          let inner_effects_map = StringMap.fold (fun effect outer_presence_spec map ->
              if StringMap.mem effect inner_effects_map_from_branches then
                map
              else
                StringMap.add effect outer_presence_spec map
            )  inner_effects_map_from_branches outer_effects_map in
          let inner_effects = Row (inner_effects_map, outer_effects_var, outer_effects_dualized) in

        (if not (Types.is_closed_row outer_effects) then
          let outer_effects_contain e = StringMap.mem e outer_effects_map in
          ensure (StringMap.for_all (fun e _ -> outer_effects_contain e) cases) "Outer effects are open but do not mention an effect handled by handler" (SSpec special));

          (* comp_t  is A_c in the IR formalization *)
          let o, _ = o#set_allowed_effects inner_effects in
          let (o, comp, comp_t) = o#computation ih_comp in
          let (o, depth) =
            match ih_depth with
            | Deep params ->
                (* TODO: Find out what these "params" are for *)
                let (o, bindings) =
                  List.fold_left
                    (fun (o, bvs) (b,v) ->
                      let (o, b) = o#binder b in
                      let (o, v, _) = o#value v in
                      let o = o#remove_binder b in
                      (o, (b,v) :: bvs))
                    (o, []) params
                in
                o, Deep (List.rev bindings)
            | Shallow -> o, Shallow in
          let o, _ = o#set_allowed_effects outer_effects in

          o#check_eq_types return_binder_type comp_t (SSpec special);

        (match !resumption_effects, !resumption_return_type, depth with
          | Some re, Some rrt, (Deep _) ->
            o#check_eq_types (Effect re) (Effect outer_effects) (SSpec special);
            o#check_eq_types return_t rrt (SSpec special)
          | Some re, Some rrt, Shallow ->
            o#check_eq_types (Effect re) (Effect inner_effects) (SSpec special);
            o#check_eq_types comp_t rrt (SSpec special)
          | _ -> ());


          o, Handle { ih_comp = comp; ih_cases = cases; ih_return = return; ih_depth = depth}, return_t

        | DoOperation (name, vs, t) -> (* TODO perform checks specific to this constructor *)
          let (o, vs, vs_t) = o#list (fun o -> o#value) vs in
          let arg_type_actual =  make_tuple_type vs_t in

          (* Checks that "name" is Present in the current effect row *)
          let effect_type = fst (TypeUtils.split_row name allowed_effects) in
           (* contrary to normal functions, the argument type is not tuple-ified if there is only a single argument.
              Therefore, can't use return_type and arg_types from TypeUtils here, because these have those assumptions hard-coded *)
          let (arg_type_expected, effects, ret_type_expected) = match TypeUtils.concrete_type effect_type with
            | Function (at, et, rt) -> (at, et, rt)
            | _ -> raise_ir_type_error "Non-function type associated with effect" (SSpec special) in

          ensure (Types.is_empty_row effects) "Effect case's function type has non-empty effect row" (SSpec special);
          o#check_eq_types arg_type_expected arg_type_actual (SSpec special);
          o#check_eq_types ret_type_expected t (SSpec special);

          (o, DoOperation (name, vs, t), t)

        | Lens _
        | LensSerial _
        | LensDrop _
        | LensSelect _
        | LensGet _
        | LensCheck _
        | LensJoin _
        | LensPut _ -> (* just do type reconstruction *) super#special special
      in
      fun special -> translate_helper_exns special_inner special (SSpec special)


   method! bindings : binding list -> ('self_type * binding list) =
      fun bs ->
        let o, bs =
          List.fold_left
            (fun (o, bs) b ->
               let (o, b) = o#binding b in
                 (o, b::bs))
            (o, [])
            bs
        in
          o, List.rev bs

    method! computation : computation -> ('self_type * computation * datatype) =
      fun (bs, tc) ->
        let o, bs = o#bindings bs in
        let o, tc, t = o#tail_computation tc in
        let o = o#remove_bindings bs in
          o, (bs, tc), t


   (* The function parameters (and potentially other recursive functions), must
      be added to the environment before calling.
      The closure variable is handled here and must not be added to the environment beforehand
      (if it exists) *)
    method handle_funbinding
             (expected_overall_funtype : datatype)
             (tyvars : Quantifier.t list)
             (parameter_types : datatype list)
             (body : computation)
             (closure_var: binder option)
             (is_recursive : bool)
             (occurrence : ir_snippet)
           : ('self_type * computation) =

        (* Get all toplevel quantifiers, even if nested as in
           forall a. forall b. t1 -> t2. Additionally, get type with quantifiers stripped away *)
        let exp_quant, exp_unquant_t =
          TypeUtils.split_quantified_type expected_overall_funtype in

        ensure
          (TypeUtils.is_function_type exp_unquant_t)
          "Function annotated with non-function type"
          occurrence;

        if List.length tyvars <> List.length exp_quant then
          raise_ir_type_error
            "Mismatch in number of function's quantifiers in tyvar list vs type annotation"
            occurrence
        else
          compare_quantifiers tyvars exp_quant "Mismatch in quantifier kinds in tyvar list vs type annotation" occurrence;

        (* In order to obtain the effects to type-check the body with,
           we take the expected effects and substitute the quantifiers from the type
           annotation with those in the tyvar list *)
        let quantifiers_as_type_args = List.map Types.type_arg_of_quantifier tyvars in
        let exp_t_substed = Instantiate.apply_type expected_overall_funtype quantifiers_as_type_args in
        let exp_effects_substed = TypeUtils.effect_row exp_t_substed in

        (* The binder for the closure variable has its own quantifiers.
           We instantiate those quantifiers with the corresponding quantifiers in the tyvar list
           of the function (i.e., the "inner" type parameters)
           This is so that the closure variable uses the same type names as, say, the parameters. *)
        let o =
          match closure_var with
            | Some closure_binder ->
               let closure_type = Var.type_of_binder closure_binder in
               let closure_var_quants, _ = TypeUtils.split_quantified_type closure_type in

               if List.length closure_var_quants > List.length tyvars then
                 raise_ir_type_error
                   "Too many quantifiers on closure binder type!"
                   occurrence
               else
                 compare_quantifiers
                   closure_var_quants
                   (ListUtils.take (List.length closure_var_quants) tyvars)
                   "Mismatch in quantifier kinds in tyvar list vs closure variable quantifiers"
                   occurrence;

               let (closure_var_rem_type, instantiation_maps) =
                 Instantiate.instantiation_maps_of_type_arguments
                   true (* This means we fail if |tyvars| < |closure_type_quants| *)
                   closure_type
                   (ListUtils.take (List.length closure_var_quants) quantifiers_as_type_args) in
               let substed_closure_var_type = Instantiate.datatype instantiation_maps closure_var_rem_type in
               let updated_closure_binder = Var.update_type substed_closure_var_type closure_binder in
               fst (o#binder updated_closure_binder)
            | None -> o
        in

        (if is_recursive then o#impose_presence_of_effect Types.wild_present occurrence);

        let previous_tyenv = o#get_type_var_env in
        let o = List.fold_left
              (fun o quant ->
                let var  = Quantifier.to_var  quant in
                let kind = Quantifier.to_kind quant in
                o#add_typevar_to_context var kind) o tyvars in

        (* determine body type, using translated version of expected effects in context *)
        let o, previously_allowed_effects = o#set_allowed_effects exp_effects_substed in
        let o, body, body_type = o#computation body in

        let o = o#set_type_var_env previous_tyenv in
        let o, _ = o#set_allowed_effects previously_allowed_effects in


        let is_linear = not (Types.Unl.type_satisfies exp_unquant_t) in
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

        let o = match closure_var with
          | Some closure_binder -> o#remove_binder closure_binder
          | None -> o
        in

        o, body

    method! binding : binding -> ('self_type * binding) =
      let binding_inner = function
        | (Let (x, (tyvars, tc))) as orig ->
            let lazy_check =
            lazy
              begin
                let previous_tyenv = o#get_type_var_env in
                let o = List.fold_left
                          (fun o quant ->
                            let var  = Quantifier.to_var  quant in
                            let kind = Quantifier.to_kind quant in
                            o#add_typevar_to_context var kind) o tyvars in
                let o, tc, act = o#tail_computation tc in
                let o = o#set_type_var_env previous_tyenv in
                let exp = Var.type_of_binder x in
                let act_foralled = Types.for_all (tyvars, act) in
                o#check_eq_types exp act_foralled (SBind orig);
                o, tc
              end in
            let (o, tc) = handle_ir_type_error lazy_check (o, tc) (SBind orig) in
            let o, x = o#binder x in
            o, Let (x, (tyvars, tc))

        | Fun fundef as binding ->
           (* It is important that the type annotations of the parameters are
              expressed in terms of the type variables from tyvars (also for rec
              functions) *)
              let {fn_binder = f; fn_tyvars = tyvars; fn_params = xs; fn_body; fn_closure = z;
                        fn_location; fn_unsafe} = fundef
              in
              let lazy_check =
              lazy(
                let (o, xs) =
                  List.fold_right
                    (fun x (o, xs) ->
                      let o, x = o#binder x in
                        (o, x::xs))
                    xs
                    (o, []) in

                let whole_function_expected = Var.type_of_binder f in
                let actual_parameter_types = (List.map Var.type_of_binder xs) in
                let o, body = o#handle_funbinding
                                whole_function_expected
                                tyvars
                                actual_parameter_types
                                fn_body
                                z
                                false
                                (SBind binding) in
                let o = o#add_function_closure_binder (Var.var_of_binder f) z in
                (* Debug.print ("added " ^ string_of_int (Var.var_of_binder f) ^ " to closure env"); *)

                let o = OptionUtils.opt_app o#remove_binder o z in
                let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                o, f, tyvars, xs, body, z, fn_location
              ) in
              let o, f, tyvars, xs, body, z, location =
               handle_ir_type_error lazy_check (o, f, tyvars, xs, fn_body, z, fn_location) (SBind binding) in
              let o, f = o#binder f in
              let o = o#add_function_closure_binder (Var.var_of_binder f) z in
              let fundef = {fn_binder = f; fn_tyvars = tyvars; fn_params = xs; fn_body = body; fn_closure = z;
                            fn_location = location; fn_unsafe}
              in
              o, Fun fundef

        | Rec defs  as binding ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let o, defs =
              List.fold_right
                (fun  fundef (o, fs) ->
                   let {fn_binder = f; fn_closure = z; _} = fundef in
                   let o = o#add_function_closure_binder (Var.var_of_binder f) z in
                   let o, f = o#binder f in
                     (o, {fundef with fn_binder = f}::fs))
                defs
                (o, []) in

            let lazy_check =
            lazy (
              let o, defs =
              List.fold_left
                (fun ((o : 'self_type), defs) fundef ->
                   let {fn_binder = f; fn_tyvars; fn_params = xs; fn_body; fn_closure = z;
                        fn_location; fn_unsafe} = fundef
                   in
                   let o, xs =
                     List.fold_right
                       (fun x (o, xs) ->
                          let (o, x) = o#binder x in
                            (o, x::xs))
                       xs
                       (o, []) in

                  let whole_function_expected = Var.type_of_binder f in
                  let actual_parameter_types = (List.map Var.type_of_binder xs) in
                  let o, body = o#handle_funbinding
                                whole_function_expected
                                fn_tyvars
                                actual_parameter_types
                                fn_body
                                z
                                (not fn_unsafe) (* Treat recursive bindings with unsafe sig as nonrecursive *)
                                (SBind binding) in
                  let o = o#add_function_closure_binder (Var.var_of_binder f) z in
                  (* Debug.print ("added " ^ string_of_int (Var.var_of_binder f) ^ " to closure env"); *)

                  let o = OptionUtils.opt_app o#remove_binder o z in
                  let o = List.fold_right (fun b o -> o#remove_binder b) xs o in
                  let fundef = {fn_binder = f; fn_tyvars; fn_params = xs; fn_body = body; fn_closure = z;
                                fn_location; fn_unsafe}
                  in
                    o, fundef::defs)
                (o, [])
                defs in
              let defs = List.rev defs in
              o, defs
            ) in
            let o, defs = handle_ir_type_error lazy_check (o, defs) (SBind binding) in
            o, Rec defs


        | Alien { binder; object_name; language } ->
           let o, x = o#binder binder in
           o, Alien { binder = x; object_name; language }

        | Module (name, defs) ->
            let o, defs =
              match defs with
                | None -> o, None
                | Some defs ->
                    let o, defs = o#bindings defs
                    in
                      o, Some defs
            in
              o, Module (name, defs)
      in
      fun b -> translate_helper_exns binding_inner b (SBind b)


    method! binder : binder -> ('self_type * binder) =
      fun b ->
      let var = Var.var_of_binder b in
      let t = Var.type_of_binder b in
      let tyenv = Env.bind var t tyenv in
      {< tyenv=tyenv >}, b

    (* WARNING: use of remove_binder / remove_binding is only sound
       because we guarantee uniqueness of the names of bound term
       variables; a more robust approach is to remember the
       environment before extensions and then restore as necessary *)
    method remove_binder : binder -> 'self_type = fun binder ->
      let tyenv = Env.unbind (Var.var_of_binder binder) tyenv in
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
                  o#remove_function_closure_binder f)
                o
                fundefs
      | Alien { binder; _ } -> o#remove_binder binder
      | Module _ -> o

    method remove_bindings : binding list -> 'self_type =
      List.fold_left (fun o b -> o#remove_binding b) o


    method! program : program -> ('self_type * program * datatype) = o#computation

    method! get_type_environment : environment = tyenv
  end

  let name = "ir_typechecker"

  let program state program =
    let open IrTransform in
    let tenv = Context.variable_environment (context state) in
    let check =
      lazy (let _, program', _ = (checker tenv)#program program in program')
    in
    let program'' = handle_ir_type_error check program (SProg program) in
    return state program''
end
