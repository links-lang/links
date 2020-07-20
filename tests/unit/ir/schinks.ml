open Links_core
open Utility

(* look at the .mli file if you just want to use this to write test cases *)

module CT = CommonTypes

module Repr = struct
  type name_maps = {
    tname_to_id : int StringMap.t;
    varname_to_id : int StringMap.t;
  }

  (* [@@deriving show] *)

  type 'a maker = {
    mk_ir_form : name_maps -> 'a;
    (* [@printer fun fmt _ -> ()] *)
    reserved_tids : IntSet.t;
    reserved_ids : IntSet.t;
    used_names : string list;
    used_tnames : string list;
  }

  (* [@@deriving show] *)

  type 'a t = ('a maker, string) Result.t

  let make (maker : 'a maker) : 'a =
    let rec add_binding reserved next_id used map =
      match (used, IntSet.mem next_id reserved) with
      | [], _ -> map
      | _, true -> add_binding reserved (next_id + 1) used map
      | name :: rem_names, false ->
          add_binding reserved (next_id + 1) rem_names
            (StringMap.add name next_id map)
    in
    let varname_to_id =
      add_binding maker.reserved_ids 0 maker.used_names StringMap.empty
    in
    let tname_to_id =
      add_binding maker.reserved_tids 0 maker.used_tnames StringMap.empty
    in
    maker.mk_ir_form { varname_to_id; tname_to_id }

  (* Note that this isn' quite monadic bind due to the param type of the passed function *)
  let bind (type a b) : a t -> ((name_maps -> a) -> b t) -> b t =
   fun t1 func ->
    let merge_info (prev_maker : a maker) (new_maker : b maker) : b t =
      (* Printf.printf "Prev Maker %s\n%!" (show_maker (fun fmt m -> ()) prev_maker);
       * Printf.printf "New Maker %s\n%!" (show_maker (fun fmt m -> ()) new_maker); *)
      let do_merge prev_res prev_used new_res new_used =
        let reserved_ids = IntSet.union prev_res new_res in
        let prev_used_names = StringSet.from_list prev_used in
        let used_names =
          List.append prev_used
            (List.filter
               (fun x -> not (StringSet.mem x prev_used_names))
               new_used)
        in
        (reserved_ids, used_names)
      in
      let reserved_ids, used_names =
        do_merge prev_maker.reserved_ids prev_maker.used_names
          new_maker.reserved_ids new_maker.used_names
      in
      let reserved_tids, used_tnames =
        do_merge prev_maker.reserved_tids prev_maker.used_tnames
          new_maker.reserved_tids new_maker.used_tnames
      in
      let result_maker =
        { new_maker with reserved_ids; used_names; reserved_tids; used_tnames }
      in
      (* Printf.printf "Result Maker %s\n%!" (show_maker (fun fmt m -> ()) result_maker); *)
      result_maker |> Result.ok
    in
    match (t1 : a t) with
    | Result.Error m -> Result.error m
    | Result.Ok prev_maker ->
        Result.bind (func prev_maker.mk_ir_form : b t) (merge_info prev_maker)

  let pure (ir_form : 'a) : 'a t =
    Result.ok
      {
        mk_ir_form = (fun _ -> ir_form);
        reserved_tids = IntSet.empty;
        reserved_ids = IntSet.empty;
        used_tnames = [];
        used_names = [];
      }

  let with_maps (mk_ir_form : name_maps -> 'a) : 'a t =
    Result.ok
      {
        mk_ir_form;
        reserved_tids = IntSet.empty;
        reserved_ids = IntSet.empty;
        used_tnames = [];
        used_names = [];
      }
end

open Repr

(* enables new OCaml magic syntax where let* x = M in N is desugared to
   bind M (fun x -> N) *)
let ( let* ) = bind

type 'a t = 'a Repr.t

let reify x = Result.map (fun maker -> make maker) x

(* helpers *)

let mk_const c = pure (Ir.Constant c)

let mk_primitive p = pure (Types.Primitive p)

let singleton_name mk_ir_form name =
  Result.ok
    {
      mk_ir_form;
      reserved_ids = IntSet.empty;
      used_names = [ name ];
      reserved_tids = IntSet.empty;
      used_tnames = [];
    }

let singleton_tname mk_ir_form tname =
  Result.ok
    {
      mk_ir_form;
      reserved_tids = IntSet.empty;
      used_tnames = [ tname ];
      reserved_ids = IntSet.empty;
      used_names = [];
    }

let singleton_var_id mk_ir_form id =
  Result.ok
    {
      mk_ir_form;
      reserved_ids = IntSet.singleton id;
      used_names = [];
      reserved_tids = IntSet.empty;
      used_tnames = [];
    }

let singleton_tvar_id mk_ir_form id =
  Result.ok
    {
      mk_ir_form;
      reserved_ids = IntSet.singleton id;
      used_names = [];
      reserved_tids = IntSet.empty;
      used_tnames = [];
    }

let empty (mk_ir_form : name_maps -> 'a) : 'a t =
  Result.ok
    {
      mk_ir_form;
      reserved_ids = IntSet.empty;
      used_names = [];
      reserved_tids = IntSet.empty;
      used_tnames = [];
    }

let lift_list_custom (type a b c) (l : a list) ~(inspect : a -> b t)
    ~(update : a -> b -> c) : c list t =
  List.fold_right
    (fun (el : a) (res : c list t) ->
      let* (r : name_maps -> c list) = res in
      (* let _test : name_maps -> c list = r in *)
      match (inspect el : b t) with
      | Result.Error m -> Result.Error m
      | Result.Ok (part_maker : b maker) ->
          Result.ok
            {
              part_maker with
              mk_ir_form =
                (fun maps -> update el (part_maker.mk_ir_form maps) :: r maps);
            })
    l
    (empty (fun _ -> []) : c list t)

let lift_list : 'a t list -> 'a list t =
 fun l -> lift_list_custom l ~inspect:Functional.identity ~update:(fun _ x -> x)

let default_subkind = Sugartypes.default_subkind

(*
 *
 * Links types
 *
 *)

let unit = pure Types.unit_type

let int = mk_primitive CT.Primitive.Int
let string = mk_primitive CT.Primitive.String

let forall qs t =
  let* qs = lift_list qs in
  let* t = t in
  with_maps (fun maps -> Types.ForAll (qs maps, t maps))

let tvar ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) name =
  let kind = (pk, sk) in
  let mk_var_point maps =
    let id = StringMap.find name maps.tname_to_id in
    Types.Meta (Unionfind.fresh (Types.Var (id, kind, `Rigid)))
  in
  singleton_tname mk_var_point name

let tvar_row ?(sk = default_subkind) name = tvar ~pk:CT.PrimaryKind.Row ~sk name

let fun_t ?effects parameters codomain =
  let* p = lift_list parameters in
  let params_tupled maps = Types.make_tuple_type (p maps) in
  let* c = codomain in
  match effects with
  | None ->
      with_maps (fun maps ->
          Types.Function
            (params_tupled maps, Types.make_empty_closed_row (), c maps))
  | Some et ->
      let* e = et in
      with_maps (fun maps ->
          Types.Function (params_tupled maps, e maps, c maps))

(* need to make sure that d .-->{e} c
  becomes Function(d,c,e) even though the operator is just binary, not ternary *)
let ( .-->{} ) lhs middle rhs = fun_t ~effects:middle lhs rhs

let fun_ct parameters codomain = fun_t parameters codomain

let ( |--> ) = fun_ct

let wild_fun_ct parameters codomain =
  let effects =
    pure
      (Types.make_singleton_closed_row ("wild", Types.Present Types.unit_type))
  in
  fun_t ~effects parameters codomain

let ( |~~> ) = wild_fun_ct

(* Quantifiers *)

let wi_q ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tid =
  let kind = (pk, sk) in
  singleton_tvar_id (fun _maps -> (tid, kind)) tid

let q ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) name =
  let kind = (pk, sk) in
  singleton_tname
    (fun maps -> (StringMap.find name maps.tname_to_id, kind))
    name

let q_row ?(sk = default_subkind) name = q ~pk:CT.PrimaryKind.Row ~sk name

(*
 * IR constructs
 *)

let int_const i = mk_const (CT.Constant.Int i)
let i = int_const

let string_const s = mk_const (CT.Constant.String s)
let s = string_const

let var name =
  singleton_name
    (fun maps -> Ir.Variable (StringMap.find name maps.varname_to_id))
    name

let wi_var id = singleton_var_id (fun _ -> Ir.Variable id) id

let binder ?(scope = Var.Scope.Local) name ty =
  let* ty = ty in
  singleton_name
    (fun maps ->
      let var_info = Var.make_info (ty maps) name scope in
      let id = StringMap.find name maps.varname_to_id in
      Var.make_binder id var_info)
    name

let wi_binder ?(scope = Var.Scope.Local) var_id ty : Ir.binder t =
  let* ty = ty in
  singleton_var_id
    (fun maps ->
      let fake_name = Printf.sprintf "gen_%d" var_id in
      let var_info = Var.make_info (ty maps) fake_name scope in
      Var.make_binder var_id var_info)
    var_id

let unit_value = pure (Ir.Extend (StringMap.empty, None))

let return value =
  let* v = value in
  with_maps (fun maps -> Ir.Return (v maps))

let if_ cond then_ else_ =
  let* c = cond in
  let* t = then_ in
  let* e = else_ in
  with_maps (fun maps -> Ir.If (c maps, t maps, e maps))

let apply f args =
  let* f = f in
  let* args = lift_list args in
  with_maps (fun maps -> Ir.Apply (f maps, args maps))

(*
 *
 * IR computations
 *
 *)

let tc_to_comp tail_computation =
  let* tc = tail_computation in
  with_maps (fun maps -> ([], tc maps))

let binding_to_comp (binding : Ir.binding t) =
  let* bs = lift_list [ binding ] in
  let* unit = return unit_value in
  with_maps (fun maps -> (bs maps, unit maps))

let bindings_to_comp (bindings : Ir.binding t list) =
  let* bs = lift_list bindings in
  let* unit = return unit_value in
  with_maps (fun maps -> (bs maps, unit maps))

let computation bindings tail_comp =
  let* bs = lift_list bindings in
  let* tc = tail_comp in
  with_maps (fun maps -> (bs maps, tc maps))

(*
 *
 * IR bindings
 *
 *)

let wi_let_ binder ?(tparams = []) body =
  let* bin = binder in
  let* (tparams : name_maps -> CT.Quantifier.t list) = lift_list tparams in
  let* body = body in
  with_maps (fun maps -> Ir.Let (bin maps, (tparams maps, body maps)))

let let_ name ty ?(tparams = []) ?scope body =
  let bin = binder ?scope name ty in
  wi_let_ bin ~tparams body

let var' name ty =
  let bin = binder name ty in
  bin

let ( let** ) (vs, maps) f = Ir.Let (maps, f vs)

let wi_def bin ?(tparams = []) params ?closure_var
    ?(location = CT.Location.Server) ?(unsafe_sig = false) body : Ir.fun_def t =
  let* bin = bin in
  let* (tparams : name_maps -> CT.Quantifier.t list) = lift_list tparams in
  let* (params : name_maps -> Ir.binder list) = lift_list params in
  let* body = body in
  match closure_var with
  | None ->
      with_maps (fun m ->
          (bin m, (tparams m, params m, body m), None, location, unsafe_sig))
  | Some cv_binder ->
      let* cv_binder = cv_binder in
      with_maps (fun m ->
          ( bin m,
            (tparams m, params m, body m),
            Some (cv_binder m),
            location,
            unsafe_sig ))

let def name ty ?(tparams = []) params ?scope ?closure_var
    ?(location = CT.Location.Server) ?(unsafe_sig = false) body : Ir.fun_def t =
  let bin = binder ?scope name ty in
  let params =
    List.map
      (fun (p_name, p_ty) -> binder ~scope:Var.Scope.Local p_name p_ty)
      params
  in
  let closure_var =
    Option.map
      (fun (cv_name, cv_ty) -> binder ~scope:Var.Scope.Local cv_name cv_ty)
      closure_var
  in
  wi_def bin ~tparams params ?closure_var ~location ~unsafe_sig body

type adef = { params : (string * Types.typ t) list }
let param name typ = (var name, { params = [ (name, typ) ] })

let ( let+ ) (b, _p) f = (f b, _p.params)

let ( and+ ) (b1, p1) (b2, p2) =
  ((b1, b2), { params = List.append p1.params p2.params })

let def' _name _ty _par = ()

let _foo =
  def' "f"
    ([ int ] |~~> int)
    (let+ x = param "x" int and+ _y = param "y" string in
     x)

let rec_ fun_defs =
  let* fun_defs = lift_list fun_defs in
  with_maps (fun maps -> Ir.Rec (fun_defs maps))

let wi_fun_ binder ?tparams params ?closure_var ?location ?unsafe_sig body :
    Ir.binding t =
  let* fun_def =
    wi_def binder ?tparams params ?closure_var ?location ?unsafe_sig body
  in
  with_maps (fun maps -> Ir.Fun (fun_def maps))

let fun_ name ty ?tparams params ?closure_var ?location ?unsafe_sig body :
    Ir.binding t =
  let* fun_def =
    def name ty ?tparams params ?closure_var ?location ?unsafe_sig body
  in
  with_maps (fun maps -> Ir.Fun (fun_def maps))
