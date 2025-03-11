type mtypid = int    (* Module type ID *)
type mvarid = int32  (* Module variable ID *)
type closid = int32  (* Module variable ID *)
type mfunid = int32  (* Module function ID *)
type meffid = int32  (* Module effect ID *)
module MTypMap = Utility.IntMap
module FunIDMap = Utility.Map.Make(struct
  type t = mfunid
  let pp fmt v = Format.fprintf fmt "%lu" v
  let show v = Int32.unsigned_to_int v |> Option.get |> Int.to_string
  let compare = Int32.compare
end)
module EffectIDMap = Utility.Map.Make(struct
  type t = meffid
  let pp fmt v = Format.fprintf fmt "%lu" v
  let show v = Int32.unsigned_to_int v |> Option.get |> Int.to_string
  let compare = Int32.compare
end)

type ('a, 'b) rawfunctyp = private RawFunction of ('a -> 'b)
type ('a, 'b) functyp = private Function of ('a -> 'b)
type abs_closure_content = private AbsClosureContent
type 'a closure_content = private ClosureContent of 'a
type 'a continuation = private Continuation of 'a

type 'a typ =
  | TUnit : unit typ
  | TInt : int typ
  | TBool : bool typ
  | TFloat : float typ
  | TFunc : 'a typ_list * 'b typ -> ('a, 'b) functyp typ
  | TClosedFun : 'a typ_list * 'b typ -> ('a, 'b) rawfunctyp typ
  | TClosedVar : 'a typ_list * 'b typ -> ('a -> 'b) typ
  | TAbsClosArg : abs_closure_content typ
  | TClosArg : 'a typ_list -> 'a closure_content typ
  | TCont : 'a typ -> 'a continuation typ
  | TPair : 'a typ * 'b typ -> ('a * 'b) typ
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : ('a typ * 'b typ_list) -> ('a * 'b) typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list

let rec compare_anytyp (Type t1) (Type t2) = match t1, t2 with
  | TUnit, TUnit -> 0
  | TUnit, _ -> ~-1
  | _, TUnit -> 1
  | TInt, TInt -> 0
  | TInt, _ -> ~-1
  | _, TInt -> 1
  | TBool, TBool -> 0
  | TBool, _ -> ~-1
  | _, TBool -> 1
  | TFloat, TFloat -> 0
  | TFloat, _ -> ~-1
  | _, TFloat -> 1
  | TFunc (args1, ret1), TFunc (args2, ret2) ->
      let cret = compare_anytyp (Type ret1) (Type ret2) in if cret = 0 then compare_anytyp_list (TypeList args1) (TypeList args2) else cret
  | TFunc _, _ -> ~-1
  | _, TFunc _ -> 1
  | TClosedFun (args1, ret1), TClosedFun (args2, ret2) ->
      let cret = compare_anytyp (Type ret1) (Type ret2) in if cret = 0 then compare_anytyp_list (TypeList args1) (TypeList args2) else cret
  | TClosedFun _, _ -> ~-1
  | _, TClosedFun _ -> 1
  | TClosedVar (args1, ret1), TClosedVar (args2, ret2) ->
      let cret = compare_anytyp (Type ret1) (Type ret2) in if cret = 0 then compare_anytyp_list (TypeList args1) (TypeList args2) else cret
  | TClosedVar _, _ -> ~-1
  | _, TClosedVar _ -> 1
  | TAbsClosArg, TAbsClosArg -> 0
  | TAbsClosArg, _ -> ~-1
  | _, TAbsClosArg -> 1
  | TClosArg t1, TClosArg t2 -> compare_anytyp_list (TypeList t1) (TypeList t2)
  | TClosArg _, _ -> ~-1
  | _, TClosArg _ -> 1
  | TCont t1, TCont t2 -> compare_anytyp (Type t1) (Type t2)
  | TCont _, _ -> ~-1
  | _, TCont _ -> 1
  | TPair (t11, t21), TPair (t12, t22) ->
      let c1 = compare_anytyp (Type t11) (Type t12) in if c1 = 0 then compare_anytyp (Type t21) (Type t22) else c1
and compare_anytyp_list (TypeList tl1) (TypeList tl2) = match tl1, tl2 with
  | TLnil, TLnil -> 0
  | TLnil, TLcons _ -> ~-1 | TLcons _, TLnil -> 1
  | TLcons (hd1, tl1), TLcons (hd2, tl2) ->
      let chd = compare_anytyp (Type hd1) (Type hd2) in if chd = 0 then compare_anytyp_list (TypeList tl1) (TypeList tl2) else chd

module TypeMap = Utility.Map.Make(struct
  type t = anytyp
  let pp fmt (_ : t) = Format.fprintf fmt "<some type>"
  let show (_ : t) = "<some type>"
  let compare = compare_anytyp
end)

type ('a, 'r) unop =
  | UONegI : (int,   int)   unop
  | UONegF : (float, float) unop
type anyunop = Unop : ('a, 'b) unop -> anyunop

type ('a, 'b, 'r) binop =
  | BOAddI : (int,   int,   int)   binop
  | BOAddF : (float, float, float) binop
  | BOSubI : (int,   int,   int)   binop
  | BOSubF : (float, float, float) binop
  | BOMulI : (int,   int,   int)   binop
  | BOMulF : (float, float, float) binop
  | BODivI : (int,   int,   int)   binop
  | BODivF : (float, float, float) binop
  | BORemI : (int,   int,   int)   binop
  | BOEq : ('a, 'a, bool) binop
  | BONe : ('a, 'a, bool) binop
type anybinop = Binop : ('a, 'b, 'c) binop -> anybinop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = 'a typ * mvarid
type ('a, 'b, 'c) funcid = ('a, 'b) functyp typ * 'c typ_list * mtypid * mfunid
type ('a, 'b, 'c) closed_like = Function of ('a, 'b, unit) funcid * 'c | Closure of locality * ('a -> 'b) varid
type ('a, 'b) effectid = ('a -> 'b) typ * meffid
type handle_depth = Shallow | Deep

type anyvarid = VarID : 'a varid -> anyvarid
type 'a anyfuncid = FuncID : ('b, 'c, 'd) funcid * 'a -> 'a anyfuncid
type 'a anyclike = ClosedLike : ('b, 'c, 'a) closed_like -> 'a anyclike
  
type ('a, 'b) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap of 'a varid * 'b typ * 'b block
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
  | EConvertClosure : mvarid * 'a closure_content typ * mtypid -> 'a closure_content expr
  | EConstUnit : unit expr
  | EConstInt : int64 -> int expr
  | EConstBool : bool -> bool expr
  | EConstFloat : float -> float expr
  | EUnop : ('a, 'b) unop * 'a expr -> 'b expr
  | EBinop : ('a, 'b, 'c) binop * 'a expr * 'b expr -> 'c expr
  | EVariable : locality * 'a varid -> 'a expr
  | EClose : ('a, 'b, 'c) funcid * 'c expr_list -> ('a -> 'b) expr
  | ECallClosed : ('a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : 'a typ * bool expr * 'a block * 'a block -> 'a expr
  (* | ELoop : 'a typ * bool expr * 'a block -> 'a expr *) (* TODO *)
  | EDo : ('a, 'b) effectid * 'a expr_list -> 'b expr
  | EHandle : handle_depth * 'd continuation varid * (unit, 'b, 'c) funcid * 'c expr_list * ('b, 'd) finisher * 'd handler list -> 'd expr
and 'a handler =
  | HandlerRet : ('a, 'c) effectid * 'a varid_list * 'b block -> 'b handler
  | HandlerCont : ('a, 'b) effectid * 'a varid_list * assign list * 'b expr_list * ('c, 'c) finisher -> 'c handler
and 'a expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list
and 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type anyblock = Block : 'a typ * 'a block -> anyblock
type anyexpr = Expr : 'a typ * 'a expr -> anyexpr
type anyexpr_list = ExprList : 'a typ_list * 'a expr_list -> anyexpr_list

type func = {
  fun_typ : mtypid;
  fun_id : mfunid;
  fun_export_data : (string * mtypid) option;
  fun_closure : anytyp_list * mtypid; fun_converted_closure : mvarid option;
  fun_args : anytyp_list;
  fun_locals: anytyp_list;
  fun_block : anyblock;
}
type modu = {
  mod_nfuns : int32;
  mod_funs : func list;
  mod_needs_export : mtypid FunIDMap.t;
  mod_typs : anytyp MTypMap.t;
  mod_neffs : int32;
  mod_effs : anytyp_list EffectIDMap.t;
  mod_nglobals : int32;
  mod_global_vars : (mvarid * anytyp * string) list;
  mod_locals: anytyp_list;
  mod_block : anyblock;
}

(* TRANSLATION *)

let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

module Builtins : sig
  val get_unop : string -> anyunop option
  val get_binop : string -> anybinop option
end = struct
  open Utility
  
  let unops = StringMap.from_alist ["-", Unop UONegI; "-.", Unop UONegF]
  let binops = StringMap.from_alist [
    "+", Binop BOAddI; "+.", Binop BOAddF; "-", Binop BOSubI; "-.", Binop BOSubF;
    "*", Binop BOMulI; "*.", Binop BOMulF; "/", Binop BODivI; "/.", Binop BODivF;
    "%", Binop BORemI; "==", Binop BOEq; "<>", Binop BONe;
  ]
  
  let get_unop op = StringMap.find_opt op unops
  let get_binop op = StringMap.find_opt op binops
end

open Ir

let rec skip_toplevel_polymorphism v = match v with
	| TAbs (_, v) | TApp (v, _) -> skip_toplevel_polymorphism v
	| _ -> v
let rec _convert_type (t : Types.typ)
    (normal : anytyp -> 'a)
    (func : Types.typ -> Types.typ -> Types.typ -> 'a)
    (row : Types.field_spec_map -> Types.meta_row_var -> bool -> 'a) : 'a = match t with
  | Types.Not_typed -> failwith "TODO _convert_type Not_typed"
  | Types.Var _ -> failwith "TODO _convert_type Var"
  | Types.Recursive _ -> failwith "TODO _convert_type Recursive"
  | Types.Alias _ -> failwith "TODO _convert_type Alias"
  | Types.Application _ -> failwith "TODO _convert_type Application"
  | Types.RecursiveApplication _ -> failwith "TODO _convert_type RecursiveApplication"
  | Types.Meta t -> _convert_type (Unionfind.find t) normal func row
  | Types.Primitive CommonTypes.Primitive.Bool -> normal (Type TBool)
  | Types.Primitive CommonTypes.Primitive.Int -> normal (Type TInt)
  | Types.Primitive CommonTypes.Primitive.Float -> normal (Type TFloat)
  | Types.Primitive _ -> failwith "TODO _convert_type Primitive"
  | Types.Function (args, eff, ret) -> func args eff ret
  | Types.Lolli _ -> failwith "TODO _convert_type Lolli"
  | Types.Record t -> _convert_type t normal func row
  | Types.Variant _ -> failwith "TODO _convert_type Variant"
  | Types.Table _ -> failwith "TODO _convert_type Table"
  | Types.Lens _ -> failwith "TODO _convert_type Lens"
  | Types.ForAll (_, t) -> _convert_type t normal func row (* Ignore polymorphism for now *)
  | Types.Effect _ -> failwith "TODO _convert_type Effect"
  | Types.Operation _ -> failwith "TODO _convert_type Operation"
  | Types.Row (fsm, mrv, b) -> row fsm mrv b
  | Types.Closed -> failwith "TODO _convert_type Closed"
  | Types.Absent -> failwith "TODO _convert_type Absent"
  | Types.Present t -> _convert_type t normal func row
  | Types.Input _ -> failwith "TODO _convert_type Input"
  | Types.Output _ -> failwith "TODO _convert_type Output"
  | Types.Select _ -> failwith "TODO _convert_type Select"
  | Types.Choice _ -> failwith "TODO _convert_type Choice"
  | Types.Dual _ -> failwith "TODO _convert_type Dual"
  | Types.End -> failwith "TODO _convert_type End"
let rec convert_type (t : Types.typ) : anytyp =
  _convert_type t
    (fun t -> t)
    (fun args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        Type (TClosedVar (args, ret)))
    (fun _ _ _ -> failwith "TODO convert_type Row")
and convert_type_list (t : Types.typ) : anytyp_list =
  _convert_type t
    (fun (Type t) -> TypeList (TLcons (t, TLnil)))
    (fun args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        TypeList (TLcons (TClosedVar (args, ret), TLnil)))
    (fun fsm _ _ -> if Utility.StringMap.is_empty fsm then TypeList TLnil else failwith "TODO convert_type_list Row")

let sort_name_map (nm : 'a Ir.name_map) : (string * 'a) list =
  nm
    |> Utility.StringMap.bindings
    (* Since bindings are unique, there is no issue using an unstable sort *)
    |> List.fast_sort (fun (s1, _) (s2, _) -> String.compare s1 s2)

module LEnv : sig (* Contains the arguments, the local variables, etc *)
  type t
  val toplevel : t
  
  (* Internal use by the global env only *)
  val args_typ : t -> anytyp_list * (anytyp_list * mtypid)
  
  type args
  val no_arg : args
  val add_arg : args -> binder -> args
  val env_of_args : args -> binder option -> (anytyp -> 'a * mtypid) -> 'a * t (* closure binder is true if this is for the init function *)
  val add_closure : t -> t * mvarid * mvarid * anytyp_list
  
  val add_var : t -> binder -> anytyp -> t * mvarid
  val find_var : t -> var -> anyvarid option
  val is_closure : t -> var -> bool
  val find_closure : t -> string -> anyvarid
  
  val locals_of_env : t -> anytyp_list
  val compile : t -> mtypid -> mfunid -> string option -> ('a -> anytyp -> 'a * mtypid) -> 'a -> anyblock -> 'a * func
end = struct
  type t = {
    nargs : int32;
    args : anytyp_list;
    nlocs : int32;
    locs : anytyp_list;
    varmap : anyvarid Env.Int.t;
    cvarmap : anyvarid Env.String.t;
    clos : mvarid * (anytyp_list * mtypid) * bool; (* true if closure has been converted *)
    cbid : var option;
  }
  
  let toplevel = {
    nargs = 0l;
    args = TypeList TLnil;
    nlocs = 0l;
    locs = TypeList TLnil;
    varmap = Env.Int.empty;
    cvarmap = Env.String.empty;
    clos = Int32.minus_one, (TypeList TLnil, ~-1), true;
    cbid = None;
  }
  
  let args_typ (env : t) : anytyp_list * (anytyp_list * mtypid) =
    env.args, (let (_, cat, _) = env.clos in cat)
  
  type args = int32 * anytyp_list * anyvarid Env.Int.t
  
  let no_arg : args = 0l, TypeList TLnil, Env.Int.empty
  
  let add_arg (nargs, TypeList args, map : args) (argbind : binder) : args =
    let Type argt = convert_type (Var.type_of_binder argbind) in
    Int32.succ nargs,
      TypeList (TLcons (argt, args)),
      Env.Int.bind (Var.var_of_binder argbind) (VarID (argt, nargs)) map
  
  (* Similar to rev_append args tl, but works on anytyp_lists *)
  let extract_args (TypeList args) (TypeList tl) : anytyp_list =
    let rec inner : 'a 'b. 'a typ_list -> 'b typ_list -> anytyp_list =
      fun (type a b) (args : a typ_list) (acc : b typ_list) : anytyp_list -> match args with
      | TLnil -> TypeList acc
      | TLcons (hd, tl) -> inner tl (TLcons (hd, acc))
    in inner args tl
  
  let env_of_args (nargs, args, varmap : args) (closure : binder option) (add_typ : anytyp -> 'a * mtypid) : 'a * t =
    let closid = nargs in
    let rec add_all_clos varmap (TypeList acc) i ts = match ts with
      | [] -> varmap, TypeList acc
      | (n, t) :: ts -> (* TODO: optimize closures by giving the function reference and the continuation in two distinct members *)
          let Type t = convert_type t in
          let varmap = Env.String.bind n (VarID (t, i)) varmap in
          add_all_clos varmap (TypeList (TLcons (t, acc))) (Int32.succ i) ts
    in let add_all_clos (closure : binder option) = match closure with
      | None -> Env.String.empty, None, TypeList TLnil
      | Some bclos ->
          let nm = _convert_type (Var.type_of_binder bclos)
            (fun _ -> raise (internal_error "Expected a row type, got another type"))
            (fun _ _ _ -> raise (internal_error "Expected a row type, got a function type"))
            (fun fsm _ _ -> sort_name_map fsm) in
          let cvarmap, cat = add_all_clos Env.String.empty (TypeList TLnil) 0l nm in
          cvarmap, Some (Var.var_of_binder bclos), cat
    in let cvarmap, cbid, cat = add_all_clos closure in
    let env, ctid = add_typ (let TypeList cat = cat in Type (TClosArg cat)) in
    let args = extract_args args (TypeList TLnil) in
    let nargs = Int32.succ nargs in
    env, {
      nargs;
      args;
      nlocs = 0l;
      locs = TypeList TLnil;
      varmap; cvarmap;
      clos = closid, (cat, ctid), false;
      cbid;
    }
  
  let add_closure (env : t) : t * mvarid * mvarid * anytyp_list =
    let acid, (TypeList cat, ctid) = match env.clos with
      | _, _, true -> raise (internal_error "Double add_closure call")
      | acid, cat, false -> acid, cat
    in let ccid = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cat, tl)));
      clos = ccid, (TypeList cat, ctid), true;
    }, acid, ccid, TypeList cat
  
  let add_var (env : t) (b : binder) (Type t : anytyp) : t * mvarid =
    let vidx = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
      varmap = Env.Int.bind (Var.var_of_binder b) (VarID (t, vidx)) env.varmap;
    }, vidx
  let find_var (env : t) (v : var) : anyvarid option = Env.Int.find_opt v env.varmap
  let is_closure (env : t) (v : var) : bool = match env.cbid with None -> false | Some cbid -> Int.equal cbid v
  let find_closure (env : t) (v : string) : anyvarid = Env.String.find v env.cvarmap
  
  let locals_of_env (env : t) : anytyp_list = extract_args env.locs (TypeList TLnil)
  
  let compile (env : t) (ftid : mtypid) (fid : mfunid) (export_name : string option)
              (add_typ : 'a -> anytyp -> 'a * mtypid) (acc : 'a) (b : anyblock) : 'a * func =
    let closid, clostyp, has_converted_closure = env.clos in
    let acc, export_data = match export_name with
      | Some name ->
          let TypeList args = env.args in
          let Block (ret, _) = b in
          let acc, exportidx = add_typ acc (Type (TClosedFun (args, ret))) in
          acc, Some (name, exportidx)
      | None -> acc, None
    in let convclos = if has_converted_closure then Some closid else None
    in let f = {
      fun_typ = ftid;
      fun_id = fid;
      fun_export_data = export_data;
      fun_closure = clostyp; fun_converted_closure = convclos;
      fun_args = env.args;
      fun_locals = extract_args env.locs (TypeList TLnil);
      fun_block = b;
    } in acc, f
end

module GEnv : sig (* Contains the functions, the types, etc *)
  type t
  type funid
  val empty : string Env.Int.t -> Utility.IntSet.t -> t
  
  val add_typ : t -> anytyp -> t * mtypid
  
  val get_var_name : t -> var -> string
  val find_fun : t -> LEnv.t -> var -> funid anyclike
  val find_closable_fun : t -> LEnv.t -> var -> funid anyfuncid
  
  val add_var : t -> LEnv.t -> binder -> anytyp -> t * LEnv.t * locality * mvarid
  val find_var : t -> LEnv.t -> var -> locality * anyvarid
  
  val allocate_function : t -> LEnv.t -> binder -> funid * mfunid * mtypid * t
  val assign_function : t -> funid -> func -> t
  val do_export_function : t -> funid -> t
  
  val add_effect : t -> string -> anytyp_list -> Types.typ -> t * meffid
  
  val compile : t -> LEnv.t -> anyblock -> modu
end = struct
  (* In-place function emplacement in the environment *)
  type funid = func option ref * bool ref
  
  module EffectMap = Utility.Map.Make(struct
    type t = string * anytyp_list * Types.typ
    let pp _ = failwith "TODO Wasmir.GEnv.EffectMap pp"
    let show _ = failwith "TODO Wasmir.GEnv.EffectMap show"
    let compare (n1, ea1, er1) (n2, ea2, er2) =
      let c = String.compare n1 n2 in if c <> 0 then c else
      let c = compare_anytyp_list ea1 ea2 in if c <> 0 then c else
      compare er1 er2
  end)
  
  type t = {
    ge_map : string Env.Int.t;
    ge_nfuns : mfunid;
    ge_funs : (func option ref * mfunid * bool ref) list;
    ge_ntyps : mtypid;
    ge_typs : anytyp list;
    ge_typemap : mtypid TypeMap.t;
    ge_neffs : meffid;
    ge_effs : anytyp_list EffectIDMap.t;
    ge_effmap : meffid EffectMap.t;
    ge_ngbls : mvarid;
    ge_gbls : (mvarid * anytyp * string) list;
    ge_gblbinders : Utility.IntSet.t;
    ge_gblmap : anyvarid Env.Int.t;
    ge_fmap : funid anyfuncid Env.Int.t;
  }
  let empty (m : string Env.Int.t) (global_binders : Utility.IntSet.t) : t =
    let tmap = [] in {
      ge_map = m;
      ge_nfuns = 0l;
      ge_funs = [];
      ge_ntyps = 0;
      ge_typs = List.rev_map fst tmap;
      ge_typemap = TypeMap.of_list tmap;
      ge_neffs = 0l;
      ge_effs = EffectIDMap.empty;
      ge_effmap = EffectMap.empty;
      ge_ngbls = 0l;
      ge_gbls = [];
      ge_gblbinders = global_binders;
      ge_gblmap = Env.Int.empty;
      ge_fmap = Env.Int.empty;
    }
  
  let add_typ (env : t) (typ : anytyp) : t * mtypid = match TypeMap.find_opt typ env.ge_typemap with
    | Some idx -> env, idx
    | None ->
        let ge_ntyps = Int.succ env.ge_ntyps in
        let ge_typs = typ :: env.ge_typs in
        let ge_typemap = TypeMap.add typ env.ge_ntyps env.ge_typemap in
      { env with ge_ntyps; ge_typs; ge_typemap }, env.ge_ntyps
  
  let get_var_name (ge : t) (v : var) = Env.Int.find v ge.ge_map
  
  let add_var (ge : t) (le : LEnv.t) (b : binder) (t : anytyp) : t * LEnv.t * locality * mvarid =
    if Utility.IntSet.mem (Var.var_of_binder b) ge.ge_gblbinders then begin
      let ge_ngbls = Int32.succ ge.ge_ngbls in
      let ge_gbls = (ge.ge_ngbls, t, Var.name_of_binder b) :: ge.ge_gbls in
      let Type t = t in
      let newvar = VarID (t, ge.ge_ngbls) in
      let ge_gblmap = Env.Int.bind (Var.var_of_binder b) newvar ge.ge_gblmap in
      { ge with ge_ngbls; ge_gbls; ge_gblmap }, le, Global, ge.ge_ngbls
    end else let le, v = LEnv.add_var le b t in ge, le, Local StorVariable, v
  let find_var (ge : t) (le : LEnv.t) (v : var) : locality * anyvarid = match LEnv.find_var le v with
    | Some v -> Local StorVariable, v
    | None -> Global, Env.Int.find v ge.ge_gblmap
  
  let find_fun (ge : t) (le : LEnv.t) (v : var) : funid anyclike = match Env.Int.find_opt v ge.ge_fmap with
    | Some (FuncID ((TFunc (_, _), ctyp, _, _) as fid, fdata)) -> begin match ctyp with
        | TLnil -> ClosedLike (Function (fid, fdata))
        | TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
      end
    | None -> begin
        let loc, VarID ((t, _) as vid) = find_var ge le v in
        match t with
        | TClosedFun _ -> raise (internal_error "Unexpected raw closed function")
        | TClosedVar _ -> ClosedLike (Closure (loc, vid))
        | _ -> raise (internal_error "Unexpected variable type, expected closed function")
      end
  let find_closable_fun (ge : t) (_ : LEnv.t) (v : var) : funid anyfuncid = Env.Int.find v ge.ge_fmap
  
  let allocate_function (env : t) (args : LEnv.t) (b : binder) : funid * mfunid * mtypid * t =
    let f = ref None in
    let TypeList args, (TypeList ctyp, cltypid) = LEnv.args_typ args in
    let Type ret = _convert_type (Var.type_of_binder b)
        (fun _ -> raise (internal_error "Expected a function type, got another type"))
        (fun _ _ ret -> convert_type ret)
        (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type")) in
    let ftyp = TFunc (args, ret) in
    let env, ftypid = add_typ env (Type ftyp) in
    let fid = env.ge_nfuns in
    let needs_export = ref false in
    let fdata = (f, needs_export) in
    let ge_fmap = Env.Int.bind (Var.var_of_binder b) (FuncID ((ftyp, ctyp, cltypid, fid), fdata)) env.ge_fmap in
    fdata, fid, ftypid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = (f, fid, needs_export) :: env.ge_funs;
      ge_fmap;
    }
  let assign_function (env : t) ((fid, _) : funid) (f : func) : t = match !fid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> fid := Some f; env
  let do_export_function (env : t) ((_, fref) : funid) : t = fref := true; env
  
  let add_effect (env : t) (ename : string) (eargs : anytyp_list) (eret : Types.typ) : t * meffid =
    let eff = (ename, eargs, eret) in
    match EffectMap.find_opt eff env.ge_effmap with
    | Some i -> env, i
    | None ->
        let i = env.ge_neffs in
        let ge_neffs = Int32.succ env.ge_neffs in
        let ge_effs = EffectIDMap.add i eargs env.ge_effs in
        let ge_effmap = EffectMap.add eff i env.ge_effmap in
        let env = { env with ge_neffs; ge_effs; ge_effmap; } in
        env, i
  
  let compile (ge : t) (le : LEnv.t) (blk : anyblock) : modu =
    let lvs = LEnv.locals_of_env le in
    {
      mod_nfuns = ge.ge_nfuns;
      mod_funs =
        List.rev_map
          (fun (r, _, _) -> match !r with None -> raise (internal_error "function was allocated but never assigned") | Some f -> f)
          ge.ge_funs;
      mod_needs_export =
        List.fold_left (fun acc (fo, fid, rb) ->
          if !rb then
            match !fo with
            | None -> raise (internal_error "function was allocated but never assigned")
            | Some f -> FunIDMap.add fid f.fun_typ acc
          else acc) FunIDMap.empty ge.ge_funs;
      mod_typs = MTypMap.of_seq (Seq.mapi (fun i v -> i, v) (List.to_seq (List.rev ge.ge_typs)));
      mod_neffs = ge.ge_neffs;
      mod_effs = ge.ge_effs;
      mod_nglobals = ge.ge_ngbls;
      mod_global_vars = List.rev ge.ge_gbls;
      mod_locals = lvs;
      mod_block = blk;
    }
end
type genv = GEnv.t
type lenv = LEnv.t

type ('a, 'b) eq = Eq : 'a. ('a, 'a) eq
let rec assert_eq_typ : 'a 'b. 'a typ -> 'b typ -> string -> ('a, 'b) eq =
  fun (type a b) (t1 : a typ) (t2 : b typ) (onfail : string) : (a, b) eq -> match t1, t2 with
  | TUnit, TUnit -> Eq
  | TUnit, _ | _, TUnit -> raise (internal_error onfail)
  | TInt, TInt -> Eq
  | TInt, _ | _, TInt -> raise (internal_error onfail)
  | TBool, TBool -> Eq
  | TBool, _ | _, TBool -> raise (internal_error onfail)
  | TFloat, TFloat -> Eq
  | TFloat, _ | _, TFloat -> raise (internal_error onfail)
  | TFunc (tl1, r1), TFunc (tl2, r2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Eq
  | TFunc _, _ | _, TFunc _ -> raise (internal_error onfail)
  | TClosedFun (tl1, r1), TClosedFun (tl2, r2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Eq
  | TClosedFun _, _ | _, TClosedFun _ -> raise (internal_error onfail)
  | TClosedVar (tl1, r1), TClosedVar (tl2, r2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Eq
  | TClosedVar _, _ | _, TClosedVar _ -> raise (internal_error onfail)
  | TAbsClosArg, TAbsClosArg -> Eq
  | TAbsClosArg, _ | _, TAbsClosArg -> raise (internal_error onfail)
  | TClosArg t1, TClosArg t2 -> let Eq = assert_eq_typ_list t1 t2 onfail in Eq
  | TClosArg _, _ | _, TClosArg _ -> raise (internal_error onfail)
  | TCont t1, TCont t2 -> let Eq = assert_eq_typ t1 t2 onfail in Eq
  | TCont _, _ | _, TCont _ -> raise (internal_error onfail)
  | TPair (t11, t21), TPair (t12, t22) -> let Eq, Eq = assert_eq_typ t11 t12 onfail, assert_eq_typ t21 t22 onfail in Eq
and assert_eq_typ_list : 'a 'b. 'a typ_list -> 'b typ_list -> string -> ('a, 'b) eq =
  fun (type a b) (t1 : a typ_list) (t2 : b typ_list) (onfail : string) : (a, b) eq -> match t1, t2 with
  | TLnil, TLnil -> Eq
  | TLcons _, TLnil | TLnil, TLcons _ -> raise (internal_error onfail)
  | TLcons (t1, tl1), TLcons (t2, tl2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ t1 t2 onfail in Eq
let target_expr (type a) (Expr (t1, e) : anyexpr) (t2 : a typ) : a expr = let Eq = assert_eq_typ t1 t2 "Unexpected type" in e
let target_block (type a) (Block (t1, b) : anyblock) (t2 : a typ) : a block = let Eq = assert_eq_typ t1 t2 "Unexpected type" in b

let of_constant (c : CommonTypes.Constant.t) : anyexpr = let open CommonTypes.Constant in match c with
  | Float f -> Expr (TFloat, EConstFloat f)
  | Int i -> Expr (TInt, EConstInt (Int64.of_int i))
  | Bool b -> Expr (TBool, EConstBool b)
  | String _ -> failwith "TODO: of_constant String"
  | Char _ -> failwith "TODO: of_constant Char"
  | DateTime _ -> failwith "TODO: of_constant DateTime"

let rec of_value (ge : genv) (le: lenv) (v : value) : genv * anyexpr = match v with
  | Constant c -> let Expr (ct, cv) = of_constant c in ge, Expr (ct, cv)
  | Variable v ->
      let loc, VarID (t, vid) = GEnv.find_var ge le v in
      ge, Expr (t, EVariable (loc, (t, vid)))
  | Extend _ -> failwith "TODO: of_value Extend"
  | Project (n, v) -> begin match v with
      | Variable v ->
          if LEnv.is_closure le v then begin
            let VarID (t, i) = LEnv.find_closure le n in
            ge, Expr (t, EVariable (Local StorClosure, (t, i)))
          end else failwith "TODO: of_value Project Variable with non-closure"
      | _ -> ignore (n, ge); failwith "TODO: of_value Project with non-Variable"
      end
  | Erase _ -> failwith "TODO: of_value Erase"
  | Inject _ -> failwith "TODO: of_value Inject"
  | TAbs (_, v)
  | TApp (v, _) -> of_value ge le v
  | XmlNode _ -> failwith "TODO: of_value XmlNode"
  | ApplyPure (f, args) -> begin
      let v =
        let rec inner v = match v with TApp (v, _) | TAbs (_, v) -> inner v | _ -> v
        in inner f
      in match v with
        | Variable v -> begin
            let name = GEnv.get_var_name ge v in match args with
            | [arg] -> begin match Builtins.get_unop name with
              | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin unary operation"))
              | Some (Unop UONegI) ->
                  let ge, arg = of_value ge le arg in let arg = target_expr arg TInt in
                  ge, Expr (TInt, EUnop (UONegI, arg))
              | Some (Unop UONegF) ->
                  let ge, arg = of_value ge le arg in let arg = target_expr arg TFloat in
                  ge, Expr (TFloat, EUnop (UONegF, arg))
              end
            | [arg1; arg2] -> begin match Builtins.get_binop name with
              | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin binary operation"))
              | Some (Binop BOAddI) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                  ge, Expr (TInt, EBinop (BOAddI, arg1, arg2))
              | Some (Binop BOAddF) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                  ge, Expr (TFloat, EBinop (BOAddF, arg1, arg2))
              | Some (Binop BOSubI) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                  ge, Expr (TInt, EBinop (BOSubI, arg1, arg2))
              | Some (Binop BOSubF) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                  ge, Expr (TFloat, EBinop (BOSubF, arg1, arg2))
              | Some (Binop BOMulI) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                  ge, Expr (TInt, EBinop (BOMulI, arg1, arg2))
              | Some (Binop BOMulF) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                  ge, Expr (TFloat, EBinop (BOMulF, arg1, arg2))
              | Some (Binop BODivI) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                  ge, Expr (TInt, EBinop (BODivI, arg1, arg2))
              | Some (Binop BODivF) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                  ge, Expr (TFloat, EBinop (BODivF, arg1, arg2))
              | Some (Binop BORemI) ->
                  let ge, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                  ge, Expr (TInt, EBinop (BORemI, arg1, arg2))
              | Some (Binop BOEq) ->
                  let ge, Expr (t, arg1) = of_value ge le arg1 in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                  ge, Expr (TBool, EBinop (BOEq, arg1, arg2))
              | Some (Binop BONe) ->
                  let ge, Expr (t, arg1) = of_value ge le arg1 in
                  let ge, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                  ge, Expr (TBool, EBinop (BONe, arg1, arg2))
              end
            | _ -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin n-ary operation"))
          end
        | _ -> failwith "TODO: of_value ApplyPure for non-Variable"
    end
  | Closure (f, _, cls) ->
      let FuncID (type a b c) ((TFunc (targs, tret), ctyp, _, _) as fid, fdata : (a, b, c) funcid * _) = GEnv.find_closable_fun ge le f in
      let ge, (cls : c expr_list) = match cls with
        | Extend (vm, None) ->
            let cls = sort_name_map vm |> List.map snd in
            convert_values ge le cls ctyp
        | Extend (_, Some _) -> failwith "TODO: of_value Closure Extend Some"
        | _ -> failwith "TODO: of_value Closure non-Extend" in
      let ge = GEnv.do_export_function ge fdata in
      ge, Expr (TClosedVar (targs, tret), EClose (fid, cls))
  | Coerce (v, _) -> of_value ge le v (* Assume coercion never needs to occur, which is unfortunately unlikely *)

and convert_values : 'a. _ -> _ -> _ -> 'a typ_list -> _ * 'a expr_list =
  fun (type a) (ge : genv) (le : lenv) (vs : value list) (ts : a typ_list) : (genv * a expr_list) ->
  let ge = ref ge in
  let [@tail_mod_cons] rec convert_args : 'a. value list -> 'a typ_list -> 'a expr_list
      = fun (type a) (args : value list) (targs : a typ_list) : a expr_list -> match args, targs with
    | [], TLnil -> ELnil
    | [], TLcons _ -> raise (internal_error "Type mismatch: not enough expressions")
    | _ :: _, TLnil -> raise (internal_error "Type mismatch: too many expressions")
    | ehd :: etl, TLcons (thd, ttl) ->
        let ge', Expr (t, e) = of_value !ge le ehd in
        let Eq = assert_eq_typ t thd "Type mismatch: the argument does not have the correct type" in
        ge := ge'; ELcons (e, convert_args etl ttl)
  in let ret = convert_args vs ts in !ge, ret

let convert_values_unk (ge : genv) (le : lenv) (vs : value list) : (genv * anyexpr_list) =
  let ge = ref ge in
  let rec convert_args (args : value list) : anyexpr_list = match args with
    | [] -> ExprList (TLnil, ELnil)
    | ehd :: etl ->
        let ge', Expr (t, e) = of_value !ge le ehd in
        let ExprList (tl, el) = convert_args etl in
        ge := ge'; ExprList ((TLcons (t, tl)), ELcons (e, el))
  in let ret = convert_args vs in !ge, ret

let allocate_function (ge : genv) (args : binder list) (b : binder) (closure : binder option) : genv * GEnv.funid * mfunid * mtypid * LEnv.t =
  let le_args =
    let rec inner args acc = match args with
      | [] -> acc
      | hd :: tl -> let acc = LEnv.add_arg acc hd in inner tl acc
    in inner args LEnv.no_arg
  in
  let ge, le = LEnv.env_of_args le_args closure (GEnv.add_typ ge) in
  let loc_fid, fid, ftid, ge = GEnv.allocate_function ge le b in
  ge, loc_fid, fid, ftid, le

let rec of_tail_computation (ge : genv) (le: lenv) (tc : tail_computation) : genv * lenv * anyexpr = match tc with
  | Return v -> let ge, v = of_value ge le v in ge, le, v
  | Apply (f, args) -> begin
      match skip_toplevel_polymorphism f with
      | Variable v -> begin match GEnv.find_fun ge le v with
          | ClosedLike (Function ((TFunc (targs, tret), TLnil, _, _) as fid, fdata)) ->
              let ge, args = convert_values ge le args targs in
              let ge = GEnv.do_export_function ge fdata in
              ge, le, Expr (tret, ECallClosed (EClose (fid, ELnil), args, tret))
          | ClosedLike (Closure (loc, ((TClosedVar (targs, tret), _) as vid))) ->
              let ge, args = convert_values ge le args targs in
              ge, le, Expr (tret, ECallClosed (EVariable (loc, vid), args, tret))
        end
      | Closure (f, _, cls) ->
          let FuncID ((TFunc (targs, tret), ctyp, _, _) as fid, fdata) = GEnv.find_closable_fun ge le f in
          let ge, args = convert_values ge le args targs in
          let ge, cls = match cls with
            | Extend (vm, None) ->
                let cls = sort_name_map vm |> List.map snd in
                convert_values ge le cls ctyp
            | Extend (_, Some _) -> failwith "TODO of_tail_computation Apply Closure Extend Some"
            | _ -> failwith "TODO of_tail_computation Apply Closure non-Extend" in
          let ge = GEnv.do_export_function ge fdata in
          ge, le, Expr (tret, ECallClosed (EClose (fid, cls), args, tret))
      | Project (n, v) -> begin match v with
          | Variable v ->
              if LEnv.is_closure le v then begin
                let VarID (t, i) = LEnv.find_closure le n in
                let TypeList targs, Type tret = match t with
                  | TClosedVar (targs, tret) -> TypeList targs, Type tret
                  | _ -> raise (internal_error "Unexpected type, expected a function")
                in
                let ge, args = convert_values ge le args targs in
                ge, le, Expr (tret, ECallClosed (EVariable (Local StorClosure, (TClosedVar (targs, tret), i)), args, tret))
              end else failwith "TODO of_tail_computation Apply Project with non-closure"
          | _ -> failwith "TODO of_tail_computation Apply Project with non-Variable"
        end
      | _ -> failwith "TODO of_tail_computation Apply with non-Variable and non-Closure"
      end
  | Special (DoOperation (tagname, args, tret)) ->
      let ge, ExprList (targs, args) = convert_values_unk ge le args in
      let ge, eid = GEnv.add_effect ge tagname (TypeList targs) tret in
      let Type tret = convert_type tret in
      ge, le, Expr (tret, EDo ((TClosedVar (targs, tret), eid), args))
  | Special (Handle _) -> failwith "TODO of_tail_computation Special Handle"
  | Special _ -> failwith "TODO of_tail_computation Special"
  | Case _ -> failwith "TODO of_tail_computation Case"
  | If (b, t, f) ->
      let ge, Expr (tb, eb) = of_value ge le b in
      let Eq = assert_eq_typ tb TBool "Expected a boolean expression" in
      let ge, le, Block (tt, bt) = of_computation ge le t in
      let ge, le, Block (tf, bf) = of_computation ge le f in
      let Eq = assert_eq_typ tt tf "Expected the same type in both branches" in
      ge, le, Expr (tt, ECond (tt, eb, bt, bf))
and of_computation (ge : genv) (le: lenv) ((bs, tc) : computation) : genv * lenv * anyblock =
  let finish_computation ge fd loc_fid fid ftypid new_le =
    let ge, new_le, b = of_computation ge new_le fd.fn_body in
    let ge, new_le, b =
      let Block (t, (ass, e)) = b in
      match fd.fn_closure with
      | None -> ge, new_le, b
      | Some _ ->
          let new_le, absid, concid, TypeList ctyp = LEnv.add_closure new_le in
          let ge, ctid = GEnv.add_typ ge (Type (TClosArg ctyp)) in
          ge, new_le, Block (t, (Assign (Local StorVariable, (TClosArg ctyp, concid), EConvertClosure (absid, TClosArg ctyp, ctid)) :: ass, e)) in
    let export_name = match fd.fn_closure with None -> Some (Js.name_binder fd.fn_binder) | Some _ -> None in
    let ge, f = LEnv.compile new_le ftypid fid export_name GEnv.add_typ ge b in
    let ge = GEnv.assign_function ge loc_fid f in
    ge in
  let rec inner (ge : genv) (le: lenv) (bs : binding list) (acc : assign list) = match bs with
    | [] ->
        let ge, le, Expr (t, e) = of_tail_computation ge le tc in
        ge, le, Block (t, (List.rev acc, e))
    | Let (b, (_, tc)) :: bs ->
        let ge, le, Expr (t, e) = of_tail_computation ge le tc in
        let ge, le, loc, v = GEnv.add_var ge le b (Type t) in
        let v : _ varid = t, v in
        let a = Assign (loc, v, e) in
        inner ge le bs (a :: acc)
    | Fun fd :: bs ->
        let ge, loc_fid, fid, ftypid, new_le = allocate_function ge fd.fn_params fd.fn_binder fd.fn_closure in
        let ge = finish_computation ge fd loc_fid fid ftypid new_le in
        inner ge le bs acc
    | Rec fds :: bs ->
        let rec compute ge fds = match fds with
          | [] -> inner ge le bs acc
          | (fd, loc_fid, fid, ftypid, new_le) :: fds ->
              let ge = finish_computation ge fd loc_fid fid ftypid new_le in
              compute ge fds
        in let rec allocate ge fds acc' = match fds with
          | [] -> compute ge acc'
          | fd :: fds ->
              let ge, loc_fid, fid, ftypid, new_le = allocate_function ge fd.fn_params fd.fn_binder fd.fn_closure in
              allocate ge fds ((fd, loc_fid, fid, ftypid, new_le) :: acc')
        in allocate ge fds []
    | Alien _ :: bs -> ignore bs; failwith "TODO of_computation Alien"
    | Module _ :: bs -> ignore bs; failwith "TODO of_computation Module"
  in inner ge le bs []

let find_global_binders ((bs, _) : computation) =
  let rec inner bs acc = match bs with
    | [] -> Utility.StringMap.fold (fun _ b acc -> Utility.IntSet.add b acc) acc Utility.IntSet.empty
    | Let (b, _) :: bs ->
        let name = Var.name_of_binder b in inner bs (if name = "" then acc else Utility.StringMap.add name (Var.var_of_binder b) acc)
    | Fun _ :: bs | Rec _ :: bs | Alien _ :: bs | Module _ :: bs -> inner bs acc
  in inner bs Utility.StringMap.empty

let module_of_ir (c : computation) (map : string Env.Int.t) : modu =
  let ge = GEnv.empty map (find_global_binders c) in
  let ge, le, blk = of_computation ge LEnv.toplevel c in
  GEnv.compile ge le blk
