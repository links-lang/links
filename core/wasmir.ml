open Ir

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
  | TInt : int typ
  | TBool : bool typ
  | TFloat : float typ
  | TFunc : 'a typ_list * 'b typ -> ('a, 'b) functyp typ
  | TClosed : 'a typ_list * 'b typ -> ('a -> 'b) typ
  | TAbsClosArg : abs_closure_content typ
  | TClosArg : 'a typ_list -> 'a closure_content typ
  | TCont : 'a typ -> 'a continuation typ
  | TTuple : 'a typ_list -> 'a list typ
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : ('a typ * 'b typ_list) -> ('a * 'b) typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list

let rec compare_anytyp (Type t1) (Type t2) = match t1, t2 with
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
  | TClosed (args1, ret1), TClosed (args2, ret2) ->
      let cret = compare_anytyp (Type ret1) (Type ret2) in if cret = 0 then compare_anytyp_list (TypeList args1) (TypeList args2) else cret
  | TClosed _, _ -> ~-1
  | _, TClosed _ -> 1
  | TAbsClosArg, TAbsClosArg -> 0
  | TAbsClosArg, _ -> ~-1
  | _, TAbsClosArg -> 1
  | TClosArg t1, TClosArg t2 -> compare_anytyp_list (TypeList t1) (TypeList t2)
  | TClosArg _, _ -> ~-1
  | _, TClosArg _ -> 1
  | TCont t1, TCont t2 -> compare_anytyp (Type t1) (Type t2)
  | TCont _, _ -> ~-1
  | _, TCont _ -> 1
  | TTuple tl1, TTuple tl2 -> compare_anytyp_list (TypeList tl1) (TypeList tl2)
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
type ('a, 'b, 'c) closed_like =
  | Function of ('a, 'b, unit) funcid * 'c
  | Closure of locality * ('a -> 'b) varid
  (* FIXME: see ECont
  | Contin : locality * 'd continuation varid * ('d continuation * ('a closure_content * unit), 'b, 'e) funcid *
             locality * 'e closure_content varid -> ('a, 'b, 'c) closed_like *)
  | Contin : locality * 'd continuation varid * ('d continuation * ('a list * unit), 'b, unit) funcid *
             locality * mvarid -> ('a, 'b, 'c) closed_like
type ('a, 'b) effectid = ('a -> 'b) typ * meffid

type anyvarid = VarID : 'a varid -> anyvarid
type 'a anyfuncid = FuncID : ('b, 'c, 'd) funcid * 'a -> 'a anyfuncid
type 'a anycfuncid = ACFunction : ('b, 'c, unit) funcid * 'a -> 'a anycfuncid
type 'a anyclike =
  | ClosedLike : ('b, 'c, 'a) closed_like -> 'a anyclike
  | CLBuiltin of string

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list
type anyvarid_list = VarIDList : 'a typ_list * 'a varid_list -> anyvarid_list

type ('a, 'b) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap of 'a varid * 'b typ * 'b block
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
  | EConvertClosure : mvarid * 'a closure_content typ * mtypid -> 'a closure_content expr
  | EIgnore : 'a typ * 'a expr -> unit list expr
  | EConstUnit : unit list expr
  | EConstInt : int64 -> int expr
  | EConstBool : bool -> bool expr
  | EConstFloat : float -> float expr
  | EUnop : ('a, 'b) unop * 'a expr -> 'b expr
  | EBinop : ('a, 'b, 'c) binop * 'a expr * 'b expr -> 'c expr
  | EVariable : locality * 'a varid -> 'a expr
  | EClose : ('a, 'b, 'c) funcid * 'c expr_list -> ('a -> 'b) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ_list * 'b expr_list * abs_closure_content expr * 'd typ -> 'd expr
  | ECallClosed : ('a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : 'a typ * bool expr * 'a block * 'a block -> 'a expr
  | EDo : ('a, 'b) effectid * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'a, 'c) funcid * 'c expr_list * ('a, 'b) finisher * ('a, 'b) handler list -> 'b expr
  | EDeepHandle : (unit, 'b, 'c) funcid * 'c expr_list * ('b continuation * ('c closure_content * unit), 'd, 'e) funcid * 'e expr_list -> 'd expr
  (* FIXME: cannot use the following definition, as we don't know what 'e should be when building this expression
     However, the funcid contain the correct mtypid.
  | ECont : locality * 'b continuation varid * 'a expr_list *
            ('b continuation * ('a closure_content * unit), 'd, 'e) funcid * locality * 'e closure_content varid -> 'd expr *)
  | ECont : locality * 'b continuation varid * 'a expr_list *
            ('b continuation * ('a list * unit), 'd, unit) funcid * locality * mvarid -> 'd expr
and ('a, 'b) handler = (* The continuation itself returns 'a, the handler returns 'b *)
  (* Note: we lose the information that the continuation takes 'b as parameter(s) *)
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and 'a expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

let typ_of_expr (type a) (e : a expr) : a typ = match e with
  | EConvertClosure (_, t, _) -> t
  | EIgnore _ -> TTuple TLnil
  | EConstUnit -> TTuple TLnil
  | EConstInt _ -> TInt
  | EConstBool _ -> TBool
  | EConstFloat _ -> TFloat
  | EUnop (UONegI, _) -> TInt
  | EUnop (UONegF, _) -> TFloat
  | EBinop (BOAddI, _, _) -> TInt | EBinop (BOAddF, _, _) -> TFloat
  | EBinop (BOSubI, _, _) -> TInt | EBinop (BOSubF, _, _) -> TFloat
  | EBinop (BOMulI, _, _) -> TInt | EBinop (BOMulF, _, _) -> TFloat
  | EBinop (BODivI, _, _) -> TInt | EBinop (BODivF, _, _) -> TFloat
  | EBinop (BORemI, _, _) -> TInt
  | EBinop (BOEq, _, _) -> TBool
  | EBinop (BONe, _, _) -> TBool
  | EVariable (_, (t, _)) -> t
  | EClose ((TFunc (args, ret), _, _, _), _) -> TClosed (args, ret)
  | ECallRawHandler (_, _, _, _, _, _, t) -> t
  | ECallClosed (_, _, t) -> t
  | ECond (t, _, _, _) -> t
  | EDo ((TClosed (_, t), _), _) -> t
  | EShallowHandle (_, _, FId t, _) -> t
  | EShallowHandle (_, _, FMap (_, t, _), _) -> t
  | EDeepHandle (_, _, (TFunc (_, t), _, _, _), _) -> t
  | ECont (_, _, _, (TFunc (_, t), _, _, _), _, _) -> t
(* val typ_of_expr_list : 'a expr_list -> 'a typ_list *)
(* val typ_of_varid_list : 'a varid_list -> 'a typ_list *)

type anyblock = Block : 'a typ * 'a block -> anyblock
type anyexpr = Expr : 'a typ * 'a expr -> anyexpr
type anyexpr_list = ExprList : 'a typ_list * 'a expr_list -> anyexpr_list

type func' = {
  fun_typ : mtypid;
  fun_id : mfunid;
  fun_export_data : string option;
  fun_closure : anytyp_list * mtypid; fun_converted_closure : mvarid option;
  fun_args : anytyp_list;
  fun_locals: anytyp_list;
  fun_block : anyblock;
}
type ('a, 'b, 'c) fhandler = {
  fh_contarg : 'a continuation varid * mvarid;
  fh_closure : (mvarid * 'c closure_content varid) option;
  fh_locals : anytyp_list;
  fh_finisher : ('a, 'b) finisher;
  fh_handlers : ('a, 'b) handler list;
}
type func = FFunction of func' | FHandler : ('a, 'b, 'c) fhandler -> func
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

type anyhandler = AHandler : ('a, 'b, 'c) fhandler -> anyhandler

let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

type ('a, 'b) eq = Eq : 'a. ('a, 'a) eq
let rec assert_eq_typ : 'a 'b. 'a typ -> 'b typ -> string -> ('a, 'b) eq =
  fun (type a b) (t1 : a typ) (t2 : b typ) (onfail : string) : (a, b) eq -> match t1, t2 with
  | TInt, TInt -> Eq
  | TInt, _ | _, TInt -> raise (internal_error onfail)
  | TBool, TBool -> Eq
  | TBool, _ | _, TBool -> raise (internal_error onfail)
  | TFloat, TFloat -> Eq
  | TFloat, _ | _, TFloat -> raise (internal_error onfail)
  | TFunc (tl1, r1), TFunc (tl2, r2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Eq
  | TFunc _, _ | _, TFunc _ -> raise (internal_error onfail)
  | TClosed (tl1, r1), TClosed (tl2, r2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Eq
  | TClosed _, _ | _, TClosed _ -> raise (internal_error onfail)
  | TAbsClosArg, TAbsClosArg -> Eq
  | TAbsClosArg, _ | _, TAbsClosArg -> raise (internal_error onfail)
  | TClosArg t1, TClosArg t2 -> let Eq = assert_eq_typ_list t1 t2 onfail in Eq
  | TClosArg _, _ | _, TClosArg _ -> raise (internal_error onfail)
  | TCont t1, TCont t2 -> let Eq = assert_eq_typ t1 t2 onfail in Eq
  | TCont _, _ | _, TCont _ -> raise (internal_error onfail)
  | TTuple tl1, TTuple tl2 -> let Eq = assert_eq_typ_list tl1 tl2 onfail in Eq
and assert_eq_typ_list : 'a 'b. 'a typ_list -> 'b typ_list -> string -> ('a, 'b) eq =
  fun (type a b) (t1 : a typ_list) (t2 : b typ_list) (onfail : string) : (a, b) eq -> match t1, t2 with
  | TLnil, TLnil -> Eq
  | TLcons _, TLnil | TLnil, TLcons _ -> raise (internal_error onfail)
  | TLcons (t1, tl1), TLcons (t2, tl2) -> let Eq, Eq = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ t1 t2 onfail in Eq
let target_expr (type a) (Expr (t1, e) : anyexpr) (t2 : a typ) : a expr = let Eq = assert_eq_typ t1 t2 "Unexpected type" in e
let target_block (type a) (Block (t1, b) : anyblock) (t2 : a typ) : a block = let Eq = assert_eq_typ t1 t2 "Unexpected type" in b

module Builtins : sig
  val get_unop : string -> anyunop option
  val get_binop : string -> anybinop option
  val gen_impure : string -> anyexpr_list -> anyexpr
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
  
  let gen_impure op (ExprList (targs, args)) : anyexpr = match op with
    | "ignore" -> begin
        match targs, args with
        | TLcons (targ, TLnil), ELcons (arg, ELnil) -> Expr (TTuple TLnil, EIgnore (targ, arg))
        | _, ELnil -> raise (internal_error ("Not enough arguments for builtin function 'ignore'"))
        | _, ELcons (_, ELcons _) -> raise (internal_error ("Too many arguments for builtin function 'ignore'"))
      end
    | _ -> raise (internal_error ("Unknown builtin impure function" ^ op))
end

let sort_name_map (nm : 'a Ir.name_map) : (string * 'a) list =
  Utility.StringMap.bindings nm (* Since binding names are unique, there is no issue with the given list as it is sorted *)

let rec skip_toplevel_polymorphism v = match v with
	| TAbs (_, v) | TApp (v, _) -> skip_toplevel_polymorphism v
	| _ -> v
let rec _convert_type (t : Types.typ)
    (normal : anytyp -> 'a)
    (func : Types.typ -> Types.typ -> Types.typ -> 'a)
    (row : Types.field_spec_map -> Types.meta_row_var -> bool -> 'a)
    (any : Types.tid -> CommonTypes.Kind.t -> CommonTypes.Freedom.t -> 'a) : 'a = match t with
  | Types.Not_typed -> failwith "TODO _convert_type Not_typed"
  | Types.Var (id, k, f) -> any id k f
  | Types.Recursive _ -> failwith "TODO _convert_type Recursive"
  | Types.Alias _ -> failwith "TODO _convert_type Alias"
  | Types.Application _ -> failwith "TODO _convert_type Application"
  | Types.RecursiveApplication _ -> failwith "TODO _convert_type RecursiveApplication"
  | Types.Meta t -> _convert_type (Unionfind.find t) normal func row any
  | Types.Primitive CommonTypes.Primitive.Bool -> normal (Type TBool)
  | Types.Primitive CommonTypes.Primitive.Int -> normal (Type TInt)
  | Types.Primitive CommonTypes.Primitive.Float -> normal (Type TFloat)
  | Types.Primitive _ -> failwith "TODO _convert_type Primitive"
  | Types.Function (args, eff, ret) -> func args eff ret
  | Types.Lolli (args, eff, ret) -> func args eff ret (* Assume Lolli and Function are the same thing *)
  | Types.Record t -> _convert_type t normal func row any
  | Types.Variant _ -> failwith "TODO _convert_type Variant"
  | Types.Table _ -> failwith "TODO _convert_type Table"
  | Types.Lens _ -> failwith "TODO _convert_type Lens"
  | Types.ForAll (_, t) -> _convert_type t normal func row any (* Ignore polymorphism for now *)
  | Types.Effect _ -> failwith "TODO _convert_type Effect"
  | Types.Operation _ -> failwith "TODO _convert_type Operation"
  | Types.Row (fsm, mrv, b) -> row fsm mrv b
  | Types.Closed -> failwith "TODO _convert_type Closed"
  | Types.Absent -> failwith "TODO _convert_type Absent"
  | Types.Present t -> _convert_type t normal func row any
  | Types.Input _ -> failwith "TODO _convert_type Input"
  | Types.Output _ -> failwith "TODO _convert_type Output"
  | Types.Select _ -> failwith "TODO _convert_type Select"
  | Types.Choice _ -> failwith "TODO _convert_type Choice"
  | Types.Dual _ -> failwith "TODO _convert_type Dual"
  | Types.End -> failwith "TODO _convert_type End"
let _to_typelist (conv : Types.typ -> anytyp) (ts : Types.typ Ir.name_map) : anytyp_list =
  let rec inner l = match l with
    | [] -> TypeList TLnil
    | (_, hd) :: tl -> let Type hd = conv hd in let TypeList tl = inner tl in TypeList (TLcons (hd, tl))
  in inner (sort_name_map ts)
let rec convert_type (t : Types.typ) : anytyp =
  _convert_type t
    (fun t -> t)
    (fun args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        Type (TClosed (args, ret)))
    (fun fsm _ _ -> let TypeList tl = _to_typelist convert_type fsm in Type (TTuple tl))
    (fun _ _ _ -> Type (TTuple TLnil)) (* TODO check if this is correct; this is used for eg never-called continuations *)
and convert_type_list (t : Types.typ) : anytyp_list =
  _convert_type t
    (fun (Type t) -> TypeList (TLcons (t, TLnil)))
    (fun args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        TypeList (TLcons (TClosed (args, ret), TLnil)))
    (fun fsm _ _ -> _to_typelist convert_type fsm)
    (fun _ _ _ -> failwith "TODO convert_type_list Var")
let convert_type_function_ret (t : Types.typ) : anytyp =
  _convert_type t
    (fun _ -> raise (internal_error "Expected a function type, got another type"))
    (fun _ _ ret -> convert_type ret)
    (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type"))
    (fun _ _ _ -> failwith "TODO convert_type_function_ret Var")
let convert_type_function_args (t : Types.typ) : anytyp_list =
  _convert_type t
    (fun _ -> raise (internal_error "Expected a function type, got another type"))
    (fun args _ _ -> convert_type_list args)
    (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type"))
    (fun _ _ _ -> failwith "TODO convert_type_function_args Var")

module LEnv : sig (* Contains the arguments, the local variables, etc *)
  type t
  type realt
  type subt
  val toplevel : realt
  val create_sub : t -> bool -> subt
  
  val of_real : realt -> t
  val of_sub : subt -> t
  val to_real : t -> realt
  val to_sub : t -> subt
  
  val get_closure : subt -> subt * (mvarid * mvarid) option * anyexpr_list
  
  (* Internal use by the global env only *)
  val args_typ : realt -> anytyp_list * (anytyp_list * mtypid)
  val find_continuation :
    t -> var -> (t * local_storage * anytyp_list * anyvarid * mfunid * anytyp * mtypid * local_storage * mvarid) option
  
  type args
  val no_arg : args
  val add_arg : args -> binder -> args
  val env_of_args : args -> binder option -> (anytyp -> 'a * mtypid) -> 'a * realt (* closure binder is true if this is for the init function *)
  val add_closure : realt -> realt * mvarid * mvarid * anytyp_list
  
  val add_var : t -> binder -> anytyp -> t * mvarid
  val find_var : t -> var -> (t * local_storage * anyvarid) option
  val find_closure : t -> var -> string -> (t * local_storage * anyvarid) option
  
  val set_handler_args : subt -> binder -> subt * anyvarid_list
  val add_cont : subt -> anytyp -> mfunid -> anytyp -> mtypid -> subt * mvarid
  val set_continuation : subt -> binder -> anytyp_list -> subt
  
  val locals_of_env : realt -> anytyp_list
  val compile : realt -> mtypid -> mfunid -> string option -> anyblock -> func'
  val compile_sub : subt -> mtypid -> mfunid -> anyblock -> anytyp_list -> mtypid -> mvarid option -> func'
  val compile_handler : subt -> (meffid * meffid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> anyhandler
end = struct
  module IntString = Env.Make(struct
    type t = int * string
    let pp fmt (i, n) = Format.fprintf fmt "(%d, '%s')" i n
    let show (i, n) = "(" ^ (string_of_int i) ^ ", '" ^ n ^ "')"
    let compare (i1, n1) (i2, n2) =
      let c = Int.compare i1 i2 in if c <> 0 then c else String.compare n1 n2
  end)
  
  type realt = {
    nargs : int32;
    args : anytyp_list;
    nlocs : int32;
    locs : anytyp_list;
    varmap : anyvarid Env.Int.t;
    cvarmap : anyvarid Env.String.t;
    clos : mvarid * (anytyp_list * mtypid) * bool; (* true if closure has been converted *)
    cbid : var option;
  }
  type subt = {
    base: t;
    nargs : int32;
    nlocs : int32;
    locs : anytyp_list;
    varmap : (local_storage * anyvarid) Env.Int.t;
    contmap : (anytyp_list * anyvarid * mfunid * anytyp * mtypid * mvarid) Env.Int.t;
    nclos : int32;
    clos : anyexpr_list;
    cvarmap : (local_storage * anyvarid) IntString.t;
    mutable contid : (anyvarid * mvarid * mfunid * anytyp * mtypid * mvarid) option;
    mutable contv : anytyp_list * var;
    mutable hdlb : var;
  }
  and t = (realt, subt) Either.t
  
  let toplevel : realt = {
    nargs = 0l;
    args = TypeList TLnil;
    nlocs = 0l;
    locs = TypeList TLnil;
    varmap = Env.Int.empty;
    cvarmap = Env.String.empty;
    clos = Int32.minus_one, (TypeList TLnil, ~-1), true;
    cbid = None;
  }
  let create_sub (penv : t) (is_handler : bool) : subt = {
      base = penv;
      nargs = if is_handler then 3l else 1l;
      nlocs = 0l;
      locs = TypeList TLnil;
      varmap = Env.Int.empty;
      contmap = Env.Int.empty;
      nclos = 0l;
      clos = ExprList (TLnil, ELnil);
      cvarmap = IntString.empty;
      contid = None;
      contv = (TypeList TLnil, ~-1);
      hdlb = ~-1;
    }
  
  let of_real (env : realt) : t = Either.Left env
  let of_sub (env : subt) : t = Either.Right env
  let to_real (env : t) : realt = match env with Either.Left env -> env | Either.Right _ -> raise (internal_error "Invalid local environment kind")
  let to_sub (env : t) : subt = match env with Either.Right env -> env | Either.Left _ -> raise (internal_error "Invalid local environment kind")
  
  (* Similar to rev_append args tl, but works on anytyp_lists *)
  let extract_args (TypeList args) (TypeList tl) : anytyp_list =
    let rec inner : 'a 'b. 'a typ_list -> 'b typ_list -> anytyp_list =
      fun (type a b) (args : a typ_list) (acc : b typ_list) : anytyp_list -> match args with
      | TLnil -> TypeList acc
      | TLcons (hd, tl) -> inner tl (TLcons (hd, acc))
    in inner args tl
  let extract_exprs (ExprList (typs, exprs)) : anyexpr_list =
    let rec inner : 'a 'b. 'a typ_list -> 'a expr_list -> 'b typ_list -> 'b expr_list -> anyexpr_list =
      fun (type a b) (typs : a typ_list) (exprs : a expr_list) (tacc : b typ_list) (eacc : b expr_list) : anyexpr_list -> match typs, exprs with
      | TLnil, ELnil -> ExprList (tacc, eacc)
      | TLcons (thd, ttl), ELcons (ehd, etl) -> inner ttl etl (TLcons (thd, tacc)) (ELcons (ehd, eacc))
    in inner typs exprs TLnil ELnil
  
  let get_closure (env : subt) : subt * (mvarid * mvarid) option * anyexpr_list =
    let cexpr = extract_exprs env.clos in
    let oconv, nlocs, locs = match cexpr with
      | ExprList (TLnil, ELnil) -> None, env.nlocs, env.locs
      | ExprList (cts, _) ->
        Some (Int32.sub env.nargs 1l, Int32.add env.nargs env.nlocs),
          Int32.succ env.nlocs,
          (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cts, tl)))
    in { env with nlocs; locs }, oconv, cexpr
  
  let args_typ (env : realt) : anytyp_list * (anytyp_list * mtypid) =
    env.args, (let (_, cat, _) = env.clos in cat)
  let rec find_continuation (env : t) (v : var)
      : (t * local_storage * anytyp_list * anyvarid * mfunid * anytyp * mtypid * local_storage * mvarid) option =
    match env with
    | Either.Left _ -> None
    | Either.Right ({ base; contid; contv = targs, contv; contmap; _ } as env) -> begin match Env.Int.find_opt v contmap with
        | Some (targs, VarID (t, cid), hdlfid, thdlret, thdlid, hdlcid) ->
            Some (Either.Right env, StorClosure, targs, VarID (t, cid), hdlfid, thdlret, thdlid, StorClosure, hdlcid)
        | None -> begin match
              match contid with None -> None | Some (VarID (t, cid), _, hdlfid, thdlret, thdlid, hdlcid) ->
                if Var.equal_var v contv then
                  Some (Either.Right env, StorVariable, targs, VarID (t, cid),
                        hdlfid, thdlret, thdlid, StorVariable, hdlcid)
                else None
              with Some v -> Some v | None ->
            match find_continuation base v with
            | None -> None
            | Some (base, loc, targs, VarID (t, bid), hdlfid, thdlret, thdlid, hdlcloc, hdlbcid) ->
                let cid = env.nclos in
                let hdlcid = Int32.succ cid in
                let nclos = Int32.succ hdlcid in
                let clos =
                  let ExprList (tl, es) = env.clos in
                  ExprList (TLcons (TClosArg TLnil, TLcons (TCont t, tl)),
                            ELcons (EVariable (Local hdlcloc, (TClosArg TLnil, hdlbcid)), ELcons (EVariable (Local loc, (TCont t, bid)), es))) in
                let contmap = Env.Int.bind v (targs, VarID (t, cid), hdlfid, thdlret, thdlid, hdlcid) env.contmap in
                Some (Either.Right { env with base; nclos; clos; contmap }, StorClosure, targs, VarID (t, cid),
                      hdlfid, thdlret, thdlid, StorClosure, hdlcid)
          end
        end
  
  type args = int32 * anytyp_list * anyvarid Env.Int.t
  
  let no_arg : args = 0l, TypeList TLnil, Env.Int.empty
  
  let add_arg (nargs, TypeList args, map : args) (argbind : binder) : args =
    let Type argt = convert_type (Var.type_of_binder argbind) in
    Int32.succ nargs,
      TypeList (TLcons (argt, args)),
      Env.Int.bind (Var.var_of_binder argbind) (VarID (argt, nargs)) map
  
  let env_of_args (nargs, args, varmap : args) (closure : binder option) (add_typ : anytyp -> 'a * mtypid) : 'a * realt =
    let closid = nargs in
    let rec add_all_clos varmap (TypeList acc) i ts = match ts with
      | [] -> varmap, extract_args (TypeList acc) (TypeList TLnil)
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
            (fun fsm _ _ -> sort_name_map fsm)
            (fun _ _ _ -> raise (internal_error "Expected a row type, got any type")) in (* TODO: replace by []? *)
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
  
  let add_closure (env : realt) : realt * mvarid * mvarid * anytyp_list =
    let acid, (TypeList cat, ctid) = match env.clos with
      | _, _, true -> raise (internal_error "Double add_closure call")
      | acid, cat, false -> acid, cat
    in let ccid = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cat, tl)));
      clos = ccid, (TypeList cat, ctid), true;
    }, acid, ccid, TypeList cat
  
  let add_sub_to_env (env : subt) (base : t) (loc : local_storage) (VarID (t, bid) : anyvarid) : subt * anyvarid =
    let cid = env.nclos in
    let hdlcid = Int32.succ cid in
    let nclos = Int32.succ hdlcid in
    let clos =
      let ExprList (tl, es) = env.clos in
      ExprList (TLcons (t, tl),
                ELcons (EVariable (Local loc, (t, bid)), es)) in
    { env with base; nclos; clos }, VarID (t, cid)
  
  let add_var (env : t) (b : binder) (Type t : anytyp) : t * mvarid = match env with
    | Either.Left env ->
        let vidx = Int32.add env.nargs env.nlocs in
        Either.Left { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
          varmap = Env.Int.bind (Var.var_of_binder b) (VarID (t, vidx)) env.varmap;
        }, vidx
    | Either.Right env ->
        let vidx = Int32.add env.nargs env.nlocs in
        Either.Right { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
          varmap = Env.Int.bind (Var.var_of_binder b) (StorVariable, VarID (t, vidx)) env.varmap;
        }, vidx
  let rec find_var (env : t) (v : var) : (t * local_storage * anyvarid) option = match env with
    | Either.Left env' -> Option.map (fun i -> env, StorVariable, i) (Env.Int.find_opt v env'.varmap)
    | Either.Right env' -> begin match Env.Int.find_opt v env'.varmap with
        | Some (loc, i) -> Some (env, loc, i)
        | None -> begin match find_var env'.base v with
            | Some (base, loc, bid) ->
                let env, cid = add_sub_to_env env' base loc bid in
                let varmap = Env.Int.bind v (StorClosure, cid) env.varmap in
                Some (Either.Right { env with varmap }, StorClosure, cid)
            | None -> None
          end
      end
  let rec find_closure (env : t) (v : var) (n : string) : (t * local_storage * anyvarid) option = match env with
    | Either.Left env -> begin match env.cbid with
        | None -> None
        | Some cbid -> if Int.equal cbid v then Some (Either.Left env, StorClosure, Env.String.find n env.cvarmap) else None
      end
    | Either.Right env -> begin match IntString.find_opt (v, n) env.cvarmap with
        | Some (lst, ret) -> Some (Either.Right env, lst, ret)
        | None -> begin match find_closure env.base v n with
            | Some (base, lst, bid) ->
                let env, cid = add_sub_to_env env base lst bid in
                let cvarmap = IntString.bind (v, n) (lst, cid) env.cvarmap in
                Some (Either.Right { env with cvarmap }, lst, cid)
            | None -> None
          end
      end
  
  let add_freevar (env : subt) (Type t : anytyp) : subt * mvarid =
    let vidx = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
    }, vidx
  let set_handler_args (env : subt) (b : binder) : subt * anyvarid_list =
    let TypeList (type v) (eargs : v typ_list) = convert_type_list (Var.type_of_binder b) in
    match eargs with
    | TLcons (t, TLnil) ->
        let env, varid = add_var (of_sub env) b (Type t) in
        to_sub env, VarIDList (eargs, VLcons ((t, varid), VLnil))
    | _ ->
        let argsb = Var.var_of_binder b in
        let env, (vargs : v varid_list) =
          let env = ref env in
          let [@tail_mod_cons] rec inner : 'a. 'a typ_list -> _ -> 'a varid_list =
            fun (type v) (a : v typ_list) (i : int) : v varid_list -> match a with
            | TLnil -> VLnil
            | TLcons (thd, ttl) ->
                let i = i + 1 in
                let env', vid = add_freevar !env (Type thd) in
                let cvarmap = IntString.bind (argsb, string_of_int i) (StorVariable, VarID (thd, vid)) env'.cvarmap in
                env := { env' with cvarmap }; VLcons ((thd, vid), inner ttl i)
          in let ret = inner eargs 0 in !env, ret in
        env, VarIDList (eargs, vargs)
  (* Modifies in-place the environment *)
  let add_cont (env : subt) (Type tcontret : anytyp) (hdlfid : mfunid) (thdlret : anytyp) (thdlclid : mtypid) : subt * mvarid =
    match env.contid with
    | Some _ -> raise (internal_error "Cannot add multiple continuations")
    | None ->
        let env, id = env, 0l in   (* Continuation argument *)
        let env, ccid = env, 1l in (* Argument of the continuation argument *)
        let env, hcid = env, 2l in (* Closure of the current handler function *)
        env.contid <- Some (VarID (tcontret, id), ccid, hdlfid, thdlret, thdlclid, hcid);
        env, id
  let set_continuation (env : subt) (b : binder) (targs : anytyp_list) : subt = match env.contid with
    | None -> raise (internal_error "Cannot set missing continuation")
    | Some _ -> env.contv <- (targs, Var.var_of_binder b); env
  
  let locals_of_env (env : realt) : anytyp_list = extract_args env.locs (TypeList TLnil)
  
  let compile (env : realt) (ftid : mtypid) (fid : mfunid) (export_name : string option) (b : anyblock) : func' =
    let closid, clostyp, has_converted_closure = env.clos in
    let export_data = match export_name with
      | Some name -> Some name
      | None -> None
    in let convclos = if has_converted_closure then Some closid else None
    in {
      fun_typ = ftid;
      fun_id = fid;
      fun_export_data = export_data;
      fun_closure = clostyp; fun_converted_closure = convclos;
      fun_args = env.args;
      fun_locals = extract_args env.locs (TypeList TLnil);
      fun_block = b;
    }
  
  let compile_sub (env : subt) (ftid : mtypid) (fid : mfunid) (b : anyblock) (ctype : anytyp_list) (ctypeid : mtypid) (cclosid : mvarid option) : func' =
    {
      fun_typ = ftid;
      fun_id = fid;
      fun_export_data = None;
      fun_closure = (ctype, ctypeid); fun_converted_closure = cclosid;
      fun_args = TypeList TLnil;
      fun_locals = extract_args env.locs (TypeList TLnil);
      fun_block = b;
    }
  
  let compile_handler (type b d) (env : subt) (oabsconc : (meffid * meffid) option)
                      (onret : (b, d) finisher) (ondo : (b, d) handler list) : anyhandler =
    let contarg, argcontidx = match env.contid with
      | Some (VarID (_, i), j, _, _, _, _) -> i, j
      | None -> raise (internal_error "Handlers must have a continuation added") in
    let tcont = match onret with FId t | FMap ((t, _), _, _) -> t in
    let ExprList (clostyp, _) = env.clos in AHandler {
      fh_contarg  = (TCont tcont, contarg), argcontidx;
      fh_closure  = Option.map (fun (absid, concid) -> absid, (TClosArg clostyp, concid)) oabsconc;
      fh_locals   = extract_args env.locs (TypeList TLnil);
      fh_finisher = onret;
      fh_handlers = ondo;
    }
end

module GEnv : sig (* Contains the functions, the types, etc *)
  type t
  type funid
  type hid
  val empty : string Env.Int.t -> Utility.IntSet.t -> t
  
  val add_typ : t -> anytyp -> t * mtypid
  val type_nilclosure : mtypid
  
  type typid
  val new_generic : t -> t * typid * mtypid
  val set_generic : t -> typid -> anytyp -> t
  
  val get_var_name : t -> var -> string
  val find_fun : t -> LEnv.t -> var -> LEnv.t * funid anyclike
  val find_closable_fun : t -> LEnv.t -> var -> funid anyfuncid
  
  val add_var : t -> LEnv.t -> binder -> anytyp -> t * LEnv.t * locality * mvarid
  val find_var : t -> LEnv.t -> var -> (LEnv.t * locality * anyvarid, funid anycfuncid) Either.t option
  
  val allocate_function : t -> LEnv.realt -> binder -> funid * mfunid * mtypid * t
  val assign_function : t -> funid -> func' -> t
  val do_export_function : t -> funid -> t
  
  val new_continuator : t -> mfunid -> anytyp -> anytyp_list -> anytyp -> t * mfunid
  val new_function : t -> LEnv.subt -> anytyp_list -> anyblock -> mvarid option -> t * mtypid * mfunid * funid
  val allocate_fhandler : t -> hid * mfunid * t
  val assign_fhandler : t -> hid -> LEnv.subt -> (meffid * meffid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> t
  
  val add_effect : t -> string -> anytyp_list -> t * meffid
  
  val compile : t -> LEnv.realt -> anyblock -> modu
end = struct
  (* In-place function emplacement in the environment *)
  type funid = func' option ref * bool ref
  type hid = anyhandler option ref
  
  module EffectMap = Utility.Map.Make(struct
    type t = string * anytyp_list
    let pp _ = failwith "TODO Wasmir.GEnv.EffectMap pp"
    let show _ = failwith "TODO Wasmir.GEnv.EffectMap show"
    let compare (n1, ea1) (n2, ea2) =
      let c = String.compare n1 n2 in if c <> 0 then c else
      compare_anytyp_list ea1 ea2
  end)
  
  type t = {
    ge_map : string Env.Int.t;
    ge_nfuns : mfunid;
    ge_funs : (func' option ref * mfunid * bool ref, (anyhandler option ref, func' * mfunid) Either.t) Either.t list;
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
    let tmap = [Type (TClosArg TLnil), 0] in {
      ge_map = m;
      ge_nfuns = 0l;
      ge_funs = [];
      ge_ntyps = List.length tmap;
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
  let type_nilclosure : mtypid = 0
  
  let add_typ (env : t) (typ : anytyp) : t * mtypid = match TypeMap.find_opt typ env.ge_typemap with
    | Some idx -> env, idx
    | None ->
        let typid = env.ge_ntyps in
        let ge_ntyps = Int.succ typid in
        let ge_typs = typ :: env.ge_typs in
        let ge_typemap = TypeMap.add typ typid env.ge_typemap in
      { env with ge_ntyps; ge_typs; ge_typemap }, typid
  
  type typid = int
  let new_generic (env : t) : t * typid * mtypid =
    let typid = env.ge_ntyps in
    let ge_ntyps = Int.succ typid in
    let ge_typs = Type TInt :: env.ge_typs in
    { env with ge_ntyps; ge_typs }, typid, typid
  let set_generic (env : t) (typid : typid) (typ : anytyp) : t =
    let ge_typs =
      let rec inner i typs acc = match i, typs with
        | _, [] -> raise (internal_error "Invalid typid in Wasmir.GEnv.set_generic")
        | 0, _ :: tl -> List.rev_append acc (typ :: tl)
        | _, hd :: tl -> inner (i - 1) tl (hd :: acc)
      in inner (env.ge_ntyps - typid - 1) env.ge_typs [] in
    let ge_typemap = TypeMap.add typ typid env.ge_typemap in
    { env with ge_typs; ge_typemap }
  
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
  let find_var (ge : t) (le : LEnv.t) (v : var) : (LEnv.t * locality * anyvarid, funid anycfuncid) Either.t option = match LEnv.find_var le v with
    | Some (le, st, v) -> Some (Either.Left (le, Local st, v))
    | None -> begin match Env.Int.find_opt v ge.ge_fmap with
        | Some (FuncID ((TFunc (_, _), ctyp, _, _) as fid, fdata)) -> begin match ctyp with
            | TLnil -> Some (Either.Right (ACFunction (fid, fdata)))
            | TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
          end
        | None -> Option.map (fun v -> Either.Left (le, Global, v)) (Env.Int.find_opt v ge.ge_gblmap)
      end
  
  let find_fun (ge : t) (le : LEnv.t) (v : var) : LEnv.t * funid anyclike = match Env.Int.find_opt v ge.ge_fmap with
    | Some (FuncID ((TFunc (_, _), ctyp, _, _) as fid, fdata)) -> begin match ctyp with
        | TLnil -> le, ClosedLike (Function (fid, fdata))
        | TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
      end
    | None -> begin match LEnv.find_continuation le v with
        | Some (le, loc, TypeList args, VarID (ret, vid), hdlfid, Type tret, thdlid, hdlcloc, hdlcid) ->
            le, ClosedLike (Contin (
              Local loc,
              (TCont ret, vid),
              (TFunc (TLcons (TCont ret, TLcons (TTuple args, TLnil)), tret), TLnil, thdlid, hdlfid),
              Local hdlcloc,
              hdlcid))
        | None -> begin match find_var ge le v with
            | Some (Either.Left (le, loc, VarID ((t, _) as vid))) -> begin match t with
                | TClosed _ -> le, ClosedLike (Closure (loc, vid))
                | _ -> raise (internal_error "Unexpected variable type, expected closed function")
              end
            | Some (Either.Right (ACFunction (f, fid))) -> le, ClosedLike (Function (f, fid))
            | None ->
                let name = get_var_name ge v in
                le, CLBuiltin name
          end
      end
  let find_closable_fun (ge : t) (_ : LEnv.t) (v : var) : funid anyfuncid = Env.Int.find v ge.ge_fmap
  
  let allocate_function (env : t) (args : LEnv.realt) (b : binder) : funid * mfunid * mtypid * t =
    let f = ref None in
    let TypeList args, (TypeList ctyp, cltypid) = LEnv.args_typ args in
    let Type ret = convert_type_function_ret (Var.type_of_binder b) in
    let ftyp = TFunc (args, ret) in
    let env, ftypid = add_typ env (Type ftyp) in
    let fid = env.ge_nfuns in
    let needs_export = ref false in
    let fdata = (f, needs_export) in
    let ge_fmap = Env.Int.bind (Var.var_of_binder b) (FuncID ((ftyp, ctyp, cltypid, fid), fdata)) env.ge_fmap in
    fdata, fid, ftypid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = Either.Left (f, fid, needs_export) :: env.ge_funs;
      ge_fmap;
    }
  let assign_function (env : t) ((fid, _) : funid) (f : func') : t = match !fid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> fid := Some f; env
  let do_export_function (env : t) ((_, fref) : funid) : t = fref := true; env
  
  let new_continuator (env : t) (hdlfid : mfunid) (Type cret : anytyp) (TypeList targs : anytyp_list) (Type tret : anytyp) : t * mfunid =
    let TypeList targs, acid = let rec inner : 'a. 'a typ_list -> _ = fun (type a) (tl : a typ_list) (TypeList tacc) acc -> match tl with
      | TLnil -> TypeList tacc, acc
      | TLcons (t, tl) -> inner tl (TypeList (TLcons (t, tacc))) (Int32.succ acc)
      in inner targs (TypeList TLnil) 0l in
    let ExprList (targs, args) = let rec inner : 'a. 'a typ_list -> _ =
          fun (type a) (tl : a typ_list) n acc -> match tl with
      | TLnil -> acc
      | TLcons (t, tl) ->
          let n = Int32.pred n in
          let ExprList (ts, es) = acc in
          inner tl n (ExprList (TLcons (t, ts), ELcons (EVariable (Local StorVariable, (t, n)), es)))
      in inner targs acid (ExprList (TLnil, ELnil)) in
    let ccid = Int32.succ acid in
    let ct = TLcons (TCont cret, TLcons (TAbsClosArg, TLnil)) in
    let env, ctid = add_typ env (Type (TClosArg ct)) in
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ fid in
    let b = Block (tret, ([
      Assign (Local StorVariable, (TClosArg ct, ccid), EConvertClosure (acid, TClosArg ct, ctid))],
      ECallRawHandler (hdlfid, cret, EVariable (Local StorClosure, (TCont cret, 0l)),
                               targs, args,
                               EVariable (Local StorClosure, (TAbsClosArg, 1l)), tret))) in
    let ftyp = TFunc (targs, tret) in
    let env, ftypid = add_typ env (Type ftyp) in
    let f = {
      fun_typ = ftypid;
      fun_id = fid;
      fun_export_data = None;
      fun_closure = TypeList ct, ctid;
      fun_converted_closure = Some ccid;
      fun_args = TypeList targs;
      fun_locals = TypeList (TLcons (TClosArg ct, TLnil));
      fun_block = b;
    } in
    { env with
      ge_nfuns;
      ge_funs = Either.Right (Either.Right (f, fid)) :: env.ge_funs;
    }, fid
  let new_function (env : t) (args : LEnv.subt) (TypeList body_closts : anytyp_list) (b : anyblock) (cclosid : mvarid option)
      : t * mtypid * mfunid * funid =
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ env.ge_nfuns in
    let Block (tret, _) = b in
    let env, body_ctid = add_typ env (Type (TClosArg body_closts)) in
    let ftyp = TFunc (TLnil, tret) in
    let env, ftypid = add_typ env (Type ftyp) in
    let f = LEnv.compile_sub args ftypid fid b (TypeList body_closts) body_ctid cclosid in
    let f, export = ref (Some f), ref false in
    let fun_data = f, export in
    { env with
      ge_nfuns;
      ge_funs = Either.Left (f, fid, export) :: env.ge_funs;
    }, body_ctid, fid, fun_data
  let allocate_fhandler (env : t) : hid * mfunid * t =
    let f = ref None in
    let fid = env.ge_nfuns in
    let fdata = f in
    fdata, fid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = Either.Right (Either.Left f) :: env.ge_funs;
    }
  let assign_fhandler (env : t) (hid : hid) (args : LEnv.subt) (oabsconc : (meffid * meffid) option)
                      (onret : ('b, 'd) finisher) (ondo : ('b, 'd) handler list) : t = match !hid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> hid := Some (LEnv.compile_handler args oabsconc onret ondo); env
  
  let add_effect (env : t) (ename : string) (eargs : anytyp_list) : t * meffid =
    let eff = (ename, eargs) in
    match EffectMap.find_opt eff env.ge_effmap with
    | Some i -> env, i
    | None ->
        let i = env.ge_neffs in
        let ge_neffs = Int32.succ env.ge_neffs in
        let ge_effs = EffectIDMap.add i eargs env.ge_effs in
        let ge_effmap = EffectMap.add eff i env.ge_effmap in
        let env = { env with ge_neffs; ge_effs; ge_effmap; } in
        env, i
  
  let compile (ge : t) (le : LEnv.realt) (blk : anyblock) : modu =
    let lvs = LEnv.locals_of_env le in
    {
      mod_nfuns = ge.ge_nfuns;
      mod_funs =
        List.rev_map
          (fun f -> match f with
            | Either.Left ({ contents = None }, _, _) -> raise (internal_error "function was allocated but never assigned")
            | Either.Left ({ contents = Some f }, _, _) -> FFunction f
            | Either.Right (Either.Left { contents = None }) -> raise (internal_error "handler was allocated but never assigned")
            | Either.Right (Either.Left { contents = Some (AHandler f) }) -> FHandler f
            | Either.Right (Either.Right (f, _)) -> FFunction f)
          ge.ge_funs;
      mod_needs_export =
        List.fold_left (fun acc -> function
          | Either.Left (fo, fid, rb) ->
              if !rb then
                match !fo with
                | None -> raise (internal_error "function was allocated but never assigned")
                | Some f -> FunIDMap.add fid f.fun_typ acc
              else acc
          | Either.Right (Either.Left _) -> acc (* We don't need to export handlers *)
          | Either.Right (Either.Right (f, fid)) -> FunIDMap.add fid f.fun_typ acc (* We do need to export continuators *))
        FunIDMap.empty ge.ge_funs;
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

let of_constant (c : CommonTypes.Constant.t) : anyexpr = let open CommonTypes.Constant in match c with
  | Float f -> Expr (TFloat, EConstFloat f)
  | Int i -> Expr (TInt, EConstInt (Int64.of_int i))
  | Bool b -> Expr (TBool, EConstBool b)
  | String _ -> failwith "TODO: of_constant String"
  | Char _ -> failwith "TODO: of_constant Char"
  | DateTime _ -> failwith "TODO: of_constant DateTime"

let rec of_value (ge : genv) (le: lenv) (v : value) : genv * lenv * anyexpr = match v with
  | Constant c -> let Expr (ct, cv) = of_constant c in ge, le, Expr (ct, cv)
  | Variable v -> begin match GEnv.find_var ge le v with
      | Some (Either.Left (le, loc, VarID (t, vid))) -> ge, le, Expr (t, EVariable (loc, (t, vid)))
      | Some (Either.Right (ACFunction ((TFunc (targs, tret), _, _, _) as f, fhandle))) ->
          let ge = GEnv.do_export_function ge fhandle in
          ge, le, Expr (TClosed (targs, tret), EClose (f, ELnil))
      | None -> begin match LEnv.find_continuation le v with
          | Some (le, loc, TypeList (type a) (args : a typ_list), VarID (ret, vid), hdlfid, Type (type b) (tret : b typ), _, hdlcloc, hdlcid) ->
              let ge, (fid : mfunid) = GEnv.new_continuator ge hdlfid (Type ret) (TypeList args) (Type tret) in
              let fct = TLcons (TCont ret, TLcons (TAbsClosArg, TLnil)) in
              let ge, fctid = GEnv.add_typ ge (Type (TClosArg fct)) in
              let fid : (a, b, _) funcid = TFunc (args, tret), fct, fctid, fid in
              ge, le, Expr (TClosed (args, tret),
                            EClose (fid, ELcons (EVariable (Local loc, (TCont ret, vid)),
                                         ELcons (EVariable (Local hdlcloc, (TAbsClosArg, hdlcid)),
                                         ELnil))))
          | None -> failwith ("TODO: of_value Variable (probable builtin: " ^ (string_of_int v) ^ ")")
        end
      end
  | Extend (nm, None) -> if Utility.StringMap.is_empty nm then ge, le, Expr (TTuple TLnil, EConstUnit) else failwith "TODO: of_value Extend None"
  | Extend (_, Some _) -> failwith "TODO: of_value Extend Some"
  | Project (n, v) -> begin match v with
      | Variable v -> begin match LEnv.find_closure le v n with
          | Some (le, lst, VarID (t, i)) ->
              ge, le, Expr (t, EVariable (Local lst, (t, i)))
          | None -> failwith "TODO: of_value Project Variable with unregistered projection"
        end
      | _ -> ignore (n, ge); failwith "TODO: of_value Project with non-Variable"
      end
  | Erase _ -> failwith "TODO: of_value Erase"
  | Inject _ -> failwith "TODO: of_value Inject"
  | TAbs (_, v)
  | TApp (v, _) -> of_value ge le v
  | XmlNode _ -> failwith "TODO: of_value XmlNode"
  | ApplyPure (f, args) -> begin match skip_toplevel_polymorphism f with
      | Variable v -> begin
          let name = GEnv.get_var_name ge v in match args with
          | [arg] -> begin match Builtins.get_unop name with
            | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin unary operation"))
            | Some (Unop UONegI) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TInt in
                ge, le, Expr (TInt, EUnop (UONegI, arg))
            | Some (Unop UONegF) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TFloat in
                ge, le, Expr (TFloat, EUnop (UONegF, arg))
            end
          | [arg1; arg2] -> begin match Builtins.get_binop name with
            | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin binary operation"))
            | Some (Binop BOAddI) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                ge, le, Expr (TInt, EBinop (BOAddI, arg1, arg2))
            | Some (Binop BOAddF) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                ge, le, Expr (TFloat, EBinop (BOAddF, arg1, arg2))
            | Some (Binop BOSubI) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                ge, le, Expr (TInt, EBinop (BOSubI, arg1, arg2))
            | Some (Binop BOSubF) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                ge, le, Expr (TFloat, EBinop (BOSubF, arg1, arg2))
            | Some (Binop BOMulI) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                ge, le, Expr (TInt, EBinop (BOMulI, arg1, arg2))
            | Some (Binop BOMulF) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                ge, le, Expr (TFloat, EBinop (BOMulF, arg1, arg2))
            | Some (Binop BODivI) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                ge, le, Expr (TInt, EBinop (BODivI, arg1, arg2))
            | Some (Binop BODivF) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TFloat in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TFloat in
                ge, le, Expr (TFloat, EBinop (BODivF, arg1, arg2))
            | Some (Binop BORemI) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TInt in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TInt in
                ge, le, Expr (TInt, EBinop (BORemI, arg1, arg2))
            | Some (Binop BOEq) ->
                let ge, le, Expr (t, arg1) = of_value ge le arg1 in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOEq, arg1, arg2))
            | Some (Binop BONe) ->
                let ge, le, Expr (t, arg1) = of_value ge le arg1 in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BONe, arg1, arg2))
            end
          | _ -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin n-ary operation"))
        end
      | _ -> failwith "TODO: of_value ApplyPure for non-Variable"
    end
  | Closure (f, _, cls) ->
      let FuncID (type a b c) ((TFunc (targs, tret), ctyp, _, _) as fid, fdata : (a, b, c) funcid * _) = GEnv.find_closable_fun ge le f in
      let ge, le, (cls : c expr_list) = match cls with
        | Extend (vm, None) ->
            let cls = sort_name_map vm |> List.map snd in
            convert_values ge le cls ctyp
        | Extend (_, Some _) -> failwith "TODO: of_value Closure Extend Some"
        | _ -> failwith "TODO: of_value Closure non-Extend" in
      let ge = GEnv.do_export_function ge fdata in
      ge, le, Expr (TClosed (targs, tret), EClose (fid, cls))
  | Coerce (v, _) -> of_value ge le v (* Assume coercion never needs to occur, which is unfortunately unlikely *)

and convert_values : 'a. _ -> _ -> _ -> 'a typ_list -> _ * _ * 'a expr_list =
  fun (type a) (ge : genv) (le : lenv) (vs : value list) (ts : a typ_list) : (genv * lenv * a expr_list) ->
  let ge = ref ge in
  let le = ref le in
  let [@tail_mod_cons] rec convert_args : 'a. value list -> 'a typ_list -> 'a expr_list
      = fun (type a) (args : value list) (targs : a typ_list) : a expr_list -> match args, targs with
    | [], TLnil -> ELnil
    | [], TLcons _ -> raise (internal_error "Type mismatch: not enough expressions")
    | _ :: _, TLnil -> raise (internal_error "Type mismatch: too many expressions")
    | ehd :: etl, TLcons (thd, ttl) ->
        let ge', le', Expr (t, e) = of_value !ge !le ehd in
        let Eq = assert_eq_typ t thd "Type mismatch: the argument does not have the correct type" in
        ge := ge'; le := le'; ELcons (e, convert_args etl ttl)
  in let ret = convert_args vs ts in !ge, !le, ret

let convert_values_unk (ge : genv) (le : lenv) (vs : value list) : genv * lenv * anyexpr_list =
  let ge = ref ge in
  let le = ref le in
  let rec convert_args (args : value list) : anyexpr_list = match args with
    | [] -> ExprList (TLnil, ELnil)
    | ehd :: etl ->
        let ge', le', Expr (t, e) = of_value !ge !le ehd in
        ge := ge'; le := le';
        let ExprList (tl, el) = convert_args etl in
        ExprList (TLcons (t, tl), ELcons (e, el))
  in let ret = convert_args vs in !ge, !le, ret

let allocate_function (ge : genv) (args : binder list) (b : binder) (closure : binder option) : genv * GEnv.funid * mfunid * mtypid * LEnv.realt =
  let le_args =
    let rec inner args acc = match args with
      | [] -> acc
      | hd :: tl -> let acc = LEnv.add_arg acc hd in inner tl acc
    in inner args LEnv.no_arg
  in
  let ge, le = LEnv.env_of_args le_args closure (GEnv.add_typ ge) in
  let loc_fid, fid, ftid, ge = GEnv.allocate_function ge le b in
  ge, loc_fid, fid, ftid, le

type 'a transformer = Transformer : ('a, 'b) finisher -> 'a transformer

let rec of_tail_computation (ge : genv) (le: lenv) (tc : tail_computation) : genv * lenv * anyexpr = match tc with
  | Return v -> let ge, le, v = of_value ge le v in ge, le, v
  | Apply (f, args) -> begin
      match skip_toplevel_polymorphism f with
      | Variable v -> begin match GEnv.find_fun ge le v with
          | le, ClosedLike (Function ((TFunc (targs, tret), TLnil, _, _) as fid, fdata)) ->
              let ge, le, args = convert_values ge le args targs in
              let ge = GEnv.do_export_function ge fdata in
              ge, le, Expr (tret, ECallClosed (EClose (fid, ELnil), args, tret))
          | le, ClosedLike (Closure (loc, ((TClosed (targs, tret), _) as vid))) ->
              let ge, le, args = convert_values ge le args targs in
              ge, le, Expr (tret, ECallClosed (EVariable (loc, vid), args, tret))
          | le, ClosedLike (Contin (loc, vid, (TFunc (TLcons (_, TLcons (TTuple targs, _)), tret), _, _, _ as hdlfid), hdlcloc, hdlcid)) ->
              let ge, le, args = convert_values ge le args targs in
              ge, le, Expr (tret, ECont (loc, vid, args, hdlfid, hdlcloc, hdlcid))
          | le, CLBuiltin name ->
              let ge, le, args = convert_values_unk ge le args in
              ge, le, Builtins.gen_impure name args
        end
      | Closure (f, _, cls) ->
          let FuncID ((TFunc (targs, tret), ctyp, _, _) as fid, fdata) = GEnv.find_closable_fun ge le f in
          let ge, le, args = convert_values ge le args targs in
          let ge, le, cls = match cls with
            | Extend (vm, None) ->
                let cls = sort_name_map vm |> List.map snd in
                convert_values ge le cls ctyp
            | Extend (_, Some _) -> failwith "TODO of_tail_computation Apply Closure Extend Some"
            | _ -> failwith "TODO of_tail_computation Apply Closure non-Extend" in
          let ge = GEnv.do_export_function ge fdata in
          ge, le, Expr (tret, ECallClosed (EClose (fid, cls), args, tret))
      | Project (n, v) -> begin match v with
          | Variable v -> begin match LEnv.find_closure le v n with
              | Some (le, lst, VarID (t, i)) ->
                  let TypeList targs, Type tret = match t with
                    | TClosed (targs, tret) -> TypeList targs, Type tret
                    | _ -> raise (internal_error "Unexpected type, expected a function")
                  in
                  let ge, le, args = convert_values ge le args targs in
                  ge, le, Expr (tret, ECallClosed (EVariable (Local lst, (TClosed (targs, tret), i)), args, tret))
              | None -> failwith "TODO of_tail_computation Apply Project with unregistered projection"
            end
          | _ -> failwith "TODO of_tail_computation Apply Project with non-Variable"
        end
      | _ -> failwith "TODO of_tail_computation Apply with non-Variable and non-Closure"
      end
  | Special (DoOperation (tagname, args, tret)) ->
      let ge, le, ExprList (targs, args) = convert_values_unk ge le args in
      let ge, eid = GEnv.add_effect ge tagname (TypeList targs) in
      let Type tret = convert_type tret in
      ge, le, Expr (tret, EDo ((TClosed (targs, tret), eid), args))
  | Special (Handle h) -> begin match h.ih_depth with
      | Shallow -> raise (Errors.RuntimeError "Wasm compilation of shallow handlers is currently not supported")
      | Deep bvs ->
          if bvs <> [] then raise (Errors.RuntimeError "Wasm compilation of deep parameterized handlers is currently not supported");
          (* First the body ("computation" in the first IR) *)
          let ge, Type (type b) (cret : b typ), body_cltid, body_id, ExprList (type c) (body_closts, body_closes : _ * c expr_list) =
            let body_le = LEnv.create_sub le false in
            let ge, body_le, Block (type b) (cret, (ass, e) : b typ * _) = of_computation ge (LEnv.of_sub body_le) h.ih_comp in
            let body_le = LEnv.to_sub body_le in
            (* Add closure conversion at the beginning of the block if required *)
            let body_le, oabsconc, ExprList (type c) (body_closts, body_closes : _ * c expr_list) = LEnv.get_closure body_le in
            let ge, body_ctid = GEnv.add_typ ge (Type (TClosArg body_closts)) in
            let ass = match oabsconc with
              | None -> ass
              | Some (absid, concid) ->
                  Assign (Local StorVariable,
                          (TClosArg body_closts, concid),
                          EConvertClosure (absid, TClosArg body_closts, body_ctid))
                    :: ass in
            let b = Block (cret, (ass, e)) in
            (* Done, now add the "body" function to the global environment *)
            let ge, body_cltid, body_id, body_locid = GEnv.new_function ge body_le (TypeList body_closts) b (Option.map snd oabsconc) in
            let ge = GEnv.do_export_function ge body_locid in
            ge, Type cret, body_cltid, body_id, ExprList (body_closts, body_closes) in
          let body_id : (unit, b, c) funcid = (TFunc (TLnil, cret), body_closts, body_cltid, body_id) in
          (* Now to the handler function (the rest of the tail_computation) *)
          let ge, Type (type d) (tret : d typ), handler_cltid, handler_id, ExprList (type e) (handler_closts, handler_closes : _ * e expr_list) =
            let hid, handler_id, ge = GEnv.allocate_fhandler ge in
            let handle_le = LEnv.create_sub le true in
            let ge, handle_le, Transformer (type d) (onret : (b, d) finisher) =
              of_finisher ge (LEnv.of_sub handle_le) h.ih_return cret in
            let handle_le = LEnv.to_sub handle_le in
            let tret = match onret with FId t -> (t : d typ) | FMap (_, t, _) -> t in
            let ge, hcllocid, hcltid = GEnv.new_generic ge in
            let handle_le, contid = LEnv.add_cont handle_le (Type cret) handler_id (Type tret) hcltid in
            let contid : b continuation varid = TCont cret, contid in
            let ge, handle_le, ondo =
              let do_case (ge : genv) (handle_le : LEnv.subt) (ename : string) ((args, k, p) : effect_case) : genv * LEnv.subt * (b, d) handler =
                let handle_le, VarIDList (eargs, vargs) = LEnv.set_handler_args handle_le args in
                let ge, eid = GEnv.add_effect ge ename (TypeList eargs) in
                let Type rarg = _convert_type (Var.type_of_binder k)
                    (fun _ -> raise (internal_error "Expected a function type, got another type"))
                    (fun args _ _ -> convert_type args)
                    (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type"))
                    (fun _ _ _ -> raise (internal_error "Expected a function type, got any type")) in (* TODO: replace by TUnit? *)
                let TypeList rarg = match rarg with
                  | TTuple tl -> TypeList tl
                  | _ -> raise (internal_error "Expected a tuple type, got something else") in
                let handle_le = LEnv.set_continuation handle_le k (TypeList rarg) in
                let ge, handle_le, Block (t, b) = of_computation ge (LEnv.of_sub handle_le) p in
                let handle_le = LEnv.to_sub handle_le in
                let Eq = assert_eq_typ t tret "Expected the same type in the return branch as in all handler branches" in
                ge, handle_le, Handler ((TClosed (eargs, TTuple rarg), eid), contid, vargs, b)
              in let do_case ename (ec : effect_case) (ge, handle_le, acc) =
                let ge, handle_le, hd = do_case ge handle_le ename ec in
                ge, handle_le, hd :: acc
              in Utility.StringMap.fold do_case h.ih_cases (ge, handle_le, []) in
            let handle_le, oabsconc, ExprList (type e) (hclosts, hcloses : _ * e expr_list) = LEnv.get_closure handle_le in
            (* let ge, hcltid = GEnv.add_typ ge (Type (TClosArg hclosts)) in *)
            let ge = GEnv.set_generic ge hcllocid (Type (TClosArg hclosts)) in
            let ge = GEnv.assign_fhandler ge hid handle_le oabsconc onret ondo in
            ge, Type tret, hcltid, handler_id, ExprList (hclosts, hcloses) in
          let handler_id : (b continuation * (c closure_content * unit), d, e) funcid =
            (TFunc (TLcons (TCont cret, TLcons (TClosArg body_closts, TLnil)), tret), handler_closts, handler_cltid, handler_id) in
          (* All done! *)
          ge, le, Expr (tret, EDeepHandle (body_id, body_closes, handler_id, handler_closes))
    end
  | Special _ -> failwith "TODO of_tail_computation Special"
  | Case _ -> failwith "TODO of_tail_computation Case"
  | If (b, t, f) ->
      let ge, le, Expr (tb, eb) = of_value ge le b in
      let Eq = assert_eq_typ tb TBool "Expected a boolean expression" in
      let ge, le, Block (tt, bt) = of_computation ge le t in
      let ge, le, Block (tf, bf) = of_computation ge le f in
      let Eq = assert_eq_typ tt tf "Expected the same type in both branches" in
      ge, le, Expr (tt, ECond (tt, eb, bt, bf))
and of_computation (ge : genv) (le : lenv) ((bs, tc) : computation) : genv * lenv * anyblock =
  let finish_computation ge fd loc_fid fid ftypid (new_le : LEnv.realt) =
    let new_le = LEnv.of_real new_le in
    let ge, new_le, b = of_computation ge new_le fd.fn_body in
    let new_le = LEnv.to_real new_le in
    let ge, new_le, b =
      let Block (t, (ass, e)) = b in
      match fd.fn_closure with
      | None -> ge, new_le, b
      | Some _ ->
          let new_le, absid, concid, TypeList ctyp = LEnv.add_closure new_le in
          let ge, ctid = GEnv.add_typ ge (Type (TClosArg ctyp)) in
          ge, new_le, Block (t, (Assign (Local StorVariable, (TClosArg ctyp, concid), EConvertClosure (absid, TClosArg ctyp, ctid)) :: ass, e)) in
    let export_name = match fd.fn_closure with None -> Some (Js.name_binder fd.fn_binder) | Some _ -> None in
    let f = LEnv.compile new_le ftypid fid export_name b in
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
and of_finisher : 'a. _ -> _ -> _ -> 'a typ -> _ * _ * 'a transformer =
    fun (type a) (ge : genv) (le : lenv) ((bind, comp) : binder * computation) (t : a typ) : (genv * lenv * a transformer) ->
  let bvar = Var.var_of_binder bind in
  match comp with
  | ([], Return (Variable v)) when Var.equal_var bvar v ->
      ge, le, Transformer (FId t)
  | ([Let (b2, ([], Return (Variable v1)))], Return (Variable v2))
        when Var.equal_var bvar v1 && Var.equal_var (Var.var_of_binder b2) v2 ->
      ge, le, Transformer (FId t)
  | _, _ ->
      let le, bid = LEnv.add_var le bind (Type t) in
      let bid' : a varid = t, bid in
      let ge, le, Block (t, b) = of_computation ge le comp in
      ge, le, Transformer (FMap (bid', t, b))

let find_global_binders ((bs, _) : computation) =
  let rec inner bs acc = match bs with
    | [] -> Utility.StringMap.fold (fun _ b acc -> Utility.IntSet.add b acc) acc Utility.IntSet.empty
    | Let (b, _) :: bs ->
        let name = Var.name_of_binder b in inner bs (if name = "" then acc else Utility.StringMap.add name (Var.var_of_binder b) acc)
    | Fun _ :: bs | Rec _ :: bs | Alien _ :: bs | Module _ :: bs -> inner bs acc
  in inner bs Utility.StringMap.empty

let module_of_ir (c : computation) (map : string Env.Int.t) : modu =
  let ge = GEnv.empty map (find_global_binders c) in
  let ge, le, blk = of_computation ge (LEnv.of_real LEnv.toplevel) c in
  GEnv.compile ge (LEnv.to_real le) blk
