type mtypid                  (* Module type ID *)
type mvarid = private int32  (* Module variable ID *)
type closid = private int32  (* Closure member ID *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID *)
module MTypMap : Utility.Map.S with type key = mtypid
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDMap : Utility.Map.S with type key = meffid

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
module TypeMap : Utility.Map.S with type key = anytyp

type ('a, 'r) unop =
  | UONegI : (int,   int)   unop
  | UONegF : (float, float) unop

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

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c) funcid = private (('a, 'b) functyp typ * 'c typ_list * mtypid * mfunid)
  (* Function type, closure type, closure type ID, module-level function ID *)
type ('a, 'b) effectid = private (('a -> 'b) typ * meffid)
type handle_depth = Shallow | Deep
  
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

val module_of_ir : Ir.program -> string Env.Int.t -> modu
