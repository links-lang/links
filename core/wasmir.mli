(* TODO: rethink how deep handlers work, so that we don't have to call another
   function in the (most common?) case of tail resumption *)

type tagid = private int
type mtypid                  (* Module type ID     *)
type mvarid = private int32  (* Module variable ID *)
type closid = private int32  (* Closure member ID  *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID   *)
module MTypMap : Utility.Map.S with type key = mtypid
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDMap : Utility.Map.S with type key = meffid

type ('a, 'b) rawfunctyp = private RawFunction of ('a -> 'b)
type ('a, 'b) functyp = private Function of ('a -> 'b)
type variant = private Variant
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
  | TTuple : 'a named_typ_list -> 'a list typ
  | TVariant : variant typ
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list
and 'a named_typ_list =
  | NTLnil : unit named_typ_list
  | NTLcons : string * 'a typ * 'b named_typ_list -> ('a * 'b) named_typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list
module TypeMap : Utility.Map.S with type key = anytyp

type ('a, 'b) extract_typ_check
type ('a, 'b) extract_typ = int * 'b typ * ('a, 'b) extract_typ_check

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
  | BOLe : (int, int, bool) binop
  | BOLt : (int, int, bool) binop
  | BOGe : (int, int, bool) binop
  | BOGt : (int, int, bool) binop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c) funcid = private (('a, 'b) functyp typ * 'c typ_list * mfunid)
  (* Function type, closure type, closure type ID, module-level function ID *)
type ('a, 'b) effectid = private (('a -> 'b) typ * meffid)

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type ('a, 'b) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap of 'a varid * 'b typ * 'b block
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
  | EConvertClosure : mvarid * 'a closure_content typ * mtypid -> 'a closure_content expr
  | EIgnore : 'a typ * 'a expr -> unit list expr
  | EConstInt : int64 -> int expr
  | EConstBool : bool -> bool expr
  | EConstFloat : float -> float expr
  | EUnop : ('a, 'b) unop * 'a expr -> 'b expr
  | EBinop : ('a, 'b, 'c) binop * 'a expr * 'b expr -> 'c expr
  | EVariable : locality * 'a varid -> 'a expr
  | ETuple : 'a named_typ_list * 'a expr_list -> 'a list expr
  | EExtract : 'a list expr * ('a, 'b) extract_typ -> 'b expr
  | EVariant : tagid * 'a typ * 'a expr -> variant expr
  | ECase : variant varid * variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EClose : ('a, 'b, 'c) funcid * 'c expr_list -> ('a -> 'b) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ_list * 'b expr_list * abs_closure_content expr * 'd typ -> 'd expr
  (* ^ Internal use only: pass the arguments in a struct without modification (no closure de/construction) *)
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
            (('b continuation * ('a closure_content * unit), 'd) functyp typ * mtypid * mfunid) * locality * mvarid -> 'd expr
and ('a, 'b) handler = (* The continuation itself returns 'a, the handler returns 'b *)
  (* Note: we lose the information that the continuation takes 'b as parameter(s) *)
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and 'a expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

type anyblock = Block : 'a typ * 'a block -> anyblock

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
  fh_id : mfunid;
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

val module_of_ir : Ir.program -> string Env.Int.t -> modu
