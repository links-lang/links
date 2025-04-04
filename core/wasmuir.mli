type tagid = private int
type mvarid = private int32  (* Module variable ID *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID   *)
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDMap : Utility.Map.S with type key = meffid

type variant = Wasmir.variant
type abs_closure_content = Wasmir.abs_closure_content
type 'a closure_content = 'a Wasmir.closure_content
type 'a continuation = 'a Wasmir.continuation

type 'a typ =
  | TInt : int typ
  | TBool : bool typ
  | TFloat : float typ
  | TClosed : 'a typ_list * 'b typ -> ('c * 'a -> 'b) typ
  | TAbsClosArg : abs_closure_content typ
  | TClosArg : 'a typ_list -> 'a closure_content typ
  | TCont : 'a typ -> 'a continuation typ
  | TTuple : 'a named_typ_list -> 'a list typ
  | TVariant : variant typ
  | TVar : unit typ
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list
and 'a named_typ_list =
  | NTLnil : unit named_typ_list
  | NTLcons : string * 'a typ * 'b named_typ_list -> ('a * 'b) named_typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list
type anynamed_typ_list = NamedTypeList : 'a named_typ_list -> anynamed_typ_list

module TypeMap : Utility.Map.S with type key = anytyp

type ('a, 'b) extract_typ = 'a named_typ_list * int * 'b typ

type ('a, 'r) unop =
  | UONegI : (int,   int)   unop
  | UONegF : (float, float) unop
type ('a, 'b, 'r) binop =
  | BOAddI : (int, int, int) binop | BOAddF : (float, float, float) binop
  | BOSubI : (int, int, int) binop | BOSubF : (float, float, float) binop
  | BOMulI : (int, int, int) binop | BOMulF : (float, float, float) binop
  | BODivI : (int, int, int) binop | BODivF : (float, float, float) binop
  | BORemI : (int, int, int) binop
  | BOEq : 'a typ -> ('a, 'a, bool) binop
  | BONe : 'a typ -> ('a, 'a, bool) binop
  | BOLe : (int, int, bool) binop | BOLt : (int, int, bool) binop
  | BOGe : (int, int, bool) binop | BOGt : (int, int, bool) binop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c) funcid = private ('a typ_list * 'b typ * 'c typ_list * mfunid)
  (* Function type, closure type, closure type ID, module-level function ID *)
type ('a, 'b) effectid = private ('a typ_list * 'b typ * meffid)

type 'a varid_list =
  | VLnil
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type (_, _) box =
  | BNone : ('a, 'a) box
  | BClosed : ('g * 'a -> 'b) typ * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b, 'g * 'c -> 'd) box
  | BCont : ('a, 'b) box -> ('a continuation, 'b continuation) box
  | BTuple : ('a, 'b) box_list -> ('a list, 'b list) box
  | BBox : 'a typ -> ('a, unit) box
and (_, _) box_list =
  | BLnone : ('a, 'a) box_list
  | BLnil : (unit, unit) box_list
  | BLcons : ('a, 'c) box * ('b, 'd) box_list -> ('a * 'b, 'c * 'd) box_list

val dst_of_box : 'a typ -> ('a, 'b) box -> 'b typ

type (_, _) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap : 'a varid * 'b typ * 'b block -> ('a, 'b) finisher
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
  | EConvertClosure : mvarid * 'a closure_content typ -> 'a closure_content expr
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
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EClose : ('a, 'b, 'c) funcid * ('d, 'c) box_list * 'd expr_list -> ('g * 'a -> 'b) expr
  | ESpecialize : (_ * 'c -> 'd) expr * ('g * 'a -> 'b) typ * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b) expr
  | ECallRawHandler : mfunid * 'c continuation typ * 'c continuation expr * 'a typ * 'a expr * abs_closure_content expr * 'b typ -> 'b expr
  | ECallClosed : ('g * 'a -> 'b) expr * 'a expr_list -> 'b expr
  | ECond : bool expr * 'a typ * 'a block * 'a block -> 'a expr
  | EDo : ('a, 'b) effectid * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'b, 'c) funcid * 'c expr_list * ('b, 'd) finisher * ('b, 'd) handler list -> 'd expr
  | EDeepHandle : (unit, 'b, 'c) funcid * 'c expr_list *
                  ('b continuation * ('c closure_content * unit), 'd, 'e) funcid * 'e expr_list -> 'd expr
  | ECont : locality * 'b continuation varid * 'a expr *
            ('b continuation * ('a * unit), 'd, 'c) funcid * (locality * 'c closure_content varid) -> 'd expr
and (_, _) handler =
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and _ expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

type ('a, 'b) func' = {
  fun_id               : mfunid;
  fun_export_data      : string option;
  fun_converted_closure: (anytyp_list * mvarid) option;
  fun_args             : 'a typ_list;
  fun_locals           : anytyp list;
  fun_ret              : 'b typ;
  fun_block            : 'b block;
}
type ('a, 'b, 'c) fhandler = {
  fh_contarg : 'a continuation varid * mvarid;
  fh_closure : (mvarid * 'c closure_content varid) option;
  fh_locals  : anytyp list;
  fh_finisher: ('a, 'b) finisher;
  fh_handlers: ('a, 'b) handler list;
  fh_id      : mfunid;
}
type func = FFunction : ('a, 'b) func' -> func | FHandler : ('a, 'b, 'c) fhandler -> func
type 'a modu = {
  mod_imports     : (string * string) list;
  mod_nfuns       : int32;
  mod_funs        : func list;
  mod_needs_export: (anytyp_list * anytyp) FunIDMap.t;
  mod_neffs       : int32;
  mod_effs        : anytyp_list EffectIDMap.t;
  mod_nglobals    : int32;
  mod_global_vars : (mvarid * anytyp * string) list;
  mod_locals      : anytyp list;
  mod_main        : 'a typ;
  mod_block       : 'a block;
}

val module_of_ir : 'a Wasmir.modu -> 'a modu
