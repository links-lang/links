type tagid = private int
type mvarid = private int32  (* Module variable ID *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID   *)
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDSet : Utility.Set.S with type elt = meffid

type process = Wasmir.process
type llist = Wasmir.llist
type variant = Wasmir.variant
type abs_closure_content = Wasmir.abs_closure_content
type 'a closure_content = 'a Wasmir.closure_content
type 'a continuation = 'a Wasmir.continuation

type !'a typ =
  | TInt : int typ
  | TBool : bool typ
  | TFloat : float typ
  | TString : string typ
  | TClosed : 'a typ_list * 'b typ -> ('c * 'a -> 'b) typ
  | TAbsClosArg : abs_closure_content typ
  | TClosArg : 'a typ_list -> 'a closure_content typ
  | TCont : 'a typ -> 'a continuation typ
  | TTuple : 'a typ_list -> 'a list typ
  | TVariant : variant typ
  | TList : 'a typ -> llist typ
  | TVar : unit typ
  | TSpawnLocation : Value.spawn_location typ
  | TProcess : process typ
and !'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list

module TypeMap : Utility.Map.S with type key = anytyp

type (!'a, !'b) extract_typ = 'a typ_list * int * 'b typ

type ('a, 'r) unop =
  | UONot  : (bool,  bool)  unop
  | UONegI : (int,   int)   unop
  | UONegF : (float, float) unop
type ('a, 'b, 'r) binop =
  | BOAddI : (int, int, int) binop | BOAddF : (float, float, float) binop
  | BOSubI : (int, int, int) binop | BOSubF : (float, float, float) binop
  | BOMulI : (int, int, int) binop | BOMulF : (float, float, float) binop
  | BODivI : (int, int, int) binop | BODivF : (float, float, float) binop
  | BORemI : (int, int, int) binop
  | BOEq : 'a typ -> ('a, 'a, bool) binop | BONe : 'a typ -> ('a, 'a, bool) binop
  | BOLe : 'a typ -> ('a, 'a, bool) binop | BOLt : 'a typ -> ('a, 'a, bool) binop
  | BOGe : 'a typ -> ('a, 'a, bool) binop | BOGt : 'a typ -> ('a, 'a, bool) binop
  | BOConcat : (string, string, string) binop
  | BOCons : 'a typ -> ('a, llist, llist) binop
  | BOConcatList : 'a typ -> ( llist, llist, llist) binop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c) funcid = private ('a typ_list * 'b typ * 'c typ_list * mfunid)
  (* Function type, closure type, closure type ID, module-level function ID *)
type 'a effectid = private ('a typ_list * meffid)

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type (!_, !_) box =
  | BNone : ('a, 'a) box
  | BClosed : ('g * 'a -> 'b) typ * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b, 'g * 'c -> 'd) box
  | BCont : ('a, 'b) box -> ('a continuation, 'b continuation) box
  | BTuple : ('a, 'b) box_list -> ('a list, 'b list) box
  | BBox : 'a typ -> ('a, unit) box
and (!_, !_) box_list =
  | BLnone : ('a, 'a) box_list
  | BLnil : (unit, unit) box_list
  | BLcons : ('a, 'c) box * ('b, 'd) box_list -> ('a * 'b, 'c * 'd) box_list

val dst_of_box : 'a typ -> ('a, 'b) box -> 'b typ

type (_, _) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap : 'a varid * 'b typ * 'b block -> ('a, 'b) finisher
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and _ expr =
  | EUnreachable : 'a typ -> 'a expr
  | EConvertClosure : mvarid * 'a closure_content typ -> 'a closure_content expr
  | EIgnore : 'a typ * 'a expr -> unit list expr
  | EConstInt : int64 -> int expr
  | EConstBool : bool -> bool expr
  | EConstFloat : float -> float expr
  | EConstString : string -> string expr
  | EUnop : ('a, 'b) unop * 'a expr -> 'b expr
  | EBinop : ('a, 'b, 'c) binop * 'a expr * 'b expr -> 'c expr
  | EVariable : locality * 'a varid -> 'a expr
  | ETuple : 'a typ_list * 'a expr_list -> 'a list expr
  | EExtract : 'a list expr * ('a, 'b) extract_typ -> 'b expr
  | EVariant : tagid * 'a typ * 'a expr -> variant expr
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EListNil : 'a typ -> llist expr
  | EListHd : llist expr * 'a typ -> 'a expr
  | EListTl : 'a typ * llist expr -> llist expr
  | EClose : ('a, 'b, 'c) funcid * ('d, 'c) box_list * 'd expr_list -> ('g * 'a -> 'b) expr
  | ERawClose : ('a, 'b, 'c) funcid * abs_closure_content expr -> ('g * 'a -> 'b) expr
  | ESpecialize : (_ * 'c -> 'd) expr * ('g * 'a -> 'b) typ * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b) expr
  | ECallRawHandler : mfunid * 'c continuation typ * 'c continuation expr * 'a typ * 'a expr * 'd typ_list * 'd expr_list *
                      abs_closure_content expr * 'b typ -> 'b expr
  | ECallClosed : ('g * 'a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : bool expr * 'a typ * 'a block * 'a block -> 'a expr
  | EDo : 'a effectid * 'b typ * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'b, 'c) funcid * 'c expr_list * ('b, 'd) finisher * ('b, 'd) handler list -> 'd expr
  | EDeepHandle : (unit, 'b, 'c) funcid * 'c expr_list *
                  ('b continuation * ('c closure_content * 'f), 'd, 'e) funcid * 'e expr_list * 'f expr_list -> 'd expr
and (_, _) handler =
  | Handler : 'a effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and _ expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

val typ_of_expr : 'a expr -> 'a typ

type ('a, 'b) func' = {
  fun_id               : mfunid;
  fun_export_data      : string option;
  fun_converted_closure: (anytyp_list * mvarid) option;
  fun_args             : 'a typ_list;
  fun_locals           : anytyp list;
  fun_ret              : 'b typ;
  fun_block            : 'b block;
}
type 'b fstart = {
  fst_id               : mfunid;
  fst_converted_closure: (anytyp_list * mvarid) option;
  fst_ret              : 'b typ;
  fst_locals           : anytyp list;
  fst_block            : 'b block;
}
type ('a, 'c, 'b) fhandler = {
  fh_contarg : 'a continuation varid * mvarid;
  fh_tis     : 'c typ_list;
  fh_closure : (mvarid * (anytyp_list * mvarid)) option;
  fh_locals  : anytyp list;
  fh_finisher: ('a, 'b) finisher;
  fh_handlers: ('a, 'b) handler list;
  fh_id      : mfunid;
}
type ('g, 'a, 'b) fbuiltin = ('g, 'a, 'b) Wasmir.fbuiltin =
  | FBHere : (unit, unit, Value.spawn_location) fbuiltin
  | FBIntToString : (unit, int * unit, string) fbuiltin
  | FBLength : (unit option, llist * unit, int) fbuiltin
  | FBRecv : (unit option, unit, unit) fbuiltin
  | FBSelf : (unit, unit, process) fbuiltin
  | FBSend : (unit option, process * (unit * unit), unit list) fbuiltin
  | FBSpawnAngelAt : (unit option, Value.spawn_location * ((unit * unit -> unit) * unit), process) fbuiltin
  | FBSpawnAt : (unit option, Value.spawn_location * ((unit * unit -> unit) * unit), process) fbuiltin
  | FBWait : (unit option, process * unit, unit) fbuiltin
type func =
  | FFunction : ('a, 'b) func' -> func
  | FContinuationStart : 'b fstart -> func
  | FHandler : ('a, 'c, 'b) fhandler -> func
  | FBuiltin : mfunid * ('g, 'a, 'b) fbuiltin -> func
type 'a modu = {
  mod_imports       : (string * string) list;
  mod_nfuns         : int32;
  mod_funs          : func list;
  mod_neffs         : int32;
  mod_effs          : EffectIDSet.t;
  mod_nglobals      : int32;
  mod_global_vars   : (mvarid * anytyp * string option) list;
  mod_locals        : anytyp list;
  mod_main          : 'a typ;
  mod_block         : 'a block;
  mod_has_processes : bool;
}

val module_of_ir : 'a Wasmir.modu -> 'a modu
