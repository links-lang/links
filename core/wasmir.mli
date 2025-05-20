type tagid = private int
type tvarid = Types.tid      (* Type variable ID   *)
type mvarid = private int32  (* Module variable ID *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID   *)
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDSet : Utility.Set.S with type elt = meffid

type process = private Process
type llist = private LinksList
type variant = private Variant
type abs_closure_content = private AbsClosureContent
type 'a closure_content = private ClosureContent of 'a
type 'a continuation = private Continuation of 'a

type !'a generalization =
  | Gnil : unit generalization
  | Gcons : tvarid * 'a generalization -> 'a option generalization
type anygeneralization = AG : 'a generalization -> anygeneralization

type !'a typ =
  | TInt : int typ
  | TBool : bool typ
  | TFloat : float typ
  | TString : string typ
  | TClosed : 'c generalization * 'a typ_list * 'b typ -> ('c * 'a -> 'b) typ
  | TAbsClosArg : abs_closure_content typ
  | TClosArg : 'a typ_list -> 'a closure_content typ (* TODO: merge this with TTuple? It still makes sense to keep them separate though *)
  | TCont : 'a typ -> 'a continuation typ
  | TTuple : 'a named_typ_list -> 'a list typ
  | TVariant : variant typ
  | TList : 'a typ -> llist typ
  | TVar : tvarid -> unit typ
  | TSpawnLocation : Value.spawn_location typ
  | TProcess : process typ
and !'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list
and !'a named_typ_list =
  | NTLnil : unit named_typ_list
  | NTLcons : string * 'a typ * 'b named_typ_list -> ('a * 'b) named_typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list
type anynamed_typ_list = NamedTypeList : 'a named_typ_list -> anynamed_typ_list

type (!'a, !'b) extract_typ_check
type (!'a, !'b) extract_typ = 'a list typ * int * 'b typ * ('a, 'b) extract_typ_check

type ('a, 'r) unop =
  | UONot  : (bool,  bool)  unop
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
  | BOEq : 'a typ -> ('a, 'a, bool) binop
  | BONe : 'a typ -> ('a, 'a, bool) binop
  | BOLe : 'a typ -> ('a, 'a, bool) binop
  | BOLt : 'a typ -> ('a, 'a, bool) binop
  | BOGe : 'a typ -> ('a, 'a, bool) binop
  | BOGt : 'a typ -> ('a, 'a, bool) binop
  | BOConcat : (string, string, string) binop
  | BOCons : 'a typ -> ('a, llist, llist) binop
  | BOConcatList : 'a typ -> (llist, llist, llist) binop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c, 'ga, 'gc) funcid = private ('ga generalization * 'gc generalization * 'a typ_list * 'b typ * 'c typ_list * mfunid)
  (* Top abstraction, closure abstraction, arguments type, return type, closure type, closure type ID, module-level function ID *)
type 'a effectid = private ('a typ_list * meffid)

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type (!'a, !'b) box =
  | BNone : 'a typ * 'a typ -> ('a, 'a) box
  | BClosed : 'g generalization * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b, 'g * 'c -> 'd) box
  | BCont : ('b, 'd) box -> ('b continuation, 'd continuation) box
  | BTuple : 'a named_typ_list * 'b named_typ_list -> ('a list, 'b list) box
  | BBox : 'a typ * tvarid -> ('a, unit) box
and (!'a, !'b) box_list =
  | BLnil : (unit, unit) box_list
  | BLcons : ('a, 'b) box * ('c, 'd) box_list -> ('a * 'c, 'b * 'd) box_list

val src_of_box : 'a 'b. ('a, 'b) box -> 'a typ
val src_of_box_list : 'a 'b. ('a, 'b) box_list -> 'a typ_list

val compose_box : ('a, 'b) box -> ('b, 'c) box -> ('a, 'c) box
val compose_box_list : ('a, 'b) box_list -> ('b, 'c) box_list -> ('a, 'c) box_list

type (!'a, !'b) specialization =
  | Snil : 'a generalization -> ('a, 'a) specialization
  | Scons : anytyp * tvarid * ('a, 'b) specialization -> ('a, 'b option) specialization

type ('a, 'b) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap of 'a varid * 'b typ * 'b block
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
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
  | ETuple : 'a named_typ_list * 'a expr_list -> 'a list expr
  | EExtract : 'a list expr * ('a, 'b) extract_typ -> 'b expr
  | EVariant : tagid * 'a typ * 'a expr -> variant expr
  | EListNil : 'a typ -> llist expr
  | EListHd : llist expr * 'a typ -> 'a expr
  | EListTl : 'a typ * llist expr -> llist expr
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EClose : ('a, 'b, 'c, 'ga, 'gc) funcid * ('d, 'c) box_list * 'd expr_list -> ('ga * 'a -> 'b) expr
  | ERawClose : ('a, 'b, 'c, 'ga, 'gc) funcid * abs_closure_content expr -> ('ga * 'a -> 'b) expr
  | ESpecialize : ('ga * 'a -> 'b) expr * ('gc, 'ga) specialization * ('c, 'a) box_list * ('d, 'b) box -> ('gc * 'c -> 'd) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ * 'b expr * 'c typ_list * 'c expr_list *
                      abs_closure_content expr * 'd typ -> 'd expr
  (* ^ Internal use only: pass the arguments in a struct without modification (no closure de/construction) *)
      (* FIXME: add information in the continuation that it takes a 'b *)
  | ECallClosed : (unit * 'a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : 'a typ * bool expr * 'a block * 'a block -> 'a expr
  | EDo : 'a effectid * 'b typ * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'a, 'c, unit, unit) funcid * 'c expr_list * ('a, 'b) finisher * ('a, 'b) handler list -> 'b expr
  | EDeepHandle : (unit, 'b, 'c, unit, unit) funcid * 'c expr_list *
                  ('b continuation * ('c closure_content * 'f), 'd, 'e, unit, unit) funcid * 'e expr_list * 'f expr_list -> 'd expr
and ('a, 'b) handler = (* The continuation itself returns 'a, the handler returns 'b *)
  (* Note: we lose the information that the continuation takes 'b as parameter(s) *)
  | Handler : 'a effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and 'a expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

val typ_of_expr : 'a expr -> 'a typ

type ('a, 'b) func' = {
  fun_id               : mfunid;
  fun_export_data      : string option;
  fun_converted_closure: (anytyp_list * mvarid) option;
  fun_args             : 'a typ_list;
  fun_ret              : 'b typ;
  fun_locals           : anytyp list;
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
type ('g, 'a, 'b) fbuiltin =
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

type process_level =
  | PL_NoProcess
  | PL_MessageBox
  | PL_SingleThread
  | PL_MultiThread
  | PL_MultiWait
  | PL_MultiAngel
  | PL_MultiAngelWait

type 'a modu = {
  mod_tags          : tagid Env.String.t;
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
  mod_process_level : process_level;
}

type anymodule = Module : 'a modu -> anymodule
val module_of_ir : Ir.program -> string Env.Int.t -> Ir.binding list -> anymodule

val convert_datatype : Types.datatype -> anytyp
val convert_field_spec_map : Types.field_spec_map -> anynamed_typ_list
