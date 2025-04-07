(* TODO: rethink how deep handlers work, so that we don't have to call another
   function in the (most common?) case of tail resumption *)

type tagid = private int
type tvarid = Types.tid      (* Type variable ID   *)
type mtypid                  (* Module type ID     *)
type mvarid = private int32  (* Module variable ID *)
type mfunid = private int32  (* Module function ID *)
type meffid = private int32  (* Module effect ID   *)
module MTypMap : Utility.Map.S with type key = mtypid
module FunIDMap : Utility.Map.S with type key = mfunid
module EffectIDMap : Utility.Map.S with type key = meffid

type 'a llist = private LinksList of 'a
type variant = private Variant
type abs_closure_content = private AbsClosureContent
type 'a closure_content = private ClosureContent of 'a
type 'a continuation = private Continuation of 'a

type 'a generalization =
  | Gnil : unit generalization
  | Gcons : tvarid * 'a generalization -> 'a option generalization
type anygeneralization = AG : 'a generalization -> anygeneralization

type 'a typ =
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
  | TList : 'a typ -> 'a llist typ
  | TVar : tvarid -> unit typ
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
type ('a, 'b) extract_typ = 'a list typ * int * 'b typ * ('a, 'b) extract_typ_check

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
  | BOConcat : (string, string, string) binop
  | BOCons : 'a typ -> ('a, 'a llist, 'a llist) binop
  | BOConcatList : 'a typ -> ('a llist, 'a llist, 'a llist) binop

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = private ('a typ * mvarid)
type ('a, 'b, 'c, 'ga, 'gc) funcid = private ('ga generalization * 'gc generalization * 'a typ_list * 'b typ * 'c typ_list * mfunid)
  (* Top abstraction, closure abstraction, arguments type, return type, closure type, closure type ID, module-level function ID *)
type ('a, 'b) effectid = private ((unit * 'a -> 'b) typ * meffid)

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type ('a, 'b) box =
  | BNone : 'a typ * 'a typ -> ('a, 'a) box
  | BClosed : 'g generalization * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b, 'g * 'c -> 'd) box
  | BCont : ('b, 'd) box -> ('b continuation, 'd continuation) box
  | BTuple : ('a, 'b) box_named_list -> ('a list, 'b list) box
  | BBox : 'a typ * tvarid -> ('a, unit) box
and ('a, 'b) box_list =
  | BLnil : (unit, unit) box_list
  | BLcons : ('a, 'b) box * ('c, 'd) box_list -> ('a * 'c, 'b * 'd) box_list
and ('a, 'b) box_named_list =
  | BNLnil : (unit, unit) box_named_list
  | BNLcons : string * ('a, 'b) box * ('c, 'd) box_named_list -> ('a * 'c, 'b * 'd) box_named_list

val src_of_box : 'a 'b. ('a, 'b) box -> 'a typ
val src_of_box_list : 'a 'b. ('a, 'b) box_list -> 'a typ_list

val compose_box : ('a, 'b) box -> ('b, 'c) box -> ('a, 'c) box
val compose_box_list : ('a, 'b) box_list -> ('b, 'c) box_list -> ('a, 'c) box_list

type ('a, 'b) specialization =
  | Snil : 'a generalization -> ('a, 'a) specialization
  | Scons : anytyp * tvarid * ('a, 'b) specialization -> ('a, 'b option) specialization

type ('a, 'b) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap of 'a varid * 'b typ * 'b block
and 'a block = assign list * 'a expr
and assign = Assign : locality * 'a varid * 'a expr -> assign
and 'a expr =
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
  | EListNil : 'a typ -> 'a llist expr
  | EListHd : 'a llist expr * 'a typ -> 'a expr
  | EListTl : 'a typ * 'a llist expr -> 'a llist expr
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EClose : ('a, 'b, 'c, 'ga, 'gc) funcid * ('d, 'c) box_list * 'd expr_list -> ('ga * 'a -> 'b) expr
  | ESpecialize : ('ga * 'a -> 'b) expr * ('gc, 'ga) specialization * ('c, 'a) box_list * ('d, 'b) box -> ('gc * 'c -> 'd) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ * 'b expr * abs_closure_content expr * 'd typ -> 'd expr
  (* ^ Internal use only: pass the arguments in a struct without modification (no closure de/construction) *)
      (* FIXME: add information in the continuation that it takes a 'b *)
  | ECallClosed : (unit * 'a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : 'a typ * bool expr * 'a block * 'a block -> 'a expr
  | EDo : ('a, 'b) effectid * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'a, 'c, unit, unit) funcid * 'c expr_list * ('a, 'b) finisher * ('a, 'b) handler list -> 'b expr
  | EDeepHandle : (unit, 'b, 'c, unit, unit) funcid * 'c expr_list *
                  ('b continuation * ('c closure_content * unit), 'd, 'e, unit, unit) funcid * 'e expr_list -> 'd expr
  (* FIXME: cannot use funcid, as we don't know what the closure should be when building this expression
     However, we do know the correct mtypid.
  | ECont : locality * 'b continuation varid * 'a expr_list *
            ('b continuation * ('a closure_content * unit), 'd, 'e, unit, unit) funcid * locality * 'e closure_content varid -> 'd expr *)
  | ECont : locality * 'b continuation varid * 'a expr *
            (('b continuation * ('a * unit)) typ_list * 'd typ * mtypid * mfunid) * locality * mvarid -> 'd expr
and ('a, 'b) handler = (* The continuation itself returns 'a, the handler returns 'b *)
  (* Note: we lose the information that the continuation takes 'b as parameter(s) *)
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and 'a expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

type anyblock = Block : 'a typ * 'a block -> anyblock

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
  mod_typs        : anytyp_list MTypMap.t;
  mod_neffs       : int32;
  mod_effs        : anytyp_list EffectIDMap.t;
  mod_nglobals    : int32;
  mod_global_vars : (mvarid * anytyp * string) list;
  mod_locals      : anytyp list;
  mod_main        : 'a typ;
  mod_block       : 'a block;
}

type anymodule = Module : 'a modu -> anymodule
val module_of_ir : Ir.program -> string Env.Int.t -> bool -> anymodule
