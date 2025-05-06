open Ir

type tagid = int
type tvarid = Types.tid  (* Type variable ID   *)
type mvarid = int32      (* Module variable ID *)
type mfunid = int32      (* Module function ID *)
type meffid = int32      (* Module effect ID   *)
module FunIDMap = Utility.Map.Make(struct
  type t = mfunid
  let pp fmt v = Format.fprintf fmt "%lu" v
  let show v = Int32.unsigned_to_int v |> Option.get |> Int.to_string
  let compare = Int32.compare
end)
module EffectIDSet = Utility.Set.Make(struct
  type t = meffid
  let pp fmt v = Format.fprintf fmt "%lu" v
  let show v = Int32.unsigned_to_int v |> Option.get |> Int.to_string
  let compare = Int32.compare
end)

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

type (!'a, !'b) extract_typ_check =
  | ExtractO : ('a * 'b, 'a) extract_typ_check
  | ExtractS : ('b, 'c) extract_typ_check -> ('a * 'b, 'c) extract_typ_check
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

let unop_arg_typ (type a r) (op : (a, r) unop) : a typ = match op with
  | UONot -> TBool
  | UONegI -> TInt | UONegF -> TFloat
let unop_ret_typ (type a r) (op : (a, r) unop) : r typ = match op with
  | UONot -> TBool
  | UONegI -> TInt | UONegF -> TFloat
let binop_argl_typ (type a b r) (op : (a, b, r) binop) : a typ = match op with
  | BOAddI -> TInt | BOAddF -> TFloat | BOSubI -> TInt | BOSubF -> TFloat
  | BOMulI -> TInt | BOMulF -> TFloat | BODivI -> TInt | BODivF -> TFloat
  | BORemI -> TInt
  | BOEq t -> t | BONe t -> t | BOLe t -> t
  | BOLt t -> t | BOGe t -> t | BOGt t -> t
  | BOConcat -> TString
  | BOCons t -> t
  | BOConcatList t -> TList t
let binop_argr_typ (type a b r) (op : (a, b, r) binop) : b typ = match op with
  | BOAddI -> TInt | BOAddF -> TFloat | BOSubI -> TInt | BOSubF -> TFloat
  | BOMulI -> TInt | BOMulF -> TFloat | BODivI -> TInt | BODivF -> TFloat
  | BORemI -> TInt
  | BOEq t -> t | BONe t -> t | BOLe t -> t
  | BOLt t -> t | BOGe t -> t | BOGt t -> t
  | BOConcat -> TString
  | BOCons t -> TList t
  | BOConcatList t -> TList t
let binop_ret_typ (type a b r) (op : (a, b, r) binop) : r typ = match op with
  | BOAddI -> TInt | BOAddF -> TFloat | BOSubI -> TInt | BOSubF -> TFloat
  | BOMulI -> TInt | BOMulF -> TFloat | BODivI -> TInt | BODivF -> TFloat
  | BORemI -> TInt
  | BOEq _ -> TBool | BONe _ -> TBool | BOLe _ -> TBool
  | BOLt _ -> TBool | BOGe _ -> TBool | BOGt _ -> TBool
  | BOConcat -> TString
  | BOCons t -> TList t
  | BOConcatList t -> TList t

type local_storage = StorVariable | StorClosure
type locality = Global | Local of local_storage
type 'a varid = 'a typ * mvarid
type ('a, 'b, 'c, 'ga, 'gc) funcid = 'ga generalization * 'gc generalization * 'a typ_list * 'b typ * 'c typ_list * mfunid
type 'a effectid = 'a typ_list * meffid

type 'a varid_list =
  | VLnil : unit varid_list
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

type (!'a, !'b) box =
  | BNone : 'a typ * 'a typ -> ('a, 'a) box
  | BClosed : 'g generalization * ('a, 'c) box_list * ('b, 'd) box -> ('g * 'a -> 'b, 'g * 'c -> 'd) box
  | BCont : ('b, 'd) box -> ('b continuation, 'd continuation) box
  | BTuple : ('a, 'b) box_named_list -> ('a list, 'b list) box
  | BBox : 'a typ * tvarid -> ('a, unit) box
and (!'a, !'b) box_list =
  | BLnil : (unit, unit) box_list
  | BLcons : ('a, 'b) box * ('c, 'd) box_list -> ('a * 'c, 'b * 'd) box_list
and (!'a, !'b) box_named_list =
  | BNLnil : (unit, unit) box_named_list
  | BNLcons : string * ('a, 'b) box * ('c, 'd) box_named_list -> ('a * 'c, 'b * 'd) box_named_list

let rec src_of_box : 'a 'b. ('a, 'b) box -> 'a typ = fun (type a b) (b : (a, b) box) : a typ -> match b with
  | BNone (src, _) -> src
  | BClosed (gen, bargs, bret) -> TClosed (gen, src_of_box_list bargs, src_of_box bret)
  | BCont bret -> TCont (src_of_box bret)
  | BTuple bs -> TTuple (src_of_box_named_list bs)
  | BBox (src, _) -> src
and src_of_box_list : 'a 'b. ('a, 'b) box_list -> 'a typ_list = fun (type a b) (b : (a, b) box_list) : a typ_list -> match b with
  | BLnil -> TLnil
  | BLcons (hd, tl) -> TLcons (src_of_box hd, src_of_box_list tl)
and src_of_box_named_list : 'a 'b. ('a, 'b) box_named_list -> 'a named_typ_list =
  fun (type a b) (b : (a, b) box_named_list) : a named_typ_list -> match b with
  | BNLnil -> NTLnil
  | BNLcons (n, hd, tl) -> NTLcons (n, src_of_box hd, src_of_box_named_list tl)

type (!'a, !'b) specialization =
  | Snil : 'a generalization -> ('a, 'a) specialization
  | Scons : anytyp * tvarid * ('a, 'b) specialization -> ('a, 'b option) specialization

let [@tail_mod_cons] rec src_of_specialization : 'a 'b. ('a, 'b) specialization -> 'a generalization =
  fun (type a b) (b : (a, b) specialization) : a generalization -> match b with
  | Snil g -> g
  | Scons (_, _, s) -> src_of_specialization s

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
  | EVariant : tagid * 'a typ * 'a expr -> variant expr (* TODO: optimize this in the case of a TTuple? *)
  | EListNil : 'a typ -> llist expr
  | EListHd : llist expr * 'a typ -> 'a expr
  | EListTl : 'a typ * llist expr -> llist expr
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EClose : ('a, 'b, 'c, 'ga, 'gc) funcid * ('d, 'c) box_list * 'd expr_list -> ('ga * 'a -> 'b) expr
  | ERawClose : ('a, 'b, 'c, 'ga, 'gc) funcid * abs_closure_content expr -> ('ga * 'a -> 'b) expr
  | ESpecialize : ('ga * 'a -> 'b) expr * ('gc, 'ga) specialization * ('c, 'a) box_list * ('d, 'b) box -> ('gc * 'c -> 'd) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ * 'b expr * 'c typ_list * 'c expr_list *
                      abs_closure_content expr * 'd typ -> 'd expr
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

let typ_of_expr (type a) (e : a expr) : a typ = match e with
  | EUnreachable t -> t
  | EConvertClosure (_, t) -> t
  | EIgnore _ -> TTuple NTLnil
  | EConstInt _ -> TInt
  | EConstBool _ -> TBool
  | EConstFloat _ -> TFloat
  | EConstString _ -> TString
  | EUnop (UONot, _) -> TBool
  | EUnop (UONegI, _) -> TInt
  | EUnop (UONegF, _) -> TFloat
  | EBinop (BOAddI, _, _) -> TInt | EBinop (BOAddF, _, _) -> TFloat
  | EBinop (BOSubI, _, _) -> TInt | EBinop (BOSubF, _, _) -> TFloat
  | EBinop (BOMulI, _, _) -> TInt | EBinop (BOMulF, _, _) -> TFloat
  | EBinop (BODivI, _, _) -> TInt | EBinop (BODivF, _, _) -> TFloat
  | EBinop (BORemI, _, _) -> TInt
  | EBinop (BOEq _, _, _) -> TBool
  | EBinop (BONe _, _, _) -> TBool
  | EBinop (BOLe _, _, _) -> TBool
  | EBinop (BOLt _, _, _) -> TBool
  | EBinop (BOGe _, _, _) -> TBool
  | EBinop (BOGt _, _, _) -> TBool
  | EBinop (BOConcat, _, _) -> TString
  | EBinop (BOCons t, _, _) -> TList t
  | EBinop (BOConcatList t, _, _) -> TList t
  | EVariable (_, (t, _)) -> t
  | EVariant _ -> TVariant
  | ECase (_, t, _, _) -> t
  | ETuple (ts, _) -> TTuple ts
  | EExtract (_, (_, _, t, _)) -> t
  | EListNil t -> TList t
  | EListHd (_, t) -> t
  | EListTl (t, _) -> TList t
  | EClose ((g, _, args, ret, _, _), _, _) -> TClosed (g, args, ret)
  | ERawClose ((g, _, args, ret, _, _), _) -> TClosed (g, args, ret)
  | ESpecialize (_, s, bargs, bret) -> TClosed (src_of_specialization s, src_of_box_list bargs, src_of_box bret)
  | ECallRawHandler (_, _, _, _, _, _, _, _, t) -> t
  | ECallClosed (_, _, t) -> t
  | ECond (t, _, _, _) -> t
  | EDo ((_, _), t, _) -> t
  | EShallowHandle (_, _, FId t, _) -> t
  | EShallowHandle (_, _, FMap (_, t, _), _) -> t
  | EDeepHandle (_, _, (_, _, _, t, _, _), _, _) -> t

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

type anymodule = Module : 'a modu -> anymodule

(* TRANSLATION *)

type backend =
  | BackendNone
  | BackendWizard

type anyblock = Block : 'a typ * 'a block -> anyblock
type anyexpr = Expr : 'a typ * 'a expr -> anyexpr
type anyexpr_list = ExprList : 'a typ_list * 'a expr_list -> anyexpr_list

let internal_error message = Errors.internal_error ~filename:"wasmir.ml" ~message

let rec assert_eq_generalization : 'a 'b. 'a generalization -> 'b generalization -> string -> ('a, 'b) Type.eq =
  fun (type a b) (t1 : a generalization) (t2 : b generalization) (onfail : string) : (a, b) Type.eq -> match t1, t2 with
  | Gnil, Gnil -> Type.Equal
  | Gnil, _ | _, Gnil -> raise (internal_error onfail)
  | Gcons (hd1, tl1), Gcons (hd2, tl2) ->
      if hd1 <> hd2 then raise (internal_error onfail) else let Type.Equal = assert_eq_generalization tl1 tl2 onfail in Type.Equal
let rec assert_eq_typ : 'a 'b. 'a typ -> 'b typ -> string -> ('a, 'b) Type.eq =
  fun (type a b) (t1 : a typ) (t2 : b typ) (onfail : string) : (a, b) Type.eq -> match t1, t2 with
  | TInt, TInt -> Type.Equal
  | TInt, _ | _, TInt -> raise (internal_error onfail)
  | TBool, TBool -> Type.Equal
  | TBool, _ | _, TBool -> raise (internal_error onfail)
  | TFloat, TFloat -> Type.Equal
  | TFloat, _ | _, TFloat -> raise (internal_error onfail)
  | TString, TString -> Type.Equal
  | TString, _ | _, TString -> raise (internal_error onfail)
  | TClosed (g1, tl1, r1), TClosed (g2, tl2, r2) ->
      let Type.Equal, Type.Equal, Type.Equal = assert_eq_generalization g1 g2 onfail, assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ r1 r2 onfail in Type.Equal
  | TClosed _, _ | _, TClosed _ -> raise (internal_error onfail)
  | TAbsClosArg, TAbsClosArg -> Type.Equal
  | TAbsClosArg, _ | _, TAbsClosArg -> raise (internal_error onfail)
  | TClosArg t1, TClosArg t2 -> let Type.Equal = assert_eq_typ_list t1 t2 onfail in Type.Equal
  | TClosArg _, _ | _, TClosArg _ -> raise (internal_error onfail)
  | TCont t1, TCont t2 -> let Type.Equal = assert_eq_typ t1 t2 onfail in Type.Equal
  | TCont _, _ | _, TCont _ -> raise (internal_error onfail)
  | TTuple ntl1, TTuple ntl2 -> let Type.Equal = assert_eq_named_typ_list ntl1 ntl2 onfail in Type.Equal
  | TTuple _, _ | _, TTuple _ -> raise (internal_error onfail)
  | TVariant, TVariant -> Type.Equal
  | TVariant, _ | _, TVariant -> raise (internal_error onfail)
  | TList _, TList _ -> Type.Equal
  | TList _, _ | _, TList _ -> raise (internal_error onfail)
  | TVar i1, TVar i2 -> if i1 = i2 then Type.Equal else raise (internal_error onfail)
  | TVar _, _ | _, TVar _ -> raise (internal_error onfail)
  | TSpawnLocation, TSpawnLocation -> Type.Equal
  | TSpawnLocation, _ | _, TSpawnLocation -> raise (internal_error onfail)
  | TProcess, TProcess -> Type.Equal
and assert_eq_typ_list : 'a 'b. 'a typ_list -> 'b typ_list -> string -> ('a, 'b) Type.eq =
  fun (type a b) (t1 : a typ_list) (t2 : b typ_list) (onfail : string) : (a, b) Type.eq -> match t1, t2 with
  | TLnil, TLnil -> Type.Equal
  | TLcons _, TLnil | TLnil, TLcons _ -> raise (internal_error onfail)
  | TLcons (t1, tl1), TLcons (t2, tl2) -> let Type.Equal, Type.Equal = assert_eq_typ_list tl1 tl2 onfail, assert_eq_typ t1 t2 onfail in Type.Equal
and assert_eq_named_typ_list : 'a 'b. 'a named_typ_list -> 'b named_typ_list -> string -> ('a, 'b) Type.eq =
  fun (type a b) (t1 : a named_typ_list) (t2 : b named_typ_list) (onfail : string) : (a, b) Type.eq -> match t1, t2 with
  | NTLnil, NTLnil -> Type.Equal
  | NTLcons _, NTLnil | NTLnil, NTLcons _ -> raise (internal_error onfail)
  | NTLcons (n1, t1, ntl1), NTLcons (n2, t2, ntl2) ->
      if String.equal n1 n2 then let Type.Equal, Type.Equal = assert_eq_typ t1 t2 onfail, assert_eq_named_typ_list ntl1 ntl2 onfail in Type.Equal
      else raise (internal_error onfail)
let target_expr (type a) (Expr (t1, e) : anyexpr) (t2 : a typ) : a expr = let Type.Equal = assert_eq_typ t1 t2 "Unexpected type" in e

type anyunop = Unop : ('a, 'b) unop -> anyunop
type anybinop = Binop : ('a, 'b, 'c) binop -> anybinop

(* let rec extract_typ_check : 'a 'b. ('a, 'b) extract_typ -> _ = fun (type a b) ((s, n, t, chk) : (a, b) extract_typ) : unit -> match s, chk with
  | TTuple (NTLcons (_, hd, _)), ExtractO ->
      if n = 0 then let Type.Equal = assert_eq_typ hd t "Invalid type extraction data" in ()
      else raise (internal_error "Invalid type extraction data")
  | TTuple (NTLcons (_, _, tl)), ExtractS chk -> extract_typ_check (TTuple tl, n - 1, t, chk)
  | TTuple NTLnil, _ -> . *)

type anyfbuiltin = AFBt : ('g, 'a, 'b) fbuiltin -> anyfbuiltin
type anyfunc' = AF' : ('a, 'b) func' -> anyfunc'
module Builtins : sig
  type t
  val empty : t
  
  val get_unop : string -> anytyp list -> anytyp option -> anyunop option
  val get_binop : string -> anytyp list -> anytyp option -> anybinop option
  val gen_impure : t -> 'a -> ('a -> anyfbuiltin -> 'a * mfunid) -> ('a -> 'a) -> (Types.typ -> anytyp) ->
                   string -> tyarg list -> anyexpr_list -> t * 'a * anyexpr
  
  val get_var : t -> 'a -> ('a -> 'a * mfunid * 'b) -> ('a -> 'b -> anyfunc' -> 'a) ->
                ('a -> anyfbuiltin -> 'a * mfunid) -> ('a -> 'a) -> (Types.typ -> anytyp) ->
                string -> (t * 'a * anyexpr) option
  
  val apply_type :
    Types.Abstype.t -> anytyp list -> (anytyp -> 'a) -> (tyvar list -> Types.typ -> Types.typ -> Types.typ -> 'a) ->
    (Types.field_spec_map -> Types.meta_row_var -> bool -> 'a) -> 'a
end = struct
  open Utility
  
  let rec compare_generalization : type a b. a generalization -> b generalization -> int = fun g1 g2 -> match g1, g2 with
    | Gnil, Gnil -> 0 | Gnil, _ -> ~-1 | _, Gnil -> 1
    | Gcons (n1, g1), Gcons (n2, g2) ->
        let c = Int.compare n1 n2 in if c <> 0 then c else
        compare_generalization g1 g2
  let rec compare_typ (Type t1 : anytyp) (Type t2 : anytyp) = match t1, t2 with
    | TInt, TInt -> 0 | TInt, _ -> ~-1 | _, TInt -> 1
    | TBool, TBool -> 0 | TBool, _ -> ~-1 | _, TBool -> 1
    | TFloat, TFloat -> 0 | TFloat, _ -> ~-1 | _, TFloat -> 1
    | TString, TString -> 0 | TString, _ -> ~-1 | _, TString -> 1
    | TClosed (gen1, args1, ret1), TClosed (gen2, args2, ret2) ->
        let c = compare_generalization gen1 gen2 in if c <> 0 then c else
        let c = compare_typ (Type ret1) (Type ret2) in if c <> 0 then c else
        compare_typ_list (TypeList args1) (TypeList args2)
    | TClosed _, _ -> ~-1 | _, TClosed _ -> 1
    | TAbsClosArg, TAbsClosArg -> 0 | TAbsClosArg, _ -> ~-1 | _, TAbsClosArg -> 1
    | TClosArg t1, TClosArg t2 -> compare_typ_list (TypeList t1) (TypeList t2) | TClosArg _, _ -> ~-1 | _, TClosArg _ -> 1
    | TCont t1, TCont t2 -> compare_typ (Type t1) (Type t2) | TCont _, _ -> ~-1 | _, TCont _ -> 1
    | TTuple tl1, TTuple tl2 -> compare_named_typ_list tl1 tl2 | TTuple _, _ -> ~-1 | _, TTuple _ -> 1
    | TVariant, TVariant -> 0 | TVariant, _ -> ~-1 | _, TVariant -> 1
    | TList t1, TList t2 -> compare_typ (Type t1) (Type t2) | TList _, _ -> ~-1 | _, TList _ -> 1
    | TVar i1, TVar i2 -> Int.compare i1 i2 | TVar _, _ -> ~-1 | _, TVar _ -> 1
    | TSpawnLocation, TSpawnLocation -> 0 | TSpawnLocation, _ -> ~-1 | _, TSpawnLocation -> 1
    | TProcess, TProcess -> 0
  and compare_typ_list (TypeList tl1 : anytyp_list) (TypeList tl2 : anytyp_list) = match tl1, tl2 with
    | TLnil, TLnil -> 0
    | TLnil, TLcons _ -> ~-1 | TLcons _, TLnil -> 1
    | TLcons (hd1, tl1), TLcons (hd2, tl2) ->
        let chd = compare_typ (Type hd1) (Type hd2) in if chd = 0 then compare_typ_list (TypeList tl1) (TypeList tl2) else chd
  and compare_named_typ_list : type a b. a named_typ_list -> b named_typ_list -> int = fun ntl1 ntl2 -> match ntl1, ntl2 with
    | NTLnil, NTLnil -> 0
    | NTLnil, NTLcons _ -> ~-1 | NTLcons _, NTLnil -> 1
    | NTLcons (_, hd1, ntl1), NTLcons (_, hd2, ntl2) -> (* Assume name keys are equal *)
        let chd = compare_typ (Type hd1) (Type hd2) in if chd = 0 then compare_named_typ_list ntl1 ntl2 else chd
  module Unop_map = Map.Make(struct
    type t = anyunop
    let compare (Unop l) (Unop r) = match l, r with
      | UONot,  UONot  -> 0 | UONot,  _ -> ~-1 | _, UONot  -> 1
      | UONegI, UONegI -> 0 | UONegI, _ -> ~-1 | _, UONegI -> 1
      | UONegF, UONegF -> 0
    let show (Unop op) = match op with
      | UONot -> "not"
      | UONegI -> "negate"
      | UONegF -> "negatef"
    let pp fmt op = Format.pp_print_string fmt (show op)
  end)
  module Binop_map = Map.Make(struct
    type t = anybinop
    let compare (Binop l) (Binop r) = match l, r with
      | BOAddI, BOAddI -> 0 | BOAddI, _ -> ~-1 | _, BOAddI -> 1
      | BOAddF, BOAddF -> 0 | BOAddF, _ -> ~-1 | _, BOAddF -> 1
      | BOSubI, BOSubI -> 0 | BOSubI, _ -> ~-1 | _, BOSubI -> 1
      | BOSubF, BOSubF -> 0 | BOSubF, _ -> ~-1 | _, BOSubF -> 1
      | BOMulI, BOMulI -> 0 | BOMulI, _ -> ~-1 | _, BOMulI -> 1
      | BOMulF, BOMulF -> 0 | BOMulF, _ -> ~-1 | _, BOMulF -> 1
      | BODivI, BODivI -> 0 | BODivI, _ -> ~-1 | _, BODivI -> 1
      | BODivF, BODivF -> 0 | BODivF, _ -> ~-1 | _, BODivF -> 1
      | BORemI, BORemI -> 0 | BORemI, _ -> ~-1 | _, BORemI -> 1
      | BOEq t1, BOEq t2 -> compare_typ (Type t1) (Type t2) | BOEq _, _ -> ~-1 | _, BOEq _ -> 1
      | BONe t1, BONe t2 -> compare_typ (Type t1) (Type t2) | BONe _, _ -> ~-1 | _, BONe _ -> 1
      | BOLe t1, BOLe t2 -> compare_typ (Type t1) (Type t2) | BOLe _, _ -> ~-1 | _, BOLe _ -> 1
      | BOLt t1, BOLt t2 -> compare_typ (Type t1) (Type t2) | BOLt _, _ -> ~-1 | _, BOLt _ -> 1
      | BOGe t1, BOGe t2 -> compare_typ (Type t1) (Type t2) | BOGe _, _ -> ~-1 | _, BOGe _ -> 1
      | BOGt t1, BOGt t2 -> compare_typ (Type t1) (Type t2) | BOGt _, _ -> ~-1 | _, BOGt _ -> 1
      | BOConcat, BOConcat -> 0 | BOConcat, _ -> ~-1 | _, BOConcat -> 1
      | BOCons t1, BOCons t2 -> compare_typ (Type t1) (Type t2) | BOCons _, _ -> ~-1 | _, BOCons _ -> 1
      | BOConcatList t1, BOConcatList t2 -> compare_typ (Type t1) (Type t2)
    let show (Binop op) = match op with
      | BOAddI -> "+"
      | BOAddF -> "+."
      | BOSubI -> "-"
      | BOSubF -> "-."
      | BOMulI -> "*"
      | BOMulF -> "*."
      | BODivI -> "/"
      | BODivF -> "/."
      | BORemI -> "%"
      | BOEq _ -> "=="
      | BONe _ -> "<>"
      | BOLe _ -> "<="
      | BOLt _ -> "<"
      | BOGe _ -> ">="
      | BOGt _ -> ">"
      | BOConcat -> "^^"
      | BOCons _ -> "Cons"
      | BOConcatList _ -> "Concat"
    let pp fmt op = Format.pp_print_string fmt (show op)
  end)
  
  type t = {
    bt_here: (unit, Value.spawn_location, unit, unit, unit) funcid option;
    bt_i2s: (int * unit, string, unit, unit, unit) funcid option;
    bt_len: (llist * unit, int, unit, unit option, unit) funcid option;
    bt_recv: (unit, unit, unit, unit option, unit) funcid option;
    bt_self: (unit, process, unit, unit, unit) funcid option;
    bt_send: (process * (unit * unit), unit list, unit, unit option, unit) funcid option;
    bt_spawnangelat: (Value.spawn_location * ((unit * unit -> unit) * unit), process, unit, unit option, unit) funcid option;
    bt_spawnat: (Value.spawn_location * ((unit * unit -> unit) * unit), process, unit, unit option, unit) funcid option;
    bt_wait: (process * unit, unit, unit, unit option, unit) funcid option;
    
    bt_unop: mfunid Unop_map.t;
    bt_binop: mfunid Binop_map.t;
  }
  let empty : t = {
    bt_here = None;
    bt_i2s = None;
    bt_len = None;
    bt_recv = None;
    bt_self = None;
    bt_send = None;
    bt_spawnangelat = None;
    bt_spawnat = None;
    bt_wait = None;
    
    bt_unop = Unop_map.empty;
    bt_binop = Binop_map.empty;
  }
  
  let find_fbuiltin (env : t) (acc : 'c) (add_builtin : 'c -> anyfbuiltin -> 'c * mfunid)
                    (type g a b) (fb : (g, a, b) fbuiltin) : t * 'c * (a, b, unit, g, unit) funcid = match env, fb with
    | { bt_here = Some f; _ }, FBHere -> env, acc, f
    | { bt_here = None; _ }, FBHere ->
        let acc, fid = add_builtin acc (AFBt FBHere) in
        let f = (Gnil, Gnil, TLnil, TSpawnLocation, TLnil, fid) in
        { env with bt_here = Some f; }, acc, f
    | { bt_i2s = Some f; _ }, FBIntToString -> env, acc, f
    | { bt_i2s = None; _ }, FBIntToString ->
        let acc, fid = add_builtin acc (AFBt FBIntToString) in
        let f = (Gnil, Gnil, TLcons (TInt, TLnil), TString, TLnil, fid) in
        { env with bt_i2s = Some f; }, acc, f
    | { bt_len = Some f; _ }, FBLength -> env, acc, f
    | { bt_len = None; _ }, FBLength ->
        let acc, fid = add_builtin acc (AFBt FBLength) in
        let f = (Gcons (0, Gnil), Gnil, TLcons (TList (TVar 0), TLnil), TInt, TLnil, fid) in
        { env with bt_len = Some f; }, acc, f
    | { bt_recv = Some f; _ }, FBRecv -> env, acc, f
    | { bt_recv = None; _ }, FBRecv ->
        let acc, fid = add_builtin acc (AFBt FBRecv) in
        let f = (Gcons (0, Gnil), Gnil, TLnil, TVar 0, TLnil, fid) in
        { env with bt_recv = Some f; }, acc, f
    | { bt_self = Some f; _ }, FBSelf -> env, acc, f
    | { bt_self = None; _ }, FBSelf ->
        let acc, fid = add_builtin acc (AFBt FBSelf) in
        let f = (Gnil, Gnil, TLnil, TProcess, TLnil, fid) in
        { env with bt_self = Some f; }, acc, f
    | { bt_send = Some f; _ }, FBSend -> env, acc, f
    | { bt_send = None; _ }, FBSend ->
        let acc, fid = add_builtin acc (AFBt FBSend) in
        let f = (Gcons (0, Gnil), Gnil, TLcons (TProcess, TLcons (TVar 0, TLnil)), TTuple NTLnil, TLnil, fid) in
        { env with bt_send = Some f; }, acc, f
    | { bt_spawnangelat = Some f; _ }, FBSpawnAngelAt -> env, acc, f
    | { bt_spawnangelat = None; _ }, FBSpawnAngelAt ->
        let acc, fid = add_builtin acc (AFBt FBSpawnAngelAt) in
        let f = (Gcons (0, Gnil), Gnil, TLcons (TSpawnLocation, TLcons (TClosed (Gnil, TLnil, TVar 0), TLnil)), TProcess, TLnil, fid) in
        { env with bt_spawnangelat = Some f; }, acc, f
    | { bt_spawnat = Some f; _ }, FBSpawnAt -> env, acc, f
    | { bt_spawnat = None; _ }, FBSpawnAt ->
        let acc, fid = add_builtin acc (AFBt FBSpawnAt) in
        let f = (Gcons (0, Gnil), Gnil, TLcons (TSpawnLocation, TLcons (TClosed (Gnil, TLnil, TVar 0), TLnil)), TProcess, TLnil, fid) in
        { env with bt_spawnat = Some f; }, acc, f
    | { bt_wait = Some f; _ }, FBWait -> env, acc, f
    | { bt_wait = None; _ }, FBWait ->
        let acc, fid = add_builtin acc (AFBt FBWait) in
        let f = (Gcons (0, Gnil), Gnil, TLcons (TProcess, TLnil), TVar 0, TLnil, fid) in
        { env with bt_wait = Some f; }, acc, f
  
  let unops = StringMap.from_alist ["not", Unop UONot; "negate", Unop UONegI; "negatef", Unop UONegF]
  
  type binops_val =
    | ZeroTArgs of anybinop
    | OneTArg of (anytyp -> anybinop)
  let binops =
    StringMap.from_alist [
      "+", ZeroTArgs (Binop BOAddI); "+.", ZeroTArgs (Binop BOAddF); "-", ZeroTArgs (Binop BOSubI); "-.", ZeroTArgs (Binop BOSubF);
      "*", ZeroTArgs (Binop BOMulI); "*.", ZeroTArgs (Binop BOMulF); "/", ZeroTArgs (Binop BODivI); "/.", ZeroTArgs (Binop BODivF);
      "%", ZeroTArgs (Binop BORemI); "==", OneTArg (fun (Type t) -> Binop (BOEq t)); "<>", OneTArg (fun (Type t) -> Binop (BONe t));
      "<=", OneTArg (fun (Type t) -> Binop (BOLe t)); "<", OneTArg (fun (Type t) -> Binop (BOLt t));
      ">=", OneTArg (fun (Type t) -> Binop (BOGe t)); ">", OneTArg (fun (Type t) -> Binop (BOGt t));
      "^^", ZeroTArgs (Binop BOConcat);
      "Cons", OneTArg (fun (Type t) -> Binop (BOCons t));
      "Concat", OneTArg (fun (Type t) -> Binop (BOConcatList t));
    ]
  
  let assert_eq_typ_unop  (Unop  op) (Type tc) = let Type.Equal = assert_eq_typ tc (unop_ret_typ  op) "Invalid type coercion" in ()
  let assert_eq_typ_binop (Binop op) (Type tc) = let Type.Equal = assert_eq_typ tc (binop_ret_typ op) "Invalid type coercion" in ()
  let get_unop op _tyargs otc = match StringMap.find_opt op unops with
    | None -> None
    | Some u -> Option.iter (assert_eq_typ_unop u) otc; Some u
  let get_binop op tyargs otc = match StringMap.find_opt op binops, tyargs with
    | None, _ -> None
    | Some (ZeroTArgs bo), [] -> Option.iter (assert_eq_typ_binop bo) otc; Some bo
    | Some (ZeroTArgs _), _ :: _ -> raise (internal_error ("Too many type applications to builtin binary operator '" ^ op ^ "'"))
    | Some (OneTArg bo), [t] -> let bo = bo t in Option.iter (assert_eq_typ_binop bo) otc; Some bo
    | Some (OneTArg _), [] -> raise (internal_error ("Missing type application to builtin binary operator '" ^ op ^ "'"))
    | Some (OneTArg _), _ :: _ :: _ -> raise (internal_error ("Too many type applications to builtin binary operator '" ^ op ^ "'"))
  
  let gen_impure (env : t) (acc : 'c) (add_builtin : 'a -> anyfbuiltin -> 'a * mfunid) (has_process : 'a -> 'a)
                 (convert_type : Types.typ -> anytyp) (op : string) (tyargs : tyarg list)
                 (ExprList (targs, args) : anyexpr_list) : t * 'c * anyexpr = match op with
    | "error" -> begin match tyargs with
        | [] | [_] -> raise (internal_error "Not enough type argument for builtin function 'error'")
        | _ :: _ :: _ :: _ -> raise (internal_error "Too many type argument for builtin function 'error'")
        | [CommonTypes.PrimaryKind.(Presence | Type), _; _, _]
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.(Presence | Row), _] ->
             raise (internal_error "Invalid kind of type argument for builtin function 'error'")
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t] ->
        let Type t = convert_type t in
        match targs, args with
        | TLcons (_, TLnil), ELcons (_, ELnil) -> env, acc, Expr (t, EUnreachable t) (* TODO: add an error message *)
        | _, ELnil -> raise (internal_error ("Not enough arguments for builtin function 'error'"))
        | _, ELcons (_, ELcons _) -> raise (internal_error ("Too many arguments for builtin function 'error'"))
      end
    | "debug" -> begin match tyargs, targs, args with
        | ([] | [CommonTypes.PrimaryKind.Row, _]), TLcons (argt, TLnil), ELcons (arg, ELnil) ->
            let Type.Equal = assert_eq_typ argt TString "Invalid type of argument of debug" in
            (* FIXME: allow debugging *)
            env, acc, Expr (TTuple NTLnil, EIgnore (TString, arg))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'debug'"))
      end
    | "not" -> begin match tyargs, targs, args with
        | ([] | [CommonTypes.PrimaryKind.Row, _]), TLcons (argt, TLnil), ELcons (arg, ELnil) ->
            let Type.Equal = assert_eq_typ argt TBool "Invalid type of argument of not" in
            env, acc, Expr (TBool, EUnop (UONot, arg))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'not'"))
      end
    | "/" -> begin match tyargs, targs, args with
        | ([] | [CommonTypes.PrimaryKind.Row, _]), TLcons (argtl, TLcons (argtr, TLnil)), ELcons (argl, ELcons (argr, ELnil)) ->
            let Type.Equal = assert_eq_typ argtl TInt "Invalid type of argument of /" in
            let Type.Equal = assert_eq_typ argtr TInt "Invalid type of argument of /" in
            env, acc, Expr (TInt, EBinop (BODivI, argl, argr))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin '/'"))
      end
    | "mod" -> begin match tyargs, targs, args with
        | ([] | [CommonTypes.PrimaryKind.Row, _]), TLcons (argtl, TLcons (argtr, TLnil)), ELcons (argl, ELcons (argr, ELnil)) ->
            let Type.Equal = assert_eq_typ argtl TInt "Invalid type of argument of mod" in
            let Type.Equal = assert_eq_typ argtr TInt "Invalid type of argument of mod" in
            env, acc, Expr (TInt, EBinop (BORemI, argl, argr))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'mod'"))
      end
    | "$$hd" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO $$hd without TApp"
        | [CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _], TLcons (argt, TLnil), ELcons (arg, ELnil) ->
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ argt (TList t) "Invalid type of argument of $$hd" in
            env, acc, Expr (t, EListHd (arg, t))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin '$$hd'"))
      end
    | "$$tl" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO $$tl without TApp"
        | [CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _], TLcons (argt, TLnil), ELcons (arg, ELnil) ->
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ argt (TList t) "Invalid type of argument of $$tl" in
            env, acc, Expr (TList t, EListTl (argt, arg))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin '$$tl'"))
      end
    | "intToString" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO intToString without TApp"
        | [CommonTypes.PrimaryKind.Row, _], TLcons (argt, TLnil), ELcons (arg, ELnil) ->
            let Type.Equal = assert_eq_typ argt TInt "Invalid type of argument of intToString" in
            let env, acc, fid = find_fbuiltin env acc add_builtin FBIntToString in
            env, acc, Expr (TString, ECallClosed (EClose (fid, BLnil, ELnil), ELcons (arg, ELnil), TString))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'intToString'"))
      end
    | "here" -> begin match tyargs, args with
        | [], _ -> failwith "TODO here without TApp"
        | [CommonTypes.PrimaryKind.Row, _], ELnil ->
            let env, acc, fid = find_fbuiltin env acc add_builtin FBHere in
            env, acc, Expr (TSpawnLocation, ECallClosed (EClose (fid, BLnil, ELnil), ELnil, TSpawnLocation))
        | _, _ -> raise (internal_error ("Invalid usage of builtin 'here'"))
      end
    | "self" -> begin match tyargs, args with
        | [], _ -> failwith "TODO self without TApp"
        | [CommonTypes.PrimaryKind.Presence, _; CommonTypes.PrimaryKind.Row, _], ELnil ->
            let env, acc, fid = find_fbuiltin env acc add_builtin FBSelf in
            env, acc, Expr (TProcess, ECallClosed (EClose (fid, BLnil, ELnil), ELnil, TProcess))
        | _, _ -> raise (internal_error ("Invalid usage of builtin 'self'"))
      end
    | "recv" -> begin match tyargs, args with
        | [], _ -> failwith "TODO recv without TApp"
        | [CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _], ELnil ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let env, acc, fid = find_fbuiltin env acc add_builtin FBRecv in
            env, acc, Expr (t, ECallClosed (ESpecialize (EClose (fid, BLnil, ELnil), Scons (Type t, 0, Snil Gnil), BLnil, BBox (t, 0)), ELnil, t))
        | _, _ -> raise (internal_error ("Invalid usage of builtin 'recv'"))
      end
    | "Send" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO Send without TApp"
        | [CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Row, _],
          TLcons (proct, TLcons (argt, TLnil)), ELcons (proc, ELcons (arg, ELnil)) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ proct TProcess "Invalid type of argument of Send" in
            let Type.Equal = assert_eq_typ argt t "Invalid type of argument of Send" in
            let env, acc, fid = find_fbuiltin env acc add_builtin FBSend in
            env, acc, Expr (TTuple NTLnil, ECallClosed (
              ESpecialize (
                EClose (fid, BLnil, ELnil),
                Scons (Type t, 0, Snil Gnil),
                BLcons (BNone (TProcess, TProcess), BLcons (BBox (argt, 1), BLnil)),
                BNone (TTuple NTLnil, TTuple NTLnil)),
              ELcons (proc, ELcons (arg, ELnil)),
              TTuple NTLnil))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'Send'"))
      end
    | "spawn" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO spawn without TApp"
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _],
          TLcons (tf, TLnil), ELcons (f, ELnil) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ tf (TClosed (Gnil, TLnil, t)) "Invalid type of argument of spawn" in
            let env, acc, hereid = find_fbuiltin env acc add_builtin FBHere in
            let env, acc, spawnid = find_fbuiltin env acc add_builtin FBSpawnAt in
            env, acc, Expr (TProcess,
              ECallClosed (
                ESpecialize (
                  EClose (spawnid, BLnil, ELnil),
                  Scons (Type t, 0, Snil Gnil),
                  BLcons (BNone (TSpawnLocation, TSpawnLocation), (BLcons (BClosed (Gnil, BLnil, BBox (t, 0)), BLnil))),
                  BNone (TProcess, TProcess)),
                ELcons (ECallClosed (EClose (hereid, BLnil, ELnil), ELnil, TSpawnLocation),
                  ELcons (f,
                  ELnil)),
                TProcess))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'spawn'"))
      end
    | "spawnAngel" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO spawnAngel without TApp"
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _],
          TLcons (tf, TLnil), ELcons (f, ELnil) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ tf (TClosed (Gnil, TLnil, t)) "Invalid type of argument of spawnAngel" in
            let env, acc, hereid = find_fbuiltin env acc add_builtin FBHere in
            let env, acc, spawnid = find_fbuiltin env acc add_builtin FBSpawnAngelAt in
            env, acc, Expr (TProcess,
              ECallClosed (
                ESpecialize (
                  EClose (spawnid, BLnil, ELnil),
                  Scons (Type t, 0, Snil Gnil),
                  BLcons (BNone (TSpawnLocation, TSpawnLocation), (BLcons (BClosed (Gnil, BLnil, BBox (t, 0)), BLnil))),
                  BNone (TProcess, TProcess)),
                ELcons (ECallClosed (EClose (hereid, BLnil, ELnil), ELnil, TSpawnLocation),
                  ELcons (f,
                  ELnil)),
                TProcess))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'spawnAngel'"))
      end
    | "spawnAngelAt" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO spawnAngelAt without TApp"
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _],
          TLcons (tl, TLcons (tf, TLnil)), ELcons (l, ELcons (f, ELnil)) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ tl TSpawnLocation "Invalid type of argument of spawnAngelAt" in
            let Type.Equal = assert_eq_typ tf (TClosed (Gnil, TLnil, t)) "Invalid type of argument of spawnAngelAt" in
            let env, acc, spawnid = find_fbuiltin env acc add_builtin FBSpawnAngelAt in
            env, acc, Expr (TProcess,
              ECallClosed (
                ESpecialize (
                  EClose (spawnid, BLnil, ELnil),
                  Scons (Type t, 0, Snil Gnil),
                  BLcons (BNone (TSpawnLocation, TSpawnLocation), (BLcons (BClosed (Gnil, BLnil, BBox (t, 0)), BLnil))),
                  BNone (TProcess, TProcess)),
                ELcons (l, ELcons (f, ELnil)),
                TProcess))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'spawnAngelAt'"))
      end
    | "spawnAt" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO spawnAt without TApp"
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _],
          TLcons (tl, TLcons (tf, TLnil)), ELcons (l, ELcons (f, ELnil)) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ tl TSpawnLocation "Invalid type of argument of spawnAt" in
            let Type.Equal = assert_eq_typ tf (TClosed (Gnil, TLnil, t)) "Invalid type of argument of spawnAt" in
            let env, acc, spawnid = find_fbuiltin env acc add_builtin FBSpawnAt in
            env, acc, Expr (TProcess,
              ECallClosed (
                ESpecialize (
                  EClose (spawnid, BLnil, ELnil),
                  Scons (Type t, 0, Snil Gnil),
                  BLcons (BNone (TSpawnLocation, TSpawnLocation), (BLcons (BClosed (Gnil, BLnil, BBox (t, 0)), BLnil))),
                  BNone (TProcess, TProcess)),
                ELcons (l, ELcons (f, ELnil)),
                TProcess))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'spawnAt'"))
      end
    | "spawnWait" -> begin match tyargs, targs, args with
        | [], _, _ -> failwith "TODO spawnWait without TApp"
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _],
          TLcons (tf, TLnil), ELcons (f, ELnil) ->
            let acc = has_process acc in
            let Type t = convert_type t in
            let Type.Equal = assert_eq_typ tf (TClosed (Gnil, TLnil, t)) "Invalid type of argument of spawnWait" in
            let env, acc, hereid = find_fbuiltin env acc add_builtin FBHere in
            let env, acc, spawnid = find_fbuiltin env acc add_builtin FBSpawnAt in
            let env, acc, waitid = find_fbuiltin env acc add_builtin FBWait in
            env, acc, Expr (t,
              ECallClosed (
                ESpecialize (
                  EClose (waitid, BLnil, ELnil),
                  Scons (Type t, 0, Snil Gnil),
                  BLcons (BNone (TProcess, TProcess), BLnil),
                  BBox (t, 0)),
                ELcons (
                  ECallClosed (
                    ESpecialize (
                      EClose (spawnid, BLnil, ELnil),
                      Scons (Type t, 0, Snil Gnil),
                      BLcons (BNone (TSpawnLocation, TSpawnLocation), (BLcons (BClosed (Gnil, BLnil, BBox (t, 0)), BLnil))),
                      BNone (TProcess, TProcess)),
                    ELcons (ECallClosed (EClose (hereid, BLnil, ELnil), ELnil, TSpawnLocation),
                      ELcons (f,
                      ELnil)),
                    TProcess),
                  ELnil),
                t))
        | _, _, _ -> raise (internal_error ("Invalid usage of builtin 'spawnWait'"))
      end
    | _ -> ignore tyargs; raise (internal_error ("Unknown builtin impure function " ^ op))
  
  let get_var (env : t) (acc : 'a) (new_fun : 'a -> 'a * mfunid * 'b) (set_fun : 'a -> 'b -> anyfunc' -> 'a)
              (add_builtin : 'a -> anyfbuiltin -> 'a * mfunid)
              (has_process : 'a -> 'a) (convert_type : Types.typ -> anytyp)
              (v : string) : (t * 'a * anyexpr) option =
    ignore (has_process, convert_type);
    match
      match StringMap.find_opt v binops with
      | Some (ZeroTArgs bo) -> Some (bo, AG Gnil)
      | Some (OneTArg bo) -> Some (bo (Type (TVar 0)), AG Gnil)
      | None -> None
    with Some (Binop bo, AG gargs) ->
        let tl = binop_argl_typ bo in
        let tr = binop_argr_typ bo in
        let tret = binop_ret_typ bo in
        let targs = TLcons (tl, TLcons (tr, TLnil)) in
        let env, acc, fid = match Binop_map.find_opt (Binop bo) env.bt_binop with Some f -> env, acc, f | None ->
            let acc, fid, fref = new_fun acc in
            let fdef = {
              fun_id = fid;
              fun_export_data = None;
              fun_converted_closure = None;
              fun_args = targs;
              fun_ret = tret;
              fun_locals = [];
              fun_block = ([], EBinop (bo, EVariable (Local StorVariable, (tl, 0l)), EVariable (Local StorVariable, (tr, 1l))));
            } in
            let acc = set_fun acc fref (AF' fdef) in
            let bt_binop = Binop_map.add (Binop bo) fid env.bt_binop in
            { env with bt_binop; }, acc, fid in
        let fid : (_ * (_ * unit), _, unit, _, unit) funcid = gargs, Gnil, targs, tret, TLnil, fid in
        Some (env, acc, Expr (TClosed (gargs, targs, tret), EClose (fid, BLnil, ELnil)))
    | None -> match StringMap.find_opt v unops with
    | Some (Unop uo) ->
        let targ = unop_arg_typ uo in
        let tret = unop_ret_typ uo in
        let env, acc, fid = match Unop_map.find_opt (Unop uo) env.bt_unop with Some f -> env, acc, f | None ->
            let acc, fid, fref = new_fun acc in
            let fdef = {
              fun_id = fid;
              fun_export_data = None;
              fun_converted_closure = None;
              fun_args = TLcons (targ, TLnil);
              fun_ret = tret;
              fun_locals = [];
              fun_block = ([], EUnop (uo, EVariable (Local StorVariable, (targ, 0l))));
            } in
            let acc = set_fun acc fref (AF' fdef) in
            let bt_unop = Unop_map.add (Unop uo) fid env.bt_unop in
            { env with bt_unop; }, acc, fid in
        let fid : (_ * unit, _, unit, unit, unit) funcid = Gnil, Gnil, TLcons (targ, TLnil), tret, TLnil, fid in
        Some (env, acc, Expr (TClosed (Gnil, TLcons (targ, TLnil), tret), EClose (fid, BLnil, ELnil)))
    | None -> match v with
    | "Nil" -> Some (env, acc, Expr (TList (TVar ~-1), EListNil (TVar ~-1)))
    | "length" ->
        let env, acc, fid = find_fbuiltin env acc add_builtin FBLength in
        Some (env, acc,
          Expr (TClosed (Gcons (0, Gnil), TLcons (TList (TVar 0), TLnil), TInt),
          EClose (fid, BLnil, ELnil)))
    | _ -> None
  
  let apply_type (at : Types.Abstype.t) (ts : anytyp list)
      (normal : anytyp -> 'a) (func : tyvar list -> Types.typ -> Types.typ -> Types.typ -> 'a)
      (row : Types.field_spec_map -> Types.meta_row_var -> bool -> 'a) : 'a =
    let is at2 = Types.Abstype.compare at at2 = 0 in
    ignore (func, row);
    if is Types.list then match ts with
      | [Type t] -> normal (Type (TList t))
      | _ -> raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
    else if is Types.spawn_location then match ts with
      | [] -> normal (Type TSpawnLocation)
      | _ -> raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
    else if is Types.process then match ts with
      | [] -> normal (Type TProcess)
      | _ -> raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
    else raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
end

let sort_name_map (nm : 'a name_map) : (string * 'a) list =
  Utility.StringMap.bindings nm (* Since binding names are unique, there is no issue with using the given list *)

let rec override_box_src : 'a 'b. 'a typ -> ('a, 'b) box -> ('a, 'b) box =
  fun (type a b) (src : a typ) (b : (a, b) box) : (a, b) box -> match b with
  | BNone (_, dst) -> BNone (src, dst)
  | BClosed (_, bargs, bret) -> begin match src with
      | TClosed (g, targs, tret) -> BClosed (g, override_box_list_src targs bargs, override_box_src tret bret)
    end
  | BCont bret -> begin match src with
      | TCont tret -> BCont (override_box_src tret bret)
    end
  | BTuple bs -> begin match src with
      | TTuple ts -> BTuple (override_box_named_list_src ts bs)
    end
  | BBox (_, id) -> BBox (src, id)
and override_box_list_src : 'a 'b. 'a typ_list -> ('a, 'b) box_list -> ('a, 'b) box_list =
  fun (type a b) (src : a typ_list) (b : (a, b) box_list) : (a, b) box_list -> match b with
  | BLnil -> BLnil
  | BLcons (bhd, btl) -> begin match src with
      | TLcons (thd, ttl) -> BLcons (override_box_src thd bhd, override_box_list_src ttl btl)
    end
and override_box_named_list_src : 'a 'b. 'a named_typ_list -> ('a, 'b) box_named_list -> ('a, 'b) box_named_list =
  fun (type a b) (src : a named_typ_list) (b : (a, b) box_named_list) : (a, b) box_named_list -> match b with
  | BNLnil -> BNLnil
  | BNLcons (n, bhd, btl) -> begin match src with
      | NTLcons (_, thd, ttl) -> BNLcons (n, override_box_src thd bhd, override_box_named_list_src ttl btl)
    end

let rec override_box_dst : 'a 'b. ('a, 'b) box -> 'b typ -> ('a, 'b) box =
  fun (type a b) (b : (a, b) box) (dst : b typ) : (a, b) box -> match b with
  | BNone (src, _) -> BNone (src, dst)
  | BClosed (g, bargs, bret) -> begin match dst with
      | TClosed (_, targs, tret) -> BClosed (g, override_box_list_dst bargs targs, override_box_dst bret tret)
    end
  | BCont bret -> begin match dst with
      | TCont tret -> BCont (override_box_dst bret tret)
    end
  | BTuple bs -> begin match dst with
      | TTuple ts -> BTuple (override_box_named_list_dst bs ts)
    end
  | BBox (src, _) -> begin match dst with
      | TVar id -> BBox (src, id)
    end
and override_box_list_dst : 'a 'b. ('a, 'b) box_list -> 'b typ_list -> ('a, 'b) box_list =
  fun (type a b) (b : (a, b) box_list) (dst : b typ_list) : (a, b) box_list -> match b with
  | BLnil -> BLnil
  | BLcons (bhd, btl) -> begin match dst with
      | TLcons (thd, ttl) -> BLcons (override_box_dst bhd thd, override_box_list_dst btl ttl)
    end
and override_box_named_list_dst : 'a 'b. ('a, 'b) box_named_list -> 'b named_typ_list -> ('a, 'b) box_named_list =
  fun (type a b) (b : (a, b) box_named_list) (dst : b named_typ_list) : (a, b) box_named_list -> match b with
  | BNLnil -> BNLnil
  | BNLcons (n, bhd, btl) -> begin match dst with
      | NTLcons (_, thd, ttl) -> BNLcons (n, override_box_dst bhd thd, override_box_named_list_dst btl ttl)
    end

let rec compose_box : 'a 'b 'c. ('a, 'b) box -> ('b, 'c) box -> ('a, 'c) box =
  fun (type a b c) (boxab : (a, b) box) (boxbc : (b, c) box) : (a, c) box -> match boxab with
  | BNone (src, _) -> override_box_src src boxbc
  | BClosed (g, bargs1, bret1) -> begin match boxbc with
    | BNone (_, dst) -> override_box_dst boxab dst
    | BClosed (_, bargs2, bret2) -> BClosed (g, compose_box_list bargs1 bargs2, compose_box bret1 bret2)
    | BBox (_, tid) -> BBox (src_of_box boxab, tid)
    end
  | BCont bret1 -> begin match boxbc with
    | BNone (_, dst) -> override_box_dst boxab dst
    | BCont bret2 -> BCont (compose_box bret1 bret2)
    | BBox (_, tid) -> BBox (src_of_box boxab, tid)
    end
  | BTuple bs1 -> begin match boxbc with
    | BNone (_, dst) -> override_box_dst boxab dst
    | BTuple bs2 -> BTuple (compose_box_named_list bs1 bs2)
    | BBox (_, tid) -> BBox (src_of_box boxab, tid)
    end
  | BBox (src, _) -> begin match boxbc with
    | BNone (_, dst) -> override_box_dst boxab dst
    | BBox (_, tid) -> BBox (src, tid)
    end
and compose_box_list : 'a 'b 'c. ('a, 'b) box_list -> ('b, 'c) box_list -> ('a, 'c) box_list =
  fun (type a b c) (bsab : (a, b) box_list) (bsbc : (b, c) box_list) : (a, c) box_list -> match bsab with
  | BLnil -> (match bsbc with BLnil -> BLnil)
  | BLcons (hd1, tl1) -> (match bsbc with BLcons (hd2, tl2) -> BLcons (compose_box hd1 hd2, compose_box_list tl1 tl2))
and compose_box_named_list : 'a 'b 'c. ('a, 'b) box_named_list -> ('b, 'c) box_named_list -> ('a, 'c) box_named_list =
  fun (type a b c) (bsab : (a, b) box_named_list) (bsbc : (b, c) box_named_list) : (a, c) box_named_list -> match bsab with
  | BNLnil -> (match bsbc with BNLnil -> BNLnil)
  | BNLcons (n, hd1, tl1) -> (match bsbc with BNLcons (_, hd2, tl2) -> BNLcons (n, compose_box hd1 hd2, compose_box_named_list tl1 tl2))

let rec generalize_of_tyvars (tvs : tyvar list) : anygeneralization = match tvs with
  | [] -> AG Gnil
  | (hd, (CommonTypes.PrimaryKind.Type, _)) :: tl -> let AG tl = generalize_of_tyvars tl in AG (Gcons (hd, tl))
  | _ :: tl -> generalize_of_tyvars tl

type anyntyp_list = NamedTypeList : 'a named_typ_list -> anyntyp_list

let _to_typelist (conv : Types.typ -> anytyp) (ts : Types.typ name_map) : anyntyp_list =
  let rec inner l = match l with
    | [] -> NamedTypeList NTLnil
    | (n, hd) :: tl -> let Type hd = conv hd in let NamedTypeList tl = inner tl in NamedTypeList (NTLcons (n, hd, tl))
  in inner (sort_name_map ts)

let rec _convert_type : type a. (_ -> a) -> (_ -> _ -> _ -> _ -> a) -> (_ -> _ -> _ -> a) -> _ -> a =
  fun (normal : anytyp -> a)
    (func : tyvar list -> Types.typ -> Types.typ -> Types.typ -> a)
    (row : Types.field_spec_map -> Types.meta_row_var -> bool -> a)
    (t : Types.typ) : a -> match t with
  | Types.Not_typed -> failwith "TODO _convert_type Not_typed"
  (* FIXME: what's the difference? *)
  | Types.Var (id, (CommonTypes.PrimaryKind.Type, (_, _)), `Flexible)
  | Types.Var (id, (CommonTypes.PrimaryKind.Type, (_, _)), `Rigid) ->
      normal (Type (TVar id))
  | Types.Var _ -> failwith "TODO _convert_type Var [non-type]"
  | Types.Recursive (_, _, t) -> _convert_type normal func row t
      (* Note: recursive types should always be broken by a Variant, otherwise we have an infinite object *)
  | Types.Alias (_, _, t) -> _convert_type normal func row t
  | Types.Application (at, ts) ->
      let ts = List.filter_map (fun (k, t) -> if k = CommonTypes.PrimaryKind.Type then Some (convert_type t) else None) ts in
      Builtins.apply_type at ts normal func row
  | Types.RecursiveApplication ra -> _convert_type normal func row Types.(ra.r_unwind ra.r_args ra.r_dual)
  | Types.Meta t -> _convert_type normal func row (Unionfind.find t)
  | Types.Primitive CommonTypes.Primitive.Bool -> normal (Type TBool)
  | Types.Primitive CommonTypes.Primitive.Int -> normal (Type TInt)
  | Types.Primitive CommonTypes.Primitive.Float -> normal (Type TFloat)
  | Types.Primitive CommonTypes.Primitive.String -> normal (Type TString)
  | Types.Primitive _ -> failwith "TODO _convert_type Primitive"
  | Types.Function (args, eff, ret) -> func [] args eff ret
  | Types.Lolli (args, eff, ret) -> func [] args eff ret (* Assume Lolli and Function are the same thing *)
  | Types.Record t -> _convert_type normal func row t
  | Types.Variant _ -> normal (Type TVariant)
  | Types.Table _ -> failwith "TODO _convert_type Table"
  | Types.Lens _ -> failwith "TODO _convert_type Lens"
  | Types.(ForAll (tv, Function (args, eff, ret)))
  | Types.(ForAll (tv, Lolli (args, eff, ret))) -> func tv args eff ret (* Uniqueness of IDs allows not having to relabel *)
  | Types.ForAll _ -> failwith "TODO _convert_type ForAll without Function or Lolli"
  | Types.Effect _ -> failwith "TODO _convert_type Effect"
  | Types.Operation _ -> failwith "TODO _convert_type Operation"
  | Types.Row (fsm, mrv, b) -> row fsm mrv b
  | Types.Closed -> failwith "TODO _convert_type Closed"
  | Types.Absent -> failwith "TODO _convert_type Absent"
  | Types.Present t -> _convert_type normal func row t
  | Types.Input _ -> failwith "TODO _convert_type Input"
  | Types.Output _ -> failwith "TODO _convert_type Output"
  | Types.Select _ -> failwith "TODO _convert_type Select"
  | Types.Choice _ -> failwith "TODO _convert_type Choice"
  | Types.Dual _ -> failwith "TODO _convert_type Dual"
  | Types.End -> failwith "TODO _convert_type End"
and convert_type (t : Types.typ) : anytyp =
  _convert_type
    (fun t -> t)
    (fun tvs args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        let AG g = generalize_of_tyvars tvs in
        Type (TClosed (g, args, ret)))
    (fun fsm _ _ -> let NamedTypeList tl = _to_typelist convert_type fsm in Type (TTuple tl))
    t
and convert_type_list (t : Types.typ) : anytyp_list =
  _convert_type
    (fun (Type t) -> TypeList (TLcons (t, TLnil)))
    (fun tvs args _eff ret ->
        let TypeList args = convert_type_list args in
        let Type ret = convert_type ret in
        let AG g = generalize_of_tyvars tvs in
        TypeList (TLcons (TClosed (g, args, ret), TLnil)))
    (fun fsm _ _ ->
        let rec inner l = match l with
          | [] -> TypeList TLnil
          | (_, hd) :: tl -> let Type hd = convert_type hd in let TypeList tl = inner tl in TypeList (TLcons (hd, tl))
        in inner (sort_name_map fsm))
    t

let rec box_list_none : type a b. (a, b) box_list -> ((a, b) Type.eq * a typ_list * b typ_list) option = function
  | BLnil -> Some (Type.Equal, TLnil, TLnil)
  | BLcons (BNone (s, d), btl) -> begin match box_list_none btl with
      | Some (Type.Equal, ss, ds) -> Some (Type.Equal, TLcons (s, ss), TLcons (d, ds))
      | None -> None
    end
  | BLcons _ -> None
let rec box_named_list_none : type a b. (a, b) box_named_list -> ((a, b) Type.eq * a named_typ_list * b named_typ_list) option = function
  | BNLnil -> Some (Type.Equal, NTLnil, NTLnil)
  | BNLcons (n, BNone (s, d), btl) -> begin match box_named_list_none btl with
      | Some (Type.Equal, ss, ds) -> Some (Type.Equal, NTLcons (n, s, ss), NTLcons (n, d, ds))
      | None -> None
    end
  | BNLcons _ -> None

module TVarMap = Types.TypeVarMap
type 'a specialize = Spec : 'a typ * ('a, 'b) box -> 'b specialize
type 'a specialize_list = SpecL : 'a typ_list * ('a, 'b) box_list -> 'b specialize_list
type 'a specialize_named_list = SpecNL : 'a named_typ_list * ('a, 'b) box_named_list -> 'b specialize_named_list
let rec specialize_typ : type a. _ -> a typ -> a specialize = fun tmap t -> match t with
  | TInt -> Spec (TInt, BNone (t, t))
  | TBool -> Spec (TBool, BNone (t, t))
  | TFloat -> Spec (TFloat, BNone (t, t))
  | TString -> Spec (TString, BNone (t, t))
  | TClosed (g, targs, tret) ->
      let SpecL (targs, bargs) = specialize_typ_list tmap targs in
      let Spec (tret, bret) = specialize_typ tmap tret in
      begin match box_list_none bargs, bret with
      | Some (Type.Equal, sargs, dargs), BNone (src, dst) -> Spec (TClosed (g, targs, tret), BNone (TClosed (g, sargs, src), TClosed (g, dargs, dst)))
      | _ -> Spec (TClosed (g, targs, tret), BClosed (g, bargs, bret))
    end
  | TAbsClosArg -> Spec (TAbsClosArg, BNone (t, t))
  | TClosArg ts -> begin let SpecL (ts, bs) = specialize_typ_list tmap ts in
      match box_list_none bs with
      | Some (Type.Equal, sb, db) -> Spec (TClosArg ts, BNone (TClosArg sb, TClosArg db))
      | None -> raise (internal_error "Cannot box in closure argument yet")
    end
  | TCont tret -> begin let Spec (tret, bret) = specialize_typ tmap tret in
      match bret with
      | BNone (sret, dret) -> Spec (TCont tret, BNone (TCont sret, TCont dret))
      | _ -> Spec (TCont tret, BCont bret)
    end
  | TTuple ts -> let SpecNL (ts, bs) = specialize_typ_named_list tmap ts in
      begin match box_named_list_none bs with
      | Some (Type.Equal, ss, ds) -> Spec (TTuple ts, BNone (TTuple (ss), TTuple (ds)))
      | None -> Spec (TTuple ts, BTuple bs)
    end
  | TVariant -> Spec (TVariant, BNone (t, t))
  | TList tc -> Spec (TList tc, BNone (t, t))
  | TVar i -> begin match TVarMap.find_opt i tmap with
      | Some (Type (TVar _ as src)) -> Spec (src, BNone (src, t))
      | Some (Type src) -> Spec (src, BBox (src, i))
      | None -> Spec (t, BNone (t, t))
    end
  | TSpawnLocation -> Spec (TSpawnLocation, BNone (t, t))
  | TProcess -> Spec (TProcess, BNone (t, t))
and specialize_typ_list : type a. _ -> a typ_list -> a specialize_list = fun tmap ts -> match ts with
  | TLnil -> SpecL (TLnil, BLnil)
  | TLcons (hd, tl) ->
      let Spec (hd, bhd) = specialize_typ tmap hd in
      let SpecL (tl, btl) = specialize_typ_list tmap tl in
      SpecL (TLcons (hd, tl), BLcons (bhd, btl))
and specialize_typ_named_list : type a. _ -> a named_typ_list -> a specialize_named_list = fun tmap ts -> match ts with
  | NTLnil -> SpecNL (NTLnil, BNLnil)
  | NTLcons (n, hd, tl) ->
      let Spec (hd, bhd) = specialize_typ tmap hd in
      let SpecNL (tl, btl) = specialize_typ_named_list tmap tl in
      SpecNL (NTLcons (n, hd, tl), BNLcons (n, bhd, btl))

type anyvarid = VarID : 'a varid -> anyvarid
type anyfuncid = FuncID : ('b, 'c, 'd, 'gb, 'gd) funcid -> anyfuncid
type anycfuncid = ACFunction : ('b, 'c, unit, 'ga, unit) funcid -> anycfuncid
type anyfhandler = AFHdl : ('a, 'c, 'b) fhandler -> anyfhandler

type anyvarid_list = VarIDList : 'a typ_list * 'a varid_list -> anyvarid_list

type closed_like =
  | Function : ('a, 'b, unit, 'ga, unit) funcid -> closed_like
  | Closure : locality * ('ga * 'a -> 'b) varid -> closed_like
  | Contin : locality * 'd continuation varid * (('d continuation * ('a * 'c)) typ_list * 'b typ * mfunid) *
             locality * mvarid -> closed_like
  | ClosedBuiltin of string

module LEnv : sig (* Contains the arguments, the local variables, etc *)
  type 'a t
  type 'a realt
  type subt
  val toplevel : unit realt
  val create_sub : 'a t -> (binder * value) list option -> subt
  
  val of_real : 'a realt -> 'a t
  val of_sub : subt -> unit t
  val to_real : 'a t -> 'a realt
  val to_sub : unit t -> subt
  
  val get_closure : subt -> subt * (mvarid * mvarid) option * anyexpr_list
  
  (* Internal use by the global env only *)
  val args_typ : 'a realt -> 'a typ_list * anytyp_list
  val find_continuation :
    'a t -> var -> ('a t * (local_storage * anytyp * anyvarid * mfunid * anytyp * local_storage * mvarid * anytyp_list)) option
  
  type args
  type anyrealt = AR : 'a realt -> anyrealt
  val no_arg : args
  val add_arg : args -> binder -> args
  val env_of_args : args -> binder option -> anyrealt * anygeneralization
  val add_closure : 'a realt -> 'a realt * (mvarid * mvarid * anytyp_list) option
  
  val [@warning "-32"] add_local : 'a t -> anytyp -> 'a t * mvarid
  val add_var : 'a t -> binder -> 'b typ -> 'a t * 'b varid
  val find_var : 'a t -> var -> ('a t * local_storage * anyvarid) option
  val find_closure : 'a t -> var -> string -> ('a t * local_storage * anyvarid) option
  val find_raw_closure : 'a t -> var -> ('a t * local_storage * abs_closure_content varid) option
  
  val set_handler_args : subt -> binder -> subt * anyvarid_list
  val add_cont : subt -> anytyp -> mfunid -> anytyp -> subt * mvarid
  val set_continuation : subt -> binder -> subt
  
  val locals_of_env : 'a realt -> anytyp list
  val compile : 'a realt -> mfunid -> string option -> 'b typ -> 'b block -> ('a, 'b) func'
  val compile_cont_start : subt -> mfunid -> 'b typ -> 'b block -> (anytyp_list * mvarid) option -> 'b fstart
  val compile_handler : subt -> mfunid -> (mvarid * mvarid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> anyfhandler
end = struct
  module IntString = Env.Make(struct
    type t = int * string
    let pp fmt (i, n) = Format.fprintf fmt "(%d, '%s')" i n
    let show (i, n) = "(" ^ (string_of_int i) ^ ", '" ^ n ^ "')"
    let compare (i1, n1) (i2, n2) =
      let c = Int.compare i1 i2 in if c <> 0 then c else String.compare n1 n2
  end)
  
  type 'a realt = {
    nargs   : int32;
    args    : 'a typ_list;
    nlocs   : int32;
    locs    : anytyp_list;
    varmap  : anyvarid Env.Int.t;
    cvarmap : anyvarid Env.String.t;
    clos    : mvarid * anytyp_list * bool; (* true if closure has been converted *)
    cbid    : var option;
  }
  type subt = {
    base           : anyt;
    nargs          : int32;
    nlocs          : int32;
    locs           : anytyp_list;
    varmap         : (local_storage * anyvarid) Env.Int.t;
    contmap        : (anytyp * anyvarid * mfunid * anytyp * mvarid * anytyp_list) Env.Int.t;
    nclos          : int32;
    clos           : anyexpr_list;
    cvarmap        : (local_storage * anyvarid) IntString.t;
    its            : anytyp_list;
    mutable contid : (anyvarid * mvarid * mfunid * anytyp * mvarid) option;
    mutable contv  : (Types.t, anytyp) Either.t * var;
  }
  and 'a t = ('a realt, subt) Either.t
  and anyt = AT : 'a t -> anyt
  
  (* Similar to rev_append args tl, but works on anytyp_lists *)
  let rec extract_to_list (TypeList args : anytyp_list) (acc : anytyp list) : anytyp list = match args with
    | TLnil -> acc
    | TLcons (hd, tl) -> extract_to_list (TypeList tl) (Type hd :: acc)
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
  
  let toplevel : unit realt = {
    nargs = 0l;
    args = TLnil;
    nlocs = 0l;
    locs = TypeList TLnil;
    varmap = Env.Int.empty;
    cvarmap = Env.String.empty;
    clos = Int32.minus_one, TypeList TLnil, true;
    cbid = None;
  }
  let create_sub (base : 'a t) (is_handler : (binder * value) list option) : subt =
    let varmap, nargs, its = match is_handler with
      | None -> Env.Int.empty, 1l, TypeList TLnil
      | Some bvs ->
          let rec inner bvs varmap nargs (TypeList its) = match bvs with
            | [] -> varmap, Int32.succ nargs, extract_args (TypeList its) (TypeList TLnil)
            | (bhd, _) :: bvs ->
                let Type t = convert_type (Var.type_of_binder bhd) in
                let its = TypeList (TLcons (t, its)) in
                inner bvs (Env.Int.bind (Var.var_of_binder bhd) (StorVariable, VarID (t, nargs)) varmap) (Int32.succ nargs) its in
          inner bvs Env.Int.empty 2l (TypeList TLnil) in
    {
      base = AT base;
      nargs;
      nlocs = 0l;
      locs = TypeList TLnil;
      varmap;
      contmap = Env.Int.empty;
      nclos = 0l;
      clos = ExprList (TLnil, ELnil);
      cvarmap = IntString.empty;
      its;
      contid = None;
      contv = (Either.Left Types.Not_typed, ~-1);
    }
  
  let of_real (env : 'a realt) : 'a t = Either.Left env
  let of_sub (env : subt) : unit t = Either.Right env
  let to_real (env : 'a t) : 'a realt = match env with Either.Left env -> env | Either.Right _ -> raise (internal_error "Invalid local environment kind")
  let to_sub (env : unit t) : subt = match env with Either.Right env -> env | Either.Left _ -> raise (internal_error "Invalid local environment kind")
  
  
  let get_closure (env : subt) : subt * (mvarid * mvarid) option * anyexpr_list =
    let cexpr = extract_exprs env.clos in
    let oconv, nlocs, locs = match cexpr with
      | ExprList (TLnil, ELnil) -> None, env.nlocs, env.locs
      | ExprList (cts, _) ->
        Some (Int32.pred env.nargs, Int32.add env.nargs env.nlocs),
          Int32.succ env.nlocs,
          (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cts, tl)))
    in { env with nlocs; locs }, oconv, cexpr
  
  let args_typ (env : 'a realt) : 'a typ_list * anytyp_list =
    env.args, (let (_, cat, _) = env.clos in cat)
  let rec find_continuation : type a. a t -> _ -> (a t * _) option = fun (env : a t) (v : var)
      : (a t * (local_storage * anytyp * anyvarid * mfunid * anytyp * local_storage * mvarid * anytyp_list)) option ->
    match env with
    | Either.Left _ -> None
    | Either.Right ({ base = AT base; its; contid; contv = targ, contv; contmap; _ } as env) -> begin match Env.Int.find_opt v contmap with
        | Some (targ, VarID (t, cid), hdlfid, thdlret, hdlcid, its) ->
            Some (Either.Right env, (StorClosure, targ, VarID (t, cid), hdlfid, thdlret, StorClosure, hdlcid, its))
        | None -> begin match
              match contid with None -> None | Some (VarID (t, cid), _, hdlfid, thdlret, hdlcid) ->
                if Var.equal_var v contv then
                  let env, targ = match targ with
                    | Either.Left targ ->
                        let targ = _convert_type
                          (fun _ -> raise (internal_error "Expected a function type, got another type"))
                          (fun g args _ _ -> match g with
                              | _ :: _ -> raise (internal_error "Invalid handler case: contains type variables")
                              | [] -> match convert_type_list args with
                                  | TypeList (TLcons (t, TLnil)) -> Type t
                                  | TypeList TLnil -> raise (internal_error "Unexpected handler type: no argument")
                                  | TypeList (TLcons (_, TLcons _)) -> raise (internal_error "Unexpected handler type: too many arguments"))
                          (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type"))
                          targ in
                        { env with contv = Either.Right targ, contv }, targ
                    | Either.Right targ -> env, targ in
                  Some (Either.Right env, (StorVariable, targ, VarID (t, cid),
                        hdlfid, thdlret, StorVariable, hdlcid, its))
                else None
              with Some v -> Some v | None ->
            match find_continuation base v with
            | None -> None
            | Some (base, (loc, targ, VarID (t, bid), hdlfid, thdlret, hdlcloc, hdlbcid, its)) ->
                let cid = env.nclos in
                let hdlcid = Int32.succ cid in
                let nclos = Int32.succ hdlcid in
                let clos =
                  let ExprList (tl, es) = env.clos in
                  ExprList (TLcons (TClosArg TLnil, TLcons (TCont t, tl)),
                            ELcons (EVariable (Local hdlcloc, (TClosArg TLnil, hdlbcid)), ELcons (EVariable (Local loc, (TCont t, bid)), es))) in
                let contmap = Env.Int.bind v (targ, VarID (t, cid), hdlfid, thdlret, hdlcid, its) env.contmap in
                Some (Either.Right { env with base = AT base; nclos; clos; contmap }, (StorClosure, targ, VarID (t, cid),
                      hdlfid, thdlret, StorClosure, hdlcid, its))
          end
        end
  
  type args = int32 * anytyp_list * anyvarid Env.Int.t
  type anyrealt = AR : 'a realt -> anyrealt
  
  let no_arg : args = 0l, TypeList TLnil, Env.Int.empty
  
  (* TODO: optimize function arguments by giving the function reference and the continuation in two distinct arguments *)
  let add_arg (nargs, TypeList args, map : args) (argbind : binder) : args =
    let Type argt = convert_type (Var.type_of_binder argbind) in
    Int32.succ nargs,
      TypeList (TLcons (argt, args)),
      Env.Int.bind (Var.var_of_binder argbind) (VarID (argt, nargs)) map
  
  (* TODO: optimize closure arguments by giving the function reference and the continuation in two distinct closure members *)
  let env_of_args (nargs, args, varmap : args) (closure : binder option) : anyrealt * anygeneralization =
    let closid = nargs in
    let rec add_all_clos varmap (TypeList acc) i ts = match ts with
      | [] -> varmap, extract_args (TypeList acc) (TypeList TLnil)
      | (n, t) :: ts ->
          let Type t = convert_type t in
          let varmap = Env.String.bind n (VarID (t, i)) varmap in
          add_all_clos varmap (TypeList (TLcons (t, acc))) (Int32.succ i) ts
    in let add_all_clos (closure : binder option) = match closure with
      | None -> Env.String.empty, None, TypeList TLnil, AG Gnil
      | Some bclos ->
          let tvs, t = match Types.normalise_datatype (Var.type_of_binder bclos) with
            | Types.ForAll (tvs, t) -> tvs, t
            | t -> [], t in
          let nm = _convert_type
              (fun _ -> raise (internal_error "Expected a row type, got another type"))
              (fun _ _ _ -> raise (internal_error "Expected a row type, got a function type"))
              (fun fsm _ _ -> sort_name_map fsm)
              t in
          let cvarmap, cat = add_all_clos Env.String.empty (TypeList TLnil) 0l nm in
          cvarmap, Some (Var.var_of_binder bclos), cat, generalize_of_tyvars tvs
    in let cvarmap, cbid, cat, gcl = add_all_clos closure in
    let TypeList args = extract_args args (TypeList TLnil) in
    let nargs = Int32.succ nargs in
    AR {
      nargs;
      args;
      nlocs = 0l;
      locs = TypeList TLnil;
      varmap; cvarmap;
      clos = closid, cat, false;
      cbid;
    }, gcl
  
  let add_closure (env : 'a realt) : 'a realt * (mvarid * mvarid * anytyp_list) option =
    let acid, TypeList cat = match env.clos with
      | _, _, true -> raise (internal_error "Double add_closure call")
      | acid, cat, false -> acid, cat
    in match cat with
    | TLnil -> { env with clos = acid, TypeList cat, true; }, None
    | TLcons _ -> let ccid = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cat, tl)));
      clos = ccid, TypeList cat, true;
    }, Some (acid, ccid, TypeList cat)
  
  let add_sub_to_env (env : subt) (base : 'a t) (loc : local_storage) (VarID (t, bid) : anyvarid) : subt * anyvarid =
    let cid = env.nclos in
    let nclos = Int32.succ cid in
    let clos =
      let ExprList (tl, es) = env.clos in
      ExprList (TLcons (t, tl),
                ELcons (EVariable (Local loc, (t, bid)), es)) in
    { env with base = AT base; nclos; clos }, VarID (t, cid)
  
  let add_local (env : 'a t) (Type t : anytyp) : 'a t * mvarid = match env with
    | Either.Left env ->
        let vidx = Int32.add env.nargs env.nlocs in
        Either.Left { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
        }, vidx
    | Either.Right env ->
        let vidx = Int32.add env.nargs env.nlocs in
        Either.Right { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
        }, vidx
  let add_var (env : 'a t) (b : binder) (t : 'b typ) : 'a t * 'b varid = match env with
    | Either.Left env ->
        let vidx = Int32.add env.nargs env.nlocs in
        let v = t, vidx in
        Either.Left { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
          varmap = Env.Int.bind (Var.var_of_binder b) (VarID v) env.varmap;
        }, v
    | Either.Right env ->
        let vidx = Int32.add env.nargs env.nlocs in
        let v = t, vidx in
        Either.Right { env with
          nlocs = Int32.succ env.nlocs;
          locs = (let TypeList tl = env.locs in TypeList (TLcons (t, tl)));
          varmap = Env.Int.bind (Var.var_of_binder b) (StorVariable, VarID v) env.varmap;
        }, v
  let rec find_var : type a. a t -> _ -> (a t * _ * _) option =
    fun (env : a t) (v : var) : (a t * local_storage * anyvarid) option -> match env with
    | Either.Left env' -> Option.map (fun i -> env, StorVariable, i) (Env.Int.find_opt v env'.varmap)
    | Either.Right env' -> begin match Env.Int.find_opt v env'.varmap with
        | Some (loc, i) -> Some (env, loc, i)
        | None -> begin
            let AT base = env'.base in
            match find_var base v with
            | Some (base, loc, bid) ->
                let env, cid = add_sub_to_env env' base loc bid in
                let varmap = Env.Int.bind v (StorClosure, cid) env.varmap in
                Some (Either.Right { env with base = AT base; varmap }, StorClosure, cid)
            | None -> None
          end
      end
  let rec find_closure : type a. a t -> _ -> _ -> (a t * _ * _) option =
    fun (env : a t) (v : var) (n : string) : (a t * local_storage * anyvarid) option -> match env with
    | Either.Left env -> begin match env.cbid with
        | None -> None
        | Some cbid -> if Int.equal cbid v then Some (Either.Left env, StorClosure, Env.String.find n env.cvarmap) else None
      end
    | Either.Right env -> begin match IntString.find_opt (v, n) env.cvarmap with
        | Some (loc, ret) -> Some (Either.Right env, loc, ret)
        | None -> begin
            let AT base = env.base in
            match find_closure base v n with
            | Some (base, loc, bid) ->
                let env, cid = add_sub_to_env env base loc bid in
                let cvarmap = IntString.bind (v, n) (StorClosure, cid) env.cvarmap in
                Some (Either.Right { env with base = AT base; cvarmap }, StorClosure, cid)
            | None -> None
          end
      end
  let rec find_raw_closure : type a. a t -> _ -> (a t * _ * _) option =
    fun (env : a t) (v : var) : (a t * local_storage * abs_closure_content varid) option -> match env with
    | Either.Left env -> begin match env.cbid with
        | None -> None
        | Some cbid -> if Int.equal cbid v then Some (Either.Left env, StorVariable, (TAbsClosArg, Int32.pred env.nargs)) else None
      end
    | Either.Right env -> begin match Env.Int.find_opt v env.varmap with
        | Some (loc, VarID (TAbsClosArg, i)) -> Some (Either.Right env, loc, (TAbsClosArg, i))
        | Some (_, VarID (_, _)) -> failwith "TODO: LEnv.find_raw_closure with non-TAbsClosArg"
        | None -> begin
            let AT base = env.base in
            match find_raw_closure base v with
            | Some (base, loc, bid) ->
                let env, cid = add_sub_to_env env base loc (VarID bid) in
                let varmap = Env.Int.bind v (StorClosure, cid) env.varmap in
                let vid = match cid with
                  | VarID (TAbsClosArg, vid) -> vid
                  | _ -> failwith "TODO: LEnv.find_raw_closure with non-TAbsClosArg" in
                Some (Either.Right { env with base = AT base; varmap }, StorClosure, (TAbsClosArg, vid))
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
        let env, varid = add_var (of_sub env) b t in
        to_sub env, VarIDList (eargs, VLcons (varid, VLnil))
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
  let add_cont (env : subt) (Type tcontret : anytyp) (hdlfid : mfunid) (thdlret : anytyp) : subt * mvarid =
    match env.contid with
    | Some _ -> raise (internal_error "Cannot add multiple continuations")
    | None ->
        let env, id = env, 0l in   (* Continuation argument *)
        let env, ccid = env, 1l in (* Argument of the continuation argument *)
        let env, hcid = env, 2l in (* Closure of the current handler function *)
        env.contid <- Some (VarID (tcontret, id), ccid, hdlfid, thdlret, hcid);
        env, id
  let set_continuation (env : subt) (b : binder) : subt = match env.contid with
    | None -> raise (internal_error "Cannot set missing continuation")
    | Some _ -> env.contv <- (Either.Left (Var.type_of_binder b), Var.var_of_binder b); env
  
  let locals_of_env (env : 'a realt) : anytyp list = extract_to_list env.locs []
  
  let compile (env : 'a realt) (fid : mfunid) (export_name : string option) (tret : 'b typ) (b : 'b block) : ('a, 'b) func' =
    let closid, clt, has_converted_closure = env.clos in
    let export_data = match export_name with
      | Some name -> Some name
      | None -> None
    in let convclos = if has_converted_closure then match clt with TypeList TLnil -> None | TypeList (TLcons _) -> Some (clt, closid) else None
    in {
      fun_id = fid;
      fun_export_data = export_data;
      fun_converted_closure = convclos;
      fun_args = env.args;
      fun_ret = tret;
      fun_locals = extract_to_list env.locs [];
      fun_block = b;
    }
  
  let compile_cont_start (env : subt) (fid : mfunid) (tret : 'b typ) (b : 'b block) (cclosid : (anytyp_list * mvarid) option) : 'b fstart =
    {
      fst_id = fid;
      fst_converted_closure = cclosid;
      fst_ret = tret;
      fst_locals = extract_to_list env.locs [];
      fst_block = b;
    }
  
  let compile_handler (type b d) (env : subt) (fid : mfunid) (oabsconc : (mvarid * mvarid) option)
                      (onret : (b, d) finisher) (ondo : (b, d) handler list) : anyfhandler =
    let contarg, argcontidx = match env.contid with
      | Some (VarID (_, i), j, _, _, _) -> i, j
      | None -> raise (internal_error "Handlers must have a continuation added") in
    let TypeList tis = env.its in
    let tcont = match onret with FId t | FMap ((t, _), _, _) -> t in
    let ExprList (clostyp, _) = env.clos in AFHdl {
      fh_contarg  = (TCont tcont, contarg), argcontidx;
      fh_tis      = tis;
      fh_closure  = Option.map (fun (absid, concid) -> absid, (TypeList clostyp, concid)) oabsconc;
      fh_locals   = extract_to_list env.locs [];
      fh_finisher = onret;
      fh_handlers = ondo;
      fh_id       = fid;
    }
end

module GEnv : sig (* Contains the functions, the types, etc *)
  type ('pi, 'pa) t
  type funid
  type hid
  
  val empty : string Env.Int.t -> Utility.IntSet.t -> backend ->
              'pi -> (('pi, 'pa) t -> 'pi -> var -> (('pi, 'pa) t * 'pi * 'pa) option) -> ('pi, 'pa) t
  
  val find_tag : ('pi, 'pa) t -> string -> ('pi, 'pa) t * tagid
  
  val get_var_name : ('pi, 'pa) t -> var -> string
  val find_fun : ('pi, 'pa) t -> 'a LEnv.t -> var -> ('pi, 'pa) t * 'a LEnv.t * closed_like
  val find_closable_fun : ('pi, 'pa) t -> 'a LEnv.t -> var -> anyfuncid
  
  (* To be used by prelude-related functions only *)
  val add_gbl : ('pi, 'pa) t -> binder -> 'b typ -> ('pi, 'pa) t * 'b varid
  
  val add_var : ('pi, 'pa) t -> 'a LEnv.t -> binder -> 'b typ -> ('pi, 'pa) t * 'a LEnv.t * locality * 'b varid
  val find_var : ('pi, 'pa) t -> 'a LEnv.t -> var -> ('pi, 'pa) t * ('a LEnv.t * locality * anyvarid, anycfuncid) Either.t option
  val find_builtin_var : ('pi, 'pa) t -> string -> (('pi, 'pa) t * anyexpr) option
  
  val allocate_function : ('pi, 'pa) t -> 'a LEnv.realt -> binder -> tyvar list -> anygeneralization -> ('pi, 'pa) t * mfunid * funid
  val assign_function : ('pi, 'pa) t -> funid -> ('a, 'b) func' -> ('pi, 'pa) t
  
  val new_continuator : ('pi, 'pa) t -> mfunid -> 'c typ -> 'a typ -> 'b typ -> 'd typ_list ->
                        ('pi, 'pa) t * ('a * 'd, 'b, 'c continuation * (abs_closure_content * unit), unit, unit) funcid
  val new_cont_start : ('pi, 'pa) t -> LEnv.subt -> anyblock -> (anytyp_list * mvarid) option -> ('pi, 'pa) t * mfunid
  val allocate_fhandler : ('pi, 'pa) t -> hid * mfunid * ('pi, 'pa) t
  val assign_fhandler : ('pi, 'pa) t -> hid -> mfunid -> LEnv.subt -> (mvarid * mvarid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> ('pi, 'pa) t
  
  val gen_impure : ('pi, 'pa) t -> string -> tyarg list -> anyexpr_list -> ('pi, 'pa) t * anyexpr
  
  val add_effect : ('pi, 'pa) t -> string -> ('pi, 'pa) t * meffid
  
  val compile : ('pi, 'pa) t -> unit LEnv.realt -> anyblock ->
                (('pi, 'pa) t -> unit LEnv.realt -> 'pa -> ('pi, 'pa) t * unit LEnv.realt * assign list) -> anymodule
end = struct
  (* In-place function emplacement in the environment *)
  type anyfunc' = AFFnc : ('a, 'b) func' -> anyfunc'
  type anyfstart = AFSt : 'b fstart -> anyfstart
  type funid = anyfunc' option ref
  type hid = anyfhandler option ref
  
  module EffectMap = Utility.StringMap
  
  module VarQueue = Utility.PQueue(struct type t = var let compare l r = Int.compare r l end)
  
  type ('pi, 'pa) t = {
    ge_builtins : Builtins.t;
    ge_imports : (string * string) list;
    ge_map : string Env.Int.t;
    ge_nfuns : mfunid;
    ge_funs : ((anyfunc' option ref,
                (anyfhandler option ref,
                 ((anyfunc',
                   anyfstart) Either.t,
                  anyfbuiltin) Either.t) Either.t) Either.t * mfunid) list;
    ge_ntags : tagid;
    ge_tagmap : tagid Env.String.t;
    ge_neffs : meffid;
    ge_effs : EffectIDSet.t;
    ge_effmap : meffid EffectMap.t;
    ge_ngbls : mvarid;
    ge_gbls : (anyvarid * string option) list;
    ge_gblbinders : Utility.IntSet.t;
    ge_gblmap : anyvarid Env.Int.t;
    ge_fmap : anyfuncid Env.Int.t;
    ge_has_processes : bool;
    ge_pmap : 'pi;
    ge_pmap_find : ('pi, 'pa) t -> 'pi -> var -> (('pi, 'pa) t * 'pi * 'pa) option;
    ge_pmap_acc : 'pa VarQueue.t; (* Changes in-place *)
  }
  let empty (m : string Env.Int.t) (global_binders : Utility.IntSet.t) (backend : backend)
            (prelude_init : 'pi) (find_prelude : ('pi, 'pa) t -> 'pi -> var -> (('pi, 'pa) t * 'pi * 'pa) option) : ('pi, 'pa) t =
    let ge_imports = match backend with
      | BackendNone -> []
      | BackendWizard -> ["wizeng", "puta"; "wizeng", "putc"] in
    let ge_nfuns = Int32.of_int (List.length ge_imports) in
    let ge_ntags = match backend with BackendNone | BackendWizard -> 0 in {
      ge_builtins = Builtins.empty;
      ge_imports;
      ge_map = m;
      ge_nfuns;
      ge_funs = [];
      ge_ntags;
      ge_tagmap = Env.String.empty;
      ge_neffs = 0l;
      ge_effs = EffectIDSet.empty;
      ge_effmap = EffectMap.empty;
      ge_ngbls = 0l;
      ge_gbls = [];
      ge_gblbinders = global_binders;
      ge_gblmap = Env.Int.empty;
      ge_fmap = Env.Int.empty;
      ge_has_processes = false;
      ge_pmap = prelude_init;
      ge_pmap_find = find_prelude;
      ge_pmap_acc = VarQueue.empty ();
    }
  
  let find_tag (env : ('pi, 'pa) t) (tname : string) : ('pi, 'pa) t * tagid = match Env.String.find_opt tname env.ge_tagmap with
    | Some i -> env, i
    | None ->
        let tagid = env.ge_ntags in
        let ge_ntags = Int.succ tagid in
        let ge_tagmap = Env.String.bind tname tagid env.ge_tagmap in
        { env with ge_ntags; ge_tagmap }, tagid
  
  let get_var_name (ge : ('pi, 'pa) t) (v : var) = Env.Int.find v ge.ge_map
  
  let add_gbl (ge : ('pi, 'pa) t) (b : binder) (t : 'a typ) : ('pi, 'pa) t * 'a varid =
    let vid = ge.ge_ngbls in
    let ge_ngbls = Int32.succ ge.ge_ngbls in
    let name = Var.name_of_binder b in
    let name = if name = "main" then "main'" else name in
    let newvar = (t, vid) in
    let ge_gbls = (VarID newvar, Some name) :: ge.ge_gbls in
    let ge_gblmap = Env.Int.bind (Var.var_of_binder b) (VarID newvar) ge.ge_gblmap in
    { ge with ge_ngbls; ge_gbls; ge_gblmap }, newvar
  
  let try_find_in_prelude ge v = match ge.ge_pmap_find ge ge.ge_pmap v with
    | Some (ge, ge_pmap, pv) ->
        (* If we found something, it has been added to the corresponding map *)
        let ge = { ge with ge_pmap } in
        VarQueue.add ge.ge_pmap_acc v pv; (* Changes in-place *)
        Some ge
    | None -> None
  
  let add_var (ge : ('pi, 'pa) t) (le : 'a LEnv.t) (b : binder) (t : 'b typ) : ('pi, 'pa) t * 'a LEnv.t * locality * 'b varid =
    if Utility.IntSet.mem (Var.var_of_binder b) ge.ge_gblbinders
    then let ge, v =      add_gbl ge b t in ge, le, Global,             v
    else let le, v = LEnv.add_var le b t in ge, le, Local StorVariable, v
  let rec find_var (ge : ('pi, 'pa) t) (le : 'a LEnv.t) (v : var) : ('pi, 'pa) t * ('a LEnv.t * locality * anyvarid, anycfuncid) Either.t option =
    match LEnv.find_var le v with
    | Some (le, st, v) -> ge, Some (Either.Left (le, Local st, v))
    | None -> begin
        match Env.Int.find_opt v ge.ge_fmap with
        | Some (FuncID (type b c d gb gd) ((_, gd, _, _, ctyp, _) as fid : (b, c, d, gb, gd) funcid)) -> begin match gd, ctyp with
            | Gnil, TLnil -> ge, Some (Either.Right (ACFunction fid))
            | Gcons _, TLnil -> raise (internal_error "Unexpected generalization of closure with no value")
            | _, TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
          end
        | None -> begin match try_find_in_prelude ge v with
            | Some ge -> find_var ge le v
            | None -> ge, Option.map (fun v -> Either.Left (le, Global, v)) (Env.Int.find_opt v ge.ge_gblmap)
          end
      end
  
  let rec find_fun (ge : ('pi, 'pa) t) (le : 'a LEnv.t) (v : var) : ('pi, 'pa) t * 'a LEnv.t * closed_like =
    match Env.Int.find_opt v ge.ge_fmap with
    | Some (FuncID ((_, gd, _, _, ctyp, _) as fid)) -> begin match gd, ctyp with
        | Gnil, TLnil -> ge, le, Function fid
        | Gcons _, TLnil -> raise (internal_error "Unexpected generalization of closure with no value")
        | _, TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
      end
    | None -> begin match LEnv.find_continuation le v with
        | Some (le, (loc, Type arg, VarID (ret, vid), hdlfid, Type tret, hdlcloc, hdlcid, TypeList its)) ->
            ge, le, Contin (
              Local loc,
              (TCont ret, vid),
              (TLcons (TCont ret, TLcons (arg, its)), tret, hdlfid),
              Local hdlcloc,
              hdlcid)
        | None -> begin match try_find_in_prelude ge v with
            | Some ge -> find_fun ge le v
            | None -> match find_var ge le v with
                | ge, Some (Either.Left (le, loc, VarID ((t, _) as vid))) -> begin match t with
                    | TClosed _ -> ge, le, Closure (loc, vid)
                    | _ -> raise (internal_error "Unexpected variable type, expected closed function")
                  end
                | ge, Some (Either.Right (ACFunction f)) -> ge, le, Function f
                | ge, None ->
                    let name = get_var_name ge v in
                    ge, le, ClosedBuiltin name
          end
      end
  let find_closable_fun (ge : ('pi, 'pa) t) (_ : 'a LEnv.t) (v : var) : anyfuncid = Env.Int.find v ge.ge_fmap
  
  let allocate_function (env : ('pi, 'pa) t) (args : 'a LEnv.realt) (b : binder) (tyvars : tyvar list)
      (gc : anygeneralization) : ('pi, 'pa) t * mfunid * funid =
    let f = ref None in
    let targs, TypeList ctyp = LEnv.args_typ args in
    let AG ga, Type tret =
      let tvs, t = match Types.normalise_datatype (Var.type_of_binder b) with
        | Types.ForAll (tvs, t) -> tvs, t
        | t -> [], t
      in
      let Type tret =
        _convert_type
          (fun _ -> raise (internal_error "Invalid type: expected a function type, got another type"))
          (fun _ _ _ ret -> convert_type ret)
          (fun _ _ _ -> raise (internal_error "Invalid type: expected a function type, got a row type"))
          t in
      let ga, tret =
        let rec inner : type a. a generalization -> _ = fun raw_ga tvs acc map -> match raw_ga, tvs with
          | _, (_, (CommonTypes.PrimaryKind.(Row | Presence), _)) :: tvs -> inner raw_ga tvs acc map
          | Gnil, [] -> List.fold_left (fun (AG acc) v -> AG (Gcons (v, acc))) (AG Gnil) acc, map
          | Gcons _, [] -> raise (internal_error "Invalid type: too many abstractions")
          | Gnil, _ :: _ -> raise (internal_error "Invalid type: too many type variables")
          | Gcons (ghd, gtl), (vhd, (CommonTypes.PrimaryKind.Type, _)) :: vtl ->
              inner gtl vtl (ghd :: acc) (TVarMap.add vhd ghd map) in
        let AG raw_ga = generalize_of_tyvars tyvars in
        let ga, map = inner raw_ga tvs [] TVarMap.empty in
        let rec inner : type a. _ -> a typ -> a typ = fun map t -> match t with
          | TInt -> TInt
          | TBool -> TBool
          | TFloat -> TFloat
          | TString -> TString
          | TClosed (g, targs, tret) -> TClosed (g, inner_list map targs, inner map tret)
          | TAbsClosArg -> TAbsClosArg
          | TClosArg ts -> TClosArg (inner_list map ts)
          | TCont tret -> TCont (inner map tret)
          | TTuple ts -> TTuple (inner_named_list map ts)
          | TVariant -> TVariant
          | TList t -> TList (inner map t)
          | TVar i -> TVar (Option.value ~default:i (TVarMap.find_opt i map))
          | TSpawnLocation -> TSpawnLocation
          | TProcess -> TProcess
        and inner_list : type a. _ -> a typ_list -> a typ_list = fun map ts -> match ts with
          | TLnil -> TLnil
          | TLcons (hd, tl) -> TLcons (inner map hd, inner_list map tl)
        and inner_named_list : type a. _ -> a named_typ_list -> a named_typ_list = fun map ts -> match ts with
          | NTLnil -> NTLnil
          | NTLcons (n, hd, tl) -> NTLcons (n, inner map hd, inner_named_list map tl)
        in ga, inner map tret in
      let ga =
        let rec inner (AG ga) (AG gc) = match gc with
          | Gnil -> AG ga
          | Gcons (_, tl) -> match ga with
              | Gnil -> raise (internal_error "Invalid type: closure has more type variables than the function")
              | Gcons (_, tlc) -> inner (AG tlc) (AG tl) in
        inner ga gc in
      ga, Type tret in
    let AG gc = gc in
    let fid = env.ge_nfuns in
    let ge_fmap = Env.Int.bind (Var.var_of_binder b) (FuncID (ga, gc, targs, tret, ctyp, fid)) env.ge_fmap in
    { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = (Either.Left f, fid) :: env.ge_funs;
      ge_fmap;
    }, fid, f
  let assign_function (env : ('pi, 'pa) t) (fid : funid) (f : ('a, 'b) func') : ('pi, 'pa) t = match !fid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> fid := Some (AFFnc f); env
  
  let new_continuator (env : ('pi, 'pa) t) (hdlfid : mfunid) (type a b c d) (cret : c typ) (targ : a typ) (tret : b typ) (tiargs : d typ_list) :
      ('pi, 'pa) t * (a * d, b, c continuation * (abs_closure_content * unit), unit, unit) funcid =
    (* Continuation argument[targ] at position 0, invariant arguments between, abstract closure at position +1, concrete closure at position +2 *)
    (* Continuation[cret] in closure at position 0, continuation handler closure content in closure at position 1 *)
    let acid, eiargs =
      let rec inner : type d. _ -> d typ_list -> _ * d expr_list = fun narg tiargs -> match tiargs with
        | TLnil -> narg, ELnil
        | TLcons (hd, tl) -> let ret, eret = inner (Int32.succ narg) tl in ret, ELcons (EVariable (Local StorVariable, (hd, narg)), eret) in
      inner 1l tiargs in
    let targs = TLcons (targ, tiargs) in
    let ct = TLcons (TCont cret, TLcons (TAbsClosArg, TLnil)) in
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ fid in
    let b = ([
      Assign (Local StorVariable, (TClosArg ct, 2l), EConvertClosure (acid, TClosArg ct))],
      ECallRawHandler (hdlfid, cret, EVariable (Local StorClosure, (TCont cret, 0l)),
                               targ, EVariable (Local StorVariable, (targ, 0l)),
                               tiargs, eiargs,
                               EVariable (Local StorClosure, (TAbsClosArg, 1l)), tret)) in
    let f = {
      fun_id = fid;
      fun_export_data = None;
      fun_converted_closure = Some (TypeList ct, 2l);
      fun_args = targs;
      fun_ret = tret;
      fun_locals = [Type (TClosArg ct)];
      fun_block = b;
    } in
    { env with
      ge_nfuns;
      ge_funs = (Either.Right (Either.Right (Either.Left (Either.Left (AFFnc f)))), fid) :: env.ge_funs;
    }, (Gnil, Gnil, targs, tret, ct, fid)
  let new_cont_start (env : ('pi, 'pa) t) (args : LEnv.subt) (b : anyblock) (cclosid : (anytyp_list * mvarid) option) : ('pi, 'pa) t * mfunid =
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ env.ge_nfuns in
    let Block (tret, b) = b in
    let f = LEnv.compile_cont_start args fid tret b cclosid in
    { env with
      ge_nfuns;
      ge_funs = (Either.Right (Either.Right (Either.Left (Either.Right (AFSt f)))), fid) :: env.ge_funs;
    }, fid
  let allocate_fhandler (env : ('pi, 'pa) t) : hid * mfunid * ('pi, 'pa) t =
    let f = ref None in
    let fid = env.ge_nfuns in
    f, fid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = (Either.Right (Either.Left f), fid) :: env.ge_funs;
    }
  let assign_fhandler (env : ('pi, 'pa) t) (hid : hid) (self_mid : mfunid) (args : LEnv.subt) (oabsconc : (mvarid * mvarid) option)
                      (onret : ('b, 'd) finisher) (ondo : ('b, 'd) handler list) : ('pi, 'pa) t = match !hid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> hid := Some (LEnv.compile_handler args self_mid oabsconc onret ondo); env
  
  let set_has_process (env : ('pi, 'pa) t) : ('pi, 'pa) t =
    if not env.ge_has_processes then
      { env with ge_has_processes = true }
    else env
  let add_builtin (env : ('pi, 'pa) t) (fb : anyfbuiltin) : ('pi, 'pa) t * mfunid =
    let i = env.ge_nfuns in
    let ge_funs = (Either.Right (Either.Right (Either.Right fb)), i) :: env.ge_funs in
    let ge_nfuns = Int32.succ i in
    { env with ge_funs; ge_nfuns; }, i
  let gen_impure (ge : ('pi, 'pa) t) (name : string) (ts : tyarg list) (args : anyexpr_list) : ('pi, 'pa) t * anyexpr =
    let ge_builtins, ge, e = Builtins.gen_impure ge.ge_builtins ge add_builtin set_has_process convert_type name ts args in
    { ge with ge_builtins }, e
  
  let declare_function (env : ('pi, 'pa) t) : ('pi, 'pa) t * mfunid * funid =
    let f = ref None in
    let fid = env.ge_nfuns in
    let ge_funs = (Either.Left f, fid) :: env.ge_funs in
    let ge_nfuns = Int32.succ fid in
    { env with ge_funs; ge_nfuns; }, fid, f
  let find_builtin_var (ge : ('pi, 'pa) t) (name : string) : (('pi, 'pa) t * anyexpr) option =
    match Builtins.get_var ge.ge_builtins ge declare_function (fun ge fid (AF' f) -> assign_function ge fid f)
                           add_builtin set_has_process convert_type name with
    | Some (ge_builtins, ge, e) -> Some ({ ge with ge_builtins }, e)
    | None -> None
  
  let add_effect (env : ('pi, 'pa) t) (ename : string) : ('pi, 'pa) t * meffid =
    let eff = ename in
    match EffectMap.find_opt eff env.ge_effmap with
    | Some i -> env, i
    | None ->
        let i = env.ge_neffs in
        let ge_neffs = Int32.succ env.ge_neffs in
        let ge_effs = EffectIDSet.add i env.ge_effs in
        let ge_effmap = EffectMap.add eff i env.ge_effmap in
        let env = { env with ge_neffs; ge_effs; ge_effmap; } in
        env, i
  
  let rec compile (ge : ('pi, 'pa) t) (le : unit LEnv.realt) (Block (t, (ass, e)) : anyblock)
                  (compile_prelude : ('pi, 'pa) t -> unit LEnv.realt -> 'pa -> ('pi, 'pa) t * unit LEnv.realt * assign list) : anymodule =
    match VarQueue.pop_min_opt ge.ge_pmap_acc with
    | Some (_, hd) ->
        (* Accumulator changed in-place *)
        let ge, le, ahd = compile_prelude ge le hd in
        let b = Block (t, (ahd @ ass, e)) in
        compile ge le b compile_prelude
    | _ ->
      let lvs = LEnv.locals_of_env le in
      Module {
        mod_imports = ge.ge_imports;
        mod_nfuns = ge.ge_nfuns;
        mod_funs =
          List.rev_map
            (fun (f, fid) -> match f with
              | Either.Left ({ contents = None }) -> raise (internal_error "function was allocated but never assigned")
              | Either.Left ({ contents = Some (AFFnc f) }) -> FFunction f
              | Either.Right (Either.Left { contents = None }) -> raise (internal_error "handler was allocated but never assigned")
              | Either.Right (Either.Left { contents = Some (AFHdl f) }) -> FHandler f
              | Either.Right (Either.Right (Either.Left (Either.Left (AFFnc f)))) -> FFunction f
              | Either.Right (Either.Right (Either.Left (Either.Right (AFSt f)))) -> FContinuationStart f
              | Either.Right (Either.Right (Either.Right (AFBt fb))) -> FBuiltin (fid, fb))
            ge.ge_funs;
        mod_neffs = ge.ge_neffs;
        mod_effs = ge.ge_effs;
        mod_nglobals = ge.ge_ngbls;
        mod_global_vars = List.rev_map (fun (VarID (t, v), n) -> (v, Type t, n)) ge.ge_gbls;
        mod_locals = lvs;
        mod_main = t;
        mod_block = (ass, e);
        mod_has_processes = ge.ge_has_processes;
      }
end
type ('pi, 'pa) genv = ('pi, 'pa) GEnv.t
type 'args lenv = 'args LEnv.t

let of_constant (c : CommonTypes.Constant.t) : anyexpr = let open CommonTypes.Constant in match c with
  | Float f -> Expr (TFloat, EConstFloat f)
  | Int i -> Expr (TInt, EConstInt (Int64.of_int i))
  | Bool b -> Expr (TBool, EConstBool b)
  | String s -> Expr (TString, EConstString s)
  | Char _ -> failwith "TODO: of_constant Char"
  | DateTime _ -> failwith "TODO: of_constant DateTime"

let collect_toplevel_polymorphism (v : value) : tyarg list * value * anytyp option =
  let rec inner v acc otc = match v with
    | TApp (v, ts) -> inner v (acc @ ts) otc
    | TAbs _ -> failwith "TODO TAbs"
    | Coerce (v, t) -> begin
        let t = convert_type t in
        match otc with
        | None -> inner v acc (Some t)
        | Some _ -> failwith "TODO collect_toplevel_polymorphism with multiple Coerce in a row"
      end
    | _ -> acc, v, otc
  in inner v [] None

let specialize_to_apply (ga : 'ga generalization) (ts : tyarg list)
    (targs : 'a typ_list) (tret : 'b typ) : (unit, 'ga) specialization * 'a specialize_list * 'b specialize =
  let s, tmap =
    let rec inner : type a. a generalization -> _ -> _ -> (unit, a) specialization * _ = fun ga ts acc -> match ga, ts with
      | _, (CommonTypes.PrimaryKind.(Row | Presence), _) :: tl -> inner ga tl acc
      | Gnil, [] -> Snil Gnil, acc
      | Gnil, _ :: _ -> raise (internal_error "Invalid closure type: too many type arguments")
      | Gcons _, [] -> raise (internal_error "Invalid closure type: not enough type arguments")
      | Gcons (ghd, gtl), (CommonTypes.PrimaryKind.Type, thd) :: ttl ->
          let thd = convert_type thd in
          let s, tmap = inner gtl ttl (TVarMap.add ghd thd acc) in
          Scons (thd, ghd, s), tmap
    in inner ga ts TVarMap.empty
  in
  s, specialize_typ_list tmap targs, specialize_typ tmap tret

type _ specialization_from = SpecFrom : 'a generalization * ('a, 'b) specialization -> 'b specialization_from
let specialize_some (ga : 'ga generalization) (ts : tyarg list)
    (targs : 'a typ_list) (tret : 'b typ) : 'ga specialization_from * 'a specialize_list * 'b specialize =
  let s, tmap =
    let rec inner : type a. a generalization -> _ -> _ -> a specialization_from * _ = fun ga ts acc -> match ts, ga with
      | (CommonTypes.PrimaryKind.(Row | Presence), _) :: tl, _ -> inner ga tl acc
      | [], g -> SpecFrom (g, Snil g), acc
      | _ :: _, Gnil -> raise (internal_error "Invalid closure type: too many type arguments")
      | (CommonTypes.PrimaryKind.Type, thd) :: ttl, Gcons (ghd, gtl) ->
          let thd = convert_type thd in
          let SpecFrom (g, s), tmap = inner gtl ttl (TVarMap.add ghd thd acc) in
          SpecFrom (g, Scons (thd, ghd, s)), tmap
    in inner ga ts TVarMap.empty
  in
  s, specialize_typ_list tmap targs, specialize_typ tmap tret

type 'a any_extract = Extract : int * 'b typ * ('a, 'b) extract_typ_check -> 'a any_extract

type anyconvclosed = ACC : 'ga generalization * ('a, 'a0) box_list * ('b, 'b0) box * 'a typ_list * 'b typ * ('ga * 'a0 -> 'b0) expr -> anyconvclosed
let rec convert_closure (ge : ('pi, 'pa) genv) (le: 'args lenv) (f : var) (tcl : tyarg list) (cls : value)
    : ('pi, 'pa) genv * 'args lenv * anyconvclosed =
  let FuncID (type a0 b0 c0 ga gc) ((ga, gc, targs, tret, tc, _) as fid : (a0, b0, c0, ga, gc) funcid) =
    GEnv.find_closable_fun ge le f in
  let tmap =
    let rec inner (AG gc) tcl acc = match gc, tcl with
      | _, (CommonTypes.PrimaryKind.(Row | Presence), _) :: tl -> inner (AG gc) tl acc
      | Gnil, [] -> acc
      | Gnil, _ :: _ -> raise (internal_error "Invalid closure type: too many type arguments")
      | Gcons _, [] -> raise (internal_error "Invalid closure type: not enough type arguments")
      | Gcons (ghd, gtl), (CommonTypes.PrimaryKind.Type, thd) :: ttl ->
          inner (AG gtl) ttl (TVarMap.add ghd (convert_type thd) acc)
    in inner (AG gc) tcl TVarMap.empty
  in
  let SpecL (type a) (targs, bargs : _ * (a, a0) box_list) = specialize_typ_list tmap targs in
  let Spec (type b) (tret, bret : _ * (b, b0) box) = specialize_typ tmap tret in
  let SpecL (type c) (tc, bc : _ * (c, c0) box_list) = specialize_typ_list tmap tc in
  let ge, le, (closed : (ga * a0 -> b0) expr) = match cls with
    | Extend (vm, None) ->
        let cls = sort_name_map vm |> List.map snd in
        let ge, le, cls = convert_values ge le cls tc in
        ge, le, EClose (fid, bc, cls)
    | Extend (_, Some _) -> failwith "TODO: convert_closure Extend Some"
    | Variable v -> begin match LEnv.find_raw_closure le v with
        | Some (le, loc, v) -> ge, le, ERawClose (fid, EVariable (Local loc, v))
        | None -> failwith "TODO: convert_closure Variable non-raw_closure"
      end
    | _ -> failwith "TODO: convert_closure non-Extend" in
  ge, le, ACC (ga, bargs, bret, targs, tret, closed)

and of_value (ge : ('pi, 'pa) genv) (le: 'args lenv) (v : value) : ('pi, 'pa) genv * 'args lenv * anyexpr = match v with
  | Constant c -> let Expr (ct, cv) = of_constant c in ge, le, Expr (ct, cv)
  | Variable v -> begin match GEnv.find_var ge le v with
      | ge, Some (Either.Left (le, loc, VarID (t, vid))) -> ge, le, Expr (t, EVariable (loc, (t, vid)))
      | ge, Some (Either.Right (ACFunction ((ga, Gnil, targs, tret, TLnil, _) as f))) ->
          ge, le, Expr (TClosed (ga, targs, tret), EClose (f, BLnil, ELnil))
      | ge, None -> begin match LEnv.find_continuation le v with
          | Some (le, (loc, Type (type a) (arg : a typ), VarID (type c) (ret, vid : c typ * _),
                  hdlfid, Type (type b) (tret : b typ), hdlcloc, hdlcid, TypeList (type d) (tiargs : d typ_list))) ->
              let ge, fid = GEnv.new_continuator ge hdlfid ret arg tret tiargs in
              ge, le, Expr (TClosed (Gnil, TLcons (arg, tiargs), tret),
                            EClose (fid, BLcons (BNone (TCont ret, TCont ret),
                                         BLcons (BNone (TAbsClosArg, TAbsClosArg), BLnil)),
                                         ELcons (EVariable (Local loc, (TCont ret, vid)),
                                         ELcons (EVariable (Local hdlcloc, (TAbsClosArg, hdlcid)), ELnil))))
          | None -> begin match GEnv.find_builtin_var ge (GEnv.get_var_name ge v) with
              | Some (ge, v) -> ge, le, v
              | None -> failwith ("TODO: of_value Variable (probable builtin: " ^ (string_of_int v) ^ ": " ^ (GEnv.get_var_name ge v) ^ ")")
            end
        end
      end
  | Extend (nm, None) ->
      let sorted = sort_name_map nm in
      let rec inner ge le sorted = match sorted with
        | [] -> ge, le, Expr (TTuple NTLnil, ETuple (NTLnil, ELnil))
        | (n, hd) :: tl -> begin
            let ge, le, Expr (thd, ehd) = of_value ge le hd in
            match inner ge le tl with
            | ge, le, Expr (TTuple ttl, ETuple (_, etl)) ->
                ge, le, Expr (TTuple (NTLcons (n, thd, ttl)), ETuple (NTLcons (n, thd, ttl), ELcons (ehd, etl)))
            | _ -> assert false
          end
      in inner ge le sorted
  | Extend (_, Some _) -> failwith "TODO: of_value Extend Some"
  | Project (n, v) -> begin match
      match v with
      | Variable v -> begin match LEnv.find_closure le v n with
          | Some (le, lst, VarID (t, i)) ->
              Some (ge, le, Expr (t, EVariable (Local lst, (t, i))))
          | None -> None
        end
      | _ -> None
      with Some ret -> ret | None -> begin
        let ge, le, Expr (t, e) = of_value ge le v in
        match t with
        | TTuple nt ->
            let Extract (type b) ((i, ft, c) : _ * _ * (_, b) extract_typ_check) =
              let rec inner : 'a. 'a named_typ_list -> 'a any_extract = fun (type a) (nt : a named_typ_list) : a any_extract -> match nt with
                | NTLnil -> raise (internal_error "Missing field from type")
                | NTLcons (fn, ft, tl) ->
                    if String.equal n fn then Extract (0, ft, ExtractO) else
                      let Extract (i, ft, e) = inner tl in Extract (Int.succ i, ft, ExtractS e)
              in inner nt
            in ge, le, Expr (ft, EExtract (e, (TTuple nt, i, ft, c)))
        | _ -> raise (internal_error "Unexpected non-tuple expression in projection")
        end
    end
  | Erase _ -> failwith "TODO: of_value Erase"
  | Inject (tname, args, _) ->
      let ge, tagid = GEnv.find_tag ge tname in
      let ge, le, Expr (targ, arg) = of_value ge le args in
      ge, le, Expr (TVariant, EVariant (tagid, targ, arg))
  | TAbs (_, _v) -> failwith "TODO: of_value TAbs"
  | TApp (v, ts) -> begin
      let ge, le, Expr (t, e) = of_value ge le v in match t with
      | TClosed (ga, targs, tret) ->
          let SpecFrom (ga, s), SpecL (targs, bargs), Spec (tret, bret) = specialize_some ga ts targs tret in
          let t = TClosed (ga, targs, tret) in
          let closed_applied = ESpecialize (e, s, bargs, bret) in
          ge, le, Expr (t, closed_applied)
      | TVariant -> begin match ts with
          | [CommonTypes.PrimaryKind.Row, _] -> ge, le, Expr (t, e)
          | _ -> raise (internal_error "Cannot apply non-Row-only type(s) to TVariant expression")
        end
      | TList (TVar -1) -> begin match ts, e with
          | [CommonTypes.PrimaryKind.Type, t], EListNil _ -> (* Probably the Links [] value *)
              let Type t = convert_type t in
              ge, le, Expr (TList t, EListNil t)
          | _, _ -> raise (internal_error "Incoherent state TApp (? : TList -1, ?)")
        end
      | _ -> raise (internal_error "Cannot apply type to non-functional (non-TClosed) expression")
    end
  | XmlNode _ -> failwith "TODO: of_value XmlNode"
  | ApplyPure (f, args) -> begin match collect_toplevel_polymorphism f with
      | ts, Variable v, otc -> begin
          let ts = List.filter_map (fun (k, t) -> if k = CommonTypes.PrimaryKind.Type then Some (convert_type t) else None) ts in
          let name = GEnv.get_var_name ge v in match args with
          | [arg] -> begin match Builtins.get_unop name ts otc with
            | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin unary operation"))
            | Some (Unop UONot) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TBool in
                ge, le, Expr (TBool, EUnop (UONot, arg))
            | Some (Unop UONegI) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TInt in
                ge, le, Expr (TInt, EUnop (UONegI, arg))
            | Some (Unop UONegF) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TFloat in
                ge, le, Expr (TFloat, EUnop (UONegF, arg))
            end
          | [arg1; arg2] -> begin match Builtins.get_binop name ts otc with
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
            | Some (Binop (BOEq t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOEq t, arg1, arg2))
            | Some (Binop (BONe t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BONe t, arg1, arg2))
            | Some (Binop (BOLe t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOLe t, arg1, arg2))
            | Some (Binop (BOLt t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOLt t, arg1, arg2))
            | Some (Binop (BOGe t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOGe t, arg1, arg2))
            | Some (Binop (BOGt t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 t in
                ge, le, Expr (TBool, EBinop (BOGt t, arg1, arg2))
            | Some (Binop BOConcat) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 TString in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 TString in
                ge, le, Expr (TString, EBinop (BOConcat, arg1, arg2))
            | Some (Binop (BOCons t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 t in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 (TList t) in
                ge, le, Expr (TList t, EBinop (BOCons t, arg1, arg2))
            | Some (Binop (BOConcatList t)) ->
                let ge, le, arg1 = of_value ge le arg1 in let arg1 = target_expr arg1 (TList t) in
                let ge, le, arg2 = of_value ge le arg2 in let arg2 = target_expr arg2 (TList t) in
                ge, le, Expr (TList t, EBinop (BOConcatList t, arg1, arg2))
            end
          | _ -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin n-ary operation"))
        end
      | _ -> failwith "TODO: of_value ApplyPure for non-Variable"
    end
  | Closure (f, tcl, cls) ->
      let ge, le, ACC (ga, bargs, bret, targs, tret, closed) = convert_closure ge le f tcl cls in
      let e = ESpecialize (closed, Snil ga, bargs, bret) in
      ge, le, Expr (TClosed (ga, targs, tret), e)
  | Coerce (v, t) ->
      let ge, le, (Expr (vt, _) as v) = of_value ge le v in
      let t = convert_type t in
      if t = Type vt then ge, le, v
      else failwith "TODO: of_value Coerce w/ different types"

and convert_values : type a. _ -> _ -> _ -> a typ_list -> _ * _ * a expr_list =
  fun (ge : ('pi, 'pa) genv) (le : 'args lenv) (vs : value list) (ts : a typ_list) : (('pi, 'pa) genv * 'args lenv * a expr_list) ->
  let ge = ref ge in
  let le = ref le in
  let [@tail_mod_cons] rec convert_args : type a. value list -> a typ_list -> a expr_list
      = fun (args : value list) (targs : a typ_list) : a expr_list -> match args, targs with
    | [], TLnil -> ELnil
    | [], TLcons _ -> raise (internal_error "Type mismatch: not enough expressions")
    | _ :: _, TLnil -> raise (internal_error "Type mismatch: too many expressions")
    | ehd :: etl, TLcons (thd, ttl) ->
        let ge', le', Expr (t, e) = of_value !ge !le ehd in
        let Type.Equal = assert_eq_typ t thd "Type mismatch: the argument does not have the correct type" in
        ge := ge'; le := le'; ELcons (e, convert_args etl ttl)
  in let ret = convert_args vs ts in !ge, !le, ret

let convert_values_unk (ge : ('pi, 'pa) genv) (le : 'args lenv) (vs : value list) : ('pi, 'pa) genv * 'args lenv * anyexpr_list =
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

let allocate_function (ge : ('pi, 'pa) genv) (tvars : tyvar list) (args : binder list) (b : binder) (closure : binder option)
    : ('pi, 'pa) genv * GEnv.funid * mfunid * LEnv.anyrealt =
  let le_args =
    let rec inner args acc = match args with
      | [] -> acc
      | hd :: tl -> let acc = LEnv.add_arg acc hd in inner tl acc
    in inner args LEnv.no_arg
  in
  let LEnv.AR le, tenv_clos = LEnv.env_of_args le_args closure in
  let ge, fid, loc_fid = GEnv.allocate_function ge le b tvars tenv_clos in
  ge, loc_fid, fid, LEnv.AR le

type 'a transformer = Transformer : ('a, 'b) finisher -> 'a transformer

let rec of_tail_computation : type args. _ -> args lenv -> _ -> ('pi, 'pa) genv * args lenv * anyexpr =
  fun (ge : ('pi, 'pa) genv) (le: args lenv) (tc : tail_computation) : (('pi, 'pa) genv * args lenv * anyexpr) -> match tc with
  | Return v -> let ge, le, v = of_value ge le v in ge, le, v
  | Apply (f, args) -> begin
      match collect_toplevel_polymorphism f with
      | ts, Variable v, otc -> begin match GEnv.find_fun ge le v with
          | ge, le, Function ((ga, Gnil, targs, tret, TLnil, _) as fid) ->
              let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
              let ge, le, args = convert_values ge le args targs in
              let () = match otc with None -> () | Some (Type t) -> ignore (assert_eq_typ t (TClosed (Gnil, targs, tret)) "Invalid type coercion") in
              let closed_applied = ECallClosed (ESpecialize (EClose (fid, BLnil, ELnil), s, bargs, bret), args, tret) in
              ge, le, Expr (tret, closed_applied)
          | ge, le, Closure (loc, ((TClosed (ga, targs, tret), _) as vid)) ->
              let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
              let ge, le, args = convert_values ge le args targs in
              let () = match otc with None -> () | Some (Type t) -> ignore (assert_eq_typ t (TClosed (Gnil, targs, tret)) "Invalid type coercion") in
              let closed_applied = ECallClosed (ESpecialize (EVariable (loc, vid), s, bargs, bret), args, tret) in
              ge, le, Expr (tret, closed_applied)
          | ge, le, Contin (loc, ((TCont tc, _) as vid), (TLcons (_, TLcons (targ, itargs)), tret, hdlfid), hdlcloc, hdlcid) ->
              if ts = [] then begin
                let targs = TLcons (targ, itargs) in
                let ge, le, ELcons (arg, iargs) = convert_values ge le args targs in
                let () = match otc with None -> () | Some (Type t) -> ignore (assert_eq_typ t (TClosed (Gnil, targs, tret)) "Invalid type coercion") in
                ge, le, Expr (tret,
                  ECallRawHandler (hdlfid, tc, EVariable (loc, vid),
                                           targ, arg,
                                           itargs, iargs,
                                           EVariable (hdlcloc, (TAbsClosArg, hdlcid)), tret))
              end else raise (internal_error "Invalid continuation: receives type applications")
          | ge, le, ClosedBuiltin name ->
              let ge, le, args = convert_values_unk ge le args in
              let ge, e = GEnv.gen_impure ge name ts args in
              let () = match otc, e with None, _ -> ()
                       | Some (Type t), Expr (tret, _) -> ignore (assert_eq_typ t tret "Invalid type coercion") in
              ge, le, e
        end
      | ts, Closure (f, tcl, cls), otc ->
          let ge, le, ACC (ga, bargs, bret, targs, tret, closed) = convert_closure ge le f tcl cls in
          (* TODO: try to remove this in some cases (i.e. if there is no unboxing) *)
          let s, SpecL (targs, bargs2), Spec (tret, bret2) = specialize_to_apply ga ts targs tret in
          let ge, le, args = convert_values ge le args targs in
          let () = match otc with None -> () | Some (Type t) -> ignore (assert_eq_typ t (TClosed (Gnil, targs, tret)) "Invalid type coercion") in
          let closed_applied = ECallClosed (ESpecialize (closed, s, compose_box_list bargs2 bargs, compose_box bret2 bret), args, tret) in
          ge, le, Expr (tret, closed_applied)
      | ts, Project (n, v), otc -> begin match v with
          | Variable v -> begin match LEnv.find_closure le v n with
              | Some (le, lst, VarID (TClosed (ga, targs, tret) as t, i)) ->
                  let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
                  let ge, le, args = convert_values ge le args targs in
                  let () = match otc with None -> () | Some (Type t) -> ignore (assert_eq_typ t (TClosed (Gnil, targs, tret)) "Invalid type coercion") in
                  let closed_applied = ECallClosed (ESpecialize (EVariable (Local lst, (t, i)), s, bargs, bret), args, tret) in
                  ge, le, Expr (tret, closed_applied)
              | Some (_, _, VarID (_, _)) -> raise (internal_error "Unexpected type, expected a function")
              | None -> failwith "TODO of_tail_computation Apply Project with unregistered projection"
            end
          | _ -> failwith "TODO of_tail_computation Apply Project with non-Variable"
        end
      | _ -> failwith "TODO of_tail_computation Apply with non-Variable and non-Closure"
      end
  | Special (DoOperation (tagname, args, tret)) ->
      let ge, le, ExprList (targs, args) = convert_values_unk ge le args in
      let ge, eid = GEnv.add_effect ge tagname in
      let Type tret = convert_type tret in
      ge, le, Expr (tret, EDo ((targs, eid), tret, args))
  | Special (Handle h) -> begin match h.ih_depth with
      | Shallow -> raise (Errors.RuntimeError "Wasm compilation of shallow handlers is currently not supported")
      | Deep bvs ->
          (* First the body ("computation" in the Links IR) *)
          let ge, Type (type b) (cret : b typ), body_id, ExprList (type c) (body_closts, body_closes : _ * c expr_list) =
            let body_le = LEnv.create_sub le None in
            let ge, body_le, Block (type b) (cret, (ass, e) : b typ * _) = of_computation ge (LEnv.of_sub body_le) h.ih_comp in
            let body_le = LEnv.to_sub body_le in
            (* Add closure conversion at the beginning of the block if required *)
            let body_le, oabsconc, ExprList (type c) (body_closts, body_closes : _ * c expr_list) = LEnv.get_closure body_le in
            let ass = match oabsconc with
              | None -> ass
              | Some (absid, concid) ->
                  Assign (Local StorVariable,
                          (TClosArg body_closts, concid),
                          EConvertClosure (absid, TClosArg body_closts))
                    :: ass in
            let b = Block (cret, (ass, e)) in
            (* Done, now add the "body" function to the global environment *)
            let ge, body_id =
              GEnv.new_cont_start ge body_le b (Option.map (fun (_, v) -> TypeList body_closts, v) oabsconc) in
            ge, Type cret, body_id, ExprList (body_closts, body_closes) in
          let body_id : (unit, b, c, unit, unit) funcid = (Gnil, Gnil, TLnil, cret, body_closts, body_id) in
          (* Now to the handler function (the rest of the tail_computation) *)
          let ge, le, ExprList (type f) (bvs_ts, bvs_es : _ * f expr_list) =
            let rec inner ge le bvs = match bvs with
              | [] -> ge, le, ExprList (TLnil, ELnil)
              | (_, vhd) :: tl ->
                  let ge, le, Expr (thd, ehd) = of_value ge le vhd in
                  let ge, le, ExprList (ts, es) = inner ge le tl in
                  ge, le, ExprList (TLcons (thd, ts), ELcons (ehd, es)) in
            inner ge le bvs in
          let ge, Type (type d) (tret : d typ), handler_id, ExprList (type e) (handler_closts, handler_closes : _ * e expr_list) =
            let hid, handler_id, ge = GEnv.allocate_fhandler ge in
            let handle_le = LEnv.create_sub le (Some bvs) in
            let ge, handle_le, Transformer (type d) (onret : (b, d) finisher) =
              of_finisher ge (LEnv.of_sub handle_le) h.ih_return cret in
            let handle_le = LEnv.to_sub handle_le in
            let tret = match onret with FId t -> (t : d typ) | FMap (_, t, _) -> t in
            let handle_le, contid = LEnv.add_cont handle_le (Type cret) handler_id (Type tret) in
            let contid : b continuation varid = TCont cret, contid in
            let ge, handle_le, ondo =
              let do_case (ge : ('pi, 'pa) genv) (handle_le : LEnv.subt) (ename : string) ((args, k, p) : effect_case)
                   : ('pi, 'pa) genv * LEnv.subt * (b, d) handler =
                let handle_le, VarIDList (eargs, vargs) = LEnv.set_handler_args handle_le args in
                let ge, eid = GEnv.add_effect ge ename in
                let handle_le = LEnv.set_continuation handle_le k in
                let ge, handle_le, Block (t, b) = of_computation ge (LEnv.of_sub handle_le) p in
                let handle_le = LEnv.to_sub handle_le in
                let Type.Equal = assert_eq_typ t tret "Expected the same type in the return branch as in all handler branches" in
                ge, handle_le, Handler ((eargs, eid), contid, vargs, b)
              in let do_case ename (ec : effect_case) (ge, handle_le, acc) =
                let ge, handle_le, hd = do_case ge handle_le ename ec in
                ge, handle_le, hd :: acc
              in Utility.StringMap.fold do_case h.ih_cases (ge, handle_le, []) in
            let handle_le, oabsconc, ExprList (type e) (hclosts, hcloses : _ * e expr_list) = LEnv.get_closure handle_le in
            let ge = GEnv.assign_fhandler ge hid handler_id handle_le oabsconc onret ondo in
            ge, Type tret, handler_id, ExprList (hclosts, hcloses) in
          let handler_id : (b continuation * (c closure_content * f), d, e, unit, unit) funcid =
            (Gnil, Gnil, TLcons (TCont cret, TLcons (TClosArg body_closts, bvs_ts)), tret, handler_closts, handler_id) in
          (* All done! *)
          ge, le, Expr (tret, EDeepHandle (body_id, body_closes, handler_id, handler_closes, bvs_es))
    end
  | Special (Wrong t) -> let Type t = convert_type t in ge, le, Expr (t, EUnreachable t)
  | Special _ -> failwith "TODO of_tail_computation Special"
  | Case (v, m, d) ->
      let ge, le, Expr (vt, v) = of_value ge le v in
      let Type.Equal = assert_eq_typ vt TVariant "Unexpected non-variant type in case computation" in
      let ge, le, m = Utility.StringMap.fold (fun tag (b, c) (ge, le, acc) ->
        let ge, tagid = GEnv.find_tag ge tag in
        let Type bt = convert_type (Var.type_of_binder b) in
        let le, argid = LEnv.add_var le b bt in
        let ge, le, blk = of_computation ge le c in
        ge, le, (tagid, VarID argid, blk) :: acc) m (ge, le, []) in
      let ge, le, d = match d with
        | None -> ge, le, None
        | Some (b, c) ->
            let Type bt = convert_type (Var.type_of_binder b) in
            let le, argid = LEnv.add_var le b bt in
            let ge, le, blk = of_computation ge le c in
            ge, le, Some (VarID argid, blk) in
      let Type (type r) (t : r typ) = match m with
        | (_, _, Block (t, _)) :: _ -> Type t
        | [] -> match d with
            | Some (_, Block (t, _)) -> Type t
            | None -> raise (internal_error "Empty case computation") in
      let m = List.map (fun (tid, VarID (bindt, bv), Block (bt, bb)) ->
          let Type.Equal = assert_eq_typ bt t "Unexpected case return type" in tid, Type bindt, bv, (bb : r block)) m in
      let d = Option.map (fun (VarID (_, bv), Block (bt, bb)) ->
                            let Type.Equal = assert_eq_typ bt t "Unexpected case return type" in bv, (bb : r block)) d in
      ge, le, Expr (t, ECase (v, t, m, d))
  | If (b, t, f) ->
      let ge, le, Expr (tb, eb) = of_value ge le b in
      let Type.Equal = assert_eq_typ tb TBool "Expected a boolean expression" in
      let ge, le, Block (tt, bt) = of_computation ge le t in
      let ge, le, Block (tf, bf) = of_computation ge le f in
      let Type.Equal = assert_eq_typ tt tf "Expected the same type in both branches" in
      ge, le, Expr (tt, ECond (tt, eb, bt, bf))
and of_computation : type args. _ -> args lenv -> _ -> _ * args lenv * _ =
  fun (ge : ('pi, 'pa) genv) (le : args lenv) ((bs, tc) : computation) : (('pi, 'pa) genv * args lenv * anyblock) ->
  let rec inner (ge : ('pi, 'pa) genv) (le : args lenv) (bs : binding list) (acc : assign list) = match bs with
    | [] ->
        let ge, le, Expr (t, e) = of_tail_computation ge le tc in
        ge, le, Block (t, (List.rev acc, e))
    (* FIXME: also support Let constructions with tyvar(s) *)
    | Let (b, (_, tc)) :: bs ->
        let ge, le, Expr (t, e) = of_tail_computation ge le tc in
        let ge, le, loc, v = GEnv.add_var ge le b t in
        let a = Assign (loc, v, e) in
        inner ge le bs (a :: acc)
    | Fun fd :: bs ->
        let ge, loc_fid, fid, LEnv.AR new_le = allocate_function ge fd.fn_tyvars fd.fn_params fd.fn_binder fd.fn_closure in
        let ge = finish_computation ge fd loc_fid fid new_le in
        inner ge le bs acc
    | Rec fds :: bs ->
        let rec compute ge fds = match fds with
          | [] -> inner ge le bs acc
          | (fd, loc_fid, fid, LEnv.AR new_le) :: fds ->
              let ge = finish_computation ge fd loc_fid fid new_le in
              compute ge fds
        in let rec allocate ge fds acc' = match fds with
          | [] -> compute ge acc'
          | fd :: fds ->
              let ge, loc_fid, fid, new_le = allocate_function ge fd.fn_tyvars fd.fn_params fd.fn_binder fd.fn_closure in
              allocate ge fds ((fd, loc_fid, fid, new_le) :: acc')
        in allocate ge fds []
    | Alien _ :: bs -> ignore bs; failwith "TODO of_computation Alien"
    | Module _ :: bs -> ignore bs; failwith "TODO of_computation Module"
  in inner ge le bs []
and of_finisher : 'a. _ -> _ -> _ -> 'a typ -> _ * _ * 'a transformer =
  fun (type a) (ge : ('pi, 'pa) genv) (le : 'args lenv) ((bind, comp) : binder * computation) (t : a typ)
    : (('pi, 'pa) genv * 'args lenv * a transformer) ->
  let bvar = Var.var_of_binder bind in
  match comp with
  | ([], Return (Variable v)) when Var.equal_var bvar v ->
      ge, le, Transformer (FId t)
  | ([Let (b2, ([], Return (Variable v1)))], Return (Variable v2))
        when Var.equal_var bvar v1 && Var.equal_var (Var.var_of_binder b2) v2 ->
      ge, le, Transformer (FId t)
  | _, _ ->
      let le, bid = LEnv.add_var le bind t in
      let ge, le, Block (t, b) = of_computation ge le comp in
      ge, le, Transformer (FMap (bid, t, b))

and finish_computation : type args. ('pi, 'pa) genv -> fun_def -> GEnv.funid -> mfunid -> args LEnv.realt -> ('pi, 'pa) genv =
  fun ge fd loc_fid fid (new_le : args LEnv.realt) ->
  let new_le = LEnv.of_real new_le in
  let ge, new_le, Block (tret, b) = of_computation ge new_le fd.fn_body in
  let new_le = LEnv.to_real new_le in
  let ge, new_le, b =
    match fd.fn_closure with
    | None -> ge, new_le, b
    | Some _ ->
        let new_le, oval = LEnv.add_closure new_le in
        match oval with
        | None -> ge, new_le, b
        | Some (_, _, TypeList TLnil) -> ge, new_le, b (* Prevent empty closure conversion (unsupported by the WasmUIR -> WasmFX translation) *)
        | Some (absid, concid, TypeList ctyp) ->
            let (ass, e) = b in
            ge, new_le, (Assign (Local StorVariable, (TClosArg ctyp, concid), EConvertClosure (absid, TClosArg ctyp)) :: ass, e) in
  let export_name =
    if Var.Scope.is_real_global (Var.scope_of_binder fd.fn_binder)
    then match fd.fn_closure with None ->
      let n = Var.name_of_binder fd.fn_binder in
      if n = "main" then Some "main'" else Some n | Some _ -> None
    else None in
  let f = LEnv.compile new_le fid export_name tret b in
  let ge = GEnv.assign_function ge loc_fid f in
  ge

let find_global_binders ((bs, _) : computation) =
  let rec inner bs acc = match bs with
    | [] -> Utility.StringMap.fold (fun _ b acc -> Utility.IntSet.add b acc) acc Utility.IntSet.empty
    | Let (b, _) :: bs ->
        if not (Var.Scope.is_real_global (Var.scope_of_binder b)) then inner bs acc
        else let name = Var.name_of_binder b in inner bs (if name = "" then acc else Utility.StringMap.add name (Var.var_of_binder b) acc)
    | Fun _ :: bs | Rec _ :: bs | Alien _ :: bs | Module _ :: bs -> inner bs acc
  in inner bs Utility.StringMap.empty

(* PRELUDE HELPERS *)

module VarMap = Utility.IntMap

type prelude_decl =
  | PreludeFun of fun_def
  | PreludeLet of binder * tail_computation
type prelude_map = prelude_decl VarMap.t

type prelude_info =
  | PreludeInfoFun of fun_def * GEnv.funid * meffid * LEnv.anyrealt
  | PreludeInfoLet of binder * anyvarid * tail_computation
type pgenv = (prelude_map, prelude_info) genv

let split_prelude (prelude : binding list) : binding list * prelude_map =
  let is_pure_tail (tc : tail_computation) : bool = match tc with
    | Return _ -> true
    | Apply _ -> false
    | Special _ -> false
    | Case _ -> false
    | If _ -> false in
  let rec inner l acc m = match l with
    | [] -> acc, m
    | Fun fd :: tl ->
        let m = VarMap.add (Var.var_of_binder fd.fn_binder) (PreludeFun fd) m in
        inner tl acc m
    | Rec rd :: tl ->
        let m = VarMap.add_seq (Seq.map (fun fd -> Var.var_of_binder fd.fn_binder, PreludeFun fd) (List.to_seq rd)) m in
        inner tl acc m
    | Let (b, ([], tc)) :: tl when is_pure_tail tc ->
        let m = VarMap.add (Var.var_of_binder b) (PreludeLet (b, tc)) m in
        inner tl acc m
    | (Let _ | Alien _ | Module _ as hd) :: tl -> inner tl (hd :: acc) m in
  inner prelude [] VarMap.empty

let locate_in_prelude (ge : pgenv) (m : prelude_map) (v : var) : (pgenv * prelude_map * prelude_info) option =
  match VarMap.find_opt v m with
  | None -> None
  | Some (PreludeFun fd) ->
      let ge, loc_fid, fid, new_le = allocate_function ge fd.fn_tyvars fd.fn_params fd.fn_binder fd.fn_closure in
      let m = VarMap.remove v m in
      Some (ge, m, PreludeInfoFun (fd, loc_fid, fid, new_le))
  | Some (PreludeLet (b, tc)) ->
      let Type t = convert_type (Var.type_of_binder b) in
      let ge, vid = GEnv.add_gbl ge b t in
      let m = VarMap.remove v m in
      Some (ge, m, PreludeInfoLet (b, VarID vid, tc))

let finish_prelude (ge : pgenv) (le : unit LEnv.realt) (pi : prelude_info) : pgenv * unit LEnv.realt * assign list = match pi with
  | PreludeInfoFun (fd, loc_fid, fid, LEnv.AR new_le) -> finish_computation ge fd loc_fid fid new_le, le, []
  | PreludeInfoLet (_, VarID (tv, vid), tc) ->
      let le = LEnv.of_real le in
      let ge, le, Expr (t, e) = of_tail_computation ge le tc in
      let le = LEnv.to_real le in
      let Type.Equal = assert_eq_typ t tv "Incompatible types in let-binding" in
      let a = Assign (Global, (t, vid), e) in
      ge, le, [a]

(* BACKEND AND FINAL VALUE PRINTER *)

let wasm_backend =
  let parse_backend s =
    match String.lowercase_ascii s with
    | "none"   -> Some BackendNone
    | "wizeng" -> Some BackendWizard
    | _ -> raise (Invalid_argument (Printf.sprintf "Unrecognised WebAssembly backend '%s'" s))
  in
  let string_of_backend = function
    | Some BackendNone -> "none"
    | Some BackendWizard -> "wizeng"
    | None -> "<none>"
  in
  (* TODO: change the default backend? *)
  Settings.(option ~default:(Some BackendWizard) "wasm_backend"
            |> privilege `System
            |> synopsis "Select the WebAssembly backend"
            |> convert parse_backend
            |> hidden
            |> to_string string_of_backend
            |> hint "<none|wizeng>"
            |> CLI.(add (long "wasm-backend"))
            |> sync)

let default_use_init () = match Settings.get wasm_backend with
  | Some BackendNone -> true
  | Some BackendWizard -> false
  | None -> true
let allow_use_init () = match Settings.get wasm_backend with
  | Some BackendNone -> true
  | Some BackendWizard -> false
  | None -> false

(* MAIN FUNCTION *)

let module_of_ir (c : program) (map : string Env.Int.t) (prelude : binding list) : anymodule =
  let rev_add_bs, prelude_funs = split_prelude prelude in
  let c =
    let (bs, tc) = c in
    (List.rev_append rev_add_bs bs, tc) in
  let ge = GEnv.empty map (find_global_binders c) (Option.get (Settings.get wasm_backend)) prelude_funs locate_in_prelude in
  let ge, le, blk = of_computation ge (LEnv.of_real LEnv.toplevel) c in
  GEnv.compile ge (LEnv.to_real le) blk finish_prelude
