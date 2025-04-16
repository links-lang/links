open Ir

type tagid = int
type tvarid = Types.tid  (* Type variable ID   *)
type mtypid = int        (* Module type ID     *)
type mvarid = int32      (* Module variable ID *)
type mfunid = int32      (* Module function ID *)
type meffid = int32      (* Module effect ID   *)
module MTypMap = Utility.IntMap
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

type llist = private LinksList
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
  | TList : 'a typ -> llist typ
  | TVar : tvarid -> unit typ
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list
and 'a named_typ_list =
  | NTLnil : unit named_typ_list
  | NTLcons : string * 'a typ * 'b named_typ_list -> ('a * 'b) named_typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list

let rec compare_generalization : type a b. a generalization -> b generalization -> _ = fun g1 g2 -> match g1, g2 with
  | Gnil, Gnil -> 0
  | Gnil, _ -> ~-1
  | _, Gnil -> 1
  | Gcons (hd1, tl1), Gcons (hd2, tl2) ->
      let c = Int.compare hd1 hd2 in if c <> 0 then c else
      compare_generalization tl1 tl2
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
  | TString, TString -> 0
  | TString, _ -> ~-1
  | _, TString -> 1
  | TClosed (g1, args1, ret1), TClosed (g2, args2, ret2) ->
      let c = compare_generalization g1 g2 in if c <> 0 then c else
      let c = compare_anytyp (Type ret1) (Type ret2) in if c <> 0 then c else
      compare_anytyp_list (TypeList args1) (TypeList args2)
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
  | TTuple tl1, TTuple tl2 -> compare_named_typ_list tl1 tl2
  | TTuple _, _ -> ~-1
  | _, TTuple _ -> 1
  | TVariant, TVariant -> 0
  | TVariant, _ -> ~-1
  | _, TVariant -> 1
  | TList t1, TList t2 -> compare_anytyp (Type t1) (Type t2)
  | TList _, _ -> ~-1
  | _, TList _ -> 1
  | TVar i1, TVar i2 -> Int.compare i1 i2
and compare_anytyp_list (TypeList tl1) (TypeList tl2) = match tl1, tl2 with
  | TLnil, TLnil -> 0
  | TLnil, TLcons _ -> ~-1 | TLcons _, TLnil -> 1
  | TLcons (hd1, tl1), TLcons (hd2, tl2) ->
      let chd = compare_anytyp (Type hd1) (Type hd2) in if chd = 0 then compare_anytyp_list (TypeList tl1) (TypeList tl2) else chd
and compare_named_typ_list : 'a 'b. 'a named_typ_list -> 'b named_typ_list -> _ = fun (type a b) (nl1 : a named_typ_list) (nl2 : b named_typ_list) ->
  match nl1, nl2 with
  | NTLnil, NTLnil -> 0
  | NTLcons _, NTLnil -> ~-1
  | NTLnil, NTLcons _ -> 1
  | NTLcons (n1, t1, nl1), NTLcons (n2, t2, nl2) ->
      let c = String.compare n1 n2 in if c <> 0 then c else
      let c = compare_anytyp (Type t1) (Type t2) in if c <> 0 then c else
      compare_named_typ_list nl1 nl2

module TypeMap = Utility.Map.Make(struct
  type t = anytyp
  let pp fmt (_ : t) = Format.fprintf fmt "<some type>"
  let show (_ : t) = "<some type>"
  let compare = compare_anytyp
end)

type ('a, 'b) extract_typ_check =
  | ExtractO : ('a * 'b, 'a) extract_typ_check
  | ExtractS : ('b, 'c) extract_typ_check -> ('a * 'b, 'c) extract_typ_check
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
type 'a varid = 'a typ * mvarid
type ('a, 'b, 'c, 'ga, 'gc) funcid = 'ga generalization * 'gc generalization * 'a typ_list * 'b typ * 'c typ_list * mfunid
type ('a, 'b) effectid = (unit * 'a -> 'b) typ * meffid

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
let rec dst_of_box : 'a 'b. ('a, 'b) box -> 'b typ = fun (type a b) (b : (a, b) box) : b typ -> match b with
  | BNone (_, dst) -> dst
  | BClosed (gen, bargs, bret) -> TClosed (gen, dst_of_box_list bargs, dst_of_box bret)
  | BCont bret -> TCont (dst_of_box bret)
  | BTuple bs -> TTuple (dst_of_box_named_list bs)
  | BBox (_, dst) -> TVar dst
and dst_of_box_list : 'a 'b. ('a, 'b) box_list -> 'b typ_list = fun (type a b) (b : (a, b) box_list) : b typ_list -> match b with
  | BLnil -> TLnil
  | BLcons (hd, tl) -> TLcons (dst_of_box hd, dst_of_box_list tl)
and dst_of_box_named_list : 'a 'b. ('a, 'b) box_named_list -> 'b named_typ_list =
  fun (type a b) (b : (a, b) box_named_list) : b named_typ_list -> match b with
  | BNLnil -> NTLnil
  | BNLcons (n, hd, tl) -> NTLcons (n, dst_of_box hd, dst_of_box_named_list tl)

type ('a, 'b) specialization =
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
  | ESpecialize : ('ga * 'a -> 'b) expr * ('gc, 'ga) specialization * ('c, 'a) box_list * ('d, 'b) box -> ('gc * 'c -> 'd) expr
  | ECallRawHandler : mfunid * 'a typ * 'a continuation expr * 'b typ * 'b expr * abs_closure_content expr * 'd typ -> 'd expr
      (* FIXME: add information in the continuation that it takes a 'b *)
  | ECallClosed : (unit * 'a -> 'b) expr * 'a expr_list * 'b typ -> 'b expr
  | ECond : 'a typ * bool expr * 'a block * 'a block -> 'a expr
  | EDo : ('a, 'b) effectid * 'a expr_list -> 'b expr
  | EShallowHandle : (unit, 'a, 'c, unit, unit) funcid * 'c expr_list * ('a, 'b) finisher * ('a, 'b) handler list -> 'b expr
  | EDeepHandle : (unit, 'b, 'c, unit, unit) funcid * 'c expr_list *
                  ('b continuation * ('c closure_content * unit), 'd, 'e, unit, unit) funcid * 'e expr_list -> 'd expr
and ('a, 'b) handler = (* The continuation itself returns 'a, the handler returns 'b *)
  (* Note: we lose the information that the continuation takes 'b as parameter(s) *)
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
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
  | ESpecialize (_, s, bargs, bret) -> TClosed (src_of_specialization s, src_of_box_list bargs, src_of_box bret)
  | ECallRawHandler (_, _, _, _, _, _, t) -> t
  | ECallClosed (_, _, t) -> t
  | ECond (t, _, _, _) -> t
  | EDo ((TClosed (_, _, t), _), _) -> t
  | EShallowHandle (_, _, FId t, _) -> t
  | EShallowHandle (_, _, FMap (_, t, _), _) -> t
  | EDeepHandle (_, _, (_, _, _, t, _, _), _) -> t

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
type ('a, 'b) fhandler = {
  fh_contarg : 'a continuation varid * mvarid;
  fh_closure : (mvarid * (anytyp_list * mvarid)) option;
  fh_locals  : anytyp list;
  fh_finisher: ('a, 'b) finisher;
  fh_handlers: ('a, 'b) handler list;
  fh_id      : mfunid;
}
type fbuiltin =
  | FBIntToString
type func =
  | FFunction : ('a, 'b) func' -> func
  | FContinuationStart : 'b fstart -> func
  | FHandler : ('a, 'b) fhandler -> func
  | FBuiltin of fbuiltin
type 'a modu = {
  mod_imports     : (string * string) list;
  mod_nfuns       : int32;
  mod_funs        : func list;
  mod_needs_export: (anytyp_list option * anytyp) FunIDMap.t;
  mod_typs        : anytyp_list MTypMap.t;
  mod_neffs       : int32;
  mod_effs        : EffectIDSet.t;
  mod_nglobals    : int32;
  mod_global_vars : (mvarid * anytyp * string) list;
  mod_locals      : anytyp list;
  mod_main        : 'a typ;
  mod_block       : 'a block;
}

type anymodule = Module : 'a modu -> anymodule

(* TRANSLATION *)

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
and assert_eq_typ : 'a 'b. 'a typ -> 'b typ -> string -> ('a, 'b) Type.eq =
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
let target_block (type a) (Block (t1, b) : anyblock) (t2 : a typ) : a block = let Type.Equal = assert_eq_typ t1 t2 "Unexpected type" in b

type anyunop = Unop : ('a, 'b) unop -> anyunop
type anybinop = Binop : ('a, 'b, 'c) binop -> anybinop

(* let rec extract_typ_check : 'a 'b. ('a, 'b) extract_typ -> _ = fun (type a b) ((s, n, t, chk) : (a, b) extract_typ) : unit -> match s, chk with
  | TTuple (NTLcons (_, hd, _)), ExtractO ->
      if n = 0 then let Type.Equal = assert_eq_typ hd t "Invalid type extraction data" in ()
      else raise (internal_error "Invalid type extraction data")
  | TTuple (NTLcons (_, _, tl)), ExtractS chk -> extract_typ_check (TTuple tl, n - 1, t, chk)
  | TTuple NTLnil, _ -> . *)

module Builtins : sig
  val ntags : tagid
  
  val get_unop : string -> anytyp list -> anyunop option
  val get_binop : string -> anytyp list -> anybinop option
  val gen_impure : 'a -> ('a -> fbuiltin -> 'a * mfunid) -> (Types.typ -> anytyp) -> string -> Ir.tyarg list -> anyexpr_list -> 'a * anyexpr
  
  val get_var : string -> anyexpr option
  
  val apply_type :
    Types.Abstype.t -> anytyp list -> (anytyp -> 'a) -> (tyvar list -> Types.typ -> Types.typ -> Types.typ -> 'a) ->
    (Types.field_spec_map -> Types.meta_row_var -> bool -> 'a) -> 'a
end = struct
  open Utility
  
  let tags = []
  let ntags = List.length tags
  let find_tag t = Option.get @@ List.find_index ((=) t) tags
  
  let unops = StringMap.from_alist ["negate", Unop UONegI; "negatef", Unop UONegF]
  let binops =
    let zero op : string -> anytyp list -> anybinop = fun opname tyargs ->
      if tyargs <> [] then raise (internal_error ("Invalid type application to builtin binary operator '" ^ opname ^ "'"))
      else op in
    let one op : string -> anytyp list -> anybinop = fun opname tyargs ->
      match tyargs with
      | [] -> raise (internal_error ("Missing type application to builtin binary operator '" ^ opname ^ "'"))
      | [t] -> op t
      | _ :: _ :: _ -> raise (internal_error ("Too many type applications to builtin binary operator '" ^ opname ^ "'")) in
    StringMap.from_alist [
      "+", zero (Binop BOAddI); "+.", zero (Binop BOAddF); "-", zero (Binop BOSubI); "-.", zero (Binop BOSubF);
      "*", zero (Binop BOMulI); "*.", zero (Binop BOMulF); "/", zero (Binop BODivI); "/.", zero (Binop BODivF);
      "%", zero (Binop BORemI); "==", one (fun (Type t) -> Binop (BOEq t)); "<>", one (fun (Type t) -> Binop (BONe t));
      "<=", one (fun (Type t) -> Binop (BOLe t)); "<", one (fun (Type t) -> Binop (BOLt t));
      ">=", one (fun (Type t) -> Binop (BOGe t)); ">", one (fun (Type t) -> Binop (BOGt t));
      "^^", zero (Binop BOConcat);
      "Cons", one (fun (Type t) -> Binop (BOCons t));
      "Concat", one (fun (Type t) -> Binop (BOConcatList t));
    ]
  
  let get_unop op _tyargs = StringMap.find_opt op unops
  let get_binop op tyargs = match StringMap.find_opt op binops with
    | None -> None
    | Some f -> Some (f op tyargs)
  
  let gen_impure (acc : 'a) (find_fbuiltin : 'a -> fbuiltin -> 'a * mfunid)
                 (convert_type : Types.typ -> anytyp) (op : string) (tyargs : Ir.tyarg list)
                 (ExprList (targs, args) : anyexpr_list) : 'a * anyexpr = match op with
    | "ignore" -> begin
        match targs, args with
        | TLcons (targ, TLnil), ELcons (arg, ELnil) -> acc, Expr (TTuple NTLnil, EIgnore (targ, arg))
        | _, ELnil -> raise (internal_error ("Not enough arguments for builtin function 'ignore'"))
        | _, ELcons (_, ELcons _) -> raise (internal_error ("Too many arguments for builtin function 'ignore'"))
      end
    | "error" -> begin match tyargs with
        | [] | [_] -> raise (internal_error "Not enough type argument for builtin function 'error'")
        | _ :: _ :: _ :: _ -> raise (internal_error "Too many type argument for builtin function 'error'")
        | [CommonTypes.PrimaryKind.(Presence | Type), _; _, _]
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.(Presence | Row), _] ->
             raise (internal_error "Invalid kind of type argument for builtin function 'error'")
        | [CommonTypes.PrimaryKind.Row, _; CommonTypes.PrimaryKind.Type, t] ->
        let Type t = convert_type t in
        match targs, args with
        | TLcons (_, TLnil), ELcons (_, ELnil) -> acc, Expr (t, EUnreachable t) (* TODO: add an error message *)
        | _, ELnil -> raise (internal_error ("Not enough arguments for builtin function 'error'"))
        | _, ELcons (_, ELcons _) -> raise (internal_error ("Too many arguments for builtin function 'error'"))
      end
    | "$$hd" -> begin match tyargs, args with
        | [], _ -> failwith "TODO $$hd without TApp"
        | [CommonTypes.PrimaryKind.Type, t; CommonTypes.PrimaryKind.Row, _], ELcons (arg, ELnil) ->
            let Type t = convert_type t in
            let argt = typ_of_expr arg in
            let Type.Equal = assert_eq_typ argt (TList t) "Invalid type of argument of $$hd" in
            acc, Expr (t, EListHd (arg, t))
        | _, _ -> raise (internal_error ("Invalid usage of builtin '$$hd'"))
      end
    | "$$tl" -> begin match tyargs, args with
        | [], _ -> failwith "TODO $$tl without TApp"
        | [CommonTypes.PrimaryKind.Type, _; CommonTypes.PrimaryKind.Row, _], ELcons (arg, ELnil) ->
            let argt = typ_of_expr arg in
            let Type.Equal = assert_eq_typ argt (TList argt) "Invalid type of argument of $$tl" in
            acc, Expr (TList argt, EListTl (argt, arg))
        | _, _ -> raise (internal_error ("Invalid usage of builtin '$$tl'"))
      end
    | "intToString" -> begin match tyargs, args with
        | [], _ -> failwith "TODO intToString without TApp"
        | [CommonTypes.PrimaryKind.Row, _], ELcons (arg, ELnil) ->
            let argt = typ_of_expr arg in
            let Type.Equal = assert_eq_typ argt TInt "Invalid type of argument of intToString" in
            let acc, fid = find_fbuiltin acc FBIntToString in
            let fid : (int * unit, string, unit, unit, unit) funcid = (Gnil, Gnil, TLcons (TInt, TLnil), TString, TLnil, fid) in
            acc, Expr (TString, ECallClosed (EClose (fid, BLnil, ELnil), ELcons (arg, ELnil), TString))
        | _, _ -> raise (internal_error ("Invalid usage of builtin 'intToString'"))
      end
    | _ -> ignore tyargs; raise (internal_error ("Unknown builtin impure function " ^ op))
  
  let get_var v : anyexpr option = match v with
    | "Nil" -> Some (Expr (TList (TVar ~-1), EListNil (TVar ~-1)))
    | _ -> None
  
  let apply_type (at : Types.Abstype.t) (ts : anytyp list)
      (normal : anytyp -> 'a) (func : tyvar list -> Types.typ -> Types.typ -> Types.typ -> 'a)
      (row : Types.field_spec_map -> Types.meta_row_var -> bool -> 'a) : 'a =
    let is at2 = Types.Abstype.compare at at2 = 0 in
    ignore (func, row);
    if is Types.list then match ts with
      | [Type t] -> normal (Type (TList t))
      | _ -> raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
    else raise (internal_error ("Unknown abstract type " ^ (Types.Abstype.show at)))
end

let sort_name_map (nm : 'a Ir.name_map) : (string * 'a) list =
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

let _to_typelist (conv : Types.typ -> anytyp) (ts : Types.typ Ir.name_map) : anyntyp_list =
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
type 'a anyfuncid = FuncID : ('b, 'c, 'd, 'gb, 'gd) funcid * 'a -> 'a anyfuncid
type 'a anycfuncid = ACFunction : ('b, 'c, unit, 'ga, unit) funcid * 'a -> 'a anycfuncid

type anyvarid_list = VarIDList : 'a typ_list * 'a varid_list -> anyvarid_list

type 'a closed_like =
  | Function : ('a, 'b, unit, 'ga, unit) funcid * 'c -> 'c closed_like
  | Closure : locality * ('ga * 'a -> 'b) varid -> 'c closed_like
  (* FIXME: see ECont
  | Contin : locality * 'd continuation varid * ('d continuation * ('a closure_content * unit), 'b, 'e) funcid *
             locality * 'e closure_content varid -> ('a, 'b, 'c) closed_like *)
  | Contin : locality * 'd continuation varid * (('d continuation * ('a * unit)) typ_list * 'b typ * mtypid * mfunid) *
             locality * mvarid -> 'c closed_like
  | ClosedBuiltin of string

module LEnv : sig (* Contains the arguments, the local variables, etc *)
  type 'a t
  type 'a realt
  type subt
  val toplevel : unit realt
  val create_sub : 'a t -> bool -> subt
  
  val of_real : 'a realt -> 'a t
  val of_sub : subt -> unit t
  val to_real : 'a t -> 'a realt
  val to_sub : unit t -> subt
  
  val get_closure : subt -> subt * (mvarid * mvarid) option * anyexpr_list
  
  (* Internal use by the global env only *)
  val args_typ : 'a realt -> 'a typ_list * anytyp_list
  val find_continuation :
    'a t -> var -> ('a t * local_storage * anytyp * anyvarid * mfunid * anytyp * mtypid * local_storage * mvarid) option
  
  type args
  type anyrealt = AR : 'a realt -> anyrealt
  val no_arg : args
  val add_arg : args -> binder -> args
  val env_of_args : args -> binder option -> anyrealt * anygeneralization
  val add_closure : 'a realt -> 'a realt * mvarid * mvarid * anytyp_list
  
  val add_local : 'a t -> anytyp -> 'a t * mvarid
  val add_var : 'a t -> binder -> anytyp -> 'a t * mvarid
  val find_var : 'a t -> var -> ('a t * local_storage * anyvarid) option
  val find_closure : 'a t -> var -> string -> ('a t * local_storage * anyvarid) option
  
  val set_handler_args : subt -> binder -> subt * anyvarid_list
  val add_cont : subt -> anytyp -> mfunid -> anytyp -> mtypid -> subt * mvarid
  val set_continuation : subt -> binder -> anytyp -> subt
  
  val locals_of_env : 'a realt -> anytyp list
  val compile : 'a realt -> mfunid -> string option -> 'b typ -> 'b block -> ('a, 'b) func'
  val compile_cont_start : subt -> mfunid -> 'b typ -> 'b block -> (anytyp_list * mvarid) option -> 'b fstart
  val compile_handler : subt -> mfunid -> (mvarid * mvarid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> ('b, 'd) fhandler
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
    contmap        : (anytyp * anyvarid * mfunid * anytyp * mtypid * mvarid) Env.Int.t;
    nclos          : int32;
    clos           : anyexpr_list;
    cvarmap        : (local_storage * anyvarid) IntString.t;
    mutable contid : (anyvarid * mvarid * mfunid * anytyp * mtypid * mvarid) option;
    mutable contv  : anytyp * var;
    mutable hdlb   : var;
  }
  and 'a t = ('a realt, subt) Either.t
  and anyt = AT : 'a t -> anyt
  
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
  let create_sub (base : 'a t) (is_handler : bool) : subt = {
      base = AT base;
      nargs = if is_handler then 3l else 1l;
      nlocs = 0l;
      locs = TypeList TLnil;
      varmap = Env.Int.empty;
      contmap = Env.Int.empty;
      nclos = 0l;
      clos = ExprList (TLnil, ELnil);
      cvarmap = IntString.empty;
      contid = None;
      contv = (Type (TTuple NTLnil), ~-1);
      hdlb = ~-1;
    }
  
  let of_real (env : 'a realt) : 'a t = Either.Left env
  let of_sub (env : subt) : unit t = Either.Right env
  let to_real (env : 'a t) : 'a realt = match env with Either.Left env -> env | Either.Right _ -> raise (internal_error "Invalid local environment kind")
  let to_sub (env : unit t) : subt = match env with Either.Right env -> env | Either.Left _ -> raise (internal_error "Invalid local environment kind")
  
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
  
  let get_closure (env : subt) : subt * (mvarid * mvarid) option * anyexpr_list =
    let cexpr = extract_exprs env.clos in
    let oconv, nlocs, locs = match cexpr with
      | ExprList (TLnil, ELnil) -> None, env.nlocs, env.locs
      | ExprList (cts, _) ->
        Some (Int32.sub env.nargs 1l, Int32.add env.nargs env.nlocs),
          Int32.succ env.nlocs,
          (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cts, tl)))
    in { env with nlocs; locs }, oconv, cexpr
  
  let args_typ (env : 'a realt) : 'a typ_list * anytyp_list =
    env.args, (let (_, cat, _) = env.clos in cat)
  let rec find_continuation : type a. a t -> _ -> (a t * _ * _ * _ * _ * _ * _ * _ * _) option = fun (env : a t) (v : var)
      : (a t * local_storage * anytyp * anyvarid * mfunid * anytyp * mtypid * local_storage * mvarid) option ->
    match env with
    | Either.Left _ -> None
    | Either.Right ({ base = AT base; contid; contv = targ, contv; contmap; _ } as env) -> begin match Env.Int.find_opt v contmap with
        | Some (targ, VarID (t, cid), hdlfid, thdlret, thdlid, hdlcid) ->
            Some (Either.Right env, StorClosure, targ, VarID (t, cid), hdlfid, thdlret, thdlid, StorClosure, hdlcid)
        | None -> begin match
              match contid with None -> None | Some (VarID (t, cid), _, hdlfid, thdlret, thdlid, hdlcid) ->
                if Var.equal_var v contv then
                  Some (Either.Right env, StorVariable, targ, VarID (t, cid),
                        hdlfid, thdlret, thdlid, StorVariable, hdlcid)
                else None
              with Some v -> Some v | None ->
            match find_continuation base v with
            | None -> None
            | Some (base, loc, targ, VarID (t, bid), hdlfid, thdlret, thdlid, hdlcloc, hdlbcid) ->
                let cid = env.nclos in
                let hdlcid = Int32.succ cid in
                let nclos = Int32.succ hdlcid in
                let clos =
                  let ExprList (tl, es) = env.clos in
                  ExprList (TLcons (TClosArg TLnil, TLcons (TCont t, tl)),
                            ELcons (EVariable (Local hdlcloc, (TClosArg TLnil, hdlbcid)), ELcons (EVariable (Local loc, (TCont t, bid)), es))) in
                let contmap = Env.Int.bind v (targ, VarID (t, cid), hdlfid, thdlret, thdlid, hdlcid) env.contmap in
                Some (Either.Right { env with base = AT base; nclos; clos; contmap }, StorClosure, targ, VarID (t, cid),
                      hdlfid, thdlret, thdlid, StorClosure, hdlcid)
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
  
  let add_closure (env : 'a realt) : 'a realt * mvarid * mvarid * anytyp_list =
    let acid, TypeList cat = match env.clos with
      | _, _, true -> raise (internal_error "Double add_closure call")
      | acid, cat, false -> acid, cat
    in let ccid = Int32.add env.nargs env.nlocs in
    { env with
      nlocs = Int32.succ env.nlocs;
      locs = (let TypeList tl = env.locs in TypeList (TLcons (TClosArg cat, tl)));
      clos = ccid, TypeList cat, true;
    }, acid, ccid, TypeList cat
  
  let add_sub_to_env (env : subt) (base : 'a t) (loc : local_storage) (VarID (t, bid) : anyvarid) : subt * anyvarid =
    let cid = env.nclos in
    let hdlcid = Int32.succ cid in
    let nclos = Int32.succ hdlcid in
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
  let add_var (env : 'a t) (b : binder) (Type t : anytyp) : 'a t * mvarid = match env with
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
        | Some (lst, ret) -> Some (Either.Right env, lst, ret)
        | None -> begin
            let AT base = env.base in
            match find_closure base v n with
            | Some (base, lst, bid) ->
                let env, cid = add_sub_to_env env base lst bid in
                let cvarmap = IntString.bind (v, n) (lst, cid) env.cvarmap in
                Some (Either.Right { env with base = AT base; cvarmap }, lst, cid)
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
  let set_continuation (env : subt) (b : binder) (targ : anytyp) : subt = match env.contid with
    | None -> raise (internal_error "Cannot set missing continuation")
    | Some _ -> env.contv <- (targ, Var.var_of_binder b); env
  
  let locals_of_env (env : 'a realt) : anytyp list = extract_to_list env.locs []
  
  let compile (env : 'a realt) (fid : mfunid) (export_name : string option) (tret : 'b typ) (b : 'b block) : ('a, 'b) func' =
    let closid, clt, has_converted_closure = env.clos in
    let export_data = match export_name with
      | Some name -> Some name
      | None -> None
    in let convclos = if has_converted_closure then Some (clt, closid) else None
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
                      (onret : (b, d) finisher) (ondo : (b, d) handler list) : (b, d) fhandler =
    let contarg, argcontidx = match env.contid with
      | Some (VarID (_, i), j, _, _, _, _) -> i, j
      | None -> raise (internal_error "Handlers must have a continuation added") in
    let tcont = match onret with FId t | FMap ((t, _), _, _) -> t in
    let ExprList (clostyp, _) = env.clos in {
      fh_contarg  = (TCont tcont, contarg), argcontidx;
      fh_closure  = Option.map (fun (absid, concid) -> absid, (TypeList clostyp, concid)) oabsconc;
      fh_locals   = extract_to_list env.locs [];
      fh_finisher = onret;
      fh_handlers = ondo;
      fh_id       = fid;
    }
end

module GEnv : sig (* Contains the functions, the types, etc *)
  type t
  type funid
  type hid
  val empty : string Env.Int.t -> Utility.IntSet.t -> bool -> t
  
  val find_tag : t -> string -> t * tagid
  
  type typid_list
  val new_generic_list : t -> t * typid_list * mtypid
  val set_generic_list : t -> typid_list -> anytyp_list -> t
  
  val get_var_name : t -> var -> string
  val find_fun : t -> 'a LEnv.t -> var -> 'a LEnv.t * funid closed_like
  val find_closable_fun : t -> 'a LEnv.t -> var -> funid anyfuncid
  
  val add_var : t -> 'a LEnv.t -> binder -> anytyp -> t * 'a LEnv.t * locality * mvarid
  val find_var : t -> 'a LEnv.t -> var -> ('a LEnv.t * locality * anyvarid, funid anycfuncid) Either.t option
  
  val allocate_function : t -> 'a LEnv.realt -> binder -> tyvar list -> anygeneralization -> funid * mfunid * t
  val assign_function : t -> funid -> ('a, 'b) func' -> t
  val do_export_function : t -> funid -> t
  
  val new_continuator : t -> mfunid -> anytyp -> anytyp -> anytyp -> t * mfunid
  val new_cont_start : t -> LEnv.subt -> anyblock -> (anytyp_list * mvarid) option -> t * mfunid * funid
  val allocate_fhandler : t -> hid * mfunid * t
  val assign_fhandler : t -> hid -> mfunid -> LEnv.subt -> (mvarid * mvarid) option -> ('b, 'd) finisher -> ('b, 'd) handler list -> t
  
  val find_fbuiltin : t -> fbuiltin -> t * mfunid
  
  val add_effect : t -> string -> t * meffid
  
  val compile : t -> unit LEnv.realt -> anyblock -> anymodule
end = struct
  (* In-place function emplacement in the environment *)
  type anyfunc' = AFFnc : ('a, 'b) func' -> anyfunc' | AFSt : 'b fstart -> anyfunc'
  type anyfhandler = AFHdl : ('a, 'b) fhandler -> anyfhandler
  type funid = anyfunc' option ref * bool ref
  type hid = anyfhandler option ref
  
  module EffectMap = Utility.StringMap
  
  type t = {
    ge_imports : (string * string) list;
    ge_map : string Env.Int.t;
    ge_nfuns : mfunid;
    ge_funs : (anyfunc' option ref * mfunid * bool ref,
               (anyfhandler option ref,
                (anyfunc' * mfunid,
                 fbuiltin) Either.t) Either.t) Either.t list;
    ge_ntags : tagid;
    ge_tagmap : tagid Env.String.t;
    ge_ntyps : mtypid;
    ge_typs : anytyp_list list;
    ge_neffs : meffid;
    ge_effs : EffectIDSet.t;
    ge_effmap : meffid EffectMap.t;
    ge_ngbls : mvarid;
    ge_gbls : (mvarid * anytyp * string) list;
    ge_gblbinders : Utility.IntSet.t;
    ge_gblmap : anyvarid Env.Int.t;
    ge_fmap : funid anyfuncid Env.Int.t;
    ge_fbs : mfunid option;
  }
  let empty (m : string Env.Int.t) (global_binders : Utility.IntSet.t) (import_wizard : bool) : t =
    let tmap = [TypeList TLnil, 0] in
    let ge_nfuns = if import_wizard then 2l else 0l in
    let ge_ntags = if import_wizard then 0 else 0 in {
      ge_imports = if import_wizard then ["wizeng", "puts"; "wizeng", "putc"] else [];
      ge_map = m;
      ge_nfuns;
      ge_funs = [];
      ge_ntyps = List.length tmap;
      ge_ntags;
      ge_tagmap = Env.String.empty;
      ge_typs = List.rev_map fst tmap;
      ge_neffs = 0l;
      ge_effs = EffectIDSet.empty;
      ge_effmap = EffectMap.empty;
      ge_ngbls = 0l;
      ge_gbls = [];
      ge_gblbinders = global_binders;
      ge_gblmap = Env.Int.empty;
      ge_fmap = Env.Int.empty;
      ge_fbs = None;
    }
  
  let find_tag (env : t) (tname : string) : t * tagid = match Env.String.find_opt tname env.ge_tagmap with
    | Some i -> env, i
    | None ->
        let tagid = env.ge_ntags in
        let ge_ntags = Int.succ tagid in
        let ge_tagmap = Env.String.bind tname tagid env.ge_tagmap in
        { env with ge_ntags; ge_tagmap }, tagid
  
  type typid_list = int
  let new_generic_list (env : t) : t * typid_list * mtypid =
    let typid = env.ge_ntyps in
    let ge_ntyps = Int.succ typid in
    let ge_typs = TypeList TLnil :: env.ge_typs in
    { env with ge_ntyps; ge_typs }, typid, typid
  let set_generic_list (env : t) (typid : typid_list) (typ : anytyp_list) : t =
    let ge_typs =
      let rec inner i typs acc = match i, typs with
        | _, [] -> raise (internal_error "Invalid typid in Wasmir.GEnv.set_generic_list")
        | 0, _ :: tl -> List.rev_append acc (typ :: tl)
        | _, hd :: tl -> inner (i - 1) tl (hd :: acc)
      in inner (env.ge_ntyps - typid - 1) env.ge_typs [] in
    { env with ge_typs }
  
  let get_var_name (ge : t) (v : var) = Env.Int.find v ge.ge_map
  
  let add_var (ge : t) (le : 'a LEnv.t) (b : binder) (t : anytyp) : t * 'a LEnv.t * locality * mvarid =
    if Utility.IntSet.mem (Var.var_of_binder b) ge.ge_gblbinders then begin
      let ge_ngbls = Int32.succ ge.ge_ngbls in
      let ge_gbls = (ge.ge_ngbls, t, Var.name_of_binder b) :: ge.ge_gbls in
      let Type t = t in
      let newvar = VarID (t, ge.ge_ngbls) in
      let ge_gblmap = Env.Int.bind (Var.var_of_binder b) newvar ge.ge_gblmap in
      { ge with ge_ngbls; ge_gbls; ge_gblmap }, le, Global, ge.ge_ngbls
    end else let le, v = LEnv.add_var le b t in ge, le, Local StorVariable, v
  let find_var (ge : t) (le : 'a LEnv.t) (v : var) : ('a LEnv.t * locality * anyvarid, funid anycfuncid) Either.t option =
    match LEnv.find_var le v with
    | Some (le, st, v) -> Some (Either.Left (le, Local st, v))
    | None -> begin match Env.Int.find_opt v ge.ge_fmap with
        | Some (FuncID (type b c d gb gd) ((_, gd, _, _, ctyp, _) as fid, fdata : (b, c, d, gb, gd) funcid * _)) -> begin match gd, ctyp with
            | Gnil, TLnil -> Some (Either.Right (ACFunction (fid, fdata)))
            | Gcons _, TLnil -> raise (internal_error "Unexpected generalization of closure with no value")
            | _, TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
          end
        | None -> Option.map (fun v -> Either.Left (le, Global, v)) (Env.Int.find_opt v ge.ge_gblmap)
      end
  
  let find_fun (ge : t) (le : 'a LEnv.t) (v : var) : 'a LEnv.t * funid closed_like = match Env.Int.find_opt v ge.ge_fmap with
    | Some (FuncID ((_, gd, _, _, ctyp, _) as fid, fdata)) -> begin match gd, ctyp with
        | Gnil, TLnil -> le, Function (fid, fdata)
        | Gcons _, TLnil -> raise (internal_error "Unexpected generalization of closure with no value")
        | _, TLcons _ -> raise (internal_error "Unexpected open function, expected closed function")
      end
    | None -> begin match LEnv.find_continuation le v with
        | Some (le, loc, Type arg, VarID (ret, vid), hdlfid, Type tret, thdlid, hdlcloc, hdlcid) ->
            le, Contin (
              Local loc,
              (TCont ret, vid),
              (TLcons (TCont ret, TLcons (arg, TLnil)), tret, thdlid, hdlfid),
              Local hdlcloc,
              hdlcid)
        | None -> begin match find_var ge le v with
            | Some (Either.Left (le, loc, VarID ((t, _) as vid))) -> begin match t with
                | TClosed _ -> le, Closure (loc, vid)
                | _ -> raise (internal_error "Unexpected variable type, expected closed function")
              end
            | Some (Either.Right (ACFunction (f, fid))) -> le, Function (f, fid)
            | None ->
                let name = get_var_name ge v in
                le, ClosedBuiltin name
          end
      end
  let find_closable_fun (ge : t) (_ : 'a LEnv.t) (v : var) : funid anyfuncid = Env.Int.find v ge.ge_fmap
  
  let allocate_function (env : t) (args : 'a LEnv.realt) (b : binder) (tyvars : tyvar list)
      (AG gc : anygeneralization) : funid * mfunid * t =
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
          | TVar i -> TVar (TVarMap.find i map)
        and inner_list : type a. _ -> a typ_list -> a typ_list = fun map ts -> match ts with
          | TLnil -> TLnil
          | TLcons (hd, tl) -> TLcons (inner map hd, inner_list map tl)
        and inner_named_list : type a. _ -> a named_typ_list -> a named_typ_list = fun map ts -> match ts with
          | NTLnil -> NTLnil
          | NTLcons (n, hd, tl) -> NTLcons (n, inner map hd, inner_named_list map tl)
        in ga, inner map tret in
      let ga =
        let rec inner : type a c. a generalization -> c generalization -> anygeneralization = fun ga gc -> match gc with
          | Gnil -> AG ga
          | Gcons (_, tl) -> match ga with
              | Gnil -> raise (internal_error "Invalid type: closure has more type variables than the function")
              | Gcons (_, tlc) -> inner tl tlc in
        let AG ga = ga in
        inner ga gc in
      ga, Type tret in
    let fid = env.ge_nfuns in
    let needs_export = ref false in
    let fdata = (f, needs_export) in
    let ge_fmap = Env.Int.bind (Var.var_of_binder b) (FuncID ((ga, gc, targs, tret, ctyp, fid), fdata)) env.ge_fmap in
    fdata, fid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = Either.Left (f, fid, needs_export) :: env.ge_funs;
      ge_fmap;
    }
  let assign_function (env : t) ((fid, _) : funid) (f : ('a, 'b) func') : t = match !fid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> fid := Some (AFFnc f); env
  let do_export_function (env : t) ((_, fref) : funid) : t = fref := true; env
  
  let new_continuator (env : t) (hdlfid : mfunid) (Type cret : anytyp) (Type targ : anytyp) (Type tret : anytyp) : t * mfunid =
    (* Continuation argument[targ] at position 0, abstract closure at position 1, concrete closure at position 2 *)
    (* Continuation[cret] in closure at position 0, continuation handler closure content in closure at position 1 *)
    let ct = TLcons (TCont cret, TLcons (TAbsClosArg, TLnil)) in
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ fid in
    let b = ([
      Assign (Local StorVariable, (TClosArg ct, 2l), EConvertClosure (1l, TClosArg ct))],
      ECallRawHandler (hdlfid, cret, EVariable (Local StorClosure, (TCont cret, 0l)),
                               targ, EVariable (Local StorVariable, (targ, 0l)),
                               EVariable (Local StorClosure, (TAbsClosArg, 1l)), tret)) in
    let f = {
      fun_id = fid;
      fun_export_data = None;
      fun_converted_closure = Some (TypeList ct, 2l);
      fun_args = TLcons (targ, TLnil);
      fun_ret = tret;
      fun_locals = [Type (TClosArg ct)];
      fun_block = b;
    } in
    { env with
      ge_nfuns;
      ge_funs = Either.Right (Either.Right (Either.Left (AFFnc f, fid))) :: env.ge_funs;
    }, fid
  let new_cont_start (env : t) (args : LEnv.subt) (b : anyblock) (cclosid : (anytyp_list * mvarid) option) : t * mfunid * funid =
    let fid = env.ge_nfuns in
    let ge_nfuns = Int32.succ env.ge_nfuns in
    let Block (tret, b) = b in
    let f = LEnv.compile_cont_start args fid tret b cclosid in
    let f, export = ref (Some (AFSt f)), ref false in
    let fun_data = f, export in
    { env with
      ge_nfuns;
      ge_funs = Either.Left (f, fid, export) :: env.ge_funs;
    }, fid, fun_data
  let allocate_fhandler (env : t) : hid * mfunid * t =
    let f = ref None in
    let fid = env.ge_nfuns in
    let fdata = f in
    fdata, fid, { env with
      ge_nfuns = Int32.succ env.ge_nfuns;
      ge_funs = Either.Right (Either.Left f) :: env.ge_funs;
    }
  let assign_fhandler (env : t) (hid : hid) (self_mid : mfunid) (args : LEnv.subt) (oabsconc : (mvarid * mvarid) option)
                      (onret : ('b, 'd) finisher) (ondo : ('b, 'd) handler list) : t = match !hid with
    | Some _ -> raise (internal_error "double assignment of function")
    | None -> hid := Some (AFHdl (LEnv.compile_handler args self_mid oabsconc onret ondo)); env
  
  let find_fbuiltin (env : t) (fb : fbuiltin) : t * mfunid = match env, fb with
    | { ge_fbs = Some i; _ }, FBIntToString -> env, i
    | { ge_fbs = None; _ }, FBIntToString ->
        let i = env.ge_nfuns in
        let ge_funs = Either.Right (Either.Right (Either.Right FBIntToString)) :: env.ge_funs in
        let ge_nfuns = Int32.succ i in
        let ge_fbs = Some i in
        { env with ge_funs; ge_nfuns; ge_fbs; }, i
  
  let add_effect (env : t) (ename : string) : t * meffid =
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
  
  let compile (ge : t) (le : unit LEnv.realt) (Block (t, blk) : anyblock) : anymodule =
    let lvs = LEnv.locals_of_env le in
    let ge, mod_needs_export = List.fold_left (fun (ge, acc) -> function
          | Either.Left (fo, fid, rb) ->
              if !rb then
                match !fo with
                | None -> raise (internal_error "function was allocated but never assigned")
                | Some (AFFnc f) ->
                    ge, FunIDMap.add fid (Some (TypeList f.fun_args), Type f.fun_ret) acc
                | Some (AFSt f) ->
                    ge, FunIDMap.add fid (None, Type f.fst_ret) acc
              else ge, acc
          | Either.Right (Either.Left _) -> ge, acc (* We don't need to export handlers *)
          | Either.Right (Either.Right (Either.Left (AFFnc f, fid))) ->
              ge, FunIDMap.add fid (Some (TypeList f.fun_args), Type f.fun_ret) acc (* We do need to export continuators *)
          | Either.Right (Either.Right (Either.Left (AFSt f, fid))) ->
              ge, FunIDMap.add fid (None, Type f.fst_ret) acc (* We do need to export continuators *)
          | Either.Right (Either.Right (Either.Right _)) -> ge, acc (* We don't need to export builtin functions *))
        (ge, FunIDMap.empty) ge.ge_funs in
    Module {
      mod_imports = ge.ge_imports;
      mod_nfuns = ge.ge_nfuns;
      mod_funs =
        List.rev_map
          (fun f -> match f with
            | Either.Left ({ contents = None }, _, _) -> raise (internal_error "function was allocated but never assigned")
            | Either.Left ({ contents = Some (AFFnc f) }, _, _) -> FFunction f
            | Either.Left ({ contents = Some (AFSt f) }, _, _) -> FContinuationStart f
            | Either.Right (Either.Left { contents = None }) -> raise (internal_error "handler was allocated but never assigned")
            | Either.Right (Either.Left { contents = Some (AFHdl f) }) -> FHandler f
            | Either.Right (Either.Right (Either.Left (AFFnc f, _))) -> FFunction f
            | Either.Right (Either.Right (Either.Left (AFSt f, _))) -> FContinuationStart f
            | Either.Right (Either.Right (Either.Right fb)) -> FBuiltin fb)
          ge.ge_funs;
      mod_needs_export;
      mod_typs = MTypMap.of_seq (Seq.mapi (fun i v -> i, v) (List.to_seq (List.rev ge.ge_typs)));
      mod_neffs = ge.ge_neffs;
      mod_effs = ge.ge_effs;
      mod_nglobals = ge.ge_ngbls;
      mod_global_vars = List.rev ge.ge_gbls;
      mod_locals = lvs;
      mod_main = t;
      mod_block = blk;
    }
end
type genv = GEnv.t
type 'args lenv = 'args LEnv.t

let of_constant (c : CommonTypes.Constant.t) : anyexpr = let open CommonTypes.Constant in match c with
  | Float f -> Expr (TFloat, EConstFloat f)
  | Int i -> Expr (TInt, EConstInt (Int64.of_int i))
  | Bool b -> Expr (TBool, EConstBool b)
  | String s -> Expr (TString, EConstString s)
  | Char _ -> failwith "TODO: of_constant Char"
  | DateTime _ -> failwith "TODO: of_constant DateTime"

let collect_toplevel_polymorphism (v : value) : Ir.tyarg list * value =
  let rec inner v acc = match v with
    | TApp (v, ts) -> inner v (acc @ ts)
    | TAbs _ -> failwith "TODO TAbs"
    | _ -> acc, v
  in inner v []

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

let rec of_value (ge : genv) (le: 'args lenv) (v : value) : genv * 'args lenv * anyexpr = match v with
  | Constant c -> let Expr (ct, cv) = of_constant c in ge, le, Expr (ct, cv)
  | Variable v -> begin match GEnv.find_var ge le v with
      | Some (Either.Left (le, loc, VarID (t, vid))) -> ge, le, Expr (t, EVariable (loc, (t, vid)))
      | Some (Either.Right (ACFunction ((ga, Gnil, targs, tret, TLnil, _) as f, fhandle))) ->
          let ge = GEnv.do_export_function ge fhandle in
          ge, le, Expr (TClosed (ga, targs, tret), EClose (f, BLnil, ELnil))
      | None -> begin match LEnv.find_continuation le v with
          | Some (le, loc, Type (type a) (arg : a typ), VarID (type c) (ret, vid : c typ * _),
                  hdlfid, Type (type b) (tret : b typ), _, hdlcloc, hdlcid) ->
              let ge, (fid : mfunid) = GEnv.new_continuator ge hdlfid (Type ret) (Type arg) (Type tret) in
              let fct = TLcons (TCont ret, TLcons (TAbsClosArg, TLnil)) in
              let fid : (a * unit, b, c continuation * (abs_closure_content * unit), _, _) funcid = Gnil, Gnil, TLcons (arg, TLnil), tret, fct, fid in
              ge, le, Expr (TClosed (Gnil, TLcons (arg, TLnil), tret),
                            EClose (fid, BLcons (BNone (TCont ret, TCont ret),
                                         BLcons (BNone (TAbsClosArg, TAbsClosArg), BLnil)),
                                         ELcons (EVariable (Local loc, (TCont ret, vid)),
                                         ELcons (EVariable (Local hdlcloc, (TAbsClosArg, hdlcid)), ELnil))))
          | None -> begin match Builtins.get_var (GEnv.get_var_name ge v) with
            | Some v -> ge, le, v
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
  | TApp (v, ts) ->
      let ge, le, Expr (t, e) = of_value ge le v in begin match t with
      | TClosed (ga, targs, tret) ->
          let SpecFrom (ga, s), SpecL (targs, bargs), Spec (tret, bret) = specialize_some ga ts targs tret in
          let t = TClosed (ga, targs, tret) in
          let closed_applied = ESpecialize (e, s, bargs, bret) in
          ge, le, Expr (t, closed_applied)
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
      | ts, Variable v -> begin
          let ts = List.filter_map (fun (k, t) -> if k = CommonTypes.PrimaryKind.Type then Some (convert_type t) else None) ts in
          let name = GEnv.get_var_name ge v in match args with
          | [arg] -> begin match Builtins.get_unop name ts with
            | None -> raise (internal_error ("Function '" ^ name ^ "' is not a (supported) builtin unary operation"))
            | Some (Unop UONegI) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TInt in
                ge, le, Expr (TInt, EUnop (UONegI, arg))
            | Some (Unop UONegF) ->
                let ge, le, arg = of_value ge le arg in let arg = target_expr arg TFloat in
                ge, le, Expr (TFloat, EUnop (UONegF, arg))
            end
          | [arg1; arg2] -> begin match Builtins.get_binop name ts with
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
      let FuncID (type a0 b0 c0 ga gc) ((ga, gc, targs, tret, tc, _) as fid, fdata : (a0, b0, c0, ga, gc) funcid * _) =
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
      let ge, le, (cls : c expr_list) = match cls with
        | Extend (vm, None) ->
            let cls = sort_name_map vm |> List.map snd in
            convert_values ge le cls tc
        | Extend (_, Some _) -> failwith "TODO: of_value Closure Extend Some"
        | _ -> failwith "TODO: of_value Closure non-Extend" in
      let e = ESpecialize (EClose (fid, bc, cls), Snil ga, bargs, bret) in
      let ge = GEnv.do_export_function ge fdata in
      ge, le, Expr (TClosed (ga, targs, tret), e)
  | Coerce (v, t) ->
      let ge, le, (Expr (vt, _) as v) = of_value ge le v in
      let t = convert_type t in
      if t = Type vt then ge, le, v
      else failwith "TODO: of_value Coerce w/ different types"

and convert_values : type a. _ -> _ -> _ -> a typ_list -> _ * _ * a expr_list =
  fun (ge : genv) (le : 'args lenv) (vs : value list) (ts : a typ_list) : (genv * 'args lenv * a expr_list) ->
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

let convert_values_unk (ge : genv) (le : 'args lenv) (vs : value list) : genv * 'args lenv * anyexpr_list =
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

let allocate_function (ge : genv) (tvars : tyvar list) (args : binder list) (b : binder) (closure : binder option)
    : genv * GEnv.funid * mfunid * LEnv.anyrealt =
  let le_args =
    let rec inner args acc = match args with
      | [] -> acc
      | hd :: tl -> let acc = LEnv.add_arg acc hd in inner tl acc
    in inner args LEnv.no_arg
  in
  let LEnv.AR le, tenv_clos = LEnv.env_of_args le_args closure in
  let loc_fid, fid, ge = GEnv.allocate_function ge le b tvars tenv_clos in
  ge, loc_fid, fid, LEnv.AR le

type 'a transformer = Transformer : ('a, 'b) finisher -> 'a transformer

let rec of_tail_computation : type args. _ -> args lenv -> _ -> genv * args lenv * anyexpr =
  fun (ge : genv) (le: args lenv) (tc : tail_computation) : (genv * args lenv * anyexpr) -> match tc with
  | Return v -> let ge, le, v = of_value ge le v in ge, le, v
  | Apply (f, args) -> begin
      match collect_toplevel_polymorphism f with
      | ts, Variable v -> begin match GEnv.find_fun ge le v with
          | le, Function ((ga, Gnil, targs, tret, TLnil, _) as fid, fdata) ->
              let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
              let ge, le, args = convert_values ge le args targs in
              let ge = GEnv.do_export_function ge fdata in
              let closed_applied = ECallClosed (ESpecialize (EClose (fid, BLnil, ELnil), s, bargs, bret), args, tret) in
              ge, le, Expr (tret, closed_applied)
          | le, Closure (loc, ((TClosed (ga, targs, tret), _) as vid)) ->
              let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
              let ge, le, args = convert_values ge le args targs in
              let closed_applied = ECallClosed (ESpecialize (EVariable (loc, vid), s, bargs, bret), args, tret) in
              ge, le, Expr (tret, closed_applied)
          | le, Contin (loc, ((TCont tc, _) as vid), (TLcons (_, TLcons (targ, _)), tret, _, hdlfid), hdlcloc, hdlcid) ->
              if ts = [] then begin
                let ge, le, ELcons (arg, ELnil) = convert_values ge le args (TLcons (targ, TLnil)) in
                ge, le, Expr (tret, ECallRawHandler (hdlfid, tc, EVariable (loc, vid), targ, arg, EVariable (hdlcloc, (TAbsClosArg, hdlcid)), tret))
              end else raise (internal_error "Invalid continuation: receives type applications")
          | le, ClosedBuiltin name ->
              let ge, le, args = convert_values_unk ge le args in
              let ge, e = Builtins.gen_impure ge GEnv.find_fbuiltin convert_type name ts args in
              ge, le, e
        end
      | ts, Closure (f, tcl, cls) ->
          let FuncID (type a0 b0 c0 ga gc) ((ga, gc, targs, tret, tc, _) as fid, fdata : (a0, b0, c0, ga, gc) funcid * _) =
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
          let ge, le, (cls : c expr_list) = match cls with
            | Extend (vm, None) ->
                let cls = sort_name_map vm |> List.map snd in
                convert_values ge le cls tc
            | Extend (_, Some _) -> failwith "TODO: of_value Closure Extend Some"
            | _ -> failwith "TODO: of_value Closure non-Extend" in
          (* TODO: try to remove this in some cases (i.e. no unboxing) *)
          let ge = GEnv.do_export_function ge fdata in
          let s, SpecL (targs, bargs2), Spec (tret, bret2) = specialize_to_apply ga ts targs tret in
          let ge, le, args = convert_values ge le args targs in
          let closed_applied = ECallClosed (ESpecialize (EClose (fid, bc, cls), s, compose_box_list bargs2 bargs, compose_box bret2 bret), args, tret) in
          ge, le, Expr (tret, closed_applied)
      | ts, Project (n, v) -> begin match v with
          | Variable v -> begin match LEnv.find_closure le v n with
              | Some (le, lst, VarID (TClosed (ga, targs, tret) as t, i)) ->
                  let s, SpecL (targs, bargs), Spec (tret, bret) = specialize_to_apply ga ts targs tret in
                  let ge, le, args = convert_values ge le args targs in
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
      ge, le, Expr (tret, EDo ((TClosed (Gnil, targs, tret), eid), args))
  | Special (Handle h) -> begin match h.ih_depth with
      | Shallow -> raise (Errors.RuntimeError "Wasm compilation of shallow handlers is currently not supported")
      | Deep bvs ->
          if bvs <> [] then raise (Errors.RuntimeError "Wasm compilation of deep parameterized handlers is currently not supported");
          (* First the body ("computation" in the first IR) *)
          let ge, Type (type b) (cret : b typ), body_id, ExprList (type c) (body_closts, body_closes : _ * c expr_list) =
            let body_le = LEnv.create_sub le false in
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
            let ge, body_id, body_locid =
              GEnv.new_cont_start ge body_le b (Option.map (fun (_, v) -> TypeList body_closts, v) oabsconc) in
            let ge = GEnv.do_export_function ge body_locid in
            ge, Type cret, body_id, ExprList (body_closts, body_closes) in
          let body_id : (unit, b, c, unit, unit) funcid = (Gnil, Gnil, TLnil, cret, body_closts, body_id) in
          (* Now to the handler function (the rest of the tail_computation) *)
          let ge, Type (type d) (tret : d typ), handler_id, ExprList (type e) (handler_closts, handler_closes : _ * e expr_list) =
            let hid, handler_id, ge = GEnv.allocate_fhandler ge in
            let handle_le = LEnv.create_sub le true in
            let ge, handle_le, Transformer (type d) (onret : (b, d) finisher) =
              of_finisher ge (LEnv.of_sub handle_le) h.ih_return cret in
            let handle_le = LEnv.to_sub handle_le in
            let tret = match onret with FId t -> (t : d typ) | FMap (_, t, _) -> t in
            let ge, hcllocid, hcltid = GEnv.new_generic_list ge in
            let handle_le, contid = LEnv.add_cont handle_le (Type cret) handler_id (Type tret) hcltid in
            let contid : b continuation varid = TCont cret, contid in
            let ge, handle_le, ondo =
              let do_case (ge : genv) (handle_le : LEnv.subt) (ename : string) ((args, k, p) : effect_case) : genv * LEnv.subt * (b, d) handler =
                let handle_le, VarIDList (eargs, vargs) = LEnv.set_handler_args handle_le args in
                let ge, eid = GEnv.add_effect ge ename in
                let TypeList rarg = _convert_type
                    (fun _ -> raise (internal_error "Expected a function type, got another type"))
                    (fun g args _ _ ->
                        if g = [] then convert_type_list args
                        else raise (internal_error "Invalid handler case: contains type variables"))
                    (fun _ _ _ -> raise (internal_error "Expected a function type, got a row type"))
                    (Var.type_of_binder k) in
                let Type rarg = match rarg with
                  | TLnil -> raise (internal_error "Unexpected handler type: no argument")
                  | TLcons (t, TLnil) -> Type t
                  | TLcons (_, TLcons _) -> raise (internal_error "Unexpected handler type: too many argument") in
                let handle_le = LEnv.set_continuation handle_le k (Type rarg) in
                let ge, handle_le, Block (t, b) = of_computation ge (LEnv.of_sub handle_le) p in
                let handle_le = LEnv.to_sub handle_le in
                let Type.Equal = assert_eq_typ t tret "Expected the same type in the return branch as in all handler branches" in
                ge, handle_le, Handler ((TClosed (Gnil, eargs, rarg), eid), contid, vargs, b)
              in let do_case ename (ec : effect_case) (ge, handle_le, acc) =
                let ge, handle_le, hd = do_case ge handle_le ename ec in
                ge, handle_le, hd :: acc
              in Utility.StringMap.fold do_case h.ih_cases (ge, handle_le, []) in
            let handle_le, oabsconc, ExprList (type e) (hclosts, hcloses : _ * e expr_list) = LEnv.get_closure handle_le in
            let ge = GEnv.set_generic_list ge hcllocid (TypeList hclosts) in
            let ge = GEnv.assign_fhandler ge hid handler_id handle_le oabsconc onret ondo in
            ge, Type tret, handler_id, ExprList (hclosts, hcloses) in
          let handler_id : (b continuation * (c closure_content * unit), d, e, unit, unit) funcid =
            (Gnil, Gnil, TLcons (TCont cret, TLcons (TClosArg body_closts, TLnil)), tret, handler_closts, handler_id) in
          (* All done! *)
          ge, le, Expr (tret, EDeepHandle (body_id, body_closes, handler_id, handler_closes))
    end
  | Special (Wrong t) -> let Type t = convert_type t in ge, le, Expr (t, EUnreachable t)
  | Special _ -> failwith "TODO of_tail_computation Special"
  | Case (v, m, d) ->
      let ge, le, Expr (vt, v) = of_value ge le v in
      let Type.Equal = assert_eq_typ vt TVariant "Unexpected non-variant type in case computation" in
      let ge, le, m = Utility.StringMap.fold (fun tag (b, c) (ge, le, acc) ->
        let ge, tagid = GEnv.find_tag ge tag in
        let bt = convert_type (Var.type_of_binder b) in
        let le, argid = LEnv.add_var le b bt in
        let ge, le, blk = of_computation ge le c in
        ge, le, (tagid, bt, argid, blk) :: acc) m (ge, le, []) in
      let ge, le, d = match d with
        | None -> ge, le, None
        | Some (b, c) ->
            let bt = convert_type (Var.type_of_binder b) in
            let le, argid = LEnv.add_var le b bt in
            let ge, le, blk = of_computation ge le c in
            ge, le, Some (argid, blk) in
      let Type (type r) (t : r typ) = match m with
        | (_, _, _, Block (t, _)) :: _ -> Type t
        | [] -> match d with
            | Some (_, Block (t, _)) -> Type t
            | None -> raise (internal_error "Empty case computation") in
      let m = List.map (fun (tid, bindt, bv, Block (bt, bb)) ->
          let Type.Equal = assert_eq_typ t bt "Unexpected case return type" in tid, bindt, bv, (bb : r block)) m in
      let d = Option.map (fun (bv, Block (bt, bb)) -> let Type.Equal = assert_eq_typ t bt "Unexpected case return type" in bv, (bb : r block)) d in
      ge, le, Expr (t, ECase (v, t, m, d))
  | If (b, t, f) ->
      let ge, le, Expr (tb, eb) = of_value ge le b in
      let Type.Equal = assert_eq_typ tb TBool "Expected a boolean expression" in
      let ge, le, Block (tt, bt) = of_computation ge le t in
      let ge, le, Block (tf, bf) = of_computation ge le f in
      let Type.Equal = assert_eq_typ tt tf "Expected the same type in both branches" in
      ge, le, Expr (tt, ECond (tt, eb, bt, bf))
and of_computation : type args. _ -> args lenv -> _ -> _ * args lenv * _ =
  fun (ge : genv) (le : args lenv) ((bs, tc) : computation) : (genv * args lenv * anyblock) ->
  let finish_computation : type args. _ -> _ -> _ -> _ -> args LEnv.realt -> _ = fun ge fd loc_fid fid (new_le : args LEnv.realt) ->
    let new_le = LEnv.of_real new_le in
    let ge, new_le, Block (tret, b) = of_computation ge new_le fd.fn_body in
    let new_le = LEnv.to_real new_le in
    let ge, new_le, b =
      match fd.fn_closure with
      | None -> ge, new_le, b
      | Some _ ->
          let (ass, e) = b in
          let new_le, absid, concid, TypeList ctyp = LEnv.add_closure new_le in
          ge, new_le, (Assign (Local StorVariable, (TClosArg ctyp, concid), EConvertClosure (absid, TClosArg ctyp)) :: ass, e) in
    let export_name =
      if Var.Scope.is_real_global (Var.scope_of_binder fd.fn_binder)
      then match fd.fn_closure with None -> Some (Var.name_of_binder fd.fn_binder) | Some _ -> None
      else None in
    let f = LEnv.compile new_le fid export_name tret b in
    let ge = GEnv.assign_function ge loc_fid f in
    ge in
  let rec inner (ge : genv) (le: args lenv) (bs : binding list) (acc : assign list) = match bs with
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
    fun (type a) (ge : genv) (le : 'args lenv) ((bind, comp) : binder * computation) (t : a typ) : (genv * 'args lenv * a transformer) ->
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

(* MAIN FUNCTION *)

let module_of_ir (c : Ir.program) (map : string Env.Int.t) (import_wizard : bool) : anymodule =
  let ge = GEnv.empty map (find_global_binders c) import_wizard in
  let ge, le, blk = of_computation ge (LEnv.of_real LEnv.toplevel) c in
  GEnv.compile ge (LEnv.to_real le) blk
