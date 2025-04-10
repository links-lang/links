type tagid = Wasmir.tagid
type mvarid = Wasmir.mvarid
type mfunid = Wasmir.mfunid
type meffid = Wasmir.meffid
module FunIDMap = Wasmir.FunIDMap
module EffectIDMap = Wasmir.EffectIDMap

type llist = Wasmir.llist
type variant = Wasmir.variant
type abs_closure_content = Wasmir.abs_closure_content
type 'a closure_content = 'a Wasmir.closure_content
type 'a continuation = 'a Wasmir.continuation

type 'a typ =
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
and 'a typ_list =
  | TLnil : unit typ_list
  | TLcons : 'a typ * 'b typ_list -> ('a * 'b) typ_list

type anytyp = Type : 'a typ -> anytyp
type anytyp_list = TypeList : 'a typ_list -> anytyp_list

let [@tail_mod_cons] rec convert_typ : type a. a Wasmir.typ -> a typ = fun (t : a Wasmir.typ) : a typ -> match t with
  | Wasmir.TInt -> TInt
  | Wasmir.TBool -> TBool
  | Wasmir.TFloat -> TFloat
  | Wasmir.TString -> TString
  | Wasmir.TClosed (_, targs, tret) -> TClosed ((convert_typ_list[@tailcall]) targs, convert_typ tret)
  | Wasmir.TAbsClosArg -> TAbsClosArg
  | Wasmir.TClosArg ts -> TClosArg (convert_typ_list ts)
  | Wasmir.TCont tret -> TCont (convert_typ tret)
  | Wasmir.TTuple ts -> TTuple (convert_named_typ_list ts)
  | Wasmir.TVariant -> TVariant
  | Wasmir.TList t -> TList (convert_typ t)
  | Wasmir.TVar _ -> TVar
and [@tail_mod_cons] convert_typ_list : type a. a Wasmir.typ_list -> a typ_list = fun (t : a Wasmir.typ_list) : a typ_list -> match t with
  | Wasmir.TLnil -> TLnil
  | Wasmir.TLcons (hd, tl) -> TLcons (convert_typ hd, (convert_typ_list[@tailcall]) tl)
and [@tail_mod_cons] convert_named_typ_list : type a. a Wasmir.named_typ_list -> a typ_list =
  fun (t : a Wasmir.named_typ_list) : a typ_list -> match t with
  | Wasmir.NTLnil -> TLnil
  | Wasmir.NTLcons (_, hd, tl) -> TLcons (convert_typ hd, (convert_named_typ_list[@tailcall]) tl)

let convert_anytyp (Wasmir.Type t : Wasmir.anytyp) : anytyp = Type (convert_typ t)
let convert_anytyp_list (Wasmir.TypeList t : Wasmir.anytyp_list) : anytyp_list = TypeList (convert_typ_list t)
let rec compare_typ (Type t1 : anytyp) (Type t2 : anytyp) = match t1, t2 with
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
  | TClosed (args1, ret1), TClosed (args2, ret2) ->
      let c = compare_typ (Type ret1) (Type ret2) in if c <> 0 then c else
      compare_typ_list (TypeList args1) (TypeList args2)
  | TClosed _, _ -> ~-1
  | _, TClosed _ -> 1
  | TAbsClosArg, TAbsClosArg -> 0
  | TAbsClosArg, _ -> ~-1
  | _, TAbsClosArg -> 1
  | TClosArg t1, TClosArg t2 -> compare_typ_list (TypeList t1) (TypeList t2)
  | TClosArg _, _ -> ~-1
  | _, TClosArg _ -> 1
  | TCont t1, TCont t2 -> compare_typ (Type t1) (Type t2)
  | TCont _, _ -> ~-1
  | _, TCont _ -> 1
  | TTuple tl1, TTuple tl2 -> compare_typ_list (TypeList tl1) (TypeList tl2)
  | TTuple _, _ -> ~-1
  | _, TTuple _ -> 1
  | TVariant, TVariant -> 0
  | TVariant, _ -> ~-1
  | _, TVariant -> 1
  | TList t1, TList t2 -> compare_typ (Type t1) (Type t2)
  | TList _, _ -> ~-1
  | _, TList _ -> 1
  | TVar, TVar -> 0
and compare_typ_list (TypeList tl1 : anytyp_list) (TypeList tl2 : anytyp_list) = match tl1, tl2 with
  | TLnil, TLnil -> 0
  | TLnil, TLcons _ -> ~-1 | TLcons _, TLnil -> 1
  | TLcons (hd1, tl1), TLcons (hd2, tl2) ->
      let chd = compare_typ (Type hd1) (Type hd2) in if chd = 0 then compare_typ_list (TypeList tl1) (TypeList tl2) else chd

module TypeMap = Utility.Map.Make(struct
  type t = anytyp
  let pp fmt (_ : t) = Format.fprintf fmt "<some type>"
  let show (_ : t) = "<some type>"
  let compare = compare_typ
end)

type ('a, 'b) extract_typ = 'a typ_list * int * 'b typ

let convert_extract_typ (Wasmir.TTuple s, n, t, _ : ('a, 'b) Wasmir.extract_typ) : ('a, 'b) extract_typ =
  convert_named_typ_list s, n, convert_typ t

type ('a, 'r) unop =
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

type local_storage = Wasmir.local_storage =
  | StorVariable
  | StorClosure
type locality = Wasmir.locality =
  | Global
  | Local of local_storage
type 'a varid = 'a typ * mvarid
type ('a, 'b, 'c) funcid = 'a typ_list * 'b typ * 'c typ_list * mfunid
type ('a, 'b) effectid = 'a typ_list * 'b typ * meffid

let convert_varid (v : 'a Wasmir.varid) : 'a varid =
  let t, v = (v : 'a Wasmir.varid :> _ * _) in
  (convert_typ t, v)
let convert_funcid (f : ('a, 'b, 'c, 'ga, 'gc) Wasmir.funcid) : ('a, 'b, 'c) funcid =
  let _, _, targs, tret, tc, i = (f : ('a, 'b, 'c, 'ga, 'gc) Wasmir.funcid :> _ * _ * _ * _ * _ * _) in
  (convert_typ_list targs, convert_typ tret, convert_typ_list tc, i)
let convert_effectid (e : ('a, 'b) Wasmir.effectid) : ('a, 'b) effectid =
  let Wasmir.(TClosed (Gnil, targs, tret)), e = (e : ('a, 'b) Wasmir.effectid :> _ * _) in
  (convert_typ_list targs, convert_typ tret, e)

type 'a varid_list =
  | VLnil
  | VLcons : 'a varid * 'b varid_list -> ('a * 'b) varid_list

let [@tail_mod_cons] rec convert_varid_list : type a. a Wasmir.varid_list -> a varid_list = function
  | Wasmir.VLnil -> VLnil
  | Wasmir.VLcons (hd, tl) -> VLcons (convert_varid hd, convert_varid_list tl)

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
type (_, _) conv_box_list =
  | CBLnone : ('a, 'a) box_list -> ('a, 'a) conv_box_list
  | CBLcons : ('a, 'c) box * ('b, 'd) box_list -> ('a * 'b, 'c * 'd) conv_box_list

let [@tail_mod_cons] rec convert_box : type a b. (a, b) Wasmir.box -> (a, b) box = fun (b : (a, b) Wasmir.box) : (a, b) box -> match b with
  | Wasmir.BNone (_, _) -> BNone
  | Wasmir.BClosed (_, bargs, bret) ->
      let targs = Wasmir.src_of_box_list bargs in
      let tret = Wasmir.src_of_box bret in
      BClosed (TClosed (convert_typ_list targs, convert_typ tret), convert_box_list bargs, (convert_box[@tailcall]) bret)
  | Wasmir.BCont bret -> BCont (convert_box bret)
  | Wasmir.BTuple bs -> BTuple (convert_box_named_list bs)
  | Wasmir.BBox (src, _) -> BBox (convert_typ src)
and convert_box_list : type a b. (a, b) Wasmir.box_list -> (a, b) box_list =
  fun (type a b) (b : (a, b) Wasmir.box_list) : (a, b) box_list ->
  let rec inner : type a b. (a, b) Wasmir.box_list -> (a, b) conv_box_list =
    fun (type a b) (b : (a, b) Wasmir.box_list) : (a, b) conv_box_list -> match b with
    | Wasmir.BLnil -> CBLnone BLnil
    | Wasmir.BLcons (hd, tl) -> match convert_box hd, inner tl with
        | BNone, CBLnone tl -> CBLnone (BLcons (BNone, tl))
        | hd, CBLnone tl -> CBLcons (hd, tl)
        | hd, CBLcons (hd2, tl) -> CBLcons (hd, BLcons (hd2, tl))
  in match inner b with
  | CBLnone _ -> BLnone
  | CBLcons (hd, tl) -> BLcons (hd, tl)
and convert_box_named_list : type a b. (a, b) Wasmir.box_named_list -> (a, b) box_list =
  fun (type a b) (b : (a, b) Wasmir.box_named_list) : (a, b) box_list ->
  let rec inner : type a b. (a, b) Wasmir.box_named_list -> (a, b) conv_box_list =
    fun (type a b) (b : (a, b) Wasmir.box_named_list) : (a, b) conv_box_list -> match b with
    | Wasmir.BNLnil -> CBLnone BLnil
    | Wasmir.BNLcons (_, hd, tl) -> match convert_box hd, inner tl with
        | BNone, CBLnone tl -> CBLnone (BLcons (BNone, tl))
        | hd, CBLnone tl -> CBLcons (hd, tl)
        | hd, CBLcons (hd2, tl) -> CBLcons (hd, BLcons (hd2, tl))
  in match inner b with
  | CBLnone _ -> BLnone
  | CBLcons (hd, tl) -> BLcons (hd, tl)

let [@tail_mod_cons] rec dst_of_box : type a b. a typ -> (a, b) box -> b typ =
  fun src box -> match src, box with
  | _, BNone -> src
  | TClosed (targs, tret), BClosed (_, bargs, bret) -> TClosed ((dst_of_box_list[@tailcall]) targs bargs, dst_of_box tret bret)
  | TCont tret, BCont bret -> TCont (dst_of_box tret bret)
  | TTuple ts, BTuple bs -> TTuple (dst_of_box_list ts bs)
  | _, BBox _ -> TVar
and [@tail_mod_cons] dst_of_box_list : type a b. a typ_list -> (a, b) box_list -> b typ_list =
  fun (t : a typ_list) (bs : (a, b) box_list) : b typ_list -> match t, bs with
  | _, BLnone -> t
  | _, BLnil -> t
  | TLcons (thd, ttl), BLcons (hd, tl) -> TLcons (dst_of_box thd hd, (dst_of_box_list[@tailcall]) ttl tl)

type (_, _) finisher =
  | FId : 'a typ -> ('a, 'a) finisher
  | FMap : 'a varid * 'b typ * 'b block -> ('a, 'b) finisher
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
  | ETuple : 'a typ_list * 'a expr_list -> 'a list expr
  | EExtract : 'a list expr * ('a, 'b) extract_typ -> 'b expr
  | EVariant : tagid * 'a typ * 'a expr -> variant expr
  | ECase : variant expr * 'a typ * (tagid * anytyp * mvarid * 'a block) list * (mvarid * 'a block) option -> 'a expr
  | EListNil : 'a typ -> llist expr
  | EListHd : llist expr * 'a typ -> 'a expr
  | EListTl : 'a typ * llist expr -> llist expr
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
      (* TODO: check if it is possible (and how) to merge ECallRawHandler and ECont *)
and (_, _) handler =
  | Handler : ('a, 'b) effectid * 'd continuation varid * 'a varid_list * 'c block -> ('d, 'c) handler
and _ expr_list =
  | ELnil : unit expr_list
  | ELcons : 'a expr * 'b expr_list -> ('a * 'b) expr_list

type tenv = Wasmir.anytyp_list Wasmir.MTypMap.t

let rec convert_finisher : type a b. _ -> (a, b) Wasmir.finisher -> (a, b) finisher =
  fun (tmap : tenv) (f : (a, b) Wasmir.finisher) ->
  match f with
  | Wasmir.FId t -> FId (convert_typ t)
  | Wasmir.FMap (v, t, b) -> FMap (convert_varid v, convert_typ t, convert_block tmap b)
and [@tail_mod_cons] convert_block : type a. _ -> a Wasmir.block -> a block =
  fun (tmap : tenv) (ass, e : a Wasmir.block) ->
  let convert_assign (Wasmir.Assign (loc, v, e) : Wasmir.assign) : assign =
    Assign (loc, convert_varid v, convert_expr tmap e)
  in (List.map convert_assign ass, convert_expr tmap e)
and [@tail_mod_cons] convert_expr : type a. _ -> a Wasmir.expr -> a expr =
  fun (tmap : tenv) (e : a Wasmir.expr) -> match e with
  | Wasmir.EUnreachable t -> EUnreachable (convert_typ t)
  | Wasmir.EConvertClosure (v, t) -> EConvertClosure (v, convert_typ t)
  | Wasmir.EIgnore (t, e) -> EIgnore (convert_typ t, (convert_expr[@tailcall]) tmap e)
  | Wasmir.EConstInt c -> EConstInt c
  | Wasmir.EConstBool c -> EConstBool c
  | Wasmir.EConstFloat c -> EConstFloat c
  | Wasmir.EConstString c -> EConstString c
  | Wasmir.EUnop (op, arg) -> begin match op with
      | Wasmir.UONegI -> EUnop (UONegI, convert_expr tmap arg)
      | Wasmir.UONegF -> EUnop (UONegF, convert_expr tmap arg)
    end
  | Wasmir.EBinop (op, arg1, arg2) -> begin match op with
      | Wasmir.BOAddI -> EBinop (BOAddI, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOAddF -> EBinop (BOAddF, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOSubI -> EBinop (BOSubI, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOSubF -> EBinop (BOSubF, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOMulI -> EBinop (BOMulI, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOMulF -> EBinop (BOMulF, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BODivI -> EBinop (BODivI, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BODivF -> EBinop (BODivF, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BORemI -> EBinop (BORemI, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOEq t -> EBinop (BOEq (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BONe t -> EBinop (BONe (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOLe t -> EBinop (BOLe (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOLt t -> EBinop (BOLt (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOGe t -> EBinop (BOGe (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOGt t -> EBinop (BOGt (convert_typ t), (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOConcat -> EBinop (BOConcat, (convert_expr[@tailcall]) tmap arg1, convert_expr tmap arg2)
      | Wasmir.BOCons t -> EBinop (BOCons (convert_typ t), convert_expr tmap arg1, (convert_expr[@tailcall]) tmap arg2)
      | Wasmir.BOConcatList t -> EBinop (BOConcatList (convert_typ t), convert_expr tmap arg1, (convert_expr[@tailcall]) tmap arg2)
    end
  | Wasmir.EVariable (loc, v) -> EVariable (loc, convert_varid v)
  | Wasmir.ETuple (ts, es) -> ETuple (convert_named_typ_list ts, (convert_expr_list[@tailcall]) tmap es)
  | Wasmir.EExtract (e, f) -> EExtract (convert_expr tmap e, convert_extract_typ f)
  | Wasmir.EVariant (tag, t, e) -> EVariant (tag, convert_typ t, (convert_expr[@tailcall]) tmap e)
  | Wasmir.ECase (e, t, l, d) ->
      let convert_l (l : (tagid * Wasmir.anytyp * mvarid * _ Wasmir.block) list) =
        let convert (i, t, v, b) = i, convert_anytyp t, v, convert_block tmap b
        in List.map convert l
      in let convert_d (o : (mvarid * _ Wasmir.block) option) =
        let convert (i, b) = i, convert_block tmap b
        in Option.map convert o
      in ECase ((convert_expr[@tailcall]) tmap e, convert_typ t, convert_l l, convert_d d)
  | Wasmir.EListNil t -> EListNil (convert_typ t)
  | Wasmir.EListHd (e, t) -> EListHd ((convert_expr[@tailcall]) tmap e, convert_typ t)
  | Wasmir.EListTl (t, e) -> EListTl (convert_typ t, (convert_expr[@tailcall]) tmap e)
  | Wasmir.EClose (f, b, cl) -> EClose (convert_funcid f, convert_box_list b, (convert_expr_list[@tailcall]) tmap cl)
  | Wasmir.(ESpecialize (ESpecialize (e, s1, bargs1, bret1), s2, bargs2, bret2)) ->
      let open Wasmir in
      let s =
        let [@tail_mod_cons] rec inner : type a b c. (b, c) specialization -> (a, b) specialization -> (a, c) specialization
          = fun (s1 : (b, c) specialization) (s2 : (a, b) specialization) : (a, c) specialization -> match s1 with
          | Snil _ -> s2
          | Scons (t1, v1, s1) -> Scons (t1, v1, inner s1 s2)
        in inner s1 s2 in
      let bargs, bret = compose_box_list bargs2 bargs1, compose_box bret2 bret1 in
      convert_expr tmap (ESpecialize (e, s, bargs, bret))
  | Wasmir.ESpecialize (e, _, bargs, bret) ->
      ESpecialize (
        (convert_expr[@tailcall]) tmap e,
        TClosed (convert_typ_list (Wasmir.src_of_box_list bargs), convert_typ (Wasmir.src_of_box bret)),
        convert_box_list bargs,
        convert_box bret)
  | Wasmir.ECallRawHandler (f, tc, c, ta, a, cl, tr) ->
      ECallRawHandler (f, TCont (convert_typ tc), convert_expr tmap c, convert_typ ta, (convert_expr[@tailcall]) tmap a, convert_expr tmap cl, convert_typ tr)
  | Wasmir.ECallClosed (f, e, _) -> ECallClosed (convert_expr tmap f, (convert_expr_list[@tailcall]) tmap e)
  | Wasmir.ECond (r, c, t, f) ->
      ECond (
        (convert_expr[@tailcall false]) tmap c, (convert_typ[@tailcall false]) r,
        convert_block tmap t, (convert_block[@tailcall false]) tmap f) (* For some reason this one cannot be @tailcall-ed *)
  | Wasmir.EDo (e, v) -> EDo (convert_effectid e, convert_expr_list tmap v)
  | Wasmir.EShallowHandle (f, e, d, h) ->
      EShallowHandle (convert_funcid f, convert_expr_list tmap e, convert_finisher tmap d, List.map (convert_handler tmap) h)
  | Wasmir.EDeepHandle (c, cl, h, a) ->
      EDeepHandle (convert_funcid c, convert_expr_list tmap cl, convert_funcid h, (convert_expr_list[@tailcall]) tmap a)
  | Wasmir.ECont (loc, c, v, Wasmir.(TLcons (cret, TLcons (targ, TLnil)), tret, i, f), l, cl) ->
      let TypeList fcltyp = convert_anytyp_list (Wasmir.MTypMap.find i tmap) in
      ECont (
        loc, convert_varid c,
        (convert_expr[@tailcall]) tmap v,
        (TLcons (convert_typ cret, TLcons (convert_typ targ, TLnil)), convert_typ tret, fcltyp, f),
        (l, (TClosArg fcltyp, cl)))
and convert_handler : type a b. _ -> (a, b) Wasmir.handler -> (a, b) handler =
  fun (tmap : tenv) (h : (a, b) Wasmir.handler) -> match h with
  | Wasmir.Handler (e, c, v, b) -> Handler (convert_effectid e, convert_varid c, convert_varid_list v, convert_block tmap b)
and [@tail_mod_cons] convert_expr_list : type a. _ -> a Wasmir.expr_list -> a expr_list =
  fun (tmap : tenv) (e : a Wasmir.expr_list) -> match e with
  | Wasmir.ELnil -> ELnil
  | Wasmir.ELcons (hd, tl) -> ELcons (convert_expr tmap hd, (convert_expr_list[@tailcall]) tmap tl)

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
type ('a, 'b) fhandler = {
  fh_contarg : 'a continuation varid * mvarid;
  fh_closure : (mvarid * (anytyp_list * mvarid)) option;
  fh_locals  : anytyp list;
  fh_finisher: ('a, 'b) finisher;
  fh_handlers: ('a, 'b) handler list;
  fh_id      : mfunid;
}
type fbuiltin = Wasmir.fbuiltin =
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
  mod_neffs       : int32;
  mod_effs        : anytyp_list EffectIDMap.t;
  mod_nglobals    : int32;
  mod_global_vars : (mvarid * anytyp * string) list;
  mod_locals      : anytyp list;
  mod_main        : 'a typ;
  mod_block       : 'a block;
}

let convert_func' (tmap : tenv) (f : ('a, 'b) Wasmir.func') : ('a, 'b) func' = {
  fun_id                = f.Wasmir.fun_id;
  fun_export_data       = f.Wasmir.fun_export_data;
  fun_converted_closure = Option.map (fun (t, v) -> convert_anytyp_list t, v) f.Wasmir.fun_converted_closure;
  fun_args              = convert_typ_list f.Wasmir.fun_args;
  fun_locals            = List.map convert_anytyp f.Wasmir.fun_locals;
  fun_ret               = convert_typ f.Wasmir.fun_ret;
  fun_block             = convert_block tmap f.Wasmir.fun_block;
}
let convert_fstart (tmap : tenv) (f : 'b Wasmir.fstart) : 'b fstart = {
  fst_id                = f.Wasmir.fst_id;
  fst_converted_closure = Option.map (fun (t, v) -> convert_anytyp_list t, v) f.Wasmir.fst_converted_closure;
  fst_locals            = List.map convert_anytyp f.Wasmir.fst_locals;
  fst_ret               = convert_typ f.Wasmir.fst_ret;
  fst_block             = convert_block tmap f.Wasmir.fst_block;
}
let convert_fhandler (tmap : tenv) (f : ('a, 'b) Wasmir.fhandler) : ('a, 'b) fhandler =
  let convert_contarg (c, a : 'a Wasmir.continuation Wasmir.varid * mvarid) =
    let t, c = (c : _ Wasmir.varid :> _ * _) in
    (convert_typ t, c), a
  in
  let convert_closure (o : (mvarid * (Wasmir.anytyp_list * Wasmir.mvarid)) option) =
    Option.map (fun (a, (l, c)) -> a, (convert_anytyp_list l, c)) o
  in {
  fh_contarg  = convert_contarg f.Wasmir.fh_contarg;
  fh_closure  = convert_closure f.Wasmir.fh_closure;
  fh_locals   = List.map convert_anytyp f.Wasmir.fh_locals;
  fh_finisher = convert_finisher tmap f.Wasmir.fh_finisher;
  fh_handlers = List.map (convert_handler tmap) f.Wasmir.fh_handlers;
  fh_id       = f.Wasmir.fh_id;
}
let convert_func (tmap : tenv) (f : Wasmir.func) : func = match f with
  | Wasmir.FFunction f -> FFunction (convert_func' tmap f)
  | Wasmir.FContinuationStart f -> FContinuationStart (convert_fstart tmap f)
  | Wasmir.FHandler f -> FHandler (convert_fhandler tmap f)
  | Wasmir.FBuiltin fb -> FBuiltin fb
let convert_module (m : 'a Wasmir.modu) : 'a modu =
  let convert_global (v, t, n : mvarid * Wasmir.anytyp * string) =
    v, convert_anytyp t, n in
  let tmap = m.Wasmir.mod_typs in
  {
    mod_imports = m.Wasmir.mod_imports;
    mod_nfuns = m.Wasmir.mod_nfuns;
    mod_funs = List.map (convert_func tmap) m.Wasmir.mod_funs;
    mod_needs_export = FunIDMap.map (fun (targs, tret) -> Option.map convert_anytyp_list targs, convert_anytyp tret) m.Wasmir.mod_needs_export;
    mod_neffs = m.Wasmir.mod_neffs;
    mod_effs = EffectIDMap.map convert_anytyp_list m.Wasmir.mod_effs;
    mod_nglobals = m.Wasmir.mod_nglobals;
    mod_global_vars = List.map convert_global m.Wasmir.mod_global_vars;
    mod_locals = List.map convert_anytyp m.Wasmir.mod_locals;
    mod_main = convert_typ m.Wasmir.mod_main;
    mod_block = convert_block tmap m.Wasmir.mod_block;
  }

let module_of_ir (m : 'a Wasmir.modu) : 'a modu =
  convert_module m
