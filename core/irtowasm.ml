let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

open Wasmir

(* TODO: make it so that TTuple TLnil becomes nothing *)
module TMap : sig
  type t
  val of_module : modu -> t
  val init_func_type : int32
  
  val ovaltype_of_type : t -> anytyp -> t * Wasm.Type.val_type option
  val type_of_type : t -> anytyp -> t * int32
  val type_of_typeid : t -> mtypid -> t * int32
  val type_of_global : t -> anytyp -> t * Wasm.Type.val_type * Wasm.Instruction.t list
  val type_of_closure_content : t -> anytyp_list -> t * int32
  val type_of_closure_struct : t -> ('a, 'b) functyp typ -> int32 -> t * int32
  val type_of_handler : t -> 'a continuation typ -> 'b typ -> t * int32 * int32
  val type_of_handler_block : t -> int32 -> 'a typ_list -> t * int32
  val type_of_handler_finish : t -> int32 -> 'a typ_list -> t * int32
  
  val type_of_locals : t -> anytyp_list -> t * Wasm.Type.val_type list * int32
  
  val to_wasm : t -> Wasm.Type.rec_type list
end = struct
  type t = {
    menv: anytyp MTypMap.t;
    cenv: int32 TypeMap.t;
    nrefs: int32;
    reftyps: Wasm.Type.rec_type list;
  }
  let of_module (m : modu) : t = {
    menv = m.mod_typs;
    cenv = TypeMap.empty;
    nrefs = 2l;
    reftyps = Wasm.Type.[RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))]; RecT [SubT (Final, [], DefStructT (StructT []))]];
  }
  let default_closure : int32 = 0l
  let init_func_type : int32 = 1l
  
  let add_rectyp (env : t) (t : Wasm.Type.rec_type) : t * int32 = match List.find_index ((=) t) env.reftyps with
    | Some tidx -> env, Int32.sub env.nrefs (Int32.of_int (tidx + 1))
    | None ->
        let tidx = env.nrefs in
        let nrefs = Int32.succ tidx in
        { env with nrefs; reftyps = t :: env.reftyps }, tidx
  
  let rec type_to_val (env : t ref) (Type t : anytyp) : Wasm.Type.val_type =
    let open Wasm.Type in
    match t with
    | TTuple TLnil -> NumT I32T
    | TInt -> NumT I64T
    | TBool -> NumT I32T
    | TFloat -> NumT F64T
    | TFunc _ -> failwith "TODO: TMap.type_to_val TFunc"
    | TClosedFun _ -> raise (internal_error "Unexpected value of IR type ClosedFun")
    | TClosedVar _ -> RefT (NoNull, VarHT (StatX (type_of_type' env (Type t))))
    | TAbsClosArg -> RefT (Null, StructHT)
    | TClosArg _ -> raise (internal_error "Unexpected value of IR type ClosArg")
    | TCont _ -> RefT (NoNull, VarHT (StatX (type_of_type' env (Type t))))
    | TTuple _ -> failwith "TODO: TMap.type_to_val TTuple"
  and [@tail_mod_cons] type_to_val_list (env : t ref) (TypeList tl : anytyp_list) : Wasm.Type.val_type list =
    let open Wasm.Type in
    match tl with
    | TLnil -> []
    | TLcons (TTuple TLnil, tl) -> NumT I32T :: type_to_val_list env (TypeList tl)
    | TLcons (TInt, tl) -> NumT I64T :: type_to_val_list env (TypeList tl)
    | TLcons (TBool, tl) -> NumT I32T :: type_to_val_list env (TypeList tl)
    | TLcons (TFloat, tl) -> NumT F64T :: type_to_val_list env (TypeList tl)
    | TLcons (TFunc _, _) -> failwith "TODO: TMap.type_to_val_list TFunc"
    | TLcons (TClosedFun _, _) -> raise (internal_error "Unexpected value in list of IR type ClosedFun")
    | TLcons (TClosedVar _ as hd, tl) -> RefT (NoNull, VarHT (StatX (type_of_type' env (Type hd)))) :: type_to_val_list env (TypeList tl)
    | TLcons (TAbsClosArg, tl) -> RefT (Null, StructHT) :: type_to_val_list env (TypeList tl)
    | TLcons (TClosArg _, _) -> raise (internal_error "Unexpected value in list of IR type ClosArg")
    | TLcons (TCont _, _) -> failwith "TODO: TMap.type_to_val_list TCont"
    | TLcons (TTuple _, _) -> failwith "TODO: TMap.type_to_val_list TTuple"
  and type_of_type' (env : t ref) (t : anytyp) : int32 = match TypeMap.find_opt t !env.cenv with
    | Some i -> i
    | None ->
        let newt =
          let open Wasm.Type in
          let convert_rec_type (Type t : anytyp) : rec_type = match t with
            | TTuple TLnil -> raise (internal_error "Cannot convert IR type TTuple TLnil to Wasm rec type")
            | TInt -> raise (internal_error "Cannot convert IR type TInt to Wasm rec type")
            | TBool -> raise (internal_error "Cannot convert IR type TBool to Wasm rec type")
            | TFloat -> raise (internal_error "Cannot convert IR type TFloat to Wasm rec type")
            | TFunc (args, ret) ->
                let args = type_to_val_list env (TypeList args) in
                let ret = type_to_val env (Type ret) in
                RecT [SubT (Final, [], DefFuncT (FuncT (args @ [RefT (Null, StructHT)], [ret])))]
            | TClosedFun (args, ret) ->
                let args = type_to_val_list env (TypeList args) in
                let ret = type_to_val_list env (TypeList (TLcons (ret, TLnil))) in
                RecT [SubT (Final, [], DefFuncT (FuncT (args, ret)))]
            | TClosedVar (args, ret) ->
                let ftid = type_of_type' env (Type (TFunc (args, ret))) in
                RecT [SubT (Final, [], DefStructT (StructT [
                  FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftid))));
                  FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
                ]))]
            | TAbsClosArg -> failwith "TODO: convert_rec_type TAbsClosArg"
            | TClosArg ts ->
                let ts = type_to_val_list env (TypeList ts) in
                RecT [SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) ts)))]
            | TCont t ->
                let fid = type_of_type' env (Type (TFunc (TLnil, t))) in
                RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX fid))))]
            | TTuple _ -> failwith "TODO: convert_rec_type TTuple"
          in convert_rec_type t
        in let env', ret = add_rectyp !env newt in
        env := { env' with cenv = TypeMap.add t ret env'.cenv }; ret
  
  let ovaltype_of_type (env : t) (t : anytyp) : t * Wasm.Type.val_type option =
    let env = ref env in
    let ret = type_to_val env t in
    !env, Some ret
  let type_of_type (env : t) (t : anytyp) : t * int32 = let env = ref env in let ret = type_of_type' env t in !env, ret
  
  let type_to_field (env : t ref) (t : anytyp) : Wasm.Type.field_type = Wasm.Type.(FieldT (Cons, ValStorageT (type_to_val env t)))
  
  let type_of_global (env : t) (Type t : anytyp) : t * Wasm.Type.val_type * Wasm.Instruction.t list =
    let open Wasm.Type in let open Wasm.Instruction in
    match t with
    | TTuple TLnil -> env, NumT I32T, [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TInt -> env, NumT I64T, [Const (Wasm.Value.(I64 (I64.of_bits 0L)))]
    | TBool -> env, NumT I32T, [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TFloat -> env, NumT F64T, [Const (Wasm.Value.(F64 (F64.of_float 0.)))]
    | TFunc _ -> failwith "TODO: TMap.type_of_global TFunc"
    | TClosedFun _ -> raise (internal_error "Unexpected global of IR type ClosedFun")
    | TClosedVar _ -> failwith "TODO: TMap.type_of_global TClosedVar"
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: TMap.type_of_global TCont"
    | TTuple _ -> failwith "TODO: TMap.type_of_global TTuple"
  
  let type_of_typeid (env : t) (t : mtypid) : t * int32 = type_of_type env (MTypMap.find t env.menv)
  let type_of_closure_content (env : t) (TypeList tl : anytyp_list) : t * int32 =
    let env = ref env in
    let [@tail_mod_cons] rec inner : 'a. 'a typ_list -> _ = fun (type a) (tl : a typ_list) -> match tl with
      | TLnil -> []
      | TLcons (hd, tl) -> type_to_field env (Type hd) :: inner tl
    in let ret = inner tl in
    let env = !env in
    let open Wasm.Type in
    add_rectyp env (RecT [SubT (Final, [], DefStructT (StructT ret))])
  let type_of_closure_struct (env : t) (tf : ('a, 'b) functyp typ) (closidx : int32) : t * int32 =
    let env, funidx = type_of_type env (Type tf) in
    ignore closidx;
    let open Wasm.Type in
    add_rectyp env (RecT [SubT (Final, [], DefStructT (StructT [
      FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX funidx))));
      FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
    ]))])
  
  let type_of_handler (env : t) (TCont tcret : 'a continuation typ) (tret : 'b typ) : t * int32 * int32 =
    let open Wasm.Type in
    let env, tcret, tret =
      let env = ref env in
      let tcret = type_to_val env (Type tcret) in
      let tret = type_to_val env (Type tret) in
      !env, tcret, tret in
    let env, rawid = add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT)], [tcret])))]) in
    let env, contid = add_rectyp env (RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX rawid))))]) in
    let env, hdlid = add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT (
      [RefT (NoNull, VarHT (StatX contid)); RefT (Null, StructHT); RefT (Null, StructHT)],
      [tret])))]) in
    env, contid, hdlid
  let type_of_handler_block (env : t) (contid : int32) (eargs : 'a typ_list) : t * int32 =
    let open Wasm.Type in
    let env, eargs =
      let env = ref env in
      let eargs = type_to_val_list env (TypeList eargs) in
      !env, eargs @ [RefT (NoNull, VarHT (StatX contid))] in
    add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], eargs)))])
  let type_of_handler_finish (env : t) (contid : int32) (eargs : 'a typ_list) : t * int32 =
    let open Wasm.Type in
    let env, eargs =
      let env = ref env in
      let eargs = type_to_val_list env (TypeList eargs) in
      !env, eargs in
    add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], eargs)))])
  
  let type_of_locals (env : t) (locs : anytyp_list) : t * Wasm.Type.val_type list * int32 =
    let env = ref env in
    let cid = ref default_closure in
    let rec inner (TypeList tl) = match tl with
      | TLnil -> []
      | TLcons (TClosArg ca, tl) ->
          let env', hd = type_of_closure_content !env (TypeList ca) in
          env := env'; cid := hd;
          Wasm.Type.(RefT (NoNull, VarHT (StatX hd))) :: inner (TypeList tl)
      | TLcons (hd, tl) -> type_to_val env (Type hd) :: inner (TypeList tl)
    in let ret = inner locs in !env, ret, !cid
  
  let to_wasm (env : t) : Wasm.Type.rec_type list = List.rev (env.reftyps)
end
type tmap = TMap.t

let convert_global (tm : tmap) ((_, t, name) : 'a * anytyp * string) : tmap * Wasm.global =
  let tm, t, init = TMap.type_of_global tm t in
  tm, Wasm.(Type.(GlobalT (Var, t)), init, Some name)
let convert_globals (tm : tmap) (gs : (mvarid * anytyp * string) list) (tl : Wasm.global list) : tmap * Wasm.global list =
  let tm = ref tm in
  let [@tail_mod_cons] rec inner gs = match gs with
    | [] -> tl
    | hd :: tl -> let tm', hd = convert_global !tm hd in tm := tm'; hd :: inner tl
  in let ret = inner gs in !tm, ret

(* The following functions build the instructions in reverse order *)
type instr_conv = Wasm.Instruction.t list -> Wasm.Instruction.t list
type tinstr_conv = tmap * anytyp * (Wasm.Instruction.t list -> Wasm.Instruction.t list)
let convert_unop (type a b) (tm : tmap) (op : (a, b) unop) (arg : instr_conv) : tinstr_conv = let open Wasm.Instruction in match op with
    | UONegI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg (Const Wasm.Value.(I64 (I64.of_bits 0L)) :: acc)
    | UONegF -> tm, Type TFloat, fun acc -> Unop (Wasm.Value.F64 FloatOp.Neg) :: arg acc
let convert_binop (type a b c) (tm : tmap) (op : (a, b, c) binop) (arg1 : instr_conv) (argt1 : anytyp) (arg2 : instr_conv) (_ : anytyp)
   : tinstr_conv =
  let open Wasm.Instruction in match op with
  | BOAddI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Add) :: arg2 (arg1 acc)
  | BOAddF -> tm, Type TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Add) :: arg2 (arg1 acc)
  | BOSubI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg2 (arg1 acc)
  | BOSubF -> tm, Type TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Sub) :: arg2 (arg1 acc)
  | BOMulI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Mul) :: arg2 (arg1 acc)
  | BOMulF -> tm, Type TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Mul) :: arg2 (arg1 acc)
  | BODivI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.DivS) :: arg2 (arg1 acc)
  | BODivF -> tm, Type TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Div) :: arg2 (arg1 acc)
  | BORemI -> tm, Type TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.RemS) :: arg2 (arg1 acc)
  | BOEq ->
      let op : relop = let open Wasm.Value in match argt1 with
        | Type (TTuple TLnil) -> I32 IntOp.Eq
        | Type TInt -> I64 IntOp.Eq
        | Type TBool -> I32 IntOp.Eq
        | Type TFloat -> F64 FloatOp.Eq
        | _ -> raise (internal_error "Invalid operand type for builtin equality")
      in tm, Type TBool, fun acc -> Relop op :: arg2 (arg1 acc)
  | BONe ->
      let op : relop = let open Wasm.Value in match argt1 with
        | Type (TTuple TLnil) -> I32 IntOp.Ne
        | Type TInt -> I64 IntOp.Ne
        | Type TBool -> I32 IntOp.Ne
        | Type TFloat -> F64 FloatOp.Ne
        | _ -> raise (internal_error "Invalid operand type for builtin inequality")
      in tm, Type TBool, fun acc -> Relop op :: arg2 (arg1 acc)

let convert_get_var' (loc : locality) (vid : int32) (ctid : int32) (cid : int32) : instr_conv =
  let open Wasm.Instruction in match loc with
  | Global -> fun acc -> GlobalGet vid :: acc
  | Local StorVariable -> fun acc -> LocalGet vid :: acc
  | Local StorClosure -> fun acc -> StructGet (ctid, vid, None) :: LocalGet cid :: acc
let convert_get_var (loc : locality) (vid : 'a varid) (ctid : int32) (cid : int32) : instr_conv =
  let _, vid = (vid : _ varid :> _ typ * int32) in convert_get_var' loc vid ctid cid
let rec convert_block : 'a. _ -> 'a block -> _ -> _ -> _ -> tinstr_conv =
  fun (type a) (tm : tmap) ((ass, e) : a block) (is_last : bool) (ctid : int32) (cid : int32) : tinstr_conv ->
  let open Wasm.Instruction in
  let rec inner tm (ass : assign list) : tinstr_conv = match ass with
    | [] -> convert_expr tm e is_last ctid cid
    | Assign (l, v, e) :: tl ->
        let _, v = (v : _ varid :> (_ typ * int32)) in
        let tm, t, f = inner tm tl in
        let tm, _, e = convert_expr tm e false ctid cid in
        tm, t, fun acc -> f ((match l with
          | Global -> GlobalSet v
          | Local StorVariable -> LocalSet v
          | Local StorClosure -> raise (internal_error "unexpected assignment to closure variable")
          ) :: e acc)
  in inner tm ass
and convert_expr : 'a. _ -> 'a expr -> _ -> _ -> _ -> tinstr_conv =
    fun (type a) (tm : tmap) (e : a expr) (is_last : bool) (ctid : int32) (cid : int32) : tinstr_conv ->
  let open Wasm.Instruction in match e with
  | EConvertClosure (src, _, _) ->
      tm, Type (TTuple TLnil), fun acc -> RefCast Wasm.Type.(NoNull, VarHT (StatX ctid)) :: LocalGet (src :> int32) :: acc
  | EIgnore (_, e) ->
      let tm, _, e = convert_expr tm e false ctid cid in
      tm, Type (TTuple TLnil), fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: e acc
  | EConstUnit -> tm, Type (TTuple TLnil), fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: acc
  | EConstInt i -> tm, Type TInt, fun acc -> Const Wasm.Value.(I64 (I64.of_bits i)) :: acc
  | EConstBool b -> tm, Type TBool, fun acc -> Const Wasm.Value.(I32 (I32.of_bits (if b then 1l else 0l))) :: acc
  | EConstFloat f -> tm, Type TFloat, fun acc -> Const Wasm.Value.(F64 (F64.of_float f)) :: acc
  | EUnop (op, e) ->
      let tm, _, arg = convert_expr tm e false ctid cid in
      convert_unop tm op arg
  | EBinop (op, e1, e2) ->
      let tm, argt1, arg1 = convert_expr tm e1 false ctid cid in
      let tm, argt2, arg2 = convert_expr tm e2 false ctid cid in
      convert_binop tm op arg1 argt1 arg2 argt2
  | EVariable (loc, vid) -> let t, _ = (vid : _ varid :> _ typ * _) in tm, Type t, convert_get_var loc vid ctid cid
  | EClose (f, cls) ->
      let (TFunc (_, tret) as tf), clts, _, fid = (f : _ funcid :> _ * _ * mtypid * int32) in
      let tm, fcid = TMap.type_of_closure_content tm (TypeList clts) in
      let tm, new_ctid = TMap.type_of_closure_struct tm tf fcid in
      let tm, gen_new_struct = convert_new_struct tm cls fcid ctid cid in
      tm, Type tret, fun acc -> StructNew (new_ctid, Explicit) :: gen_new_struct (RefFunc fid :: acc)
  | ECallRawHandler (fid, _, contarg, targs, args, hdlarg, tret) ->
      let fid = (fid :> int32) in
      let tm, fcid = TMap.type_of_closure_content tm (TypeList targs) in
      let tm, args = convert_new_struct tm args fcid ctid cid in
      let tm, _, hdlarg = convert_expr tm hdlarg false ctid cid in
      let tm, _, contarg = convert_expr tm contarg false ctid cid in
      tm, Type tret, fun acc -> (if is_last then ReturnCall fid else Call fid) :: hdlarg (args (contarg acc))
  | ECallClosed (EClose (f, cls), args, _) ->
      let TFunc (_, tret), clts, _, fid = (f : _ funcid :> _ * _ * mtypid * int32) in
      let tm, fcid = TMap.type_of_closure_content tm (TypeList clts) in
      let tm, args = convert_exprs tm args ctid cid in
      let tm, gen_new_struct = convert_new_struct tm cls fcid ctid cid in
      tm, Type tret, fun acc -> (if is_last then ReturnCall fid else Call fid) :: gen_new_struct (args acc)
  | ECallClosed (EVariable (loc, vid), args, _) ->
      let (TClosedVar (targs, tret) as t), _ = (vid : _ varid :> _ typ * int32) in
      let tm, vtid = TMap.type_of_type tm (Type t) in
      let tm, fid = TMap.type_of_type tm (Type (TFunc (targs, tret))) in
      let getv = convert_get_var loc vid ctid cid in
      let tm, args = convert_exprs tm args ctid cid in
      tm, Type tret, fun acc -> (if is_last then ReturnCallRef fid else CallRef fid) ::
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: getv (args acc))
  | ECallClosed (_f, _args, _) -> failwith "TODO: convert_expr ECallClosed non-Function and non-Variable"
  | ECond (rt, e, t, f) ->
      let tm, _, ti = convert_block tm t is_last ctid cid in
      let tm, _, fi = convert_block tm f is_last ctid cid in
      let tm, _, ei = convert_expr tm e false ctid cid in
      let tm, rt' = TMap.ovaltype_of_type tm (Type rt) in
      tm, Type rt, fun acc -> If (Wasm.Type.(ValBlockType rt'), List.rev (ti []), List.rev (fi []))
                           :: ei acc
  | EDo (eid, args) ->
      let TClosedVar (_, tret), eid = (eid : _ effectid :> _ * int32) in
      let tm, args = convert_exprs tm args ctid cid in
      let tm, ret = match tret with
        | TTuple TLnil -> tm, fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: acc
        | TTuple tl ->
            let tm, stid = TMap.type_of_closure_content tm (TypeList tl) in
            tm, fun acc -> RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc
        | _ ->
            let tm, stid = TMap.type_of_closure_content tm (TypeList (TLcons (tret, TLnil))) in
            tm, fun acc -> StructGet (stid, 0l, None) :: RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc in
      tm, Type tret, fun acc -> ret (Suspend eid :: args acc)
  | EShallowHandle _ -> failwith "TODO: convert_expr EShallowHandle"
  | EDeepHandle (contid, contargs, hdlid, hdlargs) ->
      let TFunc (_, thdlret), thdlcl, _, hdlid = (hdlid : _ funcid :> _ * _ * mtypid * int32) in
      let tm, hdlcid = TMap.type_of_closure_content tm (TypeList thdlcl) in
      let tm, gen_hdl_struct = convert_new_struct tm hdlargs hdlcid ctid cid in
      let TFunc (TLnil, tcret), tcontcl, _, contid = (contid : _ funcid :> _ * _ * mtypid * int32) in
      let tm, contcid = TMap.type_of_closure_content tm (TypeList tcontcl) in
      let tm, gen_cont_struct = convert_new_struct tm contargs contcid ctid cid in
      let tm, tcontid = TMap.type_of_type tm (Type (TCont tcret)) in
      tm, Type thdlret, fun acc -> Call hdlid :: gen_hdl_struct (gen_cont_struct (ContNew tcontid :: RefFunc contid :: acc))
  | ECont (loc, contid, contargs, hdlfid, hdlclloc, hdlclid) ->
      let TFunc (TLcons (_, TLcons (TTuple cats, _)), tret), _unusable, _thdlclid, hdlfid = (hdlfid : _ funcid :> _ * _ * _ * int32) in
      let hdlclid = (hdlclid :> int32) in
      let tm, fcid = TMap.type_of_closure_content tm (TypeList cats) in
      let tm, args = convert_new_struct tm contargs fcid ctid cid in
      tm, Type tret, fun acc ->
        Call hdlfid ::
        convert_get_var' hdlclloc hdlclid ctid cid (
        args (
        convert_get_var loc contid ctid cid acc))
and convert_exprs : 'a. _ -> 'a expr_list -> _ -> _ -> tmap * instr_conv =
  fun (type a) (tm : tmap) (es : a expr_list) (ctid : int32) (cid : int32) : (tmap * instr_conv) ->
  match es with
  | ELnil -> tm, fun acc -> acc
  | ELcons (ehd, etl) ->
      let tm, _, f = convert_expr tm ehd false ctid cid in
      let tm, f2 = convert_exprs tm etl ctid cid in
      tm, fun acc -> f2 (f acc)
and convert_new_struct : 'a. _ -> 'a expr_list -> _ -> _ -> _ -> _ * instr_conv =
  fun (type a) tm (cls : a expr_list) fcid ctid cid -> let open Wasm.Instruction in match cls with
  | ELnil -> tm, fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
  | ELcons _ -> let tm, f = convert_exprs tm cls ctid cid in tm, fun acc -> StructNew (fcid, Explicit) :: f acc

(* This function returns the instruction in reverse order *)
let convert_anyblock (tm : tmap) (Block (_, b) : anyblock) (is_last : bool) (ctid : int32) (cid : int32) : tmap * Wasm.Instruction.t list =
  let tm, _, b = convert_block tm b is_last ctid cid in tm, b []
let convert_finisher (type a b) (tm : tmap) (f : (a, b) finisher) (is_last : bool) (ctid : int32) (cid : int32) : tmap * Wasm.Instruction.t list =
  match f with
  | FId _ -> tm, []
  | FMap (v, _, b) ->
      let _, v = (v : _ varid :> _ * int32) in
      let tm, _, b = convert_block tm b is_last ctid cid in
      tm, b Wasm.Instruction.[LocalSet v]

let convert_hdl (tm : tmap) (type a b c) (f : (a, b, c) fhandler) : tmap * Wasm.fundef =
  let (tcont, contidx), contcl = (f.fh_contarg : _ varid * mvarid :> (_ * int32) * int32) in
  let tret = match f.fh_finisher with FId t -> (t : b typ) | FMap (_, t, _) -> t in
  let tm, contid, fun_typ = TMap.type_of_handler tm tcont tret in
  let tm, fn_locals, clostyp = TMap.type_of_locals tm f.fh_locals in
  let open Wasm.Instruction in
  let convert_clos = match f.fh_closure with
    | None -> []
    | Some (src, dst) ->
        let _, dst = (dst : _ varid :> _ * int32) in
        [LocalSet dst; RefCast Wasm.Type.(NoNull, (VarHT (StatX clostyp))); LocalGet (src :> int32)]
  in let cid = (Option.value ~default:2l (Option.map fst f.fh_closure :> int32 option))
  in let tm, code =
    let nblocks, handlers =
      let rec inner (hdls : _ handler list) len acc = match hdls with
        | [] -> len, acc
        | Handler (eid, _, _, _) :: tl -> inner tl (Int32.succ len) ((snd (eid : _ effectid :> _ * int32), OnLabel len) :: acc)
      in inner f.fh_handlers 0l [] in
    let tm, code = convert_finisher tm f.fh_finisher true clostyp cid in
    let code = Resume (contid, handlers) :: List.rev_append code [Return] in
    let rec do_cases tm nblocks (cases : (a, b) handler list) code = match cases with
      | [] -> tm, code
      | Handler (type ea er) (eid, varc, vars, blk : (ea, er) effectid * _ * _ * _) :: tl ->
          let tm, codehdl =
            let tm, _, b = convert_block tm blk true clostyp cid in
            tm, List.rev_append (b []) [Return] in
          let tm, codehdl =
            let rec inner : 'r. _ -> 'r varid_list -> _ -> _ = fun (type r) tm (vars : r varid_list) codehdl -> match vars with
              | VLnil -> tm, codehdl
              | VLcons (vhd, vtl) ->
                  let _, v = (vhd : _ varid :> _ * int32) in
                  inner tm vtl (LocalSet v :: codehdl)
            in inner tm vars codehdl in
          let _, varc = (varc : _ varid :> _ * int32) in
          let codehdl = LocalSet varc :: codehdl in
          let TClosedVar (eargs, _), _ = (eid : (ea, er) effectid :> (ea -> er) typ * int32) in
          let tm, blkid = TMap.type_of_handler_block tm contid eargs in
          let code = Wasm.Instruction.Block (Wasm.Type.VarBlockType blkid, code) :: codehdl in
          do_cases tm (Int32.pred nblocks) tl code
    in do_cases tm nblocks f.fh_handlers code
  in let tm, finalblk = TMap.type_of_handler_finish tm contid (TLcons (tret, TLnil))
  in tm, Wasm.{
    fn_name = None;
    fn_type = fun_typ;
    fn_locals;
    fn_code = List.rev_append convert_clos [LocalGet contcl; LocalGet contidx; Loop (Wasm.Type.VarBlockType finalblk, code)];
  }

let convert_fun_aux (tm : tmap) (ft : int32) (l : anytyp_list) (f : anyblock) (init_dest : int32 option) (closid : int32) :
    tmap * int32 * Wasm.fundef =
  let tm, fn_locals, clostyp = TMap.type_of_locals tm l in
  let tm, code = convert_anyblock tm f (Option.is_none init_dest) clostyp closid in
  tm, clostyp, Wasm.{
    fn_name = None;
    fn_type = ft;
    fn_locals;
    fn_code = List.rev (match init_dest with Some gv -> Wasm.Instruction.GlobalSet gv :: code | None -> code);
  }

let convert_fun (tm : tmap) (f : func') : tmap * int32 * Wasm.fundef =
  let tm, fun_typ = TMap.type_of_typeid tm f.fun_typ in
  convert_fun_aux tm fun_typ f.fun_locals f.fun_block None (Option.value ~default:0l (f.fun_converted_closure :> int32 option))
let convert_fun_step2 (tm : tmap) (f, clostyp : func' * int32) : tmap * Wasm.fundef option = match f.fun_export_data with
  | None -> tm, None
  | Some (name, typid) ->
      let tm, fn_type = TMap.type_of_typeid tm typid in
      tm, Some Wasm.{
        fn_name = Some name;
        fn_type;
        fn_locals = [];
        fn_code = List.rev Wasm.Instruction.(
          ReturnCall (f.fun_id :> int32) ::
          RefNull Wasm.Type.(VarHT (StatX clostyp)) ::
          let rec inner : 'a. _ -> _ -> 'a typ_list -> _ = fun i acc (type a) (ls : a typ_list) -> match ls with
            | TLnil -> acc
            | TLcons (_, ls) -> inner (Int32.succ i) (LocalGet i :: acc) ls
          in let TypeList args = f.fun_args in inner 0l [] args);
      }
let convert_funs (tm : tmap) (fs : func list) (i : Wasm.fundef) : tmap * Wasm.fundef list =
  let tm = ref tm in
  let convert_fun_step2' v = let tm', v = convert_fun_step2 !tm v in tm := tm'; v in
  let [@tail_mod_cons] rec inner fs acc = match fs with
    | [] -> i :: List.filter_map convert_fun_step2' acc
    | FFunction hd :: tl -> let tm', ctid, fhd = convert_fun !tm hd in tm := tm'; fhd :: inner tl ((hd, ctid) :: acc)
    | FHandler hd :: tl -> let tm', fhd = convert_hdl !tm hd in tm := tm'; fhd :: inner tl acc
  in let ret = inner fs [] in !tm, ret

let generate_type_map (m : modu) : tmap = TMap.of_module m

let convert_fun_refs (tm : tmap) (fs : mtypid FunIDMap.t) : tmap * Wasm.global list =
  let convert_fun_ref tm (fid : mfunid) ft : tmap * Wasm.global =
    let tm, gt = TMap.type_of_typeid tm ft in
    tm, Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX gt))), Wasm.Instruction.[RefFunc (fid :> int32)], None)
  in FunIDMap.fold (fun fid ft (tm, acc) -> let tm, hd = convert_fun_ref tm fid ft in tm, hd :: acc) fs (tm, [])

let convert_effects (tm : tmap) (es : anytyp_list EffectIDMap.t) : tmap * int32 list =
  let es = EffectIDMap.bindings es in
  let convert_effect (tm : tmap) (e : meffid * anytyp_list) : tmap * int32 =
    let _, TypeList targs = e in
    let tret = TAbsClosArg in
    let tm, tid = TMap.type_of_type tm (Type (TClosedFun (targs, tret))) in
    tm, tid
  in List.fold_left_map convert_effect tm es

let compile (prog : Ir.program) (env : string Env.Int.t) : Wasm.module_ =
  let m = module_of_ir prog env in
  let tm = generate_type_map m in
  let tm, init_res =
    let Block (t, _) = m.mod_block in
    let tm, cg = convert_global tm (0, Type t, "_init_result") in
    tm, cg in
  let tm, frgbls = convert_fun_refs tm m.mod_needs_export in
  let tm, globals = convert_globals tm m.mod_global_vars (init_res :: frgbls) in
  let tm, _, init = convert_fun_aux tm TMap.init_func_type m.mod_locals m.mod_block (Some m.mod_nglobals) 0l in
  let tm, funs = convert_funs tm m.mod_funs init in
  let tm, tags = convert_effects tm m.mod_effs in
  let types = TMap.to_wasm tm in
  Wasm.{ types; globals; tags; funs; init = Some m.mod_nfuns }
