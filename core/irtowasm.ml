let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

open Wasmir

(* TODO: make it so that TUnit becomes nothing *)
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
  val type_of_conv_closure : t -> mtypid -> t * Wasm.Type.ref_type
  
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
    | TUnit -> NumT I32T
    | TInt -> NumT I64T
    | TBool -> NumT I32T
    | TFloat -> NumT F64T
    | TFunc _ -> failwith "TODO: TMap.type_to_val TFunc"
    | TClosedFun _ -> raise (internal_error "Unexpected value of IR type ClosedFun")
    | TClosedVar _ -> RefT (NoNull, VarHT (StatX (type_of_type' env (Type t))))
    | TAbsClosArg -> raise (internal_error "Unexpected value of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected value of IR type ClosArg")
    | TCont _ -> failwith "TODO: TMap.type_to_val TCont"
    | TPair _ -> failwith "TODO: TMap.type_to_val TPair"
  and [@tail_mod_cons] type_to_val_list (env : t ref) (TypeList tl : anytyp_list) : Wasm.Type.val_type list =
    let open Wasm.Type in
    match tl with
    | TLnil -> []
    | TLcons (TUnit, tl) -> NumT I32T :: type_to_val_list env (TypeList tl)
    | TLcons (TInt, tl) -> NumT I64T :: type_to_val_list env (TypeList tl)
    | TLcons (TBool, tl) -> NumT I32T :: type_to_val_list env (TypeList tl)
    | TLcons (TFloat, tl) -> NumT F64T :: type_to_val_list env (TypeList tl)
    | TLcons (TFunc _, _) -> failwith "TODO: TMap.type_to_val_list TFunc"
    | TLcons (TClosedFun _, _) -> raise (internal_error "Unexpected value in list of IR type ClosedFun")
    | TLcons (TClosedVar _ as hd, tl) -> RefT (NoNull, VarHT (StatX (type_of_type' env (Type hd)))) :: type_to_val_list env (TypeList tl)
    | TLcons (TAbsClosArg, tl) -> RefT (Null, StructHT) :: type_to_val_list env (TypeList tl)
    | TLcons (TClosArg _, _) -> raise (internal_error "Unexpected value in list of IR type ClosArg")
    | TLcons (TCont _, _) -> failwith "TODO: TMap.type_to_val_list TCont"
    | TLcons (TPair _, _) -> failwith "TODO: TMap.type_to_val_list TPair"
  and type_of_type' (env : t ref) (t : anytyp) : int32 = match TypeMap.find_opt t !env.cenv with
    | Some i -> i
    | None ->
        let newt =
          let open Wasm.Type in
          let convert_rec_type (Type t : anytyp) : rec_type = match t with
            | TUnit -> raise (internal_error "Cannot convert IR type TUnit to Wasm rec type")
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
            | TCont _ -> failwith "TODO: convert_rec_type TCont"
            | TPair _ -> failwith "TODO: convert_rec_type TPair"
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
    | TUnit -> env, NumT I32T, [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TInt -> env, NumT I64T, [Const (Wasm.Value.(I64 (I64.of_bits 0L)))]
    | TBool -> env, NumT I32T, [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TFloat -> env, NumT F64T, [Const (Wasm.Value.(F64 (F64.of_float 0.)))]
    | TFunc _ -> failwith "TODO: TMap.type_of_global TFunc"
    | TClosedFun _ -> raise (internal_error "Unexpected global of IR type ClosedFun")
    | TClosedVar _ -> failwith "TODO: TMap.type_of_global TClosedVar"
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: TMap.type_of_global TCont"
    | TPair _ -> failwith "TODO: TMap.type_of_global TPair"
  
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
  let type_of_conv_closure (env : t) (t : mtypid) : t * Wasm.Type.ref_type =
    let Type t =  MTypMap.find t env.menv in
    let TypeList tl = match t with
      | TClosArg tl -> TypeList tl
      | _ -> raise (internal_error "Unexpected type for local closure")
    in let env, tidx = type_of_closure_content env (TypeList tl) in
    env, Wasm.Type.(NoNull, VarHT (StatX tidx))
  let type_of_closure_struct (env : t) (tf : ('a, 'b) functyp typ) (closidx : int32) : t * int32 =
    let env, funidx = type_of_type env (Type tf) in
    ignore closidx;
    let open Wasm.Type in
    add_rectyp env (RecT [SubT (Final, [], DefStructT (StructT [
      FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX funidx))));
      FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
    ]))])
  
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
        | Type TUnit -> I32 IntOp.Eq
        | Type TInt -> I64 IntOp.Eq
        | Type TBool -> I32 IntOp.Eq
        | Type TFloat -> F64 FloatOp.Eq
        | _ -> raise (internal_error "Invalid operand type for builtin equality")
      in tm, Type TBool, fun acc -> Relop op :: arg2 (arg1 acc)
  | BONe ->
      let op : relop = let open Wasm.Value in match argt1 with
        | Type TUnit -> I32 IntOp.Ne
        | Type TInt -> I64 IntOp.Ne
        | Type TBool -> I32 IntOp.Ne
        | Type TFloat -> F64 FloatOp.Ne
        | _ -> raise (internal_error "Invalid operand type for builtin inequality")
      in tm, Type TBool, fun acc -> Relop op :: arg2 (arg1 acc)

let convert_get_var (loc : locality) (vid : 'a varid) (ctid : int32) (cid : int32) : instr_conv = fun acc ->
  let _, vid = (vid : _ varid :> _ typ * int32) in let open Wasm.Instruction in match loc with
  | Global -> GlobalGet vid :: acc
  | Local StorVariable -> LocalGet vid :: acc
  | Local StorClosure -> StructGet (ctid, vid, None) :: LocalGet cid :: acc
let rec convert_block : 'a. _ -> 'a block -> _ -> _ -> _ -> tinstr_conv =
  fun (type a) (tm : tmap) ((ass, e) : a block) (is_toplevel : bool) (ctid : int32) (cid : int32) : tinstr_conv ->
  let open Wasm.Instruction in
  let rec inner tm (ass : assign list) : tinstr_conv = match ass with
    | [] -> convert_expr tm e is_toplevel ctid cid
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
    fun (type a) (tm : tmap) (e : a expr) (is_toplevel : bool) (ctid : int32) (cid : int32) : tinstr_conv ->
  let open Wasm.Instruction in match e with
  | EConvertClosure (src, _, ctid) ->
      let tm, rt = TMap.type_of_conv_closure tm ctid in
      tm, Type TUnit, fun acc -> RefCast rt :: LocalGet (src :> int32) :: acc
  | EConstUnit -> tm, Type TUnit, fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: acc
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
      let tm, gen_new_struct = match cls with
        | ELnil -> tm, fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
        | ELcons _ -> let tm, f = convert_exprs tm cls ctid cid in tm, fun acc -> StructNew (fcid, Explicit) :: f acc
      in tm, Type tret, fun acc -> StructNew (new_ctid, Explicit) :: gen_new_struct (RefFunc fid :: acc)
  | ECallClosed (EClose (f, cls), args, _) ->
      let TFunc (_, tret), clts, _, fid = (f : _ funcid :> _ * _ * mtypid * int32) in
      let tm, fcid = TMap.type_of_closure_content tm (TypeList clts) in
      let tm, args = convert_exprs tm args ctid cid in
      let tm, gen_new_struct = match cls with
        | ELnil -> tm, fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
        | ELcons _ -> let tm, f = convert_exprs tm cls ctid cid in tm, fun acc -> StructNew (fcid, Explicit) :: f acc
      in tm, Type tret, fun acc -> (if is_toplevel then ReturnCall fid else Call fid) :: gen_new_struct (args acc)
  | ECallClosed (EVariable (loc, vid), args, _) ->
      let (TClosedVar (targs, tret) as t), _ = (vid : _ varid :> _ typ * int32) in
      let tm, vtid = TMap.type_of_type tm (Type t) in
      let tm, fid = TMap.type_of_type tm (Type (TFunc (targs, tret))) in
      let getv = convert_get_var loc vid ctid cid in
      let tm, args = convert_exprs tm args ctid cid in
      tm, Type tret, fun acc -> (if is_toplevel then CallRef fid else ReturnCallRef fid) ::
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: getv (args acc))
  | ECallClosed (_f, _args, _) -> failwith "TODO: convert_expr ECallClosed non-Function and non-Variable"
  | ECond (rt, e, t, f) ->
      let tm, _, ti = convert_block tm t false ctid cid in
      let tm, _, fi = convert_block tm f false ctid cid in
      let tm, _, ei = convert_expr tm e false ctid cid in
      let tm, rt' = TMap.ovaltype_of_type tm (Type rt) in
      tm, Type rt, fun acc -> If (Wasm.Type.(ValBlockType rt'), List.rev (ti []), List.rev (fi []))
                           :: ei acc
  (* | ELoop _ -> failwith "TODO: convert_expr ELoop" *)
  | EDo _ -> failwith "TODO: convert_expr EDo"
  | EHandle _ -> failwith "TODO: convert_expr EHandle"
and convert_exprs : 'a. _ -> 'a expr_list -> _ -> _ -> tmap * instr_conv =
  fun (type a) (tm : tmap) (es : a expr_list) (ctid : int32) (cid : int32) : (tmap * instr_conv) ->
  match es with
  | ELnil -> tm, fun acc -> acc
  | ELcons (ehd, etl) ->
      let tm, _, f = convert_expr tm ehd false ctid cid in
      let tm, f2 = convert_exprs tm etl ctid cid in
      tm, fun acc -> f2 (f acc)

(* This function returns the instruction in reverse order *)
let convert_anyblock (tm : tmap) (Block (_, b) : anyblock) (is_toplevel : bool) (ctid : int32) (cid : int32) : tmap * Wasm.Instruction.t list =
  let tm, _, b = convert_block tm b is_toplevel ctid cid in tm, b []

let convert_fun_aux (tm : tmap) (ft : int32) (l : anytyp_list) (f : anyblock) (isinit : int32 option) (closid : int32) :
    tmap * int32 * Wasm.fundef =
  let tm, fn_locals, clostyp = TMap.type_of_locals tm l in
  let tm, code = convert_anyblock tm f (Option.is_some isinit) clostyp closid in
  tm, clostyp, Wasm.{
    fn_name = None;
    fn_type = ft;
    fn_locals;
    fn_code = List.rev (match isinit with Some gv -> Wasm.Instruction.GlobalSet gv :: code | None -> code);
  }

let convert_fun (tm : tmap) (f : func) : tmap * int32 * Wasm.fundef =
  let tm, fun_typ = TMap.type_of_typeid tm f.fun_typ in
  convert_fun_aux tm fun_typ f.fun_locals f.fun_block None (Option.value ~default:0l (f.fun_converted_closure :> int32 option))
let convert_fun_step2 (tm : tmap) (f, clostyp : func * int32) : tmap * Wasm.fundef option = match f.fun_export_data with
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
  let [@tail_mod_cons] rec inner fs' acc = match fs' with
    | [] -> i :: List.filter_map convert_fun_step2' acc
    | hd :: tl -> let tm', ctid, fhd = convert_fun !tm hd in tm := tm'; fhd :: inner tl ((hd, ctid) :: acc)
  in let ret = inner fs [] in !tm, ret

let generate_type_map (m : modu) : tmap = TMap.of_module m

let convert_fun_refs (tm : tmap) (fs : mtypid FunIDMap.t) : tmap * Wasm.global list =
  let convert_fun_ref tm (fid : mfunid) ft : tmap * Wasm.global =
    let tm, gt = TMap.type_of_typeid tm ft in
    tm, Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX gt))), Wasm.Instruction.[RefFunc (fid :> int32)], None)
  in FunIDMap.fold (fun fid ft (tm, acc) -> let tm, hd = convert_fun_ref tm fid ft in tm, hd :: acc) fs (tm, [])

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
  let types = TMap.to_wasm tm in
  Wasm.{ types; globals; funs = funs; init = Some m.mod_nfuns }
