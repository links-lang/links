let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

open Wasmir

(* TODO: use cont.bind instead of struct.new *)

(* TODO: make it so that TTuple NTLnil becomes nothing *)
module TMap : sig
  type t
  val of_module : modu -> t
  val default_closure : int32
  val init_func_type : int32
  val variant_tid : int32
  
  val val_of_type : t -> 'a typ -> Wasm.Type.val_type
  val recid_of_type : t -> 'a typ -> int32
  val recid_of_typeid : t -> mtypid -> int32
  
  val oval_of_type : t -> 'a typ -> Wasm.Type.val_type option
  val recid_of_closed : t -> ('a, 'b) functyp typ -> int32 -> int32
  
  val recid_of_locals : t -> anytyp_list -> Wasm.Type.val_type list * int32
  
  val recids_of_handler : t -> 'a continuation typ -> 'b typ -> int32 * int32
  val recid_of_handler_block : t -> int32 -> 'a typ_list -> int32
  val recid_of_handler_finish : t -> int32 -> 'a typ -> int32
  
  val recid_of_exported_type : t -> 'a typ_list -> 'b typ -> int32
  val recid_of_exported_typeid : t -> mtypid -> int32
  
  val to_wasm : t -> Wasm.Type.rec_type list
end = struct
  type t = {
    mutable menv: anytyp MTypMap.t;
    mutable cenv: Wasm.Type.rec_type TypeMap.t;
    mutable eenv: int32 TypeMap.t;
    mutable nrefs: int32;
    mutable reftyps: Wasm.Type.rec_type list;
  }
  let of_module (m : modu) : t = {
    menv = m.mod_typs;
    cenv = TypeMap.empty;
    eenv = TypeMap.empty;
    nrefs = 3l;
    reftyps = Wasm.Type.[
      RecT [SubT (Final, [], DefStructT (StructT [
        FieldT (Cons, ValStorageT (NumT I32T));
        FieldT (Cons, ValStorageT (RefT (Null, AnyHT)));
      ]))];
      RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))];
      RecT [SubT (Final, [], DefStructT (StructT []))];
    ];
  }
  let default_closure : int32 = 0l
  let init_func_type : int32 = 1l
  let variant_tid : int32 = 2l
  
  let add_rectyp (env : t) (t : Wasm.Type.rec_type) : int32 = match List.find_index ((=) t) env.reftyps with
    | Some tidx -> Int32.sub env.nrefs (Int32.of_int (tidx + 1))
    | None ->
        let tidx = env.nrefs in
        let nrefs = Int32.succ tidx in
        env.nrefs <- nrefs; env.reftyps <- t :: env.reftyps; tidx
  
  let rec val_of_type : 'a. _ -> 'a typ -> _ = fun (type a) (env : t) (t : a typ) : Wasm.Type.val_type ->
    let open Wasm.Type in match t with
    | TTuple NTLnil -> NumT I32T
    | TInt -> NumT I64T
    | TBool -> NumT I32T
    | TFloat -> NumT F64T
    | TFunc _ -> failwith "TODO: TMap.val_of_type TFunc"
    | TClosed _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TAbsClosArg -> RefT (Null, StructHT)
    | TClosArg _ -> raise (internal_error "Unexpected value of IR type ClosArg")
    | TCont _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TTuple _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TVariant -> RefT (NoNull, VarHT (StatX variant_tid))
  
  and [@tail_mod_cons] val_list_of_type_list : 'a. _ -> 'a typ_list -> _ = fun (type a) (env : t) (tl : a typ_list) : Wasm.Type.val_type list ->
    match tl with
    | TLnil -> []
    | TLcons (hd, tl) -> let hd = val_of_type env hd in hd :: val_list_of_type_list env tl
  and [@tail_mod_cons] val_list_of_named_type_list : 'a. _ -> 'a named_typ_list -> _ =
    fun (type a) (env : t) (tl : a named_typ_list) : Wasm.Type.val_type list ->
    match tl with
    | NTLnil -> []
    | NTLcons (_, hd, tl) -> let hd = val_of_type env hd in hd :: val_list_of_named_type_list env tl
  
  and rec_of_type : 'a. _ -> 'a typ -> _ = fun (type a) (env : t) (t : a typ) : Wasm.Type.rec_type ->
    match TypeMap.find_opt (Type t) env.cenv with
    | Some rt -> rt
    | None ->
        let newt =
          let open Wasm.Type in match t with
          | TTuple NTLnil -> failwith "TODO: TMap.rec_of_type TTuple NTLnil"
          | TInt -> failwith "TODO: TMap.rec_of_type TInt"
          | TBool -> failwith "TODO: TMap.rec_of_type TBool"
          | TFloat -> failwith "TODO: TMap.rec_of_type TFloat"
          | TFunc (args, ret) ->
              let args = val_list_of_type_list env args in
              let ret = val_of_type env ret in
              RecT [SubT (Final, [], DefFuncT (FuncT (args @ [RefT (Null, StructHT)], [ret])))]
          | TClosed (args, ret) ->
              let ftyp = recid_of_type env (TFunc (args, ret)) in
              RecT [SubT (Final, [], DefStructT (StructT [
                FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftyp))));
                FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
              ]))]
          | TAbsClosArg -> failwith "TODO: TMap.rec_of_type TAbsClosArg"
          | TClosArg clos ->
              let clos = val_list_of_type_list env clos in
              RecT [SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) clos)))]
          | TCont cret ->
              let ftyp = recid_of_type env (TFunc (TLnil, cret)) in
              RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX ftyp))))]
          | TTuple elems ->
              let elems = val_list_of_named_type_list env elems in
              RecT [SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) elems)))]
          | TVariant -> RecT [SubT (Final, [], DefStructT (StructT [
                FieldT (Cons, ValStorageT (NumT I32T));
                FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
              ]))]
        in env.cenv <- TypeMap.add (Type t) newt env.cenv; newt
  
  and recid_of_type : 'a. _ -> 'a typ -> _ = fun (type a) (env : t) (t : a typ) : int32 ->
    let t = rec_of_type env t in add_rectyp env t
  
  let recid_of_typeid (env : t) (t : mtypid) : int32 =
    let Type t = MTypMap.find t env.menv in recid_of_type env t
  
  (* TODO: optimize ()s away? *)
  let oval_of_type (env : t) (t : 'a typ) : Wasm.Type.val_type option =
    Some (val_of_type env t)
  
  let recid_of_closed (env : t) (TFunc (targs, tret) : ('a, 'b) functyp typ) (_clostid : int32) : int32 =
    recid_of_type env (TClosed (targs, tret))
  
  let recid_of_locals (env : t) (locs : anytyp_list) : Wasm.Type.val_type list * int32 =
    let cid = ref default_closure in
    let [@tail_mod_cons] rec inner (TypeList tl) = match tl with
      | TLnil -> []
      | TLcons (TClosArg ca, tl) ->
          let hd = recid_of_type env (TClosArg ca) in
          cid := hd;
          Wasm.Type.(RefT (NoNull, VarHT (StatX hd))) :: inner (TypeList tl)
      | TLcons (hd, tl) -> val_of_type env hd :: inner (TypeList tl)
    in let ret = inner locs in ret, !cid
  
  let recids_of_handler (env : t) (TCont cret : 'a continuation typ) (tret : 'b typ) : int32 * int32 =
    let open Wasm.Type in
    let cret = val_of_type env cret in
    let tret = val_of_type env tret in
    let rawid = add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT)], [cret])))]) in
    let contid = add_rectyp env (RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX rawid))))]) in
    let hdlid = add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT (
      [RefT (NoNull, VarHT (StatX contid)); RefT (Null, StructHT); RefT (Null, StructHT)],
      [tret])))]) in
    contid, hdlid
  let recid_of_handler_block (env : t) (contid : int32) (eargs : 'a typ_list) : int32 =
    let open Wasm.Type in
    let eargs = val_list_of_type_list env eargs @ [RefT (NoNull, VarHT (StatX contid))] in
    add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], eargs)))])
  let recid_of_handler_finish (env : t) (contid : int32) (eret : 'a typ) : int32 =
    let eret = val_of_type env eret in
    let open Wasm.Type in
    add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], [eret])))])
  
  let recid_of_exported_type (env : t) (targs : 'a typ_list) (tret : 'b typ) : int32 =
    match TypeMap.find_opt (Type (TFunc (targs, tret))) env.eenv with
    | Some n -> n | None ->
        let open Wasm.Type in
        let targs' = val_list_of_type_list env targs in
        let tret' = val_of_type env tret in
        let ret = add_rectyp env (RecT [SubT (Final, [], DefFuncT (FuncT (targs', [tret'])))]) in
        env.eenv <- TypeMap.add (Type (TFunc (targs, tret))) ret env.eenv;
        ret
  let recid_of_exported_typeid (env : t) (tid : mtypid) : int32 =
    match MTypMap.find tid env.menv with
    | Type (TFunc (targs, tret)) -> recid_of_exported_type env targs tret
    | _ -> raise (internal_error "Attempting to export non-function type")
  
  let to_wasm (env : t) : Wasm.Type.rec_type list = List.rev (env.reftyps)
end
type tmap = TMap.t

let convert_global (tm : tmap) ((_, Type t, name) : 'a * anytyp * string) : Wasm.global =
  let init =
    let open Wasm.Instruction in match t with
    | TTuple NTLnil -> [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TInt -> [Const (Wasm.Value.(I64 (I64.of_bits 0L)))]
    | TBool -> [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TFloat -> [Const (Wasm.Value.(F64 (F64.of_float 0.)))]
    | TFunc _ -> failwith "TODO: convert_global TFunc"
    | TClosed _ -> failwith "TODO: convert_global TClosed"
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: convert_global TCont"
    | TTuple _ -> failwith "TODO: convert_global TTuple"
    | TVariant -> [
          Const (Wasm.Value.(I32 (I32.of_bits 0l)));
          RefNull Wasm.Type.StructHT;
          StructNew (TMap.variant_tid, Explicit)
        ] in
  let t = TMap.val_of_type tm t in
  Wasm.(Type.(GlobalT (Var, t)), init, Some name)
let convert_globals (tm : tmap) (gs : (mvarid * anytyp * string) list) (tl : Wasm.global list) : Wasm.global list =
  let [@tail_mod_cons] rec inner gs = match gs with
    | [] -> tl
    | hd :: tl -> let hd = convert_global tm hd in hd :: inner tl
  in let ret = inner gs in ret

(* The following functions build the instructions in reverse order *)
type instr_conv = Wasm.Instruction.t list -> Wasm.Instruction.t list
type 'a tinstr_conv = 'a typ * (Wasm.Instruction.t list -> Wasm.Instruction.t list)
let convert_unop (type a b) (_ : tmap) (op : (a, b) unop) (arg : instr_conv) : b tinstr_conv = let open Wasm.Instruction in match op with
    | UONegI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg (Const Wasm.Value.(I64 (I64.of_bits 0L)) :: acc)
    | UONegF -> TFloat, fun acc -> Unop (Wasm.Value.F64 FloatOp.Neg) :: arg acc
let convert_binop (type a b c) (_ : tmap) (op : (a, b, c) binop) (arg1 : instr_conv) (argt1 : a typ) (arg2 : instr_conv) (_ : b typ)
   : c tinstr_conv =
  let open Wasm.Instruction in match op with
  | BOAddI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Add) :: arg2 (arg1 acc)
  | BOAddF -> TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Add) :: arg2 (arg1 acc)
  | BOSubI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg2 (arg1 acc)
  | BOSubF -> TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Sub) :: arg2 (arg1 acc)
  | BOMulI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.Mul) :: arg2 (arg1 acc)
  | BOMulF -> TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Mul) :: arg2 (arg1 acc)
  | BODivI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.DivS) :: arg2 (arg1 acc)
  | BODivF -> TFloat, fun acc -> Binop (Wasm.Value.F64 FloatOp.Div) :: arg2 (arg1 acc)
  | BORemI -> TInt, fun acc -> Binop (Wasm.Value.I64 IntOp.RemS) :: arg2 (arg1 acc)
  | BOEq ->
      let op : relop = let open Wasm.Value in match argt1 with
        | TTuple NTLnil -> I32 IntOp.Eq
        | TInt -> I64 IntOp.Eq
        | TBool -> I32 IntOp.Eq
        | TFloat -> F64 FloatOp.Eq
        | _ -> raise (internal_error "Invalid operand type for builtin equality")
      in TBool, fun acc -> Relop op :: arg2 (arg1 acc)
  | BONe ->
      let op : relop = let open Wasm.Value in match argt1 with
        | TTuple NTLnil -> I32 IntOp.Ne
        | TInt -> I64 IntOp.Ne
        | TBool -> I32 IntOp.Ne
        | TFloat -> F64 FloatOp.Ne
        | _ -> raise (internal_error "Invalid operand type for builtin inequality")
      in TBool, fun acc -> Relop op :: arg2 (arg1 acc)
  | BOLe -> TBool, fun acc -> Relop (Wasm.Value.I64 IntOp.LeS) :: arg2 (arg1 acc)
  | BOLt -> TBool, fun acc -> Relop (Wasm.Value.I64 IntOp.LtS) :: arg2 (arg1 acc)
  | BOGe -> TBool, fun acc -> Relop (Wasm.Value.I64 IntOp.GeS) :: arg2 (arg1 acc)
  | BOGt -> TBool, fun acc -> Relop (Wasm.Value.I64 IntOp.GtS) :: arg2 (arg1 acc)

type clos_info = (int32 * int32) option (* Closure type ID, closure ID *)
let convert_get_var' (loc : locality) (vid : int32) (cinfo : clos_info) : instr_conv =
  let open Wasm.Instruction in match loc with
  | Global -> fun acc -> GlobalGet vid :: acc
  | Local StorVariable -> fun acc -> LocalGet vid :: acc
  | Local StorClosure -> begin match cinfo with
    | None -> raise (internal_error "Variable stored in non-existent closure")
    | Some (ctid, cid) -> fun acc -> StructGet (ctid, vid, None) :: LocalGet cid :: acc
    end
let convert_get_var (loc : locality) (vid : 'a varid) (cinfo : clos_info) : instr_conv =
  let _, vid = (vid : _ varid :> _ typ * int32) in convert_get_var' loc vid cinfo
let rec convert_block : 'a. _ -> 'a block -> _ -> _ -> _ -> 'a tinstr_conv =
  fun (type a) (tm : tmap) ((ass, e) : a block) (is_last : bool) (cinfo : clos_info) (can_jmp : (mfunid * int32) option) : a tinstr_conv ->
  let open Wasm.Instruction in
  let rec inner (ass : assign list) : a tinstr_conv = match ass with
    | [] -> convert_expr tm e is_last cinfo can_jmp
    | Assign (l, v, e) :: tl ->
        let _, v = (v : _ varid :> (_ typ * int32)) in
        let t, f = inner tl in
        let _, e = convert_expr tm e false cinfo None in
        t, fun acc -> f ((match l with
          | Global -> GlobalSet v
          | Local StorVariable -> LocalSet v
          | Local StorClosure -> raise (internal_error "unexpected assignment to closure variable")
          ) :: e acc)
  in inner ass
and convert_expr : 'a. _ -> 'a expr -> _ -> _ -> _ -> 'a tinstr_conv =
    fun (type a) (tm : tmap) (e : a expr) (is_last : bool) (cinfo : clos_info) (can_jmp : (mfunid * int32) option) : a tinstr_conv ->
  let open Wasm.Instruction in match e with
  | EConvertClosure (src, t, _) ->
      let ctid = match cinfo with None -> raise (internal_error "Closure conversion without closure info") | Some (ctid, _) -> ctid in
      t, fun acc -> RefCast Wasm.Type.(NoNull, VarHT (StatX ctid)) :: LocalGet (src :> int32) :: acc
  | EIgnore (_, e) ->
      let _, e = convert_expr tm e false cinfo None in
      TTuple NTLnil, fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: e acc
  | EConstInt i -> TInt, fun acc -> Const Wasm.Value.(I64 (I64.of_bits i)) :: acc
  | EConstBool b -> TBool, fun acc -> Const Wasm.Value.(I32 (I32.of_bits (if b then 1l else 0l))) :: acc
  | EConstFloat f -> TFloat, fun acc -> Const Wasm.Value.(F64 (F64.of_float f)) :: acc
  | EUnop (op, e) ->
      let _, arg = convert_expr tm e false cinfo None in
      convert_unop tm op arg
  | EBinop (op, e1, e2) ->
      let argt1, arg1 = convert_expr tm e1 false cinfo None in
      let argt2, arg2 = convert_expr tm e2 false cinfo None in
      convert_binop tm op arg1 argt1 arg2 argt2
  | EVariable (loc, vid) -> let t, _ = (vid : _ varid :> _ typ * _) in t, convert_get_var loc vid cinfo
  | ETuple (NTLnil, ELnil) ->
      TTuple NTLnil, fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: acc
  | ETuple (ntl, es) ->
      let tid = TMap.recid_of_type tm (TTuple ntl) in
      let es = convert_new_struct tm es tid cinfo in
      TTuple ntl, fun acc -> es acc
  | EExtract (e, (i, t, _)) ->
      let ttup, e = convert_expr tm e false cinfo None in
      let tid = TMap.recid_of_type tm ttup in
      t, fun acc -> StructGet (tid, Int32.of_int i, None) :: (e acc)
  | EVariant (tagid, targ, arg) ->
      let vctid = TMap.recid_of_type tm (TTuple (NTLcons ("0", targ, NTLnil))) in
      let arg = convert_new_struct tm (ELcons (arg, ELnil)) vctid cinfo in
      TVariant, fun acc -> StructNew (TMap.variant_tid, Explicit) :: arg (Const Wasm.Value.(I32 (I32.of_int_u (tagid :> int))) :: acc)
  | ECase (tmpvar, v, t, cs, od) ->
      let loc, vid, stv = match v with
        | EVariable (loc, vid) ->
            let _, vid = (vid : _ varid :> _ * int32) in
            loc, vid, (fun acc -> acc)
        | _ ->
            let _, tmpvar = (tmpvar : _ varid :> _ * int32) in
            let _, stv = convert_expr tm v false cinfo None in
            Local StorVariable, tmpvar, (fun acc -> LocalSet tmpvar :: stv acc)
      in
      let branches = ref (Array.make 0 None) in
      let ncases, min_id, code = List.fold_left (fun (i, min_id, code) (id, Type btyp, bid, blk) ->
        let id = (id : tagid :> int) in
        if Array.length !branches <= id then
          branches := Array.init (id + 1) (fun j -> if j < Array.length !branches then !branches.(j) else None);
        !branches.(id) <- Some i;
        let _, blk = convert_block tm blk is_last cinfo can_jmp in
        let tid = TMap.recid_of_type tm (TTuple (NTLcons ("0", btyp, NTLnil))) in
        Int32.succ i, Int.min min_id id, fun content depth ->
          Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code content (Int32.succ depth)) ::
          convert_get_var' loc vid cinfo (
          StructGet (TMap.variant_tid, 1l, None) ::
          RefCast Wasm.Type.(NoNull, (VarHT (StatX tid))) ::
          StructGet (tid, 0l, None) ::
          LocalSet (bid : mvarid :> int32) ::
          List.rev_append (blk []) [Br depth])
      ) (0l, Int.max_int, fun content _ -> content) cs in
      let branches = !branches in
      let min_id = if min_id <= 3 then 0 else min_id in
      let code =
        let table = [
          BrTable (
            List.init
              (Array.length branches - min_id)
              (fun i -> match branches.(i + min_id) with None -> ncases | Some v -> v),
            ncases)
        ] in
        let table =
          if min_id = 0 then table
          else Const Wasm.Value.(I32 (I32.of_int_u min_id)) :: Binop Wasm.Value.(I32 IntOp.Sub) :: table in
        code (convert_get_var' loc vid cinfo (
          StructGet (TMap.variant_tid, 0l, None) ::
          table)) 1l in
      let code = match od with
        | None -> Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code) :: [Unreachable]
        | Some (bid, blk) ->
          let _, blk = convert_block tm blk is_last cinfo can_jmp in
          Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code) ::
          convert_get_var' loc vid cinfo (
          LocalSet (bid :> int32) ::
          List.rev (blk []))
      in
      let tret = TMap.val_of_type tm t in
      t, fun acc -> Wasm.Instruction.Block (Wasm.Type.(ValBlockType (Some tret)), code) :: stv acc
  | EClose (f, cls) ->
      let (TFunc (targs, tret) as funt), clts, fid = (f : _ funcid :> _ * _ * int32) in
      let fctid = TMap.recid_of_type tm (TClosArg clts) in
      let new_ctid = TMap.recid_of_closed tm funt fctid in
      let gen_new_struct = convert_new_struct tm cls fctid cinfo in
      TClosed (targs, tret), fun acc -> StructNew (new_ctid, Explicit) :: gen_new_struct (RefFunc fid :: acc)
  | ECallRawHandler (fid, _, contarg, targs, args, hdlarg, tret) ->
      let fid = (fid :> int32) in
      let fcid = TMap.recid_of_type tm (TClosArg targs) in
      let args = convert_new_struct tm args fcid cinfo in
      let _, hdlarg = convert_expr tm hdlarg false cinfo None in
      let _, contarg = convert_expr tm contarg false cinfo None in
      tret, fun acc -> (if is_last then ReturnCall fid else Call fid) :: hdlarg (args (contarg acc))
  | ECallClosed (EClose (f, cls), args, _) ->
      let TFunc (_, tret), clts, fid = (f : _ funcid :> _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm args cinfo in
      let gen_new_struct = convert_new_struct tm cls fcid cinfo in
      tret, fun acc -> (if is_last then ReturnCall fid else Call fid) :: gen_new_struct (args acc)
  | ECallClosed (EVariable (loc, vid), args, _) ->
      let (TClosed (targs, tret) as t), _ = (vid : _ varid :> _ typ * int32) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_type tm (TFunc (targs, tret)) in
      let getv = convert_get_var loc vid cinfo in
      let args = convert_exprs tm args cinfo in
      tret, fun acc -> (if is_last then ReturnCallRef fid else CallRef fid) ::
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: getv (args acc))
  | ECallClosed (_f, _args, _) -> failwith "TODO: convert_expr ECallClosed non-Function and non-Variable"
  | ECond (rt, e, t, f) ->
      let _, ti = convert_block tm t is_last cinfo can_jmp in
      let _, fi = convert_block tm f is_last cinfo can_jmp in
      let _, ei = convert_expr tm e false cinfo None in
      let rt' = TMap.oval_of_type tm rt in
      rt, fun acc -> If (Wasm.Type.(ValBlockType rt'), List.rev (ti []), List.rev (fi []))
                           :: ei acc
  | EDo (eid, args) ->
      let TClosed (_, tret), eid = (eid : _ effectid :> _ * int32) in
      let args = convert_exprs tm args cinfo in
      let ret = match tret with
        | TTuple NTLnil -> fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: acc
        | TTuple tl ->
            let stid = TMap.recid_of_type tm (TTuple tl) in
            fun acc -> RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc
        | _ ->
            let stid = TMap.recid_of_type tm (TClosArg (TLcons (tret, TLnil))) in
            fun acc -> StructGet (stid, 0l, None) :: RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc in
      tret, fun acc -> ret (Suspend eid :: args acc)
  | EShallowHandle _ -> failwith "TODO: convert_expr EShallowHandle"
  | EDeepHandle (contid, contargs, hdlid, hdlargs) ->
      let TFunc (_, thdlret), thdlcl, hdlid = (hdlid : _ funcid :> _ * _ * int32) in
      let hdlcid = TMap.recid_of_type tm (TClosArg thdlcl) in
      let gen_hdl_struct = convert_new_struct tm hdlargs hdlcid cinfo in
      let TFunc (TLnil, tcret), tcontcl, contid = (contid : _ funcid :> _ * _ * int32) in
      let contcid = TMap.recid_of_type tm (TClosArg tcontcl) in
      let gen_cont_struct = convert_new_struct tm contargs contcid cinfo in
      let tcontid = TMap.recid_of_type tm (TCont tcret) in
      thdlret, fun acc -> (if is_last then ReturnCall hdlid else Call hdlid) :: gen_hdl_struct (gen_cont_struct (ContNew tcontid :: RefFunc contid :: acc))
  | ECont (loc, contid, contargs, hdlfid, hdlclloc, hdlclid) -> begin
      let TFunc (TLcons (_, TLcons (TClosArg cats, _)), tret), _thdlclid, hdlfid = (hdlfid : _ * _ * mfunid :> _ * _ * int32) in
      match can_jmp with
      | Some (refid, jmplv) when Int32.equal hdlfid (refid :> int32) ->
          let fcid = TMap.recid_of_type tm (TClosArg cats) in
          let args = convert_new_struct tm contargs fcid cinfo in
          tret, fun acc ->
            Br jmplv ::
            convert_get_var loc contid cinfo (
            args acc)
      | _ ->
          let hdlclid = (hdlclid :> int32) in
          let fcid = TMap.recid_of_type tm (TClosArg cats) in
          let args = convert_new_struct tm contargs fcid cinfo in
          tret, fun acc ->
            (if is_last then ReturnCall hdlfid else Call hdlfid) ::
            convert_get_var' hdlclloc hdlclid cinfo (
            args (
            convert_get_var loc contid cinfo acc))
    end
and convert_exprs : 'a. _ -> 'a expr_list -> _ -> instr_conv =
  fun (type a) (tm : tmap) (es : a expr_list) (cinfo : clos_info) : instr_conv ->
  match es with
  | ELnil -> fun acc -> acc
  | ELcons (ehd, etl) ->
      let _, f = convert_expr tm ehd false cinfo None in
      let f2 = convert_exprs tm etl cinfo in
      fun acc -> f2 (f acc)
and convert_new_struct : 'a. _ -> 'a expr_list -> _ -> _ -> instr_conv =
  fun (type a) tm (cls : a expr_list) fcid cinfo -> let open Wasm.Instruction in match cls with
  | ELnil -> fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
  | ELcons _ -> let f = convert_exprs tm cls cinfo in fun acc -> StructNew (fcid, Explicit) :: f acc

(* This function returns the instruction in reverse order *)
let convert_anyblock (tm : tmap) (Block (_, b) : anyblock) (is_last : bool) (cinfo : clos_info) : Wasm.Instruction.t list =
  let _, b = convert_block tm b is_last cinfo None in b []
let convert_finisher (type a b) (tm : tmap) (f : (a, b) finisher) (is_last : bool) (cinfo : clos_info) can_jmp : Wasm.Instruction.t list =
  match f with
  | FId _ -> []
  | FMap (v, _, b) ->
      let _, v = (v : _ varid :> _ * int32) in
      let _, b = convert_block tm b is_last cinfo can_jmp in
      b Wasm.Instruction.[LocalSet v]

let convert_hdl (tm : tmap) (type a b c) (f : (a, b, c) fhandler) : Wasm.fundef =
  let (tcont, contidx), contcl = (f.fh_contarg : _ varid * mvarid :> (_ * int32) * int32) in
  let tret = match f.fh_finisher with FId t -> (t : b typ) | FMap (_, t, _) -> t in
  let contid, fun_typ = TMap.recids_of_handler tm tcont tret in
  let fn_locals, clostyp = TMap.recid_of_locals tm f.fh_locals in
  let open Wasm.Instruction in
  let convert_clos, cinfo = match f.fh_closure with
    | None -> [], None
    | Some (src, dst) ->
        let _, dst = (dst : _ varid :> _ * int32) in
        let cid = (Option.value ~default:2l (Option.map (fun (_, v) -> snd (v : _ varid :> _ * int32)) f.fh_closure)) in
        [LocalSet dst; RefCast Wasm.Type.(NoNull, (VarHT (StatX clostyp))); LocalGet (src :> int32)], Some (clostyp, cid)
  in let code =
    let nblocks, handlers =
      let rec inner (hdls : _ handler list) len acc = match hdls with
        | [] -> len, acc
        | Handler (eid, _, _, _) :: tl -> inner tl (Int32.succ len) ((snd (eid : _ effectid :> _ * int32), OnLabel len) :: acc)
      in inner f.fh_handlers 0l [] in
    let code = convert_finisher tm f.fh_finisher true cinfo None in
    let code = Resume (contid, handlers) :: List.rev_append code [Return] in
    let rec do_cases nblocks (cases : (a, b) handler list) code = match cases with
      | [] -> code
      | Handler (type ea er) (eid, varc, vars, blk : (ea, er) effectid * _ * _ * _) :: tl ->
          let nblocks = Int32.pred nblocks in
          let codehdl =
            let _, b = convert_block tm blk true cinfo (Some (f.fh_id, nblocks)) in
            List.rev_append (b []) [Return] in
          let codehdl =
            let rec inner : 'r. 'r varid_list -> _ -> _ = fun (type r) (vars : r varid_list) codehdl -> match vars with
              | VLnil -> codehdl
              | VLcons (vhd, vtl) ->
                  let _, v = (vhd : _ varid :> _ * int32) in
                  inner vtl (LocalSet v :: codehdl)
            in inner vars codehdl in
          let _, varc = (varc : _ varid :> _ * int32) in
          let codehdl = LocalSet varc :: codehdl in
          let TClosed (eargs, _), _ = (eid : (ea, er) effectid :> (ea -> er) typ * int32) in
          let blkid = TMap.recid_of_handler_block tm contid eargs in
          let code = Wasm.Instruction.Block (Wasm.Type.VarBlockType blkid, code) :: codehdl in
          do_cases nblocks tl code
    in do_cases nblocks f.fh_handlers code
  in let finalblk = TMap.recid_of_handler_finish tm contid tret
  in Wasm.{
    fn_name = None;
    fn_type = fun_typ;
    fn_locals;
    fn_code = List.rev_append convert_clos [LocalGet contcl; LocalGet contidx; Loop (Wasm.Type.VarBlockType finalblk, code)];
  }

let convert_fun_aux (tm : tmap) (ft : int32) (l : anytyp_list) (f : anyblock) (init_dest : int32 option) (closid : int32 option) :
    int32 * Wasm.fundef =
  let fn_locals, clostid = TMap.recid_of_locals tm l in
  let cinfo = match closid with None -> None | Some cid -> Some (clostid, cid) in
  let code = convert_anyblock tm f (Option.is_none init_dest) cinfo in
  clostid, Wasm.{
    fn_name = (match init_dest with Some _ -> Some "main" | None -> None);
    fn_type = ft;
    fn_locals;
    fn_code = List.rev (match init_dest with Some gv -> Wasm.Instruction.GlobalSet gv :: code | None -> code);
  }

let convert_fun (tm : tmap) (f : func') : int32 * Wasm.fundef =
  let fun_typ = TMap.recid_of_typeid tm f.fun_typ in
  convert_fun_aux tm fun_typ f.fun_locals f.fun_block None (f.fun_converted_closure :> int32 option)
let convert_fun_step2 (tm : tmap) (f, clostyp : func' * int32) : Wasm.fundef option = match f.fun_export_data with
  | None -> None
  | Some name ->
      let fn_type = TMap.recid_of_exported_typeid tm f.fun_typ in
      Some Wasm.{
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
let convert_funs (tm : tmap) (fs : func list) (i : Wasm.fundef) : Wasm.fundef list =
  let [@tail_mod_cons] rec inner fs acc = match fs with
    | [] -> i :: List.filter_map (convert_fun_step2 tm) acc
    | FFunction hd :: tl -> let ctid, fhd = convert_fun tm hd in fhd :: inner tl ((hd, ctid) :: acc)
    | FHandler hd :: tl -> let fhd = convert_hdl tm hd in fhd :: inner tl acc
  in inner fs []

let generate_type_map (m : modu) : tmap = TMap.of_module m

let convert_fun_refs (tm : tmap) (fs : mtypid FunIDMap.t) : Wasm.global list =
  let convert_fun_ref (fid : mfunid) ft : Wasm.global =
    let gt = TMap.recid_of_typeid tm ft in
    Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX gt))), Wasm.Instruction.[RefFunc (fid :> int32)], None)
  in FunIDMap.fold (fun fid ft acc -> let hd = convert_fun_ref fid ft in hd :: acc) fs []

let convert_effects (tm : tmap) (es : anytyp_list EffectIDMap.t) : int32 list =
  let es = EffectIDMap.bindings es in
  let convert_effect (e : meffid * anytyp_list) : int32 =
    let _, TypeList targs = e in
    let tret = TAbsClosArg in
    let tid = TMap.recid_of_exported_type tm targs tret in
    tid
  in List.map convert_effect es

let compile (prog : Ir.program) (env : string Env.Int.t) : Wasm.module_ =
  let m = module_of_ir prog env in
  let tm = generate_type_map m in
  let init_res =
    let Block (t, _) = m.mod_block in
    let cg = convert_global tm (0, Type t, "_init_result") in
    cg in
  let frgbls = convert_fun_refs tm m.mod_needs_export in
  let globals = convert_globals tm m.mod_global_vars (init_res :: frgbls) in
  let _, init = convert_fun_aux tm TMap.init_func_type m.mod_locals m.mod_block (Some m.mod_nglobals) None in
  let funs = convert_funs tm m.mod_funs init in
  let tags = convert_effects tm m.mod_effs in
  let types = TMap.to_wasm tm in
  Wasm.{ types; globals; tags; funs; init = Some m.mod_nfuns }
