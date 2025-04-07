let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

open Wasmuir

(* TODO: use cont.bind instead of struct.new *)
(* TODO: do_deep_box and do_deep_unbox for effects *)

let unit_rtt       =  1l
let int_rtt        =  2l
let bool_rtt       =  3l
let float_rtt      =  4l
let string_rtt     =  5l
let absclosarg_rtt =  6l
let variant_rtt    =  7l
let list_rtt       =  8l
let func_rtt       =  9l
let cont_rtt       = 10l

(* TODO: make it so that TTuple NTLnil becomes nothing *)
module TMap : sig
  type t
  val empty : t
  val default_closure : int32
  val init_func_type : int32
  val rtt_array_tid : int32
  val boxed_tid : int32
  val variant_tid : int32
  val list_tid : int32
  val string_tid : int32
  
  val recid_of_rec_type : t -> Wasm.Type.rec_type -> int32
  val val_of_type : t -> 'a typ -> Wasm.Type.val_type
  val recid_of_type : t -> 'a typ -> int32
  val rec_of_functyp : t -> 'a typ_list -> 'b typ -> Wasm.Type.rec_type
  val recid_of_functyp : t -> 'a typ_list -> 'b typ -> int32
  
  val oval_of_type : t -> 'a typ -> Wasm.Type.val_type option
  val recid_of_closed : t -> 'a typ_list -> 'b typ -> int32 -> int32
  
  val recids_of_handler : t -> 'a continuation typ -> 'b typ -> int32 * int32
  val recid_of_handler_block : t -> int32 -> 'a typ_list -> int32
  val recid_of_handler_finish : t -> int32 -> 'a typ -> int32
  
  val recid_of_exported_type : t -> 'a typ_list -> 'b typ -> int32
  
  val to_wasm : t -> Wasm.Type.rec_type list
end = struct
  type t = {
    mutable cenv: Wasm.Type.rec_type TypeMap.t;
    mutable eenv: int32 TypeMap.t;
    mutable nrefs: int32;
    mutable reftyps: Wasm.Type.rec_type list;
  }
  let boxed_typ = Wasm.Type.(RecT [SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (RefT (Null, AnyHT)));
    FieldT (Cons, ValStorageT (RefT (Null, StructHT)));
  ]))])
  let variant_typ = Wasm.Type.(RecT [SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (NumT I32T));
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX 3l))));
  ]))])
  let list_typ = Wasm.Type.(RecT [SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX 3l))));
    FieldT (Cons, ValStorageT (RefT (Null, VarHT (StatX 5l))));
  ]))])
  let string_typ = Wasm.Type.(RecT [SubT (Final, [], DefArrayT (ArrayT (FieldT (Var, PackStorageT Wasm.Pack.Pack8))))])
  let empty : t =
    let reftyps = Wasm.Type.[
      (* 6 *) string_typ;
      (* 5 *) list_typ;
      (* 4 *) variant_typ;
      (* 3 *) boxed_typ;
      (* 2 *) RecT [SubT (Final, [], DefArrayT (ArrayT (FieldT (Cons, ValStorageT (RefT (Null, AnyHT))))))];
      (* 1 *) RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))];
      (* 0 *) RecT [SubT (Final, [], DefStructT (StructT []))];
    ] in {
    cenv = TypeMap.of_list [
      Type TString, string_typ;
      Type TVariant, variant_typ;
      Type TVar, boxed_typ;
    ];
    eenv = TypeMap.empty;
    nrefs = Int32.of_int (List.length reftyps);
    reftyps;
  }
  let default_closure : int32 = 0l
  let init_func_type : int32 = 1l
  let rtt_array_tid : int32 = 2l
  let boxed_tid : int32 = 3l
  let variant_tid : int32 = 4l
  let list_tid : int32 = 5l
  let string_tid : int32 = 6l
  
  let recid_of_rec_type (env : t) (t : Wasm.Type.rec_type) : int32 = match List.find_index ((=) t) env.reftyps with
    | Some tidx -> Int32.sub env.nrefs (Int32.of_int (tidx + 1))
    | None ->
        let tidx = env.nrefs in
        let nrefs = Int32.succ tidx in
        env.nrefs <- nrefs; env.reftyps <- t :: env.reftyps; tidx
  
  let rec val_of_type : type a. t -> a typ -> Wasm.Type.val_type = fun (env : t) (t : a typ) : Wasm.Type.val_type ->
    let open Wasm.Type in match t with
    | TTuple NTLnil -> NumT I32T
    | TInt -> NumT I64T
    | TBool -> NumT I32T
    | TFloat -> NumT F64T
    | TString -> RefT (NoNull, VarHT (StatX string_tid))
    | TClosed _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TAbsClosArg -> RefT (Null, StructHT)
    | TClosArg _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TCont _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TTuple _ -> RefT (NoNull, VarHT (StatX (recid_of_type env t)))
    | TVariant -> RefT (NoNull, VarHT (StatX variant_tid))
    | TList _ -> RefT (Null, VarHT (StatX list_tid))
    | TVar -> RefT (NoNull, VarHT (StatX boxed_tid))
  
  and [@tail_mod_cons] val_list_of_type_list : type a. t -> a typ_list -> Wasm.Type.val_type list =
    fun (env : t) (tl : a typ_list) : Wasm.Type.val_type list ->
    match tl with
    | TLnil -> []
    | TLcons (hd, tl) -> let hd = val_of_type env hd in hd :: val_list_of_type_list env tl
  and [@tail_mod_cons] val_list_of_named_type_list : type a. t -> a named_typ_list -> Wasm.Type.val_type list =
    fun (env : t) (tl : a named_typ_list) : Wasm.Type.val_type list -> match tl with
    | NTLnil -> []
    | NTLcons (_, hd, tl) -> let hd = val_of_type env hd in hd :: val_list_of_named_type_list env tl
  
  and rec_of_type : type a. t -> a typ -> Wasm.Type.rec_type = fun (env : t) (t : a typ) : Wasm.Type.rec_type ->
    match TypeMap.find_opt (Type t) env.cenv with
    | Some rt -> rt
    | None ->
        let newt =
          let open Wasm.Type in match t with
          | TTuple NTLnil -> failwith "TODO: TMap.rec_of_type TTuple NTLnil"
          | TInt -> failwith "TODO: TMap.rec_of_type TInt"
          | TBool -> failwith "TODO: TMap.rec_of_type TBool"
          | TFloat -> failwith "TODO: TMap.rec_of_type TFloat"
          | TString -> string_typ (* cached *)
          | TClosed (args, ret) ->
              let ftyp = recid_of_functyp env args ret in
              RecT [SubT (Final, [], DefStructT (StructT [
                FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftyp))));
                FieldT (Cons, ValStorageT (RefT (Null, StructHT)))
              ]))]
          | TAbsClosArg -> failwith "TODO: TMap.rec_of_type TAbsClosArg"
          | TClosArg clos ->
              let clos = val_list_of_type_list env clos in
              RecT [SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) clos)))]
          | TCont cret ->
              let ftyp = recid_of_functyp env TLnil cret in
              RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX ftyp))))]
          | TTuple elems ->
              let elems = val_list_of_named_type_list env elems in
              RecT [SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) elems)))]
          | TVariant -> variant_typ (* cached *)
          | TList _ -> list_typ
          | TVar -> boxed_typ (* cached *)
        in env.cenv <- TypeMap.add (Type t) newt env.cenv; newt
  
  and recid_of_type : type a. t -> a typ -> int32 = fun (env : t) (t : a typ) : int32 ->
    let t = rec_of_type env t in recid_of_rec_type env t
  
  and rec_of_functyp : type a b. t -> a typ_list -> b typ -> Wasm.Type.rec_type =
    fun (env : t) (args : a typ_list) (ret : b typ) : Wasm.Type.rec_type ->
    let args = val_list_of_type_list env args in
    let ret = val_of_type env ret in
    let open Wasm.Type in
    RecT [SubT (Final, [], DefFuncT (FuncT (args @ [RefT (Null, StructHT)], [ret])))]
  
  and recid_of_functyp : type a b. t -> a typ_list -> b typ -> int32 = fun (env : t) (args : a typ_list) (ret : b typ) : int32 ->
    let t = rec_of_functyp env args ret in recid_of_rec_type env t
  
  (* TODO: optimize ()s away? *)
  let oval_of_type (env : t) (t : 'a typ) : Wasm.Type.val_type option =
    Some (val_of_type env t)
  
  let recid_of_closed (env : t) (targs : 'a typ_list) (tret : 'b typ) (_clostid : int32) : int32 =
    recid_of_type env (TClosed (targs, tret))
  
  let recids_of_handler (env : t) (TCont cret : 'a continuation typ) (tret : 'b typ) : int32 * int32 =
    let open Wasm.Type in
    let cret = val_of_type env cret in
    let tret = val_of_type env tret in
    let rawid = recid_of_rec_type env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT)], [cret])))]) in
    let contid = recid_of_rec_type env (RecT [SubT (Final, [], DefContT (ContT (VarHT (StatX rawid))))]) in
    let hdlid = recid_of_rec_type env (RecT [SubT (Final, [], DefFuncT (FuncT (
      [RefT (NoNull, VarHT (StatX contid)); RefT (Null, StructHT); RefT (Null, StructHT)],
      [tret])))]) in
    contid, hdlid
  let recid_of_handler_block (env : t) (contid : int32) (eargs : 'a typ_list) : int32 =
    let open Wasm.Type in
    let eargs = val_list_of_type_list env eargs @ [RefT (NoNull, VarHT (StatX contid))] in
    recid_of_rec_type env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], eargs)))])
  let recid_of_handler_finish (env : t) (contid : int32) (eret : 'a typ) : int32 =
    let eret = val_of_type env eret in
    let open Wasm.Type in
    recid_of_rec_type env (RecT [SubT (Final, [], DefFuncT (FuncT ([RefT (Null, StructHT); RefT (NoNull, VarHT (StatX contid))], [eret])))])
  
  let recid_of_exported_type (env : t) (targs : 'a typ_list) (tret : 'b typ) : int32 =
    match TypeMap.find_opt (Type (TClosed (targs, tret))) env.eenv with
    | Some n -> n | None ->
        let open Wasm.Type in
        let targs' = val_list_of_type_list env targs in
        let tret' = val_of_type env tret in
        let ret = recid_of_rec_type env (RecT [SubT (Final, [], DefFuncT (FuncT (targs', [tret'])))]) in
        env.eenv <- TypeMap.add (Type (TClosed (targs, tret))) ret env.eenv;
        ret
  
  let to_wasm (env : t) : Wasm.Type.rec_type list = List.rev (env.reftyps)
end
type tmap = TMap.t

let rtt_wrapping (type a) (tm : tmap) (t : a typ) : (int32 option, (a, unit) Type.eq) Either.t =
  let convt tm t = Either.Left (Some (TMap.recid_of_type tm (TTuple (NTLcons ("1", t, NTLnil))))) in
  match t with
  | TInt -> convt tm t
  | TBool -> convt tm t
  | TFloat -> convt tm t
  | TString -> convt tm t
  | TClosed _ -> Either.Left None
  | TAbsClosArg -> convt tm t
  | TClosArg _ -> Either.Left None
  | TCont _ -> convt tm t
  | TTuple NTLnil -> convt tm t
  | TTuple _ -> Either.Left None
  | TVariant -> convt tm t
  | TList _ -> convt tm t
  | TVar -> Either.Right Type.Equal

(* The following functions build the instructions in reverse order *)
type instr_conv = Wasm.Instruction.t list -> Wasm.Instruction.t list

let rec generate_rtt : type a. a typ -> instr_conv =
  fun (t : a typ) : instr_conv ->
  let open Wasm.Instruction in match t with
  | TTuple NTLnil -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits unit_rtt)) :: acc
  | TInt -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits int_rtt)) :: acc
  | TBool -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits bool_rtt)) :: acc
  | TFloat -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits float_rtt)) :: acc
  | TString -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits string_rtt)) :: acc
  | TClosed _ -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits func_rtt)) :: acc
  | TAbsClosArg -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits absclosarg_rtt)) :: acc
  | TClosArg ts -> generate_rtt_array ts
  | TCont _ -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits cont_rtt)) :: acc
  | TTuple ts -> generate_rtt_array_named ts
  | TVariant -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits variant_rtt)) :: acc
  | TList _ -> fun acc -> RefI31 :: Const Wasm.Value.(I32 (I32.of_bits list_rtt)) :: acc
  | TVar -> failwith "TODO: generate_rtt TVar: load RTT from value"
and generate_rtt_array : type a. a typ_list -> instr_conv =
  fun (ts : a typ_list) : instr_conv -> let open Wasm.Instruction in
  let rec inner : type a. a typ_list -> int32 -> _ =
    fun ts nbefore -> match ts with
    | TLnil -> fun acc -> ArrayNewFixed (TMap.rtt_array_tid, nbefore) :: acc
    | TLcons (thd, ttl) ->
        let genhd = generate_rtt thd in
        let gentl = inner ttl (Int32.succ nbefore) in
        fun acc -> gentl (genhd acc) in
  inner ts 0l
and generate_rtt_array_named : type a. a named_typ_list -> instr_conv =
  fun (ts : a named_typ_list) : instr_conv -> let open Wasm.Instruction in
  let rec inner : type a. a named_typ_list -> int32 -> _ =
    fun ts nbefore -> match ts with
    | NTLnil -> fun acc -> ArrayNewFixed (TMap.rtt_array_tid, nbefore) :: acc
    | NTLcons (_, thd, ttl) ->
        let genhd = generate_rtt thd in
        let gentl = inner ttl (Int32.succ nbefore) in
        fun acc -> gentl (genhd acc) in
  inner ts 0l

module NewMetadata : sig
  type g
  type t
  
  val empty_global : int32 -> g
  val wasm_of_funs : g -> Wasm.fundef list
  val wasm_of_exports : g -> Wasm.global list
  
  val extend : g -> Wasm.Type.val_type list -> t
  val wasm_of_locals : tmap -> t -> Wasm.Type.val_type list
  
  val find_list_concat : tmap -> t -> int32
  val find_eq_fun : tmap -> t -> 'a typ -> int32
  
  val add_local : tmap -> t -> 'a typ -> int32
  val add_specialization : tmap -> t -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> int32
  val add_unspecialization : tmap -> t -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> 'c typ_list * 'd typ * int32
  val maybe_do_box : tmap -> t -> ('a, 'b) box -> instr_conv -> ('b typ * instr_conv, ('a, 'b) Type.eq) Either.t
  val maybe_do_unbox : tmap -> t -> ('a, 'b) box -> instr_conv -> ('a typ * instr_conv, ('a, 'b) Type.eq) Either.t
end = struct
  type g = {
    mutable nfuns: int32;
    mutable exps: (int32 * int32) list;
    mutable funs: Wasm.fundef list;
    mutable eqfuns: int32 TypeMap.t;
    mutable list_concat: int32 option;
  }
  type t = g * (Wasm.Type.val_type list * int32) ref
  
  let empty_global (nfuns : int32) : g = {
    nfuns;
    exps = [];
    funs =  [];
    eqfuns = TypeMap.empty;
    list_concat = None;
  }
  
  let wasm_of_funs ({ funs; _ } : g) : Wasm.fundef list =
    List.rev funs
  let wasm_of_exports ({ exps; _ } : g) : Wasm.global list =
    List.rev_map (fun (ftid, fid) ->
      Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX ftid)))), Wasm.Instruction.[RefFunc fid], None) exps
  
  let extend (glob : g) (base : Wasm.Type.val_type list) : t =
    let rec inner glob base acc n : t = match base with
      | [] -> glob, ref (acc, n)
      | hd :: tl -> inner glob tl (hd :: acc) (Int32.succ n)
    in inner glob base [] 0l
  let wasm_of_locals (_ : tmap) (_, { contents = (l, _) } : t) : Wasm.Type.val_type list =
    List.rev l
  
  let add_local (tm : tmap) (_, new_meta : t) (t : 'a typ) : int32 =
    let loc, i = !new_meta in
    new_meta := TMap.val_of_type tm t :: loc, Int32.succ i;
    i
  
  (* Warning: not the same ABI (the closure content is not wrapped again) *)
  let rec add_unspecialization : type a b c d. tmap -> t -> a typ_list -> b typ -> (a, c) box_list -> (b, d) box -> c typ_list * d typ * int32 =
    fun tm ({ nfuns = fid; exps = eacc; funs = facc; _ } as glob, _ as new_meta) targs tret bargs bret ->
    let clvid, fargs =
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 * b typ_list = fun targs bargs -> match targs, bargs with
        | TLnil, BLnone -> 0l, TLnil
        | TLnil, BLnil -> 0l, TLnil
        | TLcons (hd, tl), BLnone -> let n, tl = inner tl BLnone in Int32.succ n, TLcons (hd, tl)
        | TLcons (thd, tl), BLcons (bhd, btl) ->
            let n, tl = inner tl btl in
            let hd = dst_of_box thd bhd in
            Int32.succ n, TLcons (hd, tl) in
      inner targs bargs in
    let fret = dst_of_box tret bret in
    let cltid = TMap.recid_of_type tm (TClosed (targs, tret)) in
    let ftid = TMap.recid_of_functyp tm fargs fret in
    (* TODO: optimize the next two lines (caching is possible) *)
    let inner_ftid = TMap.recid_of_type tm (TClosed (targs, tret)) in
    let inner_fid = TMap.recid_of_functyp tm targs tret in
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT (StatX inner_ftid))) in
    let conv_closure = Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT (StatX cltid));
      LocalGet clvid
    ] in
    let load_args =
      let open Wasm.Instruction in
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 -> t list -> t list =
        fun t box i acc -> match t, box with
        | TLnil, _ -> acc
        | TLcons (_, ttl), BLnone -> inner ttl BLnone (Int32.succ i) (LocalGet i :: acc)
        | TLcons (_, ttl), BLcons (bhd, btl) ->
          let arg = match maybe_do_unbox tm new_meta bhd (fun acc -> LocalGet i :: acc) with
            | Either.Right Type.Equal -> LocalGet i :: acc
            | Either.Left (_, arg) -> arg acc
          in inner ttl btl (Int32.succ i) arg in
      inner targs bargs 0l conv_closure in
    let do_unbox =
      let do_call = Wasm.Instruction.(fun acc ->
          CallRef inner_fid ::
          StructGet (inner_ftid, 0l, None) :: LocalGet (Int32.succ clvid) ::
          StructGet (inner_ftid, 1l, None) :: LocalGet (Int32.succ clvid) ::
          load_args @ acc) in
      match maybe_do_box tm new_meta bret do_call with
      | Either.Right Type.Equal -> do_call []
      | Either.Left (_, conv) -> conv [] in
    let f = Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = [inner_fval];
      fn_code = List.rev do_unbox;
    } in
    let nfuns, eacc, facc = Int32.succ fid, (ftid, fid) :: eacc, f :: facc in
    glob.nfuns <- nfuns;
    glob.exps <- eacc;
    glob.funs <- facc;
    fargs, fret, fid
  
  and maybe_do_box : type a b. tmap -> t -> (a, b) box -> instr_conv -> (b typ * instr_conv, (a, b) Type.eq) Either.t =
    fun tm new_meta box get_val ->
    let open Wasm.Instruction in match box with
    | BNone -> Either.Right Type.Equal
    | BClosed (_, BLnone, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLnil, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLcons (BNone, BLnil), BNone) -> Either.Right Type.Equal
    | BClosed (TClosed (targs, tret), bargs, bret) ->
        let targs, tret, convfid = add_unspecialization tm new_meta targs tret bargs bret in
        let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
        Either.Left (TClosed (targs, tret), fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
    | BCont _ -> failwith "TODO maybe_do_box"
    | BTuple _ -> failwith "TODO maybe_do_box"
    | BBox t ->
        let rtt = generate_rtt t in
        begin match rtt_wrapping tm t with
          | Either.Right Type.Equal -> Either.Right Type.Equal
          | Either.Left None -> Either.Left (TVar, fun acc -> StructNew (TMap.boxed_tid, Explicit) :: get_val (rtt acc))
          | Either.Left (Some boxed) ->
              Either.Left (TVar, fun acc -> StructNew (TMap.boxed_tid, Explicit) :: StructNew (boxed, Explicit) :: get_val (rtt acc))
      end
  
  (* Warning: not the same ABI (the closure content is not wrapped again) *)
  and add_specialization : type a b c d. tmap -> t -> a typ_list -> b typ -> (a, c) box_list -> (b, d) box -> int32 =
    fun tm ({ nfuns = fid; exps = eacc; funs = facc; _ } as glob, _ as new_meta) targs tret bargs bret ->
    let ftid = TMap.recid_of_functyp tm targs tret in
    let clvid, fargs =
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 * b typ_list = fun targs bargs -> match targs, bargs with
        | TLnil, BLnone -> 0l, TLnil
        | TLnil, BLnil -> 0l, TLnil
        | TLcons (hd, tl), BLnone -> let n, tl = inner tl BLnone in Int32.succ n, TLcons (hd, tl)
        | TLcons (thd, tl), BLcons (bhd, btl) ->
            let n, tl = inner tl btl in
            let hd = dst_of_box thd bhd in
            Int32.succ n, TLcons (hd, tl) in
      inner targs bargs in
    let fret = dst_of_box tret bret in
    let cltid = TMap.recid_of_type tm (TClosed (fargs, fret)) in
    (* TODO: optimize the next two lines (caching is possible) *)
    let inner_ftid = TMap.recid_of_type tm (TClosed (fargs, fret)) in
    let inner_fid = TMap.recid_of_functyp tm fargs fret in
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT (StatX inner_ftid))) in
    let conv_closure = Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT (StatX cltid));
      LocalGet clvid
    ] in
    let load_args =
      let open Wasm.Instruction in
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 -> t list -> t list =
        fun t box i acc -> match t, box with
        | TLnil, _ -> acc
        | TLcons (_, ttl), BLnone -> inner ttl BLnone (Int32.succ i) (LocalGet i :: acc)
        | TLcons (_, ttl), BLcons (bhd, btl) ->
          let arg = match maybe_do_box tm new_meta bhd (fun acc -> LocalGet i :: acc) with
            | Either.Right Type.Equal -> LocalGet i :: acc
            | Either.Left (_, arg) -> arg acc
          in inner ttl btl (Int32.succ i) arg in
      inner targs bargs 0l conv_closure in
    let do_unbox =
      let do_call = Wasm.Instruction.(fun acc ->
        CallRef inner_fid ::
        StructGet (inner_ftid, 0l, None) :: LocalGet (Int32.succ clvid) ::
        StructGet (inner_ftid, 1l, None) :: LocalGet (Int32.succ clvid) ::
        load_args @ acc) in
      match maybe_do_unbox tm new_meta bret do_call with
      | Either.Right Type.Equal -> do_call []
      | Either.Left (_, conv) -> conv [] in
    let f = Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = [inner_fval];
      fn_code = List.rev do_unbox;
    } in
    let nfuns, eacc, facc = Int32.succ fid, (ftid, fid) :: eacc, f :: facc in
    glob.nfuns <- nfuns;
    glob.exps <- eacc;
    glob.funs <- facc;
    fid
  
  (* get_val is affine *)
  and maybe_do_unbox : type a b. tmap -> t -> (a, b) box -> instr_conv -> (a typ * instr_conv, (a, b) Type.eq) Either.t =
    fun tm new_meta box get_val ->
    let open Wasm.Instruction in match box with
    | BNone -> Either.Right Type.Equal
    | BClosed (_, BLnone, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLnil, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLcons (BNone, BLnil), BNone) -> Either.Right Type.Equal
    | BClosed (TClosed (targs, tret) as tsrc, bargs, bret) ->
        let convfid = add_specialization tm new_meta targs tret bargs bret in
        let new_ctid = TMap.recid_of_type tm tsrc in
        Either.Left (tsrc, fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
    | BCont _ -> failwith "TODO maybe_do_unbox BCont"
    | BTuple _ -> failwith "TODO maybe_do_unbox BTuple"
    | BBox t -> begin match rtt_wrapping tm t with
        | Either.Right Type.Equal -> Either.Right Type.Equal
        | Either.Left None -> let tid = TMap.recid_of_type tm t in Either.Left (t, fun acc ->
            RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) ::
            StructGet (TMap.boxed_tid, 1l, None) ::
            get_val
            acc)
        | Either.Left (Some tid) -> Either.Left (t, fun acc ->
            StructGet (tid, 0l, None) ::
            RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) ::
            StructGet (TMap.boxed_tid, 1l, None) ::
            get_val
            acc)
      end
  
  let find_list_concat (tm : tmap) ({ list_concat; _ } as glob, _ : t) : int32 = match list_concat with
    | Some i -> i
    | None ->
        let fid = glob.nfuns in
        let ftid = TMap.recid_of_exported_type tm (TLcons (TList TVar, TLcons (TList TVar, TLnil))) (TList TVar) in
        let f = Wasm.{
          fn_name = None; fn_type = ftid; fn_locals = []; fn_code = Instruction.[
            Block (Type.ValBlockType None, [
              LocalGet 0l;
              BrOnNull 0l;
              StructGet (TMap.list_tid, 0l, None);
              LocalGet 0l;
              RefAsNonNull;
              StructGet (TMap.list_tid, 1l, None);
              LocalGet 1l;
              Call fid;
              StructNew (TMap.list_tid, Explicit);
              Return;
            ]);
            LocalGet 1l;
          ]
        } in
        glob.nfuns <- Int32.succ fid;
        glob.funs <- f :: glob.funs;
        glob.list_concat <- Some fid;
        fid
  
  let find_eq_fun (tm : tmap) ({ nfuns; funs; eqfuns; _ } as glob, _ as new_meta : t) (t : 'a typ) : int32 =
    match TypeMap.find_opt (Type t) eqfuns with
    | Some i -> i
    | None ->
        let rec prepare : type a. a typ -> _ = fun t nfuns eqfuns ->
          let funid = nfuns in
          let nfuns = Int32.succ funid in
          let eqfuns = TypeMap.add (Type t) funid eqfuns in
          let ft = TMap.recid_of_exported_type tm (TLcons (t, TLcons (t, TLnil))) TBool in
          let nfuns, fs, eqfuns = match t with
            | TTuple NTLnil ->
                nfuns, [Wasm.{
                  fn_type = ft;
                  fn_name = None;
                  fn_locals = [];
                  fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.I32 IntOp.Eq)];
                }], eqfuns
            | TInt ->
                nfuns, [Wasm.{
                  fn_type = ft;
                  fn_name = None;
                  fn_locals = [];
                  fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.I64 IntOp.Eq)];
                }], eqfuns
            | TBool ->
                nfuns, [Wasm.{
                  fn_type = ft;
                  fn_name = None;
                  fn_locals = [];
                  fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.I32 IntOp.Eq)];
                }], eqfuns
            | TFloat ->
                nfuns, [Wasm.{
                  fn_type = ft;
                  fn_name = None;
                  fn_locals = [];
                  fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.F64 FloatOp.Eq)];
                }], eqfuns
            | TList t ->
                let nfuns, fs, eqfuns, recid = prepare t nfuns eqfuns in
                let unbox i =
                  let get_val = Wasm.Instruction.(fun acc -> StructGet (TMap.list_tid, 0l, None) :: LocalGet i :: acc) in
                  match maybe_do_unbox tm new_meta (BBox t) get_val with
                  | Either.Left (_, v) -> v
                  | Either.Right Type.Equal -> get_val in
                nfuns, Wasm.{
                  fn_type = ft;
                  fn_name = None;
                  fn_locals = Type.[RefT (NoNull, VarHT (StatX TMap.list_tid)); RefT (NoNull, VarHT (StatX TMap.list_tid))];
                  fn_code = Type.(Value.(Instruction.[
                    Block (ValBlockType None, [
                      Block (ValBlockType None, List.rev (
                        If (ValBlockType None, [
                          LocalGet 2l;
                          StructGet (TMap.list_tid, 1l, None);
                          LocalGet 3l;
                          StructGet (TMap.list_tid, 1l, None);
                          ReturnCall funid;
                        ], [
                          Const (I32 (I32.of_bits 0l));
                          Return;
                        ]) ::
                        Call recid ::
                        unbox 3l (
                        unbox 2l [
                        LocalSet 3l;
                        BrOnNull 1l;
                        LocalGet 1l;
                        LocalSet 2l;
                        BrOnNull 0l;
                        LocalGet 0l;
                        ])
                      ));
                      LocalGet 1l;
                      RefIsNull;
                      Return;
                    ]);
                    Const (I32 (I32.of_bits 0l));
                  ]));
                } :: fs, eqfuns
            | _ -> failwith "TODO NewMetadata.find_eq_fun" in
          nfuns, fs, eqfuns, funid in
        let nfuns, add_funs, eqfuns, retid = prepare t nfuns eqfuns in
        let funs = List.rev_append add_funs funs in
        glob.nfuns <- nfuns;
        glob.funs <- funs;
        glob.eqfuns <- eqfuns;
        retid
end
type new_meta = NewMetadata.t

let convert_global (tm : tmap) ((_, Type t, name) : 'a * anytyp * string) : Wasm.global =
  let init =
    let open Wasm.Instruction in match t with
    | TTuple NTLnil -> [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TInt -> [Const (Wasm.Value.(I64 (I64.of_bits 0L)))]
    | TBool -> [Const (Wasm.Value.(I32 (I32.of_bits 0l)))]
    | TFloat -> [Const (Wasm.Value.(F64 (F64.of_float 0.)))]
    | TString -> [ArrayNewFixed (TMap.string_tid, 0l)]
    | TClosed _ -> let idx = TMap.recid_of_type tm t in [StructNew (idx, Implicit)]
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: convert_global TCont"
    | TTuple _ -> failwith "TODO: convert_global TTuple"
    | TVariant -> [
          Const (Wasm.Value.(I32 (I32.of_bits 0l)));
          StructNew (TMap.boxed_tid, Implicit);
          StructNew (TMap.variant_tid, Explicit)
        ]
    | TList _ -> [RefNull Wasm.Type.(VarHT (StatX TMap.list_tid))]
    | TVar -> raise (internal_error "Unexpected global of IR type Var") in
  let t = TMap.val_of_type tm t in
  Wasm.(Type.(GlobalT (Var, t)), init, Some name)
let convert_globals (tm : tmap) (gs : (mvarid * anytyp * string) list) (tl : Wasm.global list) : Wasm.global list =
  let [@tail_mod_cons] rec inner gs = match gs with
    | [] -> tl
    | hd :: tl -> let hd = convert_global tm hd in hd :: inner tl
  in let ret = inner gs in ret

(* v is affine *)
let do_box (type a b) (tm : tmap) (new_meta : new_meta) (box : (a, b) box) (v : instr_conv) : instr_conv =
  match NewMetadata.maybe_do_box tm new_meta box v with
  | Either.Right Type.Equal -> v
  | Either.Left (_, v) -> v

(* get_val is affine *)
let do_unbox (type a b) (tm : tmap) (new_meta : new_meta) (box : (a, b) box) (get_val : instr_conv) : instr_conv =
  match NewMetadata.maybe_do_unbox tm new_meta box get_val with
  | Either.Left (_, v) -> v
  | Either.Right Type.Equal -> get_val

let convert_unop (type a b) (_ : tmap) (_ : new_meta) (op : (a, b) unop) (arg : instr_conv) : instr_conv =
  let open Wasm.Instruction in match op with
    | UONegI -> fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg (Const Wasm.Value.(I64 (I64.of_bits 0L)) :: acc)
    | UONegF -> fun acc -> Unop (Wasm.Value.F64 FloatOp.Neg) :: arg acc
let convert_binop (type a b c) (tm : tmap) (new_meta : new_meta) (op : (a, b, c) binop) (arg1 : instr_conv) (arg2 : instr_conv) : instr_conv =
  let open Wasm.Instruction in match op with
  | BOAddI -> fun acc -> Binop (Wasm.Value.I64 IntOp.Add) :: arg2 (arg1 acc)
  | BOAddF -> fun acc -> Binop (Wasm.Value.F64 FloatOp.Add) :: arg2 (arg1 acc)
  | BOSubI -> fun acc -> Binop (Wasm.Value.I64 IntOp.Sub) :: arg2 (arg1 acc)
  | BOSubF -> fun acc -> Binop (Wasm.Value.F64 FloatOp.Sub) :: arg2 (arg1 acc)
  | BOMulI -> fun acc -> Binop (Wasm.Value.I64 IntOp.Mul) :: arg2 (arg1 acc)
  | BOMulF -> fun acc -> Binop (Wasm.Value.F64 FloatOp.Mul) :: arg2 (arg1 acc)
  | BODivI -> fun acc -> Binop (Wasm.Value.I64 IntOp.DivS) :: arg2 (arg1 acc)
  | BODivF -> fun acc -> Binop (Wasm.Value.F64 FloatOp.Div) :: arg2 (arg1 acc)
  | BORemI -> fun acc -> Binop (Wasm.Value.I64 IntOp.RemS) :: arg2 (arg1 acc)
  | BOEq t -> begin let open Wasm.Value in
      match (match t with
        | TTuple NTLnil -> Either.Left (I32 IntOp.Eq)
        | TInt -> Either.Left (I64 IntOp.Eq)
        | TBool -> Either.Left (I32 IntOp.Eq)
        | TFloat -> Either.Left (F64 FloatOp.Eq)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm new_meta t in
            Either.Right (fun arg1 arg2 -> fun acc -> Call fid :: arg2 (arg1 acc)))
      with
      | Either.Left op -> fun acc -> Relop op :: arg2 (arg1 acc)
      | Either.Right f -> f arg1 arg2
    end
  | BONe t -> begin let open Wasm.Value in
      match (match t with
        | TTuple NTLnil -> Either.Left (I32 IntOp.Ne)
        | TInt -> Either.Left (I64 IntOp.Ne)
        | TBool -> Either.Left (I32 IntOp.Ne)
        | TFloat -> Either.Left (F64 FloatOp.Ne)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm new_meta t in
            Either.Right (fun arg1 arg2 -> fun acc -> Testop (I32 IntOp.Eqz) :: Call fid :: arg2 (arg1 acc)))
      with
      | Either.Left op -> fun acc -> Relop op :: arg2 (arg1 acc)
      | Either.Right f -> f arg1 arg2
    end
  | BOLe t -> begin let open Wasm.Value in
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LeS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Le on non-integer")
    end
  | BOLt t -> begin let open Wasm.Value in
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LtS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Lt on non-integer")
    end
  | BOGe t -> begin let open Wasm.Value in
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.GeS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Ge on non-integer")
    end
  | BOGt t -> begin let open Wasm.Value in
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.GtS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Gt on non-integer")
    end
  | BOConcat ->
      let tmparg1 = NewMetadata.add_local tm new_meta TString in
      let tmparg2 = NewMetadata.add_local tm new_meta TString in
      let tmpret = NewMetadata.add_local tm new_meta TString in
      fun acc ->
        LocalGet tmpret ::
        ArrayCopy (TMap.string_tid, TMap.string_tid) ::
        ArrayLen :: LocalGet tmparg2 ::
        Const Wasm.Value.(I32 (I32.of_bits 0l)) ::
        LocalGet tmparg2 ::
        ArrayLen :: LocalGet tmparg1 ::
        LocalGet tmpret ::
        ArrayCopy (TMap.string_tid, TMap.string_tid) ::
        ArrayLen :: LocalGet tmparg1 ::
        Const Wasm.Value.(I32 (I32.of_bits 0l)) ::
        LocalGet tmparg1 ::
        Const Wasm.Value.(I32 (I32.of_bits 0l)) ::
        LocalGet tmpret ::
        LocalSet tmpret ::
        ArrayNew (TMap.string_tid, Implicit) ::
        Binop (Wasm.Value.I32 IntOp.Add) ::
        ArrayLen :: LocalGet tmparg2 ::
        ArrayLen :: LocalGet tmparg1 ::
        LocalSet tmparg2 :: arg2 (LocalSet tmparg1 :: arg1 acc)
  | BOCons t ->
      let arg1 = do_box tm new_meta (BBox t) arg1 in
      fun acc -> StructNew (TMap.list_tid, Explicit) :: arg2 (arg1 acc)
  | BOConcatList _ ->
      let fid = NewMetadata.find_list_concat tm new_meta in
      fun acc -> Call fid :: arg2 (arg1 acc)

type last_info = (mfunid * int32) option option
type clos_info = (int32 * mvarid) option (* Closure type ID, closure ID *)
let convert_get_var' (loc : locality) (vid : int32) (cinfo : clos_info) : instr_conv =
  let open Wasm.Instruction in match loc with
  | Global -> fun acc -> GlobalGet vid :: acc
  | Local StorVariable -> fun acc -> LocalGet vid :: acc
  | Local StorClosure -> begin match cinfo with
    | None -> raise (internal_error "Variable stored in non-existent closure")
    | Some (ctid, cid) -> fun acc -> StructGet (ctid, vid, None) :: LocalGet (cid :> int32) :: acc
    end
let convert_get_var (loc : locality) (vid : 'a varid) (cinfo : clos_info) : instr_conv =
  let _, vid = (vid : _ varid :> _ typ * int32) in convert_get_var' loc vid cinfo
let rec convert_block : type a b. _ -> _ -> a block -> (a, b) box -> _ =
  fun (tm : tmap) (new_meta : new_meta) ((ass, e) : a block) box (is_last : last_info) (cinfo : clos_info) : instr_conv ->
  let open Wasm.Instruction in
  let rec inner (ass : assign list) : instr_conv = match ass with
    | [] -> convert_expr tm new_meta e box is_last cinfo
    | Assign (l, v, e) :: tl ->
        let _, v = (v : _ varid :> _ typ * int32) in
        let f = inner tl in
        let e = convert_expr tm new_meta e BNone None cinfo in
        fun acc -> f ((match l with
          | Global -> GlobalSet v
          | Local StorVariable -> LocalSet v
          | Local StorClosure -> raise (internal_error "unexpected assignment to closure variable")
          ) :: e acc)
  in inner ass
and convert_expr : type a b. _ -> _ -> a expr -> (a, b) box -> _ =
  fun (tm : tmap) (new_meta : new_meta) (e : a expr) (box : (a, b) box) (is_last : last_info) (cinfo : clos_info) : instr_conv ->
  let can_early_ret = (match box with BNone -> true | _ -> false) && (Option.is_some is_last) in
  let open Wasm.Instruction in match e with
  | EConvertClosure (src, _) ->
      let ctid = match cinfo with None -> raise (internal_error "Closure conversion without closure info") | Some (ctid, _) -> ctid in
      fun acc -> RefCast Wasm.Type.(NoNull, VarHT (StatX ctid)) :: LocalGet (src :> int32) :: acc
  | EIgnore (_, e) ->
      let e = convert_expr tm new_meta e BNone None cinfo in
      fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: e acc
  | EConstInt i -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I64 (I64.of_bits i)) :: acc)
  | EConstBool b -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I32 (I32.of_bits (if b then 1l else 0l))) :: acc)
  | EConstFloat f -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(F64 (F64.of_float f)) :: acc)
  | EConstString s -> do_box tm new_meta box (fun acc ->
        ArrayNewFixed (TMap.string_tid, Int32.of_int (String.length s)) ::
        String.fold_left (fun acc c -> Const Wasm.Value.(I32 (I32.of_int_s (Char.code c))) :: acc) acc s)
  | EUnop (op, e) ->
      let arg = convert_expr tm new_meta e BNone None cinfo in
      let v = convert_unop tm new_meta op arg in
      do_box tm new_meta box v
  | EBinop (op, e1, e2) ->
      let arg1 = convert_expr tm new_meta e1 BNone None cinfo in
      let arg2 = convert_expr tm new_meta e2 BNone None cinfo in
      let v = convert_binop tm new_meta op arg1 arg2 in
      do_box tm new_meta box v
  | EVariable (loc, vid) -> do_box tm new_meta box (convert_get_var loc vid cinfo)
  | ETuple (NTLnil, ELnil) ->
      do_box tm new_meta box (fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: acc)
  | ETuple (ntl, es) ->
      let tid = TMap.recid_of_type tm (TTuple ntl) in
      begin match box with
        | BNone | BBox _ -> do_box tm new_meta box (convert_new_struct tm new_meta es BLnone tid cinfo)
        | BTuple box -> convert_new_struct tm new_meta es box tid cinfo
      end
  | EExtract (e, (ttup, i, _)) ->
      let e = convert_expr tm new_meta e BNone None cinfo in
      let tid = TMap.recid_of_type tm (TTuple ttup) in
      do_box tm new_meta box (fun acc -> StructGet (tid, Int32.of_int i, None) :: (e acc))
  | EVariant (tagid, targ, arg) ->
      let arg = convert_expr tm new_meta arg (BBox targ) None cinfo in
      do_box tm new_meta box (fun acc -> StructNew (TMap.variant_tid, Explicit) :: arg (Const Wasm.Value.(I32 (I32.of_int_u (tagid :> int))) :: acc))
  | ECase (v, t, cs, od) ->
      let loc, vid, stv = match v with
        | EVariable (loc, vid) ->
            let _, vid = (vid : _ varid :> _ * int32) in
            loc, vid, (fun acc -> acc)
        | _ ->
            let tmpvar = NewMetadata.add_local tm new_meta TVariant in
            let stv = convert_expr tm new_meta v BNone None cinfo in
            Local StorVariable, tmpvar, (fun acc -> LocalSet tmpvar :: stv acc)
      in
      let branches = ref (Array.make 0 None) in
      let ncases, min_id, code = List.fold_left (fun (i, min_id, code) (id, Type btyp, bid, blk) ->
        let id = (id : tagid :> int) in
        if Array.length !branches <= id then
          branches := Array.init (id + 1) (fun j -> if j < Array.length !branches then !branches.(j) else None);
        !branches.(id) <- Some i;
        let blk = convert_block tm new_meta blk BNone is_last cinfo in
        let unbox = do_unbox tm new_meta (BBox btyp) (fun acc -> StructGet (TMap.variant_tid, 1l, None) :: convert_get_var' loc vid cinfo acc) in
        Int32.succ i, Int.min min_id id, fun content depth ->
          Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code content (Int32.succ depth)) ::
          List.rev_append (unbox []) (
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
          let blk = convert_block tm new_meta blk BNone is_last cinfo in
          Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code) ::
          convert_get_var' loc vid cinfo (
          LocalSet (bid :> int32) ::
          List.rev (blk []))
      in
      let tret = TMap.val_of_type tm t in
      do_box tm new_meta box (fun acc -> Wasm.Instruction.Block (Wasm.Type.(ValBlockType (Some tret)), code) :: stv acc)
  | EListNil _ -> do_box tm new_meta box (fun acc -> RefNull Wasm.Type.(VarHT (StatX TMap.list_tid)) :: acc)
  | EListHd (l, t) ->
      let l = convert_expr tm new_meta l BNone None cinfo in
      do_box tm new_meta box (do_unbox tm new_meta (BBox t) (fun acc -> StructGet (TMap.list_tid, 0l, None) :: l acc))
  | EListTl (_, l) ->
      let l = convert_expr tm new_meta l BNone None cinfo in
      do_box tm new_meta box (fun acc -> StructGet (TMap.list_tid, 1l, None) :: l acc)
  | EClose (f, bcl, cls) ->
      let targs, tret, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fctid = TMap.recid_of_type tm (TClosArg clts) in
      let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
      let gen_new_struct = convert_new_struct tm new_meta cls bcl fctid cinfo in
      do_box tm new_meta box (fun acc -> StructNew (new_ctid, Explicit) :: gen_new_struct (RefFunc fid :: acc))
  | ESpecialize (e, _, BLnone, BNone) -> begin match box with
      | BNone -> convert_expr tm new_meta e BNone is_last cinfo
      | BClosed (TClosed (targs, tret), bargs, bret) -> convert_expr tm new_meta e (BClosed (TClosed (targs, tret), bargs, bret)) is_last cinfo
      | BBox (TClosed (bargs, bret)) -> convert_expr tm new_meta e (BBox (TClosed (bargs, bret))) is_last cinfo
    end
  | ESpecialize (e, tdst, bargs, bret) ->
      do_box tm new_meta box (do_unbox tm new_meta (BClosed (tdst, bargs, bret)) (convert_expr tm new_meta e BNone None cinfo))
  | ECallRawHandler (fid, _, contarg, targ, arg, hdlarg, _) ->
      let fid = (fid :> int32) in
      let fcid = TMap.recid_of_type tm (TTuple (NTLcons ("1", targ, NTLnil))) in
      let args = convert_new_struct tm new_meta (ELcons (arg, ELnil)) BLnone fcid cinfo in
      let hdlarg = convert_expr tm new_meta hdlarg BNone None cinfo in
      let contarg = convert_expr tm new_meta contarg BNone None cinfo in
      do_box tm new_meta box
        (fun acc -> (if can_early_ret then ReturnCall fid else Call fid) :: hdlarg (args (contarg acc)))
  | ECallClosed (EClose (f, bcl, cls), args) ->
      let _, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta args BLnone cinfo in
      let gen_new_struct = convert_new_struct tm new_meta cls bcl fcid cinfo in
      do_box tm new_meta box (fun acc -> (if can_early_ret then ReturnCall fid else Call fid) :: gen_new_struct (args acc))
  | ECallClosed (ESpecialize (EClose (f, bcl, cls), _, bargs, bret), args) ->
      let _, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta args bargs cinfo in
      let gen_new_struct = convert_new_struct tm new_meta cls bcl fcid cinfo in
      let unbox = do_unbox tm new_meta bret (fun acc ->
          (if can_early_ret && (match bret with BNone -> true | _ -> false) then ReturnCall fid else Call fid) ::
          gen_new_struct (args acc)) in
      do_box tm new_meta box unbox
  | ECallClosed (EVariable (loc, vid), args) ->
      let (TClosed (targs, tret) as t), _ = (vid : _ varid :> _ typ * int32) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let getv = convert_get_var loc vid cinfo in
      let args = convert_exprs tm new_meta args BLnone cinfo in
      do_box tm new_meta box (fun acc -> (if can_early_ret then ReturnCallRef fid else CallRef fid) ::
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: getv (args acc)))
  | ECallClosed (ESpecialize (EVariable (loc, vid), _, bargs, bret), args) ->
      let (TClosed (targs, tret) as t), _ = (vid : _ varid :> _ typ * int32) in
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let getv = convert_get_var loc vid cinfo in
      let args = convert_exprs tm new_meta args bargs cinfo in
      let unbox = do_unbox tm new_meta bret
        (let test = ref false in fun acc -> if !test then failwith "TODO: convert_expr ECallClosed w/ non-simple bret"
        else (if can_early_ret then ReturnCallRef fid else CallRef fid) ::
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: getv (args acc))) in
      do_box tm new_meta box unbox
  | ECallClosed (_f, _args) -> failwith "TODO: convert_expr ECallClosed non-Function and non-Variable"
  | ECond (e, rt, t, f) ->
      let ti = convert_block tm new_meta t box is_last cinfo in
      let fi = convert_block tm new_meta f box is_last cinfo in
      let ei = convert_expr tm new_meta e BNone None cinfo in
      let rt' = TMap.oval_of_type tm rt in
      fun acc -> If (Wasm.Type.(ValBlockType rt'), List.rev (ti []), List.rev (fi []))
                           :: ei acc
  | EDo (eid, args) ->
      let _, tret, eid = (eid : _ effectid :> _ * _ * int32) in
      let args = convert_exprs tm new_meta args BLnone cinfo in
      let ret = match tret with
        | TTuple NTLnil -> fun acc -> Const Wasm.Value.(I32 (I32.of_bits 0l)) :: Drop :: acc
        | TTuple tl ->
            let stid = TMap.recid_of_type tm (TTuple tl) in
            fun acc -> RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc
        | _ ->
            let stid = TMap.recid_of_type tm (TClosArg (TLcons (tret, TLnil))) in
            fun acc -> StructGet (stid, 0l, None) :: RefCast Wasm.Type.(NoNull, (VarHT (StatX stid))) :: acc in
      do_box tm new_meta box (fun acc -> ret (Suspend eid :: args acc))
  | EShallowHandle _ -> failwith "TODO: convert_expr EShallowHandle"
  | EDeepHandle (contid, contargs, hdlid, hdlargs) ->
      let _, _, thdlcl, hdlid = (hdlid : _ funcid :> _ * _ * _ * int32) in
      let hdlcid = TMap.recid_of_type tm (TClosArg thdlcl) in
      let gen_hdl_struct = convert_new_struct tm new_meta hdlargs BLnone hdlcid cinfo in
      let _, tcret, tcontcl, contid = (contid : _ funcid :> _ * _ * _ * int32) in
      let contcid = TMap.recid_of_type tm (TClosArg tcontcl) in
      let gen_cont_struct = convert_new_struct tm new_meta contargs BLnone contcid cinfo in
      let tcontid = TMap.recid_of_type tm (TCont tcret) in
      do_box tm new_meta box (fun acc ->
        (if can_early_ret then ReturnCall hdlid else Call hdlid) ::
        gen_hdl_struct (
        gen_cont_struct (
        ContNew tcontid ::
        RefFunc contid ::
        acc)))
  | ECont (loc, contid, contarg, hdlfid, (hdlclloc, hdlclid)) -> begin
      let TLcons (_, TLcons (carg, TLnil)), _, _, hdlfid = (hdlfid : _ funcid :> _ * _ * _ * int32) in
      match is_last with
      | Some (Some (refid, jmplv)) when Int32.equal hdlfid (refid :> int32) ->
          let fcid = TMap.recid_of_type tm (TTuple (NTLcons ("1", carg, NTLnil))) in
          let args = convert_new_struct tm new_meta (ELcons (contarg, ELnil)) BLnone fcid cinfo in
          do_box tm new_meta box (fun acc ->
            Br jmplv ::
            convert_get_var loc contid cinfo (
            args acc))
      | _ ->
          let _, hdlclid = (hdlclid : _ varid :> _ * int32) in
          let fcid = TMap.recid_of_type tm (TTuple (NTLcons ("1", carg, NTLnil))) in
          let args = convert_new_struct tm new_meta (ELcons (contarg, ELnil)) BLnone fcid cinfo in
          do_box tm new_meta box (fun acc ->
            (if can_early_ret then ReturnCall hdlfid else Call hdlfid) ::
            convert_get_var' hdlclloc hdlclid cinfo (
            args (
            convert_get_var loc contid cinfo acc)))
    end
and convert_exprs : type a b. _ -> _ -> a expr_list -> (a, b) box_list -> _ =
  fun (tm : tmap) (new_meta : new_meta) (es : a expr_list) box (cinfo : clos_info) : instr_conv -> match box, es with
  | _, ELnil -> fun acc -> acc
  | BLnone, ELcons (ehd, etl) ->
      let f = convert_expr tm new_meta ehd BNone None cinfo in
      let f2 = convert_exprs tm new_meta etl BLnone cinfo in
      fun acc -> f2 (f acc)
  | BLcons (bhd, btl), ELcons (ehd, etl) ->
      let f = convert_expr tm new_meta ehd bhd None cinfo in
      let f2 = convert_exprs tm new_meta etl btl cinfo in
      fun acc -> f2 (f acc)
and convert_new_struct : type a b. _ -> _ -> a expr_list -> (a, b) box_list -> _ =
  fun (tm : tmap) (new_meta : new_meta) (cls : a expr_list) box (fcid : int32) (cinfo : clos_info) : instr_conv ->
  let open Wasm.Instruction in
  match cls with
  | ELnil -> fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
  | ELcons _ -> let f = convert_exprs tm new_meta cls box cinfo in fun acc -> StructNew (fcid, Explicit) :: f acc

(* These two functions return the instructions in reverse order *)
let convert_anyblock (tm : tmap) (new_meta : new_meta) (b : 'a block) (is_last : bool) (cinfo : clos_info) : Wasm.Instruction.t list =
  let b = convert_block tm new_meta b BNone (if is_last then Some None else None) cinfo in b []
let convert_finisher : type a b. _ -> _ -> (a, b) finisher -> _ =
  fun (tm : tmap) (new_meta : new_meta) (f : (a, b) finisher) (is_last : last_info) (cinfo : clos_info) : Wasm.Instruction.t list ->
  match f with
  | FId _ -> []
  | FMap (v, _, b) ->
      let _, v = (v : _ varid :> _ * int32) in
      let b = convert_block tm new_meta b BNone is_last cinfo in
      b Wasm.Instruction.[LocalSet v]

let convert_hdl (tm : tmap) (glob : NewMetadata.g) (type a b c) (f : (a, b, c) fhandler) : Wasm.fundef =
  let new_meta = NewMetadata.extend glob (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fh_locals) in
  let (tcont, contidx), contcl = (f.fh_contarg : _ varid * mvarid :> (_ * int32) * int32) in
  let tret = match f.fh_finisher with FId t -> (t : b typ) | FMap (_, t, _) -> t in
  let contid, fun_typ = TMap.recids_of_handler tm tcont tret in
  let open Wasm.Instruction in
  let convert_clos, cinfo = match f.fh_closure with
    | None -> [], None
    | Some (src, dst) ->
        let clt, dst = (dst : _ varid :> _ * mvarid) in
        let clostyp = TMap.recid_of_type tm clt in
        [LocalSet (dst :> int32); RefCast Wasm.Type.(NoNull, (VarHT (StatX clostyp))); LocalGet (src :> int32)], Some (clostyp, dst)
  in let code =
    let nblocks, handlers =
      let rec inner (hdls : _ handler list) len acc = match hdls with
        | [] -> len, acc
        | Handler (eid, _, _, _) :: tl -> inner tl (Int32.succ len) ((let _, _, eid = (eid : _ effectid :> _ * _ * int32) in eid, OnLabel len) :: acc)
      in inner f.fh_handlers 0l [] in
    let code = convert_finisher tm new_meta f.fh_finisher (Some None) cinfo in
    let code = Resume (contid, handlers) :: List.rev_append code [Return] in
    let rec do_cases nblocks (cases : _ handler list) code = match cases with
      | [] -> code
      | Handler (eid, varc, vars, blk) :: tl ->
          let nblocks = Int32.pred nblocks in
          let codehdl =
            let b = convert_block tm new_meta blk BNone (Some (Some (f.fh_id, nblocks))) cinfo in
            List.rev_append (b []) [Return] in
          let codehdl =
            let rec inner : type a. a varid_list -> _ = fun (vars : a varid_list) codehdl -> match vars with
              | VLnil -> codehdl
              | VLcons (vhd, vtl) ->
                  let _, v = (vhd : _ varid :> _ * int32) in
                  inner vtl (LocalSet v :: codehdl)
            in inner vars codehdl in
          let _, varc = (varc : _ varid :> _ * int32) in
          let codehdl = LocalSet varc :: codehdl in
          let eargs, _, _ = (eid : _ effectid :> _ * _ * _) in
          let blkid = TMap.recid_of_handler_block tm contid eargs in
          let code = Wasm.Instruction.Block (Wasm.Type.VarBlockType blkid, code) :: codehdl in
          do_cases nblocks tl code
    in do_cases nblocks f.fh_handlers code
  in let finalblk = TMap.recid_of_handler_finish tm contid tret
  in Wasm.{
    fn_name = None;
    fn_type = fun_typ;
    fn_locals = NewMetadata.wasm_of_locals tm new_meta;
    fn_code = List.rev_append convert_clos [LocalGet contcl; LocalGet contidx; Loop (Wasm.Type.VarBlockType finalblk, code)];
  }

let convert_fun_aux (tm : tmap) (glob : NewMetadata.g) (ft : int32) (locals : Wasm.Type.val_type list) (f : 'a block)
    (init_dest : Wasm.Instruction.t list option) (closid : (anytyp_list * mvarid) option) : int32 * Wasm.fundef =
  let new_meta = NewMetadata.extend glob locals in
  let clostid, cinfo = match closid with
    | None -> TMap.default_closure, None
    | Some (TypeList ct, cid) ->
        let ctid = TMap.recid_of_type tm (TClosArg ct) in
        ctid, Some (ctid, cid) in
  let code = convert_anyblock tm new_meta f (Option.is_none init_dest) cinfo in
  clostid, Wasm.{
    fn_name = (match init_dest with Some _ -> Some "main" | None -> None);
    fn_type = ft;
    fn_locals = NewMetadata.wasm_of_locals tm new_meta;
    fn_code = List.rev (match init_dest with Some app_code -> app_code @ code | None -> code);
  }

let convert_fun (tm : tmap) (glob : NewMetadata.g) (f : ('a, 'b) func') : int32 * Wasm.fundef =
  let fun_typ = TMap.recid_of_functyp tm f.fun_args f.fun_ret in
  convert_fun_aux tm glob fun_typ (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fun_locals) f.fun_block None f.fun_converted_closure
let convert_fun_step2 (tm : tmap) (f, clostyp : func * int32) : Wasm.fundef option = match f with FHandler _ -> None | FFunction f ->
  match f.fun_export_data with
  | None -> None
  | Some name ->
      let targs = f.fun_args in
      let tret = f.fun_ret in
      let fn_type = TMap.recid_of_exported_type tm targs tret in
      Some Wasm.{
        fn_name = Some name;
        fn_type;
        fn_locals = [];
        fn_code = List.rev Wasm.Instruction.(
          ReturnCall (f.fun_id :> int32) ::
          RefNull Wasm.Type.(VarHT (StatX clostyp)) ::
          let rec inner : type a. _ -> _ -> a typ_list -> _ = fun i acc (ls : a typ_list) -> match ls with
            | TLnil -> acc
            | TLcons (_, ls) -> inner (Int32.succ i) (LocalGet i :: acc) ls
          in inner 0l [] f.fun_args);
      }
let convert_funs (tm : tmap) (glob : NewMetadata.g) (fs : func list) (is : unit -> Wasm.fundef list) : Wasm.fundef list =
  let [@tail_mod_cons] rec inner glob fs acc = match fs with
    | [] -> is () @ List.filter_map (convert_fun_step2 tm) acc
    | FFunction hd :: tl -> let ctid, fhd = convert_fun tm glob hd in fhd :: inner glob tl ((FFunction hd, ctid) :: acc)
    | FHandler hd :: tl -> let fhd = convert_hdl tm glob hd in fhd :: inner glob tl acc
  in inner glob fs []

let generate_type_map (_ : 'a modu) : tmap = TMap.empty

let convert_fun_refs (tm : tmap) (fs : (anytyp_list * anytyp) FunIDMap.t) (init : Wasm.global list) : Wasm.global list =
  let convert_fun_ref (fid : mfunid) (TypeList targs, Type tret) : Wasm.global =
    let gt = TMap.recid_of_functyp tm targs tret in
    Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX gt))), Wasm.Instruction.[RefFunc (fid :> int32)], None)
  in FunIDMap.fold (fun fid ft acc -> let hd = convert_fun_ref fid ft in hd :: acc) fs init

let convert_effects (tm : tmap) (es : anytyp_list EffectIDMap.t) : int32 list =
  let es = EffectIDMap.bindings es in
  let convert_effect (_, TypeList targs : meffid * anytyp_list) : int32 =
    let tret = TAbsClosArg in
    let tid = TMap.recid_of_exported_type tm targs tret in
    tid
  in List.map convert_effect es

type 'a import_info = {
  impinfo_putc : 'a;
  impinfo_puts : 'a;
}
let impinfo_empty : int32 option import_info = {
  impinfo_putc = None;
  impinfo_puts = None;
}
let convert_import (tm : tmap) ((fidx, tidx, impinfo) : int32 * int32 * int32 option import_info) ((m, i) : string * string)
    : (int32 * int32 * int32 option import_info) * Wasm.import =
  let acc, desc = match m, i with
  | "wizeng", "puts" ->
      let fid = TMap.recid_of_rec_type tm Wasm.Type.(RecT [SubT (Final, [], DefFuncT (FuncT ([NumT I32T; NumT I32T], [])))]) in
      (Int32.succ fidx, tidx, { impinfo with impinfo_puts = Some fidx }), Wasm.FuncImport fid
  | "wizeng", "putc" ->
      let fid = TMap.recid_of_rec_type tm Wasm.Type.(RecT [SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [])))]) in
      (Int32.succ fidx, tidx, { impinfo with impinfo_putc = Some fidx }), Wasm.FuncImport fid
  | _ -> raise (internal_error ("Unknown import '" ^ m ^ "'.'" ^ i ^ "'"))
  in acc, Wasm.{ module_name = m; item_name = i; desc }
let finish_import_info (impinfo : int32 option import_info) : int32 import_info option = match impinfo with
  | { impinfo_putc = Some putc; impinfo_puts = Some puts; } ->
    Some { impinfo_putc = putc; impinfo_puts = puts; }
  | { impinfo_putc = None; _ }
  | { impinfo_putc = Some _; impinfo_puts = None } -> None

let generate_wizard
  = Settings.(flag ~default:true "generate_wizard"
              |> synopsis "Generate WizardEngine-style outputs"
              |> convert parse_bool
              |> sync)

let compile (prog : Ir.program) (env : string Env.Int.t) (main_typ_name : string) : Wasm.module_ =
  let gen_wizeng = Settings.get generate_wizard in
  let Wasmir.Module m = Wasmir.module_of_ir prog env gen_wizeng in
  let m = module_of_ir m in
  let tm = generate_type_map m in
  let init_res =
    let cg = convert_global tm (0, Type m.mod_main, "_init_result") in
    cg in
  let (_, _, impinfo), imports = List.fold_left_map (convert_import tm) (0l, 0l, impinfo_empty) m.mod_imports in
  let impinfo = finish_import_info impinfo in
  let init_code, extra_locals, extra_funs, nfuns =
    let mainid = m.mod_nfuns in
    let open Wasm in
    let open Value in
    let open Instruction in
    match impinfo with
    | None -> [
          GlobalSet m.mod_nglobals;
        ], [], [], Int32.succ mainid
    | Some { impinfo_putc = putc; impinfo_puts = _puts } ->
        let rec prepare : type a. a typ -> _ = fun (t : a typ) (nlocs : int32) (nfuns : int32) -> match t with
          | TTuple NTLnil ->
              Either.Left [
                Drop;
                Const (I32 (I32.of_int_s (Char.code '('))); Call putc;
                Const (I32 (I32.of_int_s (Char.code ')'))); Call putc;
              ], [], [], nfuns
          | TInt ->
              let funtid = TMap.recid_of_rec_type tm Type.(RecT [SubT (Final, [], DefFuncT (FuncT ([NumT I64T], [])))]) in
              let funid = nfuns in
              let auxfunid = Int32.succ funid in
              let nfuns = Int32.succ auxfunid in
              Either.Right funid, [], [
                { fn_name = None; fn_type = funtid; fn_locals = []; fn_code = [
                  LocalGet 0l;
                  Const (I64 (I64.of_bits 0L));
                  Relop (I64 IntOp.GeS);
                  If (Type.(ValBlockType (Some (NumT I64T))), [
                    LocalGet 0l;
                    Testop (I64 IntOp.Eqz);
                    If (Type.(ValBlockType None), [
                      Const (I32 (I32.of_int_s (Char.code '0'))); ReturnCall putc;
                    ], []);
                    LocalGet 0l;
                  ], [
                    Const (I32 (I32.of_int_s (Char.code '-')));
                    Call putc;
                    Const (I64 (I64.of_bits 0L));
                    LocalGet 0l;
                    Binop (I64 IntOp.Sub);
                  ]);
                  Call auxfunid;
                ]};
                { fn_name = None; fn_type = funtid; fn_locals = []; fn_code = [
                  LocalGet 0l;
                  Testop (I64 IntOp.Eqz);
                  BrIf 0l;
                  LocalGet 0l;
                  Const (I64 (I64.of_bits 10L));
                  Relop (I64 IntOp.LtU);
                  If (Type.(ValBlockType (Some (NumT I64T))), [
                    LocalGet 0l;
                  ], [
                    LocalGet 0l;
                    Const (I64 (I64.of_bits 10L));
                    Binop (I64 IntOp.DivU);
                    Call auxfunid;
                    LocalGet 0l;
                    Const (I64 (I64.of_bits 10L));
                    Binop (I64 IntOp.RemU);
                  ]);
                  Cvtop (I32 IntOp.WrapI64);
                  Const (I32 (I32.of_int_s (Char.code '0')));
                  Binop (I32 IntOp.Add);
                  Call putc;
                ] };
              ], nfuns
          | TBool ->
              Either.Left [
                If (Type.(ValBlockType None), [
                  Const (I32 (I32.of_int_s (Char.code 't'))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code 'r'))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code 'u'))); Call putc;
                ], [
                  Const (I32 (I32.of_int_s (Char.code 'f'))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code 'a'))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code 'l'))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code 's'))); Call putc;
                ]);
                Const (I32 (I32.of_int_s (Char.code 'e'))); Call putc;
              ], [], [], Int32.succ mainid
          | TString ->
              let funtid = TMap.recid_of_rec_type tm Type.(RecT [SubT (Final, [],
                    DefFuncT (FuncT ([RefT (NoNull, VarHT (StatX TMap.string_tid))], [])))]) in
              let funid = nfuns in
              let nfuns = Int32.succ funid in
              Either.Right funid, [], [
                { fn_name = None; fn_type = funtid; fn_locals = Type.[NumT I32T]; fn_code = [
                  Const (I32 (I32.of_int_s (Char.code '"'))); Call putc;
                  Const (I32 (I32.of_bits 0l)); LocalSet 1l;
                  Block (Type.(ValBlockType None), [ Loop (Type.(ValBlockType None), [
                    LocalGet 1l;
                    LocalGet 0l; ArrayLen;
                    Relop (I32 IntOp.GeU);
                    BrIf 1l;
                    LocalGet 0l;
                    LocalGet 1l;
                    ArrayGet (TMap.string_tid, Some Pack.ZX); Call putc;
                    LocalGet 1l; Const (I32 (I32.of_bits 1l)); Binop (I32 IntOp.Add); LocalSet 1l;
                    Br 0l;
                  ])]);
                  Const (I32 (I32.of_int_s (Char.code '"'))); Call putc;
                ]}
              ], nfuns
          | TList t ->
              let mainblocktid = TMap.recid_of_rec_type tm Type.(RecT [SubT (Final, [],
                    DefFuncT (FuncT ([RefT (Null, VarHT (StatX TMap.list_tid))], [])))]) in
              let locid = nlocs in
              let nlocs = Int32.succ locid in
              let unbox = match rtt_wrapping tm t with
                | Either.Right Stdlib.Type.Equal -> (* failwith "TODO: print variadic lists" *) fun _ -> [Unreachable]
                | Either.Left None -> let tid = TMap.recid_of_type tm t in fun acc ->
                    LocalGet locid ::
                    StructGet (TMap.list_tid, 0l, None) ::
                    StructGet (TMap.boxed_tid, 1l, None) ::
                    RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) ::
                    acc
                | Either.Left (Some tid) -> fun acc ->
                    LocalGet locid ::
                    StructGet (TMap.list_tid, 0l, None) ::
                    StructGet (TMap.boxed_tid, 1l, None) ::
                    RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) ::
                    StructGet (tid, 0l, None) ::
                    acc in
              let elem, locs, funs, nfuns = prepare t nlocs nfuns in
              let print_elem = match elem with
                | Either.Left code -> fun acc -> code @ acc
                | Either.Right fid -> fun acc -> Call fid :: acc
                in
              let locs = Type.(RefT (NoNull, VarHT (StatX TMap.list_tid))) :: locs in
              Either.Left [
                Const (I32 (I32.of_int_s (Char.code '['))); Call putc;
                Block (Type.(VarBlockType mainblocktid), [
                  BrOnNull 0l;
                  LocalSet locid;
                  Loop (Type.(ValBlockType None),
                    unbox (
                    print_elem [
                    LocalGet locid;
                    StructGet (TMap.list_tid, 1l, None);
                    BrOnNull 1l;
                    LocalSet locid;
                    Const (I32 (I32.of_int_s (Char.code ','))); Call putc;
                    Const (I32 (I32.of_int_s (Char.code ' '))); Call putc;
                    Br 0l;
                  ]));
                ]);
                Const (I32 (I32.of_int_s (Char.code ']'))); Call putc;
              ], locs, funs, nfuns
          | _ -> failwith "TODO: Irtowasm.compile.prepare for this type"
        in
        let code, add_locs, add_funs, nfuns = prepare m.mod_main (Int32.of_int (List.length m.mod_locals)) (Int32.succ mainid) in
        let code = match code with
            | Either.Left code ->
                List.rev_append code [GlobalGet m.mod_nglobals; GlobalSet m.mod_nglobals]
            | Either.Right fid ->
                [Call fid; GlobalGet m.mod_nglobals; GlobalSet m.mod_nglobals] in
        let code =
          let add_string code s =
            String.fold_left (fun acc c -> Call putc :: Const (I32 (I32.of_int_s (Char.code c))) :: acc)
              code s in
          add_string (add_string code " : ") main_typ_name in
        ReturnCall putc :: Const (I32 (I32.of_int_s (Char.code '\n'))) :: code, add_locs, add_funs, nfuns in
  let glob = NewMetadata.empty_global nfuns in
  let _, init =
    let locals =
      let [@tail_mod_cons] rec inner tm l tl = match l with
        | [] -> tl
        | Type hd :: l -> TMap.val_of_type tm hd :: inner tm l tl in
      inner tm m.mod_locals extra_locals in
    convert_fun_aux tm glob TMap.init_func_type locals m.mod_block (Some init_code) None in
  let funs = convert_funs tm glob m.mod_funs (fun () -> init :: extra_funs @ NewMetadata.wasm_of_funs glob) in
  let frgbls = convert_fun_refs tm m.mod_needs_export (NewMetadata.wasm_of_exports glob) in
  let globals = convert_globals tm m.mod_global_vars (init_res :: frgbls) in
  let tags = convert_effects tm m.mod_effs in
  let types = TMap.to_wasm tm in
  Wasm.{ types; globals; tags; imports; funs; init = if gen_wizeng then None else Some m.mod_nfuns }
