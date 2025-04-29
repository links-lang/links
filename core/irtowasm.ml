let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

open Wasmuir

(*****************************************************************************
 * ACTORS IN THE WASM BACKEND
 *
 * A process ID is represented by a structure with index [pid_tid].
 * The first element of that structure is a unique integer, used as a process
 * ID.
 * The second element is either a boxed element, or a structure with index
 * [pid_active_tid] (which cannot be directly accessed by any IR expression).
 * 
 * A process on hold (whether blocked or having yielded) is represented in a
 * structure with index [process_list_tid]. It is a pair of the process ID and
 * the continuation. Said continuation does not take anything and returns a
 * boxed value. A process that has not yet terminated has a structure with
 * index [pid_active_tid] in its process ID.
 * The boolean in that structure is true if the process is blocked and waiting
 * for mail. The second element is the mailbox for that process. The third
 * element is a list of pair of a process ID and a continuation for that
 * process.
 *
 * A process that returned does not have a corresponding process. The process
 * ID is then represented by a structure with index [pid_tid] which second
 * element is a boxed element, the return value.
 *
 * Updates to those structures are usually made in-place.
 *****************************************************************************)

(* TODO: use cont.bind instead of struct.new *)

(* TODO: make it so that TTuple TLnil becomes nothing *)
module TMap : sig
  type t
  val empty : bool -> t
  val main_func_type : int32
  val variant_tid : int32
  val list_tid : int32
  val string_tid : int32
  val boxed_int_tid : int32
  val boxed_float_tid : int32
  val pid_tid : int32
  val process_active_tid : int32
  val process_waiting_tid : int32
  val pid_active_tid : int32
  val waiting_list_tid : int32
  val process_list_tid : int32
  val pid_list_tid : int32
  
  val sched_effects : t -> int32 list
  val spawn_offset : int32
  val self_offset : int32
  val yield_offset : int32
  val wait_offset : int32
  
  val recid_of_sub_type : t -> Wasm.Type.sub_type -> int32
  val val_list_of_type_list : t -> 'a typ_list -> Wasm.Type.val_type list
  val val_of_type : t -> 'a typ -> Wasm.Type.val_type
  val recid_of_type : t -> 'a typ -> int32
  val recid_of_functyp : t -> 'a typ_list -> 'b typ -> int32
  val recid_of_cfunctyp : t -> 'b typ -> int32
  
  val oval_of_type : t -> 'a typ -> Wasm.Type.val_type option
  
  val recids_of_handler : t -> 'a continuation typ -> 'b typ -> int32 * int32
  val recid_of_handler_block : t -> int32 -> 'a typ_list -> int32
  val recid_of_handler_finish : t -> int32 -> 'a typ -> int32
  
  val recid_of_exported_type : t -> 'a typ_list -> 'b typ -> int32
  
  val to_wasm : t -> Wasm.Type.rec_type list
end = struct
  type t = {
    mutable cenv: int32 TypeMap.t;
    mutable eenv: int32 TypeMap.t;
    mutable nrefs: int32;
    mutable reftyps: Wasm.Type.rec_type list;
  }
  
  let main_func_type : int32 = 0l
  let variant_tid : int32 = 1l
  let variant_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (NumT I32T));
    FieldT (Cons, ValStorageT (RefT (Null, EqHT)));
  ])))
  let list_tid : int32 = 2l
  let list_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (RefT (Null, EqHT)));
    FieldT (Cons, ValStorageT (RefT (Null, VarHT (StatX list_tid))));
  ])))
  let string_tid : int32 = 3l
  let string_typ = Wasm.Type.(SubT (Final, [], DefArrayT (ArrayT (FieldT (Var, PackStorageT Wasm.Pack.Pack8)))))
  let boxed_int_tid : int32 = 4l
  let boxed_float_tid : int32 = 5l
  let pid_tid : int32 = 6l
  let pid_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (NumT I32T)); (* "Real" process ID *)
    FieldT (Var, ValStorageT (RefT (Null, EqHT))); (* boxed or pid_active (note: pid_active cannot be the boxed value) *)
  ])))
  let process_active_ftid : int32 = 7l
  let process_active_ftyp = Wasm.Type.(SubT (Final, [], DefFuncT (FuncT ([], [RefT (Null, EqHT)]))))
  let process_active_tid : int32 = 8l
  let process_active_typ = Wasm.Type.(SubT (Final, [], DefContT (ContT (VarHT (StatX process_active_ftid)))))
  let process_waiting_ftid : int32 = 9l
  let process_waiting_ftyp = Wasm.Type.(SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT)], [RefT (Null, EqHT)]))))
  let process_waiting_tid : int32 = 10l
  let process_waiting_typ = Wasm.Type.(SubT (Final, [], DefContT (ContT (VarHT (StatX process_waiting_ftid)))))
  let pid_active_tid : int32 = 11l
  let waiting_list_tid : int32 = 12l
  let pid_active_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Var, PackStorageT Wasm.Pack.Pack8); (* Blocked? *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX list_tid)))); (* Push mailbox (Send) *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX list_tid)))); (* Pop mailbox (recv) *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX waiting_list_tid)))); (* Waiting list *)
  ])))
  let waiting_list_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Var, ValStorageT (RefT (NoNull, VarHT (StatX pid_tid))));
    FieldT (Var, ValStorageT (RefT (NoNull, VarHT (StatX process_waiting_tid))));
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX waiting_list_tid))));
  ])))
  let process_list_tid : int32 = 13l
  let process_list_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX pid_tid))));
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX process_active_tid))));
    FieldT (Var, ValStorageT (RefT (Null, VarHT (StatX process_list_tid))));
  ])))
  let pid_list_tid : int32 = 14l
  let pid_list_typ = Wasm.Type.(SubT (Final, [], DefStructT (StructT [
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX pid_tid))));
    FieldT (Cons, ValStorageT (RefT (Null, VarHT (StatX pid_list_tid))));
  ])))
  
  let empty (has_process : bool) : t =
    let reftyps =
      if has_process then Wasm.Type.[
        (* 14 *) RecT [pid_list_typ];
        (* 13 *) RecT [process_list_typ];
        (* 11-12 *) RecT [pid_active_typ; waiting_list_typ];
        (* 10 *) RecT [process_waiting_typ];
        (* 9 *) RecT [process_waiting_ftyp];
        (* 8 *) RecT [process_active_typ];
        (* 7 *) RecT [process_active_ftyp];
        (* 6 *) RecT [pid_typ];
        (* 5 *) RecT [SubT (Final, [], DefStructT (StructT [FieldT (Cons, ValStorageT (NumT F64T))]))];
        (* 4 *) RecT [SubT (Final, [], DefStructT (StructT [FieldT (Cons, ValStorageT (NumT I64T))]))];
        (* 3 *) RecT [string_typ];
        (* 2 *) RecT [list_typ];
        (* 1 *) RecT [variant_typ];
        (* 0 *) RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))];
      ] else Wasm.Type.[
        (* 5 *) RecT [SubT (Final, [], DefStructT (StructT [FieldT (Cons, ValStorageT (NumT F64T))]))];
        (* 4 *) RecT [SubT (Final, [], DefStructT (StructT [FieldT (Cons, ValStorageT (NumT I64T))]))];
        (* 3 *) RecT [string_typ];
        (* 2 *) RecT [list_typ];
        (* 1 *) RecT [variant_typ];
        (* 0 *) RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))];
      ] in {
    cenv = TypeMap.of_list [
      Type TString, string_tid;
      Type TVariant, variant_tid;
    ];
    eenv = TypeMap.empty;
    nrefs = Int32.of_int (List.fold_left (fun acc (Wasm.Type.RecT l) -> acc + List.length l) 0 reftyps);
    reftyps;
  }
  
  let recid_of_sub_type (env : t) (t : Wasm.Type.sub_type) : int32 =
    let rec inner reftyps nnext = match reftyps with
      | Wasm.Type.RecT hd :: tl ->
          let rec inner2 reftyps nnext = match reftyps with
            | hd :: tl ->
                if t = hd then Int32.pred nnext
                else inner2 tl (Int32.pred nnext)
            | [] -> inner tl nnext in
          inner2 (List.rev hd) nnext
      | [] ->
          let tidx = env.nrefs in
          let nrefs = Int32.succ tidx in
          env.nrefs <- nrefs; env.reftyps <- Wasm.Type.RecT [t] :: env.reftyps; tidx in
    inner env.reftyps env.nrefs
  
  let rec val_of_type : type a. t -> a typ -> Wasm.Type.val_type = fun (env : t) (t : a typ) : Wasm.Type.val_type ->
    let open Wasm.Type in match t with
    | TTuple TLnil -> RefT (Null, NoneHT)
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
    | TVar -> RefT (Null, EqHT)
    | TSpawnLocation -> RefT (Null, NoneHT) (* SpawnLocation is isomorphic to an empty tuple for now *)
    | TProcess -> RefT (Null, VarHT (StatX pid_tid))
  
  and [@tail_mod_cons] val_list_of_type_list : type a. t -> a typ_list -> Wasm.Type.val_type list =
    fun (env : t) (tl : a typ_list) : Wasm.Type.val_type list ->
    match tl with
    | TLnil -> []
    | TLcons (hd, tl) -> let hd = val_of_type env hd in hd :: val_list_of_type_list env tl
  
  and recid_of_type : type a. t -> a typ -> int32 = fun (env : t) (t : a typ) : int32 ->
    match TypeMap.find_opt (Type t) env.cenv with
    | Some tid -> tid
    | None ->
        let st =
          let open Wasm.Type in match t with
          | TTuple TLnil -> failwith "TODO: TMap.recid_of_type TTuple TLnil"
          | TInt -> failwith "TODO: TMap.recid_of_type TInt"
          | TBool -> failwith "TODO: TMap.recid_of_type TBool"
          | TFloat -> failwith "TODO: TMap.recid_of_type TFloat" (* cached *)
          | TString -> string_typ (* cached *)
          | TClosed (args, ret) ->
              let ftyp = recid_of_functyp env args ret in
              SubT (Final, [], DefStructT (StructT [
                FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftyp))));
                FieldT (Cons, ValStorageT (RefT (Null, StructHT)));
              ]))
          | TAbsClosArg -> failwith "TODO: TMap.recid_of_type TAbsClosArg"
          | TClosArg clos ->
              let clos = val_list_of_type_list env clos in
              SubT (Final, [], DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) clos)))
          | TCont cret ->
              let ftyp = recid_of_cfunctyp env cret in
              SubT (Final, [], DefContT (ContT (VarHT (StatX ftyp))))
          | TTuple elems ->
              let elems = val_list_of_type_list env elems in
              SubT (Final, [], DefStructT (StructT (List.map (fun _ -> FieldT (Cons, ValStorageT (RefT (Null, EqHT)))) elems)))
          | TVariant -> variant_typ (* cached *)
          | TList _ -> list_typ
          | TVar -> failwith "TODO: TMap.recid_of_type TVar"
          | TSpawnLocation -> failwith "TODO: TMap.recid_of_type TSpawnLocation"
          | TProcess -> pid_typ in
        let tid = recid_of_sub_type env st in
        env.cenv <- TypeMap.add (Type t) tid env.cenv;
        tid
  
  and recid_of_functyp : type a b. t -> a typ_list -> b typ -> int32 = fun (env : t) (args : a typ_list) (ret : b typ) : int32 ->
    let args = val_list_of_type_list env args in
    let ret = val_of_type env ret in
    let open Wasm.Type in
    let t = SubT (Final, [], DefFuncT (FuncT (args @ [RefT (Null, StructHT)], [ret]))) in
    recid_of_sub_type env t
  
  and recid_of_cfunctyp : type b. t -> b typ -> int32 = fun (env : t) (ret : b typ) : int32 ->
    let ret = val_of_type env ret in
    let open Wasm.Type in
    let t = SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT)], [ret]))) in
    recid_of_sub_type env t
  
  (* TODO: optimize ()s away? *)
  let oval_of_type (env : t) (t : 'a typ) : Wasm.Type.val_type option =
    Some (val_of_type env t)
  
  let recids_of_handler (env : t) (TCont cret : 'a continuation typ) (tret : 'b typ) : int32 * int32 =
    let open Wasm.Type in
    let cret = val_of_type env cret in
    let tret = val_of_type env tret in
    let rawid = recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT)], [cret])))) in
    let contid = recid_of_sub_type env (SubT (Final, [], DefContT (ContT (VarHT (StatX rawid))))) in
    let hdlid = recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT (
      [RefT (NoNull, VarHT (StatX contid)); RefT (Null, EqHT); RefT (Null, StructHT)],
      [tret])))) in
    contid, hdlid
  let recid_of_handler_block (env : t) (contid : int32) (_ : 'a typ_list) : int32 =
    let open Wasm.Type in
    let eargs = [RefT (Null, EqHT); RefT (NoNull, VarHT (StatX contid))] in
    recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT); RefT (NoNull, VarHT (StatX contid))], eargs))))
  let recid_of_handler_finish (env : t) (contid : int32) (eret : 'a typ) : int32 =
    let eret = val_of_type env eret in
    let open Wasm.Type in
    recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT); RefT (NoNull, VarHT (StatX contid))], [eret]))))
  
  let recid_of_exported_type (env : t) (targs : 'a typ_list) (tret : 'b typ) : int32 =
    match TypeMap.find_opt (Type (TClosed (targs, tret))) env.eenv with
    | Some n -> n | None ->
        let open Wasm.Type in
        let targs' = val_list_of_type_list env targs in
        let tret' = val_of_type env tret in
        let ret = recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT (targs', [tret'])))) in
        env.eenv <- TypeMap.add (Type (TClosed (targs, tret))) ret env.eenv;
        ret
  
  let to_wasm (env : t) : Wasm.Type.rec_type list = List.rev (env.reftyps)
  
  let spawn_offset : int32 = 0l
  let self_offset : int32 = 1l
  let yield_offset : int32 = 2l
  let wait_offset : int32 = 3l
  let sched_effects (env : t) : int32 list = let open Wasm.Type in [
    (* spawn *) recid_of_sub_type env (SubT (Final, [], DefFuncT (
      FuncT ([RefT (NoNull, VarHT (StatX (recid_of_type env (TClosed (TLnil, TVar)))))], [RefT (NoNull, VarHT (StatX pid_tid))]))));
    (* self *) recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([], [RefT (NoNull, VarHT (StatX pid_tid))]))));
    (* yield *) recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([], []))));
    (* wait *) recid_of_sub_type env (SubT (Final, [], DefFuncT (FuncT ([RefT (NoNull, VarHT (StatX pid_active_tid))], [RefT (Null, EqHT)]))));
  ]
end
type tmap = TMap.t

(* The following functions build the instructions in reverse order *)
type instr_conv = Wasm.Instruction.t list -> Wasm.Instruction.t list

module NewMetadata : sig
  type g
  type t
  
  val g_of_t : t -> g
  
  val effect_offset : g -> int32
  
  val empty_global : nfuns:int32 -> neffects:int32 -> nglob:int32 -> g
  val wasm_of_funs : g -> Wasm.fundef list
  val wasm_of_exports : g -> Wasm.global list
  
  val extend : g -> int32 -> Wasm.Type.val_type list -> t
  val wasm_of_locals : tmap -> t -> Wasm.Type.val_type list
  
  val find_list_concat : tmap -> t -> int32
  val find_eq_fun : tmap -> g -> 'a typ -> int32
  
  val add_function : g -> Wasm.fundef option ref * int32
  val register_function : g -> fid:int32 -> ftid:int32 -> unit
  val add_export_function : g -> int32 -> unit
  
  val add_local : tmap -> t -> 'a typ -> int32
  val add_raw_local : t -> Wasm.Type.val_type -> int32
  
  val add_global : tmap -> g -> string option -> 'a typ -> Wasm.Instruction.t list -> int32
  val add_raw_global : g -> Wasm.global -> int32
  
  val add_specialization : tmap -> g -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> int32
  val add_unspecialization : tmap -> g -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> 'c typ_list * 'd typ * int32
  val maybe_do_box : tmap -> t -> ('a, 'b) box -> instr_conv -> (instr_conv, ('a, 'b) Type.eq) Either.t
  val do_box : tmap -> 'a typ -> instr_conv -> (instr_conv, ('a, unit) Type.eq) Either.t
  val maybe_do_unbox : tmap -> t -> ('a, 'b) box -> instr_conv -> (instr_conv, ('a, 'b) Type.eq) Either.t
  val do_unbox : tmap -> 'a typ -> instr_conv -> (instr_conv, ('a, unit) Type.eq) Either.t
end = struct
  module Int32Map = Map.Make(Int32)
  
  type g = {
    mutable nfuns: int32;
    mutable funs: Wasm.fundef option ref list;
    mutable exps: (bool * int32) Int32Map.t;
    mutable eqfuns: int32 TypeMap.t;
    mutable list_concat: int32 option;
    mutable nglob: int32;
    mutable gbls: Wasm.global list;
    neffects: int32;
  }
  type t = g * (Wasm.Type.val_type list * int32) ref
  
  let g_of_t : t -> g = fst
  
  let effect_offset ({ neffects; _ } : g) : int32 = neffects
  
  let empty_global ~(nfuns : int32) ~(neffects : int32) ~(nglob : int32) : g = {
    nfuns;
    funs = [];
    exps = Int32Map.empty;
    eqfuns = TypeMap.empty;
    list_concat = None;
    nglob;
    gbls = [];
    neffects;
  }
  
  let wasm_of_funs ({ funs; _ } : g) : Wasm.fundef list =
    List.rev_map
      (function {contents = Some f} -> f | {contents = None} -> raise (internal_error "Function was not assigned"))
      funs
  let wasm_of_exports ({ exps; gbls; _ } : g) : Wasm.global list =
    let fglobals =
      Int32Map.fold
        (fun fid (b, ftid) acc ->
          if b then (Wasm.Type.(GlobalT (Cons, RefT (NoNull, VarHT (StatX ftid)))), Wasm.Instruction.[RefFunc fid], None) :: acc
          else acc)
        exps []
        |> List.rev in
    List.rev_append gbls fglobals
  
  let extend (glob : g) (nparams : int32) (base : Wasm.Type.val_type list) : t =
    let rec inner glob base acc n : t = match base with
      | [] -> glob, ref (acc, n)
      | hd :: tl -> inner glob tl (hd :: acc) (Int32.succ n)
    in inner glob base [] nparams
  let wasm_of_locals (_ : tmap) (_, { contents = (l, _) } : t) : Wasm.Type.val_type list =
    List.rev l
  
  let add_function (glob : g) : Wasm.fundef option ref * int32 =
    let fref = ref None in
    let fid = glob.nfuns in
    glob.nfuns <- Int32.succ fid;
    glob.funs <- fref :: glob.funs;
    fref, fid
  
  let register_function (glob : g) ~(fid:int32) ~(ftid:int32) : unit =
    glob.exps <- Int32Map.update fid (function None -> Some (false, ftid) | Some (b, _) -> Some (b, ftid)) glob.exps
  
  let add_export_function (glob : g) (fid : int32) : unit =
    glob.exps <- Int32Map.update fid (function None -> Some (true, 0l) | Some (_, ftid) -> Some (true, ftid)) glob.exps
  
  let add_raw_local (_, new_meta : t) (t : Wasm.Type.val_type) : int32 =
    let loc, i = !new_meta in
    new_meta := t :: loc, Int32.succ i;
    i
  let add_local (tm : tmap) (new_meta : t) (t : 'a typ) : int32 =
    add_raw_local new_meta (TMap.val_of_type tm t)
  
  let add_raw_global (glob : g) (g : Wasm.global) : int32 =
    let i = glob.nglob in
    glob.gbls <- g :: glob.gbls;
    glob.nglob <- Int32.succ i;
    i
  let add_global (tm : tmap) (glob : g) (oname : string option) (t : 'a typ) (init : Wasm.Instruction.t list) : int32 =
    add_raw_global glob (Wasm.Type.(GlobalT (Var, TMap.val_of_type tm t)), init, oname)
  
  (* Warning: not the same ABI (the closure content is not wrapped again) *)
  (* TODO: add caching *)
  let rec add_unspecialization : type a b c d. tmap -> g -> a typ_list -> b typ -> (a, c) box_list -> (b, d) box -> c typ_list * d typ * int32 =
    fun tm glob targs tret bargs bret ->
    let fref, fid = add_function glob in
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
    let ftid = TMap.recid_of_functyp tm fargs fret in
    glob.exps <- Int32Map.add fid (false, ftid) glob.exps;
    (* TODO: optimize the next two lines (caching is possible) *)
    let inner_ftid = TMap.recid_of_type tm (TClosed (targs, tret)) in
    let inner_fid = TMap.recid_of_functyp tm targs tret in
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT (StatX inner_ftid))) in
    let new_meta = extend glob (Int32.succ clvid) [inner_fval] in
    let conv_closure = Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT (StatX inner_ftid));
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
            | Either.Left arg -> arg acc
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
      | Either.Left conv -> conv [] in
    fref := Some Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = wasm_of_locals tm new_meta;
      fn_code = List.rev do_unbox;
    };
    fargs, fret, fid
  
  (* get_val is affine and only called on the accumulator *)
  and do_box : type a. tmap -> a typ -> instr_conv -> (instr_conv, (a, unit) Type.eq) Either.t =
    fun _tm box get_val ->
    let open Wasm.Instruction in match box with
    | TVar -> Either.Right Type.Equal
    | TInt -> Either.Left (fun acc -> StructNew (TMap.boxed_int_tid, Explicit) :: get_val acc)
    | TBool -> Either.Left (fun acc -> RefI31 :: get_val acc)
    | TFloat -> Either.Left (fun acc -> StructNew (TMap.boxed_float_tid, Explicit) :: get_val acc)
    | _ -> Either.Left (get_val)
  (* get_val is affine *)
  and maybe_do_box : type a b. tmap -> t -> (a, b) box -> instr_conv -> (instr_conv, (a, b) Type.eq) Either.t =
    fun tm new_meta box get_val ->
    let open Wasm.Instruction in match box with
    | BNone -> Either.Right Type.Equal
    | BClosed (_, BLnone, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLnil, BNone) -> Either.Right Type.Equal
    | BClosed (_, BLcons (BNone, BLnil), BNone) -> Either.Right Type.Equal
    | BClosed (TClosed (targs, tret), bargs, bret) ->
        let targs, tret, convfid = add_unspecialization tm (g_of_t new_meta) targs tret bargs bret in
        add_export_function (g_of_t new_meta) convfid;
        let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
        Either.Left (fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
    | BCont _ -> failwith "TODO maybe_do_box BCont"
    | BTuple BLnone -> Either.Right Type.Equal
    | BTuple _ -> Either.Left get_val (* Tuples always have their values boxed *)
    | BBox t -> do_box tm t get_val
  
  (* Warning: not the same ABI (the closure content is not wrapped again) *)
  (* TODO: add caching *)
  and add_specialization : type a b c d. tmap -> g -> a typ_list -> b typ -> (a, c) box_list -> (b, d) box -> int32 =
    fun tm glob targs tret bargs bret ->
    let fref, fid = add_function glob in
    let ftid = TMap.recid_of_functyp tm targs tret in
    glob.exps <- Int32Map.add fid (false, ftid) glob.exps;
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
    (* TODO: optimize the next two lines (caching is possible) *)
    let inner_ftid = TMap.recid_of_type tm (TClosed (fargs, fret)) in
    let inner_fid = TMap.recid_of_functyp tm fargs fret in
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT (StatX inner_ftid))) in
    let new_meta = extend glob (Int32.succ clvid) [inner_fval] in
    let conv_closure = Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT (StatX inner_ftid));
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
            | Either.Left arg -> arg acc
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
      | Either.Left conv -> conv [] in
    fref := Some Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = wasm_of_locals tm new_meta;
      fn_code = List.rev do_unbox;
    };
    fid
  
  (* get_val is affine and only called on the accumulator *)
  and do_unbox : type a. tmap -> a typ -> instr_conv -> (instr_conv, (a, unit) Type.eq) Either.t =
    fun tm box get_val ->
    let open Wasm.Instruction in match box with
    | TVar -> Either.Right Type.Equal
    | TInt -> Either.Left (fun acc ->
        StructGet (TMap.boxed_int_tid, 0l, None) :: RefCast Wasm.Type.(NoNull, VarHT (StatX TMap.boxed_int_tid)) :: get_val acc)
    | TBool -> Either.Left (fun acc -> I31Get Wasm.Pack.ZX :: RefCast Wasm.Type.(NoNull, I31HT) :: get_val acc)
    | TFloat -> Either.Left (fun acc ->
        StructGet (TMap.boxed_float_tid, 0l, None) :: RefCast Wasm.Type.(NoNull, VarHT (StatX TMap.boxed_float_tid)) :: get_val acc)
    | (TTuple TLnil) -> Either.Left (fun acc -> RefNull Wasm.Type.NoneHT :: Drop :: get_val acc)
        (* fun acc -> RefCast Wasm.Type.(Null, NoneHT) :: get_val acc) *)
    | TList _ -> Either.Left (fun acc -> RefCast Wasm.Type.(Null, VarHT (StatX TMap.list_tid)) :: get_val acc)
    | t -> let tid = TMap.recid_of_type tm t in Either.Left (fun acc -> RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) :: get_val acc)
  (* get_val is affine *)
  and maybe_do_unbox : type a b. tmap -> t -> (a, b) box -> instr_conv -> (instr_conv, (a, b) Type.eq) Either.t =
    fun tm new_meta box get_val ->
    let open Wasm.Instruction in match box with
    | BNone -> Either.Right Type.Equal
    | BClosed (TClosed (targs, tret) as tsrc, bargs, bret) ->
        let canon_box : type a b. (a, b) box -> ((a, b) box, (a, b) Type.eq) Either.t = function
          | BNone -> Either.Right Type.Equal
          | BBox TVar -> Either.Right Type.Equal
          | b -> Either.Left b in
        let rec canon_box_list : type a b. (a, b) box_list -> ((a, b) box_list, (a, b) Type.eq) Either.t = function
          | BLnone -> Either.Right Type.Equal
          | BLnil -> Either.Right Type.Equal
          | BLcons (bhd, btl) -> begin match canon_box bhd, canon_box_list btl with
              | Either.Left bhd, Either.Left btl -> Either.Left (BLcons (bhd, btl))
              | Either.Left bhd, Either.Right Type.Equal -> Either.Left (BLcons (bhd, BLnone))
              | Either.Right Type.Equal, Either.Left btl -> Either.Left (BLcons (BNone, btl))
              | Either.Right Type.Equal, Either.Right Type.Equal -> Either.Right Type.Equal
            end in
        begin match canon_box_list bargs, canon_box bret with
        | Either.Right Type.Equal, Either.Right Type.Equal -> Either.Right Type.Equal
        | Either.Left bargs, Either.Right Type.Equal ->
            let convfid = add_specialization tm (g_of_t new_meta) targs tret bargs BNone in
            add_export_function (g_of_t new_meta) convfid;
            let new_ctid = TMap.recid_of_type tm tsrc in
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
        | Either.Right Type.Equal, Either.Left bret ->
            let convfid = add_specialization tm (g_of_t new_meta) targs tret BLnone bret in
            add_export_function (g_of_t new_meta) convfid;
            let new_ctid = TMap.recid_of_type tm tsrc in
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
        | Either.Left bargs, Either.Left bret ->
            let convfid = add_specialization tm (g_of_t new_meta) targs tret bargs bret in
            add_export_function (g_of_t new_meta) convfid;
            let new_ctid = TMap.recid_of_type tm tsrc in
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) :: get_val (RefFunc convfid :: acc))
      end
    | BCont _ -> failwith "TODO maybe_do_unbox BCont"
    | BTuple BLnone -> Either.Right Type.Equal
    | BTuple _ -> Either.Left get_val (* Tuples always have their values boxed *)
    | BBox t -> do_unbox tm t get_val
  
  let find_list_concat (tm : tmap) (glob, _ : t) : int32 = match glob.list_concat with
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
        glob.funs <- ref (Some f) :: glob.funs;
        glob.list_concat <- Some fid;
        fid
  
  let rec find_eq_fun : type a. tmap -> g -> a typ -> int32 = fun tm glob t ->
    match TypeMap.find_opt (Type t) glob.eqfuns with
    | Some i -> i
    | None ->
        let fref, funid = add_function glob in
        let eqfuns = TypeMap.add (Type t) funid glob.eqfuns in
        glob.eqfuns <- eqfuns;
        let ft = TMap.recid_of_exported_type tm (TLcons (t, TLcons (t, TLnil))) TBool in
        register_function glob ~fid:funid ~ftid:ft;
        let () = match t with
          | TTuple TLnil ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [];
                fn_code = Instruction.[Const Value.(I32 I32.one)];
              }
          | TInt ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [];
                fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.I64 IntOp.Eq)];
              }
          | TBool ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [];
                fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.I32 IntOp.Eq)];
              }
          | TFloat ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [];
                fn_code = Instruction.[LocalGet 0l; LocalGet 1l; Relop (Value.F64 FloatOp.Eq)];
              }
          | TList t ->
              let recid = find_eq_fun tm glob t in
              let new_meta = extend glob 3l Wasm.Type.[RefT (NoNull, VarHT (StatX TMap.list_tid)); RefT (NoNull, VarHT (StatX TMap.list_tid))] in
              let unbox i =
                let get_val = Wasm.Instruction.(fun acc -> StructGet (TMap.list_tid, 0l, None) :: LocalGet i :: acc) in
                match maybe_do_unbox tm new_meta (BBox t) get_val with
                | Either.Left v -> v
                | Either.Right Type.Equal -> get_val in
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = wasm_of_locals tm new_meta;
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
                        Const (I32 I32.zero);
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
                  Const (I32 I32.zero);
                ]));
              }
          | _ -> failwith "TODO NewMetadata.find_eq_fun" in
        funid
end
type new_meta = NewMetadata.t

let convert_global (tm : tmap) ((_, Type t, name) : 'a * anytyp * string option) : Wasm.global =
  let init =
    let open Wasm.Instruction in match t with
    | TTuple TLnil -> [RefNull Wasm.Type.NoneHT]
    | TInt -> [Const (Wasm.Value.(I64 I64.zero))]
    | TBool -> [Const (Wasm.Value.(I32 I32.zero))]
    | TFloat -> [Const (Wasm.Value.(F64 (F64.of_float 0.)))]
    | TString -> [ArrayNewFixed (TMap.string_tid, 0l)]
    | TClosed _ -> let idx = TMap.recid_of_type tm t in [RefNull Wasm.Type.(VarHT (StatX idx))]
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: convert_global TCont"
    | TTuple _ -> let idx = TMap.recid_of_type tm t in [StructNew (idx, Implicit)]
    | TVariant -> [
          Const (Wasm.Value.(I32 I32.zero));
          RefNull Wasm.Type.NoneHT;
          StructNew (TMap.variant_tid, Explicit)
        ]
    | TList _ -> [RefNull Wasm.Type.(VarHT (StatX TMap.list_tid))]
    | TVar -> raise (internal_error "Unexpected global of IR type Var")
    | TSpawnLocation -> [RefNull Wasm.Type.NoneHT]
    | TProcess -> [StructNew (TMap.pid_tid, Implicit)] in
  let t = match t with
    | TClosed _ -> begin let open Wasm.Type in match TMap.val_of_type tm t with
        | RefT (NoNull, ht) -> RefT (Null, ht)
        | _ -> raise (internal_error "Unexpected val_of_type of TClosed")
      end
    | _ -> TMap.val_of_type tm t in
  Wasm.(Type.(GlobalT (Var, t)), init, name)
let convert_globals (tm : tmap) (gs : (mvarid * anytyp * string option) list) (tl : Wasm.global list) : Wasm.global list =
  let [@tail_mod_cons] rec inner gs = match gs with
    | [] -> tl
    | hd :: tl -> let hd = convert_global tm hd in hd :: inner tl
  in let ret = inner gs in ret

(* get_val is affine *)
let do_box (type a b) (tm : tmap) (new_meta : new_meta) (box : (a, b) box) (get_val : instr_conv) : instr_conv =
  match NewMetadata.maybe_do_box tm new_meta box get_val with
  | Either.Right Type.Equal -> get_val
  | Either.Left v -> v

(* get_val is affine *)
(* If box is BBox _, get_val is called on the accumulator *)
let do_unbox (type a b) (tm : tmap) (new_meta : new_meta) (box : (a, b) box) (get_val : instr_conv) : instr_conv =
  match NewMetadata.maybe_do_unbox tm new_meta box get_val with
  | Either.Left v -> v
  | Either.Right Type.Equal -> get_val

let convert_unop (type a b) (_ : tmap) (_ : new_meta) (op : (a, b) unop) (arg : instr_conv) : instr_conv =
  let open Wasm in let open Instruction in let open Value in match op with
    | UONot -> fun acc -> Testop (I32 IntOp.Eqz) :: arg acc
    | UONegI -> fun acc -> Binop (I64 IntOp.Sub) :: arg (Const (I64 I64.zero) :: acc)
    | UONegF -> fun acc -> Unop (F64 FloatOp.Neg) :: arg acc
let convert_binop (type a b c) (tm : tmap) (new_meta : new_meta) (op : (a, b, c) binop) (arg1 : instr_conv) (arg2 : instr_conv) : instr_conv =
  let open Wasm in let open Instruction in let open Value in match op with
  | BOAddI -> fun acc -> Binop (I64   IntOp.Add)  :: arg2 (arg1 acc)
  | BOAddF -> fun acc -> Binop (F64 FloatOp.Add)  :: arg2 (arg1 acc)
  | BOSubI -> fun acc -> Binop (I64   IntOp.Sub)  :: arg2 (arg1 acc)
  | BOSubF -> fun acc -> Binop (F64 FloatOp.Sub)  :: arg2 (arg1 acc)
  | BOMulI -> fun acc -> Binop (I64   IntOp.Mul)  :: arg2 (arg1 acc)
  | BOMulF -> fun acc -> Binop (F64 FloatOp.Mul)  :: arg2 (arg1 acc)
  | BODivI -> fun acc -> Binop (I64   IntOp.DivS) :: arg2 (arg1 acc)
  | BODivF -> fun acc -> Binop (F64 FloatOp.Div)  :: arg2 (arg1 acc)
  | BORemI -> fun acc -> Binop (I64   IntOp.RemS) :: arg2 (arg1 acc)
  | BOEq t -> begin match t with
        | TTuple TLnil -> fun acc -> Const (I32 I32.one) :: Drop :: arg2 (Drop :: arg1 acc)
        | TInt -> fun acc -> Relop (I64 IntOp.Eq) :: arg2 (arg1 acc)
        | TBool -> fun acc -> Relop (I32 IntOp.Eq) :: arg2 (arg1 acc)
        | TFloat -> fun acc -> Relop (F64 FloatOp.Eq) :: arg2 (arg1 acc)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm (NewMetadata.g_of_t new_meta) t in
            fun acc -> Call fid :: arg2 (arg1 acc)
    end
  | BONe t -> begin match t with
        | TTuple TLnil -> fun acc -> Const (I32 I32.zero) :: Drop :: arg2 (Drop :: arg1 acc)
        | TInt -> fun acc -> Relop (I64 IntOp.Ne) :: arg2 (arg1 acc)
        | TBool -> fun acc -> Relop (I32 IntOp.Ne) :: arg2 (arg1 acc)
        | TFloat -> fun acc -> Relop (F64 FloatOp.Ne) :: arg2 (arg1 acc)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm (NewMetadata.g_of_t new_meta) t in
            fun acc -> Testop (I32 IntOp.Eqz) :: Call fid :: arg2 (arg1 acc)
    end
  | BOLe t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LeS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Le on non-integer")
    end
  | BOLt t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LtS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Lt on non-integer")
    end
  | BOGe t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.GeS) :: arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Ge on non-integer")
    end
  | BOGt t -> begin
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
        Const (I32 I32.zero) ::
        LocalGet tmparg2 ::
        ArrayLen :: LocalGet tmparg1 ::
        LocalGet tmpret ::
        ArrayCopy (TMap.string_tid, TMap.string_tid) ::
        ArrayLen :: LocalGet tmparg1 ::
        Const (I32 I32.zero) ::
        LocalGet tmparg1 ::
        Const (I32 I32.zero) ::
        LocalTee tmpret ::
        ArrayNew (TMap.string_tid, Implicit) ::
        Binop (I32 IntOp.Add) ::
        ArrayLen :: LocalTee tmparg2 :: arg2 (
        ArrayLen :: LocalTee tmparg1 :: arg1 acc)
  | BOCons t ->
      let arg1 = do_box tm new_meta (BBox t) arg1 in
      fun acc -> StructNew (TMap.list_tid, Explicit) :: arg2 (arg1 acc)
  | BOConcatList _ ->
      let fid = NewMetadata.find_list_concat tm new_meta in
      fun acc -> Call fid :: arg2 (arg1 acc)

type 'a anybox_list = AnyBoxList : ('a, 'b) box_list -> 'a anybox_list

let optimize_size
  = Settings.(flag ~default:true "optimize_size"
              |> synopsis "Generate a smaller output, at the cost of runtime speed"
              |> convert parse_bool
              |> sync)

type procinfo = (int32 * int32 * int32 option ref) option (* global counter variable ID, angel threads list variable ID *)
let gbl_count_reset = 100L (* Yield every [gbl_count_reset] calls *)
let generate_real_yield (glob : NewMetadata.g) : instr_conv =
  let open Wasm in let open Instruction in fun acc ->
    Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset) :: acc
let generate_yield (glob : NewMetadata.g) (procinfo : procinfo) : instr_conv = match procinfo with
  | None -> Fun.id
  | Some (procinfo, _, yield_fun) -> let open Wasm in let open Value in let open Type in let open Instruction in
      let do_check_yield = fun acc ->
        If (ValBlockType None,
          List.rev (generate_real_yield glob [])
        , [
          GlobalGet procinfo;
          Const (I64 I64.one);
          Binop (I64 IntOp.Sub);
          GlobalSet procinfo;
        ]) ::
        Testop (I64 IntOp.Eqz) ::
        GlobalGet procinfo ::
        acc in
      if Settings.get optimize_size then
        let yield_fun = match !yield_fun with
          | None ->
              let fref, fid = NewMetadata.add_function glob in
              yield_fun := Some fid;
              let ftid = TMap.main_func_type in
              fref := Some {
                fn_name = None;
                fn_type = ftid;
                fn_locals = [];
                fn_code = List.rev (do_check_yield [])
              };
              NewMetadata.register_function glob ~fid ~ftid;
              fid
          | Some fid -> fid in
        fun acc -> Call yield_fun :: acc
      else do_check_yield

type last_info = (mfunid * int32) option
let incr_depth_n (is_last : last_info) (depth : int32) : last_info = match is_last with
  | None -> is_last
  | Some (refid, jmplv) -> Some (refid, Int32.add jmplv depth)
let incr_depth (is_last : last_info) : last_info = incr_depth_n is_last 1l
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
let rec convert_block : type a b. _ -> _ -> _ -> a block -> (a, b) box -> _ -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      ((ass, e) : a block) (box : (a, b) box) (is_last : last_info) (cinfo : clos_info) ->
  let open Wasm.Instruction in
  let rec inner (ass : assign list) (facc : instr_conv) : instr_conv = match ass with
    | [] -> let e = convert_expr tm new_meta procinfo e box is_last cinfo in fun acc -> e (facc acc)
    | Assign (l, v, e) :: tl ->
        let _, v = (v : _ varid :> _ typ * int32) in
        let e = convert_expr tm new_meta procinfo e BNone None cinfo in
        inner tl (fun acc -> (match l with
          | Global -> GlobalSet v
          | Local StorVariable -> LocalSet v
          | Local StorClosure -> raise (internal_error "unexpected assignment to closure variable")
          ) :: e (facc acc))
  in inner ass Fun.id
and convert_expr : type a b. _ -> _ -> _ -> a expr -> (a, b) box -> _ -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (e : a expr) (box : (a, b) box) (is_last : last_info) (cinfo : clos_info) ->
  let can_early_ret = (match box with BNone -> true | _ -> false) && (Option.is_some is_last) in
  let open Wasm.Instruction in match e with
  | EUnreachable _ -> fun acc -> Unreachable :: acc
  | EConvertClosure (src, _) ->
      let ctid = match cinfo with None -> raise (internal_error "Closure conversion without closure info") | Some (ctid, _) -> ctid in
      fun acc -> RefCast Wasm.Type.(NoNull, VarHT (StatX ctid)) :: LocalGet (src :> int32) :: acc
  | EIgnore (_, e) ->
      let e = convert_expr tm new_meta procinfo e BNone None cinfo in
      fun acc -> RefNull Wasm.Type.NoneHT :: Drop :: e acc
  | EConstInt i -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I64 (I64.of_bits i)) :: acc)
  | EConstBool b -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I32 (if b then I32.one else I32.zero)) :: acc)
  | EConstFloat f -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(F64 (F64.of_float f)) :: acc)
  | EConstString s -> do_box tm new_meta box (fun acc ->
        ArrayNewFixed (TMap.string_tid, Int32.of_int (String.length s)) ::
        String.fold_left (fun acc c -> Const Wasm.Value.(I32 (I32.of_int_s (Char.code c))) :: acc) acc s)
  | EUnop (op, e) ->
      let arg = convert_expr tm new_meta procinfo e BNone None cinfo in
      let v = convert_unop tm new_meta op arg in
      do_box tm new_meta box v
  | EBinop (BOEq (TList _), e, EListNil _) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> RefIsNull :: arg1 acc)
  | EBinop (BOEq (TList _), EListNil _, e) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> RefIsNull :: arg1 acc)
  | EBinop (BONe (TList _), e, EListNil _) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> Testop (Wasm.Value.I32 IntOp.Eqz) :: RefIsNull :: arg1 acc)
  | EBinop (BONe (TList _), EListNil _, e) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> Testop (Wasm.Value.I32 IntOp.Eqz) :: RefIsNull :: arg1 acc)
  | EBinop (op, e1, e2) ->
      let arg1 = convert_expr tm new_meta procinfo e1 BNone None cinfo in
      let arg2 = convert_expr tm new_meta procinfo e2 BNone None cinfo in
      let v = convert_binop tm new_meta op arg1 arg2 in
      do_box tm new_meta box v
  | EVariable (loc, vid) -> do_box tm new_meta box (convert_get_var loc vid cinfo)
  | ETuple (TLnil, ELnil) ->
      do_box tm new_meta box (fun acc -> RefNull Wasm.Type.NoneHT :: acc)
  | ETuple (ts, es) ->
      let tid = TMap.recid_of_type tm (TTuple ts) in
      let AnyBoxList bcontent =
        let rec inner : type a. a typ_list -> a anybox_list = function
          | TLnil -> AnyBoxList BLnil
          | TLcons (thd, ttl) -> let AnyBoxList btl = inner ttl in AnyBoxList (BLcons (BBox thd, btl)) in
        inner ts in
      begin match box with
        | BNone | BBox _ -> do_box tm new_meta box (convert_new_struct tm new_meta procinfo es bcontent tid cinfo)
        | BTuple _ -> convert_new_struct tm new_meta procinfo es bcontent tid cinfo
      end
  | EExtract (e, (ttup, i, t)) ->
      let e = convert_expr tm new_meta procinfo e BNone None cinfo in
      let tid = TMap.recid_of_type tm (TTuple ttup) in
      do_box tm new_meta box (do_unbox tm new_meta (BBox t) (fun acc -> StructGet (tid, Int32.of_int i, None) :: (e acc)))
  | EVariant (tagid, targ, arg) ->
      let arg = convert_expr tm new_meta procinfo arg (BBox targ) None cinfo in
      do_box tm new_meta box (fun acc ->
        StructNew (TMap.variant_tid, Explicit) ::
        arg (Const Wasm.Value.(I32 (I32.of_int_u (tagid :> int))) :: acc))
  | ECase (v, t, cs, od) ->
      let loc, vid, stv = match v with
        | EVariable (loc, vid) ->
            let _, vid = (vid : _ varid :> _ * int32) in
            loc, vid, (fun acc -> acc)
        | _ ->
            let tmpvar = NewMetadata.add_local tm new_meta TVariant in
            let stv = convert_expr tm new_meta procinfo v BNone None cinfo in
            Local StorVariable, tmpvar, (fun acc -> LocalSet tmpvar :: stv acc)
      in
      let branches = ref (Array.make 0 None) in
      let ncases, min_id, code = List.fold_left (fun (i, min_id, code) (id, Type btyp, bid, blk) ->
        let id = (id : tagid :> int) in
        if Array.length !branches <= id then
          branches := Array.init (id + 1) (fun j -> if j < Array.length !branches then !branches.(j) else None);
        !branches.(id) <- Some i;
        let unbox = do_unbox tm new_meta (BBox btyp) (fun acc -> StructGet (TMap.variant_tid, 1l, None) :: convert_get_var' loc vid cinfo acc) in
        Int32.succ i, Int.min min_id id, fun content depth ->
          let blk = convert_block tm new_meta procinfo blk BNone (incr_depth_n is_last depth) cinfo in
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
          let blk = convert_block tm new_meta procinfo blk BNone is_last cinfo in
          Wasm.Instruction.Block (Wasm.Type.(ValBlockType None), code) ::
          convert_get_var' loc vid cinfo (
          LocalSet (bid :> int32) ::
          List.rev (blk []))
      in
      let tret = TMap.val_of_type tm t in
      do_box tm new_meta box (fun acc -> Wasm.Instruction.Block (Wasm.Type.(ValBlockType (Some tret)), code) :: stv acc)
  | EListNil _ -> do_box tm new_meta box (fun acc -> RefNull Wasm.Type.(VarHT (StatX TMap.list_tid)) :: acc)
  | EListHd (l, t) ->
      let l = convert_expr tm new_meta procinfo l BNone None cinfo in
      do_box tm new_meta box (do_unbox tm new_meta (BBox t) (fun acc -> StructGet (TMap.list_tid, 0l, None) :: l acc))
  | EListTl (_, l) ->
      let l = convert_expr tm new_meta procinfo l BNone None cinfo in
      do_box tm new_meta box (fun acc -> StructGet (TMap.list_tid, 1l, None) :: l acc)
  | EClose (f, bcl, cls) ->
      let targs, tret, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) fid;
      let fctid = TMap.recid_of_type tm (TClosArg clts) in
      let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fctid cinfo in
      do_box tm new_meta box (fun acc -> StructNew (new_ctid, Explicit) :: gen_new_struct (RefFunc fid :: acc))
  | ERawClose (f, cl) ->
      let targs, tret, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) fid;
      let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      do_box tm new_meta box (fun acc -> StructNew (new_ctid, Explicit) :: get_cl (RefFunc fid :: acc))
  | ESpecialize (e, _, BLnone, BNone) -> begin match box with (* Update the type to ignore generalizations *)
      | BNone -> convert_expr tm new_meta procinfo e BNone is_last cinfo
      | BClosed (TClosed (targs, tret), bargs, bret) -> convert_expr tm new_meta procinfo e (BClosed (TClosed (targs, tret), bargs, bret)) is_last cinfo
      | BBox (TClosed (bargs, bret)) -> convert_expr tm new_meta procinfo e (BBox (TClosed (bargs, bret))) is_last cinfo
    end
  | ESpecialize (e, tdst, bargs, bret) ->
      do_box tm new_meta box (do_unbox tm new_meta (BClosed (tdst, bargs, bret)) (convert_expr tm new_meta procinfo e BNone None cinfo))
  | ECallRawHandler (fid, _, contarg, targ, arg, hdlarg, _) ->
      let fid = (fid :> int32) in
      let arg = convert_expr tm new_meta procinfo arg (BBox targ) None cinfo in
      let contarg = convert_expr tm new_meta procinfo contarg BNone None cinfo in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          do_box tm new_meta box
            (fun acc -> Br jmplv :: contarg (arg acc))
      | _ ->
          let hdlarg = convert_expr tm new_meta procinfo hdlarg BNone None cinfo in
          do_box tm new_meta box
            (fun acc -> (if can_early_ret then ReturnCall fid else Call fid) :: hdlarg (arg (contarg acc)))
    end
  | ECallClosed (ESpecialize (EClose (f, bcl, cls), _, bargs, bret), args, _) ->
      let _, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fcid cinfo in
      let unbox = do_unbox tm new_meta bret (fun acc ->
          (if can_early_ret && (match bret with BNone -> true | _ -> false) then ReturnCall fid else Call fid) ::
          generate_yield (NewMetadata.g_of_t new_meta) procinfo (gen_new_struct (args acc))) in
      do_box tm new_meta box unbox
  | ECallClosed (EClose (f, bcl, cls), args, _) ->
      let _, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fcid cinfo in
      do_box tm new_meta box (fun acc -> (if can_early_ret then ReturnCall fid else Call fid) ::
        generate_yield (NewMetadata.g_of_t new_meta) procinfo (gen_new_struct (args acc)))
  | ECallClosed (ESpecialize (ERawClose (f, cl), _, bargs, bret), args, _) ->
      let _, _, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      let can_early_ret = can_early_ret && (match bret with BNone -> true | _ -> false) in
      let unbox = do_unbox tm new_meta bret (fun acc ->
          (if can_early_ret then ReturnCall fid else Call fid) ::
          generate_yield (NewMetadata.g_of_t new_meta) procinfo (get_cl (args acc))) in
      do_box tm new_meta box unbox
  | ECallClosed (ERawClose (f, cl), args, _) ->
      let _, _, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      do_box tm new_meta box (fun acc -> (if can_early_ret then ReturnCall fid else Call fid) ::
        generate_yield (NewMetadata.g_of_t new_meta) procinfo (get_cl (args acc)))
  | ECallClosed (ESpecialize (e, _, bargs, bret), args, _) ->
      let TClosed (targs, tret) as t, getv, etee = match e with
        | EVariable (loc, vid) ->
            let t, vid = (vid : _ varid :> _ * int32) in
            let getv = convert_get_var' loc vid cinfo in
            t, getv, getv
        | _ ->
            let t = typ_of_expr e in
            let e = convert_expr tm new_meta procinfo e BNone None cinfo in
            let vid = NewMetadata.add_local tm new_meta t in
            t, (fun acc -> LocalGet vid :: acc), (fun acc -> LocalTee vid :: e acc) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let can_early_ret = can_early_ret && (match bret with BNone -> true | _ -> false) in
      let unbox = do_unbox tm new_meta bret (fun acc -> (if can_early_ret then ReturnCallRef fid else CallRef fid) ::
          generate_yield (NewMetadata.g_of_t new_meta) procinfo (
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: etee (
          args acc)))) in
      do_box tm new_meta box unbox
  | ECallClosed (e, args, _) ->
      let TClosed (targs, tret) as t, getv, etee = match e with
        | EVariable (loc, vid) ->
            let t, vid = (vid : _ varid :> _ * int32) in
            let getv = convert_get_var' loc vid cinfo in
            t, getv, getv
        | _ ->
            let t = typ_of_expr e in
            let e = convert_expr tm new_meta procinfo e BNone None cinfo in
            let vid = NewMetadata.add_local tm new_meta t in
            t, (fun acc -> LocalGet vid :: acc), (fun acc -> LocalTee vid :: e acc) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      do_box tm new_meta box (fun acc -> (if can_early_ret then ReturnCallRef fid else CallRef fid) ::
          generate_yield (NewMetadata.g_of_t new_meta) procinfo (
          StructGet (vtid, 0l, None) :: getv (
          StructGet (vtid, 1l, None) :: etee (
          args acc))))
  | ECond (e, rt, t, f) ->
      let ti = convert_block tm new_meta procinfo t box (incr_depth is_last) cinfo [] in
      let fi = convert_block tm new_meta procinfo f box (incr_depth is_last) cinfo [] in
      let ei = convert_expr tm new_meta procinfo e BNone None cinfo in
      let rt' = TMap.oval_of_type tm rt in
      fun acc -> If (Wasm.Type.(ValBlockType rt'), List.rev ti, List.rev fi) :: ei acc
  | EDo (eid, tret, args) ->
      let targs, eid = (eid : _ effectid :> _ * int32) in
      let args = match targs, args with
        | TLnil, ELnil -> fun acc -> RefNull Wasm.Type.NoneHT :: acc
        | TLcons (targ, TLnil), ELcons (arg, ELnil) -> convert_expr tm new_meta procinfo arg (BBox targ) None cinfo
        | TLcons (_, TLcons (_, _)), ELcons (_, ELcons (_, _)) ->
            let tid = TMap.recid_of_type tm (TTuple targs) in
            let AnyBoxList bcontent =
              let rec inner : type a. a typ_list -> a anybox_list = function
                | TLnil -> AnyBoxList BLnil
                | TLcons (thd, ttl) -> let AnyBoxList btl = inner ttl in AnyBoxList (BLcons (BBox thd, btl)) in
              inner targs in
            convert_new_struct tm new_meta procinfo args bcontent tid cinfo in
      let ret = do_unbox tm new_meta (BBox tret) (fun acc -> Suspend eid :: generate_yield (NewMetadata.g_of_t new_meta) procinfo (args acc)) in
      do_box tm new_meta box ret
  | EShallowHandle _ -> failwith "TODO: convert_expr EShallowHandle"
  | EDeepHandle (contid, contargs, hdlid, hdlargs) ->
      let _, _, thdlcl, hdlid = (hdlid : _ funcid :> _ * _ * _ * int32) in
      let hdlcid = TMap.recid_of_type tm (TClosArg thdlcl) in
      let gen_hdl_struct = convert_new_struct tm new_meta procinfo hdlargs BLnone hdlcid cinfo in
      let _, tcret, tcontcl, contid = (contid : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) contid;
      let contcid = TMap.recid_of_type tm (TClosArg tcontcl) in
      let gen_cont_struct = convert_new_struct tm new_meta procinfo contargs BLnone contcid cinfo in
      let tcontid = TMap.recid_of_type tm (TCont tcret) in
      do_box tm new_meta box (fun acc ->
        (if can_early_ret then ReturnCall hdlid else Call hdlid) ::
        gen_hdl_struct (
        gen_cont_struct (
        ContNew tcontid ::
        RefFunc contid ::
        acc)))
and convert_exprs : type a b. _ -> _ -> _ -> a expr_list -> (a, b) box_list -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (es : a expr_list) box (cinfo : clos_info) -> match box, es with
  | _, ELnil -> Fun.id
  | BLnone, ELcons (ehd, etl) ->
      let f = convert_expr tm new_meta procinfo ehd BNone None cinfo in
      let f2 = convert_exprs tm new_meta procinfo etl BLnone cinfo in
      fun acc -> f2 (f acc)
  | BLcons (bhd, btl), ELcons (ehd, etl) ->
      let f = convert_expr tm new_meta procinfo ehd bhd None cinfo in
      let f2 = convert_exprs tm new_meta procinfo etl btl cinfo in
      fun acc -> f2 (f acc)
and convert_new_struct : type a b. _ -> _ -> _ -> a expr_list -> (a, b) box_list -> _ -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (cls : a expr_list) box (fcid : int32) (cinfo : clos_info) ->
  let open Wasm.Instruction in
  match cls with
  | ELnil -> fun acc -> RefNull Wasm.Type.(VarHT (StatX fcid)) :: acc
  | ELcons _ -> let f = convert_exprs tm new_meta procinfo cls box cinfo in fun acc -> StructNew (fcid, Explicit) :: f acc

(* These two functions return the instructions in reverse order *)
let convert_anyblock (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
                     (b : 'a block) (is_last : mfunid option) (cinfo : clos_info) : Wasm.Instruction.t list =
  let b = convert_block tm new_meta procinfo b BNone (Option.map (fun fid -> (fid, 0l)) is_last) cinfo in b []
let convert_finisher : type a b. _ -> _ -> _ -> (a, b) finisher -> _ =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (f : (a, b) finisher) (is_last : last_info) (cinfo : clos_info) : Wasm.Instruction.t list ->
  match f with
  | FId _ -> []
  | FMap (v, _, b) ->
      let _, v = (v : _ varid :> _ * int32) in
      let b = convert_block tm new_meta procinfo b BNone is_last cinfo in
      b Wasm.Instruction.[LocalSet v]

let convert_hdl (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (type a b) (f : (a, b) fhandler) : Wasm.fundef =
  let new_meta = NewMetadata.extend glob 3l (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fh_locals) in
  let (tcont, contidx), contcl = (f.fh_contarg : _ varid * mvarid :> (_ * int32) * int32) in
  let tret = match f.fh_finisher with FId t -> (t : b typ) | FMap (_, t, _) -> t in
  let contid, fun_typ = TMap.recids_of_handler tm tcont tret in
  let open Wasm.Instruction in
  let convert_clos, cinfo = match f.fh_closure with
    | None -> [], None
    | Some (src, (TypeList clt, dst)) ->
        let clostyp = TMap.recid_of_type tm (TClosArg clt) in
        [LocalSet (dst :> int32); RefCast Wasm.Type.(NoNull, (VarHT (StatX clostyp))); LocalGet (src :> int32)], Some (clostyp, dst)
  in let code =
    let nblocks, handlers =
      let rec inner (hdls : _ handler list) len acc = match hdls with
        | [] -> len, acc
        | Handler (eid, _, _, _) :: tl -> inner tl (Int32.succ len) ((let _, eid = (eid : _ effectid :> _ * int32) in eid, OnLabel len) :: acc)
      in inner f.fh_handlers 0l [] in
    let code = convert_finisher tm new_meta procinfo f.fh_finisher (Some (f.fh_id, nblocks)) cinfo in
    let code = Resume (contid, handlers) :: List.rev_append code [Return] in
    let rec do_cases nblocks (cases : _ handler list) code = match cases with
      | [] -> code
      | Handler (eid, varc, vars, blk) :: tl ->
          let nblocks = Int32.pred nblocks in
          let codehdl =
            let b = convert_block tm new_meta procinfo blk BNone (Some (f.fh_id, nblocks)) cinfo in
            List.rev_append (b []) [Return] in
          let codehdl = match vars with
            | VLnil -> Drop :: codehdl
            | VLcons (v, VLnil) ->
                let t, v = (v : _ varid :> _ * int32) in
                let unbox = do_unbox tm new_meta (BBox t) (fun acc -> acc) in
                List.rev_append (unbox []) (LocalSet v :: codehdl)
            | VLcons (_, VLcons (_, _)) ->
                let ts =
                  let [@tail_mod_cons] rec inner : type a. a varid_list -> a typ_list = fun (vars : a varid_list) : a typ_list -> match vars with
                    | VLnil -> TLnil
                    | VLcons (vhd, vtl) ->
                        let thd, _ = (vhd : _ varid :> _ * int32) in
                        (TLcons (thd, inner vtl)) in
                  inner vars in
                let tid = TMap.recid_of_type tm (TTuple ts) in
                let tmpv = NewMetadata.add_local tm new_meta (TTuple ts) in
                let rec inner : type a. a varid_list -> _ = fun (vars : a varid_list) i codehdl -> match vars with
                  | VLnil -> codehdl
                  | VLcons (vhd, vtl) ->
                      let t, v = (vhd : _ varid :> _ * int32) in
                      let get = do_unbox tm new_meta (BBox t) (fun acc -> StructGet (tid, i, None) :: LocalGet tmpv :: acc) in
                      inner vtl (Int32.succ i) (List.rev_append (get []) (LocalSet v :: codehdl)) in
                RefCast Wasm.Type.(NoNull, VarHT (StatX tid)) :: LocalSet tmpv :: inner vars 0l codehdl in
          let _, varc = (varc : _ varid :> _ * int32) in
          let codehdl = LocalSet varc :: codehdl in
          let eargs, _ = (eid : _ effectid :> _ * _) in
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

let convert_builtin (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (_fid : mfunid)
                    (type g a b) (fb : (g, a, b) fbuiltin) : Wasm.fundef = match fb with
  | FBHere ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TSpawnLocation in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = let open Type in Instruction.[
        RefNull NoneHT;
      ];
    }
  | FBIntToString ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TInt, TLnil)) TString in
    let i32toi32 = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [NumT I32T])))) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[NumT I64T; NumT I32T; RefT (NoNull, VarHT (StatX TMap.string_tid))];
      fn_code = let open Value in let open Type in Instruction.[
        Const (I32 (I32.of_int_s (Char.code '0')));
        LocalGet 0l;
        Const (I64 I64.zero);
        Relop (I64 IntOp.LtS);
        If (ValBlockType (Some (NumT I32T)), [
          Const (I64 I64.zero);
          LocalGet 0l;
          Binop (I64 IntOp.Sub);
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Const (I32 (I32.of_bits 2l));
        ], [
          LocalGet 0l;
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Const (I32 I32.one);
        ]);
        Block (VarBlockType i32toi32, [
          Loop (VarBlockType i32toi32, [
            LocalGet 2l;
            Testop (I64 IntOp.Eqz);
            BrIf 1l;
            Const (I32 I32.one);
            Binop (I32 IntOp.Add);
            LocalGet 2l;
            Const (I64 (I64.of_bits 10L));
            Binop (I64 IntOp.DivU);
            LocalSet 2l;
            Br 0l;
          ]);
        ]);
        LocalTee 3l;
        ArrayNew (TMap.string_tid, Explicit);
        LocalSet 4l;
        LocalGet 0l;
        Const (I64 I64.zero);
        Relop (I64 IntOp.LtS);
        If (ValBlockType (Some (NumT I64T)), [
          LocalGet 4l;
          Const (I32 I32.zero);
          Const (I32 (I32.of_int_s (Char.code '-')));
          ArraySet TMap.string_tid;
          Const (I64 I64.zero);
          LocalGet 0l;
          Binop (I64 IntOp.Sub);
        ], [
          LocalGet 0l;
        ]);
        LocalSet 2l;
        Loop (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.string_tid)))), [
          LocalGet 4l;
          LocalGet 2l;
          Testop (I64 IntOp.Eqz);
          BrIf 1l;
          LocalGet 3l;
          Const (I32 I32.one);
          Binop (I32 IntOp.Sub);
          LocalTee 3l;
          LocalGet 2l;
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.RemU);
          Cvtop (I32 IntOp.WrapI64);
          Const (I32 (I32.of_int_s (Char.code '0')));
          Binop (I32 IntOp.Add);
          ArraySet TMap.string_tid;
          LocalGet 2l;
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Br 0l;
        ]);
      ];
    }
  | FBLength ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TList TVar, TLnil)) TInt in
    let i64toi64 = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [], DefFuncT (FuncT ([NumT I64T], [NumT I64T])))) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = let open Value in let open Type in Instruction.[
        Const (I64 (I64.of_bits 0L));
        Loop (VarBlockType i64toi64, [
          LocalGet 0l;
          BrOnNull 1l;
          StructGet (TMap.list_tid, 1l, None);
          LocalSet 0l;
          Const (I64 I64.one);
          Binop (I64 IntOp.Add);
          Br 0l;
        ]);
      ];
    }
  | FBRecv ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TVar in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[
        RefT (NoNull, VarHT (StatX TMap.pid_active_tid));
        RefT (NoNull, VarHT (StatX TMap.list_tid));
      ];
      fn_code = let open Value in let open Type in Instruction.[
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.self_offset);
        StructGet (TMap.pid_tid, 1l, None);
        RefCast (NoNull, VarHT (StatX TMap.pid_active_tid));
        LocalTee 1l;
        Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.list_tid)))), [
          LocalGet 1l;
          StructGet (TMap.pid_active_tid, 2l, None); (* Is the pop queue empty? *)
          BrOnNonNull 0l;
          (* Yes, transfer the push queue to the pop queue *)
          Loop (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.list_tid)))), [
            Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.pid_active_tid)))), [
              Loop (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.pid_active_tid)))), [
                LocalGet 1l;
                LocalGet 1l;
                StructGet (TMap.pid_active_tid, 1l, None);
                BrOnNull 1l; (* Nothing left in the push queue *)
                LocalTee 2l;
                (* Pop the head of the push queue; the stack contains the active PID and the push queue *)
                StructGet (TMap.list_tid, 1l, None);
                StructSet (TMap.pid_active_tid, 1l);
                LocalGet 1l; (* Push the old head of the push queue to the pop queue *)
                LocalGet 2l;
                StructGet (TMap.list_tid, 0l, None);
                LocalGet 1l;
                StructGet (TMap.pid_active_tid, 2l, None);
                StructNew (TMap.list_tid, Explicit);
                StructSet (TMap.pid_active_tid, 2l);
                Br 0l; (* Repeat *)
              ]);
            ]);
            StructGet (TMap.pid_active_tid, 2l, None);
            BrOnNonNull 1l; (* There was something in the push queue, which is now moved to the pop queue *)
            (* There is nothing in both queue: block and try again *)
            LocalGet 1l;
            Const (I32 I32.one);
            StructSet (TMap.pid_active_tid, 0l); (* Set blocked *)
            Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset); (* Wait until unblocked *)
            (* Note that by scheduler logic, the push queue is now non-empty: transfer the push queue to the pop queue *)
            Br 0l;
          ]);
        ]);
        (* We have the head of the pop queue *)
        LocalTee 2l;
        StructGet (TMap.list_tid, 1l, None);
        StructSet (TMap.pid_active_tid, 2l);
        LocalGet 2l;
        StructGet (TMap.list_tid, 0l, None);
      ];
    }
  | FBSelf ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TProcess in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = Instruction.[
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.self_offset);
      ];
    }
  | FBSend ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TProcess, TLcons (TVar, TLnil))) (TTuple TLnil) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[
        RefT (NoNull, VarHT (StatX TMap.pid_active_tid));
      ];
      fn_code = let open Value in let open Type in Instruction.[
        (* If the process is inactive, drop the message; otherwise, find the pid_active_tid and cache it *)
        Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.pid_active_tid)))), [
          LocalGet 0l;
          StructGet (TMap.pid_tid, 1l, None);
          BrOnCast (0l, (Null, EqHT), (NoNull, VarHT (StatX TMap.pid_active_tid)));
          Drop;
          RefNull NoneHT;
          Return;
        ]);
        LocalTee 3l;
        (* Now unblock the process and push the message into the queue *)
        Const (I32 I32.zero);
        StructSet (TMap.pid_active_tid, 0l);
        LocalGet 3l;
        LocalGet 1l;
        LocalGet 3l;
        StructGet (TMap.pid_active_tid, 1l, None);
        StructNew (TMap.list_tid, Explicit);
        StructSet (TMap.pid_active_tid, 1l);
        RefNull NoneHT;
      ];
    }
  | FBSpawnAngelAt ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TSpawnLocation, TLcons (TClosed (TLnil, TVar), TLnil))) TProcess in
    let _, angels_vid, _ = Option.get procinfo in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = Instruction.[
        LocalGet 1l;
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset);
        GlobalGet angels_vid;
        StructNew (TMap.pid_list_tid, Explicit);
        GlobalSet angels_vid;
        GlobalGet angels_vid;
        StructGet (TMap.pid_list_tid, 0l, None);
      ];
    }
  | FBSpawnAt ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TSpawnLocation, TLcons (TClosed (TLnil, TVar), TLnil))) TProcess in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = Instruction.[
        LocalGet 1l;
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset);
      ];
    }
  | FBWait ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TProcess, TLnil)) TVar in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [];
      fn_code = let open Type in Instruction.[
        LocalGet 0l;
        StructGet (TMap.pid_tid, 1l, None);
        BrOnCastFail (0l, (Null, EqHT), (NoNull, VarHT (StatX TMap.pid_active_tid)));
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset);
      ];
    }

let convert_fun_aux (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo)
                    (ft : int32) (nparams : int32) (locals : Wasm.Type.val_type list) (f : 'a block)
    (init_dest : (bool * Wasm.Instruction.t list, mfunid) Either.t) (closid : (anytyp_list * mvarid) option) : int32 option * Wasm.fundef =
  let new_meta = NewMetadata.extend glob nparams locals in
  let clostid, cinfo = match closid with
    | None -> None, None
    | Some (TypeList ct, cid) ->
        let ctid = TMap.recid_of_type tm (TClosArg ct) in
        Some ctid, Some (ctid, cid) in
  let code = convert_anyblock tm new_meta procinfo f (Either.find_right init_dest) cinfo in
  clostid, Wasm.{
    fn_name = (match init_dest with Either.Left (true, _) -> Some "main" | Either.Left (false, _) | Either.Right _ -> None);
    fn_type = ft;
    fn_locals = NewMetadata.wasm_of_locals tm new_meta;
    fn_code = List.rev (match init_dest with Either.Left (_, app_code) -> app_code @ code | Either.Right _ -> code);
  }

let convert_fun (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (f : ('a, 'b) func') : int32 option * Wasm.fundef =
  let fun_typ = TMap.recid_of_functyp tm f.fun_args f.fun_ret in
  let nparams =
    let rec inner : type a. a typ_list -> _ = fun tl acc -> match tl with
      | TLnil -> acc
      | TLcons (_, tl) -> inner tl (Int32.succ acc) in
    inner f.fun_args 1l in
  convert_fun_aux
    tm glob procinfo fun_typ nparams (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fun_locals) f.fun_block (Either.Right f.fun_id) f.fun_converted_closure
let convert_fst (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (f : 'b fstart) : int32 option * Wasm.fundef =
  let fun_typ = TMap.recid_of_cfunctyp tm f.fst_ret in
  convert_fun_aux
    tm glob procinfo fun_typ 1l (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fst_locals) f.fst_block (Either.Right f.fst_id) f.fst_converted_closure
let convert_fun_step2 (tm : tmap) (f, clostyp : func * int32 option) : Wasm.fundef option = match f with
  | FContinuationStart _ | FHandler _ | FBuiltin _ -> None
  | FFunction f ->
  match f.fun_export_data with
  | None -> None
  | Some name ->
      let targs = f.fun_args in
      let tret = f.fun_ret in
      let fn_type = TMap.recid_of_exported_type tm targs tret in
      let clostyp = let open Wasm.Type in match clostyp with None -> NoneHT | Some clostid -> VarHT (StatX clostid) in
      Some Wasm.{
        fn_name = Some name;
        fn_type;
        fn_locals = [];
        fn_code = List.rev Wasm.Instruction.(
          ReturnCall (f.fun_id :> int32) ::
          RefNull clostyp ::
          let rec inner : type a. _ -> _ -> a typ_list -> _ = fun i acc (ls : a typ_list) -> match ls with
            | TLnil -> acc
            | TLcons (_, ls) -> inner (Int32.succ i) (LocalGet i :: acc) ls
          in inner 0l [] f.fun_args);
      }
let convert_funs (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo)
                 (fs : func list) (is : NewMetadata.g -> Wasm.fundef list) : Wasm.fundef list =
  let [@tail_mod_cons] rec inner glob fs acc = match fs with
    | [] -> is glob @ List.filter_map (convert_fun_step2 tm) acc
    | hd :: tl ->
        let fid, fhd, acc = match hd with
          | FFunction hd ->
              let ctid, fhd = convert_fun tm glob procinfo hd in (hd.fun_id :> int32), fhd, ((FFunction hd, ctid) :: acc)
          | FContinuationStart hd ->
              let ctid, fhd = convert_fst tm glob procinfo hd in (hd.fst_id :> int32), fhd, ((FContinuationStart hd, ctid) :: acc)
          | FHandler hd ->
              let fhd = convert_hdl tm glob procinfo hd in (hd.fh_id :> int32), fhd, acc
          | FBuiltin (fid, hd) ->
              let fhd = convert_builtin tm glob procinfo fid hd in (fid :> int32), fhd, acc in
        NewMetadata.register_function glob ~fid ~ftid:fhd.Wasm.fn_type;
        fhd :: inner glob tl acc
  in inner glob fs []

let generate_type_map (m : 'a modu) : tmap = TMap.empty m.mod_has_processes

let convert_effects (tm : tmap) (es : EffectIDSet.t) (has_sched_effects : bool) : int32 list =
  let es = EffectIDSet.elements es in
  let generic_tid =
    let open Wasm.Type in
    TMap.recid_of_sub_type tm (SubT (Final, [], DefFuncT (FuncT ([RefT (Null, EqHT)], [RefT (Null, EqHT)])))) in
  let convert_effect (_ : meffid) : int32 = generic_tid in
  let [@tail_mod_cons] rec map_append f l1 l2 = match l1 with
    | [] -> l2
    | hd :: tl -> f hd :: map_append f tl l2 in
  map_append convert_effect es (if has_sched_effects then TMap.sched_effects tm else [])

type 'a import_info = {
  impinfo_putc : 'a;
  impinfo_puta : 'a;
}
let impinfo_empty : int32 option import_info = {
  impinfo_putc = None;
  impinfo_puta = None;
}
let convert_import (tm : tmap) ((fidx, tidx, impinfo) : int32 * int32 * int32 option import_info) ((m, i) : string * string)
    : (int32 * int32 * int32 option import_info) * Wasm.import =
  let acc, desc = match m, i with
  | "wizeng", "puta" ->
      let fid = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [], DefFuncT (
        FuncT ([RefT (Null, VarHT (StatX TMap.string_tid)); NumT I32T; NumT I32T], [])))) in
      (Int32.succ fidx, tidx, { impinfo with impinfo_puta = Some fidx }), Wasm.FuncImport fid
  | "wizeng", "putc" ->
      let fid = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [])))) in
      (Int32.succ fidx, tidx, { impinfo with impinfo_putc = Some fidx }), Wasm.FuncImport fid
  | _ -> raise (internal_error ("Unknown import '" ^ m ^ "'.'" ^ i ^ "'"))
  in acc, Wasm.{ module_name = m; item_name = i; desc }
let finish_import_info (impinfo : int32 option import_info) : int32 import_info option = match impinfo with
  | { impinfo_putc = Some putc; impinfo_puta = Some puta; } ->
    Some { impinfo_putc = putc; impinfo_puta = puta; }
  | { impinfo_putc = None; _ }
  | { impinfo_putc = Some _; impinfo_puta = None } -> None

let compile (m : 'a modu) (use_init : bool) (main_typ_name : string) : Wasm.module_ =
  let tm = generate_type_map m in
  let main_res =
    let cg = convert_global tm (0, Type m.mod_main, Some "_init_result") in
    cg in
  let glob =
    let mainid = m.mod_nfuns in
    let nfuns = Int32.succ mainid in
    let neffects = m.mod_neffs in
    let nglob = Int32.succ m.mod_nglobals in
    NewMetadata.empty_global ~nfuns ~neffects ~nglob in
  let procinfo =
    if m.mod_has_processes then begin
      let open Wasm in
      let procinfo_vid = NewMetadata.add_global tm glob None TInt Instruction.[Const Value.(I64 (I64.of_bits Int64.zero))] in
      let angels_vid = NewMetadata.add_raw_global glob (
        Type.(GlobalT (Var, RefT (Null, VarHT (StatX TMap.pid_list_tid)))),
        Instruction.[RefNull Type.(VarHT (StatX TMap.pid_list_tid))],
        None) in
      Some (procinfo_vid, angels_vid, ref None)
    end else None in
  let (_, _, impinfo), imports = List.fold_left_map (convert_import tm) (0l, 0l, impinfo_empty) m.mod_imports in
  let impinfo = finish_import_info impinfo in
  let glob, main =
    let glob, main_code, extra_locals =
      let open Wasm in
      let open Value in
      let open Instruction in
      match impinfo with
      | None -> glob, [
            GlobalSet m.mod_nglobals;
          ], []
      | Some { impinfo_putc = putc; impinfo_puta = puta } ->
          let new_meta = NewMetadata.extend glob (Int32.of_int (List.length m.mod_locals)) [] in
          let rec prepare : type a. a typ -> _ = fun (t : a typ) (new_meta : new_meta) ->
            let glob = NewMetadata.g_of_t new_meta in match t with
            | TTuple TLnil ->
                Either.Left [
                  Drop;
                  Const (I32 (I32.of_int_s (Char.code '('))); Call putc;
                  Const (I32 (I32.of_int_s (Char.code ')'))); Call putc;
                ]
            | TInt ->
                let funtid = TMap.recid_of_sub_type tm Type.(SubT (Final, [], DefFuncT (FuncT ([NumT I64T], [])))) in
                let auxfref, auxfunid = NewMetadata.add_function glob in
                let fref, funid = NewMetadata.add_function glob in
                NewMetadata.register_function glob ~fid:auxfunid ~ftid:funtid;
                auxfref := Some { fn_name = None; fn_type = funtid; fn_locals = []; fn_code = [
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
                NewMetadata.register_function glob ~fid:funid ~ftid:funtid;
                fref := Some { fn_name = None; fn_type = funtid; fn_locals = []; fn_code = [
                  LocalGet 0l;
                  Const (I64 I64.zero);
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
                    Const (I64 I64.zero);
                    LocalGet 0l;
                    Binop (I64 IntOp.Sub);
                  ]);
                  Call auxfunid;
                ]};
                Either.Right funid
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
                ]
            | TString ->
                let funtid = TMap.recid_of_sub_type tm Type.(SubT (Final, [],
                      DefFuncT (FuncT ([RefT (NoNull, VarHT (StatX TMap.string_tid))], [])))) in
                let fref, funid = NewMetadata.add_function glob in
                NewMetadata.register_function glob ~fid:funid ~ftid:funtid;
                fref := Some { fn_name = None; fn_type = funtid; fn_locals = Type.[NumT I32T]; fn_code = [
                  Const (I32 (I32.of_int_s (Char.code '"'))); Call putc;
                  LocalGet 0l;
                  Const (I32 I32.zero);
                  LocalGet 0l; ArrayLen;
                  Call puta;
                  Const (I32 (I32.of_int_s (Char.code '"'))); Call putc;
                ]};
                Either.Right funid
            | TList t ->
                let mainblocktid = TMap.recid_of_sub_type tm Type.(SubT (Final, [],
                      DefFuncT (FuncT ([RefT (Null, VarHT (StatX TMap.list_tid))], [])))) in
                let locid = NewMetadata.add_raw_local new_meta Type.(RefT (NoNull, VarHT (StatX TMap.list_tid))) in
                let unbox =
                  List.rev_append (do_unbox tm new_meta (BBox t) (fun acc -> StructGet (TMap.list_tid, 0l, None) :: LocalGet locid :: acc) []) in
                let elem = prepare t new_meta in
                let print_elem = match elem with
                  | Either.Left code -> fun acc -> code @ acc
                  | Either.Right fid -> fun acc -> Call fid :: acc
                  in
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
                ]
            | TVariant -> failwith "TODO: Irtowasm.compile.prepare for variant"
            | TClosed _ ->
                let add_string code s =
                  String.fold_right (fun c acc -> Const (I32 (I32.of_int_s (Char.code c))) :: Call putc :: acc)
                    s code in
                Either.Left (Drop :: add_string [] "fun")
            | _ -> failwith "TODO: Irtowasm.compile.prepare for this type"
          in
          let code = prepare m.mod_main new_meta in
          let code = match code with
              | Either.Left code -> List.rev_append code [GlobalGet m.mod_nglobals; GlobalSet m.mod_nglobals]
              | Either.Right fid -> [Call fid; GlobalGet m.mod_nglobals; GlobalSet m.mod_nglobals] in
          let code =
            let add_string code s =
              String.fold_left (fun acc c -> Call putc :: Const (I32 (I32.of_int_s (Char.code c))) :: acc)
                code s in
            add_string (add_string code " : ") main_typ_name in
          glob, Call putc :: Const (I32 (I32.of_int_s (Char.code '\n'))) :: code, NewMetadata.wasm_of_locals tm new_meta in
    let locals =
      let [@tail_mod_cons] rec inner tm l tl = match l with
        | [] -> tl
        | Type hd :: l -> TMap.val_of_type tm hd :: inner tm l tl in
      inner tm m.mod_locals extra_locals in
    let is_init = (Option.is_none procinfo, main_code) in
    let _, main = convert_fun_aux tm glob procinfo TMap.main_func_type 0l locals m.mod_block (Either.Left is_init) None in
    (* NewMetadata.register_function glob ~fid:m.mod_nfuns ~ftid:TMap.main_func_type; *)
    glob, main in
  let mainid =
    match procinfo with
    | None -> m.mod_nfuns
    | Some (procinfo, angel_list, _) ->
      let open Wasm in let open Type in
      let spawn_fid = TMap.recid_of_exported_type tm TLnil TVar in
      (* Wrap the main function *)
      let aux_mainref, aux_mainid = NewMetadata.add_function glob in
      NewMetadata.register_function glob ~fid:aux_mainid ~ftid:spawn_fid;
      NewMetadata.add_export_function glob aux_mainid;
      aux_mainref := Some {
        fn_name = None;
        fn_type = spawn_fid;
        fn_locals = [];
        fn_code = Instruction.[
          (* Run main *)
          Call m.mod_nfuns;
          (* Wait until all angels are done *)
          Block (ValBlockType None, [
            Loop (ValBlockType None, [
              Block (ValBlockType (Some (RefT (Null, EqHT))), [
                GlobalGet angel_list; (* Get next angel PID *)
                BrOnNull 2l; (* Is there no next angel? *)
                StructGet (TMap.pid_list_tid, 0l, None); (* There is a next angel *)
                GlobalGet angel_list; (* Mark next angel PID as finished (remove it from the list of angels) *)
                StructGet (TMap.pid_list_tid, 1l, None);
                GlobalSet angel_list;
                (* Wait for the PID in the stack to finish, then loop *)
                StructGet (TMap.pid_tid, 1l, None);
                BrOnCastFail (0l, (Null, EqHT), (NoNull, VarHT (StatX TMap.pid_active_tid))); (* Already done? *)
                (* Angel has not returned *)
                Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset);
              ]);
              (* Ignore the angel return value *)
              Drop;
              Br 0l; (* Wait for the next angel *)
            ]);
          ]);
          RefNull NoneHT;
        ];
      };
      let initref, mainid = NewMetadata.add_function glob in
      (* NewMetadata.register_function glob ~fid:mainid ~ftid:TMap.main_func_type; *)
      let self_eid = Int32.add (NewMetadata.effect_offset glob) TMap.self_offset in
      let spawn_eid = Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset in
      let yield_eid = Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset in
      let wait_eid = Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset in
      let self_ftid = TMap.recid_of_sub_type tm (SubT (Final, [], DefFuncT (
        FuncT ([RefT (NoNull, VarHT (StatX TMap.pid_tid))], [RefT (Null, EqHT)])))) in
      let self_ctid = TMap.recid_of_sub_type tm (SubT (Final, [], DefContT (ContT (VarHT (StatX self_ftid))))) in
      let spawn_cbid = TMap.recid_of_type tm (TClosed (TLnil, TVar)) in
      let spawn_cbfid = TMap.recid_of_functyp tm TLnil TVar in
      let spawn_cbcid = TMap.recid_of_sub_type tm (SubT (Final, [], DefContT (ContT (VarHT (StatX spawn_cbfid))))) in
      let spawn_contid = self_ctid in
      let spawn_bt = TMap.recid_of_sub_type tm (SubT (Final, [], DefFuncT (
        FuncT ([], [RefT (NoNull, VarHT (StatX spawn_cbid)); RefT (NoNull, VarHT (StatX spawn_contid))])))) in
      let wait_bt = TMap.recid_of_sub_type tm (SubT (Final, [], DefFuncT (
        FuncT ([], [RefT (NoNull, VarHT (StatX TMap.pid_active_tid)); RefT (NoNull, VarHT (StatX TMap.process_waiting_tid))])))) in
      let self_bt = TMap.recid_of_sub_type tm (SubT (Final, [], DefFuncT (
        FuncT ([RefT (NoNull, VarHT (StatX self_ctid))], [RefT (Null, EqHT)])))) in
      initref := Some {
        fn_name = Some "main";
        fn_type = TMap.main_func_type;
        fn_locals = [
          NumT I32T;                                             (* 0: next pid *)
          RefT (NoNull, VarHT (StatX TMap.process_list_tid));    (* 1: all active and zombie processes *)
          RefT (NoNull, VarHT (StatX TMap.process_list_tid));    (* 2: active processes cache *)
          RefT (Null, EqHT);                                     (* 3: boxed value *)
          RefT (NoNull, VarHT (StatX self_ctid));                (* 4: $self/$spawn effect continuation *)
          RefT (NoNull, VarHT (StatX spawn_cbid));               (* 5: $spawn callback *)
          RefT (NoNull, VarHT (StatX TMap.process_active_tid));  (* 6: $yield continuation *)
          RefT (NoNull, VarHT (StatX TMap.process_waiting_tid)); (* 7: $wait continuation *)
          RefT (NoNull, VarHT (StatX TMap.pid_active_tid));      (* 8: $spawn/$wait PID / main/returned process PID *)
        ];
        fn_code = Value.(Instruction.[
          (* Next PID *)
          Const (I32 I32.one);
          LocalSet 0l;
          (* Create main process *)
          Const (I32 I32.zero); (* PID *)
          Const (I32 I32.zero); (* Unblocked by default *)
          RefNull (VarHT (StatX TMap.list_tid)); (* Mailbox *)
          RefNull (VarHT (StatX TMap.list_tid));
          RefNull (VarHT (StatX TMap.waiting_list_tid)); (* Processes waiting on main *)
          StructNew (TMap.pid_active_tid, Explicit);
          StructNew (TMap.pid_tid, Explicit);
          RefFunc aux_mainid; (* Wrapper function *)
          ContNew TMap.process_active_tid;
          RefNull (VarHT (StatX TMap.process_list_tid)); (* Next processes *)
          StructNew (TMap.process_list_tid, Explicit);
          LocalTee 1l; (* All processes *)
          LocalSet 2l; (* Process list iterator *)
          Block (ValBlockType None, [
            Loop (ValBlockType None, [
              Block (ValBlockType None, [                                                               (* Load next process *)
                Block (VarBlockType wait_bt, [                                                          (* $wait *)
                  Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.process_active_tid)))), [  (* $yield *)
                    Block (VarBlockType spawn_bt, [                                                     (* $spawn *)
                      Block (ValBlockType (Some (RefT (Null, EqHT))), [                                 (* (returned) *)
                        Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX self_ctid)))), [          (* $self *)
                          (* Run the process pointed by the iterator if it is unblocked *)
                          LocalGet 2l;
                          StructGet (TMap.process_list_tid, 0l, None);
                          StructGet (TMap.pid_tid, 1l, None);
                          RefCast (NoNull, VarHT (StatX TMap.pid_active_tid));
                          StructGet (TMap.pid_active_tid, 0l, Some Pack.ZX);
                          BrIf 5l; (* Blocked *)
                          (* OK, reset yield timer then resume process *)
                          Const (I64 (I64.of_bits (Int64.pred gbl_count_reset)));
                          GlobalSet procinfo;
                          LocalGet 2l;
                          StructGet (TMap.process_list_tid, 1l, None);
                          Resume (TMap.process_active_tid, [self_eid, OnLabel 0l; spawn_eid, OnLabel 2l; yield_eid, OnLabel 3l; wait_eid, OnLabel 4l]);
                          (* Process returned, boxed return value is the only value in the stack *)
                          Br 1l;
                        ]);
                        Loop (VarBlockType self_bt, [ (* $self: allow repeats *)
                          LocalSet 4l;
                          LocalGet 2l;
                          StructGet (TMap.process_list_tid, 0l, None);
                          LocalGet 4l;
                          Resume (self_ctid, [self_eid, OnLabel 0l; spawn_eid, OnLabel 2l; yield_eid, OnLabel 3l; wait_eid, OnLabel 4l]);
                          (* Process returned, boxed return value is the only value in the stack *)
                          Br 1l;
                        ]);
                      ]);
                      LocalSet 3l;
                      LocalGet 2l;
                      StructGet (TMap.process_list_tid, 0l, None);
                      StructGet (TMap.pid_tid, 1l, None);
                      RefCast (NoNull, VarHT (StatX TMap.pid_active_tid));
                      LocalSet 8l;
                      (* Unwait all waiting processes *)
                      Block (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.process_list_tid)))), [
                        Loop (ValBlockType (Some (RefT (NoNull, VarHT (StatX TMap.process_list_tid)))), [
                          LocalGet 2l;
                          LocalGet 8l;
                          StructGet (TMap.pid_active_tid, 3l, None);
                          BrOnNull 1l;
                          StructGet (TMap.waiting_list_tid, 0l, None); (* PID *)
                          LocalGet 3l;
                          LocalGet 8l;
                          StructGet (TMap.pid_active_tid, 3l, None);
                          RefAsNonNull;
                          StructGet (TMap.waiting_list_tid, 1l, None); (* Waiting continuation *)
                          ContBind (TMap.process_waiting_tid, TMap.process_active_tid); (* Active continuation *)
                          LocalGet 2l;
                          StructGet (TMap.process_list_tid, 2l, None); (* Old tail *)
                          StructNew (TMap.process_list_tid, Explicit); (* New tail *)
                          StructSet (TMap.process_list_tid, 2l);
                          (* Pop in the waiting list *)
                          LocalGet 8l;
                          LocalGet 8l;
                          StructGet (TMap.pid_active_tid, 3l, None);
                          RefAsNonNull;
                          StructGet (TMap.waiting_list_tid, 2l, None);
                          StructSet (TMap.pid_active_tid, 3l);
                          Br 0l;
                        ]);
                      ]);
                      (* Update the PID *)
                      StructGet (TMap.process_list_tid, 0l, None);
                      LocalGet 3l;
                      StructSet (TMap.pid_tid, 1l);
                      (* Update the process (remove the continuation) *)
                      LocalGet 2l;
                      RefNull (VarHT (StatX TMap.process_active_tid));
                      StructSet (TMap.process_list_tid, 1l);
                      Br 3l; (* Go to the next active process *)
                    ]);
                    (* stack is [callback_function continuation] *)
                    LocalSet 4l;
                    LocalSet 5l;
                    (* We will insert the new process just after the current process *)
                    LocalGet 2l;
                    (* Create a new PID *)
                    LocalGet 0l;
                    LocalGet 0l;
                    Const (I32 I32.one);
                    Binop (I32 IntOp.Add);
                    LocalSet 0l;
                    Const (I32 I32.zero);
                    RefNull (VarHT (StatX TMap.list_tid));
                    RefNull (VarHT (StatX TMap.list_tid));
                    RefNull (VarHT (StatX TMap.waiting_list_tid));
                    StructNew (TMap.pid_active_tid, Explicit);
                    LocalTee 8l;
                    StructNew (TMap.pid_tid, Explicit);
                    (* Create the continuation *)
                    LocalGet 5l;
                    StructGet (spawn_cbid, 1l, None);
                    LocalGet 5l;
                    StructGet (spawn_cbid, 0l, None);
                    ContNew spawn_cbcid;
                    ContBind (spawn_cbcid, TMap.process_active_tid);
                    (* Finish adding the process to the process list *)
                    LocalGet 2l;
                    StructGet (TMap.process_list_tid, 2l, None);
                    StructNew (TMap.process_list_tid, Explicit);
                    (* The stack is now [cached_process_list new_process_list_tail] *)
                    StructSet (TMap.process_list_tid, 2l);
                    (* Finally, update the current process *)
                    LocalGet 2l; (* Cached process list *)
                    LocalGet 2l; (* Get the child PID *)
                    StructGet (TMap.process_list_tid, 2l, None);
                    StructGet (TMap.process_list_tid, 0l, None);
                    LocalGet 4l; (* Continuation *)
                    ContBind (spawn_contid, TMap.process_active_tid); (* Bind the continuation argument *)
                    StructSet (TMap.process_list_tid, 1l); (* Update in the process list *)
                    Br 3l; (* Resume the current process *)
                  ]);
                  (* Update the current process *)
                  LocalSet 6l;
                  LocalGet 2l;
                  LocalGet 6l;
                  StructSet (TMap.process_list_tid, 1l);
                  Br 1l; (* Go to the next active process *)
                ]);
                (* Set continuation of self to Null and add the current process to the list of waiting processes *)
                LocalSet 7l;
                LocalSet 8l;
                LocalGet 2l;
                RefNull (VarHT (StatX TMap.process_active_tid));
                StructSet (TMap.process_list_tid, 1l);
                LocalGet 8l;
                LocalGet 2l;
                StructGet (TMap.process_list_tid, 0l, None);
                LocalGet 7l;
                LocalGet 8l;
                StructGet (TMap.pid_active_tid, 3l, None);
                StructNew (TMap.waiting_list_tid, Explicit);
                StructSet (TMap.pid_active_tid, 3l);
                Br 0l; (* Go to the next active process *)
              ]);
              (* Load the next active process
              An active process is a process in the process list that has a non-Null continuation.
              An inactive process is a process in the process list with a Null continuation.
              A zombie process is an inactive process with no mailbox (it has already returned).
              We first prioritize the cache (local 2l) to find an active process.
              If we reach the end of the cache, we repeat once with the entire process list (local 1l).
              If the main process is a zombie (the first process in the entire list), we are done and need to exit immediately.
              Otherwise, if we see another inactive process (Null continuation), we need to remove it from the list and try the step again.
              *)
              Loop (ValBlockType None, [
                Block (ValBlockType None, [
                  Loop (ValBlockType None, [
                    (* Search the cache *)
                    Block (ValBlockType None, [
                      LocalGet 2l;
                      StructGet (TMap.process_list_tid, 2l, None);
                      BrOnNull 2l;
                      (* Something is in the tail of the cache *)
                      StructGet (TMap.process_list_tid, 1l, None);
                      BrOnNull 0l;
                      (* Next process is active, select it *)
                      LocalGet 2l;
                      StructGet (TMap.process_list_tid, 2l, None);
                      RefAsNonNull;
                      LocalSet 2l;
                      Br 4l;
                    ]);
                    (* Next process is inactive, delete it *)
                    LocalGet 2l;
                    LocalGet 2l;
                    StructGet (TMap.process_list_tid, 2l, None);
                    StructGet (TMap.process_list_tid, 2l, None);
                    StructSet (TMap.process_list_tid, 2l);
                    Br 0l;
                  ]);
                ]);
                (* No more process in the queue, start from the beginning *)
                LocalGet 1l;
                LocalTee 2l;
                (* Check the main process *)
                StructGet (TMap.process_list_tid, 1l, None);
                RefIsNull;
                Testop (I32 IntOp.Eqz);
                BrIf 1l; (* Main is active, continue *)
                (* Check if main is a zombie or waiting *)
                LocalGet 1l;
                StructGet (TMap.process_list_tid, 0l, None);
                StructGet (TMap.pid_tid, 1l, None);
                RefTest (NoNull, VarHT (StatX TMap.pid_active_tid));
                Testop (I32 IntOp.Eqz);
                BrIf 2l; (* Main is a zombie, exit *)
                (* Main is just waiting, check the next processes *)
                (* Note: this is only a kind of deadlock detection *)
                LocalGet 1l;
                StructGet (TMap.process_list_tid, 2l, None);
                RefIsNull;
                Testop (I32 IntOp.Eqz);
                BrIf 0l; (* There are some other processes (though they may be zombies) *)
                (* Deadlock: nothing to do anymore *)
                Loop (ValBlockType None, [Br 0l]);
              ]);
            ]);
          ]);
        ]);
      };
      mainid in
  let funs = convert_funs tm glob procinfo m.mod_funs (fun glob -> main :: NewMetadata.wasm_of_funs glob) in
  let frgbls = NewMetadata.wasm_of_exports glob in
  let globals = convert_globals tm m.mod_global_vars (main_res :: frgbls) in
  let tags = convert_effects tm m.mod_effs (Option.is_some procinfo) in
  let types = TMap.to_wasm tm in
  Wasm.{ types; globals; tags; imports; funs; init = if use_init then None else Some mainid }
