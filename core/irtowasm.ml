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
  val empty : process_level -> bool -> t
  val main_func_type : int32
  val variant_tid : int32
  val list_tid : int32
  val string_tid : int32
  val boxed_int_tid : int32
  val boxed_float_tid : int32
  val pid_tid : int32
  val process_active_ftid : int32
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
  val exit_offset : int32
  
  val recid_of_sub_type : t -> Wasm.Type.sub_type -> int32
  val val_of_type : t -> 'a typ -> Wasm.Type.val_type
  val recid_of_type : t -> 'a typ -> int32
  val recid_of_functyp : t -> 'a typ_list -> 'b typ -> int32
  val recid_of_cfunctyp : t -> 'b typ -> int32
  
  val oval_of_type : t -> 'a typ -> Wasm.Type.val_type option
  
  val recids_of_handler : t -> 'a continuation typ -> 'c typ_list -> 'b typ -> int32 * int32
  val recid_of_handler_block : t -> int32 -> 'a typ_list -> int32
  val recid_of_handler_finish : t -> int32 -> 'a typ -> int32
  
  val recid_of_exported_type : t -> 'a typ_list -> 'b typ -> int32
  
  val to_wasm : t -> Wasm.Type.rec_type array
end = struct
  type t = {
    use_switch: bool;
    process_level: process_level;
    mutable cenv: int32 TypeMap.t;
    mutable eenv: int32 TypeMap.t;
    mutable nrefs: int32;
    mutable reftyps: Wasm.Type.rec_type list;
  } [@@warning "-69"] (* TODO: make use of the process_level field to remove unused tags *)
  
  let main_func_type : int32 = 0l
  let variant_tid : int32 = 1l
  let variant_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Cons, ValStorageT (NumT I32T));
    FieldT (Cons, ValStorageT (RefT (Null, EqHT)));
  |])))
  let list_tid : int32 = 2l
  let list_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Cons, ValStorageT (RefT (Null, EqHT)));
    FieldT (Cons, ValStorageT (RefT (Null, VarHT list_tid)));
  |])))
  let string_tid : int32 = 3l
  let string_typ = Wasm.Type.(SubT (Final, [||], DefArrayT (ArrayT (FieldT (Var, PackStorageT Wasm.Pack.Pack8)))))
  let boxed_int_tid : int32 = 4l
  let boxed_float_tid : int32 = 5l
  let pid_tid : int32 = 6l
  let pid_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Cons, ValStorageT (NumT I32T)); (* "Real" process ID *)
    FieldT (Var, ValStorageT (RefT (Null, EqHT))); (* boxed or pid_active (note: pid_active cannot be the boxed value) *)
  |])))
  let process_active_ftid : int32 = 7l
  let process_active_ftyp_on = Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([||], [|RefT (Null, EqHT)|]))))
  let process_active_tid : int32 = 8l
  let process_active_ftyp_switch = Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, VarHT process_active_tid)|], [|RefT (Null, EqHT)|]))))
  let process_active_typ = Wasm.Type.(SubT (Final, [||], DefContT (ContT (VarHT process_active_ftid))))
  let process_waiting_ftid : int32 = 9l
  let process_waiting_ftyp_on = Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT)|], [|RefT (Null, EqHT)|]))))
  let process_waiting_ftyp_switch = Wasm.Type.(SubT (Final, [||], DefFuncT (
    FuncT ([|RefT (Null, EqHT); RefT (Null, VarHT process_active_tid)|], [|RefT (Null, EqHT)|]))))
  let process_waiting_tid : int32 = 10l
  let process_waiting_typ = Wasm.Type.(SubT (Final, [||], DefContT (ContT (VarHT process_waiting_ftid))))
  let waiting_list_tid : int32 = 11l
  let waiting_list_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Var, ValStorageT (RefT (NoNull, VarHT pid_tid)));
    FieldT (Var, ValStorageT (RefT (NoNull, VarHT process_waiting_tid)));
    FieldT (Var, ValStorageT (RefT (Null, VarHT waiting_list_tid)));
  |])))
  let pid_active_tid : int32 = 12l
  let pid_active_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Var, PackStorageT Wasm.Pack.Pack8); (* Blocked? *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT list_tid))); (* Push mailbox (Send) *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT list_tid))); (* Pop mailbox (recv) *)
    FieldT (Var, ValStorageT (RefT (Null, VarHT waiting_list_tid))); (* Waiting list *)
  |])))
  let process_list_tid : int32 = 13l
  let process_list_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT pid_tid)));
    FieldT (Var, ValStorageT (RefT (Null, VarHT process_active_tid)));
    FieldT (Var, ValStorageT (RefT (Null, VarHT process_list_tid)));
  |])))
  let pid_list_tid : int32 = 14l
  let pid_list_typ = Wasm.Type.(SubT (Final, [||], DefStructT (StructT [|
    FieldT (Cons, ValStorageT (RefT (NoNull, VarHT pid_tid)));
    FieldT (Cons, ValStorageT (RefT (Null, VarHT pid_list_tid)));
  |])))
  
  let empty (process_level : process_level) (use_switch : bool) : t =
    let reftyps =
      match process_level with
      | PL_NoProcess -> Wasm.Type.[
        (* 5 *) RecT [|SubT (Final, [||], DefStructT (StructT [|FieldT (Cons, ValStorageT (NumT F64T))|]))|];
        (* 4 *) RecT [|SubT (Final, [||], DefStructT (StructT [|FieldT (Cons, ValStorageT (NumT I64T))|]))|];
        (* 3 *) RecT [|string_typ|];
        (* 2 *) RecT [|list_typ|];
        (* 1 *) RecT [|variant_typ|];
        (* 0 *) RecT [|SubT (Final, [||], DefFuncT (FuncT ([||], [||])))|];
      ]
      | _ -> Wasm.Type.[
        (* 14 *) RecT [|pid_list_typ|];
        (* 13 *) RecT [|process_list_typ|];
        (* 12 *) RecT [|pid_active_typ|];
        (* 11 *) RecT [|waiting_list_typ|];
        (* 10 *) RecT [|process_waiting_typ|];
        (* 9 *) RecT [|(if use_switch then process_waiting_ftyp_switch else process_waiting_ftyp_on)|];
        (* 7-8 *) RecT [|(if use_switch then process_active_ftyp_switch else process_active_ftyp_on); process_active_typ|];
        (* 6 *) RecT [|pid_typ|];
        (* 5 *) RecT [|SubT (Final, [||], DefStructT (StructT [|FieldT (Cons, ValStorageT (NumT F64T))|]))|];
        (* 4 *) RecT [|SubT (Final, [||], DefStructT (StructT [|FieldT (Cons, ValStorageT (NumT I64T))|]))|];
        (* 3 *) RecT [|string_typ|];
        (* 2 *) RecT [|list_typ|];
        (* 1 *) RecT [|variant_typ|];
        (* 0 *) RecT [|SubT (Final, [||], DefFuncT (FuncT ([||], [||])))|];
      ] in
    {
      use_switch;
      process_level;
      cenv = TypeMap.of_list [
        Type TString, string_tid;
        Type TVariant, variant_tid;
      ];
      eenv = TypeMap.empty;
      nrefs = Int32.of_int (List.fold_left (fun acc (Wasm.Type.RecT l) -> acc + Array.length l) 0 reftyps);
      reftyps;
    }
  
  let recid_of_sub_type (env : t) (t : Wasm.Type.sub_type) : int32 =
    let rec inner env reftyps nnext = match reftyps with
      | Wasm.Type.RecT hd :: tl ->
          let rec inner2 env tl reftyps i nnext =
            if i = 0 then inner env tl nnext else
            let n = Int32.pred nnext in
            let i = Int.pred i in
            if t = reftyps.(i) then n
            else inner2 env tl reftyps i n in
          inner2 env tl hd (Array.length hd) nnext
      | [] ->
          let tidx = env.nrefs in
          let nrefs = Int32.succ tidx in
          env.nrefs <- nrefs; env.reftyps <- Wasm.Type.RecT [|t|] :: env.reftyps; tidx in
    inner env env.reftyps env.nrefs
  
  let rec val_of_type : type a. t -> a typ -> Wasm.Type.val_type = fun (env : t) (t : a typ) : Wasm.Type.val_type ->
    let open Wasm.Type in match t with
    | TTuple 0 -> RefT (Null, NoneHT)
    | TInt -> NumT I64T
    | TBool -> NumT I32T
    | TFloat -> NumT F64T
    | TString -> RefT (NoNull, VarHT string_tid)
    | TClosed _ -> RefT (NoNull, VarHT (recid_of_type env t))
    | TAbsClosArg -> RefT (Null, StructHT)
    | TClosArg _ -> RefT (NoNull, VarHT (recid_of_type env t))
    | TCont _ -> RefT (NoNull, VarHT (recid_of_type env t))
    | TTuple _ -> RefT (NoNull, VarHT (recid_of_type env t))
    | TVariant -> RefT (NoNull, VarHT variant_tid)
    | TList _ -> RefT (Null, VarHT list_tid)
    | TVar -> RefT (Null, EqHT)
    | TSpawnLocation -> RefT (Null, NoneHT) (* SpawnLocation is isomorphic to an empty tuple for now *)
    | TProcess -> RefT (Null, VarHT pid_tid)
  
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
          | TTuple 0 -> failwith "TODO: TMap.recid_of_type TTuple 0"
          | TInt -> failwith "TODO: TMap.recid_of_type TInt"
          | TBool -> failwith "TODO: TMap.recid_of_type TBool"
          | TFloat -> failwith "TODO: TMap.recid_of_type TFloat" (* cached *)
          | TString -> Some string_typ (* cached *)
          | TClosed (args, ret) ->
              let ftyp = recid_of_functyp env args ret in
              Some (SubT (Final, [||], DefStructT (StructT [|
                FieldT (Cons, ValStorageT (RefT (NoNull, VarHT ftyp)));
                FieldT (Cons, ValStorageT (RefT (Null, StructHT)));
              |])))
          | TAbsClosArg -> failwith "TODO: TMap.recid_of_type TAbsClosArg"
          | TClosArg TLnil -> None (* Never actually used, so we can safely return garbage *)
          | TClosArg clos ->
              let clos = val_list_of_type_list env clos in
              Some (SubT (Final, [||], DefStructT (StructT (Array.map (fun t -> FieldT (Cons, ValStorageT t)) (Array.of_list clos)))))
          | TCont cret ->
              let ftyp = recid_of_cfunctyp env cret in
              Some (SubT (Final, [||], DefContT (ContT (VarHT ftyp))))
          | TTuple n ->
              Some (SubT (Final, [||], DefStructT (StructT (Array.make n (FieldT (Cons, ValStorageT (RefT (Null, EqHT))))))))
          | TVariant -> Some variant_typ (* cached *)
          | TList _ -> Some list_typ
          | TVar -> failwith "TODO: TMap.recid_of_type TVar"
          | TSpawnLocation -> failwith "TODO: TMap.recid_of_type TSpawnLocation"
          | TProcess -> Some pid_typ (* cached *) in
        begin match st with
        | None -> Int32.minus_one
        | Some st ->
            let tid = recid_of_sub_type env st in
            env.cenv <- TypeMap.add (Type t) tid env.cenv;
            tid
      end
  
  and recid_of_functyp : type a b. t -> a typ_list -> b typ -> int32 = fun (env : t) (args : a typ_list) (ret : b typ) : int32 ->
    let open Wasm.Type in
    let args = val_list_of_type_list env args in
    let args = Array.of_list (args @ [RefT (Null, StructHT)]) in
    let ret = val_of_type env ret in
    let t = SubT (Final, [||], DefFuncT (FuncT (args, [|ret|]))) in
    recid_of_sub_type env t
  
  and recid_of_cfunctyp : type b. t -> b typ -> int32 = fun (env : t) (ret : b typ) : int32 ->
    let ret = val_of_type env ret in
    let open Wasm.Type in
    let t = SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT)|], [|ret|]))) in
    recid_of_sub_type env t
  
  (* TODO: optimize ()s away? *)
  let oval_of_type (env : t) (t : 'a typ) : Wasm.Type.val_type option =
    Some (val_of_type env t)
  
  let recids_of_handler (env : t) (TCont cret : 'a continuation typ) (tis : 'c typ_list) (tret : 'b typ) : int32 * int32 =
    let open Wasm.Type in
    let cret = val_of_type env cret in
    let tret = val_of_type env tret in
    let tis = val_list_of_type_list env tis in
    let rawid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT)|], [|cret|])))) in
    let contid = recid_of_sub_type env (SubT (Final, [||], DefContT (ContT (VarHT rawid)))) in
    let hdlid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT (
      Array.of_list (RefT (NoNull, VarHT contid) :: RefT (Null, EqHT) :: tis @ [RefT (Null, StructHT)]),
      [|tret|])))) in
    contid, hdlid
  let recid_of_handler_block (env : t) (contid : int32) (_ : 'a typ_list) : int32 =
    let open Wasm.Type in
    let eargs = [|RefT (Null, EqHT); RefT (NoNull, VarHT contid)|] in
    recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT); RefT (NoNull, VarHT contid)|], eargs))))
  let recid_of_handler_finish (env : t) (contid : int32) (eret : 'a typ) : int32 =
    let eret = val_of_type env eret in
    let open Wasm.Type in
    recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT); RefT (NoNull, VarHT contid)|], [|eret|]))))
  
  let recid_of_exported_type (env : t) (targs : 'a typ_list) (tret : 'b typ) : int32 =
    match TypeMap.find_opt (Type (TClosed (targs, tret))) env.eenv with
    | Some n -> n | None ->
        let open Wasm.Type in
        let targs' = val_list_of_type_list env targs in
        let targs' = Array.of_list targs' in
        let tret' = val_of_type env tret in
        let ret = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT (targs', [|tret'|])))) in
        env.eenv <- TypeMap.add (Type (TClosed (targs, tret))) ret env.eenv;
        ret
  
  let to_wasm (env : t) : Wasm.Type.rec_type array = Array.of_list (List.rev (env.reftyps))
  
  let spawn_offset : int32 = 0l
  let self_offset : int32 = 1l
  let yield_offset : int32 = 2l
  let wait_offset : int32 = 3l
  let exit_offset : int32 = 4l
  let sched_effects (env : t) : int32 list = let open Wasm.Type in
    let spawn_tid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT (
        [|RefT (NoNull, VarHT (recid_of_type env (TClosed (TLnil, TVar))))|],
        (if env.use_switch then [|RefT (NoNull, VarHT pid_tid); RefT (Null, VarHT process_active_tid)|] else [|RefT (NoNull, VarHT pid_tid)|])
      )))) in
    let self_tid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([||], [|RefT (NoNull, VarHT pid_tid)|])))) in
    let yield_tid =
      if env.use_switch
      then recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([||], [|RefT (Null, EqHT)|]))))
      else main_func_type in
    let wait_tid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT (
        [|RefT (NoNull, VarHT pid_active_tid)|],
        (if env.use_switch then [|RefT (Null, EqHT); RefT (Null, VarHT process_active_tid)|] else [|RefT (Null, EqHT)|])
      )))) in
    let exit_tid = recid_of_sub_type env (SubT (Final, [||], DefFuncT (FuncT ([||], [|RefT (Null, EqHT)|])))) in
    [
      spawn_tid;
      self_tid;
      yield_tid;
      wait_tid;
      exit_tid;
    ]
end
type tmap = TMap.t

(* The following functions build the instructions in reverse order *)
type 'a nlist = int * 'a list
let empty_nlist : 'a nlist = 0, []
let nlist_of_list (l : 'a list) : 'a nlist = List.length l, l
let convert_straight_nlist ((len, is) : 'a nlist) : 'a array =
  if len = 0 then [||] else
  let ret = Array.make len (List.hd is) in
  List.iteri (fun i v -> ret.(i) <- v) is;
  ret
let convert_nlist ((len, is) : 'a nlist) : 'a array =
  if len = 0 then [||] else
  let ret = Array.make len (List.hd is) in
  let rec inner ret is i = match is with
    | [] -> ()
    | hd :: tl -> let i = Int.pred i in ret.(i) <- hd; inner ret tl i in
  inner ret is len;
  ret
let (^+) (hd : 'a) ((n, tl) : 'a nlist) : 'a nlist = (Int.succ n, hd :: tl)
let (@+) ((nl, ll) : 'a nlist) ((nr, lr) : 'a nlist) : 'a nlist = (nl + nr, ll @ lr)

type instr_builder = Wasm.instr nlist
type instr_conv = instr_builder -> instr_builder

module NewMetadata : sig
  type g
  type t
  
  val g_of_t : t -> g
  
  val fun_offset : g -> int32
  val effect_offset : g -> int32
  
  val empty_global : nimports:int32 -> nfuns:int32 -> neffects:int32 -> nglob:int32 -> g
  val wasm_funs : g -> Wasm.fundef list
  val wasm_globals : g -> Wasm.global list
  val wasm_elems : g -> Wasm.elem_segment list
  
  val extend : g -> int32 -> Wasm.Type.val_type list -> t
  val wasm_locals : tmap -> t -> Wasm.Type.val_type array
  
  val find_list_concat : tmap -> g -> int32
  val find_eq_fun : tmap -> g -> 'a typ -> int32
  
  val add_function : g -> Wasm.fundef option ref * int32
  val add_export_function : g -> int32 -> unit
  
  val mark_recursive : t -> unit
  val is_recursive : t -> bool
  
  val add_local : tmap -> t -> 'a typ -> int32
  
  val add_global : tmap -> g -> string option -> 'a typ -> Wasm.instrs -> int32
  val add_raw_global : g -> Wasm.global -> int32
  
  val [@warning "-32"] add_specialization : tmap -> g -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> int32
  val [@warning "-32"] add_unspecialization : tmap -> g -> 'a typ_list -> 'b typ -> ('a, 'c) box_list -> ('b, 'd) box -> 'c typ_list * 'd typ * int32
  val maybe_do_box : tmap -> t -> ('a, 'b) box -> instr_conv -> (instr_conv, ('a, 'b) Type.eq) Either.t
  val do_box : tmap -> 'a typ -> instr_conv -> (instr_conv, ('a, unit) Type.eq) Either.t
  val maybe_do_unbox : tmap -> t -> ('a, 'b) box -> instr_conv -> (instr_conv, ('a, 'b) Type.eq) Either.t
  val do_unbox : tmap -> 'a typ -> instr_conv -> (instr_conv, ('a, unit) Type.eq) Either.t
end = struct
  module Int32Map = Map.Make(Int32)
  
  type g = {
    nimports: int32;
    mutable nfuns: int32;
    mutable funs: Wasm.fundef option ref list;
    mutable exps: bool Int32Map.t;
    mutable eqfuns: int32 TypeMap.t;
    mutable list_concat: int32 option;
    mutable nglob: int32;
    mutable gbls: Wasm.global list;
    neffects: int32;
  }
  type t = g * (Wasm.Type.val_type list * int32) ref * bool ref
  
  let g_of_t (glob, _, _ : t) : g = glob
  
  let fun_offset ({ nimports; _ } : g) : int32 = nimports
  let effect_offset ({ neffects; _ } : g) : int32 = neffects
  
  let empty_global ~(nimports : int32) ~(nfuns : int32) ~(neffects : int32) ~(nglob : int32) : g = {
    nimports;
    nfuns;
    funs = [];
    exps = Int32Map.empty;
    eqfuns = TypeMap.empty;
    list_concat = None;
    nglob;
    gbls = [];
    neffects;
  }
  
  let wasm_funs ({ funs; _ } : g) : Wasm.fundef list =
    List.rev_map
      (function {contents = Some f} -> f | {contents = None} -> raise (internal_error "Function was not assigned"))
      funs
  let wasm_globals ({ gbls; _ } : g) : Wasm.global list =
    List.rev gbls
  let wasm_elems ({ nimports; exps; _ } : g) : Wasm.elem_segment list =
    let felems =
      if Int32Map.is_empty exps then [] else [Wasm.{
        es_type = Type.(NoNull, FuncHT);
        es_init = Int32Map.fold
            (fun fid b acc ->
              if b then Instruction.[|RefFunc (Int32.add nimports fid)|] :: acc
              else acc)
            exps []
            |> List.rev
            |> Array.of_list;
          es_mode = Declarative;
        }
      ] in
    felems
  
  let extend (glob : g) (nparams : int32) (base : Wasm.Type.val_type list) : t =
    let rec inner glob base acc n : t = match base with
      | [] -> glob, ref (acc, n), ref false
      | hd :: tl -> inner glob tl (hd :: acc) (Int32.succ n)
    in inner glob base [] nparams
  let wasm_locals (_ : tmap) (_, { contents = (l, _) }, _ : t) : Wasm.Type.val_type array =
    Array.of_list (List.rev l)
  
  let add_function (glob : g) : Wasm.fundef option ref * int32 =
    let fref = ref None in
    let fid = glob.nfuns in
    glob.nfuns <- Int32.succ fid;
    glob.funs <- fref :: glob.funs;
    fref, fid
  
  let add_export_function (glob : g) (fid : int32) : unit = glob.exps <- Int32Map.add fid true glob.exps
  
  let mark_recursive (_, _, recursive : t) : unit = recursive := true
  let is_recursive (_, _, recursive : t) : bool = !recursive
  
  let add_raw_local (_, new_meta, _ : t) (t : Wasm.Type.val_type) : int32 =
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
  let add_global (tm : tmap) (glob : g) (oname : string option) (t : 'a typ) (init : Wasm.instrs) : int32 =
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
    glob.exps <- Int32Map.add fid false glob.exps;
    (* TODO: optimize the next two lines (caching is possible) *)
    let inner_ftid = TMap.recid_of_type tm (TClosed (targs, tret)) in
    let inner_fid = TMap.recid_of_functyp tm targs tret in
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT inner_ftid)) in
    let new_meta = extend glob (Int32.succ clvid) [inner_fval] in
    let conv_closure = nlist_of_list Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT inner_ftid);
      LocalGet clvid
    ] in
    let load_args =
      let open Wasm.Instruction in
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 -> t nlist -> t nlist =
        fun t box i acc -> match t, box with
        | TLnil, _ -> acc
        | TLcons (_, ttl), BLnone -> inner ttl BLnone (Int32.succ i) (LocalGet i ^+ acc)
        | TLcons (_, ttl), BLcons (bhd, btl) ->
          let arg = match maybe_do_unbox tm new_meta bhd (fun acc -> LocalGet i ^+ acc) with
            | Either.Right Type.Equal -> LocalGet i ^+ acc
            | Either.Left arg -> arg acc
          in inner ttl btl (Int32.succ i) arg in
      inner targs bargs 0l conv_closure in
    let do_unbox =
      let do_call = Wasm.Instruction.(fun acc ->
          CallRef inner_fid ^+
          StructGet (inner_ftid, 0l, None) ^+ LocalGet (Int32.succ clvid) ^+
          StructGet (inner_ftid, 1l, None) ^+ LocalGet (Int32.succ clvid) ^+
          load_args @+ acc) in
      match maybe_do_box tm new_meta bret do_call with
      | Either.Right Type.Equal -> do_call empty_nlist
      | Either.Left conv -> conv empty_nlist in
    fref := Some Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = wasm_locals tm new_meta;
      fn_code = convert_nlist do_unbox;
    };
    fargs, fret, fid
  
  (* get_val is affine and only called on the accumulator *)
  and do_box : type a. tmap -> a typ -> instr_conv -> (instr_conv, (a, unit) Type.eq) Either.t =
    fun _tm box get_val ->
    let open Wasm.Instruction in match box with
    | TVar -> Either.Right Type.Equal
    | TInt -> Either.Left (fun acc -> StructNew (TMap.boxed_int_tid, Explicit) ^+ get_val acc)
    | TBool -> Either.Left (fun acc -> RefI31 ^+ get_val acc)
    | TFloat -> Either.Left (fun acc -> StructNew (TMap.boxed_float_tid, Explicit) ^+ get_val acc)
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
        Either.Left (fun acc -> StructNew (new_ctid, Explicit) ^+ get_val (RefFunc (Int32.add (fun_offset (g_of_t new_meta)) convfid) ^+ acc))
    | BCont _ -> failwith "TODO maybe_do_box BCont"
    | BTuple _ -> Either.Left get_val (* Tuples always have their values boxed *)
    | BBox t -> do_box tm t get_val
  
  (* Warning: not the same ABI (the closure content is not wrapped again) *)
  (* TODO: add caching *)
  and add_specialization : type a b c d. tmap -> g -> a typ_list -> b typ -> (a, c) box_list -> (b, d) box -> int32 =
    fun tm glob targs tret bargs bret ->
    let fref, fid = add_function glob in
    let ftid = TMap.recid_of_functyp tm targs tret in
    glob.exps <- Int32Map.add fid false glob.exps;
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
    let inner_fval = Wasm.Type.(RefT (NoNull, VarHT inner_ftid)) in
    let new_meta = extend glob (Int32.succ clvid) [inner_fval] in
    let conv_closure = nlist_of_list Wasm.Instruction.[
      LocalSet (Int32.succ clvid);
      RefCast Wasm.Type.(NoNull, VarHT inner_ftid);
      LocalGet clvid
    ] in
    let load_args =
      let open Wasm.Instruction in
      let rec inner : type a b. a typ_list -> (a, b) box_list -> int32 -> t nlist -> t nlist =
        fun t box i acc -> match t, box with
        | TLnil, _ -> acc
        | TLcons (_, ttl), BLnone -> inner ttl BLnone (Int32.succ i) (LocalGet i ^+ acc)
        | TLcons (_, ttl), BLcons (bhd, btl) ->
          let arg = match maybe_do_box tm new_meta bhd (fun acc -> LocalGet i ^+ acc) with
            | Either.Right Type.Equal -> LocalGet i ^+ acc
            | Either.Left arg -> arg acc
          in inner ttl btl (Int32.succ i) arg in
      inner targs bargs 0l conv_closure in
    let do_unbox =
      let do_call = Wasm.Instruction.(fun acc ->
        CallRef inner_fid ^+
        StructGet (inner_ftid, 0l, None) ^+ LocalGet (Int32.succ clvid) ^+
        StructGet (inner_ftid, 1l, None) ^+ LocalGet (Int32.succ clvid) ^+
        load_args @+ acc) in
      match maybe_do_unbox tm new_meta bret do_call with
      | Either.Right Type.Equal -> do_call empty_nlist
      | Either.Left conv -> conv empty_nlist in
    fref := Some Wasm.{
      fn_name = None;
      fn_type = ftid;
      fn_locals = wasm_locals tm new_meta;
      fn_code = convert_nlist do_unbox;
    };
    fid
  
  (* get_val is affine and only called on the accumulator *)
  and do_unbox : type a. tmap -> a typ -> instr_conv -> (instr_conv, (a, unit) Type.eq) Either.t =
    fun tm box get_val ->
    let open Wasm.Instruction in match box with
    | TVar -> Either.Right Type.Equal
    | TInt -> Either.Left (fun acc ->
        StructGet (TMap.boxed_int_tid, 0l, None) ^+ RefCast Wasm.Type.(NoNull, VarHT TMap.boxed_int_tid) ^+ get_val acc)
    | TBool -> Either.Left (fun acc -> I31Get Wasm.Pack.ZX ^+ RefCast Wasm.Type.(NoNull, I31HT) ^+ get_val acc)
    | TFloat -> Either.Left (fun acc ->
        StructGet (TMap.boxed_float_tid, 0l, None) ^+ RefCast Wasm.Type.(NoNull, VarHT TMap.boxed_float_tid) ^+ get_val acc)
    | (TTuple 0) -> Either.Left (fun acc -> RefNull Wasm.Type.NoneHT ^+ Drop ^+ get_val acc)
        (* fun acc -> RefCast Wasm.Type.(Null, NoneHT) ^+ get_val acc) *)
    | TList _ -> Either.Left (fun acc -> RefCast Wasm.Type.(Null, VarHT TMap.list_tid) ^+ get_val acc)
    | t -> let tid = TMap.recid_of_type tm t in Either.Left (fun acc -> RefCast Wasm.Type.(NoNull, VarHT tid) ^+ get_val acc)
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
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) ^+ get_val (RefFunc (Int32.add (fun_offset (g_of_t new_meta)) convfid) ^+ acc))
        | Either.Right Type.Equal, Either.Left bret ->
            let convfid = add_specialization tm (g_of_t new_meta) targs tret BLnone bret in
            add_export_function (g_of_t new_meta) convfid;
            let new_ctid = TMap.recid_of_type tm tsrc in
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) ^+ get_val (RefFunc (Int32.add (fun_offset (g_of_t new_meta)) convfid) ^+ acc))
        | Either.Left bargs, Either.Left bret ->
            let convfid = add_specialization tm (g_of_t new_meta) targs tret bargs bret in
            add_export_function (g_of_t new_meta) convfid;
            let new_ctid = TMap.recid_of_type tm tsrc in
            Either.Left (fun acc -> StructNew (new_ctid, Explicit) ^+ get_val (RefFunc (Int32.add (fun_offset (g_of_t new_meta)) convfid) ^+ acc))
      end
    | BCont _ -> failwith "TODO maybe_do_unbox BCont"
    | BTuple _ -> Either.Left get_val (* Tuples always have their values boxed *)
    | BBox t -> do_unbox tm t get_val
  
  let find_list_concat (tm : tmap) (glob : g) : int32 = match glob.list_concat with
    | Some i -> i
    | None ->
        let fid = glob.nfuns in
        let ftid = TMap.recid_of_exported_type tm (TLcons (TList TVar, TLcons (TList TVar, TLnil))) (TList TVar) in
        let f = Wasm.{
          fn_name = None; fn_type = ftid; fn_locals = [||]; fn_code = Instruction.[|
            Block (Type.ValBlockType None, [|
              LocalGet 0l;
              BrOnNull 0l;
              StructGet (TMap.list_tid, 0l, None);
              LocalGet 0l;
              RefAsNonNull;
              StructGet (TMap.list_tid, 1l, None);
              LocalGet 1l;
              Call (Int32.add (fun_offset glob) fid);
              StructNew (TMap.list_tid, Explicit);
              Return;
            |]);
            LocalGet 1l;
          |]
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
        let () = match t with
          | TTuple 0 ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [||];
                fn_code = Instruction.[|Const Value.(I32 I32.one)|];
              }
          | TInt ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [||];
                fn_code = Instruction.[|LocalGet 0l; LocalGet 1l; Relop (Value.I64 IntOp.Eq)|];
              }
          | TBool ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [||];
                fn_code = Instruction.[|LocalGet 0l; LocalGet 1l; Relop (Value.I32 IntOp.Eq)|];
              }
          | TFloat ->
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = [||];
                fn_code = Instruction.[|LocalGet 0l; LocalGet 1l; Relop (Value.F64 FloatOp.Eq)|];
              }
          | TList t ->
              let recid = find_eq_fun tm glob t in
              let new_meta = extend glob 3l Wasm.Type.[RefT (NoNull, VarHT TMap.list_tid); RefT (NoNull, VarHT TMap.list_tid)] in
              let unbox i =
                let get_val = Wasm.Instruction.(fun acc -> StructGet (TMap.list_tid, 0l, None) ^+ LocalGet i ^+ acc) in
                match do_unbox tm t get_val with
                | Either.Left v -> v
                | Either.Right Type.Equal -> get_val in
              fref := Some Wasm.{
                fn_type = ft;
                fn_name = None;
                fn_locals = wasm_locals tm new_meta;
                fn_code = Type.(Value.(Instruction.[|
                  Block (ValBlockType None, [|
                    Block (ValBlockType None, convert_nlist (
                      If (ValBlockType None, [|
                        LocalGet 2l;
                        StructGet (TMap.list_tid, 1l, None);
                        LocalGet 3l;
                        StructGet (TMap.list_tid, 1l, None);
                        ReturnCall (Int32.add (fun_offset glob) funid);
                      |], [|
                        Const (I32 I32.zero);
                        Return;
                      |]) ^+
                      Call (Int32.add (fun_offset glob) recid) ^+
                      unbox 3l (
                      unbox 2l (nlist_of_list [
                      LocalSet 3l;
                      BrOnNull 1l;
                      LocalGet 1l;
                      LocalSet 2l;
                      BrOnNull 0l;
                      LocalGet 0l;
                      ]))
                    ));
                    LocalGet 1l;
                    RefIsNull;
                    Return;
                  |]);
                  Const (I32 I32.zero);
                |]));
              }
          | _ -> failwith "TODO NewMetadata.find_eq_fun" in
        funid
end
type new_meta = NewMetadata.t

let convert_global (tm : tmap) ((_, Type t, name) : 'a * anytyp * string option) : Wasm.global =
  let init =
    let open Wasm.Instruction in match t with
    | TTuple 0 -> [|RefNull Wasm.Type.NoneHT|]
    | TInt -> [|Const (Wasm.Value.(I64 I64.zero))|]
    | TBool -> [|Const (Wasm.Value.(I32 I32.zero))|]
    | TFloat -> [|Const (Wasm.Value.(F64 (F64.of_float 0.)))|]
    | TString -> [|ArrayNewFixed (TMap.string_tid, 0l)|]
    | TClosed _ -> let idx = TMap.recid_of_type tm t in [|RefNull Wasm.Type.(VarHT idx)|]
    | TAbsClosArg -> raise (internal_error "Unexpected global of IR type AbsClosArg")
    | TClosArg _ -> raise (internal_error "Unexpected global of IR type ClosArg")
    | TCont _ -> failwith "TODO: convert_global TCont"
    | TTuple _ -> let idx = TMap.recid_of_type tm t in [|StructNew (idx, Implicit)|]
    | TVariant -> [|
          Const (Wasm.Value.(I32 I32.zero));
          RefNull Wasm.Type.NoneHT;
          StructNew (TMap.variant_tid, Explicit)
        |]
    | TList _ -> [|RefNull Wasm.Type.(VarHT TMap.list_tid)|]
    | TVar -> raise (internal_error "Unexpected global of IR type Var")
    | TSpawnLocation -> [|RefNull Wasm.Type.NoneHT|]
    | TProcess -> [|StructNew (TMap.pid_tid, Implicit)|] in
  let t = match t with
    | TClosed _ -> begin let open Wasm.Type in match TMap.val_of_type tm t with
        | RefT (NoNull, ht) -> RefT (Null, ht)
        | _ -> raise (internal_error "Unexpected val_of_type of TClosed")
      end
    | _ -> TMap.val_of_type tm t in
  Wasm.(Type.(GlobalT (Var, t)), init, name)
let convert_globals (tm : tmap) (gs : (mvarid * anytyp * string option) list) (tl : Wasm.global list) : Wasm.global array =
  let [@tail_mod_cons] rec inner gs = match gs with
    | [] -> tl
    | hd :: tl -> let hd = convert_global tm hd in hd :: inner tl
  in let ret = inner gs in Array.of_list ret

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

(* get_val is affine *)
let really_box (type a) (tm : tmap) (src : a typ) (get_val : instr_conv) : instr_conv =
  match NewMetadata.do_box tm src get_val with
  | Either.Left v -> v
  | Either.Right Type.Equal -> get_val

(* get_val is affine and called on the accumulator *)
let really_unbox (type a) (tm : tmap) (src : a typ) (get_val : instr_conv) : instr_conv =
  match NewMetadata.do_unbox tm src get_val with
  | Either.Left v -> v
  | Either.Right Type.Equal -> get_val

let convert_unop (type a b) (_ : tmap) (_ : new_meta) (op : (a, b) unop) (arg : instr_conv) : instr_conv =
  let open Wasm in let open Instruction in let open Value in match op with
    | UONot -> fun acc -> Testop (I32 IntOp.Eqz) ^+ arg acc
    | UONegI -> fun acc -> Binop (I64 IntOp.Sub) ^+ arg (Const (I64 I64.zero) ^+ acc)
    | UONegF -> fun acc -> Unop (F64 FloatOp.Neg) ^+ arg acc
let convert_binop (type a b c) (tm : tmap) (new_meta : new_meta) (op : (a, b, c) binop) (arg1 : instr_conv) (arg2 : instr_conv) : instr_conv =
  let open Wasm in let open Instruction in let open Value in match op with
  | BOAddI -> fun acc -> Binop (I64   IntOp.Add)  ^+ arg2 (arg1 acc)
  | BOAddF -> fun acc -> Binop (F64 FloatOp.Add)  ^+ arg2 (arg1 acc)
  | BOSubI -> fun acc -> Binop (I64   IntOp.Sub)  ^+ arg2 (arg1 acc)
  | BOSubF -> fun acc -> Binop (F64 FloatOp.Sub)  ^+ arg2 (arg1 acc)
  | BOMulI -> fun acc -> Binop (I64   IntOp.Mul)  ^+ arg2 (arg1 acc)
  | BOMulF -> fun acc -> Binop (F64 FloatOp.Mul)  ^+ arg2 (arg1 acc)
  | BODivI -> fun acc -> Binop (I64   IntOp.DivS) ^+ arg2 (arg1 acc)
  | BODivF -> fun acc -> Binop (F64 FloatOp.Div)  ^+ arg2 (arg1 acc)
  | BORemI -> fun acc -> Binop (I64   IntOp.RemS) ^+ arg2 (arg1 acc)
  | BOEq t -> begin match t with
        | TTuple 0 -> fun acc -> Const (I32 I32.one) ^+ Drop ^+ arg2 (Drop ^+ arg1 acc)
        | TInt -> fun acc -> Relop (I64 IntOp.Eq) ^+ arg2 (arg1 acc)
        | TBool -> fun acc -> Relop (I32 IntOp.Eq) ^+ arg2 (arg1 acc)
        | TFloat -> fun acc -> Relop (F64 FloatOp.Eq) ^+ arg2 (arg1 acc)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm (NewMetadata.g_of_t new_meta) t in
            fun acc -> Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid) ^+ arg2 (arg1 acc)
    end
  | BONe t -> begin match t with
        | TTuple 0 -> fun acc -> Const (I32 I32.zero) ^+ Drop ^+ arg2 (Drop ^+ arg1 acc)
        | TInt -> fun acc -> Relop (I64 IntOp.Ne) ^+ arg2 (arg1 acc)
        | TBool -> fun acc -> Relop (I32 IntOp.Ne) ^+ arg2 (arg1 acc)
        | TFloat -> fun acc -> Relop (F64 FloatOp.Ne) ^+ arg2 (arg1 acc)
        | _ ->
            let fid = NewMetadata.find_eq_fun tm (NewMetadata.g_of_t new_meta) t in
            fun acc -> Testop (I32 IntOp.Eqz) ^+ Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid) ^+ arg2 (arg1 acc)
    end
  | BOLe t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LeS) ^+ arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Le on non-integer")
    end
  | BOLt t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.LtS) ^+ arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Lt on non-integer")
    end
  | BOGe t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.GeS) ^+ arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Ge on non-integer")
    end
  | BOGt t -> begin
      match t with
      | TInt -> fun acc -> Relop (I64 IntOp.GtS) ^+ arg2 (arg1 acc)
      | _ -> raise (internal_error "Unknown binary operation Gt on non-integer")
    end
  | BOConcat ->
      let tmparg1 = NewMetadata.add_local tm new_meta TString in
      let tmparg2 = NewMetadata.add_local tm new_meta TString in
      let tmpret = NewMetadata.add_local tm new_meta TString in
      fun acc ->
        LocalGet tmpret ^+
        ArrayCopy (TMap.string_tid, TMap.string_tid) ^+
        ArrayLen ^+ LocalGet tmparg2 ^+
        Const (I32 I32.zero) ^+
        LocalGet tmparg2 ^+
        ArrayLen ^+ LocalGet tmparg1 ^+
        LocalGet tmpret ^+
        ArrayCopy (TMap.string_tid, TMap.string_tid) ^+
        ArrayLen ^+ LocalGet tmparg1 ^+
        Const (I32 I32.zero) ^+
        LocalGet tmparg1 ^+
        Const (I32 I32.zero) ^+
        LocalTee tmpret ^+
        ArrayNew (TMap.string_tid, Implicit) ^+
        Binop (I32 IntOp.Add) ^+
        ArrayLen ^+ LocalTee tmparg2 ^+ arg2 (
        ArrayLen ^+ LocalTee tmparg1 ^+ arg1 acc)
  | BOCons t ->
      let arg1 = really_box tm t arg1 in
      fun acc -> StructNew (TMap.list_tid, Explicit) ^+ arg2 (arg1 acc)
  | BOConcatList _ ->
      let fid = NewMetadata.find_list_concat tm (NewMetadata.g_of_t new_meta) in
      fun acc -> Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid) ^+ arg2 (arg1 acc)

type 'a anybox_list = AnyBoxList : ('a, 'b) box_list -> 'a anybox_list

let optimize_size
  = Settings.(flag ~default:true "optimize_size"
              |> synopsis "Generate a smaller output, at the cost of runtime speed"
              |> convert parse_bool
              |> sync)

(* angel threads list variable ID, maybe yield function, select next process, added local variable, post-suspend *)
type procinfo = (int32 * instr_conv * instr_conv * Wasm.Type.val_type option * (int32 -> instr_conv)) option
let gbl_count_reset = 100L (* Yield every [gbl_count_reset] calls *)
let generate_yield (procinfo : procinfo) : instr_conv = match procinfo with
  | None -> Fun.id
  | Some (_, yield_fun, _, _, _) -> yield_fun

let procinfo_angels_vid (procinfo : procinfo) : int32 = match procinfo with
  | None -> raise (internal_error "Requesting angels vid, but module is actorless")
  | Some (angels_vid, _, _, _, _) -> angels_vid

let procinfo_switch_process (procinfo : procinfo) : instr_conv = match procinfo with
  | None -> raise (internal_error "Requesting process switching instr_conv, but module is actorless")
  | Some (_, _, switch_process, _, _) -> switch_process

let procinfo_save_process (procinfo : procinfo) : Wasm.Type.val_type option * (int32 -> instr_conv) =
  match procinfo with
  | None -> raise (internal_error "Requesting process saving instr_conv, but module is actorless")
  | Some (_, _, _, oadd_loc, save_process) -> oadd_loc, save_process

type last_info = (mfunid * int32) option
let incr_depth_n (is_last : last_info) (depth : int32) : last_info = match is_last with
  | None -> is_last
  | Some (refid, jmplv) -> Some (refid, Int32.add jmplv depth)
let incr_depth (is_last : last_info) : last_info = incr_depth_n is_last 1l
type clos_info = (int32 * mvarid) option (* Closure type ID, closure ID *)
let convert_get_var' (loc : locality) (vid : int32) (cinfo : clos_info) : instr_conv =
  let open Wasm.Instruction in match loc with
  | Global -> fun acc -> GlobalGet vid ^+ acc
  | Local StorVariable -> fun acc -> LocalGet vid ^+ acc
  | Local StorClosure -> begin match cinfo with
    | None -> raise (internal_error "Variable stored in non-existent closure")
    | Some (ctid, cid) -> fun acc -> StructGet (ctid, vid, None) ^+ LocalGet (cid :> int32) ^+ acc
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
          ) ^+ e (facc acc))
  in inner ass Fun.id
and convert_expr : type a b. _ -> _ -> _ -> a expr -> (a, b) box -> _ -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (e : a expr) (box : (a, b) box) (is_last : last_info) (cinfo : clos_info) ->
  let can_early_ret = (match box with BNone -> true | _ -> false) && (Option.is_some is_last) in
  let open Wasm.Instruction in match e with
  | EUnreachable _ -> fun acc -> Unreachable ^+ acc
  | EConvertClosure (src, _) ->
      let ctid = match cinfo with None -> raise (internal_error "Closure conversion without closure info") | Some (ctid, _) -> ctid in
      fun acc -> RefCast Wasm.Type.(NoNull, VarHT ctid) ^+ LocalGet (src :> int32) ^+ acc
  | EIgnore (_, e) ->
      let e = convert_expr tm new_meta procinfo e BNone None cinfo in
      fun acc -> RefNull Wasm.Type.NoneHT ^+ Drop ^+ e acc
  | EConstInt i -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I64 (I64.of_bits i)) ^+ acc)
  | EConstBool b -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(I32 (if b then I32.one else I32.zero)) ^+ acc)
  | EConstFloat f -> do_box tm new_meta box (fun acc -> Const Wasm.Value.(F64 (F64.of_float f)) ^+ acc)
  | EConstString s -> do_box tm new_meta box (fun acc ->
        ArrayNewFixed (TMap.string_tid, Int32.of_int (String.length s)) ^+
        String.fold_left (fun acc c -> Const Wasm.Value.(I32 (I32.of_int_s (Char.code c))) ^+ acc) acc s)
  | EUnop (op, e) ->
      let arg = convert_expr tm new_meta procinfo e BNone None cinfo in
      let v = convert_unop tm new_meta op arg in
      do_box tm new_meta box v
  | EBinop (BOEq (TList _), e, EListNil _) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> RefIsNull ^+ arg1 acc)
  | EBinop (BOEq (TList _), EListNil _, e) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> RefIsNull ^+ arg1 acc)
  | EBinop (BONe (TList _), e, EListNil _) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> Testop (Wasm.Value.I32 IntOp.Eqz) ^+ RefIsNull ^+ arg1 acc)
  | EBinop (BONe (TList _), EListNil _, e) ->
      let arg1 = convert_expr tm new_meta procinfo e BNone None cinfo in
      do_box tm new_meta box (fun acc -> Testop (Wasm.Value.I32 IntOp.Eqz) ^+ RefIsNull ^+ arg1 acc)
  | EBinop (op, e1, e2) ->
      let arg1 = convert_expr tm new_meta procinfo e1 BNone None cinfo in
      let arg2 = convert_expr tm new_meta procinfo e2 BNone None cinfo in
      let v = convert_binop tm new_meta op arg1 arg2 in
      do_box tm new_meta box v
  | EVariable (loc, vid) -> do_box tm new_meta box (convert_get_var loc vid cinfo)
  | ETuple (TLnil, _, ELnil) ->
      do_box tm new_meta box (fun acc -> RefNull Wasm.Type.NoneHT ^+ acc)
  | ETuple (ts, n, es) ->
      let tid = TMap.recid_of_type tm (TTuple n) in
      let AnyBoxList bcontent =
        let rec inner : type a. a typ_list -> a anybox_list = function
          | TLnil -> AnyBoxList BLnil
          | TLcons (thd, ttl) -> let AnyBoxList btl = inner ttl in AnyBoxList (BLcons (BBox thd, btl)) in
        inner ts in
      begin match box with
        | BNone | BBox _ -> do_box tm new_meta box (convert_new_struct tm new_meta procinfo es bcontent tid false cinfo)
        | BTuple _ -> convert_new_struct tm new_meta procinfo es bcontent tid false cinfo
      end
  | EExtract (e, (_, n, i, t)) ->
      let e = convert_expr tm new_meta procinfo e BNone None cinfo in
      let tid = TMap.recid_of_type tm (TTuple n) in
      do_box tm new_meta box (really_unbox tm t (fun acc -> StructGet (tid, Int32.of_int i, None) ^+ (e acc)))
  | EVariant (tagid, targ, arg) ->
      let arg = convert_expr tm new_meta procinfo arg (BBox targ) None cinfo in
      do_box tm new_meta box (fun acc ->
        StructNew (TMap.variant_tid, Explicit) ^+
        arg (Const Wasm.Value.(I32 (I32.of_int_u (tagid :> int))) ^+ acc))
  | ECase (v, _, [_, Type btyp, bid, blk], None) -> (* Optimization: assume value has the correct tag *)
      let v = convert_expr tm new_meta procinfo v BNone None cinfo in
      let unbox = really_unbox tm btyp (fun acc -> StructGet (TMap.variant_tid, 1l, None) ^+ v acc) in
      let blk = convert_block tm new_meta procinfo blk BNone is_last cinfo in
      do_box tm new_meta box (fun acc -> blk (LocalSet (bid :> int32) ^+ unbox acc))
  | ECase (v, _, [], None) -> (* Optimization: get the value, then unreachable *)
      let v = convert_expr tm new_meta procinfo v BNone None cinfo in
      do_box tm new_meta box (fun acc -> Unreachable ^+ v acc)
  | ECase (v, _, [], Some (bid, blk)) -> (* Optimization: get the value, then evaluate the default branch *)
      let v = convert_expr tm new_meta procinfo v BNone None cinfo in
      let blk = convert_block tm new_meta procinfo blk BNone is_last cinfo in
      do_box tm new_meta box (fun acc -> blk (LocalSet (bid :> int32) ^+ v acc))
  | ECase (v, t, cs, od) ->
      let loc, vid, stgv = match v with
        | EVariable (loc, vid) ->
            let _, vid = (vid : _ varid :> _ * int32) in
            loc, vid, (fun acc -> LocalGet vid ^+ acc)
        | _ ->
            let tmpvar = NewMetadata.add_local tm new_meta TVariant in
            let v = convert_expr tm new_meta procinfo v BNone None cinfo in
            Local StorVariable, tmpvar, (fun acc -> LocalTee tmpvar ^+ v acc)
      in
      let branches, ncases, min_id =
        List.fold_left
          (fun (branches, ncases, min_id) (id, _, _, _) ->
            let id = (id : tagid :> int) in
            let branches =
              if Array.length branches <= id then Array.init (id + 1) (fun j -> if j < Array.length branches then branches.(j) else None)
              else branches in
            branches.(id) <- Some ncases;
            let ncases = Int32.succ ncases in
            let min_id = Int.min min_id id in
            branches, ncases, min_id)
          ([||], 0l, Int.max_int) cs in
      let min_id = if min_id <= 3 then 0 else min_id in
      let block_content =
        StructGet (TMap.variant_tid, 0l, None) ^+
        stgv empty_nlist in
      let block_content =
        if min_id = 0 then BrTable (Array.map (Option.value ~default:ncases) branches, ncases) ^+ block_content
        else
          BrTable (
            Array.init
              (Array.length branches - min_id)
              (fun i -> match branches.(i + min_id) with None -> ncases | Some v -> v),
            ncases) ^+
          Binop Wasm.Value.(I32 IntOp.Sub) ^+
          Const Wasm.Value.(I32 (I32.of_int_u min_id)) ^+
          block_content in
      let block_content, _ = List.fold_left
        (fun (block_content, depth) (_, Type btyp, bid, blk) ->
          let br = Int32.sub ncases depth in
          let depth = Int32.succ depth in
          let block_content = nlist_of_list [Block (Wasm.Type.ValBlockType None, convert_nlist block_content)] in
          let unbox = really_unbox tm btyp (fun acc -> StructGet (TMap.variant_tid, 1l, None) ^+ convert_get_var' loc vid cinfo acc) in
          let new_block =
            LocalSet (bid : mvarid :> int32) ^+
            unbox block_content in
          let blk = convert_block tm new_meta procinfo blk BNone (incr_depth_n is_last (Int32.succ br)) cinfo in
          Br br ^+ blk new_block, depth)
        (block_content, 0l) cs in
      let block_content =
        let block_content = nlist_of_list [Block (Wasm.Type.ValBlockType None, convert_nlist block_content)] in
        match od with
        | None -> Unreachable ^+ block_content
        | Some (bid, blk) ->
          let blk = convert_block tm new_meta procinfo blk BNone (incr_depth is_last) cinfo in
            blk (LocalSet (bid :> int32) ^+ convert_get_var' loc vid cinfo block_content) in
      let tret = TMap.val_of_type tm t in
      let block_content = convert_nlist block_content in
      fun acc -> Block (Wasm.Type.ValBlockType (Some tret), block_content) ^+ acc
  | EListNil _ -> do_box tm new_meta box (fun acc -> RefNull Wasm.Type.(VarHT TMap.list_tid) ^+ acc)
  | EListHd (l, t) ->
      let l = convert_expr tm new_meta procinfo l BNone None cinfo in
      do_box tm new_meta box (really_unbox tm t (fun acc -> StructGet (TMap.list_tid, 0l, None) ^+ l acc))
  | EListTl (_, l) ->
      let l = convert_expr tm new_meta procinfo l BNone None cinfo in
      do_box tm new_meta box (fun acc -> StructGet (TMap.list_tid, 1l, None) ^+ l acc)
  | EClose (f, bcl, cls) ->
      let targs, tret, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) fid;
      let fctid = TMap.recid_of_type tm (TClosArg clts) in
      let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fctid false cinfo in
      do_box tm new_meta box (fun acc ->
        StructNew (new_ctid, Explicit) ^+
        gen_new_struct (
        RefFunc (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid) ^+
        acc))
  | ERawClose (f, cl) ->
      let targs, tret, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) fid;
      let new_ctid = TMap.recid_of_type tm (TClosed (targs, tret)) in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      do_box tm new_meta box (fun acc ->
        StructNew (new_ctid, Explicit) ^+
        get_cl (
        RefFunc (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid) ^+
        acc))
  | ESpecialize (e, _, BLnone, BNone) -> begin match box with (* Update the type to ignore generalizations *)
      | BNone -> convert_expr tm new_meta procinfo e BNone is_last cinfo
      | BClosed (TClosed (targs, tret), bargs, bret) -> convert_expr tm new_meta procinfo e (BClosed (TClosed (targs, tret), bargs, bret)) is_last cinfo
      | BBox (TClosed (bargs, bret)) -> convert_expr tm new_meta procinfo e (BBox (TClosed (bargs, bret))) is_last cinfo
    end
  | ESpecialize (e, tdst, bargs, bret) ->
      do_box tm new_meta box (do_unbox tm new_meta (BClosed (tdst, bargs, bret)) (convert_expr tm new_meta procinfo e BNone None cinfo))
  | ECallRawHandler (fid, _, contarg, targ, arg, tiargs, iargs, hdlarg, _) ->
      let fid = (fid :> int32) in
      let arg = convert_expr tm new_meta procinfo arg (BBox targ) None cinfo in
      let iargs = convert_exprs tm new_meta procinfo iargs BLnone cinfo in
      let contarg = convert_expr tm new_meta procinfo contarg BNone None cinfo in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          let set_args =
            let rec inner : type a. _ -> a typ_list -> instr_conv = fun narg ts -> match ts with
              | TLnil -> Fun.id
              | TLcons (_, ts) -> let set_tl = inner (Int32.succ narg) ts in fun acc -> LocalSet narg ^+ set_tl acc in
            inner 2l tiargs in
          do_box tm new_meta box (fun acc -> Br jmplv ^+ contarg (arg (set_args (iargs acc))))
      | _ ->
          let hdlarg = convert_expr tm new_meta procinfo hdlarg BNone None cinfo in
          do_box tm new_meta box
            (fun acc ->
              (if can_early_ret then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)
               else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)) ^+
              hdlarg (iargs (arg (contarg acc))))
    end
  | ECallClosed (ESpecialize (EClose (f, bcl, cls), _, bargs, bret), args, _) ->
      let targs, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fcid false cinfo in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          NewMetadata.mark_recursive new_meta;
          let set_args acc =
            let rec inner : type a. a typ_list -> _ = fun targs nargs -> match targs with
              | TLnil -> Int.succ nargs
              | TLcons (_, targs) -> inner targs (Int.succ nargs) in
            let nargs = inner targs 0 in
            (nargs, List.init nargs (fun i -> LocalSet (Int32.of_int i))) @+ acc in
          fun acc -> Br jmplv ^+ generate_yield procinfo (set_args (gen_new_struct (args acc)))
      | _ ->
          let unbox = do_unbox tm new_meta bret (fun acc ->
              (if can_early_ret && (match bret with BNone -> true | _ -> false)
               then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)
               else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)) ^+
              generate_yield procinfo (gen_new_struct (args acc))) in
          do_box tm new_meta box unbox
    end
  | ECallClosed (EClose (f, bcl, cls), args, _) ->
      let targs, _, clts, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let fcid = TMap.recid_of_type tm (TClosArg clts) in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      let gen_new_struct = convert_new_struct tm new_meta procinfo cls bcl fcid false cinfo in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          NewMetadata.mark_recursive new_meta;
          let set_args acc =
            let rec inner : type a. a typ_list -> _ = fun targs nargs -> match targs with
              | TLnil -> Int.succ nargs
              | TLcons (_, targs) -> inner targs (Int.succ nargs) in
            let nargs = inner targs 0 in
            (nargs, List.init nargs (fun i -> LocalSet (Int32.of_int i))) @+ acc in
          fun acc -> Br jmplv ^+ generate_yield procinfo (set_args (gen_new_struct (args acc)))
      | _ ->
          do_box tm new_meta box (fun acc ->
              (if can_early_ret then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)
               else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)) ^+
              generate_yield procinfo (gen_new_struct (args acc)))
    end
  | ECallClosed (ESpecialize (ERawClose (f, cl), _, bargs, bret), args, _) ->
      let targs, _, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      let can_early_ret = can_early_ret && (match bret with BNone -> true | _ -> false) in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          NewMetadata.mark_recursive new_meta;
          let set_args acc =
            let rec inner : type a. a typ_list -> _ = fun targs nargs -> match targs with
              | TLnil -> Int.succ nargs
              | TLcons (_, targs) -> inner targs (Int.succ nargs) in
            let nargs = inner targs 0 in
            (nargs, List.init nargs (fun i -> LocalSet (Int32.of_int i))) @+ acc in
          fun acc -> Br jmplv ^+ generate_yield procinfo (set_args (get_cl (args acc)))
      | _ ->
          let unbox = do_unbox tm new_meta bret (fun acc ->
              (if can_early_ret then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)
               else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)) ^+
              generate_yield procinfo (get_cl (args acc))) in
          do_box tm new_meta box unbox
    end
  | ECallClosed (ERawClose (f, cl), args, _) ->
      let targs, _, _, fid = (f : _ funcid :> _ * _ * _ * int32) in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      let get_cl = convert_expr tm new_meta procinfo cl BNone None cinfo in
      begin match is_last with
      | Some (refid, jmplv) when can_early_ret && Int32.equal fid (refid :> int32) ->
          NewMetadata.mark_recursive new_meta;
          let set_args acc =
            let rec inner : type a. a typ_list -> _ = fun targs nargs -> match targs with
              | TLnil -> Int.succ nargs
              | TLcons (_, targs) -> inner targs (Int.succ nargs) in
            let nargs = inner targs 0 in
            (nargs, List.init nargs (fun i -> LocalSet (Int32.of_int i))) @+ acc in
          fun acc -> Br jmplv ^+ generate_yield procinfo (set_args (get_cl (args acc)))
      | _ ->
          do_box tm new_meta box (fun acc ->
              (if can_early_ret then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)
               else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) fid)) ^+
              generate_yield procinfo (get_cl (args acc)))
    end
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
            t, (fun acc -> LocalGet vid ^+ acc), (fun acc -> LocalTee vid ^+ e acc) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let args = convert_exprs tm new_meta procinfo args bargs cinfo in
      let can_early_ret = can_early_ret && (match bret with BNone -> true | _ -> false) in
      let unbox = do_unbox tm new_meta bret (fun acc ->
          (if can_early_ret then ReturnCallRef fid
           else CallRef fid) ^+
          generate_yield procinfo (
          StructGet (vtid, 0l, None) ^+ getv (
          StructGet (vtid, 1l, None) ^+ etee (
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
            t, (fun acc -> LocalGet vid ^+ acc), (fun acc -> LocalTee vid ^+ e acc) in
      (* TODO: optimize the next two lines (caching is possible) *)
      let vtid = TMap.recid_of_type tm t in
      let fid = TMap.recid_of_functyp tm targs tret in
      let args = convert_exprs tm new_meta procinfo args BLnone cinfo in
      do_box tm new_meta box (fun acc ->
          (if can_early_ret then ReturnCallRef fid
           else CallRef fid) ^+
          generate_yield procinfo (
          StructGet (vtid, 0l, None) ^+ getv (
          StructGet (vtid, 1l, None) ^+ etee (
          args acc))))
  | ECond (e, rt, t, f) ->
      let ti = convert_block tm new_meta procinfo t box (incr_depth is_last) cinfo empty_nlist in
      let fi = convert_block tm new_meta procinfo f box (incr_depth is_last) cinfo empty_nlist in
      let ei = convert_expr tm new_meta procinfo e BNone None cinfo in
      let rt' = TMap.oval_of_type tm rt in
      fun acc -> If (Wasm.Type.(ValBlockType rt'), convert_nlist ti, convert_nlist fi) ^+ ei acc
  | EDo (eid, tret, args) ->
      let targs, n, eid = (eid : _ effectid :> _ * int * int32) in
      let args = match targs, args with
        | TLnil, ELnil -> fun acc -> RefNull Wasm.Type.NoneHT ^+ acc
        | TLcons (targ, TLnil), ELcons (arg, ELnil) -> convert_expr tm new_meta procinfo arg (BBox targ) None cinfo
        | TLcons (_, TLcons (_, _)), ELcons (_, ELcons (_, _)) ->
            let tid = TMap.recid_of_type tm (TTuple n) in
            let AnyBoxList bcontent =
              let rec inner : type a. a typ_list -> a anybox_list = function
                | TLnil -> AnyBoxList BLnil
                | TLcons (thd, ttl) -> let AnyBoxList btl = inner ttl in AnyBoxList (BLcons (BBox thd, btl)) in
              inner targs in
            convert_new_struct tm new_meta procinfo args bcontent tid false cinfo in
      let ret = really_unbox tm tret (fun acc -> Suspend eid ^+ generate_yield procinfo (args acc)) in
      do_box tm new_meta box ret
  | EShallowHandle _ -> failwith "TODO: convert_expr EShallowHandle"
  | EDeepHandle (contid, contargs, hdlid, hdlargs, iargs) ->
      let _, _, thdlcl, hdlid = (hdlid : _ funcid :> _ * _ * _ * int32) in
      let hdlcid = TMap.recid_of_type tm (TClosArg thdlcl) in
      let gen_hdl_struct = convert_new_struct tm new_meta procinfo hdlargs BLnone hdlcid false cinfo in
      let iargs = convert_exprs tm new_meta procinfo iargs BLnone cinfo in
      let _, tcret, tcontcl, contid = (contid : _ funcid :> _ * _ * _ * int32) in
      NewMetadata.add_export_function (NewMetadata.g_of_t new_meta) contid;
      let contcid = TMap.recid_of_type tm (TClosArg tcontcl) in
      let gen_cont_struct = convert_new_struct tm new_meta procinfo contargs BLnone contcid false cinfo in
      let tcontid = TMap.recid_of_type tm (TCont tcret) in
      do_box tm new_meta box (fun acc ->
        (if can_early_ret then ReturnCall (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) hdlid)
         else Call (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) hdlid)) ^+
        gen_hdl_struct (
        iargs (
        gen_cont_struct (
        ContNew tcontid ^+
        RefFunc (Int32.add (NewMetadata.fun_offset (NewMetadata.g_of_t new_meta)) contid) ^+
        acc))))
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
and convert_new_struct : type a b. _ -> _ -> _ -> a expr_list -> (a, b) box_list -> _ -> _ -> _ -> instr_conv =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (cls : a expr_list) box (fcid : int32) (needs_exact_id : bool) (cinfo : clos_info) ->
  let open Wasm.Instruction in
  match cls with
  | ELnil -> fun acc -> RefNull Wasm.Type.(if needs_exact_id then VarHT fcid else NoneHT) ^+ acc
  | ELcons _ -> let f = convert_exprs tm new_meta procinfo cls box cinfo in fun acc -> StructNew (fcid, Explicit) ^+ f acc

(* These two functions return the instructions in reverse order *)
let convert_anyblock (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
                     (b : 'a block) (is_last : mfunid option) (cinfo : clos_info) : Wasm.instr nlist =
  let b = convert_block tm new_meta procinfo b BNone (Option.map (fun fid -> (fid, 0l)) is_last) cinfo in b empty_nlist
let convert_finisher : type a b. _ -> _ -> _ -> (a, b) finisher -> _ =
  fun (tm : tmap) (new_meta : new_meta) (procinfo : procinfo)
      (f : (a, b) finisher) (is_last : last_info) (cinfo : clos_info) : Wasm.instr nlist ->
  match f with
  | FId _ -> empty_nlist
  | FMap (v, _, b) ->
      let _, v = (v : _ varid :> _ * int32) in
      let b = convert_block tm new_meta procinfo b BNone is_last cinfo in
      b (nlist_of_list Wasm.Instruction.[LocalSet v])

let convert_hdl (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (type a b c) (f : (a, c, b) fhandler) : Wasm.fundef =
  let tis = f.fh_tis in
  let nargs =
    let rec inner : type a. a typ_list -> int32 -> int32 = fun ts n -> match ts with TLnil -> n | TLcons (_, ts) -> inner ts (Int32.succ n) in
    inner tis 3l in
  let new_meta = NewMetadata.extend glob nargs (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fh_locals) in
  let (tcont, contidx), contcl = (f.fh_contarg : _ varid * mvarid :> (_ * int32) * int32) in
  let tret = match f.fh_finisher with FId t -> (t : b typ) | FMap (_, t, _) -> t in
  let contid, fun_typ = TMap.recids_of_handler tm tcont tis tret in
  let open Wasm.Instruction in
  let convert_clos, cinfo = match f.fh_closure with
    | None -> Fun.id, None
    | Some (_, (TypeList TLnil, _)) -> raise (internal_error "Invalid handler with empty closure")
    | Some (src, (TypeList clt, dst)) ->
        let clostyp = TMap.recid_of_type tm (TClosArg clt) in
        (fun acc -> LocalGet (src :> int32) ^+ RefCast Wasm.Type.(NoNull, (VarHT clostyp)) ^+ LocalSet (dst :> int32) ^+ acc), Some (clostyp, dst)
  in let code =
    let nblocks, handlers =
      let rec inner (hdls : _ handler list) len acc = match hdls with
        | [] -> len, convert_straight_nlist acc
        | Handler (eid, _, _, _) :: tl -> inner tl (Int32.succ len) ((let _, _, eid = (eid : _ effectid :> _ * _ * int32) in eid, OnLabel len) ^+ acc)
      in inner f.fh_handlers 0l empty_nlist in
    let code = convert_finisher tm new_meta procinfo f.fh_finisher (Some (f.fh_id, nblocks)) cinfo in
    let code = Return ^+ code @+ nlist_of_list [Resume (contid, handlers)] in
    let rec do_cases nblocks (cases : _ handler list) code = match cases with
      | [] -> code
      | Handler (eid, vcid, vars, blk) :: tl ->
          let nblocks = Int32.pred nblocks in
          let _, vcid = (vcid : _ varid :> _ * int32) in
          let eargs, _, _ = (eid : _ effectid :> _ * _ * _) in
          let blkid = TMap.recid_of_handler_block tm contid eargs in
          let code = nlist_of_list Wasm.Instruction.[LocalSet vcid; Block (Wasm.Type.VarBlockType blkid, convert_nlist code)] in
          let blk = convert_block tm new_meta procinfo blk BNone (Some (f.fh_id, nblocks)) cinfo in
          let code = match vars with
            | VLnil -> Drop ^+ code
            | VLcons (v, VLnil) ->
                let t, v = (v : _ varid :> _ * int32) in
                let unbox = really_unbox tm t (fun acc -> acc) in
                LocalSet v ^+ unbox code
            | VLcons (_, VLcons (_, _)) ->
                let n =
                  let rec inner : type a. a varid_list -> int -> int = fun vars acc -> match vars with
                    | VLnil -> acc
                    | VLcons (_, vtl) -> inner vtl (Int.succ acc) in
                  inner vars 0 in
                let tid = TMap.recid_of_type tm (TTuple n) in
                let tmpv = NewMetadata.add_local tm new_meta (TTuple n) in
                let rec acc_vars : type a. a varid_list -> _ = fun (vars : a varid_list) i acc -> match vars with
                  | VLnil -> acc
                  | VLcons (vhd, vtl) ->
                      let t, v = (vhd : _ varid :> _ * int32) in
                      acc_vars vtl (Int32.succ i) ((Type t, v, i) :: acc) in
                let rec convert_back acc code = match acc with
                  | [] -> raise (internal_error "Invalid convert_back argument")
                  | (Type t, v, i) :: tl ->
                      let get = really_unbox tm t (fun acc -> StructGet (tid, i, None) ^+ acc) in
                      let code = LocalSet v ^+ get code in
                      match tl with
                      | [] -> code
                      | _ :: _ -> convert_back tl (LocalGet tmpv ^+ code) in
                let vars = acc_vars vars 0l [] in
                convert_back vars (LocalTee tmpv ^+ RefCast Wasm.Type.(NoNull, VarHT tid) ^+ code) in
          let code = Return ^+ blk code in
          do_cases nblocks tl code
    in do_cases nblocks f.fh_handlers code
  in let finalblk = TMap.recid_of_handler_finish tm contid tret
  in Wasm.{
    fn_name = None;
    fn_type = fun_typ;
    fn_locals = NewMetadata.wasm_locals tm new_meta;
    fn_code = convert_straight_nlist (
      convert_clos (nlist_of_list [
      LocalGet contcl; LocalGet contidx; Loop (Wasm.Type.VarBlockType finalblk, convert_nlist code)]));
  }

let convert_builtin (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (_fid : mfunid)
                    (type g a b) (fb : (g, a, b) fbuiltin) : Wasm.fundef = match fb with
  | FBHere ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TSpawnLocation in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [||];
      fn_code = let open Type in Instruction.[|
        RefNull NoneHT;
      |];
    }
  | FBIntToString ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TInt, TLnil)) TString in
    let i32toi32 = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([|NumT I32T|], [|NumT I32T|])))) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[|NumT I64T; NumT I32T; RefT (NoNull, VarHT TMap.string_tid)|];
      fn_code = let open Value in let open Type in Instruction.[|
        Const (I32 (I32.of_int_s (Char.code '0')));
        LocalGet 0l;
        Const (I64 I64.zero);
        Relop (I64 IntOp.LtS);
        If (ValBlockType (Some (NumT I32T)), [|
          Const (I64 I64.zero);
          LocalGet 0l;
          Binop (I64 IntOp.Sub);
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Const (I32 (I32.of_bits 2l));
        |], [|
          LocalGet 0l;
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Const (I32 I32.one);
        |]);
        Block (VarBlockType i32toi32, [|
          Loop (VarBlockType i32toi32, [|
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
          |]);
        |]);
        LocalTee 3l;
        ArrayNew (TMap.string_tid, Explicit);
        LocalSet 4l;
        LocalGet 0l;
        Const (I64 I64.zero);
        Relop (I64 IntOp.LtS);
        If (ValBlockType (Some (NumT I64T)), [|
          LocalGet 4l;
          Const (I32 I32.zero);
          Const (I32 (I32.of_int_s (Char.code '-')));
          ArraySet TMap.string_tid;
          Const (I64 I64.zero);
          LocalGet 0l;
          Binop (I64 IntOp.Sub);
        |], [|
          LocalGet 0l;
        |]);
        LocalSet 2l;
        Loop (ValBlockType (Some (RefT (NoNull, VarHT TMap.string_tid))), [|
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
          Cvtop (I32 I32Op.WrapI64);
          Const (I32 (I32.of_int_s (Char.code '0')));
          Binop (I32 IntOp.Add);
          ArraySet TMap.string_tid;
          LocalGet 2l;
          Const (I64 (I64.of_bits 10L));
          Binop (I64 IntOp.DivU);
          LocalSet 2l;
          Br 0l;
        |]);
      |];
    }
  | FBLength ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TList TVar, TLnil)) TInt in
    let i64toi64 = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([|NumT I64T|], [|NumT I64T|])))) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [||];
      fn_code = let open Value in let open Type in Instruction.[|
        Const (I64 (I64.of_bits 0L));
        Loop (VarBlockType i64toi64, [|
          LocalGet 0l;
          BrOnNull 1l;
          StructGet (TMap.list_tid, 1l, None);
          LocalSet 0l;
          Const (I64 I64.one);
          Binop (I64 IntOp.Add);
          Br 0l;
        |]);
      |];
    }
  | FBRecv ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TVar in
    let switch_process = procinfo_switch_process procinfo in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[|
        RefT (NoNull, VarHT TMap.pid_active_tid);
        RefT (NoNull, VarHT TMap.list_tid);
      |];
      fn_code = let open Value in let open Type in Instruction.[|
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.self_offset);
        StructGet (TMap.pid_tid, 1l, None);
        RefCast (NoNull, VarHT TMap.pid_active_tid);
        LocalTee 1l;
        Block (ValBlockType (Some (RefT (NoNull, VarHT TMap.list_tid))), [|
          LocalGet 1l;
          StructGet (TMap.pid_active_tid, 2l, None); (* Is the pop queue empty? *)
          BrOnNonNull 0l;
          (* Yes, transfer the push queue to the pop queue *)
          Loop (ValBlockType (Some (RefT (NoNull, VarHT TMap.list_tid))), convert_nlist (
            Br 0l ^+
            (* Note that by scheduler logic, the push queue is now non-empty: transfer the push queue to the pop queue *)
            switch_process (nlist_of_list [ (* Wait until unblocked *)
            StructSet (TMap.pid_active_tid, 0l); (* Set blocked *)
            Const (I32 I32.one);
            LocalGet 1l;
            (* There is nothing in both queue: block and try again *)
            BrOnNonNull 1l; (* There was something in the push queue, which is now moved to the pop queue *)
            StructGet (TMap.pid_active_tid, 2l, None);
            Block (ValBlockType (Some (RefT (NoNull, VarHT TMap.pid_active_tid))), [|
              Loop (ValBlockType (Some (RefT (NoNull, VarHT TMap.pid_active_tid))), [|
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
              |]);
            |]);
          ])));
        |]);
        (* We have the head of the pop queue *)
        LocalTee 2l;
        StructGet (TMap.list_tid, 1l, None);
        StructSet (TMap.pid_active_tid, 2l);
        LocalGet 2l;
        StructGet (TMap.list_tid, 0l, None);
      |];
    }
  | FBSelf ->
    let fun_typ = TMap.recid_of_functyp tm TLnil TProcess in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = [||];
      fn_code = Instruction.[|
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.self_offset);
      |];
    }
  | FBSend ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TProcess, TLcons (TVar, TLnil))) (TTuple 0) in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = Type.[|
        RefT (NoNull, VarHT TMap.pid_active_tid);
      |];
      fn_code = let open Value in let open Type in Instruction.[|
        (* If the process is inactive, drop the message; otherwise, find the pid_active_tid and cache it *)
        Block (ValBlockType (Some (RefT (NoNull, VarHT TMap.pid_active_tid))), [|
          LocalGet 0l;
          StructGet (TMap.pid_tid, 1l, None);
          BrOnCast (0l, (Null, EqHT), (NoNull, VarHT TMap.pid_active_tid));
          Drop;
          RefNull NoneHT;
          Return;
        |]);
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
      |];
    }
  | FBSpawnAngelAt ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TSpawnLocation, TLcons (TClosed (TLnil, TVar), TLnil))) TProcess in
    let angels_vid = procinfo_angels_vid procinfo in
    let oadd_loc, save_process = procinfo_save_process procinfo in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = (match oadd_loc with None -> [||] | Some add_loc -> [|add_loc|]);
      fn_code = let open Instruction in convert_nlist @@
        StructGet (TMap.pid_list_tid, 0l, None) ^+
        GlobalGet angels_vid ^+
        GlobalSet angels_vid ^+
        StructNew (TMap.pid_list_tid, Explicit) ^+
        GlobalGet angels_vid ^+
        save_process 3l @@ nlist_of_list [
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset);
        LocalGet 1l;
      ];
    }
  | FBSpawnAt ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TSpawnLocation, TLcons (TClosed (TLnil, TVar), TLnil))) TProcess in
    let oadd_loc, save_process = procinfo_save_process procinfo in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = (match oadd_loc with None -> [||] | Some add_loc -> [|add_loc|]);
      fn_code = let open Instruction in convert_nlist @@
        save_process 3l @@ nlist_of_list [
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset);
        LocalGet 1l;
      ];
    }
  | FBWait ->
    let fun_typ = TMap.recid_of_functyp tm (TLcons (TProcess, TLnil)) TVar in
    let oadd_loc, save_process = procinfo_save_process procinfo in
    Wasm.{
      fn_name = None;
      fn_type = fun_typ;
      fn_locals = (match oadd_loc with None -> [||] | Some add_loc -> [|add_loc|]);
      fn_code = let open Type in let open Instruction in convert_nlist @@
        save_process 2l @@ nlist_of_list [
        Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset);
        BrOnCastFail (0l, (Null, EqHT), (NoNull, VarHT TMap.pid_active_tid));
        StructGet (TMap.pid_tid, 1l, None);
        LocalGet 0l;
      ];
    }

let convert_fun_aux (tm : tmap) (new_meta : new_meta) (procinfo : procinfo) (ft : int32) (f : 'a block)
    (init_dest : (bool * Wasm.instr nlist, mfunid * 'a typ) Either.t) (closid : (anytyp_list * mvarid) option) : int32 option * Wasm.fundef =
  let clostid, cinfo = match closid with
    | None -> None, None
    | Some (TypeList TLnil, _) -> raise (internal_error "Invalid function with empty closure")
    | Some (TypeList ct, cid) ->
        let ctid = TMap.recid_of_type tm (TClosArg ct) in
        Some ctid, Some (ctid, cid) in
  let code = convert_anyblock tm new_meta procinfo f (match init_dest with Either.Left _ -> None | Either.Right (fid, _) -> Some fid) cinfo in
  clostid, Wasm.{
    fn_name = (match init_dest with Either.Left (true, _) -> Some "main" | Either.Left (false, _) | Either.Right _ -> None);
    fn_type = ft;
    fn_locals = NewMetadata.wasm_locals tm new_meta;
    fn_code = (match init_dest with
      | Either.Left (_, app_code) -> convert_nlist (app_code @+ code)
      | Either.Right (_, tret) when NewMetadata.is_recursive new_meta ->
          let tret = TMap.val_of_type tm tret in
          Wasm.(Instruction.[|Loop (Type.(ValBlockType (Some tret)), convert_nlist code)|])
      | Either.Right _ -> convert_nlist code);
  }

let convert_fun (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (f : ('a, 'b) func') : int32 option * Wasm.fundef =
  let fun_typ = TMap.recid_of_functyp tm f.fun_args f.fun_ret in
  let nparams =
    let rec inner : type a. a typ_list -> _ = fun tl acc -> match tl with
      | TLnil -> acc
      | TLcons (_, tl) -> inner tl (Int32.succ acc) in
    inner f.fun_args 1l in
  let new_meta = NewMetadata.extend glob nparams (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fun_locals) in
  convert_fun_aux
    tm new_meta procinfo fun_typ f.fun_block
    (Either.Right (f.fun_id, f.fun_ret)) f.fun_converted_closure
let convert_fst (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo) (f : 'b fstart) : int32 option * Wasm.fundef =
  let fun_typ = TMap.recid_of_cfunctyp tm f.fst_ret in
  let new_meta = NewMetadata.extend glob 1l (List.map (fun (Type t) -> TMap.val_of_type tm t) f.fst_locals) in
  convert_fun_aux
    tm new_meta procinfo fun_typ f.fst_block
    (Either.Right (f.fst_id, f.fst_ret)) f.fst_converted_closure
let convert_fun_step2 (tm : tmap) (glob : NewMetadata.g) (f, clostyp : func * int32 option) : Wasm.fundef option = match f with
  | FContinuationStart _ | FHandler _ | FBuiltin _ -> None
  | FFunction f ->
  match f.fun_export_data with
  | None -> None
  | Some name ->
      let targs = f.fun_args in
      let tret = f.fun_ret in
      let fn_type = TMap.recid_of_exported_type tm targs tret in
      let clostyp = let open Wasm.Type in match clostyp with None -> NoneHT | Some clostid -> VarHT clostid in
      Some Wasm.{
        fn_name = Some name;
        fn_type;
        fn_locals = [||];
        fn_code = convert_nlist Wasm.Instruction.(
          ReturnCall (Int32.add (NewMetadata.fun_offset glob) (f.fun_id :> int32)) ^+
          RefNull clostyp ^+
          let rec inner : type a. _ -> _ -> a typ_list -> _ = fun i acc (ls : a typ_list) -> match ls with
            | TLnil -> acc
            | TLcons (_, ls) -> inner (Int32.succ i) (LocalGet i ^+ acc) ls
          in inner 0l empty_nlist f.fun_args);
      }
let convert_funs (tm : tmap) (glob : NewMetadata.g) (procinfo : procinfo)
                 (fs : func list) (is : NewMetadata.g -> Wasm.fundef list) : Wasm.fundef list =
  let [@tail_mod_cons] rec inner glob fs acc = match fs with
    | [] -> is glob @ List.filter_map (convert_fun_step2 tm glob) acc
    | hd :: tl ->
        let _fid, fhd, acc = match hd with
          | FFunction hd ->
              let ctid, fhd = convert_fun tm glob procinfo hd in (hd.fun_id :> int32), fhd, ((FFunction hd, ctid) :: acc)
          | FContinuationStart hd ->
              let ctid, fhd = convert_fst tm glob procinfo hd in (hd.fst_id :> int32), fhd, ((FContinuationStart hd, ctid) :: acc)
          | FHandler hd ->
              let fhd = convert_hdl tm glob procinfo hd in (hd.fh_id :> int32), fhd, acc
          | FBuiltin (fid, hd) ->
              let fhd = convert_builtin tm glob procinfo fid hd in (fid :> int32), fhd, acc in
        fhd :: inner glob tl acc
  in inner glob fs []

let convert_effects (tm : tmap) (es : EffectIDSet.t) (has_sched_effects : bool) : int32 list =
  let es = EffectIDSet.elements es in
  let generic_tid =
    let open Wasm.Type in
    TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT)|], [|RefT (Null, EqHT)|])))) in
  let convert_effect (_ : meffid) : int32 = generic_tid in
  let [@tail_mod_cons] rec map_append f l1 l2 = match l1 with
    | [] -> l2
    | hd :: tl -> f hd :: map_append f tl l2 in
  map_append convert_effect es (if has_sched_effects then TMap.sched_effects tm else [])

type backend =
  | BackendNone
  | BackendWizard

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

let use_init
  = Settings.(option "use_init"
              |> synopsis "Generate WizardEngine-style outputs"
              |> convert (fun x -> Some (parse_bool x))
              |> sync)
let use_init =
  let ret = ref None in
  fun () -> match !ret with
    | Some v -> v
    | None -> match Settings.get wasm_backend with
        | None
        | Some BackendWizard ->
            ret := Some false; false
        | Some BackendNone ->
            let ret' = Option.value ~default:true (Settings.get use_init) in
            ret := Some ret';
            ret'

let wasm_yield_is_switch
  = Settings.(flag ~default:false "wasm_yield_is_switch"
              |> synopsis "Generate switch instructions instead of suspend instructions for yielding"
              |> convert parse_bool
              |> sync)

let generate_type_map (m : 'a modu) : tmap = TMap.empty m.mod_process_level (Settings.get wasm_yield_is_switch)

type 'a printer_aux = {
  paux_putc : 'a;
  paux_puta : 'a;
}
let prepare_imports (tm : tmap) (typ : Types.datatype) : Wasm.import array * int32 option printer_aux option =
  let (let*) v f = match v with Either.Left ret -> ret | Either.Right v -> f v in
  let import_puta = 0 in
  let import_putc = 1 in
  let* mod_importname = match Settings.get wasm_backend with
    | None | Some BackendNone -> Either.Left ([||], None)
    | Some BackendWizard ->
        Either.Right (fun v ->
          if v = import_putc then ("wizeng", "putc") else
          if v = import_puta then ("wizeng", "puta") else
          raise (internal_error "Invalid import")) in
  let require_puta (paux : int32 option printer_aux) : int32 option printer_aux =
    if Option.is_some paux.paux_puta then paux else
    let tid = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [||], DefFuncT (
      FuncT ([|RefT (Null, VarHT TMap.string_tid); NumT I32T; NumT I32T|], [||])))) in
    { paux with paux_puta = Some tid } in
  let require_putc (paux : int32 option printer_aux) : int32 option printer_aux =
    if Option.is_some paux.paux_putc then paux else
    let tid = TMap.recid_of_sub_type tm Wasm.Type.(SubT (Final, [||], DefFuncT (FuncT ([|NumT I32T|], [||])))) in
    { paux with paux_putc = Some tid } in
  let rec inner typ (paux : int32 option printer_aux) (ss : Utility.stringset) : int32 option printer_aux = match typ with
    | Types.Not_typed -> failwith "TODO Irtowasm.prepare_imports.inner Not_typed"
    | Types.Var _ -> failwith "TODO Irtowasm.prepare_imports.inner Var"
    | Types.Recursive (_, _, t) -> inner t paux ss (* Assume this is fine *)
    | Types.Alias (_, _, t) -> inner t paux ss
    | Types.Application (at, ts) ->
        if Types.Abstype.equal at Types.list then inner (snd (List.hd ts)) (require_putc paux) ss
        else failwith "TODO Irtowasm.prepare_imports.inner Application"
    | Types.RecursiveApplication ra -> let open Types in
        if Utility.StringSet.mem ra.r_unique_name ss then paux
        else inner (ra.r_unwind ra.r_args ra.r_dual) (require_putc paux) (Utility.StringSet.add ra.r_unique_name ss)
    | Types.Meta t -> inner (Unionfind.find t) paux ss
    | Types.Primitive CommonTypes.Primitive.Bool -> require_putc paux
    | Types.Primitive CommonTypes.Primitive.Int -> require_putc paux
    | Types.Primitive CommonTypes.Primitive.Float -> failwith "TODO Irtowasm.prepare_imports.inner Primitive Float"
    | Types.Primitive CommonTypes.Primitive.String -> require_puta paux
    | Types.Primitive _ -> failwith "TODO Irtowasm.prepare_imports.inner Primitive"
    | Types.Function (_, _, _) -> require_putc paux
    | Types.Lolli (_, _, _) -> require_putc paux
    | Types.Record (Types.Row (fsm, _, _)) -> Utility.StringMap.fold (fun _ t paux -> inner t paux ss) fsm (require_putc paux)
    | Types.Record t -> inner t (require_putc paux) ss
    | Types.Variant (Types.Row (fsm, _, _)) -> Utility.StringMap.fold (fun _ t paux -> inner t paux ss) fsm (require_putc paux)
    | Types.Variant t -> inner t (require_putc paux) ss
    | Types.Table _ -> failwith "TODO Irtowasm.prepare_imports.inner Table"
    | Types.Lens _ -> failwith "TODO Irtowasm.prepare_imports.inner Lens"
    | Types.ForAll (_, t) -> inner t paux ss
    | Types.Effect _ -> failwith "TODO Irtowasm.prepare_imports.inner Effect"
    | Types.Operation _ -> failwith "TODO Irtowasm.prepare_imports.inner Operation"
    | Types.Row _ -> failwith "TODO Irtowasm.prepare_imports.inner Row"
    | Types.Closed -> failwith "TODO Irtowasm.prepare_imports.inner Closed"
    | Types.Absent -> failwith "TODO Irtowasm.prepare_imports.inner Absent"
    | Types.Present t -> inner t paux ss
    | Types.Input _ -> failwith "TODO Irtowasm.prepare_imports.inner Input"
    | Types.Output _ -> failwith "TODO Irtowasm.prepare_imports.inner Output"
    | Types.Select _ -> failwith "TODO Irtowasm.prepare_imports.inner Select"
    | Types.Choice _ -> failwith "TODO Irtowasm.prepare_imports.inner Choice"
    | Types.Dual _ -> failwith "TODO Irtowasm.prepare_imports.inner Dual"
    | Types.End -> failwith "TODO Irtowasm.prepare_imports.inner End" in
  let hasp = inner typ (require_putc { paux_putc = None; paux_puta = None; }) Utility.StringSet.empty in
  let nimports, imports, paux = 0l, [], { paux_putc = None; paux_puta = None; } in
  let nimports, imports, paux = match hasp.paux_putc with
    | Some tid -> Int32.succ nimports, (mod_importname import_putc, tid) :: imports, { paux with paux_putc = Some nimports }
    | None -> nimports, imports, paux in
  let nimports, imports, paux = match hasp.paux_puta with
    | Some tid -> Int32.succ nimports, (mod_importname import_puta, tid) :: imports, { paux with paux_puta = Some nimports }
    | None -> nimports, imports, paux in
  let nimports = Int32.to_int nimports in
  let open Wasm in
  let imports_ret = Array.make nimports { module_name = ""; item_name = ""; desc = FuncImport 0l } in
  let convert_import i ((mn, fn), tid) =
    imports_ret.(nimports - 1 - i) <- { module_name = mn; item_name = fn; desc = FuncImport tid } in
  List.iteri convert_import imports;
  imports_ret, Some paux
let generate_printer (tm : tmap) (glob : NewMetadata.g) (typ : Types.datatype) (paux : int32 option printer_aux option) (gid : int32)
    (tags : tagid Env.String.t) : Wasm.instr nlist =
  let open Wasm in let open Type in let open Value in let open Instruction in
  match paux with None -> nlist_of_list [GlobalSet gid] | Some paux ->
  let convert_printer (p : (instr nlist, int32) Either.t) : instr_conv = match p with
    | Either.Left il -> fun acc -> il @+ acc
    | Either.Right fid -> fun acc -> Call (Int32.add (NewMetadata.fun_offset glob) fid) ^+ acc in
  let add_string s code =
    String.fold_left (fun acc c -> Call (Option.get paux.paux_putc) ^+ Const (I32 (I32.of_int_s (Char.code c))) ^+ acc) code s in
  let rec inner glob typ (sm : int32 Utility.stringmap) : int32 Utility.stringmap * (instr nlist, int32) Either.t = match typ with
    | Types.Not_typed -> failwith "TODO Irtowasm.generate_printer.inner Not_typed"
    | Types.Var _ -> failwith "TODO Irtowasm.generate_printer.inner Var"
    | Types.Recursive (_, _, t) -> inner glob t sm (* Assume this is fine *)
    | Types.Alias (_, _, t) -> inner glob t sm
    | Types.Application (at, ts) ->
        if Types.Abstype.equal at Types.list then
          let sm, innerp = inner glob (snd (List.hd ts)) sm in
          let innerp = convert_printer innerp in
          let Type t = match ts with
            | [CommonTypes.PrimaryKind.Type, t] -> convert_datatype t
            | _ -> raise (internal_error "Invalid type application in generate_printer") in
          let unbox =
            really_unbox tm t (fun acc -> StructGet (TMap.list_tid, 0l, None) ^+ LocalGet 1l ^+ acc) in
          let fref, fid = NewMetadata.add_function glob in
          fref := Some {
            fn_name = None;
            fn_type = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, VarHT TMap.list_tid)|], [||]))));
            fn_locals = [|RefT (NoNull, VarHT TMap.list_tid)|];
            fn_code = [|
              Const (I32 (I32.of_int_s (Char.code '['))); Call (Option.get paux.paux_putc);
              Block (Type.ValBlockType None, [|
                LocalGet 0l;
                BrOnNull 0l;
                LocalSet 1l;
                Loop (Type.(ValBlockType None), convert_nlist (
                  Br 0l ^+
                  add_string ", " @@
                  LocalSet 1l ^+
                  BrOnNull 1l ^+
                  StructGet (TMap.list_tid, 1l, None) ^+
                  LocalGet 1l ^+
                  innerp (unbox empty_nlist)));
              |]);
              Const (I32 (I32.of_int_s (Char.code ']'))); Call (Option.get paux.paux_putc);
            |];
          };
          sm, Either.Right fid
        else failwith "TODO Irtowasm.generate_printer.inner Application"
    | Types.RecursiveApplication ra -> let open Types in begin match Utility.StringMap.find_opt ra.r_unique_name sm with
        | Some fid -> sm, Either.Right fid
        | None ->
            let Type t = convert_datatype typ in
            let tv = TMap.val_of_type tm t in
            let fref, fid = NewMetadata.add_function glob in
            let sm, innerp = inner glob (ra.r_unwind ra.r_args ra.r_dual) (Utility.StringMap.add ra.r_unique_name fid sm) in
            let innerp = convert_printer innerp in
            let funtid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|tv|], [||])))) in
            fref := Some {
              fn_name = None;
              fn_type = funtid;
              fn_locals = [||];
              fn_code = convert_nlist (
                innerp @@ nlist_of_list [
                LocalGet 0l;
              ]);
            };
            sm, Either.Right fid
      end
    | Types.Meta t -> inner glob (Unionfind.find t) sm
    | Types.Primitive CommonTypes.Primitive.Bool ->
        sm, Either.Left (nlist_of_list Instruction.[
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code 'e')));
          If (Type.(ValBlockType None), [|
            Const (I32 (I32.of_int_s (Char.code 't'))); Call (Option.get paux.paux_putc);
            Const (I32 (I32.of_int_s (Char.code 'r'))); Call (Option.get paux.paux_putc);
            Const (I32 (I32.of_int_s (Char.code 'u'))); Call (Option.get paux.paux_putc);
          |], [|
            Const (I32 (I32.of_int_s (Char.code 'f'))); Call (Option.get paux.paux_putc);
            Const (I32 (I32.of_int_s (Char.code 'a'))); Call (Option.get paux.paux_putc);
            Const (I32 (I32.of_int_s (Char.code 'l'))); Call (Option.get paux.paux_putc);
            Const (I32 (I32.of_int_s (Char.code 's'))); Call (Option.get paux.paux_putc);
          |]);
        ])
    | Types.Primitive CommonTypes.Primitive.Int ->
        let funtid = TMap.recid_of_sub_type tm Type.(SubT (Final, [||], DefFuncT (FuncT ([|NumT I64T|], [||])))) in
        let auxfref, auxfunid = NewMetadata.add_function glob in
        let fref, funid = NewMetadata.add_function glob in
        auxfref := Some { fn_name = None; fn_type = funtid; fn_locals = [||]; fn_code = [|
          LocalGet 0l;
          Testop (I64 IntOp.Eqz);
          BrIf 0l;
          LocalGet 0l;
          Const (I64 (I64.of_bits 10L));
          Relop (I64 IntOp.LtU);
          If (Type.(ValBlockType (Some (NumT I64T))), [|
            LocalGet 0l;
          |], [|
            LocalGet 0l;
            Const (I64 (I64.of_bits 10L));
            Binop (I64 IntOp.DivU);
            Call (Int32.add (NewMetadata.fun_offset glob) auxfunid);
            LocalGet 0l;
            Const (I64 (I64.of_bits 10L));
            Binop (I64 IntOp.RemU);
          |]);
          Cvtop (I32 I32Op.WrapI64);
          Const (I32 (I32.of_int_s (Char.code '0')));
          Binop (I32 IntOp.Add);
          Call (Option.get paux.paux_putc);
        |] };
        fref := Some { fn_name = None; fn_type = funtid; fn_locals = [||]; fn_code = [|
          LocalGet 0l;
          Const (I64 I64.zero);
          Relop (I64 IntOp.GeS);
          If (Type.(ValBlockType (Some (NumT I64T))), [|
            LocalGet 0l;
            Testop (I64 IntOp.Eqz);
            If (Type.(ValBlockType None), [|
              Const (I32 (I32.of_int_s (Char.code '0'))); ReturnCall (Option.get paux.paux_putc);
            |], [||]);
            LocalGet 0l;
          |], [|
            Const (I32 (I32.of_int_s (Char.code '-')));
            Call (Option.get paux.paux_putc);
            Const (I64 I64.zero);
            LocalGet 0l;
            Binop (I64 IntOp.Sub);
          |]);
          Call (Int32.add (NewMetadata.fun_offset glob) auxfunid);
        |]};
        sm, Either.Right funid
    | Types.Primitive CommonTypes.Primitive.Float -> failwith "TODO Irtowasm.generate_printer.inner Primitive Float"
    | Types.Primitive CommonTypes.Primitive.String ->
        let funtid = TMap.recid_of_sub_type tm Type.(SubT (Final, [||],
              DefFuncT (FuncT ([|RefT (NoNull, VarHT TMap.string_tid)|], [||])))) in
        let fref, funid = NewMetadata.add_function glob in
        fref := Some { fn_name = None; fn_type = funtid; fn_locals = Type.[|NumT I32T|]; fn_code = [|
          Const (I32 (I32.of_int_s (Char.code '"'))); Call (Option.get paux.paux_putc);
          LocalGet 0l;
          Const (I32 I32.zero);
          LocalGet 0l; ArrayLen;
          Call (Option.get paux.paux_puta);
          Const (I32 (I32.of_int_s (Char.code '"'))); Call (Option.get paux.paux_putc);
        |]};
        sm, Either.Right funid
    | Types.Primitive _ -> failwith "TODO Irtowasm.generate_printer.inner Primitive"
    | Types.Function (_, _, _)
    | Types.Lolli (_, _, _) ->
        sm, Either.Left (nlist_of_list [
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code 'n')));
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code 'u')));
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code 'f')));
          Drop;
        ])
    | Types.Record (Types.Row (fsm, _, _)) ->
        if Utility.StringMap.is_empty fsm then sm, Either.Left (nlist_of_list [
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code ')')));
          Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code '(')));
          Drop;
        ]) else
        let n, tl = convert_field_spec_map fsm in
        let tv = TMap.val_of_type tm (TTuple n) in
        let tid = match tv with
          | RefT (Null, NoneHT) -> 0l (* TTuple TLnil *)
          | RefT (NoNull, VarHT tid) -> tid (* Anything else *)
          | _ -> raise (internal_error "Invalid type conversion") in
        let funtid = TMap.recid_of_sub_type tm Type.(SubT (Final, [||], DefFuncT (FuncT ([|tv|], [||])))) in
        let fref, funid = NewMetadata.add_function glob in
        let sm, fields = Utility.StringMap.fold (fun n t (sm, acc) -> let sm, p = inner glob t sm in sm, (n, p) :: acc) fsm (sm, []) in
        let is_tuple =
          let rec inner acc fields = match fields with
            | [] -> Some (List.rev acc)
            | (x, y) :: fields -> match int_of_string_opt x with Some x -> inner ((x, y) :: acc) fields | None -> None in
          match inner [] fields with None -> None | Some fields ->
          let sorted = List.sort (fun (x, _) (y, _) -> Int.compare x y) fields in
          let numbers, values = List.split sorted in
          if Utility.ordered_consecutive numbers && match numbers with [] | 1 :: _ :: _ -> true | _ :: _ -> false then
            Some values
          else None in
        fref := Some {
          fn_name = None;
          fn_type = funtid;
          fn_locals = [||];
          fn_code = (match is_tuple with
            | Some fs -> convert_nlist @@
                Call (Option.get paux.paux_putc) ^+ Const (I32 (I32.of_int_s (Char.code ')'))) ^+
                snd (List.fold_left (fun ((i, TypeList tl), acc) p -> match tl with
                    | TLnil -> raise (internal_error "Invalid type conversion")
                    | TLcons (thd, ttl) -> (Int32.succ i, TypeList ttl),
                    let unbox = really_unbox tm thd (fun acc -> StructGet (tid, i, None) ^+ LocalGet 0l ^+ acc) in
                    let acc = if Int32.equal i 0l then acc else add_string ", " acc in
                    convert_printer p (unbox acc)
                  ) ((0l, tl), nlist_of_list [
                Call (Option.get paux.paux_putc); Const (I32 (I32.of_int_s (Char.code '(')));
              ]) fs);
            | None -> failwith "TODO Irtowasm.generate_printer.inner Record [non-tuple]");
        };
        sm, Either.Right funid
    | Types.Record _ -> failwith "TODO Irtowasm.generate_printer.inner Record"
    | Types.Variant (Types.Row (fsm, _, _)) ->
        let fref, fid = NewMetadata.add_function glob in
        let funtid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|RefT (NoNull, VarHT TMap.variant_tid)|], [||])))) in
        let sm, cs = Utility.StringMap.fold (fun n t (sm, cs) -> match Env.String.find_opt n tags with
          | None -> sm, cs
          | Some id -> match t with
              | Types.Record (Types.Row (fsm, _, _))
              | Types.Present Types.Record (Types.Row (fsm, _, _)) when Utility.StringMap.is_empty fsm ->
                  sm, (n, id, Type (TTuple 0), Either.Left (nlist_of_list [Drop])) :: cs
              | Types.Record _
              | Types.Present Types.Record _ -> let sm, p = inner glob t sm in sm, (n, id, Wasmuir.convert_datatype t, p) :: cs
              | _ ->
                  let sm, p = inner glob t sm in
                  let p = Either.Left (add_string ")" (convert_printer p (add_string "(" empty_nlist))) in
                  sm, (n, id, Wasmuir.convert_datatype t, p) :: cs) fsm (sm, []) in
        let branches, ncases, min_id =
          List.fold_left
            (fun (branches, ncases, min_id) (_, id, _, _) ->
              let id = (id : tagid :> int) in
              let branches =
                if Array.length branches <= id then Array.init (id + 1) (fun j -> if j < Array.length branches then branches.(j) else None)
                else branches in
              branches.(id) <- Some ncases;
              let ncases = Int32.succ ncases in
              let min_id = Int.min min_id id in
              branches, ncases, min_id)
            ([||], 0l, Int.max_int) cs in
        let min_id = if min_id <= 3 then 0 else min_id in
        let block_content = nlist_of_list [
          StructGet (TMap.variant_tid, 0l, None);
          LocalGet 0l;
        ] in
        let block_content =
          if min_id = 0 then BrTable (Array.map (Option.value ~default:ncases) branches, ncases) ^+ block_content
          else
            BrTable (
              Array.init
                (Array.length branches - min_id)
                (fun i -> match branches.(i + min_id) with None -> ncases | Some v -> v),
              ncases) ^+
            Binop Wasm.Value.(I32 IntOp.Sub) ^+
            Const Wasm.Value.(I32 (I32.of_int_u min_id)) ^+
            block_content in
        let block_content, _ = List.fold_left
          (fun (block_content, depth) (n, _, Type btyp, p) ->
            let depth = Int32.succ depth in
            let block_content = add_string n (nlist_of_list [Block (Wasm.Type.ValBlockType None, convert_nlist block_content)]) in
            let unbox = really_unbox tm btyp (fun acc -> StructGet (TMap.variant_tid, 1l, None) ^+ LocalGet 0l ^+ acc) in
            let new_block =
              convert_printer p @@
              unbox block_content in
            Return ^+ new_block, depth)
          (block_content, 0l) cs in
        let block_content = [|Block (Wasm.Type.ValBlockType None, convert_nlist block_content); Unreachable|] in
        fref := Some {
          fn_name = None;
          fn_type = funtid;
          fn_locals = [||];
          fn_code = block_content;
        };
        sm, Either.Right fid
    | Types.Variant _ -> failwith "TODO Irtowasm.generate_printer.inner Variant"
    | Types.Table _ -> failwith "TODO Irtowasm.generate_printer.inner Table"
    | Types.Lens _ -> failwith "TODO Irtowasm.generate_printer.inner Lens"
    | Types.ForAll (_, t) -> inner glob t sm
    | Types.Effect _ -> failwith "TODO Irtowasm.generate_printer.inner Effect"
    | Types.Operation _ -> failwith "TODO Irtowasm.generate_printer.inner Operation"
    | Types.Row _ -> failwith "TODO Irtowasm.generate_printer.inner Row"
    | Types.Closed -> failwith "TODO Irtowasm.generate_printer.inner Closed"
    | Types.Absent -> failwith "TODO Irtowasm.generate_printer.inner Absent"
    | Types.Present t -> inner glob t sm
    | Types.Input _ -> failwith "TODO Irtowasm.generate_printer.inner Input"
    | Types.Output _ -> failwith "TODO Irtowasm.generate_printer.inner Output"
    | Types.Select _ -> failwith "TODO Irtowasm.generate_printer.inner Select"
    | Types.Choice _ -> failwith "TODO Irtowasm.generate_printer.inner Choice"
    | Types.Dual _ -> failwith "TODO Irtowasm.generate_printer.inner Dual"
    | Types.End -> failwith "TODO Irtowasm.generate_printer.inner End" in
  let _, innerp = inner glob typ Utility.StringMap.empty in
  let code = nlist_of_list [GlobalGet gid; GlobalSet gid] in
  let code = convert_printer innerp code in
  let code = add_string (Types.string_of_datatype typ) (add_string " : " code) in
  Call (Option.get paux.paux_putc) ^+ Const (I32 (I32.of_int_s (Char.code '\n'))) ^+ code

let compile (m : 'a modu) (main_typ : Types.datatype) : Wasm.module_ =
  let wasm_yield_is_switch = Settings.get wasm_yield_is_switch in
  let tm = generate_type_map m in
  let main_res =
    let cg = convert_global tm (0, Type m.mod_main, Some "_init_result") in
    cg in
  let imports, paux = prepare_imports tm main_typ in
  let glob =
    let mainid = m.mod_nfuns in
    let nfuns = Int32.succ mainid in
    let neffects = m.mod_neffs in
    let nglob = Int32.succ m.mod_nglobals in
    NewMetadata.empty_global ~nimports:(Int32.of_int (Array.length imports)) ~nfuns ~neffects ~nglob in
  let procinfo, main_tid, generate_exit_code, mainid =
    if m.mod_process_level = PL_NoProcess then None, TMap.main_func_type, Fun.id, m.mod_nfuns
    else begin
      let needs_angels = match m.mod_process_level with
        | PL_NoProcess | PL_MessageBox | PL_SingleThread | PL_MultiThread | PL_MultiWait -> false
        | PL_MultiAngel | PL_MultiAngelWait -> true in
      let open Wasm in let open Value in let open Type in let open Instruction in
      let self_eid = Int32.add (NewMetadata.effect_offset glob) TMap.self_offset in
      let self_ftid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (
        FuncT ([|RefT (NoNull, VarHT TMap.pid_tid)|], [|RefT (Null, EqHT)|])))) in
      let self_ctid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefContT (ContT (VarHT self_ftid)))) in
      let self_bt = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (
        FuncT ([|RefT (NoNull, VarHT self_ctid)|], [|RefT (Null, EqHT)|])))) in
      let spawn_eid = Int32.add (NewMetadata.effect_offset glob) TMap.spawn_offset in
      let spawn_cbid = TMap.recid_of_type tm (TClosed (TLnil, TVar)) in
      let spawn_ctid =
        if wasm_yield_is_switch then
          let ftid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (
            FuncT ([|RefT (NoNull, VarHT TMap.pid_tid); RefT (Null, VarHT TMap.process_active_tid)|], [|RefT (Null, EqHT)|])))) in
          TMap.recid_of_sub_type tm (SubT (Final, [||], DefContT (ContT (VarHT ftid))))
        else self_ctid in
      let spawn_bt = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (
        FuncT ([||], [|RefT (NoNull, VarHT spawn_cbid); RefT (NoNull, VarHT spawn_ctid)|])))) in
      let yield_eid = Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset in
      let yield_lb = if wasm_yield_is_switch then Instruction.OnSwitch else Instruction.OnLabel 4l in
      let wait_eid = Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset in
      let wait_bt = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (
        FuncT ([||], [|RefT (NoNull, VarHT TMap.pid_active_tid); RefT (NoNull, VarHT TMap.process_waiting_tid)|])))) in
      let exit_eid = Int32.add (NewMetadata.effect_offset glob) TMap.exit_offset in
      let exit_lb = if wasm_yield_is_switch then Instruction.OnLabel 6l else Instruction.OnLabel 7l in
      let exit_ftid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, EqHT)|], [|RefT (Null, EqHT)|])))) in
      let exit_ctid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefContT (ContT (VarHT exit_ftid)))) in
      let procinfo_vid = NewMetadata.add_global tm glob None TInt [|Const (I64 (I64.of_bits Int64.zero))|] in
      let angels_vid = if needs_angels then NewMetadata.add_raw_global glob (
        (GlobalT (Var, RefT (Null, VarHT TMap.pid_list_tid))),
        [|RefNull (VarHT TMap.pid_list_tid)|],
        None) else 0l in
      let select_next_process, _all_set, _all_get, all_tee, cache_set, cache_get, _cache_tee, nlocals =
        let do_select = fun vbt (_all_set, all_get) (cache_set, cache_get, cache_tee) on_selected on_selected_if ->
          (* Load the next active process
            An active process is a process in the process list that has a non-Null continuation.
            An inactive process is a process in the process list with a Null continuation.
            A zombie process is an inactive process with no mailbox (it has already returned).
            We first prioritize the cache (local 2l) to find an active process.
            If we reach the end of the cache, we repeat once with the entire process list (local 1l).
            If the main process is a zombie (the first process in the entire list), we are done and need to exit immediately.
            Otherwise, if we see another inactive process (Null continuation), we need to remove it from the list and try the step again.
          *)
          Loop (ValBlockType vbt, convert_nlist (
            Loop (ValBlockType vbt, [|Br 0l|]) ^+
            (* Deadlock: nothing to do anymore *)
            BrIf 0l ^+ (* There are some other processes (though they may be zombies) *)
            Testop (I32 IntOp.Eqz) ^+
            RefIsNull ^+
            StructGet (TMap.process_list_tid, 2l, None) ^+
            all_get ^+
            (* Note: this is only a kind of deadlock detection *)
            (* Main is waiting, check the next processes *)
            on_selected_if 0l @@ (* Main is active, continue *)
            Testop (I32 IntOp.Eqz) ^+
            RefIsNull ^+
            StructGet (TMap.process_list_tid, 1l, None) ^+
            (* Check the main process *)
            cache_tee (nlist_of_list [
            all_get;
            (* No more process in the queue, start from the beginning *)
            Block (ValBlockType None, [|
              Loop (ValBlockType None, [|
                (* Search the cache *)
                Block (ValBlockType None, convert_nlist (
                  on_selected 3l @@ nlist_of_list [
                  cache_set;
                  RefAsNonNull;
                  StructGet (TMap.process_list_tid, 2l, None);
                  cache_get;
                  (* Next process is active, select it *)
                  BrOnNull 0l;
                  StructGet (TMap.process_list_tid, 1l, None);
                  (* Something is in the tail of the cache *)
                  BrOnNull 2l;
                  StructGet (TMap.process_list_tid, 2l, None);
                  cache_get;
                ]));
                (* Next process is inactive, delete it *)
                cache_get;
                cache_get;
                StructGet (TMap.process_list_tid, 2l, None);
                StructGet (TMap.process_list_tid, 2l, None);
                StructSet (TMap.process_list_tid, 2l);
                Br 0l;
              |]);
            |]);
          ]))) in
        if wasm_yield_is_switch then
          let all_vid = NewMetadata.add_raw_global glob
              (GlobalT (Var, RefT (NoNull, VarHT TMap.process_list_tid)), [|
                StructNew (TMap.pid_tid, Implicit);
                RefNull (VarHT TMap.process_active_tid);
                RefNull (VarHT TMap.process_list_tid);
                StructNew (TMap.process_list_tid, Explicit);
              |], None) in
          let cache_vid = NewMetadata.add_raw_global glob
              (GlobalT (Var, RefT (NoNull, VarHT TMap.process_list_tid)), [|
                StructNew (TMap.pid_tid, Implicit);
                RefNull (VarHT TMap.process_active_tid);
                RefNull (VarHT TMap.process_list_tid);
                StructNew (TMap.process_list_tid, Explicit);
              |], None) in
          let all_set, all_get, all_tee =
              GlobalSet all_vid, GlobalGet all_vid, fun acc -> GlobalGet all_vid ^+ GlobalSet all_vid ^+ acc in
          let cache_set, cache_get, cache_tee =
              GlobalSet cache_vid, GlobalGet cache_vid, fun acc -> GlobalGet cache_vid ^+ GlobalSet cache_vid ^+ acc in
          let do_select = do_select None (all_set, all_get) (cache_set, cache_get, cache_tee) in
          let switch_proc_ref, switch_proc_fid = NewMetadata.add_function glob in
          switch_proc_ref := Some {
              fn_name = None;
              fn_type = TMap.main_func_type;
              fn_locals = [||];
              fn_code = [|
                do_select
                  (fun _ acc -> Return ^+ Const (I32 I32.zero) ^+ acc)
                  (fun _ acc -> If (ValBlockType None, [|Const (I32 I32.zero); Return|], [||]) ^+ acc);
              |]
            };
          (fun b acc -> (if b then (^+) (Br 0l) else Fun.id) @@ Call (Int32.add (NewMetadata.fun_offset glob) switch_proc_fid) ^+ acc),
            all_set, all_get, all_tee, cache_set, cache_get, cache_tee, 8
        else
          let all_vid, cache_vid = 7l, 8l in
          let all_set, all_get, all_tee = LocalSet all_vid, LocalGet all_vid, fun acc -> LocalTee all_vid ^+ acc in
          let cache_set, cache_get, cache_tee = LocalSet cache_vid, LocalGet cache_vid, fun acc -> LocalTee cache_vid ^+ acc in
          let do_select = do_select (Some (RefT (NoNull, VarHT exit_ctid))) (all_set, all_get) (cache_set, cache_get, cache_tee) in
          (fun _ acc -> do_select (fun d acc -> Br (Int32.succ d) ^+ acc) (fun d acc -> BrIf (Int32.succ d) ^+ acc) ^+ acc),
            all_set, all_get, all_tee, cache_set, cache_get, cache_tee, 9 in
      let do_yield_fun, oadd_loc, do_update =
        if wasm_yield_is_switch then
          let tmp_vid = NewMetadata.add_raw_global glob
              (GlobalT (Var, RefT (NoNull, VarHT TMap.process_list_tid)), [|
                StructNew (TMap.pid_tid, Implicit);
                RefNull (VarHT TMap.process_active_tid);
                RefNull (VarHT TMap.process_list_tid);
                StructNew (TMap.process_list_tid, Explicit);
              |], None) in
          let yield_eid = Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset in
          let yield_ref, yield_fun = NewMetadata.add_function glob in
          let oadd_loc, do_update =
            Some (RefT (NoNull, VarHT TMap.process_active_tid)),
            (fun tmp2_vid acc ->
              let btype = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT ([|RefT (Null, VarHT TMap.process_active_tid)|], [||])))) in
              Block (VarBlockType btype, [|
                BrOnNull 0l;
                LocalSet tmp2_vid;
                GlobalGet tmp_vid;
                LocalGet tmp2_vid;
                StructSet (TMap.process_list_tid, 1l);
              |]) ^+ acc) in
          yield_ref := Some {
            fn_name = None;
            fn_type = TMap.main_func_type;
            fn_locals = [|RefT (NoNull, VarHT TMap.process_active_tid)|];
            fn_code = [|
              cache_get;
              GlobalSet tmp_vid;
              Loop (ValBlockType None, convert_nlist @@
                BrIf 0l ^+ (* Blocked *)
                StructGet (TMap.pid_active_tid, 0l, Some Pack.ZX) ^+
                RefCast (NoNull, VarHT TMap.pid_active_tid) ^+
                StructGet (TMap.pid_tid, 1l, None) ^+
                StructGet (TMap.process_list_tid, 0l, None) ^+
                cache_get ^+
                select_next_process false empty_nlist
              );
              (* OK, reset yield timer, resume process, then save old process if not null *)
              Const (I64 (I64.of_bits (Int64.pred gbl_count_reset)));
              GlobalSet procinfo_vid;
              cache_get;
              StructGet (TMap.process_list_tid, 1l, None);
              Switch (TMap.process_active_tid, yield_eid);
              BrOnNull 0l;
              LocalSet 0l;
              GlobalGet tmp_vid;
              LocalGet 0l;
              StructSet (TMap.process_list_tid, 1l);
            |];
          };
          (fun acc -> Call yield_fun ^+ acc), oadd_loc, do_update
        else (fun acc -> Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.yield_offset) ^+ acc), None, (fun _ acc -> acc) in
      let check_yield_fun =
        let do_check_yield = fun acc ->
          If (ValBlockType None, convert_nlist (
            do_yield_fun empty_nlist (* Also resets the counter to 0 *)
          ), [|
            GlobalGet procinfo_vid;
            Const (I64 I64.one);
            Binop (I64 IntOp.Sub);
            GlobalSet procinfo_vid;
          |]) ^+
          Testop (I64 IntOp.Eqz) ^+
          GlobalGet procinfo_vid ^+
          acc in
        if Settings.get optimize_size then
          let check_yield_fun =
            let fref, fid = NewMetadata.add_function glob in
            fref := Some {
              fn_name = None;
              fn_type = TMap.main_func_type;
              fn_locals = [||];
              fn_code = convert_nlist (do_check_yield empty_nlist)
            };
            fid in
          fun acc -> Call (Int32.add (NewMetadata.fun_offset glob) check_yield_fun) ^+ acc
        else do_check_yield in
      let procinfo = Some (angels_vid, check_yield_fun, do_yield_fun, oadd_loc, do_update) in
      (* Wrap the main function *)
      NewMetadata.add_export_function glob m.mod_nfuns;
      let generate_exit_code =
        if needs_angels then
          let angel_list = procinfo_angels_vid procinfo in fun trf ->
          Suspend exit_eid ^+
          (* Wait until all angels are done *)
          Block (ValBlockType None, [|
            Loop (ValBlockType None, [|
              Block (ValBlockType (Some (RefT (Null, EqHT))), convert_nlist @@
                do_update 0l @@ nlist_of_list [
                Suspend (Int32.add (NewMetadata.effect_offset glob) TMap.wait_offset);
                (* Angel has not returned *)
                BrOnCastFail (0l, (Null, EqHT), (NoNull, VarHT TMap.pid_active_tid)); (* Already done? *)
                StructGet (TMap.pid_tid, 1l, None);
                (* Wait for the PID in the stack to finish, then loop *)
                GlobalSet angel_list;
                StructGet (TMap.pid_list_tid, 1l, None);
                GlobalGet angel_list; (* Mark next angel PID as finished (remove it from the list of angels) *)
                StructGet (TMap.pid_list_tid, 0l, None); (* There is a next angel *)
                BrOnNull 2l; (* Is there no next angel? *)
                GlobalGet angel_list; (* Get next angel PID *)
              ]);
              (* Ignore the angel return value *)
              Drop;
              Br 0l; (* Wait for the next angel *)
            |]);
          |]) ^+ trf
        else fun trf -> Suspend exit_eid ^+ trf in
      let initref, mainid = NewMetadata.add_function glob in
      let locals, loc2off = if wasm_yield_is_switch then [|
          NumT I32T;                                     (*         0: next pid *)
          RefT (Null, EqHT);                             (*         1: boxed value *)
          RefT (NoNull, VarHT self_ctid);                (*         2: $self effect continuation *)
          RefT (NoNull, VarHT spawn_ctid);               (* loc2off+2: $spawn effect continuation *)
          RefT (NoNull, VarHT spawn_cbid);               (* loc2off+3: $spawn callback *)
          RefT (NoNull, VarHT TMap.process_active_tid);  (* loc2off+4: $yield continuation *)
          RefT (NoNull, VarHT TMap.process_waiting_tid); (* loc2off+5: $wait continuation *)
          RefT (NoNull, VarHT TMap.pid_active_tid);      (* loc2off+6: $spawn/$wait PID / main/returned process PID *)
        |], 1l else [|
          NumT I32T;                                     (* 0: next pid *)
          RefT (Null, EqHT);                             (* 1: boxed value *)
          RefT (NoNull, VarHT self_ctid);                (* 2: $self/$spawn effect continuation *)
          RefT (NoNull, VarHT spawn_cbid);               (* 3: $spawn callback *)
          RefT (NoNull, VarHT TMap.process_active_tid);  (* 4: $yield continuation *)
          RefT (NoNull, VarHT TMap.process_waiting_tid); (* 5: $wait continuation *)
          RefT (NoNull, VarHT TMap.pid_active_tid);      (* 6: $spawn/$wait PID / main/returned process PID *)
          RefT (NoNull, VarHT TMap.process_list_tid);    (* 7: all active and zombie processes *)
          RefT (NoNull, VarHT TMap.process_list_tid);    (* 8: active processes cache *)
        |], 0l in
      assert (Array.length locals = nlocals);
      initref := Some {
        fn_name = Some "main";
        fn_type = TMap.main_func_type;
        fn_locals = locals;
        fn_code = convert_nlist @@
          Drop ^+
          Block (ValBlockType (Some (RefT (NoNull, VarHT exit_ctid))), [|
            Loop (ValBlockType (Some (RefT (NoNull, VarHT exit_ctid))), convert_nlist @@
              select_next_process true @@ nlist_of_list [
                let content = fun top_block yield_block_offset ->
                  Block (top_block, [|                                                                  (* Load next process / $yield *)
                    Block (VarBlockType wait_bt, Array.concat [[|                                       (* $wait *)
                      Block (VarBlockType spawn_bt, [|                                                  (* $spawn *)
                        Block (ValBlockType (Some (RefT (Null, EqHT))), [|                              (* (returned) *)
                          Block (ValBlockType (Some (RefT (NoNull, VarHT self_ctid))), convert_nlist @@ (* $self *)
                            Br 1l ^+
                            (* Process returned, boxed return value is the only value in the stack *)
                            Resume (TMap.process_active_tid, [|
                              self_eid, OnLabel 0l; spawn_eid, OnLabel 2l;
                              yield_eid, yield_lb; wait_eid, OnLabel 3l;
                              exit_eid, exit_lb;
                            |]) ^+
                            StructGet (TMap.process_list_tid, 1l, None) ^+
                            cache_get ^+
                            (if wasm_yield_is_switch
                            then (^+) (RefNull (VarHT TMap.process_active_tid))
                            else Fun.id) @@ nlist_of_list [
                            GlobalSet procinfo_vid;
                            Const (I64 (I64.of_bits (Int64.pred gbl_count_reset)));
                            (* OK, reset yield timer then resume process *)
                            BrIf (Int32.add 4l yield_block_offset); (* Blocked *)
                            StructGet (TMap.pid_active_tid, 0l, Some Pack.ZX);
                            RefCast (NoNull, VarHT TMap.pid_active_tid);
                            StructGet (TMap.pid_tid, 1l, None);
                            StructGet (TMap.process_list_tid, 0l, None);
                            cache_get;
                            (* Run the process pointed by the iterator if it is unblocked *)
                          ]);
                          Loop (VarBlockType self_bt, [| (* $self: allow repeats *)
                            LocalSet 2l;
                            cache_get;
                            StructGet (TMap.process_list_tid, 0l, None);
                            LocalGet 2l;
                            Resume (self_ctid, [|
                              self_eid, OnLabel 0l; spawn_eid, OnLabel 2l;
                              yield_eid, yield_lb; wait_eid, OnLabel 3l;
                              exit_eid, exit_lb;
                            |]);
                            (* Process returned, boxed return value is the only value in the stack *)
                            Br 1l;
                          |]);
                        |]);
                        LocalSet 1l;
                        cache_get;
                        StructGet (TMap.process_list_tid, 0l, None);
                        StructGet (TMap.pid_tid, 1l, None);
                        RefCast (NoNull, VarHT TMap.pid_active_tid);
                        LocalSet (Int32.add loc2off 6l);
                        (* Unwait all waiting processes *)
                        Block (ValBlockType (Some (RefT (NoNull, VarHT TMap.process_list_tid))), [|
                          Loop (ValBlockType (Some (RefT (NoNull, VarHT TMap.process_list_tid))), [|
                            cache_get;
                            LocalGet (Int32.add loc2off 6l);
                            StructGet (TMap.pid_active_tid, 3l, None);
                            BrOnNull 1l;
                            StructGet (TMap.waiting_list_tid, 0l, None); (* PID *)
                            LocalGet 1l;
                            LocalGet (Int32.add loc2off 6l);
                            StructGet (TMap.pid_active_tid, 3l, None);
                            RefAsNonNull;
                            StructGet (TMap.waiting_list_tid, 1l, None); (* Waiting continuation *)
                            ContBind (TMap.process_waiting_tid, TMap.process_active_tid); (* Active continuation *)
                            cache_get;
                            StructGet (TMap.process_list_tid, 2l, None); (* Old tail *)
                            StructNew (TMap.process_list_tid, Explicit); (* New tail *)
                            StructSet (TMap.process_list_tid, 2l);
                            (* Pop in the waiting list *)
                            LocalGet (Int32.add loc2off 6l);
                            LocalGet (Int32.add loc2off 6l);
                            StructGet (TMap.pid_active_tid, 3l, None);
                            RefAsNonNull;
                            StructGet (TMap.waiting_list_tid, 2l, None);
                            StructSet (TMap.pid_active_tid, 3l);
                            Br 0l;
                          |]);
                        |]);
                        (* Update the PID *)
                        StructGet (TMap.process_list_tid, 0l, None);
                        LocalGet 1l;
                        StructSet (TMap.pid_tid, 1l);
                        (* Update the process (remove the continuation) *)
                        cache_get;
                        RefNull (VarHT TMap.process_active_tid);
                        StructSet (TMap.process_list_tid, 1l);
                        Br (Int32.add 2l yield_block_offset); (* Go to the next active process *)
                      |]);
                      (* stack is [callback_function continuation] *)
                      LocalSet (Int32.add loc2off 2l);
                      LocalSet (Int32.add loc2off 3l);
                      (* We will insert the new process just after the current process *)
                      cache_get;
                      (* Create a new PID *)
                      LocalGet 0l;
                      LocalGet 0l;
                      Const (I32 I32.one);
                      Binop (I32 IntOp.Add);
                      LocalSet 0l;
                      Const (I32 I32.zero);
                      RefNull (VarHT TMap.list_tid);
                      RefNull (VarHT TMap.list_tid);
                      RefNull (VarHT TMap.waiting_list_tid);
                      StructNew (TMap.pid_active_tid, Explicit);
                      LocalTee (Int32.add loc2off 6l);
                      StructNew (TMap.pid_tid, Explicit);
                      (* Create the continuation *)
                      LocalGet (Int32.add loc2off 3l);
                    |]; (
                    let spawned_cbfid = TMap.recid_of_functyp tm TLnil TVar in
                    if wasm_yield_is_switch then
                      let spawn_cbfid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefFuncT (FuncT (
                          [|RefT (NoNull, VarHT spawn_cbid); RefT (Null, VarHT TMap.process_active_tid)|],
                          [|RefT (Null, EqHT)|]
                        )))) in
                      let spawn_aux_ref, spawn_aux_fid = NewMetadata.add_function glob in
                      NewMetadata.add_export_function glob spawn_aux_fid;
                      spawn_aux_ref := Some {
                        fn_name = None;
                        fn_type = spawn_cbfid;
                        fn_locals = (match oadd_loc with None -> assert false | Some add_loc -> [|add_loc|]);
                        fn_code = convert_nlist @@
                          ReturnCallRef spawned_cbfid ^+
                          StructGet (spawn_cbid, 0l, None) ^+
                          LocalGet 0l ^+
                          StructGet (spawn_cbid, 1l, None) ^+
                          LocalGet 0l ^+
                          do_update 2l @@ nlist_of_list [
                          LocalGet 1l;
                        ];
                      };
                      let spawn_cbcid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefContT (ContT (VarHT spawn_cbfid)))) in [|
                      RefFunc (Int32.add (NewMetadata.fun_offset glob) spawn_aux_fid);
                      ContNew spawn_cbcid;
                      ContBind (spawn_cbcid, TMap.process_active_tid);
                    |] else
                      let spawn_cbcid = TMap.recid_of_sub_type tm (SubT (Final, [||], DefContT (ContT (VarHT spawned_cbfid)))) in [|
                      StructGet (spawn_cbid, 1l, None);
                      LocalGet (Int32.add loc2off 3l);
                      StructGet (spawn_cbid, 0l, None);
                      ContNew spawn_cbcid;
                      ContBind (spawn_cbcid, TMap.process_active_tid);
                    |]); [|
                      (* Finish adding the process to the process list *)
                      cache_get;
                      StructGet (TMap.process_list_tid, 2l, None);
                      StructNew (TMap.process_list_tid, Explicit);
                      (* The stack is now [cached_process_list new_process_list_tail] *)
                      StructSet (TMap.process_list_tid, 2l);
                      (* Finally, update the current process *)
                      cache_get; (* Cached process list *)
                      cache_get; (* Get the child PID *)
                      StructGet (TMap.process_list_tid, 2l, None);
                      StructGet (TMap.process_list_tid, 0l, None);
                      LocalGet (Int32.add loc2off 2l); (* Continuation *)
                      ContBind (spawn_ctid, TMap.process_active_tid); (* Bind the continuation argument *)
                      StructSet (TMap.process_list_tid, 1l); (* Update in the process list *)
                      Br (Int32.add 2l yield_block_offset); (* Resume the current process *)
                    |]]);
                    (* Set continuation of self to Null and add the current process to the list of waiting processes *)
                    LocalSet (Int32.add loc2off 5l);
                    LocalSet (Int32.add loc2off 6l);
                    cache_get;
                    RefNull (VarHT TMap.process_active_tid);
                    StructSet (TMap.process_list_tid, 1l);
                    LocalGet (Int32.add loc2off 6l);
                    cache_get;
                    StructGet (TMap.process_list_tid, 0l, None);
                    LocalGet (Int32.add loc2off 5l);
                    LocalGet (Int32.add loc2off 6l);
                    StructGet (TMap.pid_active_tid, 3l, None);
                    StructNew (TMap.waiting_list_tid, Explicit);
                    StructSet (TMap.pid_active_tid, 3l);
                    Br (Int32.add 0l yield_block_offset); (* Go to the next active process *)
                  |]) in
                let hd = if wasm_yield_is_switch then content (ValBlockType None) 0l
                else
                  Block (ValBlockType None, [| (* Load next process *)
                    content (ValBlockType (Some (RefT (NoNull, VarHT TMap.process_active_tid)))) 1l;
                    (* Update the current process *)
                    LocalSet (Int32.add loc2off 4l);
                    cache_get;
                    LocalGet (Int32.add loc2off 4l);
                    StructSet (TMap.process_list_tid, 1l);
                    Br 0l; (* Go to the next active process *)
                  |]) in
                hd
            ]);
          |]) ^+
          cache_set ^+ (* Process list iterator *)
          all_tee @@ nlist_of_list [ (* All processes *)
          StructNew (TMap.process_list_tid, Explicit);
          RefNull (VarHT TMap.process_list_tid); (* Next processes *)
          ContNew TMap.process_active_tid;
          RefFunc (Int32.add (NewMetadata.fun_offset glob) m.mod_nfuns); (* Wrapper function *)
          StructNew (TMap.pid_tid, Explicit);
          StructNew (TMap.pid_active_tid, Explicit);
          RefNull (VarHT TMap.waiting_list_tid); (* Processes waiting on main *)
          RefNull (VarHT TMap.list_tid);
          RefNull (VarHT TMap.list_tid); (* Mailbox *)
          Const (I32 I32.zero); (* Unblocked by default *)
          Const (I32 I32.zero); (* PID *)
          (* Create main process *)
          LocalSet 0l;
          Const (I32 I32.one);
          (* Next PID *)
        ];
      };
      procinfo, TMap.process_active_ftid, generate_exit_code, mainid
    end in
  let glob, main =
    let new_meta = NewMetadata.extend glob 0l (List.map (fun (Type t) -> TMap.val_of_type tm t) m.mod_locals) in
    let main_code = generate_printer tm glob main_typ paux m.mod_nglobals m.mod_tags in
    let main_code = generate_exit_code main_code in
    let is_init = (Option.is_none procinfo, main_code) in
    let _, main = convert_fun_aux tm new_meta procinfo main_tid m.mod_block (Either.Left is_init) None in
    glob, main in
  let funs = convert_funs tm glob procinfo m.mod_funs (fun glob -> main :: NewMetadata.wasm_funs glob) in
  let globals = NewMetadata.wasm_globals glob in
  let elems = NewMetadata.wasm_elems glob in
  let globals = convert_globals tm m.mod_global_vars (main_res :: globals) in
  let tags = convert_effects tm m.mod_effs (Option.is_some procinfo) in
  let types = TMap.to_wasm tm in
  let tags = Array.of_list tags in
  let elems = Array.of_list elems in
  let funs = Array.of_list funs in
  Wasm.{
    types;
    globals;
    tags;
    funs;
    imports;
    elems;
    init = if use_init () then Some (Int32.add (NewMetadata.fun_offset glob) mainid) else None
  }
