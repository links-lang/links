let internal_error message = Errors.internal_error ~filename:"wasm_CodeGen.ml" ~message

open Wasm
open Wasm.Type
open Wasm.Instruction

let rec generate_u n buf (x : int64) =
  let open Int64 in
  assert (equal (shift_right_logical x n) 0L);
  if unsigned_compare x 0x80L < 0 then Buffer.add_uint8 buf (to_int x)
  else begin Buffer.add_uint8 buf ((to_int x land 0x7F) lor 0x80); generate_u (n - 7) buf (shift_right_logical x 7) end
let generate_s n buf (x : int64) =
  let open Int64 in
  (* assert (equal (shift_right_logical (add x (shift_left 1L (n - 1))) n) 0L); *)
  let rec inner n (x : int64) =
    if compare x 0L >= 0 && compare x 0x40L < 0 then Buffer.add_uint8 buf (to_int x)
    else if compare x 0L < 0 && compare x (neg 0x40L) >= 0 then Buffer.add_uint8 buf (0x7F - (to_int (lognot x) land 0x3F))
    else begin Buffer.add_uint8 buf (to_int (logand x 0x7FL) lor 0x80); inner (n - 7) (shift_right x 7) end
  in inner n (shift_right (shift_left x (64 - n)) (64 - n))
let generate_i = generate_s

let generate_name buf (s : string) =
  generate_u 32 buf (Int64.of_int (String.length s));
  Buffer.add_bytes buf (Bytes.unsafe_of_string s)

let generate_mut buf m = match m with
  | Cons -> Buffer.add_uint8 buf 0x00
  | Var -> Buffer.add_uint8 buf 0x01

let generate_pack_size buf s = match s with
  | Pack.Pack8 -> Buffer.add_uint8 buf 0x78
  | Pack.Pack16 -> Buffer.add_uint8 buf 0x79
  | Pack.Pack32 | Pack.Pack64 -> raise (internal_error "invalid pack size")

let generate_num_type buf t = match t with
  | I32T -> Buffer.add_uint8 buf 0x7F
  | I64T -> Buffer.add_uint8 buf 0x7E
  | F32T -> Buffer.add_uint8 buf 0x7D
  | F64T -> Buffer.add_uint8 buf 0x7C
let generate_vec_type buf t = match t with
  | V128T -> Buffer.add_uint8 buf 0x7B
let ovalue_heap_type t = match t with
  | AnyHT      -> Ok 0x6E
  | NoneHT     -> Ok 0x71
  | EqHT       -> Ok 0x6D
  | I31HT      -> Ok 0x6C
  | StructHT   -> Ok 0x6B
  | ArrayHT    -> Ok 0x6A
  | FuncHT     -> Ok 0x70
  | NoFuncHT   -> Ok 0x73
  | ExnHT      -> Ok 0x68 (* s8 -0x17 = 0b01101000 *)
  | NoExnHT    -> Ok 0x74 (* s8 -0x0C = 0b01110100 *)
  | ExternHT   -> Ok 0x6F
  | NoExternHT -> Ok 0x72
  | ContHT     -> Ok 0x67 (* s8 -0x18 = 0b01100111 *)
  | NoContHT   -> Ok 0x75 (* s8 -0x0B = 0b01110101 *)
  | VarHT v -> Error v
  | BotHT -> raise (internal_error "invalid heap type BotHT")
let rec generate_heap_type buf t = match ovalue_heap_type t with
  | Ok v -> Buffer.add_uint8 buf v
  | Error v -> generate_s 33 buf (Int64.of_int32 v)
and generate_ref_type buf t = match t with
  | (NoNull, ht) -> Buffer.add_uint8 buf 0x64; generate_heap_type buf ht
  | (Null, ht) -> match ovalue_heap_type ht with
    | Ok b -> Buffer.add_uint8 buf b
    | Error _ -> Buffer.add_uint8 buf 0x63; generate_heap_type buf ht
and generate_val_type buf t = match t with
  | NumT nt -> generate_num_type buf nt
  | VecT vt -> generate_vec_type buf vt
  | RefT rt -> generate_ref_type buf rt
  | BotT -> raise (internal_error "invalid value type BotT")
and generate_result_type buf t = generate_u 32 buf (Int64.of_int (List.length t)); List.iter (generate_val_type buf) t
and generate_storage_type buf t = match t with
  | ValStorageT vt -> generate_val_type buf vt
  | PackStorageT ps -> generate_pack_size buf ps
and generate_field_type buf t = match t with
  | FieldT (m, st) -> generate_storage_type buf st; generate_mut buf m
and generate_struct_type buf t = match t with
  | StructT fts ->
      generate_u 32 buf (Int64.of_int (List.length fts));
      List.iter (generate_field_type buf) fts
and generate_array_type buf (ArrayT t) = generate_field_type buf t
and generate_func_type buf t = match t with
  | FuncT (args, rets) -> generate_result_type buf args; generate_result_type buf rets
and generate_cont_type buf (ContT t) = generate_heap_type buf t
and generate_str_type buf t = match t with
  | DefStructT st -> Buffer.add_uint8 buf 0x5F; generate_struct_type buf st
  | DefArrayT at -> Buffer.add_uint8 buf 0x5E; generate_array_type buf at
  | DefFuncT ft -> Buffer.add_uint8 buf 0x60; generate_func_type buf ft
  | DefContT ct -> Buffer.add_uint8 buf 0x5D; generate_cont_type buf ct
and generate_sub_type buf t = match t with
  | SubT (Final, [], st) -> generate_str_type buf st
  | SubT (Final, _hts, _st) -> failwith "TODO generate_sub_type 0x4F"
  | SubT (NoFinal, _hts, _st) -> failwith "TODO generate_sub_type 0x50"
and generate_rec_type buf t = match t with
  | RecT [st] -> generate_sub_type buf st
  | RecT sts ->
      Buffer.add_uint8 buf 0x4E;
      generate_u 32 buf (Int64.of_int (List.length sts));
      List.iter (generate_sub_type buf) sts

let generate_global_type buf t = match t with
  | GlobalT (m, vt) -> generate_val_type buf vt; generate_mut buf m
let generate_block_type buf t = match t with
  | VarBlockType t -> generate_s 33 buf (Int64.of_int32 t)
  | ValBlockType None -> Buffer.add_uint8 buf 0x40
  | ValBlockType (Some t) -> generate_val_type buf t

let generate_types buf ts = match ts with [] -> () | _ ->
  Buffer.add_uint8 buf 0x01;
  let ntypes = List.length ts in
  let auxbuf = Buffer.create 80 in
  generate_u 32 auxbuf (Int64.of_int ntypes);
  List.iter (generate_rec_type auxbuf) ts;
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf

let generate_tag buf tag = Buffer.add_uint8 buf 0x00; generate_u 32 buf (Int64.of_int32 tag)

let generate_import buf nfimports i =
  generate_name buf i.module_name;
  generate_name buf i.item_name;
  match i.desc with
  | FuncImport i -> Buffer.add_uint8 buf 0x00; generate_u 32 buf (Int64.of_int32 i); Int.succ nfimports
  | TagImport i -> Buffer.add_uint8 buf 0x04; generate_tag buf i; nfimports
let generate_imports buf is = match is with [] -> 0 | _ ->
  Buffer.add_uint8 buf 0x02;
  let nimports = List.length is in
  let auxbuf = Buffer.create (2 + nimports) in
  generate_u 32 auxbuf (Int64.of_int nimports);
  let ret = List.fold_left (generate_import auxbuf) 0 is in
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf;
  ret

let generate_resumetable buf hdls =
  let nhdls = List.length hdls in
  generate_u 32 buf (Int64.of_int nhdls);
  List.iter (fun (i, h) -> match h with
    | OnLabel l -> Buffer.add_uint8 buf 0x00; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 l)
    | OnSwitch -> Buffer.add_uint8 buf 0x01; generate_u 32 buf (Int64.of_int32 i)) hdls
let rec generate_instr buf i = match i with
  | Unreachable -> Buffer.add_uint8 buf 0x00
  | Nop -> Buffer.add_uint8 buf 0x01
  | Block (bt, b) ->
      Buffer.add_uint8 buf 0x02; generate_block_type buf bt;
      List.iter (generate_instr buf) b;
      Buffer.add_uint8 buf 0x0B
  | Loop (bt, b) ->
      Buffer.add_uint8 buf 0x03; generate_block_type buf bt;
      List.iter (generate_instr buf) b;
      Buffer.add_uint8 buf 0x0B
  | If (b, t, f) ->
      Buffer.add_uint8 buf 0x04; generate_block_type buf b;
      List.iter (generate_instr buf) t;
      if f <> [] then (Buffer.add_uint8 buf 0x05; List.iter (generate_instr buf) f);
      Buffer.add_uint8 buf 0x0B
  
  | Br i -> Buffer.add_uint8 buf 0x0C; generate_u 32 buf (Int64.of_int32 i)
  | BrIf i -> Buffer.add_uint8 buf 0x0D; generate_u 32 buf (Int64.of_int32 i)
  | BrTable (is, id) ->
      Buffer.add_uint8 buf 0x0E;
      generate_u 32 buf (Int64.of_int (List.length is));
      List.iter (fun i -> generate_u 32 buf (Int64.of_int32 i)) is;
      generate_u 32 buf (Int64.of_int32 id)
  | Return -> Buffer.add_uint8 buf 0x0F
  | Call i -> Buffer.add_uint8 buf 0x10; generate_u 32 buf (Int64.of_int32 i)
  
  | ReturnCall i -> Buffer.add_uint8 buf 0x12; generate_u 32 buf (Int64.of_int32 i)
  
  | CallRef i -> Buffer.add_uint8 buf 0x14; generate_u 32 buf (Int64.of_int32 i)
  | ReturnCallRef i -> Buffer.add_uint8 buf 0x15; generate_u 32 buf (Int64.of_int32 i)
  
  | Drop -> Buffer.add_uint8 buf 0x1A
  
  | LocalGet i -> Buffer.add_uint8 buf 0x20; generate_u 32 buf (Int64.of_int32 i)
  | LocalSet i -> Buffer.add_uint8 buf 0x21; generate_u 32 buf (Int64.of_int32 i)
  | LocalTee i -> Buffer.add_uint8 buf 0x22; generate_u 32 buf (Int64.of_int32 i)
  | GlobalGet i -> Buffer.add_uint8 buf 0x23; generate_u 32 buf (Int64.of_int32 i)
  | GlobalSet i -> Buffer.add_uint8 buf 0x24; generate_u 32 buf (Int64.of_int32 i)
  
  | Const (Value.I32 c) -> Buffer.add_uint8 buf 0x41; generate_i 32 buf (Int64.of_int32 (Value.I32.to_bits c))
  | Const (Value.I64 c) -> Buffer.add_uint8 buf 0x42; generate_i 64 buf (Value.I64.to_bits c)
  | Const (Value.F32 c) -> Buffer.add_uint8 buf 0x43; Buffer.add_int32_le buf (Value.F32.to_bits c)
  | Const (Value.F64 c) -> Buffer.add_uint8 buf 0x44; Buffer.add_int64_le buf (Value.F64.to_bits c)
  | Testop (Value.I32 IntOp.Eqz) -> Buffer.add_uint8 buf 0x45
  | Relop (Value.I32 IntOp.Eq) -> Buffer.add_uint8 buf 0x46
  | Relop (Value.I32 IntOp.Ne) -> Buffer.add_uint8 buf 0x47
	| Relop (Value.I32 IntOp.LtS) -> Buffer.add_uint8 buf 0x48
  | Relop (Value.I32 IntOp.LtU) -> Buffer.add_uint8 buf 0x49
  | Relop (Value.I32 IntOp.GtS) -> Buffer.add_uint8 buf 0x4A
  | Relop (Value.I32 IntOp.GtU) -> Buffer.add_uint8 buf 0x4B
  | Relop (Value.I32 IntOp.LeS) -> Buffer.add_uint8 buf 0x4C
  | Relop (Value.I32 IntOp.LeU) -> Buffer.add_uint8 buf 0x4D
  | Relop (Value.I32 IntOp.GeS) -> Buffer.add_uint8 buf 0x4E
  | Relop (Value.I32 IntOp.GeU) -> Buffer.add_uint8 buf 0x4F
  | Testop (Value.I64 IntOp.Eqz) -> Buffer.add_uint8 buf 0x50
  | Relop (Value.I64 IntOp.Eq) -> Buffer.add_uint8 buf 0x51
  | Relop (Value.I64 IntOp.Ne) -> Buffer.add_uint8 buf 0x52
	| Relop (Value.I64 IntOp.LtS) -> Buffer.add_uint8 buf 0x53
  | Relop (Value.I64 IntOp.LtU) -> Buffer.add_uint8 buf 0x54
  | Relop (Value.I64 IntOp.GtS) -> Buffer.add_uint8 buf 0x55
  | Relop (Value.I64 IntOp.GtU) -> Buffer.add_uint8 buf 0x56
  | Relop (Value.I64 IntOp.LeS) -> Buffer.add_uint8 buf 0x57
  | Relop (Value.I64 IntOp.LeU) -> Buffer.add_uint8 buf 0x58
  | Relop (Value.I64 IntOp.GeS) -> Buffer.add_uint8 buf 0x59
  | Relop (Value.I64 IntOp.GeU) -> Buffer.add_uint8 buf 0x5A
  | Testop (Value.F32 _) -> .
  | Relop (Value.F32 FloatOp.Eq) -> Buffer.add_uint8 buf 0x5B
  | Relop (Value.F32 FloatOp.Ne) -> Buffer.add_uint8 buf 0x5C
  | Testop (Value.F64 _) -> .
  
  | Relop (Value.F64 FloatOp.Eq) -> Buffer.add_uint8 buf 0x61
  | Relop (Value.F64 FloatOp.Ne) -> Buffer.add_uint8 buf 0x62
  | Unop (Value.I32 _) -> .
  
  | Binop (Value.I32 IntOp.Add) -> Buffer.add_uint8 buf 0x6A
  | Binop (Value.I32 IntOp.Sub) -> Buffer.add_uint8 buf 0x6B
  | Binop (Value.I32 IntOp.Mul) -> Buffer.add_uint8 buf 0x6C
  | Binop (Value.I32 IntOp.DivS) -> Buffer.add_uint8 buf 0x6D
  | Binop (Value.I32 IntOp.DivU) -> Buffer.add_uint8 buf 0x6E
  | Binop (Value.I32 IntOp.RemS) -> Buffer.add_uint8 buf 0x6F
  | Binop (Value.I32 IntOp.RemU) -> Buffer.add_uint8 buf 0x70
  | Binop (Value.I32 IntOp.And) -> Buffer.add_uint8 buf 0x71
  | Binop (Value.I32 IntOp.Or) -> Buffer.add_uint8 buf 0x72
  | Binop (Value.I32 IntOp.Xor) -> Buffer.add_uint8 buf 0x73
  | Binop (Value.I32 IntOp.Shl) -> Buffer.add_uint8 buf 0x74
  | Binop (Value.I32 IntOp.ShrS) -> Buffer.add_uint8 buf 0x75
  | Binop (Value.I32 IntOp.ShrU) -> Buffer.add_uint8 buf 0x76
  | Binop (Value.I32 IntOp.Rotl) -> Buffer.add_uint8 buf 0x77
  | Binop (Value.I32 IntOp.Rotr) -> Buffer.add_uint8 buf 0x78
  | Unop (Value.I64 _) -> .
  
  | Binop (Value.I64 IntOp.Add) -> Buffer.add_uint8 buf 0x7C
  | Binop (Value.I64 IntOp.Sub) -> Buffer.add_uint8 buf 0x7D
  | Binop (Value.I64 IntOp.Mul) -> Buffer.add_uint8 buf 0x7E
  | Binop (Value.I64 IntOp.DivS) -> Buffer.add_uint8 buf 0x7F
  | Binop (Value.I64 IntOp.DivU) -> Buffer.add_uint8 buf 0x80
  | Binop (Value.I64 IntOp.RemS) -> Buffer.add_uint8 buf 0x81
  | Binop (Value.I64 IntOp.RemU) -> Buffer.add_uint8 buf 0x82
  | Binop (Value.I64 IntOp.And) -> Buffer.add_uint8 buf 0x83
  | Binop (Value.I64 IntOp.Or) -> Buffer.add_uint8 buf 0x84
  | Binop (Value.I64 IntOp.Xor) -> Buffer.add_uint8 buf 0x85
  | Binop (Value.I64 IntOp.Shl) -> Buffer.add_uint8 buf 0x86
  | Binop (Value.I64 IntOp.ShrS) -> Buffer.add_uint8 buf 0x87
  | Binop (Value.I64 IntOp.ShrU) -> Buffer.add_uint8 buf 0x88
  | Binop (Value.I64 IntOp.Rotl) -> Buffer.add_uint8 buf 0x89
  | Binop (Value.I64 IntOp.Rotr) -> Buffer.add_uint8 buf 0x8A
  
  | Unop (Value.F32 FloatOp.Neg) -> Buffer.add_uint8 buf 0x8C
  
  | Binop (Value.F32 FloatOp.Add) -> Buffer.add_uint8 buf 0x92
  | Binop (Value.F32 FloatOp.Sub) -> Buffer.add_uint8 buf 0x93
  | Binop (Value.F32 FloatOp.Mul) -> Buffer.add_uint8 buf 0x94
  | Binop (Value.F32 FloatOp.Div) -> Buffer.add_uint8 buf 0x95
  
  | Unop (Value.F64 FloatOp.Neg) -> Buffer.add_uint8 buf 0x9A
  
  | Binop (Value.F64 FloatOp.Add) -> Buffer.add_uint8 buf 0xA0
  | Binop (Value.F64 FloatOp.Sub) -> Buffer.add_uint8 buf 0xA1
  | Binop (Value.F64 FloatOp.Mul) -> Buffer.add_uint8 buf 0xA2
  | Binop (Value.F64 FloatOp.Div) -> Buffer.add_uint8 buf 0xA3
  
  | Cvtop (Value.I32 IntOp.WrapI64) -> Buffer.add_uint8 buf 0xA7
  | Cvtop (Value.I64 IntOp.WrapI64) -> raise (internal_error "Cannot wrap-convert from i64 to i64")
  
  | Cvtop (Value.I32 IntOp.ReinterpretFloat) -> Buffer.add_uint8 buf 0xBC
  | Cvtop (Value.I64 IntOp.ReinterpretFloat) -> Buffer.add_uint8 buf 0xBD
  | Cvtop (Value.F32 FloatOp.ReinterpretInt) -> Buffer.add_uint8 buf 0xBE
  | Cvtop (Value.F64 FloatOp.ReinterpretInt) -> Buffer.add_uint8 buf 0xBF
  
  | RefNull ht -> Buffer.add_uint8 buf 0xD0; generate_heap_type buf ht
  | RefIsNull -> Buffer.add_uint8 buf 0xD1
  | RefFunc ti -> Buffer.add_uint8 buf 0xD2; generate_u 32 buf (Int64.of_int32 ti)
  
  | RefAsNonNull -> Buffer.add_uint8 buf 0xD4
  | BrOnNull i -> Buffer.add_uint8 buf 0xD5; generate_u 32 buf (Int64.of_int32 i)
  | BrOnNonNull i -> Buffer.add_uint8 buf 0xD6; generate_u 32 buf (Int64.of_int32 i)
  
  | ContNew ti -> Buffer.add_uint8 buf 0xE0; generate_u 32 buf (Int64.of_int32 ti)
  | ContBind (i, j) -> Buffer.add_uint8 buf 0xE1; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 j)
  | Suspend i -> Buffer.add_uint8 buf 0xE2; generate_u 32 buf (Int64.of_int32 i)
  | Resume (i, hdls) -> Buffer.add_uint8 buf 0xE3; generate_u 32 buf (Int64.of_int32 i); generate_resumetable buf hdls
  
  | StructNew (i, Explicit) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 0L; generate_u 32 buf (Int64.of_int32 i)
  | StructNew (i, Implicit) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 1L; generate_u 32 buf (Int64.of_int32 i)
  | StructGet (i, j, None) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 2L; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 j)
  | StructGet (i, j, Some Pack.SX) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 3L; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 j)
  | StructGet (i, j, Some Pack.ZX) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 4L; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 j)
  | StructSet (i, j) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 5L; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 j)
  | ArrayNew (i, Explicit) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 6L; generate_u 32 buf (Int64.of_int32 i)
  | ArrayNew (i, Implicit) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 7L; generate_u 32 buf (Int64.of_int32 i)
  | ArrayNewFixed (i, n) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 8L; generate_u 32 buf (Int64.of_int32 i); generate_u 32 buf (Int64.of_int32 n)
  
  | ArrayGet (i, None) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 11L; generate_u 32 buf (Int64.of_int32 i)
  | ArrayGet (i, Some Pack.SX) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 12L; generate_u 32 buf (Int64.of_int32 i)
  | ArrayGet (i, Some Pack.ZX) ->
        Buffer.add_uint8 buf 0xFB; generate_u 32 buf 13L; generate_u 32 buf (Int64.of_int32 i)
  | ArraySet i -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 14L; generate_u 32 buf (Int64.of_int32 i)
  | ArrayLen -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 15L
  
  | ArrayCopy (d, s) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 17L; generate_u 32 buf (Int64.of_int32 d); generate_u 32 buf (Int64.of_int32 s)
  
  | RefTest (NoNull, ht) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 20L; generate_heap_type buf ht
  | RefTest (Null, ht) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 21L; generate_heap_type buf ht
  | RefCast (NoNull, ht) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 22L; generate_heap_type buf ht
  | RefCast (Null, ht) -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 23L; generate_heap_type buf ht
  | BrOnCast (i, (n1, t1), (n2, t2)) ->
      let flags = (if n1 = Null then 1 else 0) + (if n2 = Null then 2 else 0) in
      Buffer.add_uint8 buf 0xFB; generate_u 32 buf 24L; Buffer.add_uint8 buf flags;
      generate_u 32 buf (Int64.of_int32 i); generate_heap_type buf t1; generate_heap_type buf t2
  | BrOnCastFail (i, (n1, t1), (n2, t2)) ->
      let flags = (if n1 = Null then 1 else 0) + (if n2 = Null then 2 else 0) in
      Buffer.add_uint8 buf 0xFB; generate_u 32 buf 25L; Buffer.add_uint8 buf flags;
      generate_u 32 buf (Int64.of_int32 i); generate_heap_type buf t1; generate_heap_type buf t2
  
  | RefI31 -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 28L
  | I31Get Pack.SX -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 29L
  | I31Get Pack.ZX -> Buffer.add_uint8 buf 0xFB; generate_u 32 buf 30L
let generate_instrs buf is = List.iter (generate_instr buf) is
let generate_expr buf e = generate_instrs buf e; Buffer.add_uint8 buf 0x0B

let generate_global buf g =
  let (gt, init, oname) = g in
  generate_global_type buf gt;
  generate_expr buf init;
  match oname with Some _ -> 1L | None -> 0L
let generate_globals buf gs = match gs with [] -> 0L | _ ->
  Buffer.add_uint8 buf 0x06;
  let nglobals = List.length gs in
  let auxbuf = Buffer.create 80 in
  generate_u 32 auxbuf (Int64.of_int nglobals);
  let nexports = List.fold_left (fun acc v -> Int64.add acc (generate_global auxbuf v)) 0L gs in
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf;
  nexports

let generate_fun buf auxbuf f =
  let { fn_locals = locs; fn_code = instrs; _ } = f in
  Buffer.reset auxbuf;
  let rec inner locs aux sz ret = match aux, locs with
    | Some (i, t), u :: locs when t = u -> inner locs (Some (Int64.succ i, t)) sz ret
    | Some (i, t), _ -> inner locs None (Int64.succ sz) ((i, t) :: ret)
    | None, t :: locs -> inner locs (Some (1L, t)) sz ret
    | None, [] -> sz, List.rev ret
  in let nlocvecs, locvecs = inner locs None 0L [] in
  generate_u 32 auxbuf nlocvecs; List.iter (fun (i, t) -> generate_u 32 auxbuf i; generate_val_type auxbuf t) locvecs;
  generate_expr auxbuf instrs;
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf
let generate_funs buf fs nfuns = match fs with [] -> 0L | _ ->
  Buffer.add_uint8 buf 0x03;
  let auxbuf = Buffer.create (2 + nfuns) in
  generate_u 32 auxbuf (Int64.of_int nfuns);
  let nfexports = List.fold_left (fun acc f ->
      generate_u 32 auxbuf (Int64.of_int32 f.fn_type);
      match f.fn_name with Some _ -> Int64.succ acc | None -> acc) 0L fs in
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf;
  nfexports
let generate_codes buf fs nfuns = match fs with [] -> () | _ ->
  Buffer.add_uint8 buf 0x0A;
  let auxbuf = Buffer.create 80 in
  generate_u 32 auxbuf (Int64.of_int nfuns);
  let auxbuf2 = Buffer.create 80 in
  List.iter (generate_fun auxbuf auxbuf2) fs;
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf

let generate_init buf oi = match oi with
  | None -> ()
  | Some i ->
      Buffer.add_uint8 buf 0x08;
      let auxbuf = Buffer.create 1 in
      generate_u 32 auxbuf (Int64.of_int32 i);
      generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
      Buffer.add_buffer buf auxbuf

let generate_export buf name kid sid =
  generate_name buf name;
  Buffer.add_uint8 buf kid;
  generate_u 32 buf sid
let generate_global_export buf idx g =
  let (_, _, oname) = g in match oname with
  | Some name -> generate_export buf name 0x03 (Int64.of_int idx)
  | _ -> ()
let generate_function_export buf idx f =
  match f.fn_name with
  | Some name -> generate_export buf name 0x00 (Int64.of_int idx)
  | _ -> ()
let generate_exports buf fs gs nfimports nexports = match fs, gs with [], [] -> () | _ ->
  Buffer.add_uint8 buf 0x07;
  let auxbuf = Buffer.create 80 in
  generate_u 32 auxbuf nexports;
  List.iteri (generate_global_export auxbuf) gs;
  List.iteri (fun i -> generate_function_export auxbuf (nfimports + i)) fs;
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf

let generate_tags buf tags =
  Buffer.add_uint8 buf 0x0D;
  let ntags = List.length tags in
  let auxbuf = Buffer.create (2*ntags + 1) in
  generate_u 32 auxbuf (Int64.of_int ntags);
  List.iter (generate_tag auxbuf) tags;
  generate_u 32 buf (Int64.of_int (Buffer.length auxbuf));
  Buffer.add_buffer buf auxbuf

let output (oc : out_channel) (m : module_) : unit =
  let nfuns = List.length m.funs in
  let buf = Buffer.create 80 in
  Buffer.add_int64_le buf 0x00000001_6D736100L;
  generate_types buf m.types;                               (* Section 1 *)
  let nfimports = generate_imports buf m.imports in         (* Section 2 *)
  let nfexports = generate_funs buf m.funs nfuns in         (* Section 3 *)
  generate_tags buf m.tags;                                 (* Section 13 *)
  let ngexports = generate_globals buf m.globals in         (* Section 6 *)
  let nexports = Int64.add nfexports ngexports in
  generate_exports buf m.funs m.globals nfimports nexports; (* Section 7 *)
  generate_init buf m.init;                                 (* Section 8 *)
  generate_codes buf m.funs nfuns;                          (* Section 10 *)
  output_string oc (Buffer.contents buf)
