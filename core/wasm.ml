module Sexpr : sig
  (* A LongNode is a Node with some arguments forced on the same line as the node type *)
  type t = Atom of string | Node of string * t list | LongNode of string * t list * t list
  
  val pp : int -> Format.formatter -> t -> unit
  val output : out_channel -> int -> t -> unit
  val print : int -> t -> unit
  val to_string : int -> t -> string
end = struct
  type t = Atom of string | Node of string * t list | LongNode of string * t list * t list
  
  let _pp width s =
    let rec inner off s = match s with
      | Atom s -> String.length s, s
      | LongNode (s, [], ss)
      | Node (s, ss) ->
        let cons_len, sep, fin, ss =
          if ss = [] then
            (String.length s + 2), "", ")", []
          else
            let lens, ss = List.split (List.map (inner (if off < 0 then off else off + 2)) ss) in
            let cons_len = List.fold_left (+) (String.length s + List.length ss + 2) lens in
            let sep, fin =
              if off < 0 || cons_len <= width then " ", ")"
              else let indent = String.make off ' ' in "\n  " ^ indent, "\n" ^ indent ^ ")" in
            cons_len, sep, fin, ss in
        cons_len, List.fold_left (fun acc s -> acc ^ sep ^ s) ("(" ^ s) ss ^ fin
      | LongNode (s, ss1, ss2) ->
        let lens1, ss1 = List.split (List.map (inner ~-1) ss1) in
        let cons_len1 = List.fold_left (+) (String.length s + List.length ss1 + List.length ss2 + 2) lens1 in
        let acc1 = List.fold_left (fun acc s -> acc ^ " " ^ s) ("(" ^ s) ss1 in
        let cons_len, sep, fin, ss2 =
          if ss2 = [] then
            cons_len1, "", ")", []
          else
            let lens2, ss2 = List.split (List.map (inner (if off < 0 then off else off + 2)) ss2) in
            let cons_len = List.fold_left (+) cons_len1 lens2 in
            let sep, fin =
              if off < 0 || cons_len <= width then " ", ")"
              else let indent = String.make off ' ' in "\n  " ^ indent, "\n" ^ indent ^ ")" in
            cons_len, sep, fin, ss2 in
        cons_len, List.fold_left (fun acc s -> acc ^ sep ^ s) acc1 ss2 ^ fin
    in snd (inner 0 s)
  
  let pp width fmt s = Format.pp_print_string fmt (_pp width s)
  let output oc width s = output_string oc (_pp width s); output_string oc "\n"
  
  let print = output stdout
  let to_string width s = _pp width s ^ "\n"
end

module Unicode = struct
  type codepoint = int
  type t = codepoint list
  
  exception Utf8
  
  let utf8_encode (u : t) : string =
    let buf = Buffer.create 80 in
    let con c = 0x80 lor (c land 0x3F) in
    let rec encode u = match u with
      | [] -> ()
      | c :: u -> begin
        if c < 0 then raise Utf8
        else if c < 0x80 then Buffer.add_int8 buf c
        else if c < 0x800 then (Buffer.add_int8 buf (0xC0 lor (c lsr 6)); Buffer.add_int8 buf (con c))
        else if c < 0x10000 then (Buffer.add_int8 buf (0xE0 lor (c lsr 12)); Buffer.add_int8 buf (con (c lsr 6)); Buffer.add_int8 buf (con c))
        else if c < 0x110000 then
          (Buffer.add_int8 buf (0xF0 lor (c lsr 18)); Buffer.add_int8 buf (con (c lsr 12));
           Buffer.add_int8 buf (con (c lsr 6)); Buffer.add_int8 buf (con c))
        end; encode u
    in encode u;
    Buffer.contents buf
  
  let utf8_decode (s : string) : t =
    let decode ((buf, acc, rem) : t * codepoint * int) c =
      let c = Char.code c in
      if rem > 0 then begin
        if c land 0xC0 <> 0x80 then raise Utf8
        else let newacc = (acc lsl 6) lor (c land 0x3F) in if rem = 1 then (newacc :: buf, 0, 0) else (buf, newacc, rem - 1)
      end else begin
        if c land 0x80 = 0 then (c :: buf, 0, 0)
        else if c land 0xE0 = 0xC0 then (buf, c land 0x1F, 1)
        else if c land 0xF0 = 0xE0 then (buf, c land 0x0F, 2)
        else if c land 0xF8 = 0xF0 then (buf, c land 0x0E, 3)
        else raise Utf8
      end in
    let end_decode (buf, _acc, rem) = if rem > 0 then raise Utf8 else List.rev buf in
    end_decode (String.fold_left decode ([], 0, 0) s)
end

module Pack = struct
  type pack_size = Pack8 | Pack16 | Pack32 | Pack64
  type extension = SX | ZX
  
  type pack_shape = Pack8x8 | Pack16x4 | Pack32x2
  type vec_extension =
    | ExtLane of pack_shape * extension
    | ExtSplat
    | ExtZero
  
  let packed_size = function Pack8 -> 1 | Pack16 -> 2 | Pack32 -> 4 | Pack64 -> 8
  let packed_shape_size = function Pack8x8 | Pack16x4 | Pack32x2 -> 8
end

module Type = struct
  type type_idx = int32
  type local_idx = int32
  type name = Unicode.t
  
  type null = NoNull | Null
  type mut = Cons | Var
  type init = Set | Unset
  type final = NoFinal | Final
  type limits = {min: int64; max: int64 option}
  
  type var = StatX of type_idx | RecX of int32
  
  type num_type = I32T | I64T | F32T | F64T
  type vec_type = V128T
  type heap_type =
    | AnyHT | NoneHT | EqHT | I31HT | StructHT | ArrayHT
    | FuncHT | NoFuncHT
    | ExnHT | NoExnHT
    | ExternHT | NoExternHT
    | ContHT | NoContHT
    | VarHT of var
    | DefHT of def_type
    | BotHT
  and ref_type = null * heap_type
  and val_type = NumT of num_type | VecT of vec_type | RefT of ref_type | BotT
  and result_type = val_type list
  and instr_type = InstrT of result_type * result_type * local_idx list
  and storage_type = ValStorageT of val_type | PackStorageT of Pack.pack_size
  and field_type = FieldT of mut * storage_type
  and struct_type = StructT of field_type list
  and array_type = ArrayT of field_type
  and func_type = FuncT of result_type * result_type
  and cont_type = ContT of heap_type
  and str_type =
  | DefStructT of struct_type
  | DefArrayT of array_type
  | DefFuncT of func_type
  | DefContT of cont_type
  and sub_type = SubT of final * heap_type list * str_type
  and rec_type = RecT of sub_type list
  and def_type = DefT of rec_type * int32
  
  type global_type = GlobalT of mut * val_type
  type block_type = VarBlockType of int32 | ValBlockType of val_type option
  
  let string_of_null = function
    | NoNull -> ""
    | Null -> "null "
  let string_of_final = function
    | NoFinal -> ""
    | Final -> "final "
  let string_of_mut m s = match m with
    | Cons -> s
    | Var -> "(mut " ^ s ^ ")"
  
  let string_of_var = function
    | StatX x -> Printf.sprintf "%lu" x
    | RecX x -> "rec." ^ Printf.sprintf "%lu" x
  
  let string_of_num_type = function
    | I32T -> "i32"
    | I64T -> "i64"
    | F32T -> "f32"
    | F64T -> "f64"
  let string_of_vec_type = function
    | V128T -> "v128"
  let rec string_of_heap_type = function
    | AnyHT -> "any"
    | NoneHT -> "none"
    | EqHT -> "eq"
    | I31HT -> "i31"
    | StructHT -> "struct"
    | ArrayHT -> "array"
    | FuncHT -> "func"
    | NoFuncHT -> "nofunc"
    | ExnHT -> "exn"
    | NoExnHT -> "noexn"
    | ExternHT -> "extern"
    | NoExternHT -> "noextern"
    | ContHT -> "cont"
    | NoContHT -> "nocont"
    | VarHT v -> string_of_var v
    | DefHT d -> "(" ^ string_of_def_type d ^ ")"
    | BotHT -> "something"
  and string_of_ref_type (n, h) = "(ref " ^ string_of_null n ^ string_of_heap_type h ^ ")"
  and string_of_val_type = function
    | NumT t -> string_of_num_type t
    | VecT t -> string_of_vec_type t
    | RefT t -> string_of_ref_type t
    | BotT -> "bot"
  and string_of_storage_type = function
    | ValStorageT vt -> string_of_val_type vt
    | PackStorageT ps -> "i" ^ string_of_int (Pack.packed_size ps)
  and string_of_field_type = function
    | FieldT (m, st) -> string_of_mut m (string_of_storage_type st)
  and string_of_struct_type = function
    | StructT fs -> "(struct" ^ String.concat "" (List.map (fun ft -> " (field " ^ string_of_field_type ft ^ ")") fs) ^ ")"
  and string_of_func_type = function
    | FuncT (args, rets) ->
      let rets = if rets = [] then ")" else " (result " ^ String.concat " " (List.map (fun t -> string_of_val_type t) rets) ^ "))" in
      let argsrets = if args = [] then rets else " (param " ^ String.concat " " (List.map (fun t -> string_of_val_type t) args) ^ ")" ^ rets in
      "(func" ^ argsrets
  and string_of_str_type = function
    | DefStructT st -> string_of_struct_type st
    | DefArrayT _ -> failwith "TODO string_of_str_type DefArrayT"
    | DefFuncT ft -> string_of_func_type ft
    | DefContT _ -> failwith "TODO string_of_str_type DefContT"
  and string_of_sub_type = function
    | SubT (Final, [], s) -> string_of_str_type s
    | SubT (f, hs, s) -> String.concat " " (("sub" ^ string_of_final f) :: List.map string_of_heap_type hs) ^ " (" ^ string_of_str_type s ^ ")"
  and string_of_rec_type = function
    | RecT [s] -> string_of_sub_type s
    | RecT ss -> "rec (" ^ String.concat " " (List.map string_of_sub_type ss) ^ ")"
  and string_of_def_type = function
    | DefT (RecT [s], 0l) -> string_of_sub_type s
    | DefT (r, i) -> "(" ^ string_of_rec_type r ^ ")." ^ Printf.sprintf "%lu" i
  
  let sexpr_of_null = let open Sexpr in function
    | NoNull -> []
    | Null -> [Atom "null"]
  let sexpr_of_final = let open Sexpr in function
    | NoFinal -> []
    | Final -> [Atom "final"]
  let sexpr_of_mut m s = let open Sexpr in match m with
    | Cons -> s
    | Var -> match s with Atom s -> LongNode ("mut", [Atom s], []) | _ -> Node ("mut", [s])
  
  let sexpr_of_var = let open Sexpr in function
    | StatX x -> Atom (Printf.sprintf "%lu" x)
    | RecX x -> Atom ("rec." ^ Printf.sprintf "%lu" x)
  
  let sexpr_of_num_type = let open Sexpr in function
    | I32T -> Atom "i32"
    | I64T -> Atom "i64"
    | F32T -> Atom "f32"
    | F64T -> Atom "f64"
  let sexpr_of_vec_type = let open Sexpr in function
    | V128T -> Atom "v128"
  let rec sexpr_of_heap_type = let open Sexpr in function
    | AnyHT -> Atom "any"
    | NoneHT -> Atom "none"
    | EqHT -> Atom "eq"
    | I31HT -> Atom "i31"
    | StructHT -> Atom "struct"
    | ArrayHT -> Atom "array"
    | FuncHT -> Atom "func"
    | NoFuncHT -> Atom "nofunc"
    | ExnHT -> Atom "exn"
    | NoExnHT -> Atom "noexn"
    | ExternHT -> Atom "extern"
    | NoExternHT -> Atom "noextern"
    | ContHT -> Atom "cont"
    | NoContHT -> Atom "nocont"
    | VarHT v -> sexpr_of_var v
    | DefHT d -> sexpr_of_def_type d
    | BotHT -> Atom "something"
  and sexpr_of_ref_type (n, h) = let open Sexpr in
    match sexpr_of_heap_type h with
    | Atom s -> LongNode ("ref", sexpr_of_null n @ [Atom s], [])
    | sh -> LongNode ("ref", sexpr_of_null n, [sh])
  and sexpr_of_val_type = let open Sexpr in function
    | NumT t -> sexpr_of_num_type t
    | VecT t -> sexpr_of_vec_type t
    | RefT t -> sexpr_of_ref_type t
    | BotT -> Atom "bot"
  and sexpr_of_storage_type = let open Sexpr in function
    | ValStorageT vt -> sexpr_of_val_type vt
    | PackStorageT ps -> Atom ("i" ^ string_of_int (Pack.packed_size ps))
  and sexpr_of_field_type = function
    | FieldT (m, st) -> sexpr_of_mut m (sexpr_of_storage_type st)
  and sexpr_of_struct_type = let open Sexpr in function
    | StructT fs -> Node ("struct", List.map (fun ft -> Node ("field", [sexpr_of_field_type ft])) fs)
  and sexpr_of_func_type = let open Sexpr in function
    | FuncT (args, rets) ->
      let rets = match rets with
        | [] -> []
        | [ret] -> [LongNode ("result", [sexpr_of_val_type ret], [])]
        | _ -> [Node ("result", List.map (fun t -> sexpr_of_val_type t) rets)] in
      let argsrets = match args with
        | [] -> rets
        | [arg] -> LongNode ("param", [sexpr_of_val_type arg], []) :: rets
        | _ -> Node ("param", List.map (fun t -> sexpr_of_val_type t) args) :: rets in
      Node ("func", argsrets)
  and sexpr_of_cont_type = let open Sexpr in function
    | ContT ht -> begin match sexpr_of_heap_type ht with
      | Atom s -> LongNode ("cont", [Atom s], [])
      | sh -> Node ("cont", [sh])
      end
  and sexpr_of_str_type = function
    | DefStructT st -> sexpr_of_struct_type st
    | DefArrayT _ -> failwith "TODO sexpr_of_str_type DefArrayT"
    | DefFuncT ft -> sexpr_of_func_type ft
    | DefContT ct -> sexpr_of_cont_type ct
  and sexpr_of_sub_type = let open Sexpr in function
    | SubT (Final, [], s) -> sexpr_of_str_type s
    | SubT (f, hs, s) -> Node ("sub", sexpr_of_final f @ List.map sexpr_of_heap_type hs @ [sexpr_of_str_type s])
  and sexpr_of_rec_type = let open Sexpr in function
    | RecT [s] -> sexpr_of_sub_type s
    | RecT ss -> Node ("rec", List.map sexpr_of_sub_type ss)
  and sexpr_of_def_type = function
    | DefT (RecT [s], 0l) -> sexpr_of_sub_type s
    | DefT (_r, _i) -> failwith "TODO sexpr_of_def_type generic"
  
  let sexpr_of_global_type (GlobalT (m, vt) : global_type) : Sexpr.t = let open Sexpr in
    match m with
    | Cons -> sexpr_of_val_type vt
    | Var -> match sexpr_of_val_type vt with Atom s -> LongNode ("mut", [Atom s], []) | sv -> Node ("mut", [sv])
  let sexpr_of_block_type = let open Sexpr in function
    | VarBlockType vt -> [LongNode ("type", [Atom (Int32.to_string vt)], [])]
    | ValBlockType (Some vt) -> [Node ("result", [sexpr_of_val_type vt])]
    | ValBlockType None -> []
end

module Value = struct
  exception Overflow
  exception DivideByZero
  exception InvalidConversion
  
  module Ixx = struct
    module type RepType = sig
      type t
      val zero : t
      val one : t
      val minus_one : t
      val min_int : t
      val max_int : t
      val abs : t -> t
      val neg : t -> t
      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t (* raises DivideByZero *)
      val rem : t -> t -> t (* raises DivideByZero *)
      val logand : t -> t -> t
      val lognot : t -> t
      val logor : t -> t -> t
      val logxor : t -> t -> t
      val shift_left : t -> int -> t
      val shift_right : t -> int -> t
      val shift_right_logical : t -> int -> t
      val of_int : int -> t
      val to_int : t -> int
      val of_int64 : int64 -> t
      val to_int64 : t -> int64
      val to_string : t -> string
      val to_hex_string : t -> string
      val bitwidth : int
    end
    module type S = sig
      type t
      type bits
      val of_bits : bits -> t
      val to_bits : t -> bits
      val zero : t
      val one : t
      val minus_one : t
      val abs : t -> t
      val neg : t -> t
      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div_s : t -> t -> t (* raises DivideByZero, Overflow *)
      val div_u : t -> t -> t (* raises DivideByZero *)
      val rem_s : t -> t -> t (* raises DivideByZero *)
      val rem_u : t -> t -> t (* raises DivideByZero *)
      (* val logand : t -> t -> t *)
      val lognot : t -> t
      (* val logor : t -> t -> t *)
      (* val logxor : t -> t -> t *)
      (* val shift_left : t -> int -> t *)
      (* val shift_right : t -> int -> t *)
      (* val shift_right_logical : t -> int -> t *)
      val of_int_s : int -> t
      val of_int_u : int -> t
      val to_string_s : t -> string
    end
    module Make(Rep : RepType) : S with type bits = Rep.t = struct
      type t = Rep.t
      type bits = Rep.t
      
      let sx : t -> t = let i = 64 - Rep.bitwidth in
        if i = 64 then (fun x -> x)
        else (fun x -> Rep.of_int64 Int64.(shift_right (shift_left (Rep.to_int64 x) i) i))
      let unsx op x = sx (op x)
      let binsx op x y = sx (op x y)
      let low_int = Rep.shift_left Rep.minus_one (Rep.bitwidth - 1)
      let high_int = Rep.logxor Rep.minus_one low_int
      
      let of_bits x = x
      let to_bits x = x
      let zero = Rep.zero
      let one = Rep.one
      let minus_one = Rep.minus_one
      let abs = unsx Rep.abs
      let neg = unsx Rep.neg
      let add = binsx Rep.add
      let sub = binsx Rep.sub
      let mul = binsx Rep.mul
      
      let bbinop_u x op y = op (Rep.add x Rep.min_int) (Rep.add y Rep.min_int)
      let divrem_u n d =
        if d = Rep.zero then raise DivideByZero else
        let t = Rep.shift_right d (Rep.bitwidth - 1) in
        let n' = Rep.logand n (Rep.lognot t) in
        let q = Rep.shift_left (Rep.div (Rep.shift_right_logical n' 1) d) 1 in
        let r = Rep.sub n (Rep.mul q d) in
        if bbinop_u r (<) d then q, r
        else Rep.add q Rep.one, Rep.sub r d
      let div_s x y =
        if y = Rep.zero then raise DivideByZero
        else if x = low_int && y = Rep.minus_one then raise Overflow
        else Rep.div x y
      let div_u x y = fst (divrem_u x y)
      let rem_s x y =
        if y = Rep.zero then raise DivideByZero
        else Rep.rem x y
      let rem_u x y = snd (divrem_u x y)
      let lognot = Rep.lognot
      
      let of_int_s = Rep.of_int
      let of_int_u i = Rep.logand (Rep.of_int i) (Rep.logor (Rep.shift_left (Rep.of_int max_int) 1) one)
      
      let group_digits s =
        let buf = Buffer.create (String.length s * 4 / 3) in
        let off = if s.[0] = '-' then (Buffer.add_char buf s.[0]; 1) else 0 in
        let insert_on = String.length s mod 3 in
        let rec add_digits from =
          if from < String.length s then begin
            if from <> off && from mod 3 = insert_on then Buffer.add_char buf '_';
            Buffer.add_char buf s.[from]; add_digits (from + 1)
          end
        in add_digits off;
        Buffer.contents buf
      let to_string_s i = group_digits (Rep.to_string i)
    end
  end
  module I32 = Ixx.Make(struct
    include Int32
    let of_int64 = Int64.to_int32
    let to_int64 = Int64.of_int32
    let to_hex_string = Printf.sprintf "%lx"
    let bitwidth = 32
  end)
  module I64 = Ixx.Make(struct
    include Int64
    let of_int64 x = x
    let to_int64 x = x
    let to_hex_string = Printf.sprintf "%Lx"
    let bitwidth = 64
  end)
  
  module Fxx = struct
    module type RepType = sig
      type t
      val mantissa : int
      val zero : t
      val min_int : t
      val max_int : t
      val pos_nan : t
      val neg_nan : t
      val bare_nan : t
      val bits_of_float : float -> t
      val float_of_bits : t -> float
      val of_string : string -> t
      val to_string : t -> string
      val to_hex_string : t -> string
      val lognot : t -> t
      val logand : t -> t -> t
      val logor : t -> t -> t
      val logxor : t -> t -> t
    end
    module type S = sig
      type t
      type bits
      val pos_nan : t
      val neg_nan : t
      val is_inf : t -> bool
      val is_nan : t -> bool
      val of_float : float -> t
      val to_float : t -> float
      val of_string : string -> t
      val to_string : t -> string
      val to_hex_string : t -> string
      val of_bits : bits -> t
      val to_bits : t -> bits
      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t
      val fma : t -> t -> t -> t
      val sqrt : t -> t
      val min : t -> t -> t
      val max : t -> t -> t
      val ceil : t -> t
      val floor : t -> t
      val trunc : t -> t
      val nearest : t -> t
      val abs : t -> t
      val neg : t -> t
      val copysign : t -> t -> t
      val eq : t -> t -> bool
      val ne : t -> t -> bool
      val lt : t -> t -> bool
      val le : t -> t -> bool
      val gt : t -> t -> bool
      val ge : t -> t -> bool
      val zero : t
    end
    module Make(Rep : RepType) : S with type bits = Rep.t = struct
      let _ = assert (Rep.mantissa <= 52)
      type t = Rep.t
      type bits = Rep.t
      let pos_inf = Rep.bits_of_float (1. /. 0.)
      let neg_inf = Rep.bits_of_float (-. (1. /. 0.))
      let pos_nan = Rep.pos_nan
      let neg_nan = Rep.neg_nan
      let bare_nan = Rep.bare_nan
      let of_float = Rep.bits_of_float
      let to_float = Rep.float_of_bits
      let of_bits x = x
      let to_bits x = x
      let is_inf x = x = pos_inf || x = neg_inf
      let is_nan x = let xf = Rep.float_of_bits x in xf <> xf
      let canonicalize_nan x = Rep.logor x Rep.pos_nan
      let determine_unary_nan x xf = if xf = xf then pos_nan else x
      let determine_binary_nan x xf y yf = if xf = xf then if yf = yf then pos_nan else y else x
      let unop op x = let xf = to_float x in let r = op xf in
        if r = r then of_float r else determine_unary_nan x xf
      let binop op x y = let xf = to_float x in let yf = to_float y in let r = op xf yf in
        if r = r then of_float r else determine_binary_nan x xf y yf
      let add = binop ( +. )
      let sub = binop ( -. )
      let mul = binop ( *. )
      let div = binop ( /. )
      let fma x y z = let xf = to_float x in let yf = to_float y in let zf = to_float z in
        let r = Float.fma xf yf zf in
        if r = r then of_float r else determine_binary_nan x xf y yf
      let sqrt = unop Stdlib.sqrt
      let min = binop Float.min
      let max = binop Float.max
      let ceil = unop Float.ceil
      let floor = unop Float.floor
      let trunc x =
        let xf = to_float x in if xf = 0. then x else
        let r = if xf < 0. then Stdlib.ceil xf else Stdlib.floor xf in
        if r = r then of_float r else determine_unary_nan x xf
      let nearest = unop (fun f ->
        let u = Stdlib.ceil f in let d = Stdlib.floor f in
        let um = abs_float (f -. u) in let dm = abs_float (f -. d) in
        let take_u = um < dm || um = dm && let h = u /. 2. in Stdlib.floor h = h in
        if take_u then u else d)
      let abs x = Rep.logand x Rep.max_int
      let neg x = Rep.logxor x Rep.min_int
      let copysign x y = Rep.logor (abs x) (Rep.logand y Rep.min_int)
      let eq x y = to_float x = to_float y
      let ne x y = to_float x <> to_float y
      let lt x y = to_float x < to_float y
      let le x y = to_float x <= to_float y
      let gt x y = to_float x > to_float y
      let ge x y = to_float x >= to_float y
      let zero = of_float 0.
      
      let is_digit c = '0' <= c && c <= '9'
      let is_hex_digit c = is_digit c || 'a' <= c && c <= 'f'
      
      let rec add_digits buf s i j k n =
        if i < j then begin
          if k = 0 then Buffer.add_char buf '_';
          Buffer.add_char buf s.[i];
          add_digits buf s (i + 1) j ((k + n - 1) mod n) n
        end
      let group_digits is_digit n s =
        let len = String.length s in
        let rec find_from_opt p i =
          if i >= len then None
          else if p s.[i] then Some i
          else find_from_opt p (i + 1) in
        let isnt_digit c = not (is_digit c) in
        let x = Option.value (find_from_opt ((=) 'x') 0) ~default:0 in
        let mant = Option.value (find_from_opt is_digit x) ~default:len in
        let point = Option.value (find_from_opt isnt_digit mant) ~default:len in
        let frac = Option.value (find_from_opt is_digit point) ~default:len in
        let exp = Option.value (find_from_opt isnt_digit frac) ~default:len in
        let buf = Buffer.create (len*(n+1)/n) in
        Buffer.add_substring buf s 0 mant;
        add_digits buf s mant point ((point - mant) mod n + n) n;
        Buffer.add_substring buf s point (frac - point);
        add_digits buf s frac exp n n;
        Buffer.add_substring buf s exp (len - exp);
        Buffer.contents buf
      let to_string' convert is_digit n x =
        (if x < Rep.zero then "-" else "") ^
        if is_nan x then
          let payload = Rep.logand (abs x) (Rep.lognot bare_nan) in
          "nan:0x" ^ group_digits is_hex_digit 4 (Rep.to_hex_string payload)
        else
          let s = convert (to_float (abs x)) in
          group_digits is_digit n
            (if s.[String.length s - 1] = '.' then s ^ "0" else s)
          
      let of_string (_ : string) : t = failwith "TODO of_string"
      let to_string : t -> string = to_string' (Printf.sprintf "%.17g") is_digit 3
      let to_hex_string (f : t) : string =
        if is_inf f then to_string f else
        to_string' (Printf.sprintf "%h") is_hex_digit 4 f
    end
  end
  module F32 = Fxx.Make(struct
    include Int32
    let mantissa = 23
    let pos_nan = 0x7FF8_0000l
    let neg_nan = 0xFFF8_0000l
    let bare_nan = 0xFFF8_0000l
    let to_hex_string = Printf.sprintf "%lx"
  end)
  module F64 = Fxx.Make(struct
    include Int64
    let mantissa = 52
    let pos_nan = 0x7FF8_0000_0000_0000L
    let neg_nan = 0xFFF8_0000_0000_0000L
    let bare_nan = 0x7FF0_0000_0000_0000L
    let to_hex_string = Printf.sprintf "%Lx"
  end)
  
  module V128 : sig
    type t
    type bits = string
    
    type ('i8x16, 'i16x8, 'i32x4, 'i64x2, 'f32x4, 'f64x2) laneop =
      | I8x16 of 'i8x16 | I16x8 of 'i16x8 | I32x4 of 'i32x4 | I64x2 of 'i64x2
      | F32x4 of 'f32x4 | F64x2 of 'f64x2
    type shape = (unit, unit, unit, unit, unit, unit) laneop
  end = struct
    type t = string
    type bits = string
    
    type ('i8x16, 'i16x8, 'i32x4, 'i64x2, 'f32x4, 'f64x2) laneop =
      | I8x16 of 'i8x16 | I16x8 of 'i16x8 | I32x4 of 'i32x4 | I64x2 of 'i64x2
      | F32x4 of 'f32x4 | F64x2 of 'f64x2
    type shape = (unit, unit, unit, unit, unit, unit) laneop
  end
  
  type ('i32, 'i64, 'f32, 'f64) op = I32 of 'i32 | I64 of 'i64 | F32 of 'f32 | F64 of 'f64
  type ('v128) vecop = V128 of 'v128
  
  type num = (I32.t, I64.t, F32.t, F64.t) op
  type vec = (V128.t) vecop
  
  let type_of_op op = let open Type in match op with I32 _ -> I32T | I64 _ -> I64T | F32 _ -> F32T | F64 _ -> F64T
  let type_of_vecop vecop = let open Type in match vecop with V128 _ -> V128T
  let type_of_num = type_of_op
  let type_of_vec = type_of_vecop
  
  let string_of_num = function
    | I32 i -> I32.to_string_s i
    | I64 i -> I64.to_string_s i
    | F32 z -> F32.to_string z
    | F64 z -> F64.to_string z
end

module Instruction = struct
  open Type
  open Value
  
  module IntOp = struct
    type unop = |
    type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
    type testop = Eqz
    type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
    type cvtop = |
    
    let unop _ (op : unop) : string = match op with _ -> .
    let binop _ (op : binop) : string = match op with
      | Add -> "add"
      | Sub -> "sub"
      | Mul -> "mul"
      | DivS -> "div_s"
      | DivU -> "div_u"
      | RemS -> "rem_s"
      | RemU -> "rem_u"
    let testop _ (op : testop) : string = match op with
      | Eqz -> "eqz"
    let relop _ (op : relop) : string = match op with
      | Eq -> "eq"
      | Ne -> "ne"
      | LtS -> "lt_s"
      | LtU -> "lt_u"
      | GtS -> "gt_s"
      | GtU -> "gt_u"
      | LeS -> "le_s"
      | LeU -> "le_u"
      | GeS -> "ge_s"
      | GeU -> "ge_u"
    let cvtop _ (op : cvtop) : string = match op with _ -> .
  end
  module FloatOp = struct
    type unop = Neg
    type binop = Add | Sub | Mul | Div
    type testop = |
    type relop = Eq | Ne
    type cvtop = |
    
    let unop _ (op : unop) : string = match op with
      | Neg -> "neg"
    let binop _ (op : binop) : string = match op with
      | Add -> "add"
      | Sub -> "sub"
      | Mul -> "mul"
      | Div -> "div"
    let testop _ (op : testop) : string = match op with _ -> .
    let relop _ (op : relop) : string = match op with
      | Eq -> "eq"
      | Ne -> "ne"
    let cvtop _ (op : cvtop) : string = match op with _ -> .
  end
  
  module I32Op = IntOp
  module I64Op = IntOp
  module F32Op = FloatOp
  module F64Op = FloatOp
  
  type unop = (I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop) op
  type binop = (I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop) op
  type testop = (I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop) op
  type relop = (I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop) op
  type cvtop = (I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop) op
  
  type initop = Explicit | Implicit
  type hdl = OnLabel of int32 | OnSwitch
  
  type t =
    | Unreachable
    | Nop
    | Drop
    | Const of num
    | Unop of unop
    | Binop of binop
    | Testop of testop
    | Relop of relop
    | Cvtop of cvtop
    | LocalGet of int32
    | LocalSet of int32
    | GlobalGet of int32
    | GlobalSet of int32
    | Block of block_type * t list
    | Loop of block_type * t list
    | If of block_type * t list * t list
    | Br of int32
    | Return
    | Call of int32
    | CallRef of int32
    | ReturnCall of int32
    | ReturnCallRef of int32
    | RefNull of heap_type
    | RefCast of ref_type
    | RefFunc of int32
    | ContNew of int32
    | Suspend of int32
    | Resume of int32 * (int32 * hdl) list
    | StructNew of int32 * initop
    | StructGet of int32 * int32 * Pack.extension option
    | StructSet of int32 * int32
  
  let constop v = string_of_num_type (type_of_num v) ^ ".const"
  let vec_constop v = string_of_vec_type (type_of_vec v) ^ ".const i32x4"
  let oper (iop, fop) op = string_of_num_type (type_of_op op) ^ "." ^ (match op with
    | I32 o -> iop "32" o
    | I64 o -> iop "64" o
    | F32 o -> fop "32" o
    | F64 o -> fop "64" o)
  
  let sexpr_of_idxhdl (i, h) = let open Sexpr in LongNode ("on", [
      Atom (Int32.to_string i);
      Atom (match h with OnLabel l -> Int32.to_string l | OnSwitch -> "switch")
    ], [])
  
  let rec to_sexpr i = let open Sexpr in match i with
    | Unreachable -> Node ("unreachable", [])
    | Nop -> Node ("nop", [])
    | Drop -> Node ("drop", [])
    | Const n -> Node (constop n ^ " " ^ string_of_num n, [])
    | Unop op -> Node (oper (IntOp.unop, FloatOp.unop) op, [])
    | Binop op -> Node (oper (IntOp.binop, FloatOp.binop) op, [])
    | Testop op -> Node (oper (IntOp.testop, FloatOp.testop) op, [])
    | Relop op -> Node (oper (IntOp.relop, FloatOp.relop) op, [])
    | Cvtop op -> Node (oper (IntOp.cvtop, FloatOp.cvtop) op, [])
    | LocalGet i -> LongNode ("local.get", [Atom (Int32.to_string i)], [])
    | LocalSet i -> LongNode ("local.set", [Atom (Int32.to_string i)], [])
    | GlobalGet i -> LongNode ("global.get", [Atom (Int32.to_string i)], [])
    | GlobalSet i -> LongNode ("global.set", [Atom (Int32.to_string i)], [])
    | Block (bt, b) -> LongNode ("block", sexpr_of_block_type bt, List.map to_sexpr b)
    | Loop (bt, b) -> LongNode ("loop", sexpr_of_block_type bt, List.map to_sexpr b)
    | If (b, t, f) -> LongNode ("if", sexpr_of_block_type b, [Node ("then", List.map to_sexpr t); Node ("else", List.map to_sexpr f)])
    | Br i -> LongNode ("br", [Atom (Int32.to_string i)], [])
    | Return -> Node ("return", [])
    | Call i -> LongNode ("call", [Atom (Int32.to_string i)], [])
    | CallRef i -> LongNode ("call_ref", [Atom (Int32.to_string i)], [])
    | ReturnCall i -> LongNode ("return_call", [Atom (Int32.to_string i)], [])
    | ReturnCallRef i -> LongNode ("return_call_ref", [Atom (Int32.to_string i)], [])
    | RefNull ht -> LongNode ("ref.null", [sexpr_of_heap_type ht], [])
    | RefCast rt -> LongNode ("ref.cast", [sexpr_of_ref_type rt], [])
    | RefFunc ti -> LongNode ("ref.func", [Atom (Int32.to_string ti)], [])
    | ContNew i -> LongNode ("cont.new", [Atom (Int32.to_string i)], [])
    | Suspend i -> LongNode ("suspend", [Atom (Int32.to_string i)], [])
    | Resume (i, hdls) -> LongNode ("resume", [Atom (Int32.to_string i)], List.map sexpr_of_idxhdl hdls)
    | StructNew (i, Explicit) -> LongNode ("struct.new", [Atom (Int32.to_string i)], [])
    | StructNew (i, Implicit) -> LongNode ("struct.new_default", [Atom (Int32.to_string i)], [])
    | StructGet (i, j, None) -> LongNode ("struct.get", [Atom (Int32.to_string i); Atom (Int32.to_string j)], [])
    | StructGet (i, j, Some Pack.SX) -> LongNode ("struct.get_s", [Atom (Int32.to_string i); Atom (Int32.to_string j)], [])
    | StructGet (i, j, Some Pack.ZX) -> LongNode ("struct.get_u", [Atom (Int32.to_string i); Atom (Int32.to_string j)], [])
    | StructSet (i, j) -> LongNode ("struct.new", [Atom (Int32.to_string i); Atom (Int32.to_string j)], [])
end

type raw_code = Type.val_type * Type.val_type list * Instruction.t list
type fundef = {
  fn_name  : string option;
  fn_type  : int32;
  fn_locals: Type.val_type list;
  fn_code  : Instruction.t list;
}
type global = Type.global_type * Instruction.t list * string option
type module_ = {
  types  : Type.rec_type list;
  globals: global list;
  tags   : int32 list;
  funs   : fundef list;
  init   : int32 option;
}

let sexpr_of_global ((gt, i, oname) : global) : Sexpr.t =
  let online = Type.sexpr_of_global_type gt :: List.map Instruction.to_sexpr i in
  let online = match oname with
    | Some name -> Sexpr.(LongNode ("export", [Atom ("\"" ^ name ^ "\"")], [])) :: online
    | None -> online
  in Sexpr.(Node ("global", online))

let sexpr_of_function funid ({ fn_name = ofname; fn_type = typeid; fn_locals = locals; fn_code = instrs } : fundef) =
  let open Sexpr in
  let locs = if locals = [] then [] else [Node ("local", List.map Type.sexpr_of_val_type locals)] in
  let typeloc = Node ("type", [Atom (Int32.to_string typeid)]) :: locs in
  let online = match ofname with
    | Some fname -> Atom ("$" ^ string_of_int funid) :: LongNode ("export", [Atom ("\"" ^ fname ^ "\"")], []) :: typeloc
    | None -> Atom ("$" ^ string_of_int funid) :: typeloc
  in
  LongNode ("func", online, List.map Instruction.to_sexpr instrs)
let sexpr_of_raw_code ((ret, locals, instrs) : raw_code) =
  let open Sexpr in
  let locs = if locals = [] then [] else [Node ("local", List.map (fun t -> Atom (Type.string_of_val_type t)) locals)] in
  let resloc = Node ("result", [Type.sexpr_of_val_type ret]) :: locs in
  LongNode ("raw_func", resloc, List.map Instruction.to_sexpr instrs)
let sexpr_of_module (m : module_) =
  let open Sexpr in
  let styps = List.mapi (fun i rt -> LongNode ("type", [Atom ("$" ^ string_of_int i)], [Type.sexpr_of_rec_type rt])) m.types in
  let sgbls = List.map sexpr_of_global m.globals in
  let sfuns = List.mapi sexpr_of_function m.funs in
  let sinit = match m.init with Some init -> [LongNode ("start", [Atom (Int32.to_string init)], [])] | None -> [] in
  let stags = List.map (fun t -> Node ("tag", [LongNode ("type", [Atom (Int32.to_string t)], [])])) m.tags in
  Node ("module", styps @ sgbls @ sfuns @ sinit @ stags)

let pp_raw_code width fmt v = Sexpr.pp width fmt (sexpr_of_raw_code v)
let pp_function width fmt v = Sexpr.pp width fmt (sexpr_of_function 0 v)
let pp_module width fmt v = Sexpr.pp width fmt (sexpr_of_module v)
