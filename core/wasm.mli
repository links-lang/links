module Sexpr : sig
  (* A LongNode is a Node with some arguments forced on the same line as the node type *)
  type t = Atom of string | Node of string * t list | LongNode of string * t list * t list
  
  val pp : int -> Format.formatter -> t -> unit
  val output : out_channel -> int -> t -> unit
  val print : int -> t -> unit
  val to_string : int -> t -> string
end

module Unicode : sig
  type codepoint
  type t
  
  exception Utf8
  
  val utf8_encode : t -> string
  val utf8_decode : string -> t
end

module Pack : sig
  type pack_size = Pack8 | Pack16 | Pack32 | Pack64
  type extension = SX | ZX
  
  type pack_shape = Pack8x8 | Pack16x4 | Pack32x2
  type vec_extension =
    | ExtLane of pack_shape * extension
    | ExtSplat
    | ExtZero
  
  val packed_size : pack_size -> int
  val packed_shape_size : pack_shape -> int
end

module Type : sig
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
  
  val string_of_val_type : val_type -> string
  val string_of_rec_type : rec_type -> string
  val sexpr_of_rec_type : rec_type -> Sexpr.t
end

module Value : sig
  exception Overflow
  exception DivideByZero
  exception InvalidConversion
  
  module Ixx : sig
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
  end
  module I32 : Ixx.S with type bits = Int32.t
  module I64 : Ixx.S with type bits = Int64.t
  
  module Fxx : sig
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
  end
  module F32 : Fxx.S with type bits = Int32.t
  module F64 : Fxx.S with type bits = Int64.t
  
  module V128 : sig
    type t
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
  
  val type_of_op : ('i32, 'i64, 'f32, 'f64) op -> Type.num_type
  val type_of_vecop : ('v128) vecop -> Type.vec_type
  val type_of_num : num -> Type.num_type
  val type_of_vec : vec -> Type.vec_type
end

module Instruction : sig
  open Type
  open Value
  
  module IntOp : sig
    type unop = |
    type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
    type testop = Eqz
    type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
    type cvtop = WrapI64 | ReinterpretFloat
  end
  module FloatOp : sig
    type unop = Neg
    type binop = Add | Sub | Mul | Div
    type testop = |
    type relop = Eq | Ne
    type cvtop = ReinterpretInt
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
    | BrIf of int32
    | BrOnNull of int32
    | BrOnNonNull of int32
    | BrTable of int32 list * int32
    | Return
    | Call of int32
    | CallRef of int32
    | ReturnCall of int32
    | ReturnCallRef of int32
    | RefNull of heap_type
    | RefI31
    | RefFunc of int32
    | RefCast of ref_type
    | RefIsNull
    | RefAsNonNull
    | I31Get of Pack.extension
    | ContNew of int32
    | Suspend of int32
    | Resume of int32 * (int32 * hdl) list
    | StructNew of int32 * initop
    | StructGet of int32 * int32 * Pack.extension option
    | StructSet of int32 * int32
    | ArrayNew of int32 * initop
    | ArrayNewFixed of int32 * int32
    | ArrayGet of int32 * Pack.extension option
    | ArraySet of int32
    | ArrayLen
    | ArrayCopy of int32 * int32
  
  val to_sexpr : t -> Sexpr.t
end

type import_desc =
  | FuncImport of int32
  | TagImport of int32

type import = {
  module_name : string;
  item_name : string;
  desc : import_desc;
}

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
  imports: import list;
  init   : int32 option;
}

val sexpr_of_raw_code : raw_code -> Sexpr.t
val sexpr_of_function : int -> fundef -> Sexpr.t
val sexpr_of_module : module_ -> Sexpr.t

val pp_raw_code : int -> Format.formatter -> raw_code -> unit
val pp_function : int -> Format.formatter -> fundef -> unit
val pp_module : int -> Format.formatter -> module_ -> unit
