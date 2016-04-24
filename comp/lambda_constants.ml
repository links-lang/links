open Lambda

type _ category =
  Compile_time_constant : compile_time_constant category
| Loc_kind : loc_kind category
| Boxed_integer : boxed_integer category
| Bigarray_kind : bigarray_kind category
| Bigarray_layout : bigarray_layout category
| Primitive : primitive category

type 'a result = [ `Constant of 'a
                 | `Unrecognized of 'a category * string ]

let resolve_constant : type a. a category -> string -> a result =
  fun category constant -> match category, constant with
  | Compile_time_constant, "big_endian"    -> `Constant Big_endian
  | Compile_time_constant, "word_size"     -> `Constant Word_size
  | Compile_time_constant, "ostype_unix"   -> `Constant Ostype_unix
  | Compile_time_constant, "ostype_win32"  -> `Constant Ostype_win32
  | Compile_time_constant, "ostype_cygwin" -> `Constant Ostype_cygwin
  | Loc_kind, "loc_FILE"   -> `Constant Loc_FILE
  | Loc_kind, "loc_LINE"   -> `Constant Loc_LINE
  | Loc_kind, "loc_MODULE" -> `Constant Loc_MODULE
  | Loc_kind, "loc_POS"    -> `Constant Loc_POS
  | Loc_kind, "loc_LOC"    -> `Constant Loc_LOC
  | Boxed_integer, "nativeint" -> `Constant Pnativeint
  | Boxed_integer, "int32"     -> `Constant Pint32
  | Boxed_integer, "int64"     -> `Constant Pint64
  | Bigarray_kind, "generic"   -> `Constant Pbigarray_unknown
  | Bigarray_kind, "float32"   -> `Constant Pbigarray_float32
  | Bigarray_kind, "float64"   -> `Constant Pbigarray_float64
  | Bigarray_kind, "sint8"     -> `Constant Pbigarray_sint8
  | Bigarray_kind, "uint8"     -> `Constant Pbigarray_uint8
  | Bigarray_kind, "sint16"    -> `Constant Pbigarray_sint16
  | Bigarray_kind, "uint16"    -> `Constant Pbigarray_uint16
  | Bigarray_kind, "int32"     -> `Constant Pbigarray_int32
  | Bigarray_kind, "int64"     -> `Constant Pbigarray_int64
  | Bigarray_kind, "camlint"   -> `Constant Pbigarray_caml_int
  | Bigarray_kind, "nativeint" -> `Constant Pbigarray_native_int
  | Bigarray_kind, "complex32" -> `Constant Pbigarray_complex32
  | Bigarray_kind, "complex64" -> `Constant Pbigarray_complex64
  | Bigarray_layout, "unknown" -> `Constant Pbigarray_unknown_layout
  | Bigarray_layout, "C"       -> `Constant Pbigarray_c_layout
  | Bigarray_layout, "Fortran" -> `Constant Pbigarray_fortran_layout
  (* | Pidentity -> fprintf ppf "id" *)
  (* | Pignore -> fprintf ppf "ignore" *)
  (* | Prevapply _ -> fprintf ppf "revapply" *)
  (* | Pdirapply _ -> fprintf ppf "dirapply" *)
  (* | Ploc kind -> fprintf ppf "%s" (string_of_loc_kind kind) *)
  (* | Pgetglobal id -> fprintf ppf "global %a" Ident.print id *)
  (* | Psetglobal id -> fprintf ppf "setglobal %a" Ident.print id *)
  (* | Pmakeblock(tag, Immutable) -> fprintf ppf "makeblock %i" tag *)
  (* | Pmakeblock(tag, Mutable) -> fprintf ppf "makemutable %i" tag *)
  (* | Pfield n -> fprintf ppf "field %i" n *)
  (* | Primitive, Psetfield(n, ptr) -> *)
  (*     let instr = if ptr then "setfield_ptr " else "setfield_imm " in *)
  (*     fprintf ppf "%s%i" instr n *)
  (* | Primitive, Pfloatfield n -> fprintf ppf "floatfield %i" n *)
  (* | Primitive, Psetfloatfield n -> fprintf ppf "setfloatfield %i" n *)
  (* | Primitive, Pduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size *)
  | Primitive, "force" -> `Constant Plazyforce
  (* | Primitive, Pccall p -> fprintf ppf "%s" p.prim_name *)
  (* | Primitive, Praise k -> fprintf ppf "%s" (Lambda.raise_kind k) *)
  | Primitive, "&&" -> `Constant Psequand
  | Primitive, "||" -> `Constant Psequor
  | Primitive, "not" -> `Constant Pnot
  | Primitive, "~" -> `Constant Pnegint
  | Primitive, "+" -> `Constant Paddint
  | Primitive, "-" -> `Constant Psubint
  | Primitive, "*" -> `Constant Pmulint
  | Primitive, "/" -> `Constant Pdivint
  | Primitive, "mod" -> `Constant Pmodint
  | Primitive, "and" -> `Constant Pandint
  | Primitive, "or" -> `Constant Porint
  | Primitive, "xor" -> `Constant Pxorint
  | Primitive, "lsl" -> `Constant Plslint
  | Primitive, "lsr" -> `Constant Plsrint
  | Primitive, "asr" -> `Constant Pasrint
  | Primitive, "==" -> `Constant (Pintcomp(Ceq))
  | Primitive, "!=" -> `Constant (Pintcomp(Cneq))
  | Primitive, "<" -> `Constant (Pintcomp(Clt))
  | Primitive, "<=" -> `Constant (Pintcomp(Cle))
  | Primitive, ">" -> `Constant (Pintcomp(Cgt))
  | Primitive, ">=" -> `Constant (Pintcomp(Cge))
  (* | Primitive, Poffsetint n -> fprintf ppf "%i+" n *)
  (* | Primitive, Poffsetref n -> fprintf ppf "+:=%i"n *)
  | Primitive, "int_of_float" -> `Constant Pintoffloat
  | Primitive, "float_of_int" -> `Constant Pfloatofint
  | Primitive, "~." -> `Constant Pnegfloat
  | Primitive, "abs." -> `Constant Pabsfloat
  | Primitive, "+." -> `Constant Paddfloat
  | Primitive, "-." -> `Constant Psubfloat
  | Primitive, "*." -> `Constant Pmulfloat
  | Primitive, "/." -> `Constant Pdivfloat
  | Primitive, "==." -> `Constant (Pfloatcomp(Ceq))
  | Primitive, "!=." -> `Constant (Pfloatcomp(Cneq))
  | Primitive, "<." -> `Constant (Pfloatcomp(Clt))
  | Primitive, "<=." -> `Constant (Pfloatcomp(Cle))
  | Primitive, ">." -> `Constant (Pfloatcomp(Cgt))
  | Primitive, ">=." -> `Constant (Pfloatcomp(Cge))
  | Primitive, "string.length" -> `Constant Pstringlength
  | Primitive, "string.unsafe_get" -> `Constant Pstringrefu
  | Primitive, "string.unsafe_set" -> `Constant Pstringsetu
  | Primitive, "string.get" -> `Constant Pstringrefs
  | Primitive, "string.set" -> `Constant Pstringsets
  (* | Primitive, "array.length" -> `Constant Parraylength _ *)
  (* | Primitive, "makearray " -> `Constant Pmakearray _ *)
  (* | Primitive, "array.unsafe_get" -> `Constant Parrayrefu _ *)
  (* | Primitive, "array.unsafe_set" -> `Constant Parraysetu _ *)
  (* | Primitive, "array.get" -> `Constant Parrayrefs _ *)
  (* | Primitive, "array.set" -> `Constant Parraysets _ *)
  (* | Primitive, Pctconst c -> *)
  (*    let const_name = match c with *)
  (*      | Primitive, Big_endian -> "big_endian" *)
  (*      | Primitive, Word_size -> "word_size" *)
  (*      | Primitive, Ostype_unix -> "ostype_unix" *)
  (*      | Primitive, Ostype_win32 -> "ostype_win32" *)
  (*      | Primitive, Ostype_cygwin -> "ostype_cygwin" in *)
  (*    fprintf ppf "sys.constant_%s" const_name *)
  | Primitive, "isint" -> `Constant Pisint
  | Primitive, "isout" -> `Constant Pisout
  | Primitive, "testbit" -> `Constant Pbittest
  (* | Primitive, Pbintofint bi -> print_boxed_integer "of_int" ppf bi *)
  (* | Primitive, Pintofbint bi -> print_boxed_integer "to_int" ppf bi *)
  (* | Primitive, Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2 *)
  (* | Primitive, Pnegbint bi -> print_boxed_integer "neg" ppf bi *)
  (* | Primitive, Paddbint bi -> print_boxed_integer "add" ppf bi *)
  (* | Primitive, Psubbint bi -> print_boxed_integer "sub" ppf bi *)
  (* | Primitive, Pmulbint bi -> print_boxed_integer "mul" ppf bi *)
  (* | Primitive, Pdivbint bi -> print_boxed_integer "div" ppf bi *)
  (* | Primitive, Pmodbint bi -> print_boxed_integer "mod" ppf bi *)
  (* | Primitive, Pandbint bi -> print_boxed_integer "and" ppf bi *)
  (* | Primitive, Porbint bi -> print_boxed_integer "or" ppf bi *)
  (* | Primitive, Pxorbint bi -> print_boxed_integer "xor" ppf bi *)
  (* | Primitive, Plslbint bi -> print_boxed_integer "lsl" ppf bi *)
  (* | Primitive, Plsrbint bi -> print_boxed_integer "lsr" ppf bi *)
  (* | Primitive, Pasrbint bi -> print_boxed_integer "asr" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi *)
  (* | Primitive, Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi *)
  (* | Primitive, Pbigarrayref(unsafe, n, kind, layout) -> *)
  (*     print_bigarray "get" unsafe kind ppf layout *)
  (* | Primitive, Pbigarrayset(unsafe, n, kind, layout) -> *)
  (*     print_bigarray "set" unsafe kind ppf layout *)
  (* | Primitive, Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n *)
  | Primitive, "string.unsafe_get16" -> `Constant (Pstring_load_16 true)
  | Primitive, "string.get16" -> `Constant (Pstring_load_16 false)
  | Primitive, "string.unsafe_get32" -> `Constant (Pstring_load_32 true)
  | Primitive, "string.get32" -> `Constant (Pstring_load_32 false)
  | Primitive, "string.unsafe_get64" -> `Constant (Pstring_load_64 true)
  | Primitive, "string.get64" -> `Constant (Pstring_load_64 false)
  | Primitive, "string.unsafe_set16" -> `Constant (Pstring_set_16 true)
  | Primitive, "string.set16" -> `Constant (Pstring_set_16 false)
  | Primitive, "string.unsafe_set32" -> `Constant (Pstring_set_32 true)
  | Primitive, "string.set32" -> `Constant (Pstring_set_32 false)
  | Primitive, "string.unsafe_set64" -> `Constant (Pstring_set_64 true)
  | Primitive, "string.set64" -> `Constant (Pstring_set_64 false)
  | Primitive, "bigarray.array1.unsafe_get16" -> `Constant (Pbigstring_load_16 true)
  | Primitive, "bigarray.array1.get16" -> `Constant (Pbigstring_load_16 false)
  | Primitive, "bigarray.array1.unsafe_get32" -> `Constant (Pbigstring_load_32 true)
  | Primitive, "bigarray.array1.get32" -> `Constant (Pbigstring_load_32 false)
  | Primitive, "bigarray.array1.unsafe_get64" -> `Constant (Pbigstring_load_64 true)
  | Primitive, "bigarray.array1.get64" -> `Constant (Pbigstring_load_64 false)
  | Primitive, "bigarray.array1.unsafe_set16" -> `Constant (Pbigstring_set_16 true)
  | Primitive, "bigarray.array1.set16" -> `Constant (Pbigstring_set_16 false)
  | Primitive, "bigarray.array1.unsafe_set32" -> `Constant (Pbigstring_set_32 true)
  | Primitive, "bigarray.array1.set32" -> `Constant (Pbigstring_set_32 false)
  | Primitive, "bigarray.array1.unsafe_set64" -> `Constant (Pbigstring_set_64 true)
  | Primitive, "bigarray.array1.set64" -> `Constant (Pbigstring_set_64 false)
  | Primitive, "bswap16" -> `Constant Pbswap16
  (* | Primitive, Pbbswap(bi) -> print_boxed_integer "bswap" ppf bi *)
  | Primitive, "int_as_pointer" -> `Constant Pint_as_pointer

  | Primitive, _ -> `Unrecognized (category, constant)
