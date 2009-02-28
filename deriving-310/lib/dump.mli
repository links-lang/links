type 'a dump = {
  to_buffer : Buffer.t -> 'a -> unit ;
  from_stream : char Stream.t -> 'a 
}

val to_string    : 'a dump -> 'a -> string
val to_channel   : 'a dump -> out_channel -> 'a -> unit
val to_buffer    : 'a dump -> Buffer.t -> 'a -> unit
val from_string  : 'a dump -> string -> 'a
val from_channel : 'a dump -> in_channel -> 'a
val from_stream  : 'a dump -> char Stream.t -> 'a

exception Dump_error of string

val dump_int32     : Int32.t dump
val dump_int64     : Int64.t dump
val dump_nativeint : Nativeint.t dump
val dump_int       : int dump
val dump_char      : char dump
val dump_string    : string dump
val dump_float     : float dump
val dump_num       : Num.num dump
val dump_bool      : bool dump
val dump_unit      : unit dump

val dump_list      : 'a dump ->  'a list dump
val dump_option    : 'a dump -> 'a option dump

val dump_undumpable : string -> 'a dump
val dump_via_marshal : 'a dump
