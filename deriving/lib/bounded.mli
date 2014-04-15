type +'a bounded = {
  min_bound : 'a ;
  max_bound : 'a 
}

val bounded_bool      : bool bounded
val bounded_char      : char bounded
val bounded_int       : int bounded
val bounded_int32     : int32 bounded
val bounded_int64     : int64 bounded
val bounded_nativeint : nativeint bounded
val bounded_unit      : unit bounded
val bounded_open_flag : Pervasives.open_flag bounded
val bounded_fpclass   : Pervasives.fpclass bounded
