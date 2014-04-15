(** Pretty-printing *)

(* Dictionary *)
type -'a show = {
  format : Format.formatter -> 'a -> unit ;
}

(* Overloaded function *)
val show : 'a show -> 'a -> string

(* Instances *)

val show_unprintable : 'a show

val show_char      : char show
val show_bool      : bool show
val show_unit      : unit show
val show_int       : int show
val show_int32     : int32 show
val show_int64     : int64 show
val show_nativeint : nativeint show
val show_num       : Num.num show
val show_float     : float show
val show_string    : string show
val show_open_flag : open_flag show
val show_fpclass   : fpclass show

val show_list   : 'a show -> 'a list show
val show_ref    : 'a show -> 'a ref show
val show_option : 'a show -> 'a option show
val show_array  : 'a show -> 'a array show

module Show_map (O : Map.OrderedType) : sig
  val show_t : O.t show -> 'a show -> 'a Map.Make(O).t show
end

module Show_set (O : Set.OrderedType) : sig
  val show_t : O.t show -> Set.Make(O).t show
end

val show_6 : 'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> 'a5 show -> 'a6 show -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) show
val show_5 : 'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> 'a5 show -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) show
val show_4 : 'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> ('a1 * 'a2 * 'a3 * 'a4) show
val show_3 : 'a1 show -> 'a2 show -> 'a3 show -> ('a1 * 'a2 * 'a3) show
val show_2 : 'a1 show -> 'a2 show -> ('a1 * 'a2) show
