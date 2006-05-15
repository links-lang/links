(* 
  Compiler settings
*)

(* abstract type of a setting
     this should encode a name and a mutable value
 *)
type 'a setting

(* add a new setting *)
val add_bool : bool -> string -> bool setting
val add_int : int -> string -> int setting
val add_string : string -> string -> string setting 

(* lookup a setting *)
val lookup_bool : string -> bool setting
val lookup_int : string -> int setting
val lookup_string : string -> string setting

(* get the value of a setting *)
val get_value : 'a setting -> 'a

(* get the name of a setting *)
val get_name : 'a setting -> string

(* set the value of a setting *)
val set_value : 'a setting * 'a -> unit
