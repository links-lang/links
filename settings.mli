(* 
  Compiler settings
*)

(* abstract type of a setting
     this should encode a name and a mutable value
 *)
type 'a setting

(* add a new setting *)
val add_bool : (string * bool * bool) -> bool setting
val add_int : (string * int * bool) -> int setting
val add_string : (string * string * bool) -> string setting 

(*
  parse_and_set(name, value)
    bind name to the parsed value
*)
val parse_and_set : string * string -> unit

(*
 parse_and_set_user(name, value)
   as parse_and_set(name, value), but only user settings can be set
*)
val parse_and_set_user : string * string -> unit

(* lookup a setting *)
val lookup_bool : string -> bool setting
val lookup_int : string -> int setting
val lookup_string : string -> string setting

(* get the value of a setting *)
val get_value : 'a setting -> 'a

(* get the name of a setting *)
val get_name : 'a setting -> string

(* set the value of a setting *)
val set_value : 'a setting -> 'a -> unit

(* print all registered settings *)
val print_settings : unit -> string list

(* ... *)
val from_argv : string array -> string list

(* ... *)
val load_file : string -> unit
