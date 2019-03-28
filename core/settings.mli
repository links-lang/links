(*
  Compiler settings
*)

(* abstract type of a setting
     this should encode a name and a mutable value
 *)
type 'a setting

type mode = [`User | `System]

(* add a new setting *)
val add_bool : ?hook:(bool -> unit) -> (string * bool * mode) -> bool setting
val add_int : ?hook:(int -> unit) -> (string * int * mode) -> int setting
val add_string : ?hook:(string -> unit) -> (string * string * mode) -> string setting

(*
  parse_and_set(name, value)
    bind name to the parsed value
*)
val parse_and_set : string * string -> bool -> unit

(*
 parse_and_set_user(name, value)
   as parse_and_set(name, value), but only user settings can be set
*)
val parse_and_set_user : string * string -> bool -> unit

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
val load_file : bool -> string -> unit
