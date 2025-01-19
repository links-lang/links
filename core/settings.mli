type 'a setting
type privilege = [ `System | `User ]

exception Unknown_setting of string

(* Read and write settings. *)
val get : 'a setting -> 'a
val set : ?privilege:privilege -> 'a setting -> 'a -> unit

(* Create settings. *)
val flag : ?default:bool -> string -> bool setting
val option : ?default:'a option -> ?readonly:bool -> string -> 'a option setting
val multi_option : ?default:'a list -> string -> 'a list setting

(* Decorating settings. *)
val synopsis : string -> 'a setting -> 'a setting
val action : ('a -> unit) -> 'a setting -> 'a setting
val convert : (string -> 'a) -> 'a setting -> 'a setting
val to_string : ('a -> string) -> 'a setting -> 'a setting
val privilege : privilege -> 'a setting -> 'a setting
val depends : bool setting -> bool setting -> bool setting
val hint : string -> 'a setting -> 'a setting
val hidden : 'a setting -> 'a setting
val show_default : bool -> 'a setting -> 'a setting
val keep_default : 'a list setting -> 'a list setting

(* CLI argument construction. *)
module CLI: sig
  type arg
  val add : (arg -> arg) -> 'a setting -> 'a setting
  val (<&>) : (arg -> arg) -> (arg -> arg) -> (arg -> arg)
  val long : string -> arg -> arg
  val short : char -> arg -> arg
end

(* Synchronise settings. *)
val sync : 'a setting -> 'a setting

(* Miscellaneous. *)
val get_name : 'a setting -> string
val from_string_option : string option -> string
val string_of_paths : string list -> string
val parse_paths : string -> string list
val parse_bool : string -> bool
val parse_and_set_user : string -> string -> unit

(* Pretty-printing. *)
val print_settings : out_channel -> unit
val print_cli_options : out_channel -> unit

(* Describe settings. *)
module Reflection: sig
  type t =
    { name: string;
      current_value: string option;
      default: string option;
      value_hint: string option;
      synopsis: string option;
      kind: [`Flag | `Option | `MultiOption ]
    }

  val reflect : string -> t
end

(* Getters for anonymous and rest arguments. *)
val get_anonymous_arguments : unit -> string list
val get_rest_arguments : unit -> string list

(* The following handler exits if any CLI argument is left
   unhandled. *)
val ensure_all_synchronised : unit -> unit
(* The following handler ignores any unhandled argument. *)
val synchronise_defined : unit -> unit
