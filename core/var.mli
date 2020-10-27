(** {0 IR variables} *)

module Scope: sig
  type t = Local | Global
  [@@deriving show]

  val is_global : t -> bool
  val is_local : t -> bool
end

(** Term variables *)
type var = int
  [@@deriving show,eq,yojson]
type var_info
  [@@deriving show]
type binder
  [@@deriving show]

val dummy_var : int

(** Generate just a fresh identifier *)
val fresh_raw_var : unit -> var

(** Given metadata, generate a full binder *)
val fresh_binder : var_info -> binder

(** Given metadata, generate a full binder and pair it with the new
    variable identifer; note this identifier is already the first
    component of the [binder] value *)
val fresh_var : var_info -> binder * var

(** {0 Manipulate binder metadata} *)

val info_type : var_info -> Types.datatype
val info_of_type : Types.datatype -> var_info

val make_info : Types.datatype -> string -> Scope.t -> var_info
val make_local_info : (Types.datatype * string) -> var_info
val make_global_info : (Types.datatype * string) -> var_info

val make_binder : var -> var_info -> binder
val update_type : Types.datatype -> binder -> binder

val fresh_binder_of_type : Types.datatype -> binder
val fresh_var_of_type : Types.datatype -> binder * var
val fresh_global_var_of_type : Types.datatype -> binder * var

val var_of_binder : binder -> var
val info_of_binder : binder -> var_info
val type_of_binder : binder -> Types.datatype
val name_of_binder : binder -> string
val scope_of_binder : binder -> Scope.t

val globalise_binder : binder -> binder

(** Create a copy of a type environment mapping vars (= ints) to types
    instead of strings to types
*)
val varify_env : (int Env.String.t * Types.datatype Env.String.t) -> Types.datatype Env.Int.t
