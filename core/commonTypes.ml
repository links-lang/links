(* This module contains common datatypes used in ASTs in both the frontend
   (Sugartypes) and typechecker (Types). *)

open Utility

module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]

  let is_any = function
    | Any -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false

  let to_string = function
    | Any -> "Any"
    | Unl -> "Unl"
end

(* Convenient aliases for constructing values *)
let lin_any = Linearity.Any
let lin_unl = Linearity.Unl

module DeclaredLinearity = struct
  type t = Lin | Unl
    [@@deriving show]

  let is_linear = function
    | Lin -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false
end

(* Convenient aliases for constructing values *)
let dl_lin = DeclaredLinearity.Lin
let dl_unl = DeclaredLinearity.Unl

module Restriction = struct
  type t =
    | Any
    | Base
    | Session
    | Effect
    [@@deriving eq,show]

  let is_any = function
    | Any -> true
    | _   -> false

  let is_base = function
    | Base -> true
    | _    -> false

  let is_session = function
    | Session -> true
    | _       -> false

  let is_effect = function
    | Effect -> true
    | _      -> false

  let to_string = function
    | Any     -> "Any"
    | Base    -> "Base"
    | Session -> "Session"
    | Effect  -> "Eff"
end

(* Convenient aliases for constructing values *)
let res_any     = Restriction.Any
let res_base    = Restriction.Base
let res_session = Restriction.Session
let res_effect  = Restriction.Effect

type subkind = Linearity.t * Restriction.t
    [@@deriving eq,show]

module PrimaryKind = struct
  type t =
    | Type
    | Row
    | Presence
    [@@deriving show,eq]

  let to_string = function
    | Type     -> "Type"
    | Row      -> "Row"
    | Presence -> "Presence"
end

(* Convenient aliases for constructing values *)
let pk_type     = PrimaryKind.Type
let pk_row      = PrimaryKind.Row
let pk_presence = PrimaryKind.Presence

module Location = struct
  type t = Client | Server | Native | Unknown
    [@@deriving show]

  let is_client = function
    | Client -> true
    | _      -> false

  let is_server = function
    | Server -> true
    | _      -> false

  let is_native = function
    | Native -> true
    | _      -> false

  let is_unknown = function
    | Unknown -> true
    | _      -> false

  let to_string = function
    | Client -> "client"
    | Server -> "server"
    | Native -> "native"
    | Unknown -> "unknown"
end

(* Convenient aliases for constructing values *)
let loc_client  = Location.Client
let loc_server  = Location.Server
let loc_native  = Location.Native
let loc_unknown = Location.Unknown

type freedom = [`Flexible | `Rigid]
    [@@deriving show]

module Primitive = struct
  type t = Bool | Int | Char | Float | XmlItem | DB | String
    [@@deriving show]

  let to_string = function
    | Bool    -> "Bool"
    | Int     -> "Int"
    | Char    -> "Char"
    | Float   -> "Float"
    | XmlItem -> "XmlItem"
    | DB      -> "Database"
    | String  -> "String"
end

module Constant = struct
  type t =
    | Float  of float
    | Int    of int
    | Bool   of bool
    | String of string
    | Char   of char
      [@@deriving show]

  let type_of = function
    | Float  _ -> Primitive.Float
    | Int    _ -> Primitive.Int
    | Bool   _ -> Primitive.Bool
    | Char   _ -> Primitive.Char
    | String _ -> Primitive.String

  (* SQL standard for escaping single quotes in a string *)
  let escape_string s =
    Str.global_replace (Str.regexp "'") "''" s

  (* This function is actually specific to database query generation; it should
     be moved to the database module(s). *)
  let to_string = function
    | Bool value  -> string_of_bool value
    | Int value   -> string_of_int value
    | Char c      -> "'"^ Char.escaped c ^"'"
    | String s    -> "'" ^ escape_string s ^ "'"
    | Float value -> string_of_float' value
end
