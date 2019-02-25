open Utility
open CommonTypes

type constant =
    [ `Float  of float
    | `Int    of int
    | `String of string
    | `Bool   of bool
    | `Char   of char ]
      [@@deriving show]

let constant_type = function
  | `Float _  -> `Primitive Primitive.Float
  | `Int _    -> `Primitive Primitive.Int
  | `Bool _   -> `Primitive Primitive.Bool
  | `Char _   -> `Primitive Primitive.Char
  | `String _ -> `Primitive Primitive.String

let escape_string s = (* SQL standard for escaping single quotes in a string *)
  Str.global_replace (Str.regexp "'") "''" s

let string_of_constant =
  (* This function is actually specific to database query generation;
     it should be moved to the database module(s). *)
  function
    | `Bool value  -> string_of_bool value
    | `Int value   -> string_of_int value
    | `Char c      -> "'"^ Char.escaped c ^"'"
    | `String s    -> "'" ^ escape_string s ^ "'"
    | `Float value -> string_of_float' value
