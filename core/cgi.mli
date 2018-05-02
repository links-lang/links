(*
 * ocamlcgi - Objective Caml library for writing CGIs
 * Copyright (C) 1997 Daniel de Rauglaudre, INRIA
 * Copyright (C) 1998 Jean-Christophe FILLIATRE
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* $Id: cgi.mli,v 1.5 2002/09/20 08:04:59 filliatr Exp $ *)

(* Decodes the CGI arguments. Returns an association list.
   Works whatever the method is (GET or POST) *)
val parse_args : unit -> (string * string) list

(* Decodes the CGI arguments for multipart/form-data encoding. *)
type field_data = {
  value : string;
  filename : string;
  content_type : string;
} 
val parse_multipart_args : unit -> (string * field_data) list

(* Prints the content-type header. 
   the argument is the MIME type (default value is "text/html" if the
   argument is the empty string) *)
val header : string -> unit

(* Returns the address of the CGI *)
val this_url : unit -> string

(* The list of items found in PATH\_INFO *)
val path_info : string list

(* Given a zero-based index, returns the [i]-th info element;
   returns the empty string if [i] is out of bounds *)
val nth_path_info : int -> string

(* Coding and uncoding of CGI arguments.
   The following code may be useful but is already called in [parse_args].
   Code from wserver, (c) Daniel de Rauglaudre, INRIA. *)

val decode : string -> string
val encode : string -> string
