(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Error of int * int * string

val token: Ulexing.lexbuf -> (string * string) * (int * int)
val lex: (string * string) Token.glexer

val in_comment: bool ref
val lexbuf: Ulexing.lexbuf option ref
val enc: Ulexing.enc ref
val last_tok: (string * string) ref

val dump_file: string -> unit
