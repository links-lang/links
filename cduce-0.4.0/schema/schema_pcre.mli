(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

  (* Wrappers for some Pcre functions working on Utf8.t values.
   * Given Pcre.regexp regular expressions should be compiled with `UTF8 flag
   * or with pcre_regexp below *)
open Encodings.Utf8
val pcre_regexp: string -> Pcre.regexp  (* compile using `UTF8 flag *)
val pcre_replace: rex:Pcre.regexp -> ?templ:t -> t -> t
val pcre_extract: rex:Pcre.regexp -> t -> t array
val pcre_split: rex:Pcre.regexp -> t -> t list
