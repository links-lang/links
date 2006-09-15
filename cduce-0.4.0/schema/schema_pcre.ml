(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings.Utf8

let pcre_replace ~rex ?templ s =
  match templ with
    | None -> mk (Pcre.replace ~rex (get_str s))
    | Some templ ->
        mk (Pcre.replace ~rex ~templ:(get_str templ) (get_str s))
let pcre_extract ~rex s = Array.map mk (Pcre.extract ~rex (get_str s))
let pcre_regexp s = Pcre.regexp ~flags:[`UTF8] s
let pcre_split ~rex s = List.map mk (Pcre.split ~rex (get_str s))
