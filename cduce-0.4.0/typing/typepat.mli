(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

type err = string -> exn

type node
val mk_delayed: unit -> node
val link: node -> node -> unit
val mk_type : Types.descr -> node
val mk_or : ?err:err -> node -> node -> node
val mk_and: ?err:err -> node -> node -> node
val mk_diff: ?err:err -> node -> node -> node
val mk_prod: node -> node -> node
val mk_xml: node -> node -> node
val mk_arrow: node -> node -> node
val mk_optional: ?err:err -> node -> node
val mk_record: ?err:err -> bool -> (node * node option) label_map -> node
val mk_constant: id -> Types.const -> node
val mk_capture: id -> node
val mk_concat: ?err:err -> node -> node -> node
val mk_merge: ?err:err -> node -> node -> node

val check_wf: node -> bool

val elim_concats: unit -> unit
val internalize: node -> unit

val peek_fv: node -> id option

val typ : node -> Types.descr
val typ_node : node -> Types.Node.t
val pat_node : node -> Patterns.node

val get_ct: node -> Types.t * node

type re
val mk_empty: re
val mk_epsilon: re
val mk_elem: node -> re
val mk_guard: node -> re
val mk_seq: re -> re -> re
val mk_alt: re -> re -> re
val mk_star: re -> re
val mk_weakstar: re -> re
val mk_seqcapt: id -> re -> re
val rexp: re -> node
val rexp_simplify: mix:bool -> re -> node

