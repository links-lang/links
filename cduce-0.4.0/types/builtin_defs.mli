(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(** Some CDuce predefined types *)

val pos_int : Types.t         (** 1 .. Inf *)
val non_neg_int : Types.t     (** 0 .. Inf *)
val neg_int : Types.t         (** -Inf .. -1 *)
val non_pos_int : Types.t     (** -Inf .. 0 *)
val long_int : Types.t        (** -9223372036854775808 .. 9223372036854775807 *)
val int_int : Types.t         (** -2147483648 .. 2147483647 *)
val short_int : Types.t       (** -32768 .. 32767 *)
val byte_int : Types.t        (** -128 .. 127 *)
val caml_int : Types.t        (** min_int .. max_int *)
val byte_int : Types.t        (** 0 .. 255 *)
val non_zero_int : Types.t

val intstr : Types.t
val pos_intstr : Types.t
val neg_intstr : Types.t

val true_atom : Atoms.V.t
val false_atom : Atoms.V.t

val true_type : Types.t
val false_type : Types.t

val any : Types.t
val atom : Types.t
val nil : Types.t

val bool : Types.t
val int : Types.t
val char: Types.t
val string : Types.t

val char_latin1 : Types.t
val string_latin1 : Types.t

val time_kind: Types.t

val mk_ref: get:'a -> set:'a -> 'a Ident.label_map
val ref_type: Types.Node.t -> Types.t

val float: Types.t
val float_abs: Types.Abstract.abs

val any_xml : Types.t

val any_xml_with_tag: Atoms.t -> Types.t

