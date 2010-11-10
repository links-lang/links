
type implementation_type = [`Atom | `List] 

type column_type = [ Algebra.pf_type | `Surrogate | `Unit | `EmptyRecord | `Tag | `EmptyListLit ]

val column_type_of_constant : Constant.constant -> column_type

val column_type_of_pf_type : Algebra.pf_type -> column_type 

(** the number of a column starting with 1 *)
type offset = int

type column = offset * column_type 

type field_name = string

(** the type of the cs component which describes how the represented 
    values (primitive values, records, lists, tags) are mapped onto 
    the flat columns *)
type t = 
  | Column of column
  | Tag of column * column
  | Mapping of (field_name * t) list

val show : t -> string

(** return all leafs of the cs structure, i.e. all Column/Tag nodes *)
val leafs : t -> t list

(** return all offsets used in the cs structure *)
val offsets : t -> offset list

val is_atomic : t -> bool
val is_variant : t -> bool
val is_record : t -> bool
val is_boxed_list : t -> bool
val is_empty_list_lit : t -> bool

(** number of columns in cs *)
val cardinality : t -> int

(** increase all offsets in cs by i *)
val shift : int -> t -> t

(** append two record cs structures *)
val append_mappings : t -> t -> t

(** choose the non-empty one from two cs structures based on the length *)
val choose_nonempty : t -> t -> t

(** lookup the cs node belonging to a fieldname in a Mapping *)
val lookup_record_field : t -> string -> t

(** remove all fields which are in a set from a mapping *)
val filter_record_fields : t -> Utility.StringSet.t -> t

(** recursively sort the fields of all mappings by field names *)
val sort_record_columns : t -> t

(** replace all offsets in a cs structure by new offsets in a list (in-order) *)
val map_cols : offset list -> t -> t
