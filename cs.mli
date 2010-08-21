
type implementation_type = [`Atom | `List] 

type column_type = [ Algebra.pf_type | `Surrogate | `Unit | `Tag | `EmptyListLit ]

val column_type_of_constant : Constant.constant -> column_type

(** the number of a column starting with 1 *)
type offset = int

type column = offset * column_type 

type field_name = string

(** the type of the cs component which describes how the represented 
    values (primitive values, records, lists, tags) are mapped onto 
    the flat columns *)
type cs = 
  | Column of column
  | Tag of column * column
  | Mapping of (field_name * cs) list

val show : cs -> string

(** return all leafs of the cs structure, i.e. all Column/Tag nodes *)
val leafs : cs -> cs list

(** return all offsets used in the cs structure *)
val offsets : cs -> offset list

val is_atomic : cs -> bool
val is_variant : cs -> bool
val is_record : cs -> bool
val is_boxed_list : cs -> bool
val is_empty_list_lit : cs -> bool

(** number of columns in cs *)
val cardinality : cs -> int

(** increase all offsets in cs by i *)
val shift : int -> cs -> cs

(** append two record cs structures *)
val append_mappings : cs -> cs -> cs

(** choose the non-empty one from two cs structures based on the length *)
(* FIXME: this will lead to trouble if len(cs1) = len(cs2) and type(col1) != type(col2) *)
val choose_nonempty : cs -> cs -> cs

(** lookup the cs node belonging to a fieldname in a Mapping *)
val lookup_record_field : cs -> string -> cs

(** remove all fields which are in a set from a mapping *)
val filter_record_fields : cs -> Utility.StringSet.t -> cs

(** recursively sort the fields of all mappings by field names *)
val sort_record_columns : cs -> cs

(** replace all offsets in a cs structure by new offsets in a list (in-order) *)
val map_cols : offset list -> cs -> cs
