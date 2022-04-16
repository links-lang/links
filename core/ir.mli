(** Monadic IR *)
open CommonTypes

type scope = Var.Scope.t
  [@@deriving show]

(* term variables *)
type var = Var.var
  [@@deriving show,eq,yojson]
type var_info = Var.var_info
  [@@deriving show]
type binder = Var.binder
  [@@deriving show]

(* type variables *)
type tyvar = Quantifier.t
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

type name_set = Utility.stringset
  [@@deriving show]
type 'a name_map = 'a Utility.stringmap
  [@@deriving show]

type 'a var_map = 'a Utility.intmap
  [@@deriving show]

type language = string
  [@@deriving show]

type location = CommonTypes.Location.t
  [@@deriving show]

(* INVARIANT: all IR binders have unique names *)

type value =
  | Constant   of Constant.t                      (* constant: c *)
  | Variable   of var                             (* variable use: x *)
  | Extend     of value name_map * value option   (* record extension: (l1=v1, ..., lk=vk|r) or (l1=v1, ..., lk=vk) *)
  | Project    of Name.t * value                    (* record projection: r.l *)
  | Erase      of name_set * value                (* erase fields from a record: r\{ls} *)
  | Inject     of Name.t * value * Types.t   (* variant injection: L(v) *)

  | TAbs       of tyvar list * value       (* type abstraction: /\xs.v *)
  | TApp       of value * tyarg list       (* type application: v ts *)

  | XmlNode    of Name.t * value name_map * value list
                                       (* XML node construction: <tag attributes>body</tag> *)
  | ApplyPure  of value * value list   (* non-side-effecting application: v ws *)

  | Closure    of var * tyarg list * value           (* closure creation: f env *)

  | Coerce     of value * Types.t             (* type coercion: v:A *)

and tail_computation =
  | Return     of value
  | Apply      of value * value list
  | Special    of special
  | Case       of value * (binder * computation) name_map * (binder * computation) option
  | If         of value * computation * computation
and fun_def =
  {
    fn_binder   : binder;
    fn_tyvars   : tyvar list;
    fn_params   : binder list;
    fn_body     : computation;
    fn_closure  : binder option;
    fn_location : location;
    fn_unsafe   : bool
  }
and temporal_update =
  | ValidTimeUpdate of valid_time_update
  | TransactionTimeUpdate
and valid_time_update =
  | CurrentUpdate
  | SequencedUpdate of { validity_from: value; validity_to: value }
  | NonsequencedUpdate of { from_time: computation option; to_time: computation option }
and temporal_deletion =
  | ValidTimeDeletion of valid_time_deletion
  | TransactionTimeDeletion
and valid_time_deletion =
  | CurrentDeletion
  | SequencedDeletion of { validity_from: value; validity_to: value }
  | NonsequencedDeletion
and valid_time_insertion =
  | CurrentInsertion
  | SequencedInsertion
and temporal_insertion =
  | ValidTimeInsertion of valid_time_insertion
  | TransactionTimeInsertion
and binding =
  | Let        of binder * (tyvar list * tail_computation)
  | Fun        of fun_def
  | Rec        of fun_def list
  | Alien      of { binder: binder;
                    language: ForeignLanguage.t;
                    object_name: string }
  | Module     of string * binding list option
and special =
  | Wrong      of Types.t
  | Database   of value
  | Lens of value * Lens.Type.t
  | LensSerial of { lens: value; columns : Lens.Alias.Set.t; typ : Lens.Type.t }
  | LensDrop   of { lens : value; drop : string; key : string; default : value; typ : Lens.Type.t }
  | LensSelect of { lens : value; predicate : lens_predicate; typ : Lens.Type.t }
  | LensJoin   of { left : value; right : value; on : string list; del_left : Lens.Phrase.t; del_right : Lens.Phrase.t; typ : Lens.Type.t }
  | LensCheck  of value * Lens.Type.t
  | LensGet    of value * Types.t
  | LensPut    of value * value * Types.t
  | Table      of table
  | Query      of (value * value) option * QueryPolicy.t * computation * Types.t
  | TemporalJoin of Temporality.t * computation * Types.datatype
  | InsertRows of temporal_insertion option * value * value
  | InsertReturning of temporal_insertion option * value * value * value
  | Update     of temporal_update option * (binder * value) * computation option * computation
  | Delete     of temporal_deletion option * (binder * value) * computation option
  | CallCC     of value
  | Select     of Name.t * value
  | Choice     of value * (binder * computation) name_map
  | Handle     of handler
  | DoOperation of Name.t * value list * Types.t
and computation = binding list * tail_computation
and effect_case = binder * binder * computation
and handler = {
    ih_comp: computation;
    ih_cases: effect_case name_map;
    ih_return: binder * computation;
    ih_depth: handler_depth;
}
and handler_depth = | Deep of (binder * value) list | Shallow
and lens_predicate = Static of Lens.Phrase.t | Dynamic of value
and table = {
  database: value;
  table: value;
  keys: value;
  temporal_fields: (string * string) option;
  table_type: (Temporality.t * Types.datatype * Types.datatype * Types.datatype)
}
  [@@deriving show]

val binding_scope : binding -> scope
val binder_of_fun_def : fun_def -> binder

val tapp : value * tyarg list -> value

val letm : ?tyvars:tyvar list -> binder * tail_computation -> binding
val letmv : binder * value -> binding
(*val letv : tybinder * value -> binding*)

val unit : value
val unit_comp : computation

type program = computation
  [@@deriving show]

val is_atom : value -> bool

val with_bindings : binding list -> computation -> computation

val string_of_var : var -> string

val string_of_value : value -> string
val string_of_tail_computation : tail_computation -> string
val string_of_binding : binding -> string
val string_of_special : special -> string
val string_of_computation : computation -> string
val string_of_program : program -> string

type eval_fun_def = var_info * (var list * computation) * Var.var option * location
  [@@deriving show]
