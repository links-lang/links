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
type tyvar = Types.quantifier
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

type name = string
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
  | Project    of name * value                    (* record projection: r.l *)
  | Erase      of name_set * value                (* erase fields from a record: r\{ls} *)
  | Inject     of name * value * Types.datatype   (* variant injection: L(v) *)

  | TAbs       of tyvar list * value       (* type abstraction: /\xs.v *)
  | TApp       of value * tyarg list       (* type application: v ts *)

  | XmlNode    of name * value name_map * value list
                                       (* XML node construction: <tag attributes>body</tag> *)
  | ApplyPure  of value * value list   (* non-side-effecting application: v ws *)

  | Closure    of var * tyarg list * value           (* closure creation: f env *)

  | Coerce     of value * Types.datatype             (* type coercion: v:A *)

and tail_computation =
  | Return     of value
  | Apply      of value * value list
  | Special    of special
  | Case       of value * (binder * computation) name_map * (binder * computation) option
  | If         of value * computation * computation
and fun_def = binder * (tyvar list * binder list * computation) * binder option * location
and binding =
  | Let        of binder * (tyvar list * tail_computation)
  | Fun        of fun_def
  | Rec        of fun_def list
  | Alien      of binder * name * language
  | Module     of string * binding list option
and special =
  | Wrong      of Types.datatype
  | Database   of value
  | Lens of value * Lens.Type.t
  | LensDrop   of { lens : value; drop : string; key : string; default : value; typ : Lens.Type.t }
  | LensSelect of { lens : value; predicate : lens_predicate; typ : Lens.Type.t }
  | LensJoin   of { left : value; right : value; on : string list; del_left : Lens.Phrase.t; del_right : Lens.Phrase.t; typ : Lens.Type.t }
  | LensCheck  of value * Lens.Type.t
  | LensGet    of value * Types.datatype
  | LensPut    of value * value * Types.datatype
  | Table      of value * value * value * (Types.datatype * Types.datatype * Types.datatype)
  | Query      of (value * value) option * computation * Types.datatype
  | InsertRows of value * value
  | InsertReturning of value * value * value
  | Update     of (binder * value) * computation option * computation
  | Delete     of (binder * value) * computation option
  | CallCC     of value
  | Select     of name * value
  | Choice     of value * (binder * computation) name_map
  | Handle     of handler
  | DoOperation of name * value list * Types.datatype
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
  [@@deriving show]

val binding_scope : binding -> scope
val binder_of_fun_def : fun_def -> binder

val tapp : value * tyarg list -> value

val letm : ?tyvars:tyvar list -> binder * tail_computation -> binding
val letmv : binder * value -> binding
(*val letv : tybinder * value -> binding*)

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
