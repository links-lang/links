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

type value =
  | Constant   of Constant.t
  | Variable   of var
  | Extend     of value name_map * value option
  | Project    of Name.t * value
  | Erase      of name_set * value
  | Inject     of Name.t * value * Types.t

  | TAbs       of tyvar list * value
  | TApp       of value * tyarg list

  | XmlNode    of Name.t * value name_map * value list
  | ApplyPure  of value * value list

  | Closure    of var * tyarg list * value

  | Coerce     of value * Types.t
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
  | Lens       of value * Lens.Type.t
  | LensSerial of { lens: value; columns : Lens.Alias.Set.t; typ : Lens.Type.t }
  | LensDrop   of { lens: value; drop : string; key : string; default : value; typ : Lens.Type.t }
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

let binding_scope : binding -> scope =
  function
  | Let (binder, _)
  | Fun {fn_binder = binder; _}
  | Rec ({fn_binder = binder; _}::_)
  | Alien {binder; _ } -> Var.scope_of_binder binder
  | Rec []
  | Module _ -> assert false

let binder_of_fun_def fb = fb.fn_binder

let tapp (v, tyargs) =
  match tyargs with
    | [] -> v
    | _ -> TApp (v, tyargs)

let letm ?(tyvars=[]) (b, tc) = Let (b, (tyvars, tc))
let letmv (b, v) = letm (b, Return v)

let rec is_atom =
  function
    | Constant (Constant.Bool  _)
    | Constant (Constant.Int   _)
    | Constant (Constant.Char  _)
    | Constant (Constant.Float _)
    | Variable _ -> true
(*
  This can only be an atom if
  Erase is just an upcast, and our language
  is properly parametric.
*)
(*    | Erase (_, v) *)
    | Coerce (v, _) -> is_atom v
    | _ -> false

let with_bindings bs' (bs, tc) = (bs' @ bs, tc)

let unit = Extend (Utility.StringMap.empty, None)
let unit_comp = ([], Return unit)

type program = computation
  [@@deriving show]

let string_of_var = string_of_int

let string_of_value = show_value
let string_of_tail_computation = show_tail_computation
let string_of_binding = show_binding
let string_of_special  = show_special
let string_of_computation = show_computation
let string_of_program = show_program




type eval_fun_def = var_info * (var list * computation) * Var.var option * location
  [@@deriving show]
