open Links_core

module CT = CommonTypes

(** This is Schinks, aka Scheme Links. It's a DSL for creating Links IR
  programs.
  The patron animal of Schinks is the skink.

  The types for Links types and IR program fragments are wrapped in a type t,
  providing error handling and the translation to actual IR programs under the
  hood.

  The constructors below directly map to the corresponding Links IR fragments.

  However, binders and variables need some explanation:
  Schinks provide a facility to give string names to type and term variables,
  even though the actual Links IR uses integer identifiers.

  For example, we may use (var "x") and (var "y") in an Schinks program.
  During the translation, we bijectively map all such string names to integers.
  The same holds for type variables:
  (q "T") creates a quantifier in Schinks, (tvar "T") let's you use it as a type
  variable in a type.
  Of course, type and term variables live in different name spaces for the sake
  of the translatiion to integers.

  If you wish to have manual control over the integers assigned (e.g., to fix
  what a variable will be called in an error message), you can use the wi_
  versions of the functions used above (standing for "with integer").
  For example, (wi_var 1) directly translate to the Links IR Variable with
  integerid 1. Note that any of the type or term variables you introdude using a
  wi_ function is guaranteed not to be used when assigning integer ids to
  string-based names.

  We allow _ as a binder name, which will be replaced by a fresh variable used
  nowhere else.

*)

(** Representation of IR fragments and Links types *)
type 'a t

val reify : 'a t -> 'a

(*
 *
 * LINKS TYPES
 *
 *)

val int : Types.t t
val string : Types.t t

val forall : CommonTypes.Quantifier.t t list -> Types.t t -> Types.t t

(** Creates a type variable to be used in a type, primary kind defauls to Type *)
val tvar : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> string -> Types.typ t

val wi_tvar : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> int -> Types.typ t

(** shorthand for tvar ~pk set to Type *)
val tvar_row : ?sk:CT.Subkind.t -> string -> Types.typ t

(** Creates a type variable to be used in a type, primary kind defauls to Type *)
val targ : ?pk:CT.PrimaryKind.t -> Types.t t -> Types.type_arg t

val unit_t : Types.typ t

(* Function types *)

(** General function type including effects.
    Note that the domain is represented by a list of parmeters types.
    This is then translated to the corresponding tuple type. *)
val fun_t :
  ?effects:Types.row t -> Types.typ t list -> Types.typ t -> Types.typ t

(** Syntactic sugar for fun_t.
  a .-->{e} b   becomes   Function(a,e,b).
  This operator is left-associative, use parantheses when nesting! *)
val ( .-->{} ) : Types.typ t list -> Types.row t -> Types.typ t -> Types.typ t

(** Equivalent to {}-> in Links' syntactic sugar for function types
Shorthand for closed function types (using tuple for parameters) *)
val fun_ct : Types.typ t list -> Types.typ t -> Types.typ t

(** alias for fun_ct *)
val ( |--> ) : Types.typ t list -> Types.typ t -> Types.typ t

(** Equivalent to {}~> in Links' syntactic sugar for function types *)
val wild_fun_ct : Types.typ t list -> Types.typ t -> Types.typ t

(** alias for wild_fun_ct *)
val ( |~~> ) : Types.typ t list -> Types.typ t -> Types.typ t

(* Rows *)

val closed : Types.row_var t

val row_var : string -> Types.row_var t

val row : (string * Types.field_spec t) list -> Types.row_var t -> Types.row t

(* Presence info *)

val present : Types.typ t -> Types.field_spec t

val absent : Types.field_spec t

val presence_var : string -> Types.field_spec t

(* Quantifiers *)

(** Creates quantifier, to be used in let/fun/forall  *)
val q : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> string -> CT.Quantifier.t t

(** Shorthand for q with ~pk set to Type *)
val q_row : ?sk:CT.Subkind.t -> string -> CT.Quantifier.t t

(** Version of q allowing you to specify the integer id of the quantifier *)
val wi_q : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> int -> CT.Quantifier.t t

val record_t :
  ?row_var:Types.row_var t -> (string * Types.field_spec t) list -> Types.typ t

val variant :
  ?row_var:Types.row_var t -> (string * Types.t t) list -> Types.typ t

(** Lifts a type into t, marking all quantifiers and type variable ids therein
  as reserved. *)
val lift_type : Types.t -> Types.t t

(*
 *
 * IR BINDERS
 *
 *)

val binder : ?scope:Var.Scope.t -> string -> Types.typ t -> Ir.binder t
val wi_binder : ?scope:Var.Scope.t -> int -> Types.typ t -> Ir.binder t

(*
 *
 *  IR VALUES
 *
 *)

val int_const : int -> Ir.value t

(** shorthand for int_const *)
val i : int -> Ir.value t

val string_const : string -> Ir.value t

(** shorthand for string_const *)
val s : string -> Ir.value t

val var : string -> Ir.value t

val wi_var : int -> Ir.value t

val closure : string -> Ir.tyarg t list -> Ir.value t -> Ir.value t

val record : (string * Ir.value t) list -> Ir.value t

val extend_record : Ir.value t -> (string * Ir.value t) list -> Ir.value t

val unit : Ir.value t

val tapp : Ir.value t -> (CT.PrimaryKind.t * Types.t t) list -> Ir.value t

val tabs : CT.Quantifier.t t list -> Ir.value t -> Ir.value t

val inject : string -> Ir.value t -> Types.typ t -> Ir.value t

(*
 *
 * IR TAIL COMPUTATIONS
 *
 *)

val return : Ir.value t -> Ir.tail_computation t

val if_ :
  Ir.value t -> Ir.computation t -> Ir.computation t -> Ir.tail_computation t

val apply : Ir.value t -> Ir.value t list -> Ir.tail_computation t

val case :
  Ir.value t ->
  ?default:Ir.binder t * Ir.computation t ->
  (string * Ir.binder t * Ir.computation t) list ->
  Ir.tail_computation t

(*
 *
 * IR COMPUTATIONS
 *
 *)

(** Lifts a tail computation to a computation by prepending an empty list of
  bindings to it *)
val tc_to_comp : Ir.tail_computation t -> Ir.computation t

(** Lifts a binding to a computation by using a tail computation that
  returns unit *)
val binding_to_comp : Ir.binding t -> Ir.computation t

val bindings_to_comp : Ir.binding t list -> Ir.computation t

val computation : Ir.binding t list -> Ir.tail_computation t -> Ir.computation t

(*
 *
 * IR BINDINGS
 *
 *)

(** Creates a group of recursive functions *)
val rec_ : Ir.fun_def t list -> Ir.binding t

val let_ :
  string ->
  Types.typ t ->
  ?tparams:CT.Quantifier.t t list ->
  ?scope:Var.Scope.t ->
  Ir.tail_computation t ->
  Ir.binding t

val wi_let_ :
  Ir.binder t ->
  ?tparams:CT.Quantifier.t t list ->
  Ir.tail_computation t ->
  Ir.binding t

(** Creates a function definition, to be used in recursive blocks.
  Use fun_ for non-recursive functions *)
val def :
  string ->
  Types.typ t ->
  ?tparams:CT.Quantifier.t t list ->
  (string * Types.typ t) list ->
  ?scope:Var.Scope.t ->
  ?closure_var:string * Types.typ t ->
  ?location:CT.Location.t ->
  ?unsafe_sig:bool ->
  Ir.computation t ->
  Ir.fun_def t

(** Version of def allowing you to use binders directly *)
val wi_def :
  Ir.binder t ->
  ?tparams:CT.Quantifier.t t list ->
  Ir.binder t list ->
  ?closure_var:Ir.binder t ->
  ?location:CT.Location.t ->
  ?unsafe_sig:bool ->
  Ir.computation t ->
  Ir.fun_def t

val fun_ :
  string ->
  Types.typ t ->
  ?tparams:CT.Quantifier.t t list ->
  (string * Types.typ t) list ->
  ?closure_var:string * Types.typ t ->
  ?location:CT.Location.t ->
  ?unsafe_sig:bool ->
  Ir.computation t ->
  Ir.binding t

val wi_fun_ :
  Ir.binder t ->
  ?tparams:CT.Quantifier.t t list ->
  Ir.binder t list ->
  ?closure_var:Ir.binder t ->
  ?location:CT.Location.t ->
  ?unsafe_sig:bool ->
  Ir.computation t ->
  Ir.binding t
