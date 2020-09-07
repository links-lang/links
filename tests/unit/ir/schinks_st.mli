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

*)

(** Representation of IR fragments and Links types *)
type 'a t

val reify : 'a t -> ('a, string) Result.t

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

(** shorthand for tvar ~pk set to Type *)
val tvar_row : ?sk:CT.Subkind.t -> string -> Types.typ t

val unit : Types.typ t

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

(* Quantifiers *)

(** Creates quantifier, to be used in let/fun/forall  *)
val q : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> string -> CT.Quantifier.t t

(** Shorthand for q with ~pk set to Type *)
val q_row : ?sk:CT.Subkind.t -> string -> CT.Quantifier.t t

(** Version of q allowing you to specify the integer id of the quantifier *)
val wi_q : ?pk:CT.PrimaryKind.t -> ?sk:CT.Subkind.t -> int -> CT.Quantifier.t t

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

(*
 *
 * IR TAIL COMPUTATIONS
 *
 *)

val return : Ir.value t -> Ir.tail_computation t

val if_ :
  Ir.value t -> Ir.computation t -> Ir.computation t -> Ir.tail_computation t

val apply : Ir.value t -> Ir.value t list -> Ir.tail_computation t

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

type 'a comp_param


val ( let++ ) :
  'a t comp_param -> ('a t -> Ir.tail_computation t) -> Ir.computation t

val ( and++ ) : 'a comp_param -> 'b comp_param -> ('a * 'b) comp_param
