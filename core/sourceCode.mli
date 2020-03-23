open Utility

class source_code :
  object
    val lines : (int, int) Hashtbl.t
    val text : Buffer.t
    method private extract_line : int -> string
    method extract_line_range : int -> int -> string
    method private extract_substring :
      Lexing.position -> Lexing.position -> string
    method find_line : Lexing.position -> string * int
    method lookup :
      Lexing.position * Lexing.position ->
      Lexing.position * string * string
    method parse_into : (bytes -> int -> int) -> bytes -> int -> int
  end

module Lexpos : sig
  type t = Lexing.position
  [@@deriving show]
end

(** A module for keeping track of source code positions. *)
module Position : sig
  (** An unresolved source code position. Also see [Position.Resolved]. *)
  type t [@@deriving show]

  val make : start:Lexpos.t -> finish:Lexpos.t -> code:source_code option -> t

  (** A dummy position used for when the source code location is unknown. *)
  val dummy : t

  (** Get start position of the source code position. *)
  val start : t -> Lexpos.t

  (** Get the end position of the source code position. *)
  val finish : t -> Lexpos.t

  (** Get the source code object. *)
  val code : t -> source_code option

  val traverse_map :
    t ->
    o:'o ->
    f_start:('o -> Lexpos.t ->  'a * Lexpos.t) ->
    f_finish:('a -> Lexpos.t -> 'b * Lexpos.t ) ->
    f_code:('b -> source_code option -> 'c * source_code option) ->
    'c * t

  val traverse :
    t ->
    o:'o ->
    f_start:('o -> Lexpos.t -> 'a) ->
    f_finish:('a -> Lexpos.t -> 'b) ->
    f_code:('b -> source_code option -> 'c) ->
    'c

  val map_code :
    t -> f:(source_code option -> source_code option) -> t

  exception ASTSyntaxError of t * string

  (** Resolve the source code position to the actual contents of that position. *)
  module Resolved : sig
    type unresolved = t
    type t [@@deriving show]

    val dummy : t

    val start : t -> Lexpos.t

    val source_line : t -> string

    val source_expression : t -> string

    (** Lookup the relevant source code line. *)
    val resolve : unresolved -> t
  end

  (** Resolve the source code position and return the source expression. *)
  val resolve_expression : t -> string

  (** Resolve the source code position and return the start position and the
      source expression. *)
  val resolve_start_expr : t -> Lexpos.t * string
end

module WithPos : sig
  type 'a t = private { node : 'a
              ; pos  : Position.t
              } [@@deriving show]

  (** Construct a new with_pos given a node and an optional source code
      position. Use the [dummy] position if none is specified. *)
  val make : ?pos:Position.t -> 'a -> 'a t

  (** Construct a new with_pos with a dummy source position *)
  val dummy : 'a -> 'a t

  (** Fetch the corresponding node. *)
  val node : 'a t -> 'a

  (** Fetch the corresponding source code position. *)
  val pos : 'a t -> Position.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> f_pos:(Position.t -> Position.t) -> f_node:('a -> 'b)
          -> 'b t

  val with_node : 'a t -> 'b -> 'b t

  (** Discard positions from elements in a list **)
  val nodes_of_list : 'a t list -> 'a list

  val traverse : 'a t -> o:'o -> f_pos:('o -> Position.t -> 'b)
              -> f_node:('b -> 'a -> 'c) -> 'c

  val traverse_map :
    'a t ->
    o:'o ->
    f_pos:('o -> Position.t -> 'b * Position.t) ->
    f_node:('b -> 'a -> 'c * 'd) ->
    'c * 'd t
end
