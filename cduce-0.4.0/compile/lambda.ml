(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Representation of programs used by the runtime evaluator.
   Similar to the typed abstract syntax tree representation, but:
   - the pattern matching is compiled;
   - the identifiers locations are resolved. *)

open Ident

type var_loc =
  | Local of int
      (* Slot in the table of locals *)
  | Env of int
      (* Slot in the environment *)
  | Ext of Compunit.t * int 
      (* Global slot from a given compilation unit *)
      (* If pos < 0, the first arg is the value *)
  | External of Compunit.t * int 
      (* OCaml External *)
      (* If pos < 0, the first arg is the value *)
  | Builtin of string
      (* OCaml external embedded in the runtime *)
  | Global of int 
      (* Only for the toplevel *)
  | Dummy

type expr = 
  | Var of var_loc
  | Apply of expr * expr
  | Abstraction of var_loc array * (Types.t * Types.t) list * branches * int
      (* environment, interface, branches, size of locals *)
  | Check of expr * Auto_pat.state
  | Const of Value.t
  | Pair of expr * expr
  | Xml of expr * expr * expr
  | XmlNs of expr * expr * expr * Ns.table
  | Record of expr Imap.t
  | String of U.uindex * U.uindex * U.t * expr
  | Match of expr * branches
  | Map of expr * branches
  | Transform of expr * branches
  | Xtrans of expr * branches
  | Try of expr * branches
  | Validate of expr * Schema_validator.t
  | RemoveField of expr * label
  | Dot of expr * label
  | Ref of expr * Types.Node.t
  | Op of string * expr list  
  | OpResolved of (Value.t list -> Value.t) * expr list
  | NsTable of Ns.table * expr

and branches = {
  brs_accept_chars: bool;
  brs_disp: Auto_pat.state;
  brs_rhs: expr Auto_pat.rhs array;
  brs_stack_pos: int
}

type code_item =
  | Eval of expr * int
      (* expression, size of locals *)
  | LetDecls of expr * int * Auto_pat.state * int
      (* expression, size of locals, dispatcher, number of globals to set *)
  | LetDecl of expr * int


type code = code_item list
