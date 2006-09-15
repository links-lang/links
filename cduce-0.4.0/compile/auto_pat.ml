(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* The automata for pattern matching *)
open Ident

type source =
  | Catch | Const of Types.const 
  | Stack of int | Left | Right | Nil | Recompose of int * int

type result = int * source array * int
    (* Return code, result values, number of values to pop *)

type actions =
  | AIgnore of result
  | AKind of actions_kind
and actions_kind = {
  basic: (Types.t * result) list;
  atoms: result Atoms.map;
  chars: result Chars.map;
  prod: result dispatch dispatch;
  xml: result dispatch dispatch;
  record: record option;
}
and record =
  | RecLabel of label * result dispatch dispatch
  | RecNolabel of result option * result option
and 'a dispatch =
  | Dispatch of state * 'a array
  | TailCall of state
  | Ignore of 'a
  | Impossible

and state = {
  uid : int;
  arity : int array;
  mutable actions: actions;
  mutable fail_code: int;
  mutable expected_type: string;
}


type 'a rhs = Match of int * 'a | Fail
