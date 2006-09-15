(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Typed abstract syntax *) 

(*  Some sub-expression may have to be type-checked several times.
    We first build the ``skeleton'' of the typed ast
    (basically the parsed ast with types and patterns replaced with their
    internal representation), then type check it.

    The exp_typ and br_typ fields are updated to capture all the possible
    values than can result from the expression or flow to the branch
*)

open Location
open Ident

type tpat = Patterns.node
type ttyp = Types.Node.t

type texpr  = 
    { exp_loc : loc; 
      mutable exp_typ : Types.t;  
      (* Currently exp_typ is not used. It will be used for compilation ! *)
      exp_descr : texpr';
    }
and  texpr' = 
  | Forget of texpr * ttyp
  | Check of (Types.t ref) * texpr * ttyp
  (* CDuce is a Lambda-calculus ... *)
  | Var of id
  | ExtVar of Compunit.t * id * Types.t
  | Apply of texpr * texpr
  | Abstraction of abstr
      
  (* Data constructors *)
  | Cst of Types.const
  | Pair of texpr * texpr
  | Xml of texpr * texpr * Ns.table option
  | RecordLitt of texpr label_map
  | String of U.uindex * U.uindex * U.t * texpr
      
  (* Data destructors *)
  | Match of texpr * branches
  | Map of texpr * branches
  | Transform of texpr * branches
  | Xtrans of texpr * branches
  | Validate of texpr * Types.t * Schema_validator.t
  | RemoveField of texpr * label
  | Dot of texpr * label

  (* Exception *)
  | Try of texpr * branches

  | Ref of texpr * ttyp
  | External of Types.t * [ `Builtin of string | `Ext of int ]
  | Op of string * int * texpr list
  | NsTable of Ns.table * texpr'

and abstr = { 
  fun_name : id option; 
  fun_iface : (Types.t * Types.t) list;
  fun_body : branches;
  fun_typ  : Types.t;
  fun_fv   : fv
}

and let_decl = {
  let_pat : tpat;
  let_body : texpr;
}

and branches = { 
  mutable br_typ : Types.t; (* Type of values that can flow to branches *)
  br_accept : Types.t;  (* Type accepted by all branches *)
  br_branches: branch list;
}
and branch = { 
  br_loc : loc;
  mutable br_used : bool; 
  mutable br_vars_empty : fv;
  br_pat : tpat; 
  br_body :  texpr 
}

