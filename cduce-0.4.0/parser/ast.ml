(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(*  Abstract syntax as produced by the parser *)

open Location
open Ident

type ns_expr = [ `Uri of Ns.Uri.t | `Path of U.t list ]

type pprog = pmodule_item list

and pmodule_item = pmodule_item' located
and pmodule_item' =
  | TypeDecl of (Location.loc * U.t) * ppat
  | SchemaDecl of U.t * string
  | LetDecl of ppat * pexpr
  | FunDecl of pexpr
  | Namespace of U.t * ns_expr
  | KeepNs of bool
  | Using of U.t * U.t
  | Open of U.t list
  | EvalStatement of pexpr
  | Directive of toplevel_directive
and debug_directive =
  [ `Filter of ppat * ppat
  | `Sample of ppat
  | `Accept of ppat
  | `Compile of ppat * ppat list 
  | `Subtype of ppat * ppat
  | `Single of ppat
  ]
and toplevel_directive =
  [ `Quit
  | `Env
  | `Reinit_ns
  | `Help
  | `Dump of pexpr
  | `Print_type of ppat
  | `Debug of debug_directive
  | `Verbose
  | `Silent
  | `Builtins
  ]


and pexpr = 
  | LocatedExpr of loc * pexpr

  (* CDuce is a Lambda-calculus ... *)
  | Var of U.t
  | Apply of pexpr * pexpr
  | Abstraction of abstr
      
  (* Data constructors *)
  | Const of Types.Const.t
  | Integer of Intervals.V.t
  | Char of Chars.V.t
  | Pair of pexpr * pexpr
  | Atom of U.t
  | Xml of pexpr * pexpr
  | RecordLitt of (label * pexpr) list
  | String of U.uindex * U.uindex * U.t * pexpr
      
  (* Data destructors *)
  | Match of pexpr * branches
  | Map of pexpr * branches
  | Transform of pexpr * branches
  | Xtrans of pexpr * branches
  | Validate of pexpr * U.t list
  | Dot of pexpr * label
  | TyArgs of pexpr * ppat list
  | RemoveField of pexpr * label

  (* Exceptions *)
  | Try of pexpr * branches

  (* Other *)
  | NamespaceIn of U.t * ns_expr * pexpr
  | KeepNsIn of bool * pexpr
  | Forget of pexpr * ppat    
  | Check of pexpr * ppat
  | Ref of pexpr * ppat


  (* CQL *)
  | SelectFW of pexpr * (ppat * pexpr) list * pexpr list

and label = U.t

and abstr = { 
  fun_name : (Location.loc * U.t) option; 
  fun_iface : (ppat * ppat) list;
  fun_body : branches
}

and branches = (ppat * pexpr) list
    
(* A common syntactic class for patterns and types *) 

and ppat = ppat' located
and ppat' =
  | PatVar of U.t list
  | Cst of pexpr
  | NsT of U.t
  | Recurs of ppat * (Location.loc * U.t * ppat) list
  | Internal of Types.descr
  | Or of ppat * ppat
  | And of ppat * ppat
  | Diff of ppat * ppat
  | Prod of ppat * ppat
  | XmlT of ppat * ppat
  | Arrow of ppat * ppat
  | Optional of ppat
  | Record of bool * (label * (ppat * ppat option)) list
  | Constant of U.t * pexpr
  | Regexp of regexp
  | Concat of ppat * ppat
  | Merge of ppat * ppat

and regexp =
  | Epsilon
  | Elem of ppat
  | Guard of ppat
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
  | WeakStar of regexp
  | SeqCapture of Location.loc * U.t * regexp


let pat_true = mknoloc (Internal Builtin_defs.true_type)
let pat_false = mknoloc (Internal Builtin_defs.false_type) 
let cst_true = Const (Types.Atom Builtin_defs.true_atom)
let cst_false = Const (Types.Atom Builtin_defs.false_atom)

let cst_nil =  Const Sequence.nil_cst
let pat_nil = mknoloc (Internal (Sequence.nil_type))
