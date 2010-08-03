(*pp deriving *)
open Utility

(** The syntax tree created by the parser. *)

type name = string deriving (Show)

type num = Num.num

(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)
type unary_op = [
| `Minus
| `FloatMinus
| `Name of name
]
and regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ]
    deriving (Show)
type logical_binop = [`And | `Or ]
    deriving (Show)
type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name ]
deriving (Show)
type operator = [ unary_op | binop | `Project of name ]
deriving (Show)

let string_of_unary_op =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `Name name -> name

let string_of_binop =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `RegexMatch _ -> "<some regex nonsense>"
    | `And -> "&&"
    | `Or -> "||"
    | `Cons -> "::"    
    | `Name name -> name

type position = SourceCode.pos
let dummy_position = SourceCode.dummy_pos

let show_position = Show.show_unprintable

type binder = name * Types.datatype option * position
    deriving (Show)

(* type variables *)
type tyvar = Types.quantifier
  deriving (Show)
type tyarg = Types.type_arg
  deriving (Show)

(*
   NOTE: tyvar lists represent big-lambda binders.

   Currently they are only supported at HM generalisation points,
   i.e. in let-bindings.
*)

type location = [`Client | `Server | `Native | `Unknown]
    deriving (Show)

type subkind = [`Any | `Base]
    deriving (Show)

type type_variable =
    [ `TypeVar of name * subkind | `RigidTypeVar of name * subkind
    | `RowVar of name * subkind | `RigidRowVar of name * subkind
    | `PresenceVar of name | `RigidPresenceVar of name ]
      deriving (Show)

type quantifier =
    [ `TypeVar of name * subkind
    | `RowVar of name * subkind
    | `PresenceVar of name ]
      deriving (Show)

let type_variable_of_quantifier =
  function
    | `TypeVar v -> `RigidTypeVar v
    | `RowVar v -> `RigidRowVar v
    | `PresenceVar v -> `RigidPresenceVar v

type datatype =
  | TypeVar         of name * subkind
  | RigidTypeVar    of name * subkind
  | FunctionType    of datatype list * row * datatype
  | MuType          of name * datatype
  | ForallType      of quantifier list * datatype
  | UnitType
  | TupleType       of (datatype list)
  | RecordType      of row
  | VariantType     of row
  | TableType       of datatype * datatype * datatype
  | ListType        of datatype
  | TypeApplication of (string * type_arg list)
  | PrimitiveType   of Types.primitive
  | DBType
and row = (string * fieldspec) list * row_var
and row_var =
    [ `Closed
    | `Open of name * subkind
    | `OpenRigid of name * subkind
    | `Recursive of name * row ]
and presence_flag = [ `Present | `Absent | `RigidVar of name | `Var of name ]
and fieldspec = presence_flag * datatype
and type_arg =
    [ `Type of datatype
    | `Row of row
    | `Presence of presence_flag ]
      deriving (Show)

(* Store the denotation along with the notation once it's computed *)
type datatype' = datatype * Types.datatype option
    deriving (Show)

type fieldconstraint = [ `Readonly | `Default ]
    deriving (Show)

type constant = Constant.constant
    deriving (Show)

type patternnode = [
| `Any
| `Nil
| `Cons     of pattern * pattern
| `List     of pattern list
| `Variant  of name * pattern option
| `Negative of name list
| `Record   of (name * pattern) list * pattern option
| `Tuple    of pattern list
| `Constant of constant
| `Variable of binder
| `As       of binder * pattern
| `HasType  of pattern * datatype'
]
and pattern = patternnode * position
    deriving (Show)

type replace_rhs = [
| `Literal of string
| `Splice  of phrase
] 
and regex = [
| `Range     of char * char
| `Simply    of string
| `Quote     of regex
| `Any
| `StartAnchor
| `EndAnchor
| `Seq       of regex list
| `Alternate of regex * regex
| `Group     of regex
| `Repeat    of Regex.repeat * regex
| `Splice    of phrase
| `Replace   of regex * replace_rhs 
]
and funlit = pattern list list * phrase
and iterpatt = [ 
| `List of pattern * phrase
| `Table of pattern * phrase
]
and sec = [`Minus | `FloatMinus | `Project of name | `Name of name]
and phrasenode = [
| `Constant         of constant
| `Var              of name
| `FunLit           of ((Types.datatype * Types.row) list) option * funlit
| `Spawn            of phrase * Types.row option
| `SpawnWait        of phrase * Types.row option
| `Query            of (phrase * phrase) option * phrase * Types.datatype option
| `RangeLit         of (phrase * phrase)
| `ListLit          of phrase list * Types.datatype option
| `Iteration        of iterpatt list * phrase
    * (*where:*)   phrase option 
                    * (*orderby:*) phrase option
| `Escape           of binder * phrase
| `Section          of sec
| `Conditional      of phrase * phrase * phrase
| `Block            of binding list * phrase
| `InfixAppl        of (tyarg list * binop) * phrase * phrase
| `Regex            of regex
| `UnaryAppl        of (tyarg list * unary_op) * phrase
| `FnAppl           of phrase * phrase list
| `TAbstr           of tyvar list ref * phrase
| `TAppl            of phrase * tyarg list
| `TupleLit         of phrase list
| `RecordLit        of (name * phrase) list * phrase option
| `Projection       of phrase * name
| `With             of phrase * (name * phrase) list
| `TypeAnnotation   of phrase * datatype'
| `Upcast           of phrase * datatype' * datatype'
| `ConstructorLit   of name * phrase option * Types.datatype option
| `Switch           of phrase * (pattern * phrase) list * Types.datatype option
| `Receive          of (pattern * phrase) list * Types.datatype option
| `DatabaseLit      of phrase * (phrase option * phrase option)
| `TableLit         of phrase * (datatype * (Types.datatype * Types.datatype * Types.datatype) option) * (name * fieldconstraint list) list * phrase
| `DBDelete         of pattern * phrase * phrase option
| `DBInsert         of phrase * name list * phrase * phrase option
| `DBUpdate         of pattern * phrase * phrase option * (name * phrase) list
| `Xml              of name * (name * (phrase list)) list * phrase option * phrase list
| `TextNode         of string
| `Formlet          of phrase * phrase
| `Page             of phrase
| `FormletPlacement of phrase * phrase * phrase
| `PagePlacement    of phrase
| `FormBinding      of phrase * pattern
]
and phrase = phrasenode * position
and bindingnode = [
(*
   TODO: (aesthetic change)
     change `Val constructor to:
       `Val of pattern * (tyvar list * phrase) * location * datatype' option
     which corresponds to
       let p=/\X.e in ...
*)
| `Val     of tyvar list * pattern * phrase * location * datatype' option
| `Fun     of binder * (tyvar list * funlit) * location * datatype' option
| `Funs    of (binder * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list
| `Foreign of binder * name * datatype'
| `Include of string
| `Type    of name * (quantifier * tyvar option) list * datatype'
| `Infix
| `Exp     of phrase
]
and binding = bindingnode * position
and directive = string * string list
and sentence = [ 
| `Definitions of binding list
| `Expression  of phrase
| `Directive   of directive ]
    deriving (Show)

type program = binding list * phrase option 
  deriving (Show)


(* Why does ConcreteSyntaxError take an
   unresolved position and yet
   PatternDuplicateNameError and
   RedundantPatternMatch take resolved positions?
*)
exception ConcreteSyntaxError of (string * position)
exception PatternDuplicateNameError of (SourceCode.pos * string)
exception RedundantPatternMatch of SourceCode.pos

let tabstr : tyvar list * phrasenode -> phrasenode = fun (tyvars, e) ->
  `TAbstr (Types.box_quantifiers tyvars, (e, dummy_position))

let tappl : phrasenode * tyarg list -> phrasenode = fun (e, tys) ->
  match tys with
    | [] -> e
    | _ -> `TAppl ((e, dummy_position), tys)

module Freevars =
struct
  open Utility
  open StringSet

  let union_map f = union_all -<- List.map f
  let option_map f = opt_app f empty

  let rec pattern (p, _ : pattern) : StringSet.t = match p with
    | `Any
    | `Nil
    | `Constant _
    | `Negative _            -> empty
    | `Tuple ps
    | `List ps               -> union_map pattern ps
    | `Cons (p1, p2)         -> union (pattern p1) (pattern p2)
    | `Variant (_, popt)     -> option_map pattern popt
    | `Record (fields, popt) ->
        union (option_map pattern popt)
          (union_map (snd ->- pattern) fields)
    | `Variable (v,_,_) -> singleton v
    | `As ((v,_,_), pat)     -> add v (pattern pat)
    | `HasType (pat, _)      -> pattern pat


  let rec formlet_bound (p, _ : phrase) : StringSet.t = match p with
    | `Xml (_, _, _, children) -> union_map formlet_bound children
    | `FormBinding (_, pat) -> pattern pat
    | _ -> empty 

  let rec phrase (p, _ : phrase) : StringSet.t = match p with
    | `Var v -> singleton v
    | `Section (`Name n) -> singleton n

    | `Constant _
    | `TextNode _
    | `Section (`Minus|`FloatMinus|`Project _) -> empty

    | `Spawn (p, _)
    | `SpawnWait (p, _)
    | `TAbstr (_, p)
    | `TAppl (p, _)
    | `FormBinding (p, _)
    | `Projection (p, _)
    | `Page p
    | `PagePlacement p
    | `Upcast (p, _, _)
    | `TypeAnnotation (p, _) -> phrase p

    | `ListLit (ps, _)
    | `TupleLit ps -> union_map phrase ps

    | `Query (None, p, _) -> phrase p
    | `Query (Some (limit, offset), p, _) -> union_all [phrase limit; phrase offset; phrase p]

    | `Escape ((v,_,_), p) -> diff (phrase p) (singleton v)
    | `FormletPlacement (p1, p2, p3)
    | `Conditional (p1, p2, p3) -> union_map phrase [p1;p2;p3]
    | `Block b -> block b
    | `InfixAppl ((_, `Name n), p1, p2) -> union (singleton n) (union_map phrase [p1;p2])
    | `InfixAppl (_, p1, p2) -> union_map phrase [p1;p2]
    | `RangeLit (p1, p2) -> union_map phrase [p1;p2]
    | `Regex r -> regex r
    | `UnaryAppl (_, p) -> phrase p
    | `FnAppl (p, ps) -> union_map phrase (p::ps)
    | `RecordLit (fields, p) ->
        union (union_map (snd ->- phrase) fields)
          (option_map phrase p)
    | `With (p, fields) ->
        union (union_map (snd ->- phrase) fields)
          (phrase p)
    | `ConstructorLit (_, popt, _) -> option_map phrase popt
    | `DatabaseLit (p, (popt1, popt2)) ->
        union_all [phrase p; option_map phrase popt1; option_map phrase popt2]
    | `DBInsert (p1, _labels, p2, popt) ->
        union_all [phrase p1; phrase p2; option_map phrase popt]
    | `TableLit (p1, _, _, p2) -> union (phrase p1) (phrase p2) 
    | `Xml (_, attrs, attrexp, children) -> 
        union_all
          [union_map (snd ->- union_map phrase) attrs;
           option_map phrase attrexp;
           union_map phrase children]
    | `Formlet (xml, yields) ->
        let binds = formlet_bound xml in
          union (phrase xml) (diff (phrase yields) binds)
    | `FunLit (_, fnlit) -> funlit fnlit
    | `Iteration (generators, body, where, orderby) ->
        let xs = union_map (function
                              | `List (_, source)
                              | `Table (_, source) -> phrase source) generators in
        let pat_bound = union_map (function
                                     | `List (pat, _)
                                     | `Table (pat, _) -> pattern pat) generators in
          union_all [xs;
                     diff (phrase body) pat_bound;
                     diff (option_map phrase where) pat_bound;
                     diff (option_map phrase orderby) pat_bound]                     
            (*     | `Iteration (`List (pat, source), body, where, orderby) *)
(*     | `Iteration (`Table (pat, source), body, where, orderby) ->  *)
(*         let pat_bound = pattern pat in *)
(*           union_all [phrase source; *)
(*                      diff (phrase body) pat_bound; *)
(*                      diff (option_map phrase where) pat_bound; *)
(*                      diff (option_map phrase orderby) pat_bound] *)
    | `Switch (p, cases, _) -> union (phrase p) (union_map case cases)
    | `Receive (cases, _) -> union_map case cases 
    | `DBDelete (pat, p, where) -> 
        union (phrase p) 
          (diff (option_map phrase where)
             (pattern pat))
    | `DBUpdate (pat, from, where, fields) -> 
        let pat_bound = pattern pat in
          union_all [phrase from;
                     diff (option_map phrase where) pat_bound;
                     diff (union_map (snd ->- phrase) fields) pat_bound]
  and binding (binding, _: binding) : StringSet.t (* vars bound in the pattern *)
                                    * StringSet.t (* free vars in the rhs *) =
    match binding with
    | `Val (_, pat, rhs, _, _) -> pattern pat, phrase rhs
    | `Fun ((name,_,_), (_, fn), _, _) -> singleton name, (diff (funlit fn) (singleton name))
    | `Funs funs -> 
        let names, rhss = 
          List.fold_right
            (fun ((n,_,_), (_, rhs), _, _, _) (names, rhss) ->
               (add n names, rhs::rhss))
            funs
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | `Foreign ((name, _, _), _, _) -> singleton name, empty
    | `Include _
    | `Type _
    | `Infix -> empty, empty
    | `Exp p -> empty, phrase p
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map pattern) args)
  and block (binds, expr : binding list * phrase) : StringSet.t = 
    ListLabels.fold_right binds ~init:(phrase expr)
      ~f:(fun bind bodyfree ->
            let patbound, exprfree = binding bind in
              union exprfree (diff bodyfree patbound))
  and case (pat, body) : StringSet.t = diff (phrase body) (pattern pat)
  and regex = function
    | `Range _
    | `Simply _
    | `Any
    | `StartAnchor
    | `EndAnchor
    | `Quote _ -> empty
    | `Seq rs -> union_map regex rs
    | `Alternate (r1, r2) -> union (regex r1) (regex r2)
    | `Group r
    | `Repeat (_, r) -> regex r
    | `Splice p -> phrase p
    | `Replace (r, `Literal _) -> regex r
    | `Replace (r, `Splice p) -> union (regex r) (phrase p)
end
