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
| `Abs
]
and regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ]
    deriving (Show)
type logical_binop = [`And | `Or ]
    deriving (Show)
type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name | `App ]
deriving (Show)
type operator = [ unary_op | binop | `Project of name ]
deriving (Show)

let string_of_unary_op =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `Name name -> name
    | `Abs -> "abs"

let string_of_binop =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `RegexMatch _ -> "<some regex nonsense>"
    | `And -> "&&"
    | `Or -> "||"
    | `Cons -> "::"    
    | `Name name -> name
    | `App -> "app"

type position = Lexing.position * Lexing.position * SourceCode.source_code option (* start * end * code *)

module Show_position = Show.ShowDefaults(
struct
  type a = position
  let format formatter _ = Format.pp_print_string formatter "..."
end)

type binder = name * Types.datatype option * position
    deriving (Show)

(* type variables *)
type tyvar = Types.quantifier
  deriving (Show)
type tyarg = Types.type_arg
  deriving (Show)
type tybinder = tyvar list * binder
  deriving (Show)

type location = Syntax.location
    deriving (Show)
type datatype = 
  | TypeVar         of name
  | RigidTypeVar    of name
  | FunctionType    of datatype list * datatype * datatype
  | MuType          of name * datatype
  | UnitType
  | TupleType       of (datatype list)
  | RecordType      of row
  | VariantType     of row
  | TableType       of datatype * datatype
  | ListType        of datatype
  | TypeApplication of (string * datatype list)
  | PrimitiveType   of Types.primitive
  | DBType
and row = (string * fieldspec) list * row_var
and row_var =
    [ `Closed
    | `Open of name
    | `OpenRigid of name
    | `Recursive of name * row ]
and fieldspec = [`Present of datatype | `Absent]
    deriving (Show)

(* Store the denotation along with the notation once it's computed *)
type datatype' = datatype * Types.datatype option
    deriving (Show)

type quantifier =
    [ `TypeVar of name | `RigidTypeVar of name
    | `RowVar of name | `RigidRowVar of name ]
      deriving (Show)

type fieldconstraint = [ `Readonly | `Identity ]
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
| `Variable of tybinder
| `As       of tybinder * pattern
| `HasType  of pattern * datatype'
]
and pattern = patternnode * position
    deriving (Show)

type typattern = pattern * Types.datatype option
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
and funlit = typattern list list * phrase
and iterpatt = [ 
| `List of typattern * phrase
| `Table of typattern * phrase
]
and sec = [`Minus | `FloatMinus | `Project of name | `Name of name]
and phrasenode = [
| `Constant         of constant
| `Var              of name
| `FunLit           of funlit
| `Spawn            of phrase
| `SpawnWait        of phrase
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
| `TAppl            of phrase * tyarg list
| `TupleLit         of phrase list
| `RecordLit        of (name * phrase) list * phrase option
| `Projection       of phrase * name
| `With             of phrase * (name * phrase) list
| `TypeAnnotation   of phrase * datatype'
| `Upcast           of phrase * datatype' * datatype'
| `ConstructorLit   of name * phrase option
| `Switch           of phrase * (typattern * phrase) list * Types.datatype option
| `Receive          of (typattern * phrase) list * Types.datatype option
| `DatabaseLit      of phrase * (phrase option * phrase option)
| `TableLit         of phrase * (datatype * (Types.datatype * Types.datatype) option) * (name * fieldconstraint list) list * phrase
| `DBDelete         of typattern * phrase * phrase option
| `DBInsert         of phrase * phrase * phrase option
| `DBUpdate         of typattern * phrase * phrase option * (name * phrase) list
| `Xml              of name * (name * (phrase list)) list * phrase option * phrase list
| `TextNode         of string
| `Formlet          of phrase * phrase
| `Page             of phrase
| `FormletPlacement of phrase * phrase * phrase
| `PagePlacement    of phrase
| `FormBinding      of phrase * typattern
]
and phrase = phrasenode * position
and bindingnode = [
| `Val     of tyvar list * typattern * phrase * location * datatype' option
| `Fun     of tybinder * funlit * location * datatype' option
| `Funs    of (tybinder * funlit * location * datatype' option) list
| `Foreign of tybinder * name * datatype'
| `Include of string
| `Type    of name * (name * int option) list * datatype'
| `Infix
| `Exp     of phrase
]
and binding = bindingnode * position
and directive = string * string list
and sentence = [ 
| `Definitions of binding list
| `Expression  of phrase
| `Directive   of directive ]
and sentence' = [ 
| `Definitions of Syntax.untyped_definition list
| `Expression  of Syntax.untyped_expression
| `Directive   of directive ]
    deriving (Show)

type program = binding list * phrase option 
  deriving (Show)

exception ConcreteSyntaxError of (string * position)
exception PatternDuplicateNameError of (Syntax.position * string * string)
exception RedundantPatternMatch of Syntax.position

let tappl : phrasenode * tyarg list -> phrasenode = fun (e, tys) ->
  match tys with
    | [] -> e
    | _ -> `TAppl ((e, (Lexing.dummy_pos, Lexing.dummy_pos, None)), tys)

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
    | `Variable (_, (v,_,_)) -> singleton v
    | `As ((_, (v,_,_)), pat)     -> add v (pattern pat)
    | `HasType (pat, _)      -> pattern pat


  let typattern (p, _) = pattern p

  let rec formlet_bound (p, _ : phrase) : StringSet.t = match p with
    | `Xml (_, _, _, children) -> union_map formlet_bound children
    | `FormBinding (_, pat) -> typattern pat
    | _ -> empty 

  let rec phrase (p, _ : phrase) : StringSet.t = match p with
    | `Var v -> singleton v
    | `Section (`Name n) -> singleton n

    | `Constant _
    | `TextNode _
    | `Section (`Minus|`FloatMinus|`Project _) -> empty

    | `Spawn p
    | `SpawnWait p
    | `TAppl (p, _)
    | `FormBinding (p, _)
    | `Projection (p, _)
    | `Page p
    | `PagePlacement p
    | `Upcast (p, _, _)
    | `TypeAnnotation (p, _) -> phrase p

    | `ListLit (ps, _)
    | `TupleLit ps -> union_map phrase ps

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
    | `ConstructorLit (_, popt) -> option_map phrase popt
    | `DatabaseLit (p, (popt1, popt2)) ->
        union_all [phrase p; option_map phrase popt1; option_map phrase popt2]
    | `DBInsert (p1, p2, popt) ->
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
    | `FunLit fnlit -> funlit fnlit
    | `Iteration (generators, body, where, orderby) ->
        let xs = union_map (function
                              | `List (_, source)
                              | `Table (_, source) -> phrase source) generators in
        let pat_bound = union_map (function
                                     | `List (pat, _)
                                     | `Table (pat, _) -> typattern pat) generators in
          union_all [xs;
                     diff (phrase body) pat_bound;
                     diff (option_map phrase where) pat_bound;
                     diff (option_map phrase orderby) pat_bound]                     
            (*     | `Iteration (`List (pat, source), body, where, orderby) *)
(*     | `Iteration (`Table (pat, source), body, where, orderby) ->  *)
(*         let pat_bound = typattern pat in *)
(*           union_all [phrase source; *)
(*                      diff (phrase body) pat_bound; *)
(*                      diff (option_map phrase where) pat_bound; *)
(*                      diff (option_map phrase orderby) pat_bound] *)
    | `Switch (p, cases, _) -> union (phrase p) (union_map case cases)
    | `Receive (cases, _) -> union_map case cases 
    | `DBDelete (pat, p, where) -> 
        union (phrase p) 
          (diff (option_map phrase where)
             (typattern pat))
    | `DBUpdate (pat, from, where, fields) -> 
        let pat_bound = typattern pat in
          union_all [phrase from;
                     diff (option_map phrase where) pat_bound;
                     diff (union_map (snd ->- phrase) fields) pat_bound]
  and binding (binding, _: binding) : StringSet.t (* vars bound in the typattern *)
                                    * StringSet.t (* free vars in the rhs *) =
    match binding with
    | `Val (_, pat, rhs, _, _) -> typattern pat, phrase rhs
    | `Fun ((_, (name,_,_)), fn, _, _) -> singleton name, (diff (funlit fn) (singleton name))
    | `Funs funs -> 
        let names, rhss = 
          List.fold_right
            (fun ((_, (n,_,_)), rhs, _, _) (names, rhss) ->
               (add n names, rhs::rhss))
            funs
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | `Foreign ((_, (name, _, _)), _, _) -> singleton name, empty
    | `Include _
    | `Type _
    | `Infix -> empty, empty
    | `Exp p -> empty, phrase p
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map typattern) args)
  and block (binds, expr : binding list * phrase) : StringSet.t = 
    ListLabels.fold_right binds ~init:(phrase expr)
      ~f:(fun bind bodyfree ->
            let patbound, exprfree = binding bind in
              union exprfree (diff bodyfree patbound))
  and case (pat, body) : StringSet.t = diff (phrase body) (typattern pat)
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
