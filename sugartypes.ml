open Utility

(** The syntax tree created by the parser. *)

type name = string

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
type regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ]
type logical_binop = [`And | `Or ]
type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name | `App ]

type operator = [ unary_op | binop | `Project of name ]

type pposition = Lexing.position * Lexing.position (* start * end *)

type location = Syntax.location
type datatype = 
  | TypeVar of string
  | RigidTypeVar of string
  | FunctionType of datatype list * datatype * datatype
  | MuType of string * datatype
  | UnitType
  | TupleType of (datatype list)
  | RecordType of row
  | VariantType of row
  | TableType of datatype * datatype
  | ListType of datatype
  | TypeApplication of (string * datatype list)
  | PrimitiveType of Types.primitive
  | DBType
and row = (string * [`Present of datatype | `Absent]) list * row_var
and row_var = [ `Closed | `Open of string | `Recursive of string * row ]

type quantifier = [`TypeVar of string | `RigidTypeVar of string | `RowVar of string]

type assumption = quantifier list * datatype

type fieldconstraint = [ `Readonly ]

module type PhraseArgs = sig
  type ppattern
  type phrase
  type binding
end

module type Phrase = sig
  module P : PhraseArgs

  type phrase = P.phrase
  type ppattern = P.ppattern
  type binding = P.binding

  type constant = [
  | `Float of float
  | `Int of Num.num
  | `String of string
  | `Bool of bool
  | `Char of char ]

  type pattern = [
  | `Any
  | `Nil
  | `Cons of (ppattern * ppattern)
  | `List of (ppattern list)
  | `Variant of (string * ppattern option)
  | `Record of ((string * ppattern) list * ppattern option)
  | `Tuple of (ppattern list)
  | `Constant of constant
  | `Variable of string
  | `As of (string * ppattern)
  | `HasType of ppattern * datatype
  ]
      
  type phrasenode = [
  | `Constant of constant
  | `Var of (name)
  | `FunLit of funlit
  | `Spawn of phrase
  | `SpawnWait of P.phrase
  | `ListLit of (phrase list)
  | `Iteration of ([ `List of ppattern * phrase | `Table of ppattern * phrase ] * phrase * (*where:*)phrase option 
                  * (*orderby:*)phrase option)
  | `Escape of (name * phrase)
  | `Section of ([`Minus | `FloatMinus|`Project of name|`Name of name])
  | `Conditional of (phrase * phrase * phrase)
  | `Block of block
  | `InfixAppl of (binop * phrase * phrase)
  | `Regex of (regex)
  | `UnaryAppl of (unary_op * phrase)
  | `FnAppl of (phrase * phrase list)
  | `TupleLit of (phrase list)
  | `RecordLit of ((name * phrase) list * phrase option)
  | `Projection of (phrase * name)
  | `With of (phrase * (name * phrase) list)
  | `TypeAnnotation of (phrase * datatype)
  | `ConstructorLit of (name * phrase option)
  | `Switch of (phrase * (ppattern * phrase) list)
  | `Receive of (ppattern * phrase) list
  | `DatabaseLit of (phrase * (phrase option * phrase option))
  | `TableLit of (phrase * datatype * (string * fieldconstraint list) list * phrase)
  | `DBDelete of (ppattern * phrase * phrase option)
  | `DBInsert of (phrase * phrase)
  | `DBUpdate of (ppattern * phrase * phrase option * (name * phrase) list)
  | `Xml of (name * (string * (phrase list)) list * phrase list)
  | `TextNode of (string)
  | `Formlet of (phrase * phrase)
  | `FormBinding of (phrase * ppattern) ]
  and block = binding list * phrase
  and binding' = [
  | `Val of ppattern * phrase * location * datatype option
  | `Fun of name * funlit * location * datatype option
  | `Funs of (name * funlit * location * datatype option) list
  | `Foreign of name * name * datatype
  | `Type of (name * name list * datatype)
  | `Infix
  | `Exp of phrase ]
  and funlit = ppattern list list * phrase
  and regex = [
  | `Range of (char * char)
  | `Simply of string
  | `Quote of regex
  | `Any
  | `StartAnchor
  | `EndAnchor
  | `Seq of regex list
  | `Alternate of (regex * regex)
  | `Group of regex
  | `Repeat of (Regex.repeat * regex)
  | `Splice of phrase
  | `Replace of (regex * [`Literal of string | `Splice of phrase]) ]

  type directive = string * string list

  type sentence = [ 
    `Definitions of binding list
  | `Expression of phrase
  | `Directive of directive ]
end

module rec Untyped
 : Phrase with module P = UntypedArgs
 = Untyped
and UntypedArgs : sig
  type phrase   = Untyped.phrasenode * pposition
  type ppattern = Untyped.pattern * pposition
  type binding = Untyped.binding' * pposition
end
  = UntypedArgs

include Untyped


type sentence' = [ `Definitions of Syntax.untyped_definition list
| `Expression of Syntax.untyped_expression
| `Directive of directive ]


module Freevars =
struct
  open Utility
  open StringSet

  let union_map f = union_all -<- List.map f
  let option_map f = opt_app f empty 

  let rec pattern (p, _ : ppattern) : StringSet.t = match p with
    | `Any
    | `Nil
    | `Constant _ -> empty
    | `Tuple ps
    | `List ps -> union_map pattern ps
    | `Cons (p1, p2) -> union (pattern p1) (pattern p2)
    | `Variant (_, popt) -> option_map pattern popt
    | `Record (fields, popt) ->
        union (option_map pattern popt)
          (union_map (snd ->- pattern) fields)
    | `Variable v -> singleton v
    | `As (v, pat) -> add v (pattern pat)
    | `HasType (pat, _) -> pattern pat


  let rec formlet_bound (p, _ : phrase) : StringSet.t = match p with
    | `Xml (_, _, children) -> union_map formlet_bound children
    | `FormBinding (_, pat) -> pattern pat
    | _ -> empty 

  let rec phrase (p, _ : phrase) : StringSet.t = match p with
    | `Var v -> singleton v
    | `Section (`Name n) -> singleton n

    | `Constant _
    | `TextNode _
    | `Section (`Minus|`FloatMinus|`Project _) -> empty

    | `Spawn p
    | `SpawnWait p
    | `FormBinding (p, _)
    | `Projection (p, _)
    | `TypeAnnotation (p, _) -> phrase p

    | `ListLit ps
    | `TupleLit ps -> union_map phrase ps

    | `Escape (v, p) -> diff (phrase p) (singleton v)
    | `Conditional (p1, p2, p3) -> union_map phrase [p1;p2;p3]
    | `Block b -> block b
    | `InfixAppl (_, p1, p2) -> union_map phrase [p1;p2]
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
    | `DBInsert (p1, p2)
    | `TableLit (p1, _, _, p2) -> union (phrase p1) (phrase p2) 
    | `Xml (_, attrs, children) -> 
        union (union_map (snd ->- union_map phrase) attrs)
          (union_map phrase children)
    | `Formlet (xml, yields) ->
      let binds = formlet_bound xml in
        union (phrase xml) (diff (phrase yields) binds)
    | `FunLit fnlit -> funlit fnlit
    | `Iteration (`List (pat, source), body, where, orderby)

    | `Iteration (`Table (pat, source), body, where, orderby) -> 
        let pat_bound = pattern pat in
          union_all [phrase source;
                     diff (phrase body) pat_bound;
                     diff (option_map phrase where) pat_bound;
                     diff (option_map phrase orderby) pat_bound]
    | `Switch (p, cases) -> union (phrase p) (union_map case cases)
    | `Receive cases -> union_map case cases 
    | `DBDelete (pat, p, where) -> 
        union (phrase p) 
          (diff (option_map phrase where)
             (pattern pat))
    | `DBUpdate (pat, from, where, fields) -> 
        let pat_bound = pattern pat in
          union_all [phrase from;
                     diff (option_map phrase where) pat_bound;
                     diff (union_map (snd ->- phrase) fields) pat_bound]
  and binding (binding,_: binding) : StringSet.t (* vars bound in the pattern *)
                                   * StringSet.t (* free vars in the rhs *) =
    match binding with
    | `Val (pat, rhs, _, _) -> pattern pat, phrase rhs
    | `Fun (name, fn, _, _) -> singleton name, (diff (funlit fn) (singleton name))
    | `Funs funs -> 
        let names, rhss = 
          List.fold_right
            (fun (n, rhs, _, _) (names, rhss) ->
               (add n names, rhs::rhss))
            funs
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | `Foreign (name, _, _) -> singleton name, empty
    | `Type _
    | `Infix -> empty, empty
    | `Exp p -> empty, phrase p
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map pattern) args)
  and block (binds, expr : block) : StringSet.t = 
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

(** refine_bindings locates mutually-recursive cliques in sequences of
    bindings.  (As a side effect we also dispense with `Infix
    declarations, which are only used during the parsing stage.)
*)
let refine_bindings : binding list -> binding list =
  fun bindings -> 
    (* Group sequences of functions together *)
    let initial_groups = 
      let group, groups = 
        List.fold_right
          (fun bind (thisgroup, othergroups) -> 
             let add group groups = match group with
               | [] -> groups
               | _  -> group::groups in
               match fst bind with
                 | `Funs _ -> assert false
                 | `Exp _
                 | `Foreign _
                 | `Type _
                 | `Val _ ->
                     (* collapse the group we're collecting, then start a
                        new empty group *)
                     ([], [bind] :: add thisgroup othergroups)
                 | `Fun _ ->
                     (* Add binding to group *)
                     (bind::thisgroup, othergroups)
                 | `Infix -> 
                     (* discard binding *)
                     (thisgroup, othergroups))
          bindings ([], []) in
        group::groups
    in 
    (* build a callgraph *)
    let callgraph : _ -> (string * (string list)) list
      = fun defs -> 
        let defs = List.map (function
                               | `Fun (name, funlit, _, _), _ -> (name, funlit)
                               | _ -> assert false) defs in
        let names = StringSet.from_list (List.map fst defs) in
          List.map
            (fun (name, body) -> name, 
               StringSet.elements 
                 (StringSet.inter (Freevars.funlit body) names))
            defs in
    (* refine a group of function bindings *)
    let groupFuns pos (funs : binding list) : binding list = 
      let unFun = function
        | `Fun f, _ -> f
        | _ -> assert false in
      let find_fun name = 
        List.find (function
                     | `Fun (n, _, _, _), _ when name = n -> true
                     | _ -> false) 
          funs in
      let graph = callgraph funs in
      let cliques = Graph.topo_sort_cliques graph in
        List.map
          (fun clique ->
             `Funs (List.map (find_fun ->- unFun) clique), pos)
          cliques
    in 
    (* refine a group of bindings *)
    let group = function
      | (`Fun _, pos)::_ as funs -> groupFuns pos funs
      | binds                  -> binds in
      concat_map group initial_groups
