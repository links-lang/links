(*pp deriving *)
open Operators

(** The syntax tree created by the parser. *)

type name = string [@@deriving show]

type position = SourceCode.pos
let dummy_position = SourceCode.dummy_pos

let pp_position : Format.formatter -> position -> unit = fun fmt _ -> Utility.format_omission fmt

type binder = name * Types.datatype option * position
    [@@deriving show]

(* type variables *)
type tyvar = Types.quantifier
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

(*
   NOTE: tyvar lists represent big-lambda binders.

   Currently they are only supported at HM generalisation points,
   i.e. in let-bindings.
*)

type location = [`Client | `Server | `Native | `Unknown]
    [@@deriving show]

let string_of_location = function
| `Client -> "client"
| `Server -> "server"
| `Native -> "native"
| `Unknown -> "unknown"

type restriction = [ `Any | `Base | `Session | `Effect ]
    [@@deriving eq,show]
type linearity   = [ `Any | `Unl ]
    [@@deriving eq,show]

type subkind = linearity * restriction
    [@@deriving eq,show]

let default_subkind = (`Unl, `Any)

type freedom = [`Flexible | `Rigid]
    [@@deriving show]

type primary_kind = [`Type | `Row | `Presence]
    [@@deriving show]

type kind = primary_kind * subkind option
    [@@deriving show]

type type_variable = name * kind * freedom
    [@@deriving show]

(* type variable of primary kind Type? *)
type known_type_variable = name * subkind option * freedom
    [@@deriving show]

type quantifier = type_variable
  [@@deriving show]

let rigidify (name, kind, _) = (name, kind, `Rigid)

type fieldconstraint = [ `Readonly | `Default ]
    [@@deriving show]

type datatype =
  [ `TypeVar         of known_type_variable
  | `QualifiedTypeApplication of (name list * type_arg list)
  | `Function        of datatype list * row * datatype
  | `Lolli           of datatype list * row * datatype
  | `Mu              of name * datatype
  | `Forall          of quantifier list * datatype
  | `Unit
  | `Tuple           of datatype list
  | `Record          of row
  | `Variant         of row
  | `Effect          of row
  | `Table           of datatype * datatype * datatype
  | `List            of datatype
  | `TypeApplication of (string * type_arg list)
  | `Primitive       of Types.primitive
  | `DB
  | `Input           of datatype * datatype
  | `Output          of datatype * datatype
  | `Select          of row
  | `Choice          of row
  | `Dual            of datatype
  | `End ]
and row = (string * fieldspec) list * row_var
and row_var =
    [ `Closed
    | `Open of known_type_variable
    | `Recursive of name * row ]
and fieldspec =
    [ `Present of datatype
    | `Absent
    | `Var of known_type_variable ]
and type_arg =
    [ `Type of datatype
    | `Row of row
    | `Presence of fieldspec ]
      [@@deriving show]

(* Store the denotation along with the notation once it's computed *)
type datatype' = datatype * Types.datatype option
    [@@deriving show]

type constant = Constant.constant
    [@@deriving show]

type patternnode = [
| `Any
| `Nil
| `Cons     of pattern * pattern
| `List     of pattern list
| `Variant  of name * pattern option
| `Effect   of name * pattern list * pattern
| `Negative of name list
| `Record   of (name * pattern) list * pattern option
| `Tuple    of pattern list
| `Constant of constant
| `Variable of binder
| `As       of binder * pattern
| `HasType  of pattern * datatype'
]
and pattern = patternnode * position
    [@@deriving show]

type spawn_kind = [ `Angel | `Demon | `Wait ]
    [@@deriving show]

type replace_rhs = [
| `Literal of string
| `Splice  of phrase
]
and given_spawn_location = [
  | `ExplicitSpawnLocation of phrase (* spawnAt function *)
  | `SpawnClient (* spawnClient function *)
  | `NoSpawnLocation (* spawn function *)
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
and clause = pattern * phrase
and funlit = pattern list list * phrase
and handlerlit = [`Deep | `Shallow] * pattern * clause list * pattern list list option (* computation arg, cases, parameters *)
and handler = {
  sh_expr: phrase;
  sh_effect_cases: clause list;
  sh_value_cases: clause list;
  sh_descr: handler_descriptor
}
and handler_descriptor = {
  shd_depth: [`Deep | `Shallow];
  shd_types: Types.row * Types.datatype * Types.row * Types.datatype;
  shd_raw_row: Types.row;
  shd_params: handler_parameterisation option
}
and handler_parameterisation = {
  shp_bindings: (phrase * pattern) list;
  shp_types: Types.datatype list
}
and iterpatt = [
| `List of pattern * phrase
| `Table of pattern * phrase
]
and sec = [`Minus | `FloatMinus | `Project of name | `Name of name]
and declared_linearity = [ `Lin | `Unl ]
and fn_dep = string * string
and phrasenode = [
| `Constant         of constant
| `Var              of name
| `QualifiedVar     of name list
| `FunLit           of ((Types.datatype * Types.row) list) option * declared_linearity * funlit * location
| `HandlerLit       of handlerlit
(* Spawn kind, expression referring to spawn location (client n, server...), spawn block, row opt *)
| `Spawn            of spawn_kind * given_spawn_location * phrase * Types.row option
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
| `DoOperation      of name * phrase list * Types.datatype option
| `Handle           of handler
| `Switch           of phrase * (pattern * phrase) list * Types.datatype option
| `Receive          of (pattern * phrase) list * Types.datatype option
| `DatabaseLit      of phrase * (phrase option * phrase option)
(* | `TableLit         of phrase * (datatype * (Types.datatype * Types.datatype * Types.datatype) option) * (name * fieldconstraint list) list * phrase *)
| `TableLit         of phrase * (datatype * (Types.datatype * Types.datatype * Types.datatype) option) * (name * fieldconstraint list) list * phrase * phrase
| `DBDelete         of pattern * phrase * phrase option
| `DBInsert         of phrase * name list * phrase * phrase option
| `DBUpdate         of pattern * phrase * phrase option * (name * phrase) list
| `LensLit          of phrase * Types.lens_sort option
(* the lens keys lit is a literal that takes an expression and is converted into a LensLit
   with the corresponding table keys marked in the lens_sort *)
| `LensKeysLit      of phrase * phrase * Types.lens_sort option
| `LensFunDepsLit   of phrase * (string list * string list) list * Types.lens_sort option
| `LensDropLit      of phrase * string * string * phrase * Types.lens_sort option
| `LensSelectLit    of phrase * phrase * Types.lens_sort option
| `LensJoinLit      of phrase * phrase * phrase * phrase * phrase * Types.lens_sort option
| `LensGetLit       of phrase * Types.datatype option
| `LensPutLit       of phrase * phrase * Types.datatype option
| `Xml              of name * (name * (phrase list)) list * phrase option * phrase list
| `TextNode         of string
| `Formlet          of phrase * phrase
| `Page             of phrase
| `FormletPlacement of phrase * phrase * phrase
| `PagePlacement    of phrase
| `FormBinding      of phrase * pattern
(* choose *)
| `Select           of name * phrase
(* choice *)
| `Offer            of phrase * (pattern * phrase) list * Types.datatype option
(* | `Fork             of binder * phrase *)
| `CP               of cp_phrase
| `TryInOtherwise   of (phrase * pattern * phrase * phrase * Types.datatype option)
| `Raise
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
| `Fun     of binder * declared_linearity * (tyvar list * funlit) * location * datatype' option
| `Funs    of (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list
| `Handler of binder * handlerlit * datatype' option
| `Foreign of binder * name * name * name * datatype' (* Binder, raw function name, language, external file, type *)
| `QualifiedImport of name list
| `Type    of name * (quantifier * tyvar option) list * datatype'
| `Infix
| `Exp     of phrase
| `Module  of name * binding list
| `AlienBlock of (name * name * ((binder * datatype') list))
]
and binding = bindingnode * position
and directive = string * string list
and sentence = [
| `Definitions of binding list
| `Expression  of phrase
| `Directive   of directive ]
and cp_phrasenode = [
| `Unquote of binding list * phrase
| `Grab of (string * (Types.datatype * tyarg list) option) * binder option * cp_phrase
| `Give of (string * (Types.datatype * tyarg list) option) * phrase option * cp_phrase
| `GiveNothing of binder
| `Select of binder * string * cp_phrase
| `Offer of binder * (string * cp_phrase) list
| `Link of binder * binder
| `Comp of binder * cp_phrase * cp_phrase ]
and cp_phrase = cp_phrasenode * position
    [@@deriving show]

type program = binding list * phrase option
  [@@deriving show]


let make_untyped_handler ?(val_cases = []) ?parameters expr eff_cases depth =
  let shd_params =
    match parameters with
    | None -> None
    | Some pps ->
       Some { shp_bindings = pps;
              shp_types = [] }
  in
  { sh_expr = expr;
    sh_effect_cases = eff_cases;
    sh_value_cases = val_cases;
    sh_descr = {
        shd_depth = depth;
        shd_types = (Types.make_empty_closed_row (), `Not_typed, Types.make_empty_closed_row (), `Not_typed);
        shd_raw_row = Types.make_empty_closed_row ();
        shd_params = shd_params
      };
  }

(* Why does ConcreteSyntaxError take an
   unresolved position and yet
   PatternDuplicateNameError and
   RedundantPatternMatch take resolved positions?
*)
exception ConcreteSyntaxError of (string * position)
exception PatternDuplicateNameError of (SourceCode.pos * string)
exception RedundantPatternMatch of SourceCode.pos

let tabstr : tyvar list * phrasenode -> phrasenode = fun (tyvars, e) ->
  match tyvars with
    | [] -> e
    | _ ->
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
    | `Effect (_, ps, kopt) -> union (union_map pattern ps) (pattern kopt)
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

    | `Spawn (_, _, p, _)
    | `TAbstr (_, p)
    | `TAppl (p, _)
    | `FormBinding (p, _)
    | `Projection (p, _)
    | `Page p
    | `PagePlacement p
    | `Upcast (p, _, _)
    | `Select (_, p)
    | `TypeAnnotation (p, _) -> phrase p

    | `ListLit (ps, _)
    | `TupleLit ps -> union_map phrase ps

    | `LensLit (l, _) -> phrase l
    (* this should be converted to `LensLit during typeSugar *)
    | `LensFunDepsLit _ -> assert false
    | `LensKeysLit (l, _, _) -> phrase l
    | `LensSelectLit (l, _, _) -> phrase l
    | `LensDropLit (l, _, _, _, _) -> phrase l
    | `LensJoinLit (l1, l2, _, _, _, _) -> union_all [phrase l1; phrase l2]

    | `LensGetLit (l, _) -> phrase l
    | `LensPutLit (l, data, _) -> union_all [phrase l; phrase data]

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
    | `TableLit (p1, _, _, _, p2) -> union (phrase p1) (phrase p2)
    | `Xml (_, attrs, attrexp, children) ->
        union_all
          [union_map (snd ->- union_map phrase) attrs;
           option_map phrase attrexp;
           union_map phrase children]
    | `Formlet (xml, yields) ->
        let binds = formlet_bound xml in
          union (phrase xml) (diff (phrase yields) binds)
    | `HandlerLit hnlit -> handlerlit hnlit
    | `FunLit (_, _, fnlit, _) -> funlit fnlit
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
    | `Handle { sh_expr = e; sh_effect_cases = eff_cases; sh_value_cases = val_cases; sh_descr = descr } ->
       let params_bound =
         option_map
           (fun params -> union_map (snd ->- pattern) params.shp_bindings)
           descr.shd_params
       in
       union_all [phrase e;
                  union_map case eff_cases;
                  union_map case val_cases;
                  diff (option_map (fun params -> union_map (fst ->- phrase) params.shp_bindings) descr.shd_params) params_bound]
    | `Switch (p, cases, _)
    | `Offer (p, cases, _) -> union (phrase p) (union_map case cases)
    | `CP cp -> cp_phrase cp
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
    | `DoOperation (_, ps, _) -> union_map phrase ps
    | `QualifiedVar _ -> empty
    | `TryInOtherwise (p1, pat, p2, p3, _ty) -> union (union_map phrase [p1; p2; p3]) (pattern pat)
    | `Raise -> empty
  and binding (binding, _: binding) : StringSet.t (* vars bound in the pattern *)
                                    * StringSet.t (* free vars in the rhs *) =
    match binding with
    | `Val (_, pat, rhs, _, _) -> pattern pat, phrase rhs
    | `Handler ((name,_,_), hnlit, _) -> singleton name, (diff (handlerlit hnlit) (singleton name))
    | `Fun ((name,_,_), _, (_, fn), _, _) -> singleton name, (diff (funlit fn) (singleton name))
    | `Funs funs ->
        let names, rhss =
          List.fold_right
            (fun ((n,_,_), _, (_, rhs), _, _, _) (names, rhss) ->
               (add n names, rhs::rhss))
            funs
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | `Foreign ((name, _, _), _, _, _, _) -> singleton name, empty
    | `QualifiedImport _
    | `Type _
    | `Infix -> empty, empty
    | `Exp p -> empty, phrase p
    | `AlienBlock (_, _, decls) ->
        let bound_foreigns =
          List.fold_left (fun acc ((name, _, _), _) -> StringSet.add name acc)
            (StringSet.empty) decls in
        bound_foreigns, empty
        (* TODO: this needs to be implemented *)
    | `Module _ -> failwith "Freevars for modules not implemented yet"
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map pattern) args)
  and handlerlit (_, m, cases, params : handlerlit) : StringSet.t =
    union_all [diff (union_map case cases) (option_map (union_map (union_map pattern)) params); pattern m]
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
  and cp_phrase (p, _pos) = match p with
    | `Unquote e -> block e
    | `Grab ((c, _t), Some (x, _u, _), p) -> union (singleton c) (diff (cp_phrase p) (singleton x))
    | `Grab ((c, _t), None, p) -> union (singleton c) (cp_phrase p)
    | `Give ((c, _t), e, p) -> union (singleton c) (union (option_map phrase e) (cp_phrase p))
    | `GiveNothing (c, _, _) -> singleton c
    | `Select ((c, _t, _), _label, p) -> union (singleton c) (cp_phrase p)
    | `Offer ((c, _t, _), cases) -> union (singleton c) (union_map (fun (_label, p) -> cp_phrase p) cases)
    | `Link ((c, _, _), (d, _, _)) -> union (singleton c) (singleton d)
    | `Comp ((c, _t, _), left, right) -> diff (union (cp_phrase left) (cp_phrase right)) (singleton c)
end
