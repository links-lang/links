open CommonTypes
open Operators
open SourceCode
open Utility

(** The syntax tree created by the parser. *)

module Binder: sig
  type t
  and with_pos = t WithPos.t
  [@@deriving show]

  val make : ?name:Name.t -> ?ty:Types.datatype -> unit -> t

  val to_name : with_pos -> string
  val to_type : with_pos -> Types.datatype

  val set_name : with_pos -> Name.t -> with_pos
  val set_type : with_pos -> Types.datatype -> with_pos

  val erase_type : with_pos -> with_pos
  val has_type : with_pos -> bool

  val traverse_map : with_pos -> o:'o
                     -> f_pos:('o -> Position.t -> 'a * Position.t)
                     -> f_name:('a -> Name.t -> 'b * Name.t)
                     -> f_ty:('b -> Types.datatype -> 'c * Types.datatype)
                     -> 'c * with_pos
end = struct
  type t = Name.t * Types.datatype
  and with_pos = t WithPos.t
  [@@deriving show]

  let make ?(name="") ?(ty=`Not_typed) () = (name, ty)

  let to_name b = let (n, _ ) = WithPos.node b in n
  let to_type b = let (_, ty) = WithPos.node b in ty

  let set_name b name = WithPos.map ~f:(fun (_   , ty) -> name, ty ) b
  let set_type b typ  = WithPos.map ~f:(fun (name, _ ) -> name, typ) b

  let erase_type b = WithPos.map ~f:(fun (name, _) -> name, `Not_typed) b
  let has_type   b = match to_type b with `Not_typed -> false | _ -> true

  let traverse_map : with_pos -> o:'o
            -> f_pos:('o -> Position.t -> 'a * Position.t)
            -> f_name:('a -> Name.t -> 'b * Name.t)
            -> f_ty:('b -> Types.datatype -> 'c * Types.datatype)
            -> 'c * with_pos = fun b ~o ~f_pos ~f_name ~f_ty ->
    WithPos.traverse_map b ~o ~f_pos ~f_node:(fun o (n, ty) ->
        let o, name = f_name o n  in
        let o, typ  = f_ty   o ty in
        o, (name, typ)
      )
end

(* type variables *)
type tyvar = Quantifier.t
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

(*
   NOTE: tyvar lists represent big-lambda binders.

   Currently they are only supported at HM generalisation points,
   i.e. in let-bindings.
*)

let default_subkind : Subkind.t = (lin_unl, res_any)
let default_effect_subkind : Subkind.t = (lin_unl, res_any)

type kind = PrimaryKind.t option * Subkind.t option
    [@@deriving show]

type type_variable = Name.t * kind * Freedom.t
    [@@deriving show]

(* type variable of primary kind Type? *)
type known_type_variable = Name.t * Subkind.t option * Freedom.t
    [@@deriving show]

type quantifier = type_variable
  [@@deriving show]

let rigidify (name, kind, _) = (name, kind, `Rigid)

let string_of_type_variable ((var, (kind, subkind), _) : type_variable) =
  match kind with
  | None -> var
  | Some kind ->
     let subkind = OptionUtils.opt_app Subkind.to_string "" subkind in
     var ^ "::" ^ PrimaryKind.to_string kind ^ subkind

type fieldconstraint = Readonly | Default
    [@@deriving show]

module Datatype = struct
  type t =
    | TypeVar         of known_type_variable
    | QualifiedTypeApplication of Name.t list * type_arg list
    | Function        of with_pos list * row * with_pos
    | Lolli           of with_pos list * row * with_pos
    | Mu              of Name.t * with_pos
    | Forall          of quantifier list * with_pos
    | Unit
    | Tuple           of with_pos list
    | Record          of row
    | Variant         of row
    | Effect          of row
    | Table           of with_pos * with_pos * with_pos
    | List            of with_pos
    | TypeApplication of string * type_arg list
    | Primitive       of Primitive.t
    | DB
    | Input           of with_pos * with_pos
    | Output          of with_pos * with_pos
    | Select          of row
    | Choice          of row
    | Dual            of with_pos
    | End
  and with_pos = t WithPos.t
  and row = (string * fieldspec) list * row_var
  and row_var =
    | Closed
    | Open of known_type_variable
    | Recursive of Name.t * row
  and fieldspec =
    | Present of with_pos
    | Absent
    | Var of known_type_variable
  and type_arg =
    | Type of with_pos
    | Row of row
    | Presence of fieldspec
      [@@deriving show]
end

(* Store the denotation along with the notation once it's computed *)
type datatype' = Datatype.with_pos * Types.datatype option
    [@@deriving show]

type type_arg' = Datatype.type_arg * Types.type_arg option
    [@@deriving show]


module Pattern = struct
  type t =
    | Any
    | Nil
    | Cons     of with_pos * with_pos
    | List     of with_pos list
    | Variant  of Name.t * with_pos option
    | Effect   of Name.t * with_pos list * with_pos
    | Negative of Name.t list
    | Record   of (Name.t * with_pos) list * with_pos option
    | Tuple    of with_pos list
    | Constant of Constant.t
    | Variable of Binder.with_pos
    | As       of Binder.with_pos * with_pos
    | HasType  of with_pos * datatype'
  and with_pos = t WithPos.t
   [@@deriving show]
end

module Alien: sig
  type 'a t [@@deriving show]
  type single [@@deriving show]
  type multi [@@deriving show]

  val object_file : 'a t -> string
  val object_name : single t -> string
  val declarations : 'a t -> (Binder.with_pos * datatype') list
  val declaration : single t -> (Binder.with_pos * datatype')
  val language : 'a t -> ForeignLanguage.t
  val modify : ?declarations:(Binder.with_pos * datatype') list -> ?language:ForeignLanguage.t -> ?object_file:string -> 'a t -> 'a t

  val multi : ForeignLanguage.t -> string -> (Binder.with_pos * datatype') list -> multi t
  val single : ForeignLanguage.t -> string -> Binder.with_pos -> datatype' -> single t
end = struct
  (* Ideally we would use a GADT and have `single` and `multi` be
     fully abstract types, however we cannot, since we insist on using
     `ppx_deriving show` which does not support either. Instead we
     simulate a GADT here by making use of phantom types. *)
  type single = unit
  and multi = unit
  and common =
    { language: ForeignLanguage.t;
      object_file: string }
  and 'a t =
    | Single of { common: common;
                  binder: Binder.with_pos;
                  datatype: datatype';
                  object_name: string }
    | Multi of { common: common;
                 declarations: (Binder.with_pos * datatype') list }
  [@@deriving show]

  let object_file : type a. a t -> string = function
    | Single { common; _ } -> common.object_file
    | Multi { common; _ } -> common.object_file

  let object_name : single t -> string = function
    | Single { object_name; _ } -> object_name
    | _ -> assert false

  let language : type a. a t -> ForeignLanguage.t = function
    | Single { common; _ } -> common.language
    | Multi { common; _ } -> common.language

  let declarations : type a. a t -> (Binder.with_pos * datatype') list = function
    | Single { binder; datatype; _ } -> [(binder, datatype)]
    | Multi { declarations; _ } -> declarations

  let declaration : single t -> (Binder.with_pos * datatype') = function
    | Single { binder; datatype; _ } -> (binder, datatype)
    | _ -> assert false

  let modify : type a. ?declarations:(Binder.with_pos * datatype') list -> ?language:ForeignLanguage.t -> ?object_file:string -> a t -> a t
    = fun ?declarations ?language ?object_file alien ->
    let common : type a. a t -> common = function
      | Single { common; _ } -> common
      | Multi { common; _ } -> common
    in
    let common =
      let common = common alien in
      match language, object_file with
      | Some language, Some object_file ->
         { language; object_file }
      | Some language, None ->
         { common with language }
      | None, Some object_file ->
         { common with object_file }
      | None, None ->
         common
    in
    match alien, declarations with
    | Single payload, Some [(binder, datatype)] ->
       Single { payload with common; binder; datatype }
    | Multi _, Some declarations ->
       Multi { common; declarations }
    | _, _ ->
       raise (Errors.internal_error ~filename:"sugartypes.ml" ~message:"Invalid arguments (Alien.modify)")

  let multi : ForeignLanguage.t -> string -> (Binder.with_pos * datatype') list -> multi t
    = fun language object_file declarations ->
    Multi { common = { language; object_file }; declarations }

  let single : ForeignLanguage.t -> string -> Binder.with_pos -> datatype' -> single t
    = fun language object_file binder datatype ->
    let object_name = Binder.to_name binder in
    Single { common = { language; object_file }; binder; datatype; object_name }
end

type spawn_kind = Angel | Demon | Wait
    [@@deriving show]

type fn_dep = string * string
    [@@deriving show]

type handler_depth = Deep | Shallow
    [@@deriving show]

type replace_rhs =
  | Literal     of string
  | SpliceExpr  of phrase
and given_spawn_location =
  | ExplicitSpawnLocation of phrase (* spawnAt function *)
  | SpawnClient (* spawnClient function *)
  | NoSpawnLocation (* spawn function *)
and regex =
  | Range     of char * char
  | Simply    of string
  | Quote     of regex
  | Any
  | StartAnchor
  | EndAnchor
  | Seq       of regex list
  | Alternate of regex * regex
  | Group     of regex
  | Repeat    of Regex.repeat * regex
  | Splice    of phrase
  | Replace   of regex * replace_rhs
and clause = Pattern.with_pos * phrase
and funlit = Pattern.with_pos list list * phrase
and handler =
  { sh_expr         : phrase
  ; sh_effect_cases : clause list
  ; sh_value_cases  : clause list
  ; sh_descr        : handler_descriptor
  }
and handler_descriptor =
  { shd_depth   : handler_depth
  ; shd_types   : Types.row * Types.datatype * Types.row * Types.datatype
  ; shd_raw_row : Types.row
  ; shd_params  : handler_parameterisation option
  }
and handler_parameterisation =
  { shp_bindings : (Pattern.with_pos * phrase) list
  ; shp_types    : Types.datatype list
  }
and iterpatt =
  | List  of Pattern.with_pos * phrase
  | Table of Pattern.with_pos * phrase
and phrasenode =
  | Constant         of Constant.t
  | Var              of Name.t
  | FreezeVar        of Name.t
  | QualifiedVar     of Name.t list
  | FunLit           of ((Types.datatype * Types.row) list) option *
                          DeclaredLinearity.t * funlit * Location.t
  (* Spawn kind, expression referring to spawn location (client n, server...),
      spawn block, row opt *)
  | Spawn            of spawn_kind * given_spawn_location * phrase *
                          Types.row option
  | Query            of (phrase * phrase) option * QueryPolicy.t * phrase *
                          Types.datatype option
  | RangeLit         of phrase * phrase
  | ListLit          of phrase list * Types.datatype option
  | Iteration        of iterpatt list * phrase
                        * (*where:*)   phrase option
                        * (*orderby:*) phrase option
  | Escape           of Binder.with_pos * phrase
  | Section          of Section.t
  | FreezeSection    of Section.t
  | Conditional      of phrase * phrase * phrase
  | Block            of block_body
  | InfixAppl        of (tyarg list * BinaryOp.t) * phrase * phrase
  | Regex            of regex
  | UnaryAppl        of (tyarg list * UnaryOp.t) * phrase
  | FnAppl           of phrase * phrase list
  | TAbstr           of tyvar list * phrase
  | TAppl            of phrase * type_arg' list
  | TupleLit         of phrase list
  | RecordLit        of (Name.t * phrase) list * phrase option
  | Projection       of phrase * Name.t
  | With             of phrase * (Name.t * phrase) list
  | TypeAnnotation   of phrase * datatype'
  | Upcast           of phrase * datatype' * datatype'
  | Instantiate      of phrase
  | Generalise       of phrase
  | ConstructorLit   of Name.t * phrase option * Types.datatype option
  | DoOperation      of Name.t * phrase list * Types.datatype option
  | Handle           of handler
  | Switch           of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype option
  | Receive          of (Pattern.with_pos * phrase) list * Types.datatype option
  | DatabaseLit      of phrase * (phrase option * phrase option)
  | TableLit         of phrase * (Datatype.with_pos * (Types.datatype *
                           Types.datatype * Types.datatype) option) *
                          (Name.t * fieldconstraint list) list * phrase * phrase
  | DBDelete         of Pattern.with_pos * phrase * phrase option
  | DBInsert         of phrase * Name.t list * phrase * phrase option
  | DBUpdate         of Pattern.with_pos * phrase * phrase option *
                          (Name.t * phrase) list
  | LensLit          of phrase * Lens.Type.t option
  | LensSerialLit    of phrase * string list * Lens.Type.t option
  (* the lens keys lit is a literal that takes an expression and is converted
     into a LensLit with the corresponding table keys marked in the lens_sort *)
  | LensKeysLit      of phrase * phrase * Lens.Type.t option
  | LensFunDepsLit   of phrase * (string list * string list) list *
                          Lens.Type.t option
  | LensDropLit      of phrase * string * string * phrase *
                          Lens.Type.t option
  | LensSelectLit    of phrase * phrase * Lens.Type.t option
  | LensJoinLit      of phrase * phrase * phrase * phrase * phrase *
                          Lens.Type.t option
  | LensGetLit       of phrase * Types.datatype option
  | LensCheckLit     of phrase * Lens.Type.t option
  | LensPutLit       of phrase * phrase * Types.datatype option
  | Xml              of Name.t * (Name.t * (phrase list)) list * phrase option *
                          phrase list
  | TextNode         of string
  | Formlet          of phrase * phrase
  | Page             of phrase
  | FormletPlacement of phrase * phrase * phrase
  | PagePlacement    of phrase
  | FormBinding      of phrase * Pattern.with_pos
  (* choose *)
  | Select           of Name.t * phrase
  (* choice *)
  | Offer            of phrase * (Pattern.with_pos * phrase) list *
                          Types.datatype option
  | CP               of cp_phrase
  | TryInOtherwise   of phrase * Pattern.with_pos * phrase * phrase *
                          Types.datatype option
  | Raise
and phrase = phrasenode WithPos.t
and bindingnode =
  | Val     of Pattern.with_pos * (tyvar list * phrase) * Location.t *
                 datatype' option
  | Fun     of function_definition
  | Funs    of recursive_function list
  | Foreign of Alien.single Alien.t
  | Import of { pollute: bool; path : Name.t list }
  | Open of Name.t list
  | Typenames of typename list
  | Infix
  | Exp     of phrase
  | Module  of { binder: Binder.with_pos; members: binding list }
  | AlienBlock of Alien.multi Alien.t
and binding = bindingnode WithPos.t
and block_body = binding list * phrase
and cp_phrasenode =
  | CPUnquote     of binding list * phrase
  | CPGrab        of (string * (Types.datatype * tyarg list) option) *
                       Binder.with_pos option * cp_phrase
  | CPGive        of (string * (Types.datatype * tyarg list) option) *
                       phrase option * cp_phrase
  | CPGiveNothing of Binder.with_pos
  | CPSelect      of Binder.with_pos * string * cp_phrase
  | CPOffer       of Binder.with_pos * (string * cp_phrase) list
  | CPLink        of Binder.with_pos * Binder.with_pos
  | CPComp        of Binder.with_pos * cp_phrase * cp_phrase
and cp_phrase = cp_phrasenode WithPos.t
and typenamenode = (Name.t * (quantifier * tyvar option) list * datatype')
and typename = typenamenode WithPos.t
and function_definition = {
    fun_binder: Binder.with_pos;
    fun_linearity: DeclaredLinearity.t;
    fun_definition: tyvar list * funlit;
    fun_location: Location.t;
    fun_signature: datatype' option;
    fun_unsafe_signature: bool;
    fun_frozen : bool;
  }
and recursive_functionnode = {
    rec_binder: Binder.with_pos;
    rec_linearity: DeclaredLinearity.t;
    rec_definition: (tyvar list * (Types.datatype * int option list) option) * funlit;
    rec_location: Location.t;
    rec_signature: datatype' option;
    rec_unsafe_signature: bool;
    rec_frozen : bool
  }
and recursive_function = recursive_functionnode WithPos.t
  [@@deriving show]

type directive = string * string list
                            [@@deriving show]

type sentence =
  | Definitions of binding list
  | Expression  of phrase
  | Directive   of directive
    [@@deriving show]

type program = binding list * phrase option
  [@@deriving show]

exception ConcreteSyntaxError       of (Position.t * string)

let tabstr : tyvar list * phrasenode -> phrasenode = fun (tyvars, e) ->
  match tyvars with
    | [] -> e
    | _  -> TAbstr (tyvars, WithPos.make e)

let tappl : phrasenode * tyarg list -> phrasenode = fun (e, tys) ->
  match tys with
    | [] -> e
    | _  ->
       let make_arg ty =
         (Datatype.Type (WithPos.make (Datatype.TypeVar ("$none", None, `Rigid))), Some ty)
       in
       TAppl (WithPos.make e, List.map make_arg tys)

let tappl' : phrase * tyarg list -> phrasenode = fun (e, tys) ->
  match tys with
    | [] -> WithPos.node e
    | _  ->
       let make_arg ty =
         (Datatype.Type (WithPos.make (Datatype.TypeVar ("$none", None, `Rigid))), Some ty)
       in
       TAppl (e, List.map make_arg tys)

module Freevars =
struct
  open Utility
  open StringSet

  let union_map f = union_all -<- List.map f
  let option_map f = opt_app f empty

  let rec pattern (phrase : Pattern.with_pos) : StringSet.t =
    let open Pattern in
    match WithPos.node phrase with
    | Any
    | Nil
    | Constant _
    | Negative _            -> empty
    | Tuple ps
    | List ps               -> union_map pattern ps
    | Cons (p1, p2)         -> union (pattern p1) (pattern p2)
    | Variant (_, popt)     -> option_map pattern popt
    | Effect (_, ps, kopt)  -> union (union_map pattern ps) (pattern kopt)
    | Record (fields, popt) ->
       union (option_map pattern popt)
         (union_map (snd ->- pattern) fields)
    | Variable bndr         -> singleton (Binder.to_name bndr)
    | As (bndr, pat)        -> add (Binder.to_name bndr) (pattern pat)
    | HasType (pat, _)      -> pattern pat


  let rec formlet_bound (phrase : phrase) : StringSet.t = match WithPos.node phrase with
    | Xml (_, _, _, children) -> union_map formlet_bound children
    | FormBinding (_, pat) -> pattern pat
    | _ -> empty

  let rec phrase (p : phrase) : StringSet.t =
    let p = WithPos.node p in
    match p with
    | Var v -> singleton v
    | FreezeVar v -> singleton v
    | Section (Section.Name n) -> singleton n
    | FreezeSection (Section.Name n) -> singleton n

    | Constant _
    | TextNode _
    | Section (Section.Minus|Section.FloatMinus|Section.Project _) -> empty
    | FreezeSection (Section.Minus|Section.FloatMinus|Section.Project _) -> empty

    | Spawn (_, _, p, _)
    | TAbstr (_, p)
    | TAppl (p, _)
    | FormBinding (p, _)
    | Projection (p, _)
    | Page p
    | PagePlacement p
    | Upcast (p, _, _)
    | Instantiate p
    | Generalise p
    | Select (_, p)
    | TypeAnnotation (p, _) -> phrase p

    | ListLit (ps, _)
    | TupleLit ps -> union_map phrase ps

    | LensLit (l, _) -> phrase l
    | LensSerialLit (l, _, _) -> phrase l
    | LensFunDepsLit (l, _, _) -> phrase l
    | LensKeysLit (l, _, _) -> phrase l
    | LensSelectLit (l, _, _) -> phrase l
    | LensDropLit (l, _, _, _, _) -> phrase l
    | LensJoinLit (l1, l2, _, _, _, _) -> union_all [phrase l1; phrase l2]

    | LensCheckLit (l, _) -> phrase l
    | LensGetLit (l, _) -> phrase l
    | LensPutLit (l, data, _) -> union_all [phrase l; phrase data]

    | Query (None, _, p, _) -> phrase p
    | Query (Some (limit, offset), _, p, _) ->
       union_all [phrase limit; phrase offset; phrase p]

    | Escape (v, p) -> diff (phrase p) (singleton (Binder.to_name v))
    | FormletPlacement (p1, p2, p3)
    | Conditional (p1, p2, p3) -> union_map phrase [p1;p2;p3]
    | Block b -> block b
    | InfixAppl ((_, BinaryOp.Name n), p1, p2) ->
       union (singleton n) (union_map phrase [p1;p2])
    | InfixAppl (_, p1, p2) -> union_map phrase [p1;p2]
    | RangeLit (p1, p2) -> union_map phrase [p1;p2]
    | Regex r -> regex r
    | UnaryAppl (_, p) -> phrase p
    | FnAppl (p, ps) -> union_map phrase (p::ps)
    | RecordLit (fields, p) ->
        union (union_map (snd ->- phrase) fields)
          (option_map phrase p)
    | With (p, fields) ->
        union (union_map (snd ->- phrase) fields)
          (phrase p)
    | ConstructorLit (_, popt, _) -> option_map phrase popt
    | DatabaseLit (p, (popt1, popt2)) ->
        union_all [phrase p; option_map phrase popt1; option_map phrase popt2]
    | DBInsert (p1, _labels, p2, popt) ->
        union_all [phrase p1; phrase p2; option_map phrase popt]
    | TableLit (p1, _, _, _, p2) -> union (phrase p1) (phrase p2)
    | Xml (_, attrs, attrexp, children) ->
        union_all
          [union_map (snd ->- union_map phrase) attrs;
           option_map phrase attrexp;
           union_map phrase children]
    | Formlet (xml, yields) ->
        let binds = formlet_bound xml in
          union (phrase xml) (diff (phrase yields) binds)
    | FunLit (_, _, fnlit, _) -> funlit fnlit
    | Iteration (generators, body, where, orderby) ->
        let xs = union_map (function
                             | List (_, source)
                             | Table (_, source) -> phrase source) generators in
        let pat_bound = union_map (function
                                  | List (pat, _)
                                  | Table (pat, _) -> pattern pat) generators in
          union_all [xs;
                     diff (phrase body) pat_bound;
                     diff (option_map phrase where) pat_bound;
                     diff (option_map phrase orderby) pat_bound]
    | Handle { sh_expr = e; sh_effect_cases = eff_cases;
               sh_value_cases = val_cases; sh_descr = descr } ->
       let params_bound =
         option_map
           (fun params -> union_map (fst ->- pattern) params.shp_bindings)
           descr.shd_params
       in
       union_all [phrase e;
                  union_map case eff_cases;
                  union_map case val_cases;
                  diff (option_map (fun params -> union_map (snd ->- phrase)
                                                    params.shp_bindings)
                          descr.shd_params) params_bound]
    | Switch (p, cases, _)
    | Offer (p, cases, _) -> union (phrase p) (union_map case cases)
    | CP cp -> cp_phrase cp
    | Receive (cases, _) -> union_map case cases
    | DBDelete (pat, p, where) ->
        union (phrase p)
          (diff (option_map phrase where)
             (pattern pat))
    | DBUpdate (pat, from, where, fields) ->
        let pat_bound = pattern pat in
          union_all [phrase from;
                     diff (option_map phrase where) pat_bound;
                     diff (union_map (snd ->- phrase) fields) pat_bound]
    | DoOperation (_, ps, _) -> union_map phrase ps
    | QualifiedVar _ -> empty
    | TryInOtherwise (p1, pat, p2, p3, _ty) ->
       union (union_map phrase [p1; p2; p3]) (pattern pat)
    | Raise -> empty
  and binding (binding': binding)
      : StringSet.t (* vars bound in the pattern *)
      * StringSet.t (* free vars in the rhs *) =
    match WithPos.node binding' with
    | Val (pat, (_, rhs), _, _) -> pattern pat, phrase rhs
    | Fun { fun_binder = bndr; fun_definition = (_, fn); _} ->
       let name = singleton (Binder.to_name bndr) in
       name, (diff (funlit fn) name)
    | Funs funs ->
        let names, rhss =
          List.fold_right
            (fun { rec_binder = bndr; rec_definition = (_, rhs); _ } (names, rhss) ->
               (add (Binder.to_name bndr) names, rhs::rhss))
            (WithPos.nodes_of_list funs)
            (empty, []) in
          names, union_map (fun rhs -> diff (funlit rhs) names) rhss
    | Import _
    | Open _
    | Typenames _
    | Infix -> empty, empty
    | Exp p -> empty, phrase p
    | Foreign alien ->
       let bound_foreigns =
         List.fold_left
           (fun acc (bndr, _) ->
             StringSet.add (Binder.to_name bndr) acc)
           (StringSet.empty)
           (Alien.declarations alien)
       in
       bound_foreigns, empty
    | AlienBlock alien ->
        let bound_foreigns =
          List.fold_left
            (fun acc (bndr, _) ->
              StringSet.add (Binder.to_name bndr) acc)
            (StringSet.empty)
            (Alien.declarations alien)
        in
        bound_foreigns, empty
    | Module { members; _ } ->
       List.fold_left
         (fun (bnd, fvs) b ->
           let bnd', fvs' = binding b in
           let fvs'' = diff fvs' bnd in
           union bnd bnd', union fvs fvs'')
         (empty, empty) members
  and funlit (args, body : funlit) : StringSet.t =
    diff (phrase body) (union_map (union_map pattern) args)
  and block (binds, expr : binding list * phrase) : StringSet.t =
    ListLabels.fold_right binds ~init:(phrase expr)
      ~f:(fun bind bodyfree ->
            let patbound, exprfree = binding bind in
              union exprfree (diff bodyfree patbound))
  and case (pat, body) : StringSet.t = diff (phrase body) (pattern pat)
  and regex = function
    | Range _
    | Simply _
    | Any
    | StartAnchor
    | EndAnchor
    | Quote _ -> empty
    | Seq rs -> union_map regex rs
    | Alternate (r1, r2) -> union (regex r1) (regex r2)
    | Group r
    | Repeat (_, r) -> regex r
    | Splice p -> phrase p
    | Replace (r, Literal _) -> regex r
    | Replace (r, SpliceExpr p) -> union (regex r) (phrase p)
  and cp_phrase p = match WithPos.node p with
    | CPUnquote (binds, expr) -> block (binds, expr)
    | CPGrab ((c, _t), Some bndr, p) ->
      union (singleton c) (diff (cp_phrase p) (singleton (Binder.to_name bndr)))
    | CPGrab ((c, _t), None, p) -> union (singleton c) (cp_phrase p)
    | CPGive ((c, _t), e, p) -> union (singleton c) (union (option_map phrase e)
                                                           (cp_phrase p))
    | CPGiveNothing bndr -> singleton (Binder.to_name bndr)
    | CPSelect (bndr, _label, p) ->
      union (singleton (Binder.to_name bndr)) (cp_phrase p)
    | CPOffer (bndr, cases) ->
      union (singleton (Binder.to_name bndr))
            (union_map (fun (_label, p) -> cp_phrase p) cases)
    | CPLink (bndr1, bndr2) ->
      union (singleton (Binder.to_name bndr1))
            (singleton (Binder.to_name bndr2))
    | CPComp (bndr, left, right) ->
       diff (union (cp_phrase left) (cp_phrase right))
            (singleton (Binder.to_name bndr))
end
