open Utility

(** {0 Code generation} *)

module Symbols =
struct
  let words =
    CharMap.from_alist
      [ '!', "bang";
        '$', "dollar";
        '%', "percent";
        '&', "and";
        '*', "star";
        '+', "plus";
        '/', "slash";
        '<', "lessthan";
        '=', "equals";
        '>', "greaterthan";
        '?', "huh";
        '@', "monkey";
        '\\', "backslash";
        '^', "caret";
        '-', "hyphen";
        '.', "fullstop";
        '|', "pipe";
        '_', "underscore"]

  let js_keywords= ["break";"else";"new";"var";"case";"finally";"return";"void";
                    "catch";"for";"switch";"while";"continue";"function";"this";
                    "with";"default";"if";"throw";"delete";"in";"try";"do";
                    "instanceof";"typeof";
                    (* "future keywords" *)
                    "abstract";"enum";"int";"short";"boolean";"export";
                    "interface";"static";"byte";"extends";"long";"super";"char";
                    "final";"native";"synchronized";"class";"float";"package";
                    "throws";"const";"goto";"private";"transient";"debugger";
                    "implements";"protected";"volatile";
                   ]

  let has_symbols name =
(*    not (Lib.is_primitive name) && *)
      List.exists (not -<- Utility.Char.isWord) (explode name)

  let wordify name =
    if has_symbols name then
      ("_" ^
         mapstrcat "_"
         (fun ch ->
            if (Utility.Char.isWord ch) then
              String.make 1 ch
            else if CharMap.mem ch words then
              CharMap.find ch words
            else
              failwith("Internal error: unknown symbol character: "^String.make 1 ch))
         (Utility.explode name))
        (* TBD: it would be better if this split to chunks maximally matching
           (\w+)|(\W)
           then we would not split apart words in partly-symbolic idents. *)
    else if List.mem name js_keywords then
      "_" ^ name (* FIXME: this could conflict with Links names. *)
    else name

end

(** Generate a JavaScript name from a binder *)
let name_binder (x, info) =
  let (_, name, _) = info in
  if String.length name = 0 then
    "_" ^ string_of_int x
  else
    (* Closure conversion means we can no longer rely on top-level
       functions having unique names *)
    (* match scope with *)
    (* | `Local -> *)
      if (Str.string_match (Str.regexp "^_g[0-9]") name 0) then
        "_" ^ string_of_int x (* make the generated names slightly less ridiculous in some cases *)
      else
        Symbols.wordify name ^ "_" ^ string_of_int x
    (* | `Global -> Symbols.wordify name *)

(** Generate a JavaScript name from a variable number. *)
let var_name_var x = "_" ^ string_of_int x

(** Generate a JavaScript name from a binder based on the unique
    integer for that binder. *)
let var_name_binder (x, _) = var_name_var x

module Lang = struct
  type label = string
  type fun_kind =
    [ `Regular
    | `Star ]
  and yield_kind = fun_kind
  type let_kind =
    [ `Const
    | `Let
    | `Var ]

  type literal =
    | Int of int
    | Float of float
    | Char of char
    | String of string
  type expr =
    | Lit of literal
    | Var of Ir.var
    | Func of fun_def
    | Arrow of expr list * code
    | Apply of expr * expr list
    | Array of expr list
    | Obj of (label * expr) list
    | Project of [`Dot | `Subscript] * expr * string
    | Yield of { kind: yield_kind; expr: expr }
  and stmt =
    | Expr of expr
    | Return of expr
    | If of expr * code * code option
    | Switch of expr * (label * code) list * code option
    | Try of code * catch_def
    | Assign of Ir.var * expr
    | Skip
    | Break
    | Continue
  and decl =
    | Let of { kind: let_kind; binder: Ir.binder; expr: expr }
    | Fun of fun_def
  and fun_def = {
      kind: fun_kind;
      fun_binder: [`Anonymous | `Binder of Ir.binder];
      params: Ir.binder list;
      fun_body: code
    }
  and catch_def = {
      exn_binder: Ir.binder;
      try_body: code
    }
  and decls = decl list
  and code = (decls * stmt)
  and program = code

  module LispJs = struct
    let lit : literal -> expr
      = fun c -> Lit c

    let variable : Ir.var -> expr
      = fun v -> Var v

    let fun_def : ?name:Ir.binder -> ?kind:fun_kind -> Ir.binder list -> program -> fun_def
      = fun ?name ?(kind = `Regular) params fun_body ->
      let fun_binder = match name with
        | None -> `Anonymous
        | Some binder -> `Binder binder
      in
      { kind; fun_binder; params; fun_body }

    let fun_expr : fun_def -> expr
      = fun fd -> Func fd

    let fun_decl : fun_def -> decl
      = fun fd -> Fun fd

    let arrow : expr list -> code -> expr
      = fun params body -> Arrow (params, body)

    let apply : expr -> expr list -> expr
      = fun f args -> Apply (f, args)

    let array : expr list -> expr
      = fun elems -> Array elems

    let obj : (label * expr) list -> expr
      = fun dict -> Obj dict

    let new_ : string -> expr list -> expr
      = fun name args ->
      apply (lit (String name)) args

    let project : expr -> label -> expr
      = (* let is_well_formed_numeric_index = Str.regexp "^\\(0\\|\\([1-9][0-9]*\\\)\\)$" in *)
        let is_dottable = Str.regexp "^[_A-Za-z][A-Za-z0-9_]*$" in
        let is_keyword s = List.mem s Symbols.js_keywords in
        fun structure label ->
        let kind =
          if not (is_keyword label) && Str.string_match is_dottable label 0
          then `Dot
          else `Subscript
        in
        Project (kind, structure, label)

    let return : expr -> stmt
      = fun expr -> Return expr

    let ifthenelse : expr -> code -> code -> stmt
      = fun cond tt ff -> If (cond, tt, Some ff)

    let ifthen : expr -> code -> stmt
      = fun cond tt -> If (cond, tt, None)

    let catch : Ir.binder -> code -> catch_def
      = fun exn_binder try_body -> { exn_binder; try_body }

    let try_ : code -> catch_def -> stmt
      = fun body catch -> Try (body, catch)

    let switch : ?default:code -> expr -> (label * code) list -> stmt
      = fun ?default scrutinee cases -> Switch (scrutinee, cases, default)

    let expr : expr -> stmt
      = fun expr -> Expr expr

    let break : stmt = Break
    let continue : stmt = Continue
    let skip : stmt = Skip

    let assign : Ir.var -> expr -> stmt
      = fun var expr -> Assign (var, expr)

    let const : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Const; binder; expr }

    let let_ : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Let; binder; expr }

    let var : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Var; binder; expr }
  end


  let merge_programs : (program * program) -> program = function
    | (decls, Skip), (decls', stmt)  -> (decls @ decls', stmt)
    | (decls, stmt), (decls', stmt') ->
       let decl =
         let open LispJs in
         let b = Var.fresh_binder (Var.info_of_type Types.unit_type) in
         const b (apply
                    (arrow [] ([], stmt))
                    [])
       in
       (decls @ [decl] @ decls', stmt')
end
