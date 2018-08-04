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
    | `Star
    | `Async ]
  and yield_kind =
    [ `Regular
    | `Star ]
  type let_kind =
    [ `Const
    | `Let
    | `Var ]
  type unary_op =
    [ `Plus
    | `Minus
    | `Increment of fixity
    | `Decrement of fixity
    | `LogicalNot
    | `BitwiseNot
    | `Typeof
    | `Delete
    | `Void ]
  and fixity =
    [ `Prefix
    | `Postfix ]
  type binary_op =
    [ `Plus
    | `Minus
    | `Times
    | `Division
    | `Mod
    | `Pow
    | `WeakEq
    | `StrictEq
    | `WeakNeq
    | `StrictNeq
    | `Lt
    | `Gt
    | `Ge
    | `Le
    | `BitwiseAnd
    | `BitwiseOr
    | `BitwiseXor
    | `LeftShift
    | `RightShift
    | `ZerofillRightShift
    | `Comma ]

  type literal =
    | Int of int
    | Float of float
    | Char of char
    | String of string
  type expr =
    | Lit of literal
    | Var of Ir.var
    | Func of fun_def
    | Arrow of Ir.binder list * js
    | Apply of expr * expr list
    | Unary of unary_op * expr
    | Binary of binary_op * expr * expr
    | Array of expr array
    | Obj of (label * expr) list
    | Project of [`Dot | `Subscript] * expr * string
    | Yield of { kind: yield_kind; expr: expr }
    | Await of expr
  and stmt =
    | Expr of expr
    | Return of expr
    | If of expr * js * js option
    | Switch of expr * (label * js) list * js option
    | Try of js * catch_def
    | Assign of Ir.var * expr
    | Seq of stmt list
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
      fun_body: js
    }
  and catch_def = {
      exn_binder: Ir.binder;
      catch_body: js
    }
  and decls = decl list
  and js = (decls * stmt)
  and program = js

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

    let arrow : Ir.binder list -> js -> expr
      = fun params body -> Arrow (params, body)

    let apply : expr -> expr list -> expr
      = fun f args -> Apply (f, args)

    let array : expr list -> expr
      = fun elems -> Array (Array.of_list elems)

    let obj : (label * expr) list -> expr
      = fun dict -> Obj dict

    let new_ : string -> expr list -> expr
      = fun name args ->
      apply (lit (String name)) args

    let project : expr -> label -> expr
      = let is_dottable = Str.regexp "^[_A-Za-z][A-Za-z0-9_]*$" in
        let is_keyword s = List.mem s Symbols.js_keywords in
        fun structure label ->
        let kind =
          if not (is_keyword label) && Str.string_match is_dottable label 0
          then `Dot
          else `Subscript
        in
        Project (kind, structure, label)

    let yield : ?kind:yield_kind -> expr -> expr
      = fun ?(kind=`Regular) expr -> Yield { kind; expr }

    let await : expr -> expr
      = fun expr -> Await expr

    let return : expr -> stmt
      = fun expr -> Return expr

    let ifthenelse : expr -> js -> js -> stmt
      = fun cond tt ff -> If (cond, tt, Some ff)

    let ifthen : expr -> js -> stmt
      = fun cond tt -> If (cond, tt, None)

    let catch : Ir.binder -> js -> catch_def
      = fun exn_binder catch_body -> { exn_binder; catch_body }

    let try_ : js -> catch_def -> stmt
      = fun body catch -> Try (body, catch)

    let switch : ?default:js -> expr -> (label * js) list -> stmt
      = fun ?default scrutinee cases -> Switch (scrutinee, cases, default)

    let expr : expr -> stmt
      = fun expr -> Expr expr

    let break : stmt = Break
    let continue : stmt = Continue
    let skip : stmt = Skip

    let assign : Ir.var -> expr -> stmt
      = fun var expr -> Assign (var, expr)

    let seq : stmt -> stmt -> stmt
      = fun s0 s1 ->
      match s0, s1 with
      | Seq ss0, Seq ss1 -> Seq (ss0 @ ss1)
      | Seq ss0, s1      -> Seq (ss0 @ [s1])
      | s0, Seq ss1      -> Seq (s0 :: ss1)
      | s0, s1           -> Seq [s0; s1]

    let const : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Const; binder; expr }

    let let_ : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Let; binder; expr }

    let var : Ir.binder -> expr -> decl
      = fun binder expr -> Let { kind = `Var; binder; expr }

    let lift_stmt : stmt -> js
      = fun stmt -> ([], stmt)

    let lift_expr : expr -> js
      = fun e -> lift_stmt (expr e)

    let thunk : js -> expr
      = fun body -> fun_expr (fun_def [] body)

    let thunk_and_apply : stmt -> expr
      = fun stmt ->
      apply (thunk (lift_stmt stmt)) []

    let unary : unary_op -> expr -> expr
      = fun op expr -> Unary (op, expr)

    let undefined : expr = unary `Void (lit (Int 0))

    let incr : ?fixity:fixity -> expr -> expr
      = fun ?(fixity=`Postfix) expr ->
      unary (`Increment fixity) expr

    let decr : ?fixity:fixity -> expr -> expr
      = fun ?(fixity=`Postfix) expr ->
      unary (`Decrement fixity) expr

    let binary : binary_op -> expr -> expr -> expr
      = fun op lhs rhs -> Binary (op, lhs, rhs)
  end

  let merge_programs : (program * program) -> program = function
    | (decls, Skip), (decls', stmt)  -> (decls @ decls', stmt)
    | (decls, stmt), (decls', stmt') ->
       let decl =
         let open LispJs in
         let b = Var.fresh_binder (Var.info_of_type Types.unit_type) in
         const b (thunk_and_apply stmt)
       in
       (decls @ [decl] @ decls', stmt')
end
include Lang
