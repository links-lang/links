open Irtojs
open Utility

module type JS_CODEGEN = sig
  val string_of_js : code -> string
  val emit : Js.program -> out_channel -> unit
end

module Js_CodeGen : JS_CODEGEN = struct
  (** Pretty printer for JavaScript code *)
  module PP :
  sig
    val show : code -> string
  end =
    struct
      open PP

      (** Pretty-print a Code value as a JavaScript string. *)
      let rec show (c : code) : PP.doc =
        let show_func name fn =
          match fn with
          | (Fn (vars, body)) ->
             PP.group (PP.text "function" ^+^ PP.text name ^^ (formal_list vars)
                       ^+^  (braces
                               (break ^^ group(nest 2 (show body)) ^^ break)))
          | _ -> assert false in
        let show_case v l (x, e) =
          PP.text "case" ^+^ PP.text("'"^l^"'") ^^
            PP.text ":" ^+^ braces (PP.text"var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^"._value;") ^+^
                                      show e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break in
        let show_cases v =
          fun cases ->
          StringMap.fold (fun l c s ->
              s ^+^ show_case v l c)
            cases DocNil in
        let show_default v = opt_app
                               (fun (x, e) ->
                                 PP.text "default:" ^+^ braces (PP.text "var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^";") ^+^
                                                                  show e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break) PP.DocNil in
        let maybe_parenise = function
          | Var _
            | Lit _
            | Call _
            | Dict _
            | Arr _
            | Bind _
            | Die _
            | Return _
            | Nothing as c -> show c
          | c -> parens (show c)
        in
        match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""
        | Die msg -> PP.text("error('" ^ msg ^ "', __kappa)")
        | Lit literal -> PP.text literal
        | LetFun ((name, vars, body, _location), rest) ->
           (show_func name (Fn (vars, body))) ^^ break ^^ show rest
        | LetRec (defs, rest) ->
           PP.vsep (punctuate " " (List.map (fun (name, vars, body, _loc) -> show_func name (Fn (vars, body))) defs)) ^^
             break ^^ show rest
        | Fn _ as f -> show_func "" f
        | Call (Var "LINKS.project", [record; label]) ->
           maybe_parenise record ^^ (brackets (show label))
        | Call (Var "hd", [list;kappa]) ->
           (maybe_parenise kappa) ^^ PP.text "hd" ^^ (parens (  maybe_parenise list))
        | Call (Var "tl", [list;kappa]) ->
           (maybe_parenise kappa) ^^ PP.text "tl" ^^ (parens (  maybe_parenise list))
        | Call (Var "_yield", (fn :: args)) ->
           PP.text "_yield" ^^ (parens (PP.text "function () { " ^^ maybe_parenise fn ^^
                                          parens (hsep(punctuate "," (List.map show args))) ^^ PP.text " }"))
        | Call (fn, args) -> maybe_parenise fn ^^
                               (PP.arglist (List.map show args))
        | Unop (op, body) -> PP.text op ^+^ (maybe_parenise body)
        | Binop (l, op, r) -> (maybe_parenise l) ^+^ PP.text op ^+^ (maybe_parenise r)
        | If (cond, c1, c2) ->
           PP.group (PP.text "if (" ^+^ show cond ^+^ PP.text ")"
                     ^+^  (braces
                             (break ^^ group(nest 2 (show c1)) ^^ break))
                     ^+^ PP.text "else"
                     ^+^  (braces
                             (break ^^ group(nest 2 (show c2)) ^^ break)))
        | Case (v, cases, default) ->
           PP.group (PP.text "switch" ^+^ (parens (PP.text (v^"._label"))) ^+^
                       (braces ((show_cases v cases) ^+^ (show_default v default))))
        | Dict (elems) ->
           PP.braces (hsep (punctuate ","
                              (List.map (fun (name, value) ->
                                   group (PP.text "'" ^^ PP.text name ^^
                                            PP.text "':" ^^ show value))
                                 elems)))
        | Arr elems ->
           let rec show_list = function
             | [] -> PP.text Json.nil_literal
             | x :: xs -> PP.braces (PP.text "\"_head\":" ^+^ (show x) ^^ (PP.text ",") ^|  PP.nest 1 (PP.text "\"_tail\":" ^+^  (show_list xs))) in
           show_list elems
        | Bind (name, value, body) ->
           PP.text "var" ^+^ PP.text name ^+^ PP.text "=" ^+^ show value ^^ PP.text ";" ^^
             break ^^ show body
        | Return expr ->
           PP.text "return " ^^ (show expr) ^^ PP.text ";"

      let show = show ->- PP.pretty 144
    end

  let string_of_js x = PP.show x
  let emit _src _chan = assert false
end

type env = string IntMap.t

module NewCodeGen : JS_CODEGEN = struct
  open Js

  let name_binder (x, info) =
    let name = Js.name_binder (x, info) in
    (if (name = "") then
       prerr_endline (Ir.show_binder (x, info)));
    assert (name <> "");
    name

  let extend : (Ir.var * string) -> env -> env
    = fun (k, v) env -> IntMap.add k v env

  open Prettier
  type nonrec t = Prettier.t

  module Generic = struct
    let semi : t -> t
      = fun doc ->
      hgrp (doc $ (text ";"))

    let enclose : string -> string -> t -> t
      = fun b e doc ->
      text b $ doc $ text e

    let parens = enclose "(" ")"
    let braces = enclose "{" "}"
    let brackets = enclose "[" "]"
  end
  open Generic

  let transl_unary_op : unary_op -> fixity * t
    = fun op ->
    let s, fixity =
      match op with
      | `Plus -> "+", `Prefix
      | `Minus -> "-", `Prefix
      | `Increment fix -> "++", fix
      | `Decrement fix -> "--", fix
      | `LogicalNot    -> "!", `Prefix
      | `BitwiseNot    -> "~", `Prefix
      | `Typeof        -> "typeof", `Prefix
      | `Delete        -> "delete", `Prefix
      | `Void          -> "void", `Prefix
    in
    fixity, text s

  let transl_binary_op : binary_op -> t
    = fun op ->
    let s =
      match op with
      | `Plus -> "+"
      | `Minus -> "-"
      | `Times -> "*"
      | `Division -> "/"
      | `Mod -> "%"
      | `Pow -> "**"
      | `WeakEq -> "=="
      | `StrictEq -> "==="
      | `WeakNeq -> "!="
      | `StrictNeq -> "!=="
      | `Lt -> "<"
      | `Gt -> ">"
      | `Ge -> ">="
      | `Le -> "<="
      | `BitwiseAnd -> "&"
      | `BitwiseOr  -> "|"
      | `BitwiseXor -> "^"
      | `LeftShift  -> "<<"
      | `RightShift -> ">>"
      | `ZerofillRightShift -> ">>>"
      | `Comma -> ","
    in
    text s

  module type JS = sig
    val transl : env -> js -> env * t
  end

  module type EXPR = sig
    val transl : env -> expr -> t
  end

  module type STMT = sig
    val transl : env -> stmt -> t
  end

  module type DECL = sig
    val transl : env -> decl -> env * t
    val transl_many : env -> decls -> env * t
  end

  module type MISC = sig
    val fun_def : env -> fun_def -> env * t
  end

  module Make_Misc(Js : JS) = struct
    let fun_def env : fun_def -> env * t
      = fun { kind; fun_binder; params; fun_body } ->
      let env', name =
        match fun_binder with
        | `Anonymous -> env, None
        | `Binder fb ->
           let name = name_binder fb in
           extend (Var.var_of_binder fb, name) env, Some name
      in
      let env'', params =
        let env, params =
          List.fold_left
            (fun (env, params) binder ->
              let name = name_binder binder in
              (extend (Var.var_of_binder binder, name) env, name :: params))
            (env', []) params
        in
        env, List.rev params
      in
      let pretty_f =
        let fun_keyword = match kind with
          | `Regular -> text "function"
          | `Star    -> text "function*"
          | `Async   -> text "async function"
        in
        let params = parens (commalist ~f:text params) in
        hgrp
          (fun_keyword
           $ (match name with
              | None -> params
              | Some name -> break $ (text name) $ params)
           $/ (braces (* TODO decide whether the body is a "multi-liner" *)
                 (vgrp
                    (nest 0
                       (vgrp
                          (nest 2
                             (snd @@ Js.transl env'' fun_body)))))))
      in
      env', pretty_f
  end

  module Make_Expr (Js : JS) (Misc : MISC) = struct
    let rec transl env : expr -> t = function
      | Lit c ->
         text (match c with
               | Int i    -> string_of_int i
               | Float f  -> string_of_float f
               | Char c   -> Printf.sprintf "'%c'" c
               | String s -> Printf.sprintf "\"%s\"" s)
      | Var v ->
         text (match IntMap.lookup v env with
               | None      -> failwith (Printf.sprintf "Unbound variable %d" v)
               | Some name -> name)
      | Func fun_def -> snd (Misc.fun_def env fun_def)
      | Arrow (params, body) ->
         let env', params =
           let env, params =
             List.fold_left
               (fun (env, params) binder ->
                 let name = name_binder binder in
                 (extend (Var.var_of_binder binder, name) env, name :: params))
               (env, []) params
           in
           env, List.rev params
         in
         hgrp
           (parens
              (commalist ~f:text params)
            $/ (text "=>")
            $/ (braces (snd @@ Js.transl env' body))) (* TODO: decide whether braces are necessary *)
      | Apply (f, args) ->
         let f = transl env f in
         let args = List.map (transl env) args in
         hgrp
           (f $ (parens (agrp (commalist ~f:(fun x -> x) args))))
      | Unary (op, expr) ->
         let fixity, op = transl_unary_op op in
         let expr = transl env expr in
         hgrp
           (parens
              (match fixity with
               | `Prefix -> op $ expr
               | `Postfix -> expr $ op))
      | Binary (op, lhs, rhs) ->
         let op = transl_binary_op op in
         let lhs = transl env lhs in
         let rhs = transl env rhs in
         hgrp (parens (lhs $/ op $/ rhs))
      | Array elems ->
         agrp
           (brackets (commalist ~f:(transl env) (Array.to_list elems)))
      | Obj dict ->
         let dict =
           agrp
             (commalist
                ~f:(fun (k, v) ->
                  hgrp (text (Printf.sprintf "\"%s\":" k) $/ (transl env v)))
                dict)
         in
         braces dict
      | Project (meth, expr, label) ->
         let expr = transl env expr in
         let is_well_formed_numeric_index = Str.regexp "^\\(0\\|\\([1-9][0-9]*\\)\\)$" in
         let suffix =
           match meth with
           | `Dot -> text "." $ text label
           | `Subscript when Str.string_match is_well_formed_numeric_index label 0 ->
              brackets (text label)
           | `Subscript ->
              brackets (enclose "'" "'" (text label))
         in
         hgrp (expr $ suffix)
      | Yield { kind; expr } ->
         let expr = transl env expr in
         let yield = match kind with
           | `Regular -> text "yield"
           | `Star    -> text "yield*"
         in
         hgrp (yield $/ expr)
      | Await expr ->
         let expr = transl env expr in
         hgrp (text "await" $/ expr)
      end

  module Make_Stmt (Js : JS) (Expr : EXPR) = struct
    let rec transl env : stmt -> t = function
      | Expr expr -> semi (Expr.transl env expr)
      | Return expr ->
         hgrp ( semi ((text "return") $/ (Expr.transl env expr)) )
      | If (cond, tt, ff) ->
         let cond = Expr.transl env cond in
         let tt   = snd (Js.transl env tt) in
         let ff   = opt_map (fun ff -> snd (Js.transl env ff)) ff in
         hgrp
           (    (text "if")
             $/ (parens cond)
             $/ (braces
                   (vgrp
                      (nest 0
                         (vgrp
                            (nest 2 (break_null $ tt))))))
             $ (match ff with
                | None -> empty
                | Some ff ->
                   break $ (text "else")
                   $/ (braces
                         (vgrp
                            (nest 0
                               (vgrp
                                  (nest 2 (break_null $ ff))))))))
      | Switch _ -> assert false
      | Try (try_body, { exn_binder; catch_body }) ->
         let _, try_body = Js.transl env try_body in
         let env', exn_name =
           let name = name_binder exn_binder in
           extend (Var.var_of_binder exn_binder, name) env, name
         in
         let _, catch_body = Js.transl env' catch_body in
         hgrp
           ( (text "try"
              $/ (braces (vgrp (nest 0 try_body)))
              $/ (text "catch") $/ (parens (text exn_name))
              $/ (braces (vgrp (nest 0 catch_body)))))
      | Assign (var, expr) ->
         let name =
           text (match IntMap.lookup var env with
                 | None      -> failwith (Printf.sprintf "Unbound variable %d" var)
                 | Some name -> name)
         in
         hgrp (semi (name $/ (text "=") $/ (Expr.transl env expr)))
      | Seq stmts ->
         List.fold_left
           (fun doc stmt ->
             (vgrp (nest 0 (doc $/ (transl env stmt)))))
           empty stmts
      | Skip -> empty
      | (Break : stmt) -> hgrp (semi (text "break"))
      | Continue -> hgrp (semi (text "continue"))
  end

  module Make_Decl (Misc : MISC) (Expr : EXPR) = struct
    let transl env : decl -> env * t = function
      | Let { kind; binder; expr } ->
         let name = name_binder binder in
         let env' = extend (Var.var_of_binder binder, name) env in
         let modifier =
           text (match kind with
                 | `Const -> "const"
                 | `Let   -> "let"
                 | `Var   -> "var")
         in
         let pretty_b =
           hgrp
             (   modifier
             $/ (text name)
             $/ (text "=")
             $/ (Expr.transl env' expr))
         in
         env', pretty_b
      | Fun fun_def -> Misc.fun_def env fun_def

    let transl_many env : decls -> env * t
      = fun decls ->
      List.fold_left
        (fun (env, doc) decl ->
          let env', doc' = transl env decl in
          (env', (vgrp (nest 0 (doc $/ doc')))))
        (env, empty) decls
  end

  module Make_Js (Decl : DECL) (Stmt : STMT) = struct
    let transl env : js -> env * t
      = fun (decls, stmt) ->
      let env', decls = Decl.transl_many env decls in
      let stmt = Stmt.transl env' stmt in
      env',
      (vgrp
         (nest 0
            (decls $/ stmt)))
  end

  module rec Expr : EXPR = Make_Expr(Js)(Misc)
  and        Decl : DECL = Make_Decl(Misc)(Expr)
  and        Stmt : STMT = Make_Stmt(Js)(Expr)
  and          Js : JS   = Make_Js(Decl)(Stmt)
  and        Misc : MISC = Make_Misc(Js)

  let string_of_js _js =
    let js = ([], Skip) in
    let _, js' = Js.transl IntMap.empty js (* TODO fix whole program assumption *) in
    to_string ~width:default_width js'

  let emit _src _chan = assert false
end
