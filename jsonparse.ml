type token =
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | LBRACKET
  | RBRACKET
  | TRUE
  | FALSE
  | NULL
  | STRING of (string)
  | INT of (Num.num)
  | FLOAT of (float)

open Parsing;;
# 2 "jsonparse.mly"

(* JSON <-> Sl_result.result *)



let jsonize_primitive : Sl_result.primitive -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML _
  | `PFunction _ as p -> prerr_endline ("Can't yet jsonize " ^ Sl_result.string_of_primitive p); ""

let rec jsonize_result : Sl_result.result -> string = function
  | `Variant _
  | `Database _
  | `Environment _
  | `Continuation _
  | `Collection (`List, (`Primitive(`XML _)::_))
  | `Function _ as r -> prerr_endline ("Can't yet jsonize " ^ Sl_result.string_of_result r); ""
  | `Primitive p -> jsonize_primitive p
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ " : " ^ jsonize_result v) fields) ^ "}"
  | `Collection (_, []) -> "[]"
  | `Collection (`List, `Primitive(`Char _)::_) as c  -> "\"" ^ Sl_result.escape (Sl_result.charlist_as_string c) ^ "\""
  | `Collection (`List, elems) -> "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"
  | r -> prerr_endline ("Can't yet jsonize " ^ Sl_result.string_of_result r); ""

# 45 "jsonparse.ml"
let yytransl_const = [|
  257 (* LBRACE *);
  258 (* RBRACE *);
  259 (* COLON *);
  260 (* COMMA *);
  261 (* LBRACKET *);
  262 (* RBRACKET *);
  263 (* TRUE *);
  264 (* FALSE *);
  265 (* NULL *);
    0|]

let yytransl_block = [|
  266 (* STRING *);
  267 (* INT *);
  268 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\006\000\006\000\007\000\
\007\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\008\000\005\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\003\000\005\000\002\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\014\000\015\000\016\000\017\000\
\020\000\019\000\021\000\001\000\012\000\013\000\010\000\011\000\
\002\000\018\000\000\000\000\000\006\000\008\000\000\000\003\000\
\000\000\000\000\000\000\007\000\000\000\004\000\009\000\000\000\
\005\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\019\000\020\000\014\000\023\000\015\000\
\016\000"

let yysindex = "\002\000\
\024\255\000\000\011\255\000\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\255\014\255\000\000\000\000\254\254\000\000\
\005\255\024\255\024\255\000\000\015\255\000\000\000\000\024\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\252\255\000\000\000\000\250\255\000\000\000\000\000\000\
\000\000"

let yytablesize = 36
let yytable = "\022\000\
\003\000\027\000\001\000\028\000\004\000\021\000\005\000\006\000\
\007\000\008\000\009\000\010\000\017\000\024\000\018\000\025\000\
\026\000\032\000\029\000\000\000\018\000\030\000\031\000\000\000\
\003\000\000\000\000\000\033\000\004\000\000\000\005\000\006\000\
\007\000\008\000\009\000\010\000"

let yycheck = "\004\000\
\001\001\004\001\001\000\006\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\002\001\002\001\010\001\004\001\
\003\001\003\001\025\000\255\255\010\001\026\000\027\000\255\255\
\001\001\255\255\255\255\032\000\005\001\255\255\007\001\008\001\
\009\001\010\001\011\001\012\001"

let yynames_const = "\
  LBRACE\000\
  RBRACE\000\
  COLON\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  TRUE\000\
  FALSE\000\
  NULL\000\
  "

let yynames_block = "\
  STRING\000\
  INT\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'value) in
    Obj.repr(
# 42 "jsonparse.mly"
        ( _1 )
# 143 "jsonparse.ml"
               : Sl_result.result))
; (fun parser_env ->
    Obj.repr(
# 45 "jsonparse.mly"
                        ( `Record [] )
# 149 "jsonparse.ml"
               : 'object_))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'members) in
    Obj.repr(
# 46 "jsonparse.mly"
                        ( `Record (List.rev _2) )
# 156 "jsonparse.ml"
               : 'object_))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'id) in
    let _3 = (peek_val parser_env 0 : 'value) in
    Obj.repr(
# 49 "jsonparse.mly"
                                     ( [_1, _3] )
# 164 "jsonparse.ml"
               : 'members))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'members) in
    let _3 = (peek_val parser_env 2 : 'id) in
    let _5 = (peek_val parser_env 0 : 'value) in
    Obj.repr(
# 50 "jsonparse.mly"
                                     ( (_3, _5) :: _1 )
# 173 "jsonparse.ml"
               : 'members))
; (fun parser_env ->
    Obj.repr(
# 53 "jsonparse.mly"
                                     ( `Collection (`List, []) )
# 179 "jsonparse.ml"
               : 'array))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'elements) in
    Obj.repr(
# 54 "jsonparse.mly"
                                     ( `Collection (`List, (List.rev _2)) )
# 186 "jsonparse.ml"
               : 'array))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'value) in
    Obj.repr(
# 57 "jsonparse.mly"
                                     ( [_1] )
# 193 "jsonparse.ml"
               : 'elements))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'elements) in
    let _3 = (peek_val parser_env 0 : 'value) in
    Obj.repr(
# 58 "jsonparse.mly"
                                     ( _3 :: _1 )
# 201 "jsonparse.ml"
               : 'elements))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'string) in
    Obj.repr(
# 61 "jsonparse.mly"
                                     ( _1 )
# 208 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'number) in
    Obj.repr(
# 62 "jsonparse.mly"
                                     ( _1 )
# 215 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'object_) in
    Obj.repr(
# 63 "jsonparse.mly"
                                     ( _1 )
# 222 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'array) in
    Obj.repr(
# 64 "jsonparse.mly"
                                     ( _1 )
# 229 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    Obj.repr(
# 65 "jsonparse.mly"
                                     ( `Primitive (`Bool true) )
# 235 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    Obj.repr(
# 66 "jsonparse.mly"
                                     ( `Primitive (`Bool false) )
# 241 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    Obj.repr(
# 67 "jsonparse.mly"
                                     ( `Record [] (* Or an error? *) )
# 247 "jsonparse.ml"
               : 'value))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 70 "jsonparse.mly"
                                     ( Sl_result.string_as_charlist _1 )
# 254 "jsonparse.ml"
               : 'string))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 73 "jsonparse.mly"
                                     ( _1 )
# 261 "jsonparse.ml"
               : 'id))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : float) in
    Obj.repr(
# 76 "jsonparse.mly"
                                    ( `Primitive (`Float _1) )
# 268 "jsonparse.ml"
               : 'number))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Num.num) in
    Obj.repr(
# 77 "jsonparse.mly"
                                    ( `Primitive (`Int _1) )
# 275 "jsonparse.ml"
               : 'number))
(* Entry parse_json *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let parse_json (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Sl_result.result)
