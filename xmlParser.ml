type token =
  | IGNORE
  | END
  | EQ
  | LQUOTE
  | RQUOTE
  | STRING of (string)
  | CDATA of (string)
  | VARIABLE of (string)
  | LXML of (string)
  | ENDTAG of (string)
  | RXML
  | SLASHRXML
  | LCDATA
  | RCDATA
  | CHAR of (char)

open Parsing;;
# 2 "xmlParser.mly"
open Utility
open Value

let ensure_match (start, finish, _) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (Sugartypes.ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
                                     (start, finish, None)))

let pos () : Sugartypes.position = Parsing.symbol_start_pos (), Parsing.symbol_end_pos (), None

# 31 "xmlParser.ml"
let yytransl_const = [|
  257 (* IGNORE *);
  258 (* END *);
  259 (* EQ *);
  260 (* LQUOTE *);
  261 (* RQUOTE *);
  267 (* RXML *);
  268 (* SLASHRXML *);
  269 (* LCDATA *);
  270 (* RCDATA *);
    0|]

let yytransl_block = [|
  262 (* STRING *);
  263 (* CDATA *);
  264 (* VARIABLE *);
  265 (* LXML *);
  266 (* ENDTAG *);
  271 (* CHAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\004\000\005\000\005\000\006\000\006\000\
\007\000\002\000\002\000\002\000\002\000\002\000\002\000\008\000\
\008\000\008\000\008\000\009\000\009\000\010\000\010\000\011\000\
\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\002\000\005\000\004\000\
\001\000\002\000\003\000\004\000\004\000\003\000\005\000\001\000\
\002\000\001\000\002\000\001\000\001\000\001\000\003\000\000\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\026\000\002\000\001\000\003\000\
\000\000\010\000\000\000\000\000\000\000\005\000\000\000\022\000\
\011\000\000\000\020\000\000\000\000\000\021\000\000\000\000\000\
\014\000\006\000\017\000\000\000\000\000\012\000\019\000\000\000\
\013\000\000\000\025\000\023\000\008\000\009\000\000\000\015\000\
\007\000"

let yydgoto = "\002\000\
\005\000\019\000\011\000\012\000\013\000\014\000\039\000\020\000\
\021\000\022\000\029\000"

let yysindex = "\002\000\
\006\255\000\000\006\255\012\255\000\000\000\000\000\000\000\000\
\255\254\000\000\254\254\015\255\013\255\000\000\009\255\000\000\
\000\000\018\255\000\000\025\255\009\255\000\000\030\255\004\255\
\000\000\000\000\000\000\018\255\022\255\000\000\000\000\024\255\
\000\000\027\255\000\000\000\000\000\000\000\000\033\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\255\000\000\029\255\000\000\
\000\000\026\255\000\000\000\000\031\255\000\000\000\000\000\000\
\000\000\000\000\000\000\026\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\039\000\001\000\000\000\000\000\000\000\030\000\000\000\004\000\
\000\000\000\000\016\000"

let yytablesize = 44
let yytable = "\015\000\
\023\000\006\000\001\000\006\000\015\000\016\000\003\000\004\000\
\017\000\015\000\016\000\018\000\004\000\033\000\004\000\016\000\
\018\000\004\000\027\000\008\000\008\000\018\000\009\000\010\000\
\031\000\024\000\025\000\034\000\037\000\038\000\004\000\004\000\
\028\000\032\000\030\000\036\000\040\000\041\000\016\000\024\000\
\018\000\007\000\026\000\035\000"

let yycheck = "\001\001\
\003\001\001\000\001\000\003\000\001\001\007\001\001\001\009\001\
\010\001\001\001\007\001\013\001\009\001\010\001\009\001\007\001\
\013\001\009\001\015\000\008\001\008\001\013\001\011\001\012\001\
\021\000\011\001\012\001\024\000\005\001\006\001\011\001\012\001\
\015\001\004\001\010\001\014\001\010\001\005\001\010\001\014\001\
\010\001\003\000\013\000\028\000"

let yynames_const = "\
  IGNORE\000\
  END\000\
  EQ\000\
  LQUOTE\000\
  RQUOTE\000\
  RXML\000\
  SLASHRXML\000\
  LCDATA\000\
  RCDATA\000\
  "

let yynames_block = "\
  STRING\000\
  CDATA\000\
  VARIABLE\000\
  LXML\000\
  ENDTAG\000\
  CHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.xmlitem) in
    Obj.repr(
# 32 "xmlParser.mly"
                                                               ( _2 )
# 142 "xmlParser.ml"
               : Value.xmlitem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'xml_tree) in
    Obj.repr(
# 33 "xmlParser.mly"
                                                               ( _1 )
# 149 "xmlParser.ml"
               : Value.xmlitem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "xmlParser.mly"
                                                               ( _1 )
# 156 "xmlParser.ml"
               : 'xmlid))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 39 "xmlParser.mly"
                                                               ( _1 )
# 163 "xmlParser.ml"
               : 'attrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attr) in
    Obj.repr(
# 42 "xmlParser.mly"
                                                               ( [_1] )
# 170 "xmlParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'attr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr) in
    Obj.repr(
# 43 "xmlParser.mly"
                                                               ( _2 :: _1 )
# 178 "xmlParser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'xmlid) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attr_val) in
    Obj.repr(
# 46 "xmlParser.mly"
                                                               ( Attr (_1, _4) )
# 186 "xmlParser.ml"
               : 'attr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'xmlid) in
    Obj.repr(
# 47 "xmlParser.mly"
                                                               ( Attr (_1, "") )
# 193 "xmlParser.ml"
               : 'attr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "xmlParser.mly"
                                                               ( _1 )
# 200 "xmlParser.ml"
               : 'attr_val))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 53 "xmlParser.mly"
                                                               ( Node (_1, []) )
# 207 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "xmlParser.mly"
                                                               ( ensure_match (pos()) _1 _3 (Node (_1, [])) )
# 215 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'xml_contents_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "xmlParser.mly"
                                                               ( ensure_match (pos()) _1 _4 (Node (_1, _3)) )
# 224 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'attrs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "xmlParser.mly"
                                                               ( ensure_match (pos()) _1 _4 (Node (_1, _2)) )
# 233 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'attrs) in
    Obj.repr(
# 57 "xmlParser.mly"
                                                               ( Node (_1, _2) )
# 241 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'attrs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'xml_contents_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "xmlParser.mly"
                                                               ( ensure_match (pos()) _1 _5 (Node (_1, _2 @ _4)) )
# 251 "xmlParser.ml"
               : 'xml_tree))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "xmlParser.mly"
                                                               ( [] )
# 257 "xmlParser.ml"
               : 'xml_contents_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'xml_contents_list) in
    Obj.repr(
# 62 "xmlParser.mly"
                                                               ( _2 )
# 264 "xmlParser.ml"
               : 'xml_contents_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'xml_contents) in
    Obj.repr(
# 63 "xmlParser.mly"
                                                               ( [_1] )
# 271 "xmlParser.ml"
               : 'xml_contents_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'xml_contents) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'xml_contents_list) in
    Obj.repr(
# 64 "xmlParser.mly"
                                                               ( _1 :: _2 )
# 279 "xmlParser.ml"
               : 'xml_contents_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'xml_tree) in
    Obj.repr(
# 67 "xmlParser.mly"
                                                               ( _1 )
# 286 "xmlParser.ml"
               : 'xml_contents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cdata) in
    Obj.repr(
# 68 "xmlParser.mly"
                                                               ( Text _1 )
# 293 "xmlParser.ml"
               : 'xml_contents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "xmlParser.mly"
                                                               ( _1 )
# 300 "xmlParser.ml"
               : 'cdata))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'chars) in
    Obj.repr(
# 72 "xmlParser.mly"
                                                               ( implode _2 )
# 307 "xmlParser.ml"
               : 'cdata))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "xmlParser.mly"
                                                               ( [] )
# 313 "xmlParser.ml"
               : 'chars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : char) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'chars) in
    Obj.repr(
# 76 "xmlParser.mly"
                                                               ( _1 :: _2 )
# 321 "xmlParser.ml"
               : 'chars))
(* Entry xml *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let xml (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Value.xmlitem)
