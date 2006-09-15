let eof = 0
let encoding_error = 1
let xml_char = 2
let blank = 3
let lowercase = 4
let uppercase = 5
let ascii_digit = 6
let char_23 = 7
let char_5f = 8
let char_3c = 9
let char_3e = 10
let char_3d = 11
let char_2e = 12
let char_2c = 13
let char_3a = 14
let char_3b = 15
let char_2b = 16
let char_2d = 17
let char_2a = 18
let char_2f = 19
let char_40 = 20
let char_26 = 21
let char_7b = 22
let char_7d = 23
let char_5b = 24
let char_5d = 25
let char_28 = 26
let char_29 = 27
let char_7c = 28
let char_3f = 29
let char_60 = 30
let char_22 = 31
let char_5c = 32
let char_27 = 33
let char_21 = 34
let unicode_base_char = 35
let unicode_ideographic = 36
let unicode_combining_char = 37
let unicode_digit = 38
let unicode_extender = 39

let one_char_classes = [
  (0x23, 07);
  (0x5f, 08);
  (0x3c, 09);
  (0x3e, 10);
  (0x3d, 11);
  (0x2e, 12);
  (0x2c, 13);
  (0x3a, 14);
  (0x3b, 15);
  (0x2b, 16);
  (0x2d, 17);
  (0x2a, 18);
  (0x2f, 19);
  (0x40, 20);
  (0x26, 21);
  (0x7b, 22);
  (0x7d, 23);
  (0x5b, 24);
  (0x5d, 25);
  (0x28, 26);
  (0x29, 27);
  (0x7c, 28);
  (0x3f, 29);
  (0x60, 30);
  (0x22, 31);
  (0x5c, 32);
  (0x27, 33);
  (0x21, 34);
 ]

let nb_classes = 40

# 17 "parser/wlexer.mll"
 
  (* the new Lexing lib in OCaml 3.07 has a different handling of
     locations. The following functions simulate the old behavior *)
  let lexeme_start lexbuf = 
    lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_start_pos
  let lexeme_end lexbuf = 
    lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos

  let keywords = Hashtbl.create 17

  let in_comment = ref false

  let error = Location.raise_loc
  exception Illegal_character of char
  exception Unterminated_comment
  exception Unterminated_string
  exception Unterminated_string_in_comment


  (* Buffer for string literals (always encoded in UTF8). *)
    
  let string_buff = Buffer.create 1024

  let store_ascii = Buffer.add_char string_buff
  let store_char  = Buffer.add_string string_buff
  let store_code  = Encodings.Utf8.store string_buff
  let get_stored_string () =
    let s = Buffer.contents string_buff in
    Buffer.clear string_buff;
    s
  let store_special = function
    | 'n' ->  store_ascii '\n' 
    | 'r' ->  store_ascii '\r' 
    | 't' ->  store_ascii '\t' 
    | c -> raise (Illegal_character '\\')

  let string_start_pos = ref 0;;
  let comment_start_pos : int list ref = ref [];;

  let decimal_char s =
    int_of_string (String.sub s 1 (String.length s - 2))


  let hexa_digit = function
    | '0'..'9' as c -> (Char.code c) - (Char.code '0')
    | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
    | _ -> failwith "Invalid hexadecimal digit" (* TODO: error loc *)


  let hexa_char s =
    let rec aux i accu =
      if i = String.length s - 1 then accu
      else aux (succ i) (accu * 16 + hexa_digit s.[i])
    in
    aux 0 0


# 134 "parser/wlexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\246\255\245\255\008\000\036\000\007\000\250\255\005\000\
    \005\000\015\000\013\000\002\000\000\000\034\000\036\000\005\000\
    \033\000\047\000\248\255\036\000\247\255\052\000\041\000\252\255\
    \062\000\088\000\253\255\077\000\251\255\043\000\036\000\254\255\
    \255\255\081\000\249\255\098\000\070\000\111\000\103\000\128\000\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\000\000\001\000\004\000\255\255\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\255\255\005\000\255\255\006\000\255\255\255\255\
    \255\255\001\000\255\255\255\255\255\255\004\000\004\000\255\255\
    \255\255\255\255\255\255\004\000\255\255\255\255\001\000\255\255\
    ";
  Lexing.lex_default = 
   "\006\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\000\000\028\000\000\000\255\255\255\255\000\000\
    \000\000\034\000\000\000\255\255\255\255\031\000\255\255\255\255\
    ";
  Lexing.lex_trans = 
   "\001\000\002\000\002\000\003\000\004\000\004\000\005\000\017\000\
    \004\000\013\000\014\000\003\000\007\000\005\000\010\000\008\000\
    \016\000\009\000\015\000\022\000\006\000\005\000\011\000\006\000\
    \006\000\006\000\019\000\006\000\012\000\015\000\006\000\018\000\
    \006\000\018\000\006\000\004\000\004\000\002\000\002\000\002\000\
    \004\000\004\000\004\000\006\000\004\000\006\000\006\000\006\000\
    \004\000\006\000\024\000\021\000\021\000\004\000\020\000\021\000\
    \021\000\021\000\021\000\023\000\021\000\032\000\006\000\031\000\
    \021\000\000\000\025\000\025\000\000\000\021\000\025\000\004\000\
    \004\000\004\000\004\000\004\000\036\000\023\000\000\000\000\000\
    \026\000\006\000\021\000\021\000\000\000\023\000\000\000\021\000\
    \021\000\021\000\021\000\021\000\025\000\025\000\025\000\030\000\
    \025\000\025\000\025\000\000\000\025\000\000\000\026\000\029\000\
    \036\000\025\000\000\000\039\000\026\000\039\000\026\000\255\255\
    \032\000\035\000\032\000\038\000\000\000\038\000\032\000\000\000\
    \000\000\000\000\000\000\025\000\025\000\025\000\025\000\025\000\
    \000\000\031\000\031\000\031\000\039\000\000\000\039\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\008\000\009\000\000\000\012\000\
    \010\000\009\000\000\000\010\000\000\000\000\000\011\000\000\000\
    \009\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\013\000\004\000\013\000\014\000\014\000\
    \004\000\016\000\004\000\017\000\017\000\004\000\019\000\017\000\
    \021\000\021\000\021\000\022\000\021\000\029\000\016\000\030\000\
    \021\000\255\255\024\000\024\000\255\255\021\000\024\000\004\000\
    \004\000\004\000\004\000\004\000\036\000\027\000\255\255\255\255\
    \024\000\033\000\017\000\017\000\255\255\036\000\255\255\021\000\
    \021\000\021\000\021\000\021\000\025\000\025\000\025\000\027\000\
    \025\000\024\000\024\000\255\255\025\000\255\255\035\000\027\000\
    \035\000\025\000\255\255\038\000\027\000\038\000\027\000\037\000\
    \033\000\033\000\033\000\037\000\255\255\037\000\038\000\255\255\
    \255\255\255\255\255\255\025\000\025\000\025\000\025\000\025\000\
    \255\255\035\000\035\000\035\000\039\000\255\255\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\039\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token engine lexbuf = 
  match engine __ocaml_lex_tables 0 lexbuf with
      | 0 ->
# 85 "parser/wlexer.mll"
              ( token engine lexbuf )
# 222 "parser/wlexer.ml"

  | 1 ->
# 87 "parser/wlexer.mll"
      ( 
	let s = Lexing.lexeme lexbuf in
	if Hashtbl.mem keywords s then "",s else "IDENT",s
      )
# 230 "parser/wlexer.ml"

  | 2 ->
# 92 "parser/wlexer.mll"
      ( 
	let s = Lexing.lexeme lexbuf in
	let s = String.sub s 0 (String.length s - 2) in
	"ANY_IN_NS", s
      )
# 239 "parser/wlexer.ml"

  | 3 ->
# 97 "parser/wlexer.mll"
          ( "ANY_IN_NS", "" )
# 244 "parser/wlexer.ml"

  | 4 ->
# 99 "parser/wlexer.mll"
    ( "INT",Lexing.lexeme lexbuf )
# 249 "parser/wlexer.ml"

  | 5 ->
# 104 "parser/wlexer.mll"
 ( "",Lexing.lexeme lexbuf )
# 254 "parser/wlexer.ml"

  | 6 ->
# 105 "parser/wlexer.mll"
               ( "DIRECTIVE",Lexing.lexeme lexbuf )
# 259 "parser/wlexer.ml"

  | 7 ->
# 107 "parser/wlexer.mll"
      ( let string_start = lexeme_start lexbuf in
        string_start_pos := string_start;
	let double_quote = Lexing.lexeme_char lexbuf 0 = '"' in
        string (Lexing.lexeme lexbuf) engine lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        (if double_quote then "STRING2" else "STRING1"), 
	(get_stored_string()) )
# 271 "parser/wlexer.ml"

  | 8 ->
# 117 "parser/wlexer.mll"
      ( comment_start_pos := [lexeme_start lexbuf];
	in_comment := true;
        comment engine lexbuf;
	in_comment := false;
        token engine lexbuf )
# 280 "parser/wlexer.ml"

  | 9 ->
# 124 "parser/wlexer.mll"
      ( "EOI","" )
# 285 "parser/wlexer.ml"

  | 10 ->
# 126 "parser/wlexer.mll"
      ( error 
	  (lexeme_start lexbuf) (lexeme_end lexbuf)
	  (Illegal_character ((Lexing.lexeme lexbuf).[0])) )
# 292 "parser/wlexer.ml"

  | _ -> failwith "lexing: empty token [token]"

and comment engine lexbuf = 
  match engine __ocaml_lex_tables 27 lexbuf with
      | 0 ->
# 132 "parser/wlexer.mll"
      ( comment_start_pos := lexeme_start lexbuf :: !comment_start_pos;
        comment engine lexbuf;
      )
# 303 "parser/wlexer.ml"

  | 1 ->
# 136 "parser/wlexer.mll"
      ( comment_start_pos := List.tl !comment_start_pos;
	if !comment_start_pos <> [] then comment engine lexbuf;
      )
# 310 "parser/wlexer.ml"

  | 2 ->
# 140 "parser/wlexer.mll"
      ( string_start_pos := lexeme_start lexbuf;
	Buffer.clear string_buff;
	let ender = Lexing.lexeme lexbuf in
        (try string ender engine lexbuf
         with Location.Location (_,_,Unterminated_string) ->
           let st = List.hd !comment_start_pos in
           error st (st+2) Unterminated_string_in_comment);
	Buffer.clear string_buff;
        comment engine lexbuf )
# 323 "parser/wlexer.ml"

  | 3 ->
# 150 "parser/wlexer.mll"
      ( let st = List.hd !comment_start_pos in
        error st (st+2) Unterminated_comment
      )
# 330 "parser/wlexer.ml"

  | 4 ->
# 154 "parser/wlexer.mll"
      ( comment engine lexbuf )
# 335 "parser/wlexer.ml"

  | _ -> failwith "lexing: empty token [comment]"

and string ender engine lexbuf = 
  match engine __ocaml_lex_tables 33 lexbuf with
      | 0 ->
# 158 "parser/wlexer.mll"
      ( let c = Lexing.lexeme lexbuf in
	if c = ender then ()
	else (store_char (Lexing.lexeme lexbuf); 
	      string ender engine lexbuf) )
# 347 "parser/wlexer.ml"

  | 1 ->
# 163 "parser/wlexer.mll"
      ( store_ascii (Lexing.lexeme_char lexbuf 1);
        string ender engine lexbuf )
# 353 "parser/wlexer.ml"

  | 2 ->
# 166 "parser/wlexer.mll"
      ( let c = Lexing.lexeme_char lexbuf 1 in
	if c = 'x' 
	then parse_hexa_char engine lexbuf 
	else store_special c;
	string ender engine lexbuf )
# 362 "parser/wlexer.ml"

  | 3 ->
# 172 "parser/wlexer.mll"
      ( store_code (decimal_char (Lexing.lexeme lexbuf));
        string ender engine lexbuf )
# 368 "parser/wlexer.ml"

  | 4 ->
# 175 "parser/wlexer.mll"
      ( error 
	  (lexeme_start lexbuf) (lexeme_end lexbuf)
	  (Illegal_character '\\') )
# 375 "parser/wlexer.ml"

  | 5 ->
# 179 "parser/wlexer.mll"
      ( error !string_start_pos (!string_start_pos+1) Unterminated_string )
# 380 "parser/wlexer.ml"

  | 6 ->
# 181 "parser/wlexer.mll"
      ( store_code (Char.code (Lexing.lexeme_char lexbuf 0));  
	(* Adapt when source is UTF8 *)
        string ender engine lexbuf )
# 387 "parser/wlexer.ml"

  | _ -> failwith "lexing: empty token [string]"

and parse_hexa_char engine lexbuf = 
  match engine __ocaml_lex_tables 37 lexbuf with
      | 0 ->
# 187 "parser/wlexer.mll"
      ( store_code (hexa_char (Lexing.lexeme lexbuf)) )
# 396 "parser/wlexer.ml"

  | 1 ->
# 189 "parser/wlexer.mll"
      ( error 
	  (lexeme_start lexbuf) (lexeme_end lexbuf)
	  (Illegal_character '\\') )
# 403 "parser/wlexer.ml"

  | _ -> failwith "lexing: empty token [parse_hexa_char]"

;;

# 195 "parser/wlexer.mll"
 

  let delta_loc = ref 0
  let set_delta_loc dl = delta_loc := dl

(* For synchronization on errors in the toplevel ... *)
(* Issue: file inclusion *)
  let lexbuf = ref None
  let last_tok = ref ("","")



  let lexer_func_of_wlex lexfun lexengine cs =
    let dl = !delta_loc in
    delta_loc := 0;
    let lb =
      Lexing.from_function
	(fun s n ->
           try s.[0] <- Stream.next cs; 1 
	   with Stream.Failure -> 0)
    in
    lexbuf := Some lb;
    let next () =
      let tok = lexfun lexengine lb in
      let loc = (lexeme_start lb + dl, lexeme_end lb + dl) in
      last_tok := tok;
      (tok, loc) 
    in
    Token.make_stream_and_location next
      
  let register_kw (s1,s2) =
    if s1 = "" then 
      match s2.[0] with 
	| 'a' .. 'z' when not (Hashtbl.mem keywords s2) -> 
	    Hashtbl.add keywords s2 ()      
	| _ -> ()


  let lexer lexfun lexengine =
    { 
      Token.tok_func = lexer_func_of_wlex lexfun lexengine; 
      Token.tok_using = register_kw;
      Token.tok_removing = (fun _ -> ()); 
      Token.tok_match = Token.default_match;
      Token.tok_text = Token.lexer_text;
      Token.tok_comm = None;
    }

  let classes = 
    let c i = (i,i) in
    let i ch1 ch2 = (Char.code ch1, Char.code ch2) in
    [ unicode_base_char,
      [ 0x00C0,0x00D6; 0x00D8,0x00F6; 
	0x00F8,0x00FF; 0x0100,0x0131; 0x0134,0x013E; 0x0141,0x0148; 
	0x014A,0x017E; 0x0180,0x01C3; 0x01CD,0x01F0; 0x01F4,0x01F5; 
	0x01FA,0x0217; 0x0250,0x02A8; 0x02BB,0x02C1; 0x0386,0x0386;
	0x0388,0x038A; 0x038C,0x038C; 0x038E,0x03A1; 0x03A3,0x03CE; 
	0x03D0,0x03D6; 0x03DA,0x03DA; 0x03DC,0x03DC; 0x03DE,0x03DE; 
	0x03E0,0x03E0; 0x03E2,0x03F3; 
	0x0401,0x040C; 0x040E,0x044F; 0x0451,0x045C; 0x045E,0x0481; 
	0x0490,0x04C4; 0x04C7,0x04C8; 0x04CB,0x04CC; 0x04D0,0x04EB; 
	0x04EE,0x04F5; 0x04F8,0x04F9; 0x0531,0x0556; 0x0559,0x0559;
	0x0561,0x0586; 0x05D0,0x05EA; 0x05F0,0x05F2; 0x0621,0x063A; 
	0x0641,0x064A; 0x0671,0x06B7; 0x06BA,0x06BE; 0x06C0,0x06CE; 
	0x06D0,0x06D3; 0x06D5,0x06D5; 0x06E5,0x06E6; 0x0905,0x0939; 
	0x093D,0x093D;
	0x0958,0x0961; 0x0985,0x098C; 0x098F,0x0990; 0x0993,0x09A8; 
	0x09AA,0x09B0; 0x09B2,0x09B2; 0x09B6,0x09B9; 0x09DC,0x09DD; 
	0x09DF,0x09E1; 0x09F0,0x09F1; 0x0A05,0x0A0A; 0x0A0F,0x0A10; 
	0x0A13,0x0A28; 0x0A2A,0x0A30; 0x0A32,0x0A33; 0x0A35,0x0A36; 
	0x0A38,0x0A39; 0x0A59,0x0A5C; 0x0A5E,0x0A5E; 0x0A72,0x0A74; 
	0x0A85,0x0A8B; 0x0A8D,0x0A8D; 0x0A8F,0x0A91; 0x0A93,0x0AA8; 
	0x0AAA,0x0AB0; 0x0AB2,0x0AB3; 0x0AB5,0x0AB9; 0x0ABD,0x0ABD; 
	0x0AE0,0x0AE0;
	0x0B05,0x0B0C; 0x0B0F,0x0B10; 0x0B13,0x0B28; 0x0B2A,0x0B30; 
	0x0B32,0x0B33; 0x0B36,0x0B39; 0x0B3D,0x0B3D; 0x0B5C,0x0B5D; 
	0x0B5F,0x0B61; 0x0B85,0x0B8A; 0x0B8E,0x0B90; 0x0B92,0x0B95; 
	0x0B99,0x0B9A; 0x0B9C,0x0B9C; 0x0B9E,0x0B9F; 0x0BA3,0x0BA4; 
	0x0BA8,0x0BAA; 0x0BAE,0x0BB5; 0x0BB7,0x0BB9; 0x0C05,0x0C0C; 
	0x0C0E,0x0C10; 0x0C12,0x0C28; 0x0C2A,0x0C33; 0x0C35,0x0C39; 
	0x0C60,0x0C61; 0x0C85,0x0C8C; 0x0C8E,0x0C90; 0x0C92,0x0CA8; 
	0x0CAA,0x0CB3; 0x0CB5,0x0CB9; 0x0CDE,0x0CDE; 0x0CE0,0x0CE1; 
	0x0D05,0x0D0C; 0x0D0E,0x0D10; 0x0D12,0x0D28; 0x0D2A,0x0D39; 
	0x0D60,0x0D61; 0x0E01,0x0E2E; 0x0E30,0x0E30; 0x0E32,0x0E33; 
	0x0E40,0x0E45; 0x0E81,0x0E82; 0x0E84,0x0E84; 0x0E87,0x0E88; 
	0x0E8A,0x0E8A;
	0x0E8D,0x0E8D; 0x0E94,0x0E97; 0x0E99,0x0E9F; 0x0EA1,0x0EA3; 
	0x0EA5,0x0EA5;
	0x0EA7,0x0EA7; 0x0EAA,0x0EAB; 0x0EAD,0x0EAE; 0x0EB0,0x0EB0; 
	0x0EB2,0x0EB3;
	0x0EBD,0x0EBD; 0x0EC0,0x0EC4; 0x0F40,0x0F47; 0x0F49,0x0F69; 
	0x10A0,0x10C5; 0x10D0,0x10F6; 0x1100,0x1100; 0x1102,0x1103; 
	0x1105,0x1107; 0x1109,0x1109; 0x110B,0x110C; 0x110E,0x1112; 
	0x113C,0x113C;
	0x113E,0x113E; 0x1140,0x1140; 0x114C,0x114C; 0x114E,0x114E; 
	0x1150,0x1150; 0x1154,0x1155; 0x1159,0x1159;
	0x115F,0x1161; 0x1163,0x1163; 0x1165,0x1165; 0x1167,0x1167; 
	0x1169,0x1169; 0x116D,0x116E; 
	0x1172,0x1173; 0x1175,0x1175; 0x119E,0x119E; 0x11A8,0x11A8; 
	0x11AB,0x11AB; 0x11AE,0x11AF; 
	0x11B7,0x11B8; 0x11BA,0x11BA; 0x11BC,0x11C2; 0x11EB,0x11EB; 
	0x11F0,0x11F0; 0x11F9,0x11F9;
	0x1E00,0x1E9B; 0x1EA0,0x1EF9; 0x1F00,0x1F15; 0x1F18,0x1F1D; 
	0x1F20,0x1F45; 0x1F48,0x1F4D; 0x1F50,0x1F57; 0x1F59,0x1F59; 
	0x1F5B,0x1F5B;
	0x1F5D,0x1F5D; 0x1F5F,0x1F7D; 0x1F80,0x1FB4; 0x1FB6,0x1FBC; 
	0x1FBE,0x1FBE;
	0x1FC2,0x1FC4; 0x1FC6,0x1FCC; 0x1FD0,0x1FD3; 0x1FD6,0x1FDB; 
	0x1FE0,0x1FEC; 0x1FF2,0x1FF4; 0x1FF6,0x1FFC; 0x2126,0x2126;
	0x212A,0x212B; 0x212E,0x212E; 0x2180,0x2182; 0x3041,0x3094; 
	0x30A1,0x30FA; 0x3105,0x312C; (* 0xAC00,0xD7A3 *) ];

      unicode_ideographic,
      [ 0x3007,0x3007; 0x3021,0x3029 (* 0x4E00-0x9FA5 *) ];

      unicode_combining_char,
      [ 0x0300,0x0345; 0x0360,0x0361; 0x0483,0x0486; 0x0591,0x05A1;
	0x05A3,0x05B9; 0x05BB,0x05BD; 0x05BF,0x05BF; 0x05C1,0x05C2;
	0x05C4,0x05C4; 0x064B,0x0652; 0x0670,0x0670; 0x06D6,0x06DC;
	0x06DD,0x06DF; 0x06E0,0x06E4; 0x06E7,0x06E8; 0x06EA,0x06ED;
	0x0901,0x0903; 0x093C,0x093C; 0x093E,0x094C; 0x094D,0x094D;
	0x0951,0x0954; 0x0962,0x0963; 0x0981,0x0983; 0x09BC,0x09BC;
	0x09BE,0x09BE; 0x09BF,0x09BF; 0x09C0,0x09C4; 0x09C7,0x09C8;
	0x09CB,0x09CD; 0x09D7,0x09D7; 0x09E2,0x09E3; 0x0A02,0x0A02;
	0x0A3C,0x0A3C; 0x0A3E,0x0A3E; 0x0A3F,0x0A3F; 0x0A40,0x0A42;
	0x0A47,0x0A48; 0x0A4B,0x0A4D; 0x0A70,0x0A71; 0x0A81,0x0A83;
	0x0ABC,0x0ABC; 0x0ABE,0x0AC5; 0x0AC7,0x0AC9; 0x0ACB,0x0ACD;
	0x0B01,0x0B03; 0x0B3C,0x0B3C; 0x0B3E,0x0B43; 0x0B47,0x0B48;
	0x0B4B,0x0B4D; 0x0B56,0x0B57; 0x0B82,0x0B83; 0x0BBE,0x0BC2;
	0x0BC6,0x0BC8; 0x0BCA,0x0BCD; 0x0BD7,0x0BD7; 0x0C01,0x0C03;
	0x0C3E,0x0C44; 0x0C46,0x0C48; 0x0C4A,0x0C4D; 0x0C55,0x0C56;
	0x0C82,0x0C83; 0x0CBE,0x0CC4; 0x0CC6,0x0CC8; 0x0CCA,0x0CCD;
	0x0CD5,0x0CD6; 0x0D02,0x0D03; 0x0D3E,0x0D43; 0x0D46,0x0D48;
	0x0D4A,0x0D4D; 0x0D57,0x0D57; 0x0E31,0x0E31; 0x0E34,0x0E3A;
	0x0E47,0x0E4E; 0x0EB1,0x0EB1; 0x0EB4,0x0EB9; 0x0EBB,0x0EBC;
	0x0EC8,0x0ECD; 0x0F18,0x0F19; 0x0F35,0x0F35; 0x0F37,0x0F37;
	0x0F39,0x0F39; 0x0F3E,0x0F3E; 0x0F3F,0x0F3F; 0x0F71,0x0F84;
	0x0F86,0x0F8B; 0x0F90,0x0F95; 0x0F97,0x0F97; 0x0F99,0x0FAD;
	0x0FB1,0x0FB7; 0x0FB9,0x0FB9; 0x20D0,0x20DC; 0x20E1,0x20E1;
	0x302A,0x302F; 0x3099,0x3099; 0x309A,0x309A ];

      unicode_digit,
      [ 0x0660,0x0669; 0x06F0,0x06F9; 0x0966,0x096F; 0x09E6,0x09EF;
	0x0A66,0x0A6F; 0x0AE6,0x0AEF; 0x0B66,0x0B6F; 0x0BE7,0x0BEF;
	0x0C66,0x0C6F; 0x0CE6,0x0CEF; 0x0D66,0x0D6F; 0x0E50,0x0E59;
	0x0ED0,0x0ED9; 0x0F20,0x0F29 ];


      unicode_extender,
      [ 0x00B7,0x00B7; 0x02D0,0x02D1; 0x0387,0x0387; 0x0640,0x0640;
	0x0E46,0x0E46; 0x0EC6,0x0EC6; 0x3005,0x3005; 0x3031,0x3035;
	0x309D,0x309E; 0x30FC,0x30FE ];

      ascii_digit, 
      [ i '0' '9'];

      lowercase, 
      [i 'a' 'z'];

      uppercase, 
      [i 'A' 'Z'];
      
      blank, 
      [c 8; c 9; c 10; c 13; c 32]
    ]

  let table =
    assert(nb_classes <= 256);
    let v = String.make 0x312d (Char.chr encoding_error) in
    let fill_int c (i, j) = String.fill v i (j-i+1) c in
    let fill_class (c, l) = List.iter (fill_int (Char.chr c)) l in
    let fill_char  (ch, cl) = v.[ch] <- Char.chr cl in
    List.iter fill_class classes;
    List.iter fill_char one_char_classes;
    v

  let utf8_engine = 
    Lex_engines.engine_tiny_utf8 table
      (fun code -> 
	 if code >= 0x4E00 && code <= 0x9FA5 then
           unicode_ideographic
	 else if code >= 0xAC00 && code <= 0xD7A3 then
           unicode_base_char
	 else if code <= 0xD7FF || (code >= 0xE000 && code <= 0xFFFD) ||
           (code >= 0x10000 && code <= 0x10FFFF) then
             xml_char
	 else encoding_error)

  let latin1_engine = Lex_engines.engine_tiny_8bit table

# 600 "parser/wlexer.ml"
