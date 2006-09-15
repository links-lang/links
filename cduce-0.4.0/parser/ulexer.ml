(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module L = Ulexing

let keywords : (string,unit) Hashtbl.t = Hashtbl.create 17

exception Error of int * int * string

let error i j s = raise (Error (i,j,s))

(* Buffer for string literals *)
  
let string_buff = Buffer.create 1024

let store_lexeme lexbuf = 
  Buffer.add_string string_buff (Ulexing.utf8_lexeme lexbuf)
let store_ascii = Buffer.add_char string_buff
let store_code  = Utf8.store string_buff
let clear_buff () = Buffer.clear string_buff
let get_stored_string () =
  let s = Buffer.contents string_buff in
  clear_buff ();
  Buffer.clear string_buff;
  s

let enc = ref L.Latin1

(* Parse characters literals \123; \x123; *)
      
let hexa_digit = function
  | '0'..'9' as c -> (Char.code c) - (Char.code '0')
  | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
  | 'A'..'F' as c -> (Char.code c) - (Char.code 'A') + 10
  | _ -> -1
      
let parse_char lexbuf base i =
  let s = L.latin1_sub_lexeme lexbuf i (L.lexeme_length lexbuf - i - 1) in 
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    let c = hexa_digit s.[i] in
    if (c >= base) || (c < 0) then 
      error (L.lexeme_start lexbuf) (L.lexeme_end lexbuf) "invalid digit";
    r := !r * base + c;
  done;
  !r


let regexp ncname_char = 
  xml_letter | xml_digit | [ '-' '_' ] | xml_combining_char | xml_extender | "\\."
let regexp ncname = (xml_letter ncname_char*) | ('_' ncname_char+)
let regexp qname = (ncname ':')? ncname


let illegal lexbuf =
  error
    (L.lexeme_start lexbuf)
    (L.lexeme_end lexbuf) 
    "Illegal character"

let in_comment = ref false

let return lexbuf tok = (tok, L.loc lexbuf)
let return_loc i j tok = (tok, (i,j))

let rec token = lexer 
 | xml_blank+ -> token lexbuf
 | qname ->
     let s = L.utf8_lexeme lexbuf in
     return lexbuf (if Hashtbl.mem keywords s then "",s else "IDENT",s)
 | ncname ":*" ->
     let s = L.utf8_sub_lexeme lexbuf 0 (L.lexeme_length lexbuf - 2) in
     return lexbuf ("ANY_IN_NS", s)
 | ".:*" -> 
     return lexbuf ("ANY_IN_NS", "")
 | '-'? ['0'-'9']+ ->
     return lexbuf ("INT", L.utf8_lexeme lexbuf)
 | [ "<>=.,:;+-*/@&{}[]()|?`!" ]
 | "->" | "::" | ";;" | "--" | "//" | "/@" | ":=" | "\\" | "++"
 | "<=" | ">=" | "<<" | ">>" | "||" | "&&" | "**" | "_"
 | ".."
 | ["?+*"] "?" | "#" ->
     return lexbuf ("", L.utf8_lexeme lexbuf)
 | '"' | "'" ->
     let start = L.lexeme_start lexbuf in
     let double_quote = L.latin1_lexeme_char lexbuf 0 = '"' in
     string (L.lexeme_start lexbuf) double_quote lexbuf;
     return_loc start (L.lexeme_end lexbuf)
       ((if double_quote then "STRING2" else "STRING1"), 
       (get_stored_string()))
 | "(*" ->
     in_comment := true;
     comment (L.lexeme_start lexbuf) lexbuf;
     in_comment := false;
     token lexbuf
 | "/*" ->
     in_comment := true;
     tcomment (L.lexeme_start lexbuf) lexbuf;
     in_comment := false;
     token lexbuf
 | eof ->       
     return lexbuf ("EOI","")
 | _ -> 
     illegal lexbuf


and comment start = lexer
  | "(*" ->
      comment (L.lexeme_start lexbuf) lexbuf;
      comment start lexbuf
  | "*)" ->
      ()
  | '"' | "'" ->
      let double_quote = L.latin1_lexeme_char lexbuf 0 = '"' in
      string (L.lexeme_start lexbuf) double_quote lexbuf;
      clear_buff ();
      comment start lexbuf
  | eof ->
      error start (start+2) "Unterminated comment"
  | _ ->
      comment start lexbuf

and tcomment start = lexer
  | "*/" ->
      ()
  | eof ->
      error start (start+2) "Unterminated comment"
  | _ ->
      tcomment start lexbuf

and string start double = lexer
  | '"' | "'" ->
      let d = L.latin1_lexeme_char lexbuf 0 = '"' in
      if d != double then (store_lexeme lexbuf; string start double lexbuf)
  | '\\' ['\\' '"' '\''] ->
      store_ascii (L.latin1_lexeme_char lexbuf 1);
      string start double lexbuf
  | "\\n" -> 
      store_ascii '\n';	string start double lexbuf
  | "\\t" -> 
      store_ascii '\t';	string start double lexbuf
  | "\\r" -> 
      store_ascii '\r';	string start double lexbuf
  | '\\' ['0'-'9']+ ';' ->
      store_code (parse_char lexbuf 10 1);
      string start double lexbuf
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F']+ ';' ->
      store_code (parse_char lexbuf 16 2);
      string start double lexbuf
  | '\\' ->
      illegal lexbuf;
  | eof ->
      error start (start+1) "Unterminated string"
  | _ ->
      store_lexeme lexbuf;
      string start double lexbuf


let lexbuf = ref None
let last_tok = ref ("","")

let raise_clean e =
  clear_buff ();
  in_comment := false;
  (* reinit encoding ? *)
  raise e

let pos_of_int i =
  { Lexing.pos_fname = "";
    Lexing.pos_lnum = 0;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = i }

let make_stream_and_location f =
  Token.make_stream_and_flocation
    (fun () ->
       let (tok,(i,j)) = f () in
       (tok, (pos_of_int i, pos_of_int j))
    )



let tok_func cs =
  let lb = L.from_var_enc_stream enc cs in
  (lexer ("#!" [^ '\n']* "\n")? -> ()) lb;
  lexbuf := Some lb;
  let next () =
    let tok = 
      try token lb
      with
	| Ulexing.Error -> 
	    raise_clean (Error (Ulexing.lexeme_end lb, Ulexing.lexeme_end lb,
			  "Unexpected character"))
	| Ulexing.InvalidCodepoint i ->
	    raise_clean (Error (Ulexing.lexeme_end lb, Ulexing.lexeme_end lb,
			  "Code point invalid for the current encoding"))
	| e -> raise_clean e
    in
    last_tok := fst tok;
    tok
  in
  make_stream_and_location next
      
let register_kw (s1,s2) =
  if s1 = "" then 
    match s2.[0] with 
      | 'a' .. 'z' when not (Hashtbl.mem keywords s2) -> 
	  Hashtbl.add keywords s2 ()      
      | _ -> ()
	  

let lex =
  { 
    Token.tok_func = tok_func;
    Token.tok_using = register_kw;
    Token.tok_removing = (fun _ -> ()); 
    Token.tok_match = Token.default_match;
    Token.tok_text = Token.lexer_text;
    Token.tok_comm = None;
  }



let dump_file f =
  let ic = open_in f in
  let lexbuf = L.from_var_enc_channel enc ic in
  (try
     while true do
       let ((a,b),_) = token lexbuf in
       Printf.printf "%s: \"%s\"\n" a b;
       if a = "EOI" then exit 0
     done
   with 
     | Ulexing.Error -> 
	 Printf.eprintf "Lexing error at offset %i\n:Unexpected character\n" 
	   (Ulexing.lexeme_end lexbuf)
     | Error (i,j,s) ->
	 Printf.eprintf "Lexing error at offset %i-%i:\n%s\n" 
	   i j s
     | Ulexing.InvalidCodepoint i ->
	 Printf.eprintf "Lexing error at offset %i\n:Invalid code point for the current encoding\n" 
	   (Ulexing.lexeme_end lexbuf)
  );
  close_in ic
