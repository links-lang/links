open Scanner

module XmlLexer : (LexerSig with type token         = XmlParser.token and
                                 type lexer_context = XmlLexer.lexer_context) =
  struct
    type token         = XmlParser.token
    type lexer_context = XmlLexer.lexer_context
    type 'a grammar    = (Lexing.lexbuf -> XmlParser.token) -> Lexing.lexbuf -> 'a
    let lexer          = XmlLexer.lexer
    let fresh_context  = XmlLexer.fresh_context
end

module XmlParse = Scanner (XmlLexer)

(* Given a string, return a function suitable for input to
   Lexing.from_function that reads characters from the string.
*)
let reader_of_string string =
  let current_pos = ref 0 in
    fun buffer nchars ->
      let nchars = min nchars (String.length string - !current_pos) in
        StringLabels.blit ~src:string ~src_pos:!current_pos ~dst:buffer ~dst_pos:0 ~len:nchars;
        current_pos := !current_pos + nchars;
        nchars

let xml : (Value.xmlitem) XmlLexer.grammar = XmlParser.xml

(* We parse in a "context", which is an environment with
   respect to which any parse-time resolution takes place.
*)
(* type context = XmlLexer.lexer_context *)
let fresh_context = XmlLexer.fresh_context

let normalize_context = function
  | None -> fresh_context ()
  | Some c -> c

let parse_string ?in_context:context grammar string =
  let context = normalize_context context in
    XmlParse.read ?nlhook:None ~parse:grammar ~infun:(reader_of_string string)
      ~name:"<string>" ~context ()

let parse_xml s = fst (Errors.display (lazy (parse_string xml s)))
