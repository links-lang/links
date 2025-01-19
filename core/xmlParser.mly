(* See Note [Debugging grammar conflicts] in parser.mly *)

%{
open Utility
open Value

let pos (start, finish) : SourceCode.Position.t =
  SourceCode.Position.make ~start ~finish ~code:None

let ensure_match p (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (Sugartypes.ConcreteSyntaxError (pos p,
       Printf.sprintf "Closing tag '%s' does not match start tag '%s'."
         closing opening))

%}

%token IGNORE END
%token EQ
%token LQUOTE RQUOTE
%token <string> STRING CDATA
%token <string> VARIABLE
%token <string> LXML ENDTAG
%token RXML SLASHRXML
%token LCDATA RCDATA
%token <char> CHAR

%start xml

%type <Value.xmlitem> xml

%%

/* XML */
xml:
| IGNORE xml                                                   { $2 }
| xml_tree END                                                 { $1 }

xmlid:
| VARIABLE                                                     { $1 }

attrs:
| attr_list                                                    { $1 }

attr_list:
| attr                                                         { [$1] }
| attr_list attr                                               { $2 :: $1 }

attr:
| xmlid EQ LQUOTE attr_val RQUOTE                              { Attr ($1, $4) }
| xmlid EQ LQUOTE RQUOTE                                       { Attr ($1, "") }

attr_val:
| STRING                                                       { $1 }

xml_tree:
| LXML SLASHRXML                                               { Node ($1, []) }
| LXML RXML ENDTAG                                             { ensure_match $loc $1 $3 (Node ($1, [])) }
| LXML RXML xml_contents_list ENDTAG                           { ensure_match $loc $1 $4 (Node ($1, $3)) }
| LXML attrs RXML ENDTAG                                       { ensure_match $loc $1 $4 (Node ($1, $2)) }
| LXML attrs SLASHRXML                                         { Node ($1, $2) }
| LXML attrs RXML xml_contents_list ENDTAG                     { ensure_match $loc $1 $5 (Node ($1, $2 @ $4)) }

xml_contents_list:
| IGNORE                                                       { [] }
| IGNORE xml_contents_list                                     { $2 }
| xml_contents                                                 { [$1] }
| xml_contents xml_contents_list                               { $1 :: $2 }

xml_contents:
| xml_tree                                                     { $1 }
| cdata                                                        { Text $1 }

cdata:
| CDATA                                                        { $1 }
| LCDATA chars RCDATA                                          { implode $2 }

chars:
|                                                              { [] }
| CHAR chars                                                   { $1 :: $2 }
