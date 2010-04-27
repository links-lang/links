%{
open Value

let ensure_match (start, finish, _) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (Sugartypes.ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
                                     (start, finish, None)))

let pos () : Sugartypes.position = Parsing.symbol_start_pos (), Parsing.symbol_end_pos (), None

%}

%token END
%token EQ
%token LQUOTE RQUOTE
%token <string> STRING CDATA
%token <string> VARIABLE
%token <string> LXML ENDTAG
%token RXML SLASHRXML

%start xml

%type <Value.xmlitem> xml

%%

/* XML */
xml:
| xml_tree                                                     { $1 }

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
| LXML RXML ENDTAG                                             { ensure_match (pos()) $1 $3 (Node ($1, [])) }
| LXML RXML xml_contents_list ENDTAG                           { ensure_match (pos()) $1 $4 (Node ($1, $3)) } 
| LXML attrs RXML ENDTAG                                       { ensure_match (pos()) $1 $4 (Node ($1, $2)) } 
| LXML attrs SLASHRXML                                         { Node ($1, $2) }
| LXML attrs RXML xml_contents_list ENDTAG                     { ensure_match (pos()) $1 $5 (Node ($1, $2 @ $4)) } 

xml_contents_list:
| xml_contents                                                 { [$1] }
| xml_contents xml_contents_list                               { $1 :: $2 }

xml_contents:
| xml_tree                                                     { $1 }
| CDATA                                                        { Text $1 }
