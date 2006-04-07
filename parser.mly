%{

open Sugar

let ensure_match (start, finish) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (ParseError ("Closing tag " ^ closing ^ " does not match start tag " ^ opening,
			    (start, finish)))

let pos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

%}

%token END
%token EQ IN 
%token FUN RARROW VAR
%token IF ELSE
%token EQEQ LESS LESSEQUAL MORE MOREEQUAL DIFFERENT BEGINSWITH
%token PLUS MINUS STAR SLASH PLUSDOT MINUSDOT STARDOT SLASHDOT
%token PLUSPLUS HATHAT HAT
%token SWITCH RECEIVE
%token LPAREN RPAREN
%token LBRACE RBRACE LQUOTE RQUOTE
%token LSET LBAG RBRACKET LBRACKET
%token SORT_UP SORT_DOWN
%token FOR LARROW HANDLE WHERE 
%token AMPER COMMA VBAR DOT COLON
%token TABLE FROM DATABASE WITH UNIQUE ORDER ASC DESC UPDATE DELETE INSERT BY VALUES INTO
%token ESCAPE
%token CLIENT SERVER NAMESPACE
%token SEMICOLON
%token TRUE FALSE
%token BARBAR AMPAMP BANG
%token <Num.num> UINTEGER
%token <float> UFLOAT 
%token <string> STRING CDATA
%token <char> CHAR
%token <string> VARIABLE CONSTRUCTOR
%token <string> LXML ENDTAG
%token RXML SLASHRXML
%token <int> TVARIABLE
%token TINT TFLOAT TBOOL TSTRING

%right RARROW
%start parse_links

%type <Sugar.phrase list> parse_links
%type <Sugar.phrase> xml_tree

%%

parse_links:
| toplevel_seq END                                             { $1 }

toplevel_seq:
| toplevel toplevel_seq                                        { $1 :: $2 }
| toplevel                                                     { [$1] }

toplevel:
| exp SEMICOLON                                                { $1 }
| namespace_decl                                               { $1 }
| VARIABLE perhaps_location EQ exp SEMICOLON                   { Definition ($1, $4, $2), pos() }
| VAR VARIABLE perhaps_location EQ exp SEMICOLON               { Definition ($2, $5, $3), pos() }
| FUN VARIABLE arg_list perhaps_location block perhaps_semi    { Definition ($2, (FunLit (Some $2, $3, $5), pos()), $4), pos() }
      
namespace_decl:
| NAMESPACE VARIABLE EQ STRING SEMICOLON                       { NamespaceDecl ($2, $4), pos() }

perhaps_location:
| SERVER                                                       { `Server }
| CLIENT                                                       { `Client }
| /* empty */                                                  { `Unknown }

constant:
| UINTEGER                                                     { IntLit $1    , pos() }
| UFLOAT                                                       { FloatLit $1  , pos() }
| STRING                                                       { StringLit $1 , pos() }
| TRUE                                                         { BoolLit true , pos() }
| FALSE                                                        { BoolLit false, pos() }
| CHAR                                                         { CharLit $1   , pos() }

primary_expression:
| VARIABLE                                                     { Var $1, pos() }
| constant                                                     { $1 }
| LSET RBRACKET                                                { CollectionLit (`Set, []), pos() } 
| LSET exps RBRACKET                                           { CollectionLit (`Set, $2), pos() }
| LBAG RBRACKET                                                { CollectionLit (`Bag, []), pos() } 
| LBAG exps RBRACKET                                           { CollectionLit (`Bag, $2), pos() } 
| LBRACKET RBRACKET                                            { CollectionLit (`List, []), pos() } 
| LBRACKET exps RBRACKET                                       { CollectionLit (`List, $2), pos() } 
| SORT_UP LPAREN exp RPAREN                                    { SortExp (true, $3), pos() }
| SORT_DOWN LPAREN exp RPAREN                                  { SortExp (false, $3), pos() }
| xml                                                          { $1 }
| parenthesized_thing                                          { $1 }

constructor_expression:
| CONSTRUCTOR                                                  { ConstructorLit($1, None), pos() }
| CONSTRUCTOR primary_expression                               { ConstructorLit($1, Some $2), pos() }
| CONSTRUCTOR constructor_expression                           { ConstructorLit($1, Some $2), pos() }


parenthesized_thing:
| LPAREN binop RPAREN                                          { Section $2, pos() }
| LPAREN DOT VARIABLE RPAREN                                   { Section (`Project $3), pos() }
| LPAREN DOT UINTEGER RPAREN                                   { Section (`Project (Num.string_of_num $3)), pos() }
| LPAREN RPAREN                                                { RecordLit ([], None), pos() }
| LPAREN labeled_exps VBAR exp RPAREN                          { RecordLit ($2, Some $4), pos() }
| LPAREN labeled_exps RPAREN                                   { RecordLit ($2, None),               pos() }
| LPAREN exps RPAREN                                           { TupleLit ($2), pos() }


binop:
| STAR                                                         { `Times }
| SLASH                                                        { `Div }
| HAT                                                          { `Exp }
| PLUS                                                         { `Plus }
| MINUS                                                        { `Minus }
| STARDOT                                                      { `FloatTimes }
| SLASHDOT                                                     { `FloatDiv }
| HATHAT                                                       { `FloatExp }
| PLUSDOT                                                      { `FloatPlus }
| MINUSDOT                                                     { `FloatMinus }

postfix_expression:
| primary_expression                                           { $1 }
| block                                                        { $1 }
| postfix_expression LPAREN RPAREN                             { FnAppl ($1, []), pos() }
| postfix_expression LPAREN exps RPAREN                        { FnAppl ($1, $3), pos() }
/*| postfix_expression LPAREN labeled_exps RPAREN              { FnAppl ($1, $3), pos() }*/
| postfix_expression DOT record_label                          { Projection ($1, $3), pos() }

exps:
| exp COMMA exps                                               { $1 :: $3 }
| exp                                                          { [$1] }

unary_expression:
| MINUS unary_expression                                       { UnaryAppl (`Minus,      $2), pos() }
| MINUSDOT unary_expression                                    { UnaryAppl (`FloatMinus, $2), pos() }
| postfix_expression                                           { $1 }
| constructor_expression                                       { $1 }

exponentiation_expression:
| unary_expression                                             { $1 }
| exponentiation_expression HAT    postfix_expression          { InfixAppl (`Exp,      $1, $3), pos() }
| exponentiation_expression HATHAT postfix_expression          { InfixAppl (`FloatExp, $1, $3), pos() }

multiplicative_expression:
| exponentiation_expression                                    { $1 }
| multiplicative_expression STAR exponentiation_expression     { InfixAppl (`Times, $1, $3), pos() }
| multiplicative_expression SLASH   exponentiation_expression  { InfixAppl (`Div, $1, $3), pos() }
| multiplicative_expression STARDOT  exponentiation_expression { InfixAppl (`FloatTimes, $1, $3), pos() }
| multiplicative_expression SLASHDOT exponentiation_expression { InfixAppl (`FloatDiv, $1, $3), pos() }

addition_expression: 
| multiplicative_expression                                    { $1 }
| addition_expression PLUS  multiplicative_expression          { InfixAppl (`Plus, $1, $3), pos() }
| addition_expression MINUS multiplicative_expression          { InfixAppl (`Minus, $1, $3), pos() }
| addition_expression PLUSDOT   multiplicative_expression      { InfixAppl (`FloatPlus, $1, $3), pos() }
| addition_expression MINUSDOT  multiplicative_expression      { InfixAppl (`FloatMinus, $1, $3), pos() }

comparison_expression:
| addition_expression                                          { $1 }
| comparison_expression EQEQ      addition_expression          { InfixAppl (`Eq, $1, $3), pos() }
| comparison_expression LESS      addition_expression          { InfixAppl (`Less, $1, $3), pos() }
| comparison_expression LESSEQUAL addition_expression          { InfixAppl (`LessEq, $1, $3), pos() }
| comparison_expression MORE      addition_expression          { InfixAppl (`Greater, $1, $3), pos() }
| comparison_expression MOREEQUAL addition_expression          { InfixAppl (`GreaterEq, $1, $3), pos() }
| comparison_expression DIFFERENT addition_expression          { InfixAppl (`NotEq, $1, $3), pos() }
| comparison_expression BEGINSWITH addition_expression         { InfixAppl (`BeginsWith, $1, $3), pos() }

logical_expression:
| comparison_expression                                        { $1 }
| logical_expression BARBAR comparison_expression              { InfixAppl (`Or, $1, $3), pos() }
| logical_expression AMPAMP comparison_expression              { InfixAppl (`And, $1, $3), pos() }
/*| NOT addition_expression                                    { UnaryAppl (`Not, $2), pos() }*/

send_expression:
| logical_expression                                           { $1 }
| logical_expression BANG logical_expression                   { Send ($1, $3), pos() }


union_expression:
| send_expression                                              { $1 }
| union_expression PLUSPLUS logical_expression                 { InfixAppl (`Concat, $1, $3), pos() }

db_expression:
| union_expression                                             { $1 }
| UPDATE LPAREN STRING COMMA exp RPAREN BY exp                 { DBUpdate ($3, $5, $8), pos() }
| DELETE FROM LPAREN STRING COMMA exp RPAREN VALUES exp        { DBDelete ($4, $6, $9), pos() }
| INSERT INTO LPAREN STRING COMMA exp RPAREN VALUES exp        { DBInsert ($4, $6, $9), pos() }
| DATABASE STRING                                              { DatabaseLit $2, pos() }

xml:
| xml_forest                                                   { XmlForest $1, pos() }

/* XML */
xml_forest:
| xml_tree                                                     { [$1] }
| xml_tree xml_forest                                          { $1 :: $2 }

xmlid: 
| VARIABLE                                                     { $1 }

attr_list:
| attr                                                         { [$1] }
| attr_list attr                                               { $2 :: $1 }

attr:
| xmlid EQ LQUOTE attr_val RQUOTE                              { ($1, $4) }
| xmlid EQ LQUOTE RQUOTE                                       { ($1, [StringLit "", pos()]) }

attr_val:
| block                                                        { [$1] }
| STRING                                                       { [StringLit $1, pos()] }
| block attr_val                                               { $1 :: $2 }
| STRING attr_val                                              { (StringLit $1, pos()) :: $2}

xml_tree:
| LXML SLASHRXML                                               { Xml ($1, [], []), pos() } 
| LXML RXML ENDTAG                                             { ensure_match (pos()) $1 $3 (Xml ($1, [], []), pos()) } 
| LXML RXML xml_contents_list ENDTAG                           { ensure_match (pos()) $1 $4 (Xml ($1, [], $3), pos()) } 
| LXML attr_list RXML ENDTAG                                   { ensure_match (pos()) $1 $4 (Xml ($1, $2, []), pos()) } 
| LXML attr_list SLASHRXML                                     { Xml ($1, $2, []), pos() } 
| LXML attr_list RXML xml_contents_list ENDTAG                 { ensure_match (pos()) $1 $5 (Xml ($1, $2, $4), pos()) } 

xml_contents_list:
| xml_contents                                                 { [$1] }
| xml_contents xml_contents_list                               { $1 :: $2 }

xml_contents:
| block                                                        { $1 }
| xml_tree                                                     { $1 }
| CDATA                                                        { TextNode (Utility.xml_unescape $1), pos() }

conditional_expression:
| db_expression                                                { $1 }
| IF LPAREN exp RPAREN exp ELSE exp                            { Conditional ($3, $5, $7), pos() }

cases:
| case                                                         { [$1] }
| case cases                                                   { $1 :: $2 }

case:
| CONSTRUCTOR patt  RARROW exp                                 { $1, $2, $4 }
| CONSTRUCTOR       RARROW exp                                 { $1, Pattern (RecordLit ([], None), pos ()), $3 }

case_expression:
| conditional_expression                                       { $1 }
| SWITCH exp LBRACE cases default_case RBRACE                  { Switch ($2, $4, Some $5), pos() }
| SWITCH exp LBRACE cases RBRACE                               { Switch ($2, $4, None),    pos() }
| RECEIVE LBRACE cases default_case RBRACE                     { Receive ($3, Some $4), pos() }
| RECEIVE LBRACE cases RBRACE                                  { Receive ($3, None),    pos() }

default_case :
| VARIABLE RARROW exp                                          { ($1, $3) }

table_expression:
| case_expression                                              { $1 }
| TABLE STRING WITH kind unique perhaps_order FROM exp         { TableLit ($2, $4, $5, $6, $8), pos() }

iteration_expression:
| table_expression                                             { $1 }
| FOR provider exp                                             { Iteration (fst $2, snd $2, $3, None),    pos() }
| FOR provider WHERE LPAREN exp RPAREN exp                     { Iteration (fst $2, snd $2, $7, Some $5), pos() }

escape_expression:
| iteration_expression                                         { $1 }
| ESCAPE VARIABLE IN postfix_expression                        { Escape ($2, $4), pos() }

lambda_expression:
| escape_expression                                            { $1 }
| HANDLE exp WITH VARIABLE RARROW exp                          { HandleWith ($2, $4, $6), pos() }
| FUN arg_list block                                           { FunLit (None, $2, $3), pos() }

arg_list:
| parenthesized_pattern                                        { [$1] }
| parenthesized_pattern arg_list                               { $1 :: $2 }

parenthesized_pattern:
| parenthesized_thing                                          { Pattern $1 }

binding:
| VAR patt EQ exp SEMICOLON                                    { Binding ($2, $4), pos() }
| patt EQ exp SEMICOLON                                        { Binding ($1, $3), pos() }
| exp SEMICOLON                                                { $1 }
| FUN VARIABLE arg_list block                                  { FunLit (Some $2, $3, $4), pos() }

bindings:
| binding                                                      { [$1] }
| bindings binding                                             { $1 @ [$2] }

amper_expression:
| lambda_expression                                            { $1 }
/*| LPAREN exp AMPER VARIABLE RPAREN                           { amper (fst $4) $2 $1 }*/

block:
| LBRACE bindings exp perhaps_semi RBRACE                      { Block ($2, $3), pos() }
| LBRACE exp perhaps_semi RBRACE                               { $2 }
| LBRACE perhaps_semi RBRACE                                   { Block ([], (TupleLit [], pos())), pos() }

perhaps_semi:
| SEMICOLON                                                    {}
|                                                              {}

exp:
| amper_expression                                             { ($1 : Sugar.phrase) }

unique:
| UNIQUE                                                       { true }
|                                                              { false }

perhaps_order:
| ORDER LBRACKET orders RBRACKET                               { $3 }
| ORDER LBRACKET RBRACKET                                      { [] }
|                                                              { [] }

orders:
| VARIABLE COLON ASC COMMA orders                              { (`Asc $1) :: $5 }
| VARIABLE COLON DESC COMMA orders                             { (`Desc $1) :: $5 }
| VARIABLE COLON ASC                                           { [`Asc $1] }
| VARIABLE COLON DESC                                          { [`Desc $1] }


labeled_exps:
| record_label EQ exp                                          { [$1, $3] }
| record_label EQ exp COMMA labeled_exps                       { ($1, $3) :: $5 }

record_label:
| VARIABLE                                                     { $1 } 
| UINTEGER                                                     { Num.string_of_num $1 }

provider:
| LPAREN patt LARROW exp RPAREN                                { $2, $4 }

patt:
| exp                                                          { Pattern $1 }

kind:
| LPAREN kind RPAREN                                           { $2 }
| TBOOL                                                        { `Primitive `Bool }
| TINT                                                         { `Primitive `Int }
| TFLOAT                                                       { `Primitive `Float }
| TSTRING                                                      { `Collection (`List, `Primitive `Char) }
| TVARIABLE                                                    { `TypeVar $1 }
| kind RARROW kind                                             { `Function ($1, $3) }
| LBRACE labeled_kinds RBRACE                                  { `Record $2 }
/*| LANG labeled_kinds RANG                                      { `Variant $2 }*/
| LSET kind RBRACKET                                           { `Collection (`Set, $2) }
| LBAG kind RBRACKET                                           { `Collection (`Bag, $2) }
| LBRACKET kind RBRACKET                                       { `Collection (`List, $2) }

field:
| VARIABLE COLON kind                                          { ($1, `Present $3) }
| VARIABLE COLON MINUS                                         { ($1, `Absent) }

labeled_kinds:
| field COMMA labeled_kinds                                    { Kind.TypeOps.set_field $1 $3 }
| field                                                        { Kind.TypeOps.make_singleton_closed_row $1 }
| TVARIABLE                                                    { Kind.TypeOps.make_empty_open_row_with_var $1 }
