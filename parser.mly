%{

open Utility
open List
open Sugartypes

let ensure_match (start, finish) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (Sugar.ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
			                   (start, finish)))

let pos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

%}

%token END
%token EQ IN 
%token FUN RARROW VAR
%token IF ELSE
%token EQEQ LESS LESSEQUAL MORE MOREEQUAL DIFFERENT
%token PLUS MINUS STAR SLASH PLUSDOT MINUSDOT STARDOT SLASHDOT
%token PLUSPLUS HATHAT HAT
%token SWITCH RECEIVE CASE SPAWN
%token LPAREN RPAREN
%token LBRACE RBRACE LQUOTE RQUOTE
%token RBRACKET LBRACKET LBRACKETBAR BARRBRACKET
%token FOR LARROW HANDLE WHERE 
%token COMMA VBAR DOT COLON COLONCOLON
%token TABLE TABLEHANDLE FROM DATABASE WITH UNIQUE ORDERBY
%token UPDATE DELETE INSERT BY VALUES INTO
%token ESCAPE
%token CLIENT SERVER NATIVE
%token SEMICOLON
%token TRUE FALSE
%token BARBAR AMPAMP BANG
%token <Num.num> UINTEGER
%token <float> UFLOAT 
%token <string> STRING CDATA
%token <char> CHAR
%token <string> VARIABLE CONSTRUCTOR KEYWORD
%token <string> LXML ENDTAG
%token RXML SLASHRXML
%token MU ALIEN SIG
%token QUESTION TILDE
%token <char*char> RANGE
%token UNDERSCORE AS

%start parse_links
%start just_datatype
%start sentence

%type <Sugartypes.phrase list> parse_links
%type <Sugartypes.phrase> xml_tree
%type <Sugartypes.datatype> datatype
%type <Sugartypes.datatype> just_datatype
%type <Sugartypes.sentence> sentence
%type <Sugartypes.regex list> regex_pattern_sequence
%type <Sugartypes.ppattern> pattern

%%

sentence:
| parse_links                                                  { Left $1 }
| directive                                                    { Right $1 }
| SEMICOLON END                                                { Right ("quit", []) (* rather hackish *) }

directive:
| KEYWORD args SEMICOLON                                       { ($1, $2) }

args: 
|                                                              { [] }
| arg args                                                     { $1 :: $2 }

arg:
| STRING                                                       { $1 }
| VARIABLE                                                     { $1 }
| CONSTRUCTOR                                                  { $1 }
| UINTEGER                                                     { Num.string_of_num $1 }
| UFLOAT                                                       { string_of_float $1 }
| TRUE                                                         { "true" }
| FALSE                                                        { "false" }

parse_links:
| toplevel_seq END                                             { $1 }

toplevel_seq:
| toplevel toplevel_seq                                        { $1 :: $2 }
| toplevel                                                     { [$1] }

toplevel:
| exp SEMICOLON                                                { $1 }
| ALIEN VARIABLE VARIABLE COLON datatype SEMICOLON             { Foreign ($2, $3, $5), pos() }
| VAR pattern perhaps_location EQ exp SEMICOLON                { Definition ($2, $5, $3), pos() }
| SIG 
  VARIABLE COLON datatype 
  FUN VARIABLE arg_list perhaps_location block perhaps_semi    { if $2 <> $6 then 
                                                                   raise (Sugar.ConcreteSyntaxError
                                                                            ("Signature for `" ^ $2 ^ "' should precede definition of `"
                                                                             ^ $2 ^ "', not `"^ $6 ^"'.",
                                                                             pos ()));
                                                                 TypeAnnotation (
                                                                                  (Definition ((`Variable $6, pos()), (FunLit (Some $6, $7, $9), pos()), $8), pos()),
                                                                                  $4),
                                                                 pos() }
| FUN VARIABLE arg_list perhaps_location block perhaps_semi    { Definition ((`Variable $2, pos()), (FunLit (Some $2, $3, $5), pos()), $4), pos() }

perhaps_location:
| SERVER                                                       { `Server }
| CLIENT                                                       { `Client }
| NATIVE                                                       { `Native }
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
| LBRACKET RBRACKET                                            { ListLit [], pos() } 
| LBRACKET exps RBRACKET                                       { ListLit $2, pos() } 
| xml                                                          { $1 }
| parenthesized_thing                                          { $1 }
| FUN arg_list block                                           { FunLit (None, $2, $3), pos() }
| DATABASE exp                                                 { DatabaseLit $2, pos() }

constructor_expression:
| CONSTRUCTOR                                                  { ConstructorLit($1, None), pos() }
| CONSTRUCTOR parenthesized_thing                              { ConstructorLit($1, Some $2), pos() }


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
| SPAWN block                                                  { Spawn $2, pos() }
| postfix_expression arg_spec                                  { FnAppl ($1, $2), pos() }
/*| postfix_expression LPAREN labeled_exps RPAREN              { FnAppl ($1, $3), pos() }*/
| postfix_expression DOT record_label                          { Projection ($1, $3), pos() }

arg_spec:
| LPAREN RPAREN                                                { [], pos() }
| LPAREN exps RPAREN                                           { $2, pos() }

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
| exponentiation_expression HAT    unary_expression            { InfixAppl (`Exp,      $1, $3), pos() }
| exponentiation_expression HATHAT unary_expression            { InfixAppl (`FloatExp, $1, $3), pos() }

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

cons_expression:
| addition_expression                                          { $1 }
| addition_expression COLONCOLON cons_expression               { InfixAppl (`Cons, $1, $3), pos() }
| addition_expression PLUSPLUS cons_expression                 { InfixAppl (`Concat, $1, $3), pos() }

comparison_expression:
| cons_expression                                              { $1 }
| comparison_expression TILDE     regex                        { InfixAppl (`RegexMatch, $1, $3), pos() }
| comparison_expression EQEQ      cons_expression              { InfixAppl (`Eq, $1, $3), pos() }
| comparison_expression LESS      cons_expression              { InfixAppl (`Less, $1, $3), pos() }
| comparison_expression LESSEQUAL cons_expression              { InfixAppl (`LessEq, $1, $3), pos() }
| comparison_expression MORE      cons_expression              { InfixAppl (`Greater, $1, $3), pos() }
| comparison_expression MOREEQUAL cons_expression              { InfixAppl (`GreaterEq, $1, $3), pos() }
| comparison_expression DIFFERENT cons_expression              { InfixAppl (`NotEq, $1, $3), pos() }

logical_expression:
| comparison_expression                                        { $1 }
| logical_expression BARBAR comparison_expression              { InfixAppl (`Or, $1, $3), pos() }
| logical_expression AMPAMP comparison_expression              { InfixAppl (`And, $1, $3), pos() }

typed_expression:
| logical_expression                                           { $1 }
| logical_expression COLON datatype                            { TypeAnnotation ($1, $3), pos() }

send_expression:
| typed_expression                                             { $1 }
| typed_expression BANG logical_expression                     { Send ($1, $3), pos() }

db_expression:
| send_expression                                              { $1 }
| DELETE FROM LPAREN exp RPAREN VALUES exp                     { DBDelete ($4, $7), pos() }
| INSERT INTO LPAREN exp RPAREN VALUES exp                     { DBInsert ($4, $7), pos() }

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
| CASE pattern RARROW exp SEMICOLON                            { $2, $4 }

// TBD: remove `None' from Switch constructor
case_expression:
| conditional_expression                                       { $1 }
| SWITCH exp LBRACE cases RBRACE                               { Switch ($2, $4, None),    pos() }
| RECEIVE LBRACE cases RBRACE                                  { Receive ($3, None),    pos() }

iteration_expression:
| case_expression                                              { $1 }
| FOR LPAREN VAR pattern LARROW exp RPAREN
      perhaps_where
      perhaps_orderby
      exp                                                      { Iteration ($4, $6, $10, $8, $9),    pos() }

perhaps_where:
|                                                              { None }
| WHERE LPAREN exp RPAREN                                      { Some $3 }

perhaps_orderby:
|                                                              { None }
| ORDERBY LPAREN exp RPAREN                                    { Some $3 }

escape_expression:
| iteration_expression                                         { $1 }
| ESCAPE VARIABLE IN postfix_expression                        { Escape ($2, $4), pos() }

handlewith_expression:
| escape_expression                                            { $1 }
| HANDLE exp WITH VARIABLE RARROW exp                          { HandleWith ($2, $4, $6), pos() }

table_expression:
| handlewith_expression { $1 }
| TABLE STRING datatype unique FROM exp                        { TableLit ($2, $3, $4, $6), pos()} 

arg_list:
| parenthesized_pattern                                        { [$1] }
| parenthesized_pattern arg_list                               { $1 :: $2 }

binding:
| VAR pattern EQ exp SEMICOLON                                 { Binding ($2, $4), pos() }
| exp SEMICOLON                                                { $1 }
| FUN VARIABLE arg_list block                                  { FunLit (Some $2, $3, $4), pos() }

bindings:
| binding                                                      { [$1] }
| bindings binding                                             { $1 @ [$2] }

block:
| LBRACE bindings exp SEMICOLON RBRACE                         { Block ($2 @ [$3], (RecordLit ([], None), pos())), pos() }
| LBRACE bindings exp RBRACE                                   { Block ($2, $3), pos() }
| LBRACE exp SEMICOLON RBRACE                                  { Block ([$2], (RecordLit ([], None), pos())), pos() }
| LBRACE exp RBRACE                                            { $2 }
| LBRACE perhaps_semi RBRACE                                   { Block ([], (TupleLit [], pos())), pos() }

perhaps_semi:
| SEMICOLON                                                    {}
|                                                              {}

exp:
| table_expression                                            { $1 }

unique:
| UNIQUE                                                       { true }
|                                                              { false }

labeled_exps:
| record_label EQ exp                                          { [$1, $3] }
| record_label EQ exp COMMA labeled_exps                       { ($1, $3) :: $5 }

record_label:
| VARIABLE                                                     { $1 } 
| UINTEGER                                                     { Num.string_of_num $1 }

/*
 * Datatype grammar
 */
just_datatype:
| datatype SEMICOLON                                           { $1 }

datatype:
| mu_datatype                                                  { $1 }
| mu_datatype RARROW datatype                                  { FunctionType ($1, $3) }

mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { MuType ($2, $4) }
| primary_datatype                                             { $1 }

primary_datatype:
| LPAREN RPAREN                                                { UnitType }
| LPAREN datatype RPAREN                                       { $2 }
| LPAREN datatype COMMA datatypes RPAREN                       { TupleType ($2 :: $4) }

| LPAREN row RPAREN                                            { RecordType $2 }
| LBRACE VARIABLE RBRACE                                       { RecordType ([], Some $2) }

| TABLEHANDLE LPAREN zrow RPAREN                               { TableType $3 }

| LBRACKETBAR vrow BARRBRACKET                                 { VariantType $2 }
| LBRACKET datatype RBRACKET                                   { ListType $2 }
| VARIABLE                                                     { TypeVar $1 }
| CONSTRUCTOR                                                  { match $1 with 
                                                                   | "Bool"    -> PrimitiveType `Bool
                                                                   | "Int"     -> PrimitiveType `Int
                                                                   | "Char"    -> PrimitiveType `Char
                                                                   | "Float"   -> PrimitiveType `Float
                                                                   | "XMLitem" -> PrimitiveType `XMLitem
                                                                   | "Database"-> DBType
                                                                   | "String"  -> ListType (PrimitiveType `Char)
                                                                   | "XML"     -> ListType (PrimitiveType `XMLitem)
                                                                   | t         -> PrimitiveType (`Abstract t)
                                                               }

| CONSTRUCTOR LPAREN primary_datatype RPAREN                   { match $1 with 
                                                                   | "Mailbox"    -> MailboxType $3
                                                                   | t -> failwith ("Unknown unary type constructor : " ^ t)
                                                               }
row:
| fields                                                       { $1 }

zrow:
| zfields                                                      { $1 }

vrow:
| vfields                                                      { $1 }

datatypes:
| datatype                                                     { [$1] }
| datatype COMMA datatypes                                     { $1 :: $3 }

/* this assumes that the type (a) is invalid.  Is that a reasonable assumption? 
  (i.e. that records cannot be open rows?)  The only reason to make such an
  assumption is that "(a)" is ambiguous (is it an empty open record or a 
  parenthesized regular type variable?).
*/
zfields:
| fields                                                       { $1 }
| VARIABLE                                                     { [], Some $1 }

fields:
| field                                                        { [$1], None }
| field COMMA VARIABLE                                         { [$1], Some $3 }
| field COMMA fields                                           { $1 :: fst $3, snd $3 }

vfields:
| vfield                                                       { [$1], None }
| VARIABLE                                                     { [], Some $1 }
| vfield VBAR vfields                                          { $1 :: fst $3, snd $3 }

vfield:
| CONSTRUCTOR COLON datatype                                   { $1, `Present $3 }
| CONSTRUCTOR COLON MINUS                                      { $1, `Absent     }
| CONSTRUCTOR                                                  { $1, `Present UnitType }

field:
| fname COLON datatype                                         { $1, `Present $3 }
| fname COLON MINUS                                            { $1, `Absent }

fname:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }


/*
 * Regular expression grammar
 */
regex:
| SLASH regex_pattern_sequence SLASH                           { Regex (Seq $2), pos() }
| SLASH SLASH                                                  { Regex (Simply ""), pos() }

regex_pattern:
| RANGE                                                        { Range $1 }
| STRING                                                       { Simply $1 }
| DOT                                                          { Any }
| LPAREN regex_pattern_sequence RPAREN                         { Seq $2 }
| regex_pattern STAR                                           { Repeat (Regex.Star, $1) }
| regex_pattern PLUS                                           { Repeat (Regex.Plus, $1) }
| regex_pattern QUESTION                                       { Repeat (Regex.Question, $1) }
| block                                                        { Splice $1 }

regex_pattern_sequence:
| regex_pattern                                                { [$1] }
| regex_pattern regex_pattern_sequence                         { $1 :: $2 }



/*
 * Pattern grammar
 */
pattern:
| typed_pattern                                             { $1 }
| typed_pattern COLON primary_datatype                      { `HasType ($1, $3), pos() }

typed_pattern:
| cons_pattern                                              { $1 }
| cons_pattern AS VARIABLE                                  { `As ($3, $1), pos() }

cons_pattern:
| constructor_pattern                                       { $1 }
| constructor_pattern COLONCOLON cons_pattern               { `Cons ($1, $3), pos() }

constructor_pattern:
| primary_pattern                                           { $1 }
| CONSTRUCTOR                                               { `Variant ($1, None), pos() }
| CONSTRUCTOR parenthesized_pattern                         { `Variant ($1, Some $2), pos() }

parenthesized_pattern:
| LPAREN RPAREN                                             { `Tuple [], pos() }
| LPAREN pattern RPAREN                                     { $2 }
| LPAREN pattern COMMA patterns RPAREN                      { `Tuple ($2 :: $4), pos() }
| LPAREN labeled_patterns VBAR pattern RPAREN               { `Record ($2, Some $4), pos() }
| LPAREN labeled_patterns RPAREN                            { `Record ($2, None), pos() }

primary_pattern:
| VARIABLE                                                  { `Variable $1, pos() }
| UNDERSCORE                                                { `Any, pos() }
| constant                                                  { `Constant $1, pos() }
| LBRACKET RBRACKET                                         { `Nil, pos() }
| LBRACKET patterns RBRACKET                                { `List $2, pos() }
| parenthesized_pattern                                     { $1 }

patterns:
| pattern                                                   { [$1] }
| pattern COMMA patterns                                    { $1 :: $3 }

labeled_patterns:
| record_label EQ pattern                                   { [($1, $3)] }
| record_label EQ pattern COMMA labeled_patterns            { ($1, $3) :: $5 }
