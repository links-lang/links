%{

open Utility
open List
open Sugartypes

let ensure_match (start, finish) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (Sugar.ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
                                           (start, finish)))

let pos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let default_fixity = Num.num_of_int 9

let annotate (signame, datatype) ((name, _, _) as defbit, dpos) =
  if signame <> name then 
    raise (Sugar.ConcreteSyntaxError
             ("Signature for `" ^ signame ^ "' should precede definition of `"
              ^ signame ^ "', not `"^ name ^"'.",
              pos ()));
  TypeAnnotation ((Definition defbit, dpos),datatype), pos()


%}

%token END
%token EQ IN 
%token FUN RARROW MINUSLBRACE RBRACERARROW VAR OP ABS APP
%token IF ELSE
%token MINUS MINUSDOT
%token SWITCH RECEIVE CASE SPAWN
%token LPAREN RPAREN
%token LBRACE RBRACE ELBRACE LQUOTE RQUOTE
%token RBRACKET LBRACKET LBRACKETBAR BARRBRACKET
%token FOR LARROW LLARROW HANDLE WHERE FORMLET
%token COMMA VBAR DOT COLON COLONCOLON
%token TABLE TABLEHANDLE FROM DATABASE WITH YIELDS ORDERBY
%token UPDATE DELETE INSERT VALUES SET
%token READONLY
%token ESCAPE
%token CLIENT SERVER NATIVE
%token SEMICOLON
%token TRUE FALSE
%token BARBAR AMPAMP
%token <Num.num> UINTEGER
%token <float> UFLOAT 
%token <string> STRING CDATA REGEXREPL
%token <char> CHAR
%token <string> VARIABLE CONSTRUCTOR KEYWORD QUOTEDVAR
%token <string> LXML ENDTAG
%token RXML SLASHRXML
%token MU ALIEN SIG
%token QUESTION TILDE PLUS STAR SLASH SSLASH LSLASH FLAGL FLAGN FLAGG
%token <char*char> RANGE
%token UNDERSCORE AS
%token <[`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit> INFIX INFIXL INFIXR PREFIX POSTFIX
%token TYPENAME
%token <string> PREFIXOP POSTFIXOP
%token <string> INFIX0 INFIXL0 INFIXR0
%token <string> INFIX1 INFIXL1 INFIXR1
%token <string> INFIX2 INFIXL2 INFIXR2
%token <string> INFIX3 INFIXL3 INFIXR3
%token <string> INFIX4 INFIXL4 INFIXR4
%token <string> INFIX5 INFIXL5 INFIXR5
%token <string> INFIX6 INFIXL6 INFIXR6
%token <string> INFIX7 INFIXL7 INFIXR7
%token <string> INFIX8 INFIXL8 INFIXR8
%token <string> INFIX9 INFIXL9 INFIXR9

%start just_datatype
%start interactive
%start file

%type <Sugartypes.phrase list * Sugartypes.phrase option> file
%type <Sugartypes.datatype> datatype
%type <Sugartypes.datatype> just_datatype
%type <Sugartypes.sentence> interactive
%type <Sugartypes.regex' list> regex_pattern_sequence
%type <Sugartypes.ppattern> pattern

%%

interactive:
| nofun_declaration                                            { `Definitions [$1] }
| fun_declarations SEMICOLON                                   { `Definitions $1 }
| SEMICOLON                                                    { `Definitions [] }
| exp SEMICOLON                                                { `Expression $1 }
| directive                                                    { `Directive $1 }
| END                                                          { `Directive ("quit", []) (* rather hackish *) }

file:
| declarations exp END                                          { $1, Some $2 }
| exp END                                                       { [], Some $1 }
| declarations END                                              { $1, None }

directive:
| KEYWORD args SEMICOLON                                       { ($1, $2) }

args: 
| /* empty */                                                  { [] }
| arg args                                                     { $1 :: $2 }

arg:
| STRING                                                       { $1 }
| VARIABLE                                                     { $1 }
| CONSTRUCTOR                                                  { $1 }
| UINTEGER                                                     { Num.string_of_num $1 }
| UFLOAT                                                       { string_of_float $1 }
| TRUE                                                         { "true" }
| FALSE                                                        { "false" }

declarations:
| declarations declaration                                     { $1 @ [$2] }
| declaration                                                  { [$1] }

declaration:
| fun_declaration { $1 }
| nofun_declaration { $1 }

nofun_declaration:
| ALIEN VARIABLE VARIABLE COLON datatype SEMICOLON             { Foreign ($2, $3, $5), pos() }
| fixity perhaps_uinteger op SEMICOLON                         { let assoc, set = $1 in
                                                                   set assoc (Num.int_of_num (fromOption default_fixity $2)) $3; 
                                                                   (InfixDecl, pos()) }
| tlvarbinding SEMICOLON                                       { let d, pos = $1 in Definition d, pos }
| signature tlvarbinding SEMICOLON                             { annotate $1 $2 }
| typedecl SEMICOLON                                           { $1 }

fun_declarations:
| fun_declarations fun_declaration { $1 @ [$2] }
| fun_declaration { [$1] }

perhaps_uinteger:
| /* empty */                                                  { None }
| UINTEGER                                                     { Some $1 }

fun_declaration:
| tlfunbinding                                                 { let d, pos = $1 in Definition d, pos }
| signature tlfunbinding                                       { annotate $1 $2 }

tlfunbinding:
| FUN VARIABLE arg_lists perhaps_location block                { ($2, (FunLit (Some $2, $3, $5), pos()), $4), pos() }
| OP pattern op pattern perhaps_location block                 { ($3, (FunLit (Some $3, [[$2; $4]], $6), pos()), $5), pos() }
| OP PREFIXOP pattern perhaps_location block                   { ($2, (FunLit (Some $2, [[$3]], $5), pos()), $4), pos() }
| OP pattern POSTFIXOP perhaps_location block                  { ($3, (FunLit (Some $3, [[$2]], $5), pos()), $4), pos() }

tlvarbinding:
| VAR VARIABLE perhaps_location EQ exp                         { ($2, $5, $3), pos() }

signature: 
| SIG VARIABLE COLON datatype                                  { $2, $4 }
| SIG op COLON datatype                                        { $2, $4 }

typedecl:
| TYPENAME CONSTRUCTOR typeargs_opt EQ datatype                { TypeDeclaration ($2, $3, $5), pos()  }

typeargs_opt:
| /* empty */                                                  { [] }
| LPAREN varlist RPAREN                                        { $2 }

varlist:
| VARIABLE                                                     { [$1] }
| VARIABLE COMMA varlist                                       { $1 :: $3 }

fixity:
| INFIX                                                        { `None, $1 }
| INFIXL                                                       { `Left, $1 }
| INFIXR                                                       { `Right, $1 }
| PREFIX                                                       { `Pre, $1 }
| POSTFIX                                                      { `Post, $1 }

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

atomic_expression:
| VARIABLE                                                     { Var $1, pos() }
| constant                                                     { $1 }
| parenthesized_thing                                          { $1 }

primary_expression:
| atomic_expression                                            { $1 }
| LBRACKET RBRACKET                                            { ListLit [], pos() } 
| LBRACKET exps RBRACKET                                       { ListLit $2, pos() } 
| xml                                                          { $1 }
| FUN arg_lists block                                          { FunLit (None, $2, $3), pos() }

constructor_expression:
| CONSTRUCTOR                                                  { ConstructorLit($1, None), pos() }
| CONSTRUCTOR parenthesized_thing                              { ConstructorLit($1, Some $2), pos() }

parenthesized_thing:
| LPAREN binop RPAREN                                          { Section $2, pos() }
| LPAREN DOT record_label RPAREN                               { Section (`Project $3), pos() }
| LPAREN RPAREN                                                { RecordLit ([], None), pos() }
| LPAREN labeled_exps VBAR exp RPAREN                          { RecordLit ($2, Some $4), pos() }
| LPAREN labeled_exps RPAREN                                   { RecordLit ($2, None),               pos() }
| LPAREN exps RPAREN                                           { TupleLit ($2), pos() }
| LPAREN exp WITH labeled_exps RPAREN                          { With ($2, $4), pos() }

binop:
| MINUS                                                        { `Minus }
| MINUSDOT                                                     { `FloatMinus }
| op                                                           { `Name $1 }

op:
| INFIX0                                                       { $1 }
| INFIXL0                                                      { $1 }
| INFIXR0                                                      { $1 }
| INFIX1                                                       { $1 }
| INFIXL1                                                      { $1 }
| INFIXR1                                                      { $1 }
| INFIX2                                                       { $1 }
| INFIXL2                                                      { $1 }
| INFIXR2                                                      { $1 }
| INFIX3                                                       { $1 }
| INFIXL3                                                      { $1 }
| INFIXR3                                                      { $1 }
| INFIX4                                                       { $1 }
| INFIXL4                                                      { $1 }
| INFIXR4                                                      { $1 }
| INFIX5                                                       { $1 }
| INFIXL5                                                      { $1 }
| INFIXR5                                                      { $1 }
| INFIX6                                                       { $1 }
| INFIXL6                                                      { $1 }
| INFIXR6                                                      { $1 }
| INFIX7                                                       { $1 }
| INFIXL7                                                      { $1 }
| INFIXR7                                                      { $1 }
| INFIX8                                                       { $1 }
| INFIXL8                                                      { $1 }
| INFIXR8                                                      { $1 }
| INFIX9                                                       { $1 }
| INFIXL9                                                      { $1 }
| INFIXR9                                                      { $1 }
 
postfix_expression:
| primary_expression                                           { $1 }
| primary_expression POSTFIXOP                                 { UnaryAppl (`Name $2, $1), pos() }
| block                                                        { $1 }
| SPAWN block                                                  { Spawn $2, pos() }
| postfix_expression arg_spec                                  { FnAppl ($1, $2), pos() }
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
| PREFIXOP unary_expression                                    { UnaryAppl (`Name $1, $2), pos() }
| ABS unary_expression                                         { UnaryAppl (`Abs, $2), pos() }
| postfix_expression                                           { $1 }
| constructor_expression                                       { $1 }

infixr_9:
| unary_expression                                             { $1 }
| unary_expression INFIX9 unary_expression                     { InfixAppl (`Name $2, $1, $3), pos() }
| unary_expression INFIXR9 infixr_9                            { InfixAppl (`Name $2, $1, $3), pos() }

infixl_9:
| infixr_9                                                     { $1 }
| infixl_9 INFIXL9 infixr_9                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_8:
| infixl_9                                                     { $1 }
| infixl_9 INFIX8  infixl_9                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_9 INFIXR8 infixr_8                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_9 COLONCOLON infixr_8                                 { InfixAppl (`Cons, $1, $3), pos() }

infixl_8:
| infixr_8                                                     { $1 }
| infixl_8 INFIXL8 infixr_8                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_7:
| infixl_8                                                     { $1 }
| infixl_8 INFIX7  infixl_8                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_8 INFIXR7 infixr_7                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_7:
| infixr_7                                                     { $1 }
| infixl_7 INFIXL7 infixr_7                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_6:
| infixl_7                                                     { $1 }
| infixl_7 INFIX6  infixl_7                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_7 INFIXR6 infixr_6                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_6:
| infixr_6                                                     { $1 }
| infixl_6 INFIXL6 infixr_6                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_6 MINUS infixr_6                                      { InfixAppl (`Minus, $1, $3), pos() }
| infixl_6 MINUSDOT infixr_6                                   { InfixAppl (`FloatMinus, $1, $3), pos() }

infixr_5:
| infixl_6                                                     { $1 }
| infixl_6 INFIX5  infixl_6                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_6 INFIXR5 infixr_5                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_5:
| infixr_5                                                     { $1 }
| infixl_5 INFIXL5 infixr_5                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_4:
| infixl_5                                                     { $1 }
| infixl_5 INFIX4    infixl_5                                  { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_5 INFIXR4   infixr_4                                  { InfixAppl (`Name $2, $1, $3), pos() }
| infixr_5 TILDE     regex                                     { InfixAppl (`RegexMatch, $1, $3), pos() }

infixl_4:
| infixr_4                                                     { $1 }
| infixl_4 INFIXL4 infixr_4                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_3:
| infixl_4                                                     { $1 }
| infixl_4 INFIX3  infixl_4                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_4 INFIXR3 infixr_3                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_3:
| infixr_3                                                     { $1 }
| infixl_3 INFIXL3 infixr_3                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_2:
| infixl_3                                                     { $1 }
| infixl_3 INFIX2  infixl_3                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_3 INFIXR2 infixr_2                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_2:
| infixr_2                                                     { $1 }
| infixl_2 INFIXL2 infixr_2                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_1:
| infixl_2                                                     { $1 }
| infixl_2 INFIX1  infixl_2                                    { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_2 INFIXR1 infixr_1                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixl_1:
| infixr_1                                                     { $1 }
| infixl_1 INFIXL1 infixr_1                                    { InfixAppl (`Name $2, $1, $3), pos() }

infixr_0:
| infixl_1                                                     { $1 }
| infixl_1 APP       infixl_1                                  { InfixAppl (`App, $1, $3), pos() }
| infixl_1 INFIX0    infixl_1                                  { InfixAppl (`Name $2, $1, $3), pos() }
| infixl_1 INFIXR0   infixr_0                                  { InfixAppl (`Name $2, $1, $3), pos() }

infixl_0:
| infixr_0                                                     { $1 }
| infixl_0 INFIXL0 infixr_0                                    { InfixAppl (`Name $2, $1, $3), pos() }

logical_expression:
| infixl_0                                                     { $1 }
| logical_expression BARBAR infixl_0                           { InfixAppl (`Or, $1, $3), pos() }
| logical_expression AMPAMP infixl_0                           { InfixAppl (`And, $1, $3), pos() }

typed_expression:
| logical_expression                                           { $1 }
| logical_expression COLON datatype                            { TypeAnnotation ($1, $3), pos() }

db_expression:
| typed_expression                                             { $1 }
| INSERT exp VALUES exp                                        { DBInsert ($2, $4), pos() }
| DELETE LPAREN table_generator RPAREN perhaps_where           { DBDelete ($3, $5), pos() }
| UPDATE LPAREN table_generator RPAREN
         perhaps_where
         SET LPAREN labeled_exps RPAREN                        { DBUpdate($3, $5, $8), pos() }

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
| form_binding                                                 { $1 }
| xml_tree                                                     { $1 }
| CDATA                                                        { TextNode (Utility.xml_unescape $1), pos() }

form_binding:
| LBRACE logical_expression RARROW pattern RBRACE              { FormBinding($2, $4), pos()}

conditional_expression:
| db_expression                                                { $1 }
| IF LPAREN exp RPAREN exp ELSE exp                            { Conditional ($3, $5, $7), pos() }

cases:
| case                                                         { [$1] }
| case cases                                                   { $1 :: $2 }

case:
| CASE pattern RARROW block_contents                           { $2, (Block ($4), pos()) }

perhaps_cases:
| /* empty */                                                  { [] }
| cases                                                        { $1 }

case_expression:
| conditional_expression                                       { $1 }
| SWITCH LPAREN exp RPAREN LBRACE perhaps_cases RBRACE         { Switch ($3, $6), pos() }
| RECEIVE LBRACE perhaps_cases RBRACE                          { Receive ($3), pos() }

iteration_expression:
| case_expression                                              { $1 }
| FOR LPAREN generator RPAREN
      perhaps_where
      perhaps_orderby
      exp                                                      { Iteration (($3 : generatorphrase), $7, $5, $6), pos() }


generator:
| list_generator                                               { `List $1 }
| table_generator                                              { `Table $1 }

list_generator:
| VAR pattern LARROW exp                                       { ($2, $4) }

table_generator:
| VAR pattern LLARROW exp                                      { ($2, $4) }

perhaps_where:
| /* empty */                                                  { None }
| WHERE LPAREN exp RPAREN                                      { Some $3 }

perhaps_orderby:
| /* empty */                                                  { None }
| ORDERBY LPAREN exp RPAREN                                    { Some $3 }

escape_expression:
| iteration_expression                                         { $1 }
| ESCAPE VARIABLE IN postfix_expression                        { Escape ($2, $4), pos() }

handlewith_expression:
| escape_expression                                            { $1 }
| HANDLE exp WITH VARIABLE RARROW exp                          { HandleWith ($2, $4, $6), pos() }
| FORMLET xml YIELDS exp                                       { Formlet($2, $4), pos() }

table_expression:
| handlewith_expression                                        { $1 }
| TABLE exp WITH datatype perhaps_table_constraints FROM exp   { TableLit ($2, $4, $5, $7), pos()} 

perhaps_table_constraints:
| WHERE table_constraints                                      { $2 }
| /* empty */                                                  { [] }

table_constraints:
| record_label field_constraints                               { [($1, $2)] } 
| record_label field_constraints COMMA table_constraints       { ($1, $2) :: $4 }

field_constraints:
| field_constraint                                             { [$1] }
| field_constraint field_constraints                           { $1 :: $2 }

field_constraint:
| READONLY                                                     { `Readonly }

perhaps_db_args:
| atomic_expression                                            { Some $1 }
| /* empty */                                                  { None }

perhaps_db_driver:
| atomic_expression perhaps_db_args                            { Some $1, $2 }
| /* empty */                                                  { None, None }

database_expression:
| table_expression                                             { $1 }
| DATABASE atomic_expression perhaps_db_driver                 { DatabaseLit ($2, $3), pos() }

binding:
| VAR pattern EQ exp SEMICOLON                                 { Binding ($2, $4), pos() }
| exp SEMICOLON                                                { $1 }
| FUN VARIABLE arg_lists block                                 { FunLit (Some $2, $3, $4), pos() }

bindings:
| binding                                                      { [$1] }
| bindings binding                                             { $1 @ [$2] }

block:
| LBRACE block_contents RBRACE                                 { Block $2, pos() }

block_contents:
| bindings exp SEMICOLON                                       { ($1 @ [$2], (RecordLit ([], None), pos())) }
| bindings exp                                                 { ($1, $2) }
| exp SEMICOLON                                                { ([$1], (RecordLit ([], None), pos())) }
| exp                                                          { [], $1 }
| perhaps_semi                                                 { ([], (TupleLit [], pos())) }

perhaps_semi:
| SEMICOLON                                                    {}
| /* empty */                                                  {}

exp:
| database_expression                                          { $1 }

labeled_exps:
| record_label EQ exp                                          { [$1, $3] }
| record_label EQ exp COMMA labeled_exps                       { ($1, $3) :: $5 }

/*
 * Datatype grammar
 */
just_datatype:
| datatype END                                                 { $1 }

datatype:
| mu_datatype                                                  { $1 }
| parenthesized_datatypes RARROW datatype                      { FunctionType ($1,
                                                                               Sugar.fresh_type_variable (),
                                                                               $3) }
| parenthesized_datatypes 
       MINUSLBRACE datatype RBRACERARROW datatype              { FunctionType ($1,
                                                                               TypeApplication ("Mailbox", [$3]),
                                                                               $5) }
mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { MuType ($2, $4) }
| primary_datatype                                             { $1 }

parenthesized_datatypes:
| LPAREN RPAREN                                                { [] }
| LPAREN datatypes RPAREN                                      { $2 }


primary_datatype:
| parenthesized_datatypes                                      { match $1 with
                                                                   | [] -> UnitType
                                                                   | [t] -> t
                                                                   | ts  -> TupleType ts }
| LPAREN fields RPAREN                                         { RecordType $2 }

| TABLEHANDLE LPAREN datatype COMMA datatype RPAREN            { TableType ($3, $5) }

| LBRACKETBAR vrow BARRBRACKET                                 { VariantType $2 }
| LBRACKET datatype RBRACKET                                   { ListType $2 }
| VARIABLE                                                     { RigidTypeVar $1 }
| QUOTEDVAR                                                    { TypeVar $1 }
| CONSTRUCTOR                                                  { match $1 with 
                                                                   | "Bool"    -> PrimitiveType `Bool
                                                                   | "Int"     -> PrimitiveType `Int
                                                                   | "Char"    -> PrimitiveType `Char
                                                                   | "Float"   -> PrimitiveType `Float
                                                                   | "XmlItem" -> PrimitiveType `XmlItem
								   | "NativeString" -> PrimitiveType `NativeString
                                                                (*   | "Xml"     -> ListType (PrimitiveType `XmlItem) *)
                                                                   | "Database"-> DBType
                                                                   | t         -> TypeApplication (t, [])
                                                               }

| CONSTRUCTOR LPAREN primary_datatype_list RPAREN              { TypeApplication ($1, $3) }

primary_datatype_list:
| primary_datatype                                             { [$1] }
| primary_datatype COMMA primary_datatype_list                 { $1 :: $3 }

vrow:
| vfields                                                      { $1 }
| /* empty */                                                  { [], `Closed }

datatypes:
| datatype                                                     { [$1] }
| datatype COMMA datatypes                                     { $1 :: $3 }

fields:
| field                                                        { [$1], `Closed }
| field VBAR row_variable                                      { [$1], $3 }
| VBAR row_variable                                            { [], $2 }
| field COMMA fields                                           { $1 :: fst $3, snd $3 }

vfields:
| vfield                                                       { [$1], `Closed }
| row_variable                                                 { [], $1 }
| vfield VBAR vfields                                          { $1 :: fst $3, snd $3 }

vfield:
| CONSTRUCTOR COLON datatype                                   { $1, `Present $3 }
| CONSTRUCTOR MINUS                                            { $1, `Absent }
| CONSTRUCTOR                                                  { $1, `Present UnitType }

field:
| record_label COLON datatype                                  { $1, `Present $3 }
| record_label MINUS                                           { $1, `Absent }

record_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }
| UINTEGER                                                     { Num.string_of_num $1 }

row_variable:
| VARIABLE                                                     { `Open $1 }
| LPAREN MU VARIABLE DOT vfields RPAREN                        { `Recursive ($3, $5) }

/*
 * Regular expression grammar
 */
regex:
| SLASH regex_pattern_sequence SLASH regex_flags_opt SLASH                           { (Regex ((Seq $2), $4), pos()) }
| SLASH SLASH regex_flags_opt SLASH                                                  { (Regex ((Simply ""), $3), pos()) }
| SSLASH regex_pattern_sequence SLASH regex_replace SLASH regex_flags_opt SLASH      { (Regex((Replace (Seq $2, $4)), $6), pos())} 

regex_flags_opt:
| /* empty */                                                  {[]}
| regex_flag regex_flags_opt                                   {$1::$2}

regex_flag:
| FLAGL                                                          {RegexList}
| FLAGN                                                          {RegexNative}
| FLAGG                                                          {RegexGlobal}

regex_replace:
| /* empty */                                                  {`ReplaceLiteral ""}
| REGEXREPL                                                    {`ReplaceLiteral $1}
| block                                                        {`ReplaceSplice $1}

regex_pattern:
| RANGE                                                        { Range $1 }
| STRING                                                       { Simply $1 }
| DOT                                                          { Any }
| LPAREN regex_pattern_sequence RPAREN                         { Group $2 }
| regex_pattern STAR                                           { Repeat (Regex.Star, $1) }
| regex_pattern PLUS                                           { Repeat (Regex.Plus, $1) }
| regex_pattern QUESTION                                       { Repeat (Regex.Question, $1) }
| ELBRACE block_contents RBRACE                                { Quote(Splice((Block $2, pos()))) }
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

multi_args:
| LPAREN patterns RPAREN                                    { $2 }
| LPAREN RPAREN                                             { [] }

arg_lists:
| multi_args { [$1] }
| multi_args arg_lists { $1 :: $2 }
