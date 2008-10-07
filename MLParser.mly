%{

(** ML-ish syntax.

    This syntax deliberately only covers "core Links": functions,
    variants, records, etc.  There is deliberately no support for XML,
    formlets, pages, client/server annotations, database syntax,
    for-loops, concurrency, regular expressions, list syntax in types,
    tuples, abs and app, a primitive bool type, blocks and semicolons,
    prefix and postfix operators (don't mix with sections).

    However, we should support all of the following Links features:
    functions, variants, records, conditionals, variable binding, deep
    pattern matching, most of the type language, list literals
    (including range literals), foreign function declarations, infix
    operators, literals for strings, integers, etc., continuations,
    separate signatures for bindings.

    In addition, we also support: left and right sections, constructor
    sections (in lieu of "first-class constructors"), uniform syntax
    for declaring functions and operators, more ML-like syntax for
    almost everything, infix constructors (beginning with ':';
    including '::'), some form of forall for types.

    TODO (generally):
    * switch this to use menhir.
    * post-process operators instead of handling them in the scanner/grammar.

    TODO (specifically):
    * signatures
    * remaining semantic actions
    * infix type operators (including ->)
**)
open Sugartypes

let fresh_type_variable : unit -> datatype =
  let tyvar_counter = ref 0 in
    fun () -> incr tyvar_counter; TypeVar ("_" ^ string_of_int (!tyvar_counter))

let pos () : Sugartypes.position = Parsing.symbol_start_pos (), Parsing.symbol_end_pos (), None
let datatype d = d, None
%}

%token EOF
%token FUN VAR IF THEN ELSE MATCH WITH LET ESCAPE END MU ALIEN IN AND AS TYPE INFIX INFIXL INFIXR EXCEPT FORALL
%token LPAREN RPAREN RBRACKET LBRACKET LBRACE RBRACE
%token LARROW FATRARROW COMMA VBAR DOT COLON SEMICOLON MINUS EQ UNDERSCORE
%token <Num.num> UINTEGER
%token <float>   UFLOAT 
%token <string>  STRING
%token <char>    CHAR
%token <string>  VARIABLE CONSTRUCTOR QUOTEDVAR SYMBOL

%start just_datatype
%start interactive
%start file

%type <Sugartypes.binding list> file
%type <Sugartypes.datatype>     just_datatype
%type <Sugartypes.sentence>     interactive
%type <Sugartypes.pattern>      pattern
%type <Sugartypes.phrase>       expr

%%
/** entry points **/
interactive:
(*
  Why do we need LET here?  Answer: we can't have all of the following
    * 'fun' for both declarations and expressions:    fun f x = e1      fun x -> e
    * both declarations and expressions allowed at the interactive prompt
    * unambiguous parsing
*)
| LET decls_opt SEMICOLON                                      { `Definitions $2 }
| expr  SEMICOLON                                              { `Expression $1  }
| EOF                                                          { failwith "EOF" }

file:
| decls_opt EOF                                                { $1 }

(*
 * declaration grammar
 *)
decls_opt:
| /* empty */                                                  { [] }
| decls                                                        { $1 }

decls:
| decl                                                         { [$1]     }
| decl decls                                                   { $1 :: $2 }

decl:
  FUN    fun_decl                                              { `Funs $2, pos () }
| VAR    val_decl                                              { `Val ([],
                                                                       (`Tuple (List.map fst $2), pos ()),
                                                                       (`TupleLit (List.map snd $2), pos ()),
                                                                       `Unknown,
                                                                       None), pos () }
| TYPE   type_decl                                             { $2, pos () }
| ALIEN  alien_decl                                            { $2 }
| fixity fixity_decl                                           { `Infix, pos () }

fixity:
| INFIX                                                        { `None }
| INFIXL                                                       { `Left }
| INFIXR                                                       { `Right }

fun_decl:
| fun_decl_1                                                   { [$1] }
| fun_decl_1 AND fun_decl                                      { $1 :: $3 }

fun_decl_1: 
| pattern SYMBOL pattern EQ expr                               { ($2, None, pos ()), (([], None), ([[$1; $3]], $5)), `Unknown, None, pos () }
| VARIABLE primary_pattern_list EQ expr                        { ($1, None, pos ()), (([], None), (List.map (fun x -> [x]) $2, $4)), `Unknown, None, pos () }

val_decl:
| val_decl_1                                                   { [$1] }
| val_decl_1 AND val_decl                                      { $1 :: $3 }

val_decl_1:
| pattern EQ expr                                              { $1, $3 }

type_decl: /* this should probably allow (mutual) recursion */
| CONSTRUCTOR typeargs_opt EQ datatype                         { `Type ($1, $2, ($4, None)) }
| VARIABLE CONSTRUCTOR VARIABLE EQ datatype                    { `Type ($2, [$1, None; $3, None], ($5, None)) }

typeargs_opt:
| /* empty */                                                  { [] }
| varlist                                                      { $1 }

varlist:
| VARIABLE                                                     { [$1, None] }
| VARIABLE varlist                                             { ($1, None) :: $2 }

alien_decl: 
| VARIABLE VARIABLE COLON datatype                             { `Foreign (($2, None, pos ()), $1, datatype $4), pos () }

fixity_decl:
| perhaps_uinteger SYMBOL                                      { $1 }

perhaps_uinteger:
| /* empty */                                                  { None }
| UINTEGER                                                     { Some $1 }

(*
 * expression grammar
 *)
expr:
| let_expression                                               { $1 }

let_expression:
| LET decls_opt IN let_expression                              { `Block ($2, $4), pos () }
| ESCAPE VARIABLE IN let_expression                            { `Escape (($2, None, pos ()), $4), pos () }
| fun_expression                                               { $1 }

fun_expression:
| conditional_expression                                       { $1 }
| FUN primary_pattern_list FATRARROW expr                      { `FunLit (None, ((List.map (fun p -> [p]) $2), $4)), pos () }

conditional_expression:
| IF expr THEN expr ELSE expr                                  { `Conditional ($2, $4, $6), pos () }
| typed_expression                                             { $1 }

typed_expression:
| infix_expression COLON datatype                              { `TypeAnnotation ($1, datatype $3), pos () }
| infix_expression COLON datatype LARROW datatype              { `Upcast ($1, datatype $3, datatype $5), pos () }
| infix_expression                                             { $1 }

infix_expression:
| infix_expression SYMBOL application_expression               { `InfixAppl (([], `Name $2), $1, $3), pos () }
| application_expression                                       { $1 }

application_expression:
| application_expression primary_expression                    { `FnAppl ($1, [$2]), pos () }
| CONSTRUCTOR primary_expression                               { `ConstructorLit ($1, Some $2, None), pos () }
| projection_expression                                        { $1 }

projection_expression:
| primary_expression DOT record_label                          { `Projection ($1, $3), pos () }
| primary_expression { $1 }

primary_expression:
| atomic_expression                                            { $1 }
| LBRACKET bracket_contents RBRACKET                           { `ListLit ($2, None), pos () }
| LPAREN   paren_contents   RPAREN                             { $2 }
| LBRACE   brace_contents   RBRACE                             { $2 }
| MATCH expr WITH perhaps_cases END                            { `Switch ($2, $4, None), pos () }

cases:
| case                                                         { [$1] }
| case VBAR cases                                              { $1 :: $3 }

case:
| pattern FATRARROW primary_expression                         { $1, (fst $3, pos ()) }

perhaps_cases:
| /* empty */                                                  { [] }
| VBAR cases                                                   { $2 }

atomic_expression:
| VARIABLE                                                     { `Var $1, pos () }
| constant                                                     { let c, p = $1 in `Constant c, p }

constant:
| UINTEGER                                                     { `Int $1    , pos () }
| UFLOAT                                                       { `Float $1  , pos () }
| STRING                                                       { `String $1 , pos () }
| CHAR                                                         { `Char $1   , pos () }

primary_pattern_list:
| primary_pattern                                              { [$1] }
| primary_pattern primary_pattern_list                         { $1 :: $2 }

bracket_contents:
| comma_separated_expression_list                              { $1 }
/* TODO: ranges */

comma_separated_expression_list:
| expr                                                         { [$1] }
| expr COMMA comma_separated_expression_list                   { $1 :: $3 }

brace_contents:
| field_bindings                                               { failwith "record literal" }
| expr record_modification_list                                { failwith "record update" }
| /* empty */                                                  { `RecordLit ([], None), pos () }

record_modification:
| AND field_bindings                                           { `And $2 }
| EXCEPT field_bindings                                        { `Except $2 }

record_modification_list:
| record_modification                                          { [$1]     }
| record_modification record_modification_list                 { $1 :: $2 }

field_bindings:
| field_binding                                                { [$1] }
| field_binding COMMA field_bindings                           { $1 :: $3 }

field_binding:
| record_label EQ expr                                         { $1, $3 }

paren_contents:
| expr                                                         { $1 }
| CONSTRUCTOR                                                  { failwith "constructor section" }
| DOT record_label                                             { `Section (`Project $2), pos () }
| SYMBOL                                                       { `Section (`Name $1), pos () }
| infix_expression SYMBOL                                      { failwith "left operator section" }
| SYMBOL expr                                                  { failwith "right operator section" }

(*
 * Pattern grammar
 *)
pattern:
| postfix_pattern                                              { $1 }

postfix_pattern:
| postfix_pattern COLON datatype_application                   { `HasType ($1, datatype $3), pos () }
| postfix_pattern AS VARIABLE                                  { `As (($3, None, pos ()), $1), pos () }
| constructor_pattern                                          { $1 }

constructor_pattern:
| primary_pattern                                              { $1 }
| CONSTRUCTOR primary_pattern                                  { `Variant ($1, Some $2), pos () }

primary_pattern:
| VARIABLE                                                     { `Variable ($1, None, pos ()), pos () }
| UNDERSCORE                                                   { `Any, pos () }
| constant                                                     { let c, p = $1 in `Constant c, p }
| LBRACKET RBRACKET                                            { `Nil, pos () }
| LBRACKET pattern_list RBRACKET                               { `List $2, pos () }
| LBRACE labeled_patterns VBAR pattern RBRACE                  { `Record ($2, Some $4), pos () }
| LBRACE labeled_patterns RBRACE                               { `Record ($2, None), pos () }
| LPAREN pattern RPAREN                                        { $2 }
| MINUS CONSTRUCTOR                                            { `Negative [$2], pos () }
| MINUS LPAREN constructor_list RPAREN                         { `Negative $3, pos () }

pattern_list:
| pattern                                                      { [$1] }
| pattern COMMA pattern_list                                   { $1 :: $3 }

labeled_patterns:
| record_label EQ pattern                                      { [($1, $3)] }
| record_label EQ pattern COMMA labeled_patterns               { ($1, $3) :: $5 }

constructor_list:
| CONSTRUCTOR                                                  { [$1] }
| CONSTRUCTOR COMMA constructor_list                           { $1 :: $3 } 

(*
 * Datatype grammar
 *)
just_datatype:
| datatype END                                                 { $1 }

datatype:
| mu_datatype                                                  { $1 }

mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { MuType ($2, $4) }
| FORALL VARIABLE DOT datatype                                 { failwith "forall type" }
| infix_datatype                                               { $1 }

infix_datatype:
| datatype_application                                         { $1 }
| infix_datatype SYMBOL datatype_application                   { FunctionType ([$1], fresh_type_variable (), $3) }

datatype_application:
| primary_datatype                                             { $1 }
| datatype_application primary_datatype                        { failwith "datatype application" }

primary_datatype:
| LBRACE fields RBRACE                                         { RecordType $2 }
| LBRACKET vrow RBRACKET                                       { VariantType $2 }
| LPAREN datatype RPAREN                                       { $2 }
| VARIABLE                                                     { RigidTypeVar $1 }
| QUOTEDVAR                                                    { TypeVar $1 }
| CONSTRUCTOR                                                  { TypeApplication ($1, []) }

vrow:
| vfields                                                      { $1 }
| /* empty */                                                  { [], `Closed }

fields:
| field                                                        { [$1], `Closed }
| field VBAR row_variable                                      { [$1], $3 }
|       VBAR row_variable                                      { [], $2 }
| field COMMA fields                                           { $1 :: fst $3, snd $3 }

vfields:
| vfield                                                       { [$1], `Closed }
| row_variable                                                 { [], $1 }
| vfield VBAR vfields                                          { $1 :: fst $3, snd $3 }

vfield:
| CONSTRUCTOR COLON datatype                                   { $1, `Present $3 }
| CONSTRUCTOR MINUS                                            { $1, `Absent }

field:
| record_label COLON datatype                                  { $1, `Present $3 }
| record_label MINUS                                           { $1, `Absent }

record_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }
| UINTEGER                                                     { Num.string_of_num $1 }

row_variable:
| VARIABLE                                                     { `OpenRigid $1 }
| QUOTEDVAR                                                    { `Open $1 }
| LPAREN MU VARIABLE DOT vfields RPAREN                        { `Recursive ($3, $5) }
