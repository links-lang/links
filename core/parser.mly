(*

Note [Debugging grammar conflicts]
==================================

It might happen that after modifying the grammar Menhir reports new conflicts.
To debug these go to `dune` file and add flags --dump and --explain:

  (menhir
    (modules parser jsonparse xmlParser)
    (flags "--table" "--dump" "--explain")
  )

--dump flag generates *.automaton files in _build directory.  These files
contains human-readable description of the parser automaton, including
explanation of the conflicts, located at the end of file.  --explain flag
generates *.conflicts files that contain explanation of conflicts for each
parser.  Note that this does not include end-of-stream conflicts, which are
explained in *.automaton files.

Once done with debugging remember to disable the flags!

*)

%{

open Utility
open Sugartypes

(* Generation of fresh type variables *)

let type_variable_counter = ref 0

let fresh_type_variable : subkind option -> datatypenode =
  function subkind ->
    incr type_variable_counter; `TypeVar ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_type_variable : subkind option -> datatypenode =
  function subkind ->
    incr type_variable_counter; `TypeVar ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let fresh_row_variable : subkind option -> row_var =
  function subkind ->
    incr type_variable_counter; `Open ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_row_variable : subkind option -> row_var =
  function subkind ->
    incr type_variable_counter; `Open ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let fresh_presence_variable : subkind option -> fieldspec =
  function subkind ->
    incr type_variable_counter; `Var ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_presence_variable : subkind option -> fieldspec =
  function subkind ->
    incr type_variable_counter; `Var ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let pos (start_pos, end_pos) : Sugartypes.position = start_pos, end_pos, None

let with_pos p = Sugartypes.with_pos (pos p)

let ensure_match p (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
                                     pos p))

let default_fixity = 9

let annotate sigpos (signame, datatype) dpos : _ -> binding =
  let checksig {node=signame; _} name =
    if signame <> name then
      raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `"^ name ^"'.", pos sigpos)) in
    function
      | `Fun (name, lin, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos (`Fun ( make_untyped_binder name, lin, ([], phrase)
                              , location, Some datatype))
      | `Var (name, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos
            (`Val ( [], (with_pos dpos (`Variable (make_untyped_binder name)))
                  , phrase, location, Some datatype))
      | `Handler (bndr, hnlit, _) ->
         let _ = checksig signame (name_of_binder bndr) in
         with_pos dpos (`Handler (bndr, hnlit, Some datatype))

let primary_kind_of_string p =
  function
  | "Type" -> `Type
  | "Row" -> `Row
  | "Presence" -> `Presence
  | pk -> raise (ConcreteSyntaxError ("Invalid primary kind: " ^ pk, pos p))

let linearity_of_string p =
  function
  | "Any" -> `Any
  | "Unl" -> `Unl
  | lin -> raise (ConcreteSyntaxError ("Invalid kind linearity: " ^ lin, pos p))

let restriction_of_string p =
  function
  | "Any" -> `Any
  | "Base" -> `Base
  | "Session" -> `Session
  | rest -> raise (ConcreteSyntaxError ("Invalid kind restriction: " ^ rest, pos p))

let full_kind_of pos prim lin rest =
  let p = primary_kind_of_string pos prim in
  let l = linearity_of_string pos lin in
  let r = restriction_of_string pos rest in
  p, Some (l, r)

let full_subkind_of pos lin rest =
  let l = linearity_of_string pos lin in
  let r = restriction_of_string pos rest in
  Some (l, r)

(* In kind and subkind abbreviations, we aim to provide the most
common case. For everything except session types, the default
linearity is `Unl and the default restriction is `Any. For session
types the default linearity is `Any. *)

(* Currently "Any" means `Any,`Any, but it is probably advisable to
change "Any" to something more evocative of linearity - "Lin"
perhaps. *)

let kind_of p =
  function
  (* primary kind abbreviation  *)
  | "Type" -> `Type, None
  | "Row" -> `Row, None
  | "Presence" -> `Presence, None
  (* subkind of type abbreviations *)
  | "Any" -> `Type, Some (`Any, `Any)
  | "Base" -> `Type, Some (`Unl, `Base)
  | "Session" -> `Type, Some (`Any, `Session)
  | "Eff"     -> `Row, Some (`Unl, `Effect)
  | k -> raise (ConcreteSyntaxError ("Invalid kind: " ^ k, pos p))

let subkind_of p =
  function
  (* subkind abbreviations *)
  | "Any" -> Some (`Any, `Any)
  | "Base" -> Some (`Unl, `Base)
  | "Session" -> Some (`Any, `Session)
  | "Eff"  -> Some (`Unl, `Effect)
  | sk -> raise (ConcreteSyntaxError ("Invalid subkind: " ^ sk, pos p))

let attach_kind (t, k) = (t, k, `Rigid)

let attach_subkind_helper update sk = update sk

let attach_subkind (t, subkind) =
  let update sk =
    match t with
    | `TypeVar (x, _, freedom) ->
       `TypeVar (x, sk, freedom)
    | _ -> assert false
  in
    attach_subkind_helper update subkind

let attach_row_subkind (r, subkind) =
  let update sk =
    match r with
    | `Open (x, _, freedom) ->
       `Open (x, sk, freedom)
    | _ -> assert false
  in
    attach_subkind_helper update subkind

let row_with field (fields, row_var) = field::fields, row_var

(* this preserves 1-tuples *)
let make_tuple pos =
  function
    | [e] -> with_pos pos (`RecordLit ([("1", e)], None))
    | es  -> with_pos pos (`TupleLit es)

let labels = List.map fst

let parseRegexFlags f =
  let rec asList f i l =
    if (i == String.length f) then
      List.rev l
    else
      asList f (i+1) ((String.get f i)::l) in
    List.map (function
                'l' -> `RegexList
              | 'n' -> `RegexNative
              | 'g' -> `RegexGlobal
              | _ -> assert false) (asList f 0 [])

let datatype d = d, None

let cp_unit p = with_pos p (`Unquote ([], with_pos p (`TupleLit [])))
%}

%token END
%token EQ IN
%token FUN LINFUN RARROW LOLLI FATRARROW VAR OP
%token SQUIGRARROW SQUIGLOLLI TILDE
%token IF ELSE
%token MINUS MINUSDOT
%token SWITCH RECEIVE CASE
%token HANDLE SHALLOWHANDLE HANDLER SHALLOWHANDLER
%token SPAWN SPAWNAT SPAWNANGELAT SPAWNCLIENT SPAWNANGEL SPAWNWAIT
%token OFFER SELECT
%token DOOP
%token LPAREN RPAREN
%token LBRACE RBRACE LBRACEBAR BARRBRACE LQUOTE RQUOTE
%token RBRACKET LBRACKET LBRACKETBAR BARRBRACKET
%token LBRACKETPLUSBAR BARPLUSRBRACKET
%token LBRACKETAMPBAR BARAMPRBRACKET
%token LEFTTRIANGLE RIGHTTRIANGLE NU
%token FOR LARROW LLARROW WHERE FORMLET PAGE
%token LRARROW
%token COMMA VBAR DOT DOTDOT COLON COLONCOLON
%token TABLE TABLEHANDLE TABLEKEYS FROM DATABASE QUERY WITH YIELDS ORDERBY
%token UPDATE DELETE INSERT VALUES SET RETURNING
%token LENS LENSDROP LENSSELECT LENSJOIN DETERMINED BY ON DELETE_LEFT
%token LENSPUT LENSGET
%token READONLY DEFAULT
%token ESCAPE
%token CLIENT SERVER NATIVE
%token SEMICOLON
%token TRUE FALSE
%token BARBAR AMPAMP
%token <int> UINTEGER
%token <float> UFLOAT
%token <string> STRING CDATA REGEXREPL
%token <char> CHAR
%token <string> VARIABLE CONSTRUCTOR KEYWORD PERCENTVAR
%token <string> LXML ENDTAG
%token RXML SLASHRXML
%token MU FORALL ALIEN SIG OPEN
%token MODULE
%token BANG QUESTION
%token PERCENT EQUALSTILDE PLUS STAR ALTERNATE SLASH SSLASH CARET DOLLAR
%token <char*char> RANGE
%token <string> QUOTEDMETA
%token <string> SLASHFLAGS
%token UNDERSCORE AS
%token <[`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit> INFIX INFIXL INFIXR PREFIX POSTFIX
%token TYPENAME
%token TYPE ROW PRESENCE
%token TRY OTHERWISE RAISE
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

%type <Sugartypes.binding list * Sugartypes.phrase option> file
%type <Sugartypes.datatype> datatype
%type <Sugartypes.datatype> just_datatype
%type <Sugartypes.sentence> interactive
%type <Sugartypes.regex> regex_pattern_alternate
%type <Sugartypes.regex> regex_pattern
%type <Sugartypes.regex list> regex_pattern_sequence
%type <Sugartypes.pattern> pattern
%type <(Sugartypes.name Sugartypes.with_pos) * Sugartypes.declared_linearity * Sugartypes.funlit * Sugartypes.location> tlfunbinding
%type <Sugartypes.phrase> postfix_expression
%type <Sugartypes.phrase> primary_expression
%type <Sugartypes.phrase> atomic_expression
%type <Sugartypes.constant> constant
%type <(string * Sugartypes.phrase list) list> attr_list
%type <Sugartypes.phrase list> attr_val
%type <Sugartypes.binding> binding

%%

interactive:
| nofun_declaration                                            { `Definitions [$1] }
| fun_declarations SEMICOLON                                   { `Definitions $1 }
| SEMICOLON                                                    { `Definitions [] }
| exp SEMICOLON                                                { `Expression $1 }
| directive                                                    { `Directive $1 }
| END                                                          { `Directive ("quit", []) (* rather hackish *) }

file:
| preamble declarations exp END                                { $1 @ $2, Some $3 }
| preamble exp END                                             { $1, Some $2 }
| preamble declarations END                                    { $1 @ $2, None }

directive:
| KEYWORD args SEMICOLON                                       { ($1, $2) }

args:
| /* empty */                                                  { [] }
| arg args                                                     { $1 :: $2 }

arg:
| STRING                                                       { $1 }
| VARIABLE                                                     { $1 }
| CONSTRUCTOR                                                  { $1 }
| UINTEGER                                                     { string_of_int $1 }
| UFLOAT                                                       { string_of_float' $1 }
| TRUE                                                         { "true" }
| FALSE                                                        { "false" }

var:
| VARIABLE                                                     { with_pos $loc $1 }

preamble:
| /* empty */                                                  { [] }

declarations:
| declarations declaration                                     { $1 @ [$2] }
| declaration                                                  { [$1] }

declaration:
| fun_declaration                                              { $1 }
| nofun_declaration                                            { $1 }

nofun_declaration:
| alien_block                                                  { $1 }
| ALIEN VARIABLE STRING var COLON datatype SEMICOLON           { with_pos $loc
                                                                          (`Foreign (make_untyped_binder $4, $4.node, $2, $3, datatype $6)) }
| fixity perhaps_uinteger op SEMICOLON                         { let assoc, set = $1 in
                                                                   set assoc (from_option default_fixity $2) ($3.node);
                                                                   with_pos $loc `Infix }
| tlvarbinding SEMICOLON                                       { let (bndr,p,l) = $1
                                                                 in with_pos $loc($1) (`Val ([],
                                                                    (with_pos $loc($1) (`Variable (make_untyped_binder bndr))), p, l, None)) }
| signature tlvarbinding SEMICOLON                             { annotate $loc($1) $1 $loc($2) (`Var $2) }
| typedecl SEMICOLON                                           { $1 }

| links_module                                                 { $1 }
| links_open SEMICOLON                                         { $1 }

alien_datatype:
| var COLON datatype SEMICOLON                                 { (make_untyped_binder $1, datatype $3) }

alien_datatypes:
| alien_datatype                                               { [$1] }
| alien_datatype alien_datatypes                               { $1 :: $2 }

links_module:
| MODULE module_name moduleblock                               { with_pos $loc($2) (`Module ($2, $3)) }

alien_block:
| ALIEN VARIABLE STRING LBRACE alien_datatypes RBRACE          { let language     = $2 in
                                                                 let library_name = $3 in
                                                                 with_pos $loc (`AlienBlock (language, library_name, $5)) }

module_name:
| CONSTRUCTOR                                                  { $1 }

fun_declarations:
| fun_declarations fun_declaration                             { $1 @ [$2] }
| fun_declaration                                              { [$1] }

fun_declaration:
| tlfunbinding                                                 { let (bndr,lin,p,l) = $1
                                                                 in with_pos $loc (`Fun (make_untyped_binder bndr,lin,([],p),l,None)) }
| signature tlfunbinding                                       { annotate $loc($1) $1 $loc($2) (`Fun     $2) }
| signature typed_handler_binding                              { annotate $loc($1) $1 $loc($2) (`Handler $2) }
| typed_handler_binding                                        { with_pos $loc (`Handler $1) }

typed_handler_binding:
| handler_depth optional_computation_parameter var
                handler_parameterization                       { let binder = make_untyped_binder $3 in
                                                                 let hnlit  = ($1, $2, fst $4, snd $4) in
                                                                 (binder, hnlit, None) }

optional_computation_parameter:
| /* empty */                                                 { with_pos $sloc `Any }
| LBRACKET pattern RBRACKET                                   { $2 }

perhaps_uinteger:
| /* empty */                                                  { None }
| UINTEGER                                                     { Some $1 }

prefixop:
| PREFIXOP                                                     { with_pos $loc $1 }

postfixop:
| POSTFIXOP                                                    { with_pos $loc $1 }

tlfunbinding:
| FUN var arg_lists perhaps_location block                     { ($2, `Unl, ($3, (with_pos $loc($5) (`Block $5))), $4) }
| LINFUN var arg_lists perhaps_location block                  { ($2, `Lin, ($3, (with_pos $loc($5) (`Block $5))), $4) }
| OP pattern op pattern perhaps_location block                 { ($3, `Unl, ([[$2; $4]], with_pos $loc($6) (`Block $6)), $5) }
| OP prefixop pattern perhaps_location block                   { ($2, `Unl, ([[$3]], with_pos $loc($5) (`Block $5)), $4) }
| OP pattern postfixop perhaps_location block                  { ($3, `Unl, ([[$2]], with_pos $loc($5) (`Block $5)), $4) }

tlvarbinding:
| VAR var perhaps_location EQ exp                              { ($2, $5, $3) }

signature:
| SIG var COLON datatype                                       { $2, datatype $4 }
| SIG op COLON datatype                                        { $2, datatype $4 }

typedecl:
| TYPENAME CONSTRUCTOR typeargs_opt EQ datatype                { with_pos $loc (`Type ($2, $3, datatype $5)) }

typeargs_opt:
| /* empty */                                                  { [] }
| LPAREN varlist RPAREN                                        { $2 }

kind:
| COLONCOLON CONSTRUCTOR LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN
                                                               { full_kind_of $loc $2 $4 $6 }
| COLONCOLON CONSTRUCTOR                                       { kind_of $loc($2) $2 }

subkind:
| COLONCOLON LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN       { full_subkind_of $loc $3 $5 }
| COLONCOLON CONSTRUCTOR                                       { subkind_of $loc($2) $2 }

typearg:
| VARIABLE                                                     { (($1, (`Type, None), `Rigid), None) }
| VARIABLE kind                                                { (attach_kind ($1, $2), None) }

varlist:
| typearg                                                      { [$1] }
| typearg COMMA varlist                                        { $1 :: $3 }

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
| UINTEGER                                                     { `Int $1     }
| UFLOAT                                                       { `Float $1   }
| STRING                                                       { `String $1  }
| TRUE                                                         { `Bool true  }
| FALSE                                                        { `Bool false }
| CHAR                                                         { `Char $1    }

qualified_name:
| CONSTRUCTOR DOT qualified_name_inner                         { $1 :: $3 }

qualified_name_inner:
| CONSTRUCTOR DOT qualified_name_inner                         { $1 :: $3 }
| VARIABLE                                                     { [$1] }

qualified_type_name:
| CONSTRUCTOR DOT separated_nonempty_list(DOT, CONSTRUCTOR)    { $1 :: $3 }

atomic_expression:
| qualified_name                                               { with_pos $loc (`QualifiedVar $1) }
| VARIABLE                                                     { with_pos $loc (`Var          $1) }
| constant                                                     { with_pos $loc (`Constant     $1) }
| parenthesized_thing                                          { $1 }
/* HACK: allows us to support both mailbox receive syntax
and receive for session types. */
| RECEIVE                                                      { with_pos $loc (`Var "receive") }

cp_name:
| VARIABLE                                                     { make_untyped_binder (with_pos $loc $1) }

cp_label:
| CONSTRUCTOR                                                  { $1 }

cp_case:
| CASE cp_label RARROW cp_expression                           { $2, $4 }

cp_cases:
| cp_case                                                      { [$1] }
| cp_case cp_cases                                             { $1 :: $2 }

perhaps_cp_cases:
| /* empty */                                                  { [] }
| cp_cases                                                     { $1 }

perhaps_name:
|                                                              { None }
| cp_name                                                      { Some $1 }

cp_expression:
| LBRACE block_contents RBRACE                                  { with_pos $loc (`Unquote $2) }
| cp_name LPAREN perhaps_name RPAREN DOT cp_expression          { with_pos $loc (`Grab ((name_of_binder $1, None), $3, $6)) }
| cp_name LPAREN perhaps_name RPAREN                            { with_pos $loc (`Grab ((name_of_binder $1, None), $3, cp_unit $loc)) }
| cp_name LBRACKET exp RBRACKET DOT cp_expression               { with_pos $loc (`Give ((name_of_binder $1, None), Some $3, $6)) }
| cp_name LBRACKET exp RBRACKET                                 { with_pos $loc (`Give ((name_of_binder $1, None), Some $3, cp_unit $loc)) }
| cp_name LBRACKET RBRACKET                                     { with_pos $loc (`GiveNothing $1) }
| OFFER cp_name LBRACE perhaps_cp_cases RBRACE                  { with_pos $loc (`Offer ($2, $4)) }
| cp_label cp_name DOT cp_expression                            { with_pos $loc (`Select ($2, $1, $4)) }
| cp_label cp_name                                              { with_pos $loc (`Select ($2, $1, cp_unit $loc)) }
| cp_name LRARROW cp_name                                       { with_pos $loc (`Link ($1, $3)) }
| NU cp_name DOT LPAREN cp_expression VBAR cp_expression RPAREN { with_pos $loc (`Comp ($2, $5, $7)) }

primary_expression:
| atomic_expression                                            { $1 }
| LBRACKET RBRACKET                                            { with_pos $loc (`ListLit ([], None)) }
| LBRACKET exps RBRACKET                                       { with_pos $loc (`ListLit ($2, None)) }
| LBRACKET exp DOTDOT exp RBRACKET                             { with_pos $loc (`RangeLit($2, $4))   }
| xml                                                          { $1 }
| FUN arg_lists block                                          { with_pos $loc     (`FunLit (None, `Unl, ($2,
                                                                 with_pos $loc($3) (`Block $3)), `Unknown)) }
| LINFUN arg_lists block                                       { with_pos $loc     (`FunLit (None, `Lin, ($2,
                                                                 with_pos $loc($3) (`Block $3)), `Unknown)) }
| LEFTTRIANGLE cp_expression RIGHTTRIANGLE                     { with_pos $loc (`CP $2) }
| handler_depth optional_computation_parameter handler_parameterization
                                                               {  let (body, args) = $3 in
                                                                  let hnlit = ($1, $2, body, args) in
                                                                  with_pos $loc (`HandlerLit hnlit) }
handler_parameterization:
| handler_body                         { ($1, None) }
| arg_lists handler_body               { ($2, Some $1) }

handler_depth:
| HANDLER                    { `Deep }
| SHALLOWHANDLER             { `Shallow }

handler_body:
| LBRACE cases RBRACE                                          { $2 }

constructor_expression:
| CONSTRUCTOR                                                  { with_pos $loc (`ConstructorLit($1, None   , None)) }
| CONSTRUCTOR parenthesized_thing                              { with_pos $loc (`ConstructorLit($1, Some $2, None)) }

parenthesized_thing:
| LPAREN binop RPAREN                                          { with_pos $loc (`Section $2)              }
| LPAREN DOT record_label RPAREN                               { with_pos $loc (`Section (`Project $3))   }
| LPAREN RPAREN                                                { with_pos $loc (`RecordLit ([], None))    }
| LPAREN labeled_exps VBAR exp RPAREN                          { with_pos $loc (`RecordLit ($2, Some $4)) }
| LPAREN labeled_exps RPAREN                                   { with_pos $loc (`RecordLit ($2, None))    }
| LPAREN exps RPAREN                                           { with_pos $loc (`TupleLit ($2))           }
| LPAREN exp WITH labeled_exps RPAREN                          { with_pos $loc (`With ($2, $4))           }

binop:
| MINUS                                                        { `Minus }
| MINUSDOT                                                     { `FloatMinus }
| op                                                           { `Name ($1.node) }

op:
| INFIX0                                                       { with_pos $loc $1 }
| INFIXL0                                                      { with_pos $loc $1 }
| INFIXR0                                                      { with_pos $loc $1 }
| INFIX1                                                       { with_pos $loc $1 }
| INFIXL1                                                      { with_pos $loc $1 }
| INFIXR1                                                      { with_pos $loc $1 }
| INFIX2                                                       { with_pos $loc $1 }
| INFIXL2                                                      { with_pos $loc $1 }
| INFIXR2                                                      { with_pos $loc $1 }
| INFIX3                                                       { with_pos $loc $1 }
| INFIXL3                                                      { with_pos $loc $1 }
| INFIXR3                                                      { with_pos $loc $1 }
| INFIX4                                                       { with_pos $loc $1 }
| INFIXL4                                                      { with_pos $loc $1 }
| INFIXR4                                                      { with_pos $loc $1 }
| INFIX5                                                       { with_pos $loc $1 }
| INFIXL5                                                      { with_pos $loc $1 }
| INFIXR5                                                      { with_pos $loc $1 }
| INFIX6                                                       { with_pos $loc $1 }
| INFIXL6                                                      { with_pos $loc $1 }
| INFIXR6                                                      { with_pos $loc $1 }
| INFIX7                                                       { with_pos $loc $1 }
| INFIXL7                                                      { with_pos $loc $1 }
| INFIXR7                                                      { with_pos $loc $1 }
| INFIX8                                                       { with_pos $loc $1 }
| INFIXL8                                                      { with_pos $loc $1 }
| INFIXR8                                                      { with_pos $loc $1 }
| INFIX9                                                       { with_pos $loc $1 }
| INFIXL9                                                      { with_pos $loc $1 }
| INFIXR9                                                      { with_pos $loc $1 }

spawn_expression:
| SPAWNAT LPAREN exp COMMA block RPAREN                        { with_pos $loc
                                                                 (`Spawn (`Demon, `ExplicitSpawnLocation $3,
                                                                          with_pos $loc($5) (`Block $5), None)) }
| SPAWN block                                                  { with_pos $loc
                                                                 (`Spawn (`Demon, `NoSpawnLocation,
                                                                          with_pos $loc($2) (`Block $2), None)) }
| SPAWNANGELAT LPAREN exp COMMA block RPAREN                   { with_pos $loc
                                                                 (`Spawn (`Angel, (`ExplicitSpawnLocation $3),
                                                                          with_pos $loc($5) (`Block $5), None)) }
| SPAWNANGEL block                                             { with_pos $loc
                                                                 (`Spawn (`Angel, `NoSpawnLocation,
                                                                          with_pos $loc($2) (`Block $2), None)) }
| SPAWNCLIENT block                                            { with_pos $loc
                                                                 (`Spawn (`Demon, (`SpawnClient),
                                                                          with_pos $loc($2) (`Block $2), None)) }
| SPAWNWAIT block                                              { with_pos $loc
                                                                 (`Spawn (`Wait, `NoSpawnLocation,
                                                                          with_pos $loc($2) (`Block $2), None)) }

postfix_expression:
| primary_expression                                           { $1 }
| primary_expression POSTFIXOP                                 { with_pos $loc (`UnaryAppl (([], `Name $2), $1)) }
| block                                                        { with_pos $loc (`Block $1) }
| spawn_expression                                             { $1 }
| QUERY block                                                  { with_pos $loc (`Query (None, with_pos $loc($2) (`Block $2), None)) }
| QUERY LBRACKET exp RBRACKET block                            { with_pos $loc (`Query (Some ($3, with_pos $loc (`Constant (`Int 0))),
                                                                                        with_pos $loc($5) (`Block $5), None)) }
| QUERY LBRACKET exp COMMA exp RBRACKET block                  { with_pos $loc (`Query (Some ($3, $5), with_pos $loc($7) (`Block $7), None)) }
| postfix_expression arg_spec                                  { with_pos $loc (`FnAppl ($1, $2)) }
| postfix_expression DOT record_label                          { with_pos $loc (`Projection ($1, $3)) }


arg_spec:
| LPAREN RPAREN                                                { [] }
| LPAREN exps RPAREN                                           { $2 }

exps:
| exp COMMA exps                                               { $1 :: $3 }
| exp                                                          { [$1] }

unary_expression:
| MINUS unary_expression                                       { with_pos $loc (`UnaryAppl (([], `Minus     ), $2)) }
| MINUSDOT unary_expression                                    { with_pos $loc (`UnaryAppl (([], `FloatMinus), $2)) }
| PREFIXOP unary_expression                                    { with_pos $loc (`UnaryAppl (([], `Name $1   ), $2)) }
| postfix_expression                                           { $1 }
| constructor_expression                                       { $1 }
| DOOP CONSTRUCTOR arg_spec                                    { with_pos $loc (`DoOperation ($2, $3, None)) }
| DOOP CONSTRUCTOR                                             { with_pos $loc (`DoOperation ($2, [], None)) }


infixr_9:
| unary_expression                                             { $1 }
| unary_expression INFIX9 unary_expression                     { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| unary_expression INFIXR9 infixr_9                            { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_9:
| infixr_9                                                     { $1 }
| infixl_9 INFIXL9 infixr_9                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_8:
| infixl_9                                                     { $1 }
| infixl_9 INFIX8  infixl_9                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_9 INFIXR8 infixr_8                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_9 COLONCOLON infixr_8                                 { with_pos $loc (`InfixAppl (([], `Cons   ), $1, $3)) }

infixl_8:
| infixr_8                                                     { $1 }
| infixl_8 INFIXL8 infixr_8                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_7:
| infixl_8                                                     { $1 }
| infixl_8 INFIX7  infixl_8                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_8 INFIXR7 infixr_7                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_7:
| infixr_7                                                     { $1 }
| infixl_7 INFIXL7 infixr_7                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_6:
| infixl_7                                                     { $1 }
| infixl_7 INFIX6  infixl_7                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_7 INFIXR6 infixr_6                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_6:
| infixr_6                                                     { $1 }
| infixl_6 INFIXL6 infixr_6                                    { with_pos $loc (`InfixAppl (([], `Name $2   ), $1, $3)) }
| infixl_6 MINUS infixr_6                                      { with_pos $loc (`InfixAppl (([], `Minus     ), $1, $3)) }
| infixl_6 MINUSDOT infixr_6                                   { with_pos $loc (`InfixAppl (([], `FloatMinus), $1, $3)) }
/* HACK: the type variables should get inserted later... */
| infixl_6 BANG infixr_6                                       { with_pos $loc (`InfixAppl (([], `Name "!"), $1, $3)) }

infixr_5:
| infixl_6                                                     { $1 }
| infixl_6 INFIX5  infixl_6                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_6 INFIXR5 infixr_5                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_5:
| infixr_5                                                     { $1 }
| infixl_5 INFIXL5 infixr_5                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_4:
| infixl_5                                                     { $1 }
| infixl_5 INFIX4    infixl_5                                  { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_5 INFIXR4   infixr_4                                  { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixr_5 EQUALSTILDE regex                                   { let r, flags = $3 in
                                                                 with_pos $loc (`InfixAppl (([], `RegexMatch flags), $1, r)) }

infixl_4:
| infixr_4                                                     { $1 }
| infixl_4 INFIXL4 infixr_4                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_3:
| infixl_4                                                     { $1 }
| infixl_4 INFIX3  infixl_4                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_4 INFIXR3 infixr_3                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_3:
| infixr_3                                                     { $1 }
| infixl_3 INFIXL3 infixr_3                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_2:
| infixl_3                                                     { $1 }
| infixl_3 INFIX2  infixl_3                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_3 INFIXR2 infixr_2                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_2:
| infixr_2                                                     { $1 }
| infixl_2 INFIXL2 infixr_2                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_1:
| infixl_2                                                     { $1 }
| infixl_2 INFIX1  infixl_2                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_2 INFIXR1 infixr_1                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_1:
| infixr_1                                                     { $1 }
| infixl_1 INFIXL1 infixr_1                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixr_0:
| infixl_1                                                     { $1 }
| infixl_1 INFIX0    infixl_1                                  { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }
| infixl_1 INFIXR0   infixr_0                                  { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

infixl_0:
| infixr_0                                                     { $1 }
| infixl_0 INFIXL0 infixr_0                                    { with_pos $loc (`InfixAppl (([], `Name $2), $1, $3)) }

logical_expression:
| infixl_0                                                     { $1 }
| logical_expression BARBAR infixl_0                           { with_pos $loc (`InfixAppl (([], `Or ), $1, $3)) }
| logical_expression AMPAMP infixl_0                           { with_pos $loc (`InfixAppl (([], `And), $1, $3)) }

typed_expression:
| logical_expression                                           { $1 }
| typed_expression COLON datatype                              { with_pos $loc (`TypeAnnotation ($1, datatype $3)) }
| typed_expression COLON datatype LARROW datatype              { with_pos $loc (`Upcast ($1, datatype $3, datatype $5)) }

db_expression:
| typed_expression                                             { $1 }
| DELETE LPAREN table_generator RPAREN perhaps_where           { let pat, phrase = $3 in with_pos $loc (`DBDelete (pat, phrase, $5)) }
| UPDATE LPAREN table_generator RPAREN
         perhaps_where
         SET LPAREN labeled_exps RPAREN                        { let pat, phrase = $3 in with_pos $loc (`DBUpdate(pat, phrase, $5, $8)) }

/* XML */
xml:
| xml_tree                                                     { $1 }

xmlid:
| VARIABLE                                                     { $1 }

attrs:
| block                                                        { [], Some (with_pos $loc (`Block $1)) }
| attr_list                                                    { $1, None }
| attr_list block                                              { $1, Some (with_pos $loc($2) (`Block $2)) }

attr_list:
| attr                                                         { [$1] }
| attr_list attr                                               { $2 :: $1 }

attr:
| xmlid EQ LQUOTE attr_val RQUOTE                              { ($1, $4) }
| xmlid EQ LQUOTE RQUOTE                                       { ($1, [with_pos $loc($3) (`Constant (`String ""))]) }

attr_val:
| block                                                        { [with_pos $loc (`Block $1)] }
| STRING                                                       { [with_pos $loc (`Constant (`String $1))] }
| block attr_val                                               {  with_pos $loc($1) (`Block $1) :: $2 }
| STRING attr_val                                              {  with_pos $loc($1) (`Constant (`String $1)) :: $2}

xml_tree:
| LXML SLASHRXML                                               { with_pos $loc (`Xml ($1, [], None, [])) }
| LXML RXML ENDTAG                                             { ensure_match $loc $1 $3 (with_pos $loc (`Xml ($1, [], None, []))) }
| LXML RXML xml_contents_list ENDTAG                           { ensure_match $loc $1 $4 (with_pos $loc (`Xml ($1, [], None, $3))) }
| LXML attrs RXML ENDTAG                                       { ensure_match $loc $1 $4 (with_pos $loc (`Xml ($1, fst $2, snd $2, []))) }
| LXML attrs SLASHRXML                                         { with_pos $loc (`Xml ($1, fst $2, snd $2, [])) }
| LXML attrs RXML xml_contents_list ENDTAG                     { ensure_match $loc $1 $5 (with_pos $loc (`Xml ($1, fst $2, snd $2, $4))) }

xml_contents_list:
| xml_contents                                                 { [$1] }
| xml_contents xml_contents_list                               { $1 :: $2 }

xml_contents:
| block                                                        { with_pos $loc (`Block $1) }
| formlet_binding                                              { $1 }
| formlet_placement                                            { $1 }
| page_placement                                               { $1 }
| xml_tree                                                     { $1 }
| CDATA                                                        { with_pos $loc (`TextNode (Utility.xml_unescape $1)) }

formlet_binding:
| LBRACE logical_expression RARROW pattern RBRACE              { with_pos $loc (`FormBinding($2, $4)) }

formlet_placement:
| LBRACE logical_expression
         FATRARROW logical_expression RBRACE                   { with_pos $loc (`FormletPlacement ($2, $4,
                                                                   with_pos $loc (`ListLit ([], None)))) }
| LBRACE logical_expression
         FATRARROW logical_expression
         WITH logical_expression RBRACE                        { with_pos $loc (`FormletPlacement ($2, $4, $6)) }

page_placement:
| LBRACEBAR exp BARRBRACE                                      { with_pos $loc($2) (`PagePlacement $2) }

session_expression:
| db_expression                                                { $1 }
| SELECT field_label exp                                       { with_pos $loc (`Select ($2, $3))      }
| OFFER LPAREN exp RPAREN LBRACE perhaps_cases RBRACE          { with_pos $loc (`Offer ($3, $6, None)) }

conditional_expression:
| session_expression                                           { $1 }
| IF LPAREN exp RPAREN exp ELSE exp                            { with_pos $loc (`Conditional ($3, $5, $7)) }

cases:
| case                                                         { [$1] }
| case cases                                                   { $1 :: $2 }

case:
| CASE pattern RARROW block_contents                           { $2, with_pos $loc($4) (`Block ($4)) }

perhaps_cases:
| /* empty */                                                  { [] }
| cases                                                        { $1 }

case_expression:
| conditional_expression                                       { $1 }
| SWITCH LPAREN exp RPAREN LBRACE perhaps_cases RBRACE         { with_pos $loc (`Switch ($3, $6, None)) }
| RECEIVE LBRACE perhaps_cases RBRACE                          { with_pos $loc (`Receive ($3, None)) }
| SHALLOWHANDLE LPAREN exp RPAREN LBRACE cases RBRACE          { with_pos $loc (`Handle (make_untyped_handler $3 $6 `Shallow)) }
| HANDLE LPAREN exp RPAREN LBRACE perhaps_cases RBRACE         { with_pos $loc (`Handle (make_untyped_handler $3 $6 `Deep   )) }
| HANDLE LPAREN exp RPAREN LPAREN handle_params RPAREN LBRACE perhaps_cases RBRACE
                                                               { with_pos $loc (`Handle (make_untyped_handler ~parameters:(List.rev $6)
                                                                                         $3 $9 `Deep)) }
| RAISE                                                        { with_pos $loc (`Raise) }
| TRY exp AS pattern IN exp OTHERWISE exp                      { with_pos $loc (`TryInOtherwise ($2, $4, $6, $8, None)) }

handle_params:
| logical_expression RARROW pattern { [($1, $3)] }
| handle_params COMMA logical_expression RARROW pattern  { ($3,$5) :: $1 }

iteration_expression:
| case_expression                                              { $1 }
| FOR LPAREN perhaps_generators RPAREN
      perhaps_where
      perhaps_orderby
      exp                                                      { with_pos $loc (`Iteration ($3, $7, $5, $6)) }

perhaps_generators:
| /* empty */                                                  { [] }
| generators                                                   { $1 }

generators:
| generator                                                    { [$1] }
| generator COMMA generators                                   { $1 :: $3 }

generator:
| list_generator                                               { `List $1 }
| table_generator                                              { `Table $1 }

list_generator:
| pattern LARROW exp                                           { ($1, $3) }

table_generator:
| pattern LLARROW exp                                          { ($1, $3) }

perhaps_where:
| /* empty */                                                  { None }
| WHERE LPAREN exp RPAREN                                      { Some $3 }

perhaps_orderby:
| /* empty */                                                  { None }
| ORDERBY LPAREN exps RPAREN                                   { Some (make_tuple $loc($3) $3) }

escape_expression:
| iteration_expression                                         { $1 }
| ESCAPE var IN postfix_expression                             { with_pos $loc (`Escape (make_untyped_binder $2, $4)) }

formlet_expression:
| escape_expression                                            { $1 }
| FORMLET xml YIELDS exp                                       { with_pos $loc (`Formlet ($2, $4)) }
| PAGE xml                                                     { with_pos $loc (`Page $2) }

table_expression:
| formlet_expression                                           { $1 }
| TABLE exp WITH datatype perhaps_table_constraints FROM exp   { with_pos $loc (`TableLit ($2, datatype $4, $5,
                                                                                           with_pos $loc (`ListLit ([], None)), $7)) }
/* SAND */
| TABLE exp WITH datatype perhaps_table_constraints
            TABLEKEYS exp FROM exp                             { with_pos $loc (`TableLit ($2, datatype $4, $5, $7, $9))}

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
| DEFAULT                                                      { `Default }

perhaps_db_args:
| atomic_expression                                            { Some $1 }
| /* empty */                                                  { None }

perhaps_db_driver:
| atomic_expression perhaps_db_args                            { Some $1, $2 }
| /* empty */                                                  { None, None }

database_expression:
| table_expression                                             { $1 }
| INSERT exp VALUES LPAREN RPAREN exp                          { with_pos $loc (`DBInsert ($2, [], $6, None)) }
| INSERT exp VALUES LPAREN record_labels RPAREN exp            { with_pos $loc (`DBInsert ($2, $5, $7, None)) }
| INSERT exp VALUES
  LBRACKET LPAREN labeled_exps RPAREN RBRACKET                 { with_pos $loc (`DBInsert ($2,
                                                                          labels $6,
                                                                          with_pos $loc($6)
                                                                                   (`ListLit ([with_pos $loc($6)
                                                                                                        (`RecordLit ($6, None))], None)),
                                                                          None)) }
| INSERT exp VALUES LPAREN RPAREN db_expression
  RETURNING VARIABLE                                           { with_pos $loc (`DBInsert ($2, [], $6,
                                                                          Some (with_pos $loc($8) (`Constant (`String $8))))) }
| INSERT exp VALUES LPAREN record_labels RPAREN db_expression
  RETURNING VARIABLE                                           { with_pos $loc (`DBInsert ($2, $5, $7,
                                                                          Some (with_pos $loc($9) (`Constant (`String $9))))) }
| INSERT exp VALUES
  LBRACKET LPAREN RPAREN RBRACKET
  RETURNING VARIABLE                                           { with_pos $loc (`DBInsert ($2,
                                                                          [],
                                                                          (with_pos $loc (`ListLit ([with_pos $loc (`RecordLit ([], None))],
                                                                                                    None))),
                                                                          Some (with_pos $loc($9) (`Constant (`String $9))))) }
| INSERT exp VALUES
  LBRACKET LPAREN labeled_exps RPAREN RBRACKET
  RETURNING VARIABLE                                           { with_pos $loc (`DBInsert ($2,
                                                                          labels $6,
                                                                          (with_pos $loc($6) (`ListLit ([with_pos $loc($6)
                                                                                                                  (`RecordLit ($6, None))],
                                                                                                        None))),
                                                                          Some (with_pos $loc (`Constant (`String $10))))) }
| DATABASE atomic_expression perhaps_db_driver                 { with_pos $loc (`DatabaseLit ($2, $3)) }

fn_dep_cols:
| VARIABLE                                                     { [$1] }
| VARIABLE fn_dep_cols                                         { $1 :: $2 }

fn_dep:
| fn_dep_cols RARROW fn_dep_cols                               { ($1, $3) }

fn_deps:
| fn_dep                                                       { [ $1 ] }
| fn_dep COMMA fn_deps                                         { $1 :: $3 }

lens_expression:
| database_expression                                          { $1 }
| LENS exp DEFAULT                                             { with_pos $loc (`LensLit ($2, None))}
| LENS exp TABLEKEYS exp                                       { with_pos $loc (`LensKeysLit ($2, $4, None))}
| LENS exp WITH LBRACE fn_deps RBRACE                          { with_pos $loc (`LensFunDepsLit ($2, $5, None))}
| LENSDROP VARIABLE DETERMINED BY VARIABLE
    DEFAULT exp FROM exp                                       { with_pos $loc (`LensDropLit ($9, $2, $5, $7, None)) }
| LENSSELECT FROM exp BY exp                                   { with_pos $loc (`LensSelectLit ($3, $5, None)) }
| LENSJOIN exp WITH exp ON exp DELETE LBRACE exp COMMA exp RBRACE  { with_pos $loc (`LensJoinLit ($2, $4, $6, $9, $11, None)) }
| LENSJOIN exp WITH exp ON exp DELETE_LEFT                     { with_pos $loc (`LensJoinLit ($2, $4, $6,
                                                                                       with_pos $loc (`Constant (`Bool true )),
                                                                                       with_pos $loc (`Constant (`Bool false)), None)) }
| LENSGET exp                                                  { with_pos $loc (`LensGetLit ($2, None)) }
| LENSPUT exp WITH exp                                         { with_pos $loc (`LensPutLit ($2, $4, None)) }


record_labels:
| record_label COMMA record_labels                             { $1 :: $3 }
| record_label                                                 { [$1] }

links_open:
| OPEN separated_nonempty_list(DOT, CONSTRUCTOR)               { with_pos $loc (`QualifiedImport $2) }

binding:
| VAR pattern EQ exp SEMICOLON                                 { with_pos $loc (`Val ([], $2, $4, `Unknown, None)) }
| exp SEMICOLON                                                { with_pos $loc (`Exp $1) }
| signature FUN var arg_lists block                            {  annotate $loc($1) $1 $loc
                                                                  (`Fun ($3, `Unl, ($4, with_pos $loc($5) (`Block $5)), `Unknown)) }
| signature LINFUN var arg_lists block                         {  annotate $loc($1) $1 $loc
                                                                  (`Fun ($3, `Lin, ($4, with_pos $loc($5) (`Block $5)), `Unknown)) }
| FUN var arg_lists block                                      { with_pos $loc (`Fun (make_untyped_binder $2,
                                                                                  `Unl, ([], ($3, with_pos $loc($4) (`Block $4))), `Unknown,
                                                                                  None)) }
| LINFUN var arg_lists block                                   { with_pos $loc (`Fun (make_untyped_binder $2,
                                                                                  `Lin, ([], ($3, with_pos $loc($4) (`Block $4))), `Unknown,
                                                                                  None)) }
| typedecl SEMICOLON                                           { $1 }
| typed_handler_binding                                        { with_pos $loc (`Handler $1) }
| links_module                                                 { $1 }
| alien_block                                                  { $1 }
| links_open                                                   { $1 }

bindings:
| binding                                                      { [$1] }
| bindings binding                                             { $1 @ [$2] }

moduleblock:
| LBRACE declarations RBRACE                                   { $2 }

block:
| LBRACE block_contents RBRACE                                 { $2 }

block_contents:
| bindings exp SEMICOLON                                       { ($1 @ [with_pos $loc($2) (`Exp $2)],
                                                                  with_pos $loc (`RecordLit ([], None))) }
| bindings exp                                                 { ($1, $2) }
| exp SEMICOLON                                                { ([with_pos $loc($1) (`Exp $1)],
                                                                  with_pos $loc (`RecordLit ([], None))) }
| exp                                                          { ([], $1) }
| perhaps_semi                                                 { ([], with_pos $loc (`TupleLit [])) }

perhaps_semi:
| SEMICOLON                                                    {}
| /* empty */                                                  {}

exp:
| lens_expression                                              { $1 }

labeled_exps:
| record_label EQ exp                                          { [$1, $3] }
| record_label EQ exp COMMA labeled_exps                       { ($1, $3) :: $5 }

/*
 * Datatype grammar
 */
just_datatype:
| datatype END                                                 { $1 }

datatype:
| mu_datatype                                                  { with_pos $loc $1 }
| straight_arrow                                               { with_pos $loc $1 }
| squiggly_arrow                                               { with_pos $loc $1 }

arrow_prefix:
| LBRACE RBRACE                                                { ([], `Closed) }
| LBRACE efields RBRACE                                        { $2 }

straight_arrow_prefix:
| arrow_prefix                                                 { $1 }
| MINUS nonrec_row_var                                         { ([], $2) }
| MINUS kinded_nonrec_row_var                                  { ([], $2) }

squig_arrow_prefix:
| hear_arrow_prefix                                            { $1 }
| arrow_prefix                                                 { $1 }
| TILDE nonrec_row_var                                         { ([], $2) }
| TILDE kinded_nonrec_row_var                                  { ([], $2) }

hear_arrow_prefix:
| LBRACE COLON datatype COMMA efields RBRACE                   { row_with
                                                                   ("wild", `Present (with_dummy_pos `Unit))
                                                                   (row_with ("hear", `Present $3) $5) }
| LBRACE COLON datatype RBRACE                                 { ([("wild", `Present (with_dummy_pos `Unit));
                                                                   ("hear", `Present $3)], `Closed) }
| LBRACE COLON datatype VBAR nonrec_row_var RBRACE             { ([("wild", `Present (with_dummy_pos `Unit));
                                                                   ("hear", `Present $3)], $5) }
| LBRACE COLON datatype VBAR kinded_nonrec_row_var RBRACE      { ([("wild", `Present (with_dummy_pos `Unit));
                                                                   ("hear", `Present $3)], $5) }

straight_arrow:
| parenthesized_datatypes
  straight_arrow_prefix RARROW datatype                        { `Function ($1, $2, $4) }
| parenthesized_datatypes
  straight_arrow_prefix LOLLI datatype                         { `Lolli ($1, $2, $4) }
| parenthesized_datatypes RARROW datatype                      { `Function ($1, ([], fresh_rigid_row_variable None),
                                                                            $3) }
| parenthesized_datatypes LOLLI datatype                       { `Lolli ($1, ([], fresh_rigid_row_variable None), $3) }

squiggly_arrow:
| parenthesized_datatypes
  squig_arrow_prefix SQUIGRARROW datatype                      { `Function ($1, row_with
                                                                                  ("wild", `Present (with_dummy_pos `Unit))
                                                                                $2,
                                                                            $4) }
| parenthesized_datatypes
  squig_arrow_prefix SQUIGLOLLI datatype                       { `Lolli ($1, row_with ("wild", `Present (with_dummy_pos `Unit))
                                                                             $2,
                                                                         $4) }
/*| parenthesized_datatypes hear_arrow_prefix
  SQUIGRARROW datatype                                         { `Function ($1, $2, $4) }
*/
| parenthesized_datatypes SQUIGRARROW datatype                 { `Function ($1, ([("wild", `Present (with_dummy_pos `Unit))],
                                                                                 fresh_rigid_row_variable None),
                                                                            $3) }
| parenthesized_datatypes SQUIGLOLLI datatype                  { `Lolli ($1, ([("wild", `Present (with_dummy_pos `Unit))],
                                                                              fresh_rigid_row_variable None),
                                                                         $3) }

mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { `Mu ($2, with_pos $loc($4) $4) }
| forall_datatype                                              { $1 }

forall_datatype:
| FORALL varlist DOT datatype                                  { `Forall (List.map fst $2, $4) }
| session_datatype                                             { $1 }

/* Parenthesised dts disambiguate between sending qualified types and recursion variables.
   e.g:

     S = !ModuleA.ModuleB.Type.S
     should be written
     S = !(ModuleA.ModuleB.Type).S

     Parenthesised versions take priority over non-parenthesised versions.
*/
session_datatype:
| BANG datatype DOT datatype                                   { `Output ($2, $4) }
| QUESTION datatype DOT datatype                               { `Input  ($2, $4) }
| LBRACKETPLUSBAR row BARPLUSRBRACKET                          { `Select $2 }
| LBRACKETAMPBAR row BARAMPRBRACKET                            { `Choice $2 }
| TILDE datatype                                               { `Dual $2 }
| END                                                          { `End }
| primary_datatype                                             { $1 }

parenthesized_datatypes:
| LPAREN RPAREN                                                { [] }
| LPAREN qualified_type_name RPAREN                            { [with_pos $loc (`QualifiedTypeApplication ($2, []))] }
| LPAREN datatypes RPAREN                                      { $2 }

primary_datatype:
| parenthesized_datatypes                                      { match $1 with
                                                                   | [] -> `Unit
                                                                   | [{ node  ; _ }] -> node
                                                                   | ts  -> `Tuple ts }
| LPAREN rfields RPAREN                                        { `Record $2 }
| TABLEHANDLE
     LPAREN datatype COMMA datatype COMMA datatype RPAREN      { `Table ($3, $5, $7) }
/* | TABLEHANDLE datatype perhaps_table_constraints            { `Table ($2, $3) } */

| LBRACKETBAR vrow BARRBRACKET                                 { `Variant $2 }
| LBRACKET datatype RBRACKET                                   { `List $2 }
| type_var                                                     { $1 }
| kinded_type_var                                              { $1 }
| CONSTRUCTOR                                                  { match $1 with
                                                                   | "Bool"    -> `Primitive `Bool
                                                                   | "Int"     -> `Primitive `Int
                                                                   | "Char"    -> `Primitive `Char
                                                                   | "Float"   -> `Primitive `Float
                                                                   | "XmlItem" -> `Primitive `XmlItem
                                                                   | "String"  -> `Primitive `String
                                                                   | "Database"-> `DB
                                                                   | t         -> `TypeApplication (t, [])
                                                               }
| CONSTRUCTOR LPAREN type_arg_list RPAREN                      { `TypeApplication ($1, $3) }

type_var:
| VARIABLE                                                     { `TypeVar ($1, None, `Rigid) }
| PERCENTVAR                                                   { `TypeVar ($1, None, `Flexible) }
| UNDERSCORE                                                   { fresh_rigid_type_variable None }
| PERCENT                                                      { fresh_type_variable None }

kinded_type_var:
| type_var subkind                                             { attach_subkind ($1, $2) }

type_arg_list:
| type_arg                                                     { [$1] }
| type_arg COMMA type_arg_list                                 { $1 :: $3 }

/* TODO: fix the syntax for type arguments
   (TYPE, ROW, and PRESENCE are no longer tokens...)
*/
type_arg:
| datatype                                                     { `Type $1 }
| TYPE LPAREN datatype RPAREN                                  { `Type $3 }
| ROW LPAREN row RPAREN                                        { `Row $3 }
| PRESENCE LPAREN fieldspec RPAREN                             { `Presence $3 }
| LBRACE row RBRACE                                            { `Row $2 }

vrow:
| vfields                                                      { $1 }
| /* empty */                                                  { [], `Closed }

datatypes:
| datatype                                                     { [$1] }
| datatype COMMA datatypes                                     { $1 :: $3 }

row:
| fields                                                       { $1 }
| /* empty */                                                  { [], `Closed }

fields:
| field                                                        { [$1], `Closed }
| field VBAR row_var                                           { [$1], $3 }
| field VBAR kinded_row_var                                    { [$1], $3 }
| VBAR row_var                                                 { [], $2 }
| VBAR kinded_row_var                                          { [], $2 }
| field COMMA fields                                           { $1 :: fst $3, snd $3 }

field:
| field_label                                                  { $1, `Present (with_pos $loc `Unit) }
| field_label fieldspec                                        { $1, $2 }

field_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }
| STRING                                                       { $1 }
| UINTEGER                                                     { string_of_int $1 }

rfields:
| rfield                                                       { [$1], `Closed }
| rfield VBAR row_var                                          { [$1], $3 }
| rfield VBAR kinded_row_var                                   { [$1], $3 }
| VBAR row_var                                                 { [], $2 }
| VBAR kinded_row_var                                          { [], $2 }
| rfield COMMA rfields                                         { $1 :: fst $3, snd $3 }

rfield:
/* The following sugar is tempting, but it leads to a conflict. Is
   the type (a,b,c) a record with fields a, b, c or a polymorphic tuple
   with type variables a, b, c?
*/
/*| record_label                                               { $1, `Present `Unit } */
| record_label fieldspec                                       { $1, $2 }

record_label:
| field_label                                                  { $1 }

vfields:
| vfield                                                       { [$1], `Closed }
| row_var                                                      { [], $1 }
| kinded_row_var                                               { [], $1 }
| vfield VBAR vfields                                          { $1 :: fst $3, snd $3 }

vfield:
| CONSTRUCTOR                                                  { $1, `Present (with_pos $loc `Unit) }
| CONSTRUCTOR fieldspec                                        { $1, $2 }

efields:
| efield                                                       { [$1], `Closed }
| efield VBAR nonrec_row_var                                   { [$1], $3 }
| efield VBAR kinded_nonrec_row_var                            { [$1], $3 }
| VBAR nonrec_row_var                                          { [], $2 }
| VBAR kinded_nonrec_row_var                                   { [], $2 }
| efield COMMA efields                                         { $1 :: fst $3, snd $3 }

efield:
| effect_label                                                 { $1, `Present (with_pos $loc `Unit) }
| effect_label fieldspec                                       { $1, $2 }

effect_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }

fieldspec:
| COLON datatype                                               { `Present $2 }
| LBRACE COLON datatype RBRACE                                 { `Present $3 }
| MINUS                                                        { `Absent }
| LBRACE MINUS RBRACE                                          { `Absent }
| LBRACE VARIABLE RBRACE                                       { `Var ($2, None, `Rigid) }
| LBRACE PERCENTVAR RBRACE                                     { `Var ($2, None, `Flexible) }
| LBRACE UNDERSCORE RBRACE                                     { fresh_rigid_presence_variable None }
| LBRACE PERCENT RBRACE                                        { fresh_presence_variable None }

nonrec_row_var:
| VARIABLE                                                     { `Open ($1, None, `Rigid) }
| PERCENTVAR                                                   { `Open ($1, None, `Flexible) }
| UNDERSCORE                                                   { fresh_rigid_row_variable None }
| PERCENT                                                      { fresh_row_variable None }

/* FIXME:
 *
 * recursive row vars shouldn't be restricted to vfields.
 */
row_var:
| nonrec_row_var                                               { $1 }
| LPAREN MU VARIABLE DOT vfields RPAREN                        { `Recursive ($3, $5) }

kinded_nonrec_row_var:
| nonrec_row_var subkind                                       { attach_row_subkind ($1, $2) }

kinded_row_var:
| row_var subkind                                              { attach_row_subkind ($1, $2) }

/*
 * Regular expression grammar
 */
regex:
| SLASH regex_pattern_alternate regex_flags_opt                                  { with_pos $loc($2) (`Regex $2), $3 }
| SLASH regex_flags_opt                                                          { with_pos $loc (`Regex (`Simply "")), $2 }
| SSLASH regex_pattern_alternate SLASH regex_replace regex_flags_opt             { with_pos $loc (`Regex (`Replace ($2, $4))),
                                                                                   `RegexReplace :: $5 }

regex_flags_opt:
| SLASH                                                        {[]}
| SLASHFLAGS                                                   {parseRegexFlags $1}

regex_replace:
| /* empty */                                                  { `Literal ""}
| REGEXREPL                                                    { `Literal $1}
| block                                                        { `Splice (with_pos $loc (`Block $1)) }

regex_pattern:
| RANGE                                                        { `Range $1 }
| STRING                                                       { `Simply $1 }
| QUOTEDMETA                                                   { `Quote (`Simply $1) }
| DOT                                                          { `Any }
| CARET                                                        { `StartAnchor }
| DOLLAR                                                       { `EndAnchor }
| LPAREN regex_pattern_alternate RPAREN                        { `Group $2 }
| regex_pattern STAR                                           { `Repeat (Regex.Star, $1) }
| regex_pattern PLUS                                           { `Repeat (Regex.Plus, $1) }
| regex_pattern QUESTION                                       { `Repeat (Regex.Question, $1) }
| block                                                        { `Splice (with_pos $loc (`Block $1)) }

regex_pattern_alternate:
| regex_pattern_sequence                                       { `Seq $1 }
| regex_pattern_sequence ALTERNATE regex_pattern_alternate     { `Alternate (`Seq $1, $3) }

regex_pattern_sequence:
| regex_pattern                                                { [$1] }
| regex_pattern regex_pattern_sequence                         { $1 :: $2 }

/*
 * Pattern grammar
 */
pattern:
| typed_pattern                                             { $1 }
| typed_pattern COLON primary_datatype                      { with_pos $loc (`HasType ($1, datatype (with_pos $loc($3) $3))) }

typed_pattern:
| cons_pattern                                              { $1 }
| cons_pattern AS var                                       { with_pos $loc (`As (make_untyped_binder $3, $1)) }

cons_pattern:
| constructor_pattern                                       { $1 }
| constructor_pattern COLONCOLON cons_pattern               { with_pos $loc (`Cons ($1, $3)) }

constructor_pattern:
| negative_pattern                                          { $1 }
| CONSTRUCTOR                                               { with_pos $loc (`Variant ($1, None)) }
| CONSTRUCTOR parenthesized_pattern                         { with_pos $loc (`Variant ($1, Some $2)) }

constructors:
| CONSTRUCTOR                                               { [$1] }
| CONSTRUCTOR COMMA constructors                            { $1 :: $3 }

negative_pattern:
| primary_pattern                                           { $1 }
| MINUS CONSTRUCTOR                                         { with_pos $loc (`Negative [$2]) }
| MINUS LPAREN constructors RPAREN                          { with_pos $loc (`Negative $3) }

parenthesized_pattern:
| LPAREN RPAREN                                             { with_pos $loc (`Tuple []) }
| LPAREN pattern RPAREN                                     { $2 }
| LPAREN pattern COMMA patterns RPAREN                      { with_pos $loc (`Tuple ($2 :: $4)) }
| LPAREN labeled_patterns VBAR pattern RPAREN               { with_pos $loc (`Record ($2, Some $4)) }
| LPAREN labeled_patterns RPAREN                            { with_pos $loc (`Record ($2, None)) }

primary_pattern:
| VARIABLE                                                  { with_pos $loc (`Variable (make_untyped_binder (with_pos $loc $1))) }
| UNDERSCORE                                                { with_pos $loc `Any }
| constant                                                  { with_pos $loc (`Constant $1) }
| LBRACKET RBRACKET                                         { with_pos $loc `Nil }
| LBRACKET patterns RBRACKET                                { with_pos $loc (`List $2) }
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
| multi_args                                                { [$1] }
| multi_args arg_lists                                      { $1 :: $2 }
