(*

Note [Debugging grammar conflicts]
==================================

It might happen that after modifying the grammar Menhir reports new conflicts.
To debug these go to core/dune file and add flags --dump and --explain:

  (menhir
    (modules parser jsonparse xmlParser)
    (flags "--table" "--dump" "--explain")
  )

--dump flag generates *.automaton files in _build/default/core directory.  These
files contains human-readable description of the parser automaton, including
explanation of the conflicts, located at the end of file.  --explain flag
generates *.conflicts files that contain explanation of conflicts for each
parser.  Note that this does not include end-of-stream conflicts, which are
explained in *.automaton files.

Once done with debugging remember to disable the flags!

*)

%{

open CommonTypes
open Utility
open Operators
open SourceCode
open Sugartypes
open SugarConstructors

(* Workaround path bug in OCaml 4.07 when using menhir and dune
(c.f. https://github.com/ocaml/dune/issues/1504). *)
module Links_core = struct end

(* Construction of nodes using positions produced by Menhir parser *)
module ParserPosition
       : Pos with type t = (SourceCode.Lexpos.t * SourceCode.Lexpos.t) = struct
  (* parser position produced by Menhir *)
  type t = SourceCode.Lexpos.t * SourceCode.Lexpos.t
  (* Convert position produced by a parser to SourceCode position *)
  let pos (start, finish) = SourceCode.Position.make ~start ~finish ~code:None
  (* Wrapper around SourceCode.WithPos.make.  Accepts parser positions. *)
  let with_pos p v = SourceCode.WithPos.make ~pos:(pos p) v
  (* Default (dummy) parser position *)
  let dp = (Lexing.dummy_pos, Lexing.dummy_pos)
end

(* Instantiate SugarConstructors functor and open it to avoid explicit name
   qualification. *)
module ParserConstructors = SugarConstructors(ParserPosition)
open ParserConstructors

let default_fixity = 9

let primary_kind_of_string p =
  function
  | "Type"     -> pk_type
  | "Row"      -> pk_row
  | "Presence" -> pk_presence
  | pk         ->
     raise (ConcreteSyntaxError (pos p, "Invalid primary kind: " ^ pk))

let linearity_of_string p =
  function
  | "Any" -> lin_any
  | "Unl" -> lin_unl
  | lin   ->
     raise (ConcreteSyntaxError (pos p, "Invalid kind linearity: " ^ lin))

let restriction_of_string p =
  function
  | "Any"     -> res_any
  | "Base"    -> res_base
  | "Session" -> res_session
  | "Mono"    -> res_mono
  | rest      ->
     raise (ConcreteSyntaxError (pos p, "Invalid kind restriction: " ^ rest))

let query_policy_of_string p =
  function
  | "flat" -> QueryPolicy.Flat
  | "nested" -> QueryPolicy.Nested
  | rest      ->
     raise (ConcreteSyntaxError (pos p, "Invalid query policy: " ^ rest ^ ", expected 'flat' or 'nested'"))



let full_kind_of pos prim lin rest =
  let p = primary_kind_of_string pos prim in
  let l = linearity_of_string    pos lin  in
  let r = restriction_of_string  pos rest in
  (Some p, Some (l, r))

let full_subkind_of pos lin rest =
  let l = linearity_of_string   pos lin  in
  let r = restriction_of_string pos rest in
  Some (l, r)

(* In kind and subkind abbreviations, we aim to provide the most
common case. For everything except session types, the default
linearity is Unl and the default restriction is Any. For session
types the default linearity is Any. *)

(* Currently "Any" means Any,Any, but it is probably advisable to
change "Any" to something more evocative of linearity - "Lin"
perhaps. *)

let kind_of p =
  function
  (* primary kind abbreviation  *)
  | "Type"     -> (Some pk_type, None)
  | "Row"      -> (Some pk_row, None)
  | "Presence" -> (Some pk_presence, None)
  (* subkind of type abbreviations *)
  | "Any"      -> (Some pk_type, Some (lin_any, res_any))
  | "Base"     -> (Some pk_type, Some (lin_unl, res_base))
  | "Session"  -> (Some pk_type, Some (lin_any, res_session))
  | "Eff"      -> (Some pk_row , Some (lin_unl, res_effect))
  | k          -> raise (ConcreteSyntaxError (pos p, "Invalid kind: " ^ k))

let subkind_of p =
  function
  (* subkind abbreviations *)
  | "Any"     -> Some (lin_any, res_any)
  | "Base"    -> Some (lin_unl, res_base)
  | "Session" -> Some (lin_any, res_session)
  | "Eff"     -> Some (lin_unl, res_effect)
  | sk        -> raise (ConcreteSyntaxError (pos p, "Invalid subkind: " ^ sk))

let named_quantifier name kind freedom = SugarQuantifier.mk_unresolved name kind freedom

let attach_kind (t, k) = SugarQuantifier.mk_unresolved t k `Rigid

let attach_subkind_helper update sk = update sk

let attach_subkind (t, subkind) =
  let update sk =
    match t with
    | Datatype.TypeVar stv ->
       let (x, _, freedom) = SugarTypeVar.get_unresolved_exn stv in
       let stv' = SugarTypeVar.mk_unresolved x sk freedom in
       Datatype.TypeVar stv'
    | _ -> assert false
  in attach_subkind_helper update subkind

let attach_row_subkind (r, subkind) =
  let update sk =
    match r with
    | Datatype.Open stv ->
       let (x, _, freedom) = SugarTypeVar.get_unresolved_exn stv in
       let stv' = SugarTypeVar.mk_unresolved x sk freedom in
       Datatype.Open stv'
    | _ -> assert false
  in attach_subkind_helper update subkind

let labels xs = fst (List.split xs)

let parseRegexFlags f =
  let rec asList f i l =
    if (i == String.length f) then
      List.rev l
    else
      asList f (i+1) ((String.get f i)::l) in
    List.map (function
                'l' -> RegexList
              | 'n' -> RegexNative
              | 'g' -> RegexGlobal
              | _ -> assert false) (asList f 0 [])



let named_typevar name freedom : SugarTypeVar.t =
  SugarTypeVar.mk_unresolved name None freedom

let fresh_typevar freedom : SugarTypeVar.t =
  named_typevar "$" freedom

let fresh_effects =
  let stv = SugarTypeVar.mk_unresolved "$eff" None `Rigid in
  ([], Datatype.Open stv)

module MutualBindings = struct

  type mutual_bindings =
    { mut_types: typename list;
      mut_funs: (function_definition * Position.t) list;
      mut_pos: Position.t }


  let empty pos = { mut_types = []; mut_funs = []; mut_pos = pos }

  let add ({ mut_types = ts; mut_funs = fs; _ } as block) binding =
    let pos = WithPos.pos binding in
    match WithPos.node binding with
    | Fun f ->
        { block with mut_funs = ((f, pos) :: fs) }
    | Typenames [t] ->
        { block with mut_types = (t :: ts) }
    | Typenames _ -> assert false
    | _ ->
        raise (ConcreteSyntaxError
          (pos, "Only `fun` and `typename` bindings are allowed in a `mutual` block."))

  let check_dups funs tys =
    (* Check to see whether there are any duplicate names, and report
     * an error if so. *)
  let check get_name xs =
    let dup_map =
      List.fold_left (fun acc (x, pos) ->
        let name = get_name x in
        StringMap.update name (fun x_opt ->
          OptionUtils.opt_app
            (fun positions -> Some (pos :: positions))
            (Some [pos]) x_opt) acc) StringMap.empty xs in
    let dups =
        StringMap.filter (fun _ poss -> List.length poss > 1) dup_map in
    if StringMap.cardinal dups > 0 then
      raise (Errors.MultiplyDefinedMutualNames dups) in

  let fun_name fn = Binder.to_name fn.fun_binder in
  let ty_name (n, _, _, _) = n in
  let tys_with_pos =
      List.map (fun {WithPos.node=(n, qs, dt); pos} -> ((n, qs, dt, pos), pos))
        tys in
  check fun_name funs; check ty_name tys_with_pos


  let flatten { mut_types; mut_funs; mut_pos } =
    (* We need to take care not to lift non-recursive functions to
     * recursive functions accidentally. *)
    check_dups mut_funs mut_types;
    let fun_binding = function
      | [] -> []
      | [(f, pos)] -> [WithPos.make ~pos (Fun f)]
      | fs ->
          let fs =
            List.rev_map (fun (({ fun_definition = (tvs, fl); _ } as fn), pos) ->
                WithPos.make ~pos
                  { rec_binder           = fn.fun_binder
                  ; rec_linearity        = fn.fun_linearity
                  ; rec_definition       = ((tvs, None), fl)
                  ; rec_location         = fn.fun_location
                  ; rec_signature        = fn.fun_signature
                  ; rec_unsafe_signature = fn.fun_unsafe_signature
                  ; rec_frozen           = fn.fun_frozen}) fs in
          [WithPos.make ~pos:mut_pos (Funs fs)] in

    let type_binding = function
      | [] -> []
      | ts -> [WithPos.make ~pos:mut_pos (Typenames (List.rev ts))] in
    type_binding mut_types @ fun_binding mut_funs
end

let parse_foreign_language pos lang =
  try ForeignLanguage.of_string lang
  with Invalid_argument _ ->
    raise (ConcreteSyntaxError
             (pos, Printf.sprintf "Unrecognised foreign language '%s'." lang))

%}

%token EOF
%token EQ IN
%token FUN LINFUN FROZEN_FUN FROZEN_LINFUN RARROW LOLLI FATRARROW VAR OP
%token SQUIGRARROW SQUIGLOLLI TILDE
%token IF ELSE
%token MINUS MINUSDOT
%token SWITCH RECEIVE CASE
%token HANDLE SHALLOWHANDLE
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
%token LENSPUT LENSGET LENSCHECK LENSSERIAL
%token READONLY DEFAULT
%token ESCAPE
%token CLIENT SERVER
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
%token MU FORALL ALIEN SIG UNSAFE
%token MODULE MUTUAL OPEN IMPORT
%token BANG QUESTION
%token PERCENT EQUALSTILDE PLUS STAR ALTERNATE SLASH SSLASH CARET DOLLAR AT
%token <char*char> RANGE
%token <string> QUOTEDMETA
%token <string> SLASHFLAGS
%token UNDERSCORE AS
%token <Operators.Associativity.t> FIXITY
%token TYPENAME
%token TRY OTHERWISE RAISE
%token <string> OPERATOR

%start just_datatype
%start interactive
%start file

%type <Sugartypes.binding list * Sugartypes.phrase option> file
%type <Sugartypes.Datatype.with_pos> datatype
%type <Sugartypes.Datatype.with_pos> just_datatype
%type <Sugartypes.sentence> interactive
%type <Sugartypes.regex> regex_pattern_alternate
%type <Sugartypes.regex> regex_pattern
%type <Sugartypes.regex list> regex_pattern_sequence
%type <Sugartypes.Pattern.with_pos> pattern
%type <(DeclaredLinearity.t * bool) * Name.t *
       Sugartypes.Pattern.with_pos list list * Location.t *
       Sugartypes.phrase> tlfunbinding
%type <Sugartypes.phrase> postfix_expression
%type <Sugartypes.phrase> primary_expression
%type <Sugartypes.phrase> atomic_expression
%type <Constant.t> constant
%type <(string * Sugartypes.phrase list) list> attr_list
%type <Sugartypes.phrase list> attr_val
%type <Sugartypes.binding> binding

%%

(* soption = singleton option.  Allows an argument to appear optionally.  If it
   does, it is placed in a singleton list.  This combinator is useful when
   defining various *field productions.  *)
%inline soption(X):
| /* nothing */   { []  }
| x = X           { [x] }

interactive:
| nofun_declaration                                            { Definitions [$1] }
| fun_declarations SEMICOLON                                   { Definitions $1   }
| SEMICOLON                                                    { Definitions []   }
| exp SEMICOLON                                                { Expression $1    }
| directive                                                    { Directive $1     }
| EOF                                                          { Directive ("quit", []) (* rather hackish *) }

file:
| EOF                                                          { ([], None) }
| declarations exp? EOF                                        { ($1, $2     ) }
| exp EOF                                                      { ([], Some $1) }

directive:
| KEYWORD args SEMICOLON                                       { ($1, $2) }

args:
| arg*                                                         { $1 }

arg:
| STRING                                                       { $1 }
| VARIABLE                                                     { $1 }
| CONSTRUCTOR                                                  { $1 }
| UINTEGER                                                     { string_of_int    $1 }
| UFLOAT                                                       { string_of_float' $1 }
| TRUE                                                         { "true"  }
| FALSE                                                        { "false" }

var:
| VARIABLE                                                     { with_pos $loc $1 }

mutual_decl_block:
| MUTUAL LBRACE mutual_decls RBRACE                            { MutualBindings.flatten $3 }

mutual_decls:
| declaration                                                  { MutualBindings.(add (empty (pos $loc)) $1) }
| mutual_decls declaration                                     { MutualBindings.add $1 $2 }

declarations:
| declarations mutual_decl_block                               { $1 @ $2 }
| declarations declaration                                     { $1 @ [$2] }
| declaration                                                  { [$1] }
| mutual_decl_block                                            { $1 }

declaration:
| fun_declaration | nofun_declaration                          { $1 }

nofun_declaration:
| alien_block                                                  { $1 }
| ALIEN VARIABLE STRING VARIABLE COLON datatype SEMICOLON      { let alien =
                                                                   let binder = binder ~ppos:$loc($4) $4 in
                                                                   let datatype = datatype $6 in
                                                                   let language = parse_foreign_language (pos $loc($1)) $2 in
                                                                   Alien.single language $3 binder datatype
                                                                 in
                                                                 with_pos $loc (Foreign alien) }
| FIXITY UINTEGER? op SEMICOLON                                { let precedence = from_option default_fixity $2 in
                                                                 let node = Infix { name = WithPos.node $3; precedence; assoc = $1 } in
                                                                 with_pos $loc node }
| signature? tlvarbinding SEMICOLON                            { val_binding' ~ppos:$loc($2) $1 $2 }
| typedecl SEMICOLON | links_module | links_open SEMICOLON     { $1 }
| pollute = boption(OPEN) IMPORT CONSTRUCTOR SEMICOLON         { import ~ppos:$loc($2) ~pollute [$3] }

alien_datatype:
| VARIABLE COLON datatype SEMICOLON                            { (binder ~ppos:$loc($1) $1, datatype $3) }

alien_datatypes:
| alien_datatype+                                              { $1 }

links_module:
| MODULE name = CONSTRUCTOR members = moduleblock              { module_binding ~ppos:$loc($1) (binder ~ppos:$loc(name) name) members }

alien_block:
| ALIEN VARIABLE STRING LBRACE alien_datatypes RBRACE          { let lang = parse_foreign_language (pos $loc($1)) $2 in
                                                                 with_pos $loc (AlienBlock (Alien.multi lang $3 $5)) }

fun_declarations:
| fun_declaration+                                             { $1 }

fun_declaration:
| tlfunbinding                                                 { fun_binding ~ppos:$loc($1) None $1 }
| signatures tlfunbinding                                      { fun_binding ~ppos:$loc($2) (fst $1) ~unsafe_sig:(snd $1) $2 }
| switch_tlfunbinding                                          { switch_fun_binding ~ppos:$loc($1) None $1 }
| signatures switch_tlfunbinding                               { switch_fun_binding ~ppos:$loc($2) (fst $1) ~unsafe_sig:(snd $1) $2 }

linearity:
| FUN                                                          { dl_unl }
| LINFUN                                                       { dl_lin }

fun_kind:
| FUN                                                          { (dl_unl, false) }
| LINFUN                                                       { (dl_lin, false) }
| FROZEN_FUN                                                   { (dl_unl, true) }
| FROZEN_LINFUN                                                { (dl_lin, true) }

tlfunbinding:
| fun_kind VARIABLE arg_lists perhaps_location block           { ($1, $2, $3, $4, $5)                }
| OP pattern sigop pattern perhaps_location block              { ((dl_unl, false), WithPos.node $3, [[$2; $4]], $5, $6) }
| OP OPERATOR pattern perhaps_location block                   { ((dl_unl, false), $2, [[$3]], $4, $5)          }
| OP pattern OPERATOR perhaps_location block                   { ((dl_unl, false), $3, [[$2]], $4, $5)          }

switch_tlfunbinding:
| fun_kind VARIABLE arg_lists perhaps_location switch_funlit_body     { ($1, $2, $3, $4, $5)   }

switch_funlit_body:
| SWITCH LBRACE case+ RBRACE                                   { $3 }

tlvarbinding:
| VAR VARIABLE perhaps_location EQ exp                         { (PatName $2, $5, $3) }

signatures:
| signature                                                    { (Some $1, false) }
| UNSAFE signature                                             { (Some $2, true) }

signature:
| SIG var COLON datatype                                       { with_pos $loc ($2, datatype $4) }
| SIG sigop COLON datatype                                     { with_pos $loc ($2, datatype $4) }

typedecl:
| TYPENAME CONSTRUCTOR typeargs_opt EQ datatype                { with_pos $loc (Typenames [with_pos $loc ($2, $3, datatype $5)]) }

typeargs_opt:
| /* empty */                                                  { [] }
| LPAREN varlist RPAREN                                        { $2 }

kind:
| COLONCOLON CONSTRUCTOR LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN
                                                               { full_kind_of $loc $2 $4 $6 }
| COLONCOLON CONSTRUCTOR                                       { kind_of $loc($2) $2        }

subkind:
| COLONCOLON LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN       { full_subkind_of $loc $3 $5 }
| COLONCOLON CONSTRUCTOR                                       { subkind_of $loc($2) $2     }

typearg:
| VARIABLE                                                     { (named_quantifier $1 (None, None) `Rigid) }
| VARIABLE kind                                                { attach_kind ($1, $2)                            }

varlist:
| separated_nonempty_list(COMMA, typearg)                      { $1 }

perhaps_location:
| SERVER                                                       { loc_server  }
| CLIENT                                                       { loc_client  }
| /* empty */                                                  { loc_unknown }

constant:
| UINTEGER                                                     { Constant.Int    $1  }
| UFLOAT                                                       { Constant.Float  $1  }
| STRING                                                       { Constant.String $1  }
| TRUE                                                         { Constant.Bool true  }
| FALSE                                                        { Constant.Bool false }
| CHAR                                                         { Constant.Char $1    }

qualified_name:
| CONSTRUCTOR DOT qualified_name_inner                         { $1 :: $3 }

qualified_name_inner:
| CONSTRUCTOR DOT qualified_name_inner                         { $1 :: $3 }
| VARIABLE                                                     { [$1]     }

qualified_type_name:
| CONSTRUCTOR DOT separated_nonempty_list(DOT, CONSTRUCTOR)    { $1 :: $3 }

atomic_expression:
| qualified_name                                               { with_pos $loc (QualifiedVar $1) }
| VARIABLE                                                     { with_pos $loc (Var          $1) }
| TILDE VARIABLE                                               { with_pos $loc (FreezeVar    $2) }
| constant                                                     { with_pos $loc (Constant     $1) }
| parenthesized_thing                                          { $1 }
/* HACK: allows us to support both mailbox receive syntax
and receive for session types. */
| RECEIVE                                                      { with_pos $loc (Var "receive") }

cp_name:
| VARIABLE                                                     { binder ~ppos:$loc($1) $1 }

cp_label:
| CONSTRUCTOR                                                  { $1 }

cp_case:
| CASE cp_label RARROW cp_expression                           { ($2, $4) }

cp_cases:
| cp_case+                                                     { $1 }

perhaps_cp_cases:
| loption(cp_cases)                                            { $1 }

perhaps_name:
| cp_name?                                                     { $1 }

cp_expression:
| LBRACE block_contents RBRACE                                 { with_pos $loc (CPUnquote (fst $2, snd $2)) }
| cp_name LPAREN perhaps_name RPAREN DOT cp_expression         { with_pos $loc (CPGrab ((Binder.to_name $1, None), $3, $6)) }
| cp_name LPAREN perhaps_name RPAREN                           { with_pos $loc (CPGrab ((Binder.to_name $1, None), $3, cp_unit $loc)) }
| cp_name LBRACKET exp RBRACKET DOT cp_expression              { with_pos $loc (CPGive ((Binder.to_name $1, None), Some $3, $6)) }
| cp_name LBRACKET exp RBRACKET                                { with_pos $loc (CPGive ((Binder.to_name $1, None), Some $3, cp_unit $loc)) }
| cp_name LBRACKET RBRACKET                                    { with_pos $loc (CPGiveNothing $1) }
| OFFER cp_name LBRACE perhaps_cp_cases RBRACE                 { with_pos $loc (CPOffer ($2, $4)) }
| cp_label cp_name DOT cp_expression                           { with_pos $loc (CPSelect ($2, $1, $4)) }
| cp_label cp_name                                             { with_pos $loc (CPSelect ($2, $1, cp_unit $loc)) }
| cp_name LRARROW cp_name                                      { with_pos $loc (CPLink ($1, $3)) }
| NU cp_name DOT LPAREN cp_expression VBAR cp_expression RPAREN{ with_pos $loc (CPComp ($2, $5, $7)) }

primary_expression:
| atomic_expression                                            { $1 }
| LBRACKET perhaps_exps RBRACKET                               { list ~ppos:$loc $2 }
| LBRACKET exp DOTDOT exp RBRACKET                             { with_pos $loc (RangeLit($2, $4))   }
| xml                                                          { $1 }
| linearity arg_lists block                                    { fun_lit ~ppos:$loc $1 $2 $3 }
| linearity arg_lists switch_funlit_body                       { switch_fun_lit ~ppos:$loc $1 $2 $3 }
| LEFTTRIANGLE cp_expression RIGHTTRIANGLE                     { with_pos $loc (CP $2) }
| DOLLAR primary_expression                                    { with_pos $loc (Generalise $2) }

constructor_expression:
| CONSTRUCTOR parenthesized_thing?                             { constructor ~ppos:$loc ?body:$2 $1 }

parenthesized_thing:
| LPAREN binop RPAREN                                          { with_pos $loc (Section $2)              }
| LPAREN DOT record_label RPAREN                               { with_pos $loc (Section (Section.Project $3))   }
| LPAREN RPAREN                                                { record ~ppos:$loc []                     }
| LPAREN labeled_exps preceded(VBAR, exp)? RPAREN              { record ~ppos:$loc $2 ?exp:$3             }
| LPAREN exps RPAREN                                           { with_pos $loc (TupleLit ($2))           }
| LPAREN exp WITH labeled_exps RPAREN                          { with_pos $loc (With ($2, $4))           }

binop:
| MINUS                                                        { Section.Minus          }
| MINUSDOT                                                     { Section.FloatMinus     }
| sigop                                                        { Section.Name (WithPos.node $1) }

sigop:
| DOLLAR                                                       { with_pos $loc "$" }
| op                                                           { $1 }

op:
| OPERATOR                                                     { with_pos $loc $1 }

spawn_expression:
| SPAWNAT LPAREN exp COMMA block RPAREN                        { spawn ~ppos:$loc Demon (ExplicitSpawnLocation $3) $5 }
| SPAWN block                                                  { spawn ~ppos:$loc Demon  NoSpawnLocation           $2 }
| SPAWNANGELAT LPAREN exp COMMA block RPAREN                   { spawn ~ppos:$loc Angel (ExplicitSpawnLocation $3) $5 }
| SPAWNANGEL  block                                            { spawn ~ppos:$loc Angel  NoSpawnLocation           $2 }
| SPAWNCLIENT block                                            { spawn ~ppos:$loc Demon  SpawnClient               $2 }
| SPAWNWAIT   block                                            { spawn ~ppos:$loc Wait   NoSpawnLocation           $2 }

query_policy:
| VARIABLE                                                     { query_policy_of_string $loc $1 }
| /* empty */                                                  { QueryPolicy.Flat }

postfix_expression:
| primary_expression | spawn_expression                        { $1 }
| block                                                        { $1 }
| QUERY query_policy block                                     { query ~ppos:$loc None $2 $3 }
| QUERY LBRACKET exp RBRACKET query_policy block               { query ~ppos:$loc (Some ($3, with_pos $loc (Constant (Constant.Int 0)))) $5 $6 }
| QUERY LBRACKET exp COMMA exp RBRACKET query_policy block     { query ~ppos:$loc (Some ($3, $5)) $7 $8 }
| postfix_expression arg_spec                                  { with_pos $loc (FnAppl ($1, $2)) }
| postfix_expression targ_spec                                 { with_pos $loc (TAppl ($1, $2)) }
| postfix_expression DOT record_label                          { with_pos $loc (Projection ($1, $3)) }
| postfix_expression AT                                        { with_pos $loc (Instantiate $1) }


arg_spec:
| LPAREN perhaps_exps RPAREN                                   { $2 }

targ_spec:
| LBRACKET type_arg_list RBRACKET                              { List.map (fun x -> (x, None)) $2 }

exps:
| separated_nonempty_list(COMMA, exp)                          { $1 }

perhaps_exps:
| loption(exps)                                                { $1 }

unary_expression:
| MINUS unary_expression                                       { unary_appl ~ppos:$loc UnaryOp.Minus      $2 }
| MINUSDOT unary_expression                                    { unary_appl ~ppos:$loc UnaryOp.FloatMinus $2 }
| OPERATOR unary_expression                                    { unary_appl ~ppos:$loc (UnaryOp.Name $1)  $2 }
| postfix_expression | constructor_expression                  { $1 }
| DOOP CONSTRUCTOR loption(arg_spec)                           { with_pos $loc (DoOperation ($2, $3, None)) }

infix_appl:
| unary_expression                                             { $1 }
| unary_expression OPERATOR                                    { unary_appl ~ppos:$loc (UnaryOp.Name $2) $1 } /* TODO(dhil): This is more general than the previous rule as in it will permit more expressions to be chained together. */
| unary_expression OPERATOR infix_appl                         { infix_appl' ~ppos:$loc $1 (BinaryOp.Name $2) $3 }
| unary_expression COLONCOLON infix_appl                       { infix_appl' ~ppos:$loc $1 BinaryOp.Cons $3 }
| unary_expression MINUS infix_appl                            { infix_appl' ~ppos:$loc $1 BinaryOp.Minus $3 }
| unary_expression MINUSDOT infix_appl                         { infix_appl' ~ppos:$loc $1 BinaryOp.FloatMinus $3 }
| unary_expression DOLLAR infix_appl                           { infix_appl' ~ppos:$loc $1 (BinaryOp.Name "$") $3 }
| unary_expression BANG infix_appl                             { infix_appl' ~ppos:$loc $1 (BinaryOp.Name "!") $3 }
| unary_expression EQUALSTILDE regex                           { let r, flags = $3 in
                                                                 infix_appl' ~ppos:$loc $1 (BinaryOp.RegexMatch flags) r }
logical_expression:
| infix_appl                                                   { $1 }
| logical_expression BARBAR infix_appl                         { infix_appl' ~ppos:$loc $1 BinaryOp.Or  $3 }
| logical_expression AMPAMP infix_appl                         { infix_appl' ~ppos:$loc $1 BinaryOp.And $3 }

typed_expression:
| logical_expression                                           { $1 }
| typed_expression COLON datatype                              { with_pos $loc (TypeAnnotation ($1, datatype $3)) }
| typed_expression COLON datatype LARROW datatype              { with_pos $loc (Upcast ($1, datatype $3, datatype $5)) }

db_expression:
| DELETE LPAREN table_generator RPAREN perhaps_where           { let pat, phrase = $3 in with_pos $loc (DBDelete (pat, phrase, $5)) }
| UPDATE LPAREN table_generator RPAREN
         perhaps_where
         SET LPAREN labeled_exps RPAREN                        { let pat, phrase = $3 in with_pos $loc (DBUpdate(pat, phrase, $5, $8)) }

/* XML */
xmlid:
| VARIABLE                                                     { $1 }

attr_list:
| attr*                                                        { $1 }

attr:
| xmlid EQ LQUOTE attr_val RQUOTE                              { ($1, $4) }
| xmlid EQ LQUOTE RQUOTE                                       { ($1, [constant_str ~ppos:$loc($3) ""]) }

attr_val:
| attr_val_entry+                                              { $1 }

attr_val_entry:
| block                                                        { $1 }
| STRING                                                       { constant_str ~ppos:$loc $1 }

xml:
| LXML attr_list block? SLASHRXML                              { xml ~ppos:$loc $1 $2 $3 []                }
| LXML attr_list block? RXML xml_contents* ENDTAG              { xml ~ppos:$loc $1 $2 $3 $5 ~tags:($1, $6) }

xml_contents:
| block                                                        { $1 }
| formlet_binding | formlet_placement | page_placement
| xml                                                          { $1 }
| CDATA                                                        { with_pos $loc (TextNode (Utility.xml_unescape $1)) }

formlet_binding:
| LBRACE logical_expression RARROW pattern RBRACE              { with_pos $loc (FormBinding($2, $4)) }

formlet_placement:
| LBRACE logical_expression
         FATRARROW logical_expression RBRACE                   { with_pos $loc (FormletPlacement ($2, $4,
                                                                                                   list ~ppos:$loc [])) }
| LBRACE logical_expression
         FATRARROW logical_expression
         WITH logical_expression RBRACE                        { with_pos $loc (FormletPlacement ($2, $4, $6)) }

page_placement:
| LBRACEBAR exp BARRBRACE                                      { with_pos $loc($2) (PagePlacement $2) }

session_expression:
| SELECT field_label exp                                       { with_pos $loc (Select ($2, $3))      }
| OFFER LPAREN exp RPAREN LBRACE case* RBRACE                  { with_pos $loc (Offer ($3, $6, None)) }

conditional_expression:
| IF LPAREN exp RPAREN exp ELSE exp                            { with_pos $loc (Conditional ($3, $5, $7)) }

case:
| CASE pattern RARROW case_contents                           { $2, block ~ppos:$loc($4) $4 }

case_expression:
| SWITCH LPAREN exp RPAREN LBRACE case* RBRACE                 { with_pos $loc (Switch ($3, $6, None)) }
| RECEIVE LBRACE case* RBRACE                                  { with_pos $loc (Receive ($3, None)) }
| SHALLOWHANDLE LPAREN exp RPAREN LBRACE case* RBRACE          { with_pos $loc (Handle (untyped_handler $3 $6 Shallow)) }
| HANDLE LPAREN exp RPAREN LBRACE case* RBRACE                 { with_pos $loc (Handle (untyped_handler $3 $6 Deep   )) }
| HANDLE LPAREN exp RPAREN LPAREN handle_params RPAREN LBRACE case* RBRACE
                                                               { with_pos $loc (Handle (untyped_handler ~parameters:$6 $3 $9 Deep)) }
| RAISE                                                        { with_pos $loc (Raise) }
| TRY exp AS pattern IN exp OTHERWISE exp                      { with_pos $loc (TryInOtherwise ($2, $4, $6, $8, None)) }

handle_params:
| separated_nonempty_list(COMMA,
    separated_pair(pattern, LARROW, exp))                      { $1 }

iteration_expression:
| FOR LPAREN perhaps_generators RPAREN
      perhaps_where
      perhaps_orderby
      exp                                                      { with_pos $loc (Iteration ($3, $7, $5, $6)) }

perhaps_generators:
| separated_list(COMMA, generator)                             { $1 }

generator:
| list_generator                                               { List  (fst $1, snd $1) }
| table_generator                                              { Table (fst $1, snd $1) }

list_generator:
| pattern LARROW exp                                           { ($1, $3) }

table_generator:
| pattern LLARROW exp                                          { ($1, $3) }

perhaps_where:
| /* empty */                                                  { None    }
| WHERE LPAREN exp RPAREN                                      { Some $3 }

perhaps_orderby:
| /* empty */                                                  { None }
| ORDERBY LPAREN exps RPAREN                                   { Some (orderby_tuple ~ppos:$loc($3) $3) }

escape_expression:
| ESCAPE VARIABLE IN exp                        { with_pos $loc (Escape (binder ~ppos:$loc($2) $2, $4)) }

formlet_expression:
| FORMLET xml YIELDS exp                                       { with_pos $loc (Formlet ($2, $4)) }
| PAGE xml                                                     { with_pos $loc (Page $2)          }

table_expression:
| TABLE exp WITH datatype perhaps_table_constraints FROM exp   { with_pos $loc (TableLit ($2, datatype $4, $5,
                                                                                          list ~ppos:$loc [], $7)) }
/* SAND */
| TABLE exp WITH datatype perhaps_table_constraints
            TABLEKEYS exp FROM exp                             { with_pos $loc (TableLit ($2, datatype $4, $5, $7, $9))}

perhaps_table_constraints:
| loption(preceded(WHERE, table_constraints))                  { $1 }

table_constraints:
| separated_nonempty_list(COMMA,
    pair(record_label, field_constraints))                     { $1 }

field_constraints:
| field_constraint+                                            { $1 }

field_constraint:
| READONLY                                                     { Readonly }
| DEFAULT                                                      { Default  }

perhaps_db_args:
| atomic_expression?                                           { $1 }

perhaps_db_driver:
| atomic_expression perhaps_db_args                            { Some $1, $2   }
| /* empty */                                                  { None   , None }

exp:
| case_expression
| conditional_expression
| database_expression
| db_expression
| escape_expression
| formlet_expression
| iteration_expression
| lens_expression
| session_expression
| table_expression
| typed_expression                                             { $1 }

database_expression:
| INSERT exp VALUES LPAREN record_labels RPAREN exp            { db_insert ~ppos:$loc $2 $5 $7 None }
| INSERT exp VALUES LBRACKET LPAREN loption(labeled_exps)
  RPAREN RBRACKET preceded(RETURNING, VARIABLE)?               { db_insert ~ppos:$loc $2 (labels $6) (db_exps ~ppos:$loc($6) $6) $9  }
| INSERT exp VALUES LPAREN record_labels RPAREN typed_expression
  RETURNING VARIABLE                                           { db_insert ~ppos:$loc $2 $5 $7 (Some $9) }
| DATABASE atomic_expression perhaps_db_driver                 { with_pos $loc (DatabaseLit ($2, $3))           }

fn_dep_cols:
| field_label+                                                 { $1 }

fn_dep:
| fn_dep_cols RARROW fn_dep_cols                               { ($1, $3) }

fn_deps:
| separated_nonempty_list(COMMA, fn_dep)                       { $1 }

lens_expression:
| LENS exp DEFAULT                                             { with_pos $loc (LensLit ($2, None))}
| LENS exp TABLEKEYS exp                                       { with_pos $loc (LensKeysLit ($2, $4, None))}
| LENS exp WITH LBRACE fn_deps RBRACE                          { with_pos $loc (LensFunDepsLit ($2, $5, None))}
| LENSSERIAL exp WITH VARIABLE+                                { with_pos $loc (LensSerialLit ($2, $4, None))}
| LENSDROP field_label DETERMINED BY
  field_label DEFAULT exp FROM exp                             { with_pos $loc (LensDropLit ($9, $2, $5, $7, None)) }
| LENSSELECT FROM exp BY exp                                   { with_pos $loc (LensSelectLit ($3, $5, None)) }
| LENSJOIN exp WITH exp ON exp DELETE LBRACE exp COMMA exp RBRACE  { with_pos $loc (LensJoinLit ($2, $4, $6, $9, $11, None)) }
| LENSJOIN exp WITH exp ON exp DELETE_LEFT                     { with_pos $loc (LensJoinLit ($2, $4, $6,
                                                                                       with_pos $loc (Constant (Constant.Bool true )),
                                                                                       with_pos $loc (Constant (Constant.Bool false)), None)) }
| LENSGET exp                                                  { with_pos $loc (LensGetLit ($2, None)) }
| LENSCHECK exp                                                { with_pos $loc (LensCheckLit ($2, None)) }
| LENSPUT exp WITH exp                                         { with_pos $loc (LensPutLit ($2, $4, None)) }


record_labels:
| separated_list(COMMA, record_label)                          { $1 }

links_open:
| OPEN separated_nonempty_list(DOT, CONSTRUCTOR)               { with_pos $loc (Open $2) }

binding:
| VAR pattern EQ exp SEMICOLON                                 { val_binding ~ppos:$loc $2 $4 }
| exp SEMICOLON                                                { with_pos $loc (Exp $1) }
| signatures fun_kind VARIABLE arg_lists block                 { fun_binding ~ppos:$loc (fst $1) ~unsafe_sig:(snd $1) ($2, $3, $4, loc_unknown, $5) }
| fun_kind VARIABLE arg_lists block                            { fun_binding ~ppos:$loc None ($1, $2, $3, loc_unknown, $4) }
| signatures fun_kind VARIABLE arg_lists switch_funlit_body    { switch_fun_binding ~ppos:$loc (fst $1) ~unsafe_sig:(snd $1) ($2, $3, $4, loc_unknown, $5) }
| fun_kind VARIABLE arg_lists switch_funlit_body               { switch_fun_binding ~ppos:$loc None ($1, $2, $3, loc_unknown, $4) }
| typedecl SEMICOLON | links_module
| links_open SEMICOLON                                         { $1 }

mutual_binding_block:
| MUTUAL LBRACE mutual_bindings RBRACE                         { MutualBindings.flatten $3 }

mutual_bindings:
| binding                                                      { MutualBindings.(add (empty (pos $loc)) $1) }
| mutual_bindings binding                                      { MutualBindings.add $1 $2 }

binding_or_mutual:
| binding                                                      { [$1]      }
| mutual_binding_block                                         { $1        }

bindings:
| binding_or_mutual                                            { $1 } /* See #441 and #900 */
| bindings binding_or_mutual                                   { $1 @ $2 }

moduleblock:
| LBRACE declarations RBRACE                                   { $2 }

block:
| LBRACE block_contents RBRACE                                 { block ~ppos:$loc $2 }

case_contents:
| bindings exp                                                 { ($1, $2) }
| exp                                                          { ([], $1) }

block_contents:
| case_contents                                                { $1 }
| /* empty */                                                  { ([], with_pos $loc (TupleLit [])) }

labeled_exp:
| preceded(EQ, VARIABLE)                                       { ($1, with_pos $loc (Var $1)) }
| separated_pair(record_label, EQ, exp)                        { $1 }

labeled_exps:
| separated_nonempty_list(COMMA, labeled_exp)                  { $1 }

/*
 * Datatype grammar
 */
just_datatype:
| datatype EOF                                                 { $1 }

datatype:
| mu_datatype | straight_arrow | squiggly_arrow                { with_pos $loc $1 }

arrow_prefix:
| LBRACE RBRACE                                                { ([], Datatype.Closed) }
| LBRACE efields RBRACE                                        { $2            }

straight_arrow_prefix:
| hear_arrow_prefix | arrow_prefix                             { $1       }
| MINUS nonrec_row_var | MINUS kinded_nonrec_row_var           { ([], $2) }

squig_arrow_prefix:
| hear_arrow_prefix | arrow_prefix                             { $1       }
| TILDE nonrec_row_var | TILDE kinded_nonrec_row_var           { ([], $2) }

hear_arrow_prefix:
| LBRACE COLON datatype COMMA efields RBRACE                   { hear_arrow_prefix $3 $5                    }
| LBRACE COLON datatype RBRACE                                 { hear_arrow_prefix $3 ([], Datatype.Closed) }
| LBRACE COLON datatype VBAR nonrec_row_var RBRACE
| LBRACE COLON datatype VBAR kinded_nonrec_row_var RBRACE      { hear_arrow_prefix $3 ([], $5)              }

straight_arrow:
| parenthesized_datatypes
  straight_arrow_prefix RARROW datatype                        { Datatype.Function ($1, $2, $4) }
| parenthesized_datatypes
  straight_arrow_prefix LOLLI datatype                         { Datatype.Lolli    ($1, $2, $4) }
| parenthesized_datatypes RARROW datatype                      { Datatype.Function ($1, fresh_effects, $3) }
| parenthesized_datatypes LOLLI datatype                       { Datatype.Lolli    ($1, fresh_effects, $3) }

squiggly_arrow:
| parenthesized_datatypes
  squig_arrow_prefix SQUIGRARROW datatype                      { Datatype.Function ($1, row_with_wp $2, $4) }
| parenthesized_datatypes
  squig_arrow_prefix SQUIGLOLLI datatype                       { Datatype.Lolli    ($1, row_with_wp $2, $4) }
| parenthesized_datatypes SQUIGRARROW datatype                 { Datatype.Function ($1, row_with_wp fresh_effects, $3) }
| parenthesized_datatypes SQUIGLOLLI datatype                  { Datatype.Lolli    ($1, row_with_wp fresh_effects, $3) }

mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { Datatype.Mu (named_typevar $2 `Rigid, with_pos $loc($4) $4) }
| forall_datatype                                              { $1 }

forall_datatype:
| FORALL varlist DOT datatype                                  { Datatype.Forall ($2, $4) }
| session_datatype                                             { $1 }

/* Parenthesised dts disambiguate between sending qualified types and recursion variables.
   e.g:

     S = !ModuleA.ModuleB.Type.S
     should be written
     S = !(ModuleA.ModuleB.Type).S

     Parenthesised versions take priority over non-parenthesised versions.

   24.1.19: Modified grammar to allow unparenthesized qualified type names
   in most places, but specifically forbid in ? . or ! .
   This also requires moving the TILDE production into primary_datatype
   since otherwise, ? ~M.x . y is ambiguous.
   This is not ideal since it spreads the session-related constructs
   among several nonterminals.
*/
session_datatype:
| BANG primary_datatype_pos DOT datatype                       { Datatype.Output ($2, $4) }
| QUESTION primary_datatype_pos DOT datatype                   { Datatype.Input  ($2, $4) }
| LBRACKETPLUSBAR row BARPLUSRBRACKET                          { Datatype.Select $2       }
| LBRACKETAMPBAR row BARAMPRBRACKET                            { Datatype.Choice $2       }
| primary_datatype                                             { $1                       }
| qualified_type_name                                          { Datatype.QualifiedTypeApplication ($1, []) }
| qualified_type_name LPAREN type_arg_list RPAREN              { Datatype.QualifiedTypeApplication ($1, $3) }

parenthesized_datatypes:
| LPAREN RPAREN                                                { [] }
| LPAREN datatypes RPAREN                                      { $2 }

primary_datatype_pos:
| primary_datatype                                             { with_pos $loc $1 }

primary_datatype:
| TILDE primary_datatype_pos                                   { Datatype.Dual $2 }
| parenthesized_datatypes                                      { match $1 with
                                                                   | [] -> Datatype.Unit
                                                                   | [n] -> WithPos.node n
                                                                   | ts  -> Datatype.Tuple ts }
| LPAREN rfields RPAREN                                        { Datatype.Record $2 }
| TABLEHANDLE
     LPAREN datatype COMMA datatype COMMA datatype RPAREN      { Datatype.Table ($3, $5, $7) }
| LBRACKETBAR vrow BARRBRACKET                                 { Datatype.Variant $2 }
| LBRACKET datatype RBRACKET                                   { Datatype.List $2 }
| type_var                                                     { $1 }
| kinded_type_var                                              { $1 }
| CONSTRUCTOR                                                  { let open Datatype in
                                                                 match $1 with
                                                                   | "Bool"    -> Primitive Primitive.Bool
                                                                   | "Int"     -> Primitive Primitive.Int
                                                                   | "Char"    -> Primitive Primitive.Char
                                                                   | "Float"   -> Primitive Primitive.Float
                                                                   | "XmlItem" -> Primitive Primitive.XmlItem
                                                                   | "String"  -> Primitive Primitive.String
                                                                   | "Database"-> DB
                                                                   | "End"     -> Datatype.End
                                                                   | t         -> TypeApplication (t, [])
                                                               }
| CONSTRUCTOR LPAREN type_arg_list RPAREN                      { Datatype.TypeApplication ($1, $3) }

type_var:
| VARIABLE                                                     { Datatype.TypeVar (named_typevar $1  `Rigid)   }
| PERCENTVAR                                                   { Datatype.TypeVar (named_typevar $1 `Flexible) }
| UNDERSCORE                                                   { Datatype.TypeVar (fresh_typevar `Rigid)    }
| PERCENT                                                      { Datatype.TypeVar (fresh_typevar `Flexible) }

kinded_type_var:
| type_var subkind                                             { attach_subkind ($1, $2) }

type_arg_list:
| separated_nonempty_list(COMMA, type_arg)                     { $1 }

type_arg:
| datatype                                                     { Datatype.Type $1     }
| braced_fieldspec                                             { Datatype.Presence $1 }
| LBRACE row RBRACE                                            { Datatype.Row $2      }

datatypes:
| separated_nonempty_list(COMMA, datatype)                     { $1 }

vrow:
| vfields                                                      { $1                    }
| /* empty */                                                  { ([], Datatype.Closed) }

row:
| fields                                                       { $1                    }
| /* empty */                                                  { ([], Datatype.Closed) }

fields_def(field_prod, row_var_prod, kinded_row_var_prod):
| field_prod                                                   { ([$1], Datatype.Closed) }
| soption(field_prod) VBAR row_var_prod                        { ( $1 , $3             ) }
| soption(field_prod) VBAR kinded_row_var_prod                 { ( $1 , $3             ) }
| field_prod COMMA
    fields_def(field_prod, row_var_prod, kinded_row_var_prod)  { ( $1::fst $3, snd $3 ) }

fields:
| fields_def(field, row_var, kinded_row_var)                   { $1 }

field:
| CONSTRUCTOR /* allows nullary variant labels */              { ($1, present) }
| field_label fieldspec                                        { ($1, $2) }

field_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }
| STRING                                                       { $1 }
| UINTEGER                                                     { string_of_int $1 }

rfields:
| fields_def(rfield, row_var, kinded_row_var)                  { $1 }

rfield:
/* The following sugar is tempting, but it leads to a conflict. Is
   the type (a,b,c) a record with fields a, b, c or a polymorphic tuple
   with type variables a, b, c?
*/
| record_label fieldspec                                       { ($1, $2) }

record_label:
| field_label                                                  { $1 }

vfields:
| vfield                                                       { ([$1], Datatype.Closed) }
| vrow_var                                                     { ([]  , $1             ) }
| kinded_vrow_var                                              { ([]  , $1             ) }
| vfield VBAR vfields                                          { ($1::fst $3, snd $3   ) }

vfield:
| CONSTRUCTOR                                                  { ($1, present) }
| CONSTRUCTOR fieldspec                                        { ($1, $2)      }

efields:
| fields_def(efield, row_var, kinded_row_var)                  { $1 }

efield:
| effect_label fieldspec                                       { ($1, $2)      }

effect_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }

fieldspec:
| braced_fieldspec                                             { $1 }
| COLON datatype                                               { Datatype.Present $2 }
| MINUS                                                        { Datatype.Absent }

braced_fieldspec:
| LBRACE COLON datatype RBRACE                                 { Datatype.Present $3 }
| LBRACE MINUS RBRACE                                          { Datatype.Absent }
| LBRACE VARIABLE RBRACE                                       { Datatype.Var (named_typevar $2 `Rigid) }
| LBRACE PERCENTVAR RBRACE                                     { Datatype.Var (named_typevar $2 `Flexible) }
| LBRACE UNDERSCORE RBRACE                                     { Datatype.Var (fresh_typevar `Rigid)    }
| LBRACE PERCENT RBRACE                                        { Datatype.Var (fresh_typevar `Flexible) }

nonrec_row_var:
| VARIABLE                                                     { Datatype.Open (named_typevar $1 `Rigid   ) }
| PERCENTVAR                                                   { Datatype.Open (named_typevar $1 `Flexible) }
| UNDERSCORE                                                   { Datatype.Open (fresh_typevar `Rigid)    }
| PERCENT                                                      { Datatype.Open (fresh_typevar `Flexible) }

row_var:
| nonrec_row_var                                               { $1 }
| LPAREN MU VARIABLE DOT fields RPAREN                         { Datatype.Recursive (named_typevar $3 `Rigid, $5) }

kinded_nonrec_row_var:
| nonrec_row_var subkind                                       { attach_row_subkind ($1, $2) }

kinded_row_var:
| row_var subkind                                              { attach_row_subkind ($1, $2) }


vrow_var:
/* This uses the usual nonrec_row_var, because a variant version would be exactly the same. */
| nonrec_row_var                                               { $1 }
| LPAREN MU VARIABLE DOT vfields RPAREN                        { Datatype.Recursive (named_typevar $3 `Rigid, $5) }

kinded_vrow_var:
| vrow_var subkind                                             { attach_row_subkind ($1, $2) }


/*
 * Regular expression grammar
 */
regex:
| SLASH regex_pattern_alternate regex_flags_opt                { with_pos $loc($2) (Regex $2), $3 }
| SLASH regex_flags_opt                                        { with_pos $loc (Regex (Simply "")), $2 }
| SSLASH regex_pattern_alternate SLASH regex_replace
    regex_flags_opt                                            { with_pos $loc (Regex (Replace ($2, $4))),
                                                                 RegexReplace :: $5 }

regex_flags_opt:
| SLASH                                                        { [] }
| SLASHFLAGS                                                   { parseRegexFlags $1 }

regex_replace:
| /* empty */                                                  { Literal "" }
| REGEXREPL                                                    { Literal $1 }
| block                                                        { SpliceExpr $1 }

regex_pattern:
| RANGE                                                        { Range (fst $1, snd $1) }
| STRING                                                       { Simply $1 }
| QUOTEDMETA                                                   { Quote (Simply $1) }
| DOT                                                          { Any }
| CARET                                                        { StartAnchor }
| DOLLAR                                                       { EndAnchor }
| LPAREN regex_pattern_alternate RPAREN                        { Group $2 }
| regex_pattern STAR                                           { Repeat (Regex.Star, $1) }
| regex_pattern PLUS                                           { Repeat (Regex.Plus, $1) }
| regex_pattern QUESTION                                       { Repeat (Regex.Question, $1) }
| block                                                        { Splice $1 }

regex_pattern_alternate:
| regex_pattern_sequence                                       { Seq $1 }
| regex_pattern_sequence ALTERNATE regex_pattern_alternate     { Alternate (Seq $1, $3) }

regex_pattern_sequence:
| regex_pattern+                                               { $1 }

/*
 * Pattern grammar
 */
pattern:
| typed_pattern                                                { $1 }
| typed_pattern COLON primary_datatype_pos                     { with_pos $loc (Pattern.HasType ($1, datatype $3)) }

typed_pattern:
| cons_pattern                                                 { $1 }
| cons_pattern AS VARIABLE                                     { with_pos $loc (Pattern.As (binder ~ppos:$loc($3) $3, $1)) }

cons_pattern:
| constructor_pattern                                          { $1 }
| constructor_pattern COLONCOLON cons_pattern                  { with_pos $loc (Pattern.Cons ($1, $3)) }

constructor_pattern:
| negative_pattern                                             { $1 }
| CONSTRUCTOR parenthesized_pattern?                           { with_pos $loc (Pattern.Variant ($1, $2)) }

constructors:
| separated_nonempty_list(COMMA, CONSTRUCTOR)                  { $1 }

negative_pattern:
| primary_pattern                                              { $1 }
| MINUS CONSTRUCTOR                                            { with_pos $loc (Pattern.Negative [$2]) }
| MINUS LPAREN constructors RPAREN                             { with_pos $loc (Pattern.Negative $3)   }

parenthesized_pattern:
| LPAREN RPAREN                                                { with_pos $loc (Pattern.Tuple []) }
| LPAREN pattern RPAREN                                        { $2 }
| LPAREN pattern COMMA patterns RPAREN                         { with_pos $loc (Pattern.Tuple ($2 :: $4)) }
| LPAREN labeled_patterns preceded(VBAR, pattern)? RPAREN      { with_pos $loc (Pattern.Record ($2, $3))  }

primary_pattern:
| VARIABLE                                                     { variable_pat ~ppos:$loc $1   }
| UNDERSCORE                                                   { any_pat $loc                 }
| constant                                                     { with_pos $loc (Pattern.Constant $1) }
| LBRACKET RBRACKET                                            { with_pos $loc Pattern.Nil           }
| LBRACKET patterns RBRACKET                                   { with_pos $loc (Pattern.List $2)     }
| parenthesized_pattern                                        { $1 }

patterns:
| separated_nonempty_list(COMMA, pattern)                      { $1 }

labeled_pattern:
| preceded(EQ, VARIABLE)                                       { ($1, variable_pat ~ppos:$loc $1) }
| separated_pair(record_label, EQ,  pattern)                   { $1 }

labeled_patterns:
| separated_nonempty_list(COMMA, labeled_pattern)              { $1 }

multi_args:
| LPAREN separated_list(COMMA, pattern) RPAREN                 { $2 }

arg_lists:
| multi_args+                                                  { $1 }
