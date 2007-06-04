(*pp camlp4of *)
(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils
open Camlp4.PreCast

module Gram = MakeGram(Lexer)

open Syntax

let derive loc types classname : Ast.str_item =
  let context = Base.setup_context loc types in
    Base.find classname (loc, context, List.map Types.Translate.decl types) 

let derive_sig loc types classname : Ast.sig_item =
  failwith "nyi"

(*DELETE_RULE Gram str_item: "type"; LIST1 type_declaration SEP "and" END
DELETE_RULE Gram sig_item: "type"; LIST1 type_declaration SEP "and" END
*)
EXTEND Gram
str_item:
[[ "type"; types = (*LIST1*) type_declaration(* SEP "and"*) -> 
     <:str_item< type $types$ >>
  | "type"; types = (*LIST1*) type_declaration (*SEP "and"*); 
    "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
    let types = Types.Translate.split types in
    <:str_item< type $list:types$
                $list:List.map (derive loc types) cl$ >> ]]
;
sig_item:
[[ "type"; types = LIST1 type_declaration SEP "and" ->
    <:sig_item< type $list:types$ >>
 | "type"; types = LIST1 type_declaration SEP "and"; 
   "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
    <:sig_item< type $list:types$
                $list:List.map (derive_sig loc types) cl$ >> ]]
;
END
