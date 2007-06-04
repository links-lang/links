(*pp camlp4of *)
(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils
open Camlp4.PreCast

module Gram = MakeGram(Lexer)

open Syntax

let derive_str loc types classname : Ast.str_item =
  let context = Base.setup_context loc types in
    Base.find classname (loc, context, List.map Types.Translate.decl types) 

let derive_sig loc types classname : Ast.sig_item =
  failwith "nyi"

DELETE_RULE Gram str_item: "type"; type_declaration END
DELETE_RULE Gram sig_item: "type"; type_declaration END

EXTEND Gram
str_item:
[[ "type"; types = type_declaration -> <:str_item< type $types$ >>
  | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
    <:str_item< type $types$ $list:List.map (derive_str loc (Types.Translate.split types)) cl$ >> ]]
;
sig_item:
[[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
 | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
    <:sig_item< type $types$ $list:List.map (derive_sig loc (Types.Translate.split types)) cl$ >> ]]
;
END
