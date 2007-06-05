(*pp camlp4of *)
(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils
open Camlp4.PreCast

module Gram = MakeGram(Lexer)

open Syntax

let derive_str loc (tdecls : Types.decl list) classname : Ast.str_item =
  let context = Base.setup_context loc tdecls in
    Base.find classname (loc, context, tdecls)

let derive_sig loc types classname : Ast.sig_item =
  failwith "nyi"

DELETE_RULE Gram str_item: "type"; type_declaration END
DELETE_RULE Gram sig_item: "type"; type_declaration END

EXTEND Gram
str_item:
[[ "type"; types = type_declaration -> <:str_item< type $types$ >>
  | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
      let decls : Types.decl list = Types.Translate.decls types in 
      let module U = Types.Untranslate(struct let loc = loc end) in
      let tdecls : Ast.ctyp list = List.map U.decl decls in
        <:str_item< type $list:tdecls$ $list:List.map (derive_str loc decls) cl$ >>
 ]]
;
sig_item:
[[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
 | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
    <:sig_item< type $types$ $list:List.map (derive_sig loc types) cl$ >> ]]
;
END
