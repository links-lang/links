(*pp camlp4of *)
(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils

module Deriving (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  open Camlp4.PreCast

  include Syntax

  let fatal_error loc msg = 
    Syntax.print_warning loc msg;
    exit 1

  let display_errors loc f p =
    try
      f p
    with 
        Base.Underivable msg | Failure msg ->
          fatal_error loc msg

  let derive proj (loc : Loc.t) tdecls classname =
    let context = display_errors loc (Base.setup_context loc) tdecls in
      display_errors loc
        (proj (Base.find classname)) (loc, context, tdecls)
  
  let derive_str loc (tdecls : Type.decl list) classname : Ast.str_item =
    derive fst loc tdecls classname
  
  let derive_sig loc tdecls classname : Ast.sig_item =
    derive snd loc tdecls classname


  DELETE_RULE Gram str_item: "type"; type_declaration END
  DELETE_RULE Gram sig_item: "type"; type_declaration END

  open Ast

  EXTEND Gram
  str_item:
  [[ "type"; types = type_declaration -> <:str_item< type $types$ >>
    | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
        let decls = display_errors loc Type.Translate.decls types in 
        let module U = Type.Untranslate(struct let loc = loc end) in
        let tdecls = List.map U.decl decls in
          <:str_item< type $list:tdecls$ $list:List.map (derive_str loc decls) cl$ >>
   ]]
  ;
  sig_item:
  [[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
   | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
       let decls  = display_errors loc Type.Translate.decls types in 
       let module U = Type.Untranslate(struct let loc = loc end) in
       let tdecls = List.concat_map U.sigdecl decls in
       let ms = List.map (derive_sig loc decls) cl in
         <:sig_item< type $list:tdecls$ $list:ms$ >> ]]
  ;
  END

  EXTEND Gram
  expr: LEVEL "simple"
  [
  [e1 = val_longident ; "<" ; t = ctyp; ">" ->
     match e1 with
       | <:ident< $uid:classname$ . $lid:methodname$ >> ->
           if Base.is_registered classname then
             let module U = Type.Untranslate(struct let loc = loc end) in
             let binding = Ast.TyDcl (loc, "inline", [], t, []) in
             let decls = display_errors loc Type.Translate.decls binding in
             let tdecls = List.map U.decl decls in
             let m = derive_str loc decls classname in
               <:expr< let module $uid:classname$ = 
                           struct
                             type $list:tdecls$
                             $m$ 
                             include $uid:classname ^ "_inline"$
                           end
                        in $uid:classname$.$lid:methodname$ >>
           else
             fatal_error loc ("deriving: "^ classname ^" is not a known `class'");
             
       | _ -> 
           fatal_error loc ("deriving: this looks a bit like a method application, but "
                            ^"the syntax is not valid");
  ]];
  END
  
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
