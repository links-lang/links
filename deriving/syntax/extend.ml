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
        Base.Underivable (loc, msg) ->
          fatal_error loc msg
      | Failure msg ->
          fatal_error loc msg


  let derive_str loc (tdecls : Type.decl) classname : Ast.str_item =
    (Base.find classname ~loc)#decls tdecls
  
  let derive_sig loc tdecls classname : Ast.sig_item =
    (Base.find classname ~loc)#signature tdecls


  DELETE_RULE Gram str_item: "type"; type_declaration END
  DELETE_RULE Gram sig_item: "type"; type_declaration END

  open Ast

  EXTEND Gram
  str_item:
  [[ "type"; types = type_declaration -> <:str_item< type $types$ >>
    | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
        let decls = display_errors loc Type.Translate.decl types in 
(*        let () = prerr_endline (Showdecl.Show_decl.show decls) in*)
        let tdecls = Type.Untranslate.decl ~loc decls in
          <:str_item< type $list:tdecls$ $list:List.map (derive_str loc decls) cl$ >>
   ]]
  ;
  sig_item:
  [[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
   | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
       let decls  = display_errors loc Type.Translate.sigdecl types in 
       let tdecls = Type.Untranslate.sigdecl ~loc decls in
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
         if not (Base.is_registered classname) then
           fatal_error loc ("deriving: "^ classname ^" is not a known `class'")
         else
           let binding = Ast.TyDcl (loc, "inline", [], t, []) in
           let decls = display_errors loc Type.Translate.decl binding in
             if Base.contains_tvars_decl decls then
               fatal_error loc ("deriving: type variables cannot be used in `method' instantiations")
             else
               let tdecls = Type.Untranslate.decl ~loc decls in
               let m = derive_str loc decls classname in
                 <:expr< let module $uid:classname$ = 
                             struct
                               type $list:tdecls$
                               $m$ 
                               include $uid:classname ^ "_inline"$
                             end
                          in $uid:classname$.$lid:methodname$ >>
       | _ -> 
           fatal_error loc ("deriving: this looks a bit like a method application, but "
                            ^"the syntax is not valid");
  ]];
  END
  
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
