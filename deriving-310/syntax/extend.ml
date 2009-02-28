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


  let derive_str loc params (tdecls :  (Type.is_generated * Type.rhs) Type.NameMap.t list) classname : Ast.str_item =
    (Base.find classname ~loc)#decls params tdecls
  
  let derive_sig loc params tdecls classname : Ast.sig_item =
    let sigdecl = 
      (params,
       (List.fold_right (Type.NameMap.fold Type.NameMap.add) tdecls Type.NameMap.empty)) in
      (Base.find classname ~loc)#signature sigdecl

  let group_rhss (params, rhs) = (params, Analyse.group_rhss rhs)

  let find_superclasses loc classname : Type.name list =
    (Base.find classname ~loc)#superclasses

  DELETE_RULE Gram str_item: "type"; type_declaration END
  DELETE_RULE Gram sig_item: "type"; type_declaration END

  let just_generated : (Type.is_generated * _) Type.NameMap.t as 'a -> 'a
    = fun map ->
      Type.NameMap.fold
        (fun k ((generated, _) as v) map ->
           if generated then Type.NameMap.add k v map
           else map)
        map
        Type.NameMap.empty

  open Ast

  (*
    Suppose that we're generating an instance of a class C for a type t.
    
    If we're not generating instances of the superclasses S1 ... Sn of
    C for t then we should generate instances of S1 ... Sn for the
    types generated from t.
  *)
  EXTEND Gram
  str_item:
  [[ "type"; types = type_declaration -> <:str_item< type $types$ >>
    | "type"; types = type_declaration; "deriving"; "("; classes = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
        let params, (rhss : (Type.is_generated * _) Type.NameMap.t list) = group_rhss (display_errors loc Type.Translate.decl types) in 
        let generated_rhss = List.map just_generated rhss in
        let generated_types = List.map (fun rhs -> <:str_item< type $list:Type.Untranslate.decl ~loc (params, rhs)$ >>) generated_rhss in
        let generated_rhss = 
          List.concat_map
            (fun rhs ->
               let m = (Type.NameMap.fold 
                          (fun name (is_generated, rhs) map -> 
                             if (is_generated && List.exists (Type.NameMap.mem name) rhss) 
                             then Type.NameMap.add name (is_generated, rhs) map
                             else map)
                          rhs
                          Type.NameMap.empty)
               in if Type.NameMap.is_empty m then [] else [m])
            rhss 
        in

        let instances = List.map (fun cl -> 
                                    let instance = derive_str loc params rhss cl in
                                    let superclasses = find_superclasses loc cl in
                                    let underived_superclasses = List.filter (fun cl -> not (List.mem cl classes)) superclasses in
                                    let superclass_instances = List.map (fun c -> derive_str loc params generated_rhss c) underived_superclasses in
                                      <:str_item< $list:superclass_instances$ $instance$ >>) classes in
          <:str_item< type $types$ $list:generated_types$ $list:instances$ >>
   ]]
  ;
  sig_item:
  [[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
   | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
       let params, rhss  = display_errors loc Type.Translate.sigdecl types in 
       let decl_cliques = Analyse.group_sigrhss rhss in
       let generated_rhss = List.map just_generated decl_cliques in
       let tdecls = List.map (fun rhs -> <:sig_item< type $list:Type.Untranslate.sigdecl ~loc (params, rhs)$ >>) generated_rhss in
       let ms = List.map (derive_sig loc params decl_cliques) cl in
         <:sig_item< type $types$ $list:tdecls$ $list:ms$ >> ]]
  ;
  END

  EXTEND Gram
  expr: LEVEL "simple"
  [
  [e1 = val_longident ; "."; "<" ; t = ctyp; ">." ->
     match e1 with
       | <:ident< $uid:classname$ . $lid:methodname$ >> ->
         if not (Base.is_registered classname) then
           fatal_error loc ("deriving: "^ classname ^" is not a known `class'")
         else
           let binding = Ast.TyDcl (loc, "inline", [], t, []) in
           let params, rhss as decls = display_errors loc Type.Translate.decl binding in
             if Base.contains_tvars_decl decls then
               fatal_error loc ("deriving: type variables cannot be used in `method' instantiations")
             else
               let utdecls = Type.Untranslate.decl ~loc decls in
               let tdecls = Analyse.group_rhss rhss in
               let superclasses = find_superclasses loc classname in
               let superclass_instances = List.map (derive_str loc params tdecls) superclasses in
               let instance = derive_str loc params tdecls classname in
                 <:expr< let module $uid:classname^"_"$ = 
                             struct
                               type $list:utdecls$
                               $list:superclass_instances$
                               $instance$ 
                             end
                          in ($uid:classname$.$lid:methodname$ $uid:classname^"_"$.$lid:String.lowercase classname ^ "_inline"$) >>
       | _ -> 
           fatal_error loc ("deriving: this looks a bit like a method application, but "
                            ^"the syntax is not valid");
  ]];
  END
  
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
