(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Type
  open Base
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Typeable"

  let mkName : name -> string = 
    let file_name, sl, _, _, _, _, _, _ = Loc.to_tuple loc in
      Printf.sprintf "%s_%d_%f_%s" 
        file_name sl (Unix.gettimeofday ())

  let gen ?eq ctxt ((tname,_,_,_) as decl : Type.decl) _ = 
    let paramList = 
      List.fold_right 
        (fun (p,_) cdr ->
             <:expr< $uid:NameMap.find p ctxt.argmap$.typeRep::$cdr$ >>)
        ctxt.params
      <:expr< [] >>
    in <:module_expr< struct type a = $atype ctxt decl$
          let typeRep = TypeRep.mkFresh $str:mkName tname$ $paramList$ end >>

  let tup ctxt ts mexpr expr = 
      let params = 
        expr_list 
          (List.map (fun t -> <:expr< let module M = $expr ctxt t$ 
                                       in $mexpr$ >>) ts) in
        <:module_expr< Typeable_defaults(struct type a = $atype_expr ctxt (`Tuple ts)$
                                                let typeRep = Typeable.TypeRep.mkTuple $params$ end) >>

  let instance = object(self)
    inherit make_module_expr ~classname ~allow_private:true 

    method tuple ctxt ts = tup ctxt ts <:expr< M.typeRep >> (self#expr)
    method sum = gen 
    method record = gen
    method variant ctxt decl (_,tags) =
    let tags, extends = 
      List.fold_left 
        (fun (tags, extends) -> function
           | Tag (l, None)  -> <:expr< ($str:l$, None) :: $tags$ >>, extends
           | Tag (l,Some t) ->
               <:expr< ($str:l$, Some $mproject (self#expr ctxt t) "typeRep"$) ::$tags$ >>,
               extends
           | Extends t -> 
               tags,
               <:expr< $mproject (self#expr ctxt t) "typeRep"$::$extends$ >>)
        (<:expr< [] >>, <:expr< [] >>) tags in
      <:module_expr< Typeable_defaults(
        struct type a = $atype ctxt decl$
               let typeRep = Typeable.TypeRep.mkPolyv $tags$ $extends$
        end) >>
  end
end

let _ = Base.register "Typeable" 
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
         ~default_module:"Typeable_defaults" ()),
  (fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.gen_sigs ~context ~decls ~classname:M.classname))
