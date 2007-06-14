(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Pickle"

  let wrap ~atype ~picklers ~unpickle =
    <:module_expr< struct type a = $atype$
                          let pickle buffer = function $list:picklers$
                          let unpickle stream = $unpickle$ end >>

  let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:false

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr * Ast.expr =
      List.fold_right
        (fun (id,t) (p,u) -> 
           <:expr< $mproject (self#expr ctxt t) "pickle"$ buffer $lid:id$; $p$ >>,
           <:expr< let $lid:id$ = $mproject (self#expr ctxt t) "unpickle"$ stream in $u$ >>)
        exprs (<:expr<>>, <:expr< $tuple_expr (List.map (fun (id,_) -> <:expr< $lid:id$ >>) exprs)$>>)

    method tuple ctxt ts = 
      let atype = atype_expr ctxt (`Tuple ts)
      and picklers, unpickle = 
        let n = List.length ts in 
        let pinner, unpickle = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) ts) in
        let patt, expr = tuple n in
          [ <:match_case< $patt$ -> $pinner$ >> ], unpickle in
        <:module_expr< Pickle_defaults( $wrap ~atype ~picklers ~unpickle$) >>

    method polycase ctxt tagspec n : Ast.match_case * Ast.match_case = 
      let picklen = <:expr< Pickle_int.pickle buffer $`int:n$ >> in
        match tagspec with
          | Tag (name, args) -> (match args with 
              | None   -> <:match_case< `$name$ -> $picklen$ >>,
                          <:match_case< $`int:n$ -> `$name$ >>
              | Some e -> <:match_case< `$name$ x -> $picklen$;
                                         $mproject (self#expr ctxt e) "pickle"$ buffer x >>,
                          <:match_case< $`int:n$ -> 
                                        `$name$ ($mproject (self#expr ctxt e) "unpickle"$ stream) >>)
          | Extends t -> 
              let patt, guard, cast = cast_pattern ctxt t in
                <:match_case< $patt$ when $guard$ -> 
                               $picklen$; $mproject (self#expr ctxt t) "pickle"$ buffer $cast$ >>,
                <:match_case< $`int:n$ -> ($mproject (self#expr ctxt t) "unpickle"$ stream :> a) >>

    method case ctxt (ctor,args) n =
      match args with 
        | [] -> (<:match_case< $uid:ctor$ -> Pickle_int.pickle buffer $`int:n$ >>,
                 <:match_case< $`int:n$ -> $uid:ctor$ >>)
        | _ -> 
        let nargs = List.length args in
        let patt, exp = tuple nargs in
        let pickle, unpickle = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) args) in
        <:match_case< $uid:ctor$ $patt$ -> 
                      Pickle_int.pickle buffer $`int:n$;
                      $pickle$ >>,
        <:match_case< $`int:n$ -> let $patt$ = $unpickle$ in $uid:ctor$ $exp$  >>
    
    method field ctxt : Type.field -> Ast.expr * Ast.expr = function
      | (name, _, `Mutable) -> 
          raise (Underivable ("Pickle cannot be derived for record types with mutable fields ("^name^")"))
      | (name, ([], t), _) -> 
          <:expr< $mproject (self#expr ctxt t) "pickle"$ buffer $lid:name$ >>,
          <:expr< $mproject (self#expr ctxt t) "unpickle"$ stream >>
      | f -> raise (Underivable ("Pickle cannot be derived for record types with polymorphic fields")) 

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) summands = 
      let msg = "Unexpected tag when unpickling " ^ tname ^ ": " in
      let picklers, unpicklers = 
        List.split (List.mapn (self#case ctxt) summands) in
        wrap ~atype:(atype ctxt decl) ~picklers
          ~unpickle:<:expr< match Pickle_int.unpickle stream with $list:unpicklers$ 
                                | n -> raise (Unpickling_failure ($str:msg$ ^ string_of_int n)) >>

    method record ?eq ctxt decl fields = 
       let picklers, unpicklers = 
         List.split (List.map (self#field ctxt) fields) in
       let unpickle = 
         List.fold_right2
           (fun (field,_,_) unpickler e -> 
              <:expr< let $lid:field$ = $unpickler$ in $e$ >>)
           fields
           unpicklers
           (record_expression fields) in
         wrap ~atype:(atype ctxt decl) ~unpickle
               ~picklers:[ <:match_case< $record_pattern fields$ -> $List.fold_left1 seq picklers$ >>]
   
    method variant ctxt decl (_, tags) = 
      let msg = "Unexpected tag when unpickling polymorphic variant: " in
      let picklers, unpicklers = 
        List.split (List.mapn (self#polycase ctxt) tags) in
        wrap ~atype:(atype ctxt decl) ~picklers:(picklers @ [ <:match_case< _ -> assert false >>])
        ~unpickle:<:expr< match Pickle_int.unpickle stream with $list:unpicklers$
                              | n -> raise (Unpickling_failure ($str:msg$ ^ string_of_int n)) >>
  end
end

let _ = Base.register "Pickle"
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
         ~default_module:"Pickle_defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
