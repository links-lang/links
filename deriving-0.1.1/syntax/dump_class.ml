(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Dump"

  let wrap ~atype ~dumpers ~undump =
    <:module_expr< struct type a = $atype$
                          let to_buffer buffer = function $list:dumpers$
                          let from_stream stream = $undump$ end >>

  let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:false

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr * Ast.expr =
      List.fold_right
        (fun (id,t) (p,u) -> 
           <:expr< $mproject (self#expr ctxt t) "to_buffer"$ buffer $lid:id$; $p$ >>,
           <:expr< let $lid:id$ = $mproject (self#expr ctxt t) "from_stream"$ stream in $u$ >>)
        exprs (<:expr<>>, <:expr< $tuple_expr (List.map (fun (id,_) -> <:expr< $lid:id$ >>) exprs)$>>)

    method tuple ctxt ts = 
      let atype = atype_expr ctxt (`Tuple ts)
      and dumpers, undump = 
        let n = List.length ts in 
        let pinner, undump = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) ts) in
        let patt, expr = tuple n in
          [ <:match_case< $patt$ -> $pinner$ >> ], undump in
        <:module_expr< Defaults( $wrap ~atype ~dumpers ~undump$) >>

    method polycase ctxt tagspec n : Ast.match_case * Ast.match_case = 
      let dumpn = <:expr< Dump_int.to_buffer buffer $`int:n$ >> in
        match tagspec with
          | Tag (name, args) -> (match args with 
              | None   -> <:match_case< `$name$ -> $dumpn$ >>,
                          <:match_case< $`int:n$ -> `$name$ >>
              | Some e -> <:match_case< `$name$ x -> $dumpn$;
                                         $mproject (self#expr ctxt e) "to_buffer"$ buffer x >>,
                          <:match_case< $`int:n$ -> 
                                        `$name$ ($mproject (self#expr ctxt e) "from_stream"$ stream) >>)
          | Extends t -> 
              let patt, guard, cast = cast_pattern ctxt t in
                <:match_case< $patt$ when $guard$ -> 
                               $dumpn$; $mproject (self#expr ctxt t) "to_buffer"$ buffer $cast$ >>,
                <:match_case< $`int:n$ -> ($mproject (self#expr ctxt t) "from_stream"$ stream :> a) >>

    method case ctxt (ctor,args) n =
      match args with 
        | [] -> (<:match_case< $uid:ctor$ -> Dump_int.to_buffer buffer $`int:n$ >>,
                 <:match_case< $`int:n$ -> $uid:ctor$ >>)
        | _ -> 
        let nargs = List.length args in
        let patt, exp = tuple nargs in
        let dump, undump = self#nargs ctxt (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) args) in
        <:match_case< $uid:ctor$ $patt$ -> 
                      Dump_int.to_buffer buffer $`int:n$;
                      $dump$ >>,
        <:match_case< $`int:n$ -> let $patt$ = $undump$ in $uid:ctor$ $exp$  >>
    
    method field ctxt : Type.field -> Ast.expr * Ast.expr = function
      | (name, _, `Mutable) -> 
          raise (Underivable ("Dump cannot be derived for record types with mutable fields ("^name^")"))
      | (name, ([], t), _) -> 
          <:expr< $mproject (self#expr ctxt t) "to_buffer"$ buffer $lid:name$ >>,
          <:expr< $mproject (self#expr ctxt t) "from_stream"$ stream >>
      | f -> raise (Underivable ("Dump cannot be derived for record types with polymorphic fields")) 

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) summands = 
      let msg = "Dump: unexpected tag %d at character %d when deserialising " ^ tname in
      let dumpers, undumpers = 
        List.split (List.mapn (self#case ctxt) summands) in
        wrap ~atype:(atype ctxt decl) ~dumpers
          ~undump:<:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                                | n -> raise (Dump_error
                                                (Printf.sprintf $str:msg$ n
                                                   (Stream.count stream))) >>

    method record ?eq ctxt decl fields = 
       let dumpers, undumpers = 
         List.split (List.map (self#field ctxt) fields) in
       let undump = 
         List.fold_right2
           (fun (field,_,_) undumper e -> 
              <:expr< let $lid:field$ = $undumper$ in $e$ >>)
           fields
           undumpers
           (record_expression fields) in
         wrap ~atype:(atype ctxt decl) ~undump
               ~dumpers:[ <:match_case< $record_pattern fields$ -> $List.fold_left1 seq dumpers$ >>]
   
    method variant ctxt decl (_, tags) = 
      let msg = "Dump: unexpected tag %d at character %d when deserialising polymorphic variant" in
      let dumpers, undumpers = 
        List.split (List.mapn (self#polycase ctxt) tags) in
        wrap ~atype:(atype ctxt decl) ~dumpers:(dumpers @ [ <:match_case< _ -> assert false >>])
          ~undump:<:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                                | n -> raise (Dump_error
                                                (Printf.sprintf $str:msg$ n
                                                   (Stream.count stream))) >>
  end
end

let _ = Base.register "Dump"
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
         ~default_module:"Defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
