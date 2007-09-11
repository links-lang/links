(*pp camlp4of *)

module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)
  
  let classname = "Show"
    
  let wrap (ctxt:Base.context) (decl : Type.decl) matches = <:module_expr< 
  struct type a = $atype ctxt decl$
         let format formatter = function $list:matches$ end >>
    
  let in_a_box box e =
    <:expr< 
      Format.$lid:box$ formatter 0;
      $e$;
      Format.pp_close_box formatter () >>

  let in_hovbox = in_a_box "pp_open_hovbox" and in_box = in_a_box "pp_open_box"


  let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:true
    
    method polycase ctxt : Type.tagspec -> Ast.match_case = function
      | Tag (name, None) -> 
          <:match_case< `$uid:name$ -> 
                        Format.pp_print_string formatter $str:"`" ^ name ^" "$ >>
      | Tag (name, Some e) ->
          <:match_case< `$uid:name$ x ->
                         $in_hovbox <:expr< 
                            Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                            $mproject (self#expr ctxt e) "format"$ formatter x >>$ >>
      | Extends t -> 
          let patt, guard, cast = cast_pattern ctxt t in
            <:match_case<
              $patt$ when $guard$ -> 
              $in_hovbox <:expr< $mproject (self#expr ctxt t) "format"$ formatter $cast$ >>$ >>

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr =
      match exprs with
        | [id,t] -> 
              <:expr< $mproject (self#expr ctxt t) "format"$ formatter $lid:id$ >>
        | exprs ->
            let fmt = 
              "@[<hov 1>("^ String.concat ",@;" (List.map (fun _ -> "%a") exprs) ^")@]" in
              List.fold_left
                (fun f (id, t) ->
                   <:expr< $f$ $mproject (self#expr ctxt t) "format"$ $lid:id$ >>)
                <:expr< Format.fprintf formatter $str:fmt$ >>
                exprs

    method tuple ctxt args = 
      let n = List.length args in
      let tpatt, _ = tuple n in
      <:module_expr< Defaults (struct type a = $atype_expr ctxt (`Tuple args)$
                            let format formatter $tpatt$ = 
                              $self#nargs ctxt 
                                (List.mapn (fun t n -> Printf.sprintf "v%d" n, t) args)$ end) >>

    method case ctxt : Type.summand -> Ast.match_case = 
      fun (name, args) ->
        match args with 
          | [] -> <:match_case< $uid:name$ -> Format.pp_print_string formatter $str:name$ >>
          | _ -> 
              let patt, exp = tuple (List.length args) in
                <:match_case<
                  $uid:name$ $patt$ ->
                  $in_hovbox <:expr<
                    Format.pp_print_string formatter $str:name$;
                Format.pp_print_break formatter 1 2;
                $self#nargs ctxt (List.mapn (fun t n -> Printf.sprintf "v%d" n, t) args)$ >>$ >>
    
    method field ctxt : Type.field -> Ast.expr = function
      | (name, ([], t), _) -> <:expr< Format.pp_print_string formatter $str:name ^ " = "$;
                                      $mproject (self#expr ctxt t) "format"$ formatter $lid:name$ >>
      | f -> raise (Underivable ("Show cannot be derived for record types with polymorphic fields")) 

    method sum ?eq ctxt decl summands = wrap ctxt decl (List.map (self#case ctxt) summands)

    method record ?eq ctxt decl fields = wrap ctxt decl [ <:match_case<
      $record_pattern fields$ -> $in_hovbox
       <:expr<
          Format.pp_print_char formatter '{';
          $List.fold_left1
            (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
            (List.map (self#field ctxt) fields)$;
          Format.pp_print_char formatter '}'; >>$ >>]

    method variant ctxt decl (_,tags) = wrap ctxt decl (List.map (self#polycase ctxt) tags
                                                        @ [ <:match_case< _ -> assert false >> ])
  end
end

let _ = Base.register "Show" 
  ((fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in   
        M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
          ~default_module:"Defaults" ()),
   (fun (loc, context, decls) ->
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~classname:M.classname ~context ~decls))
