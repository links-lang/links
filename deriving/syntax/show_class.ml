(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Show"

  let wrap ctxt decl matches = <:module_expr< struct type a = $atype ctxt decl$
      let format formatter = function $list:matches$ end >>

  let in_a_box box e =
    <:expr< 
      Format.$lid:box$ formatter 0;
      $e$;
      Format.pp_close_box formatter ()
    >>

  let in_hovbox = in_a_box "pp_open_hovbox" and in_box = in_a_box "pp_open_box"

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum)

  and polycase ctxt : Types.tagspec -> Ast.match_case = function
    | Tag (name, None) -> 
        <:match_case< `$uid:name$ -> 
                      Format.pp_print_string formatter $str:"`" ^ name ^" "$ >>
    | Tag (name, Some e) ->
        <:match_case< `$uid:name$ x ->
                       $in_hovbox <:expr< 
                          Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                          let module M = $expr ctxt e$ in M.format formatter x >>$ >>
    | Extends t -> 
        let patt, guard, cast = cast_pattern ctxt t in
          <:match_case<
            $patt$ when $guard$ -> 
            $in_hovbox <:expr< let module M = $expr ctxt t$ 
                                in M.format formatter $cast$ >>$ >>

  and case ctxt : Types.summand -> Ast.match_case = 
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
               let module M = $expr ctxt (`Tuple args)$ 
                in M.format formatter $exp$
               >>$ >>
    
  and field ctxt : Types.field -> Ast.expr = function
    | (name, ([], t), _) -> <:expr< Format.pp_print_string formatter $str:name ^ " ="$;
                                    let module M = $expr ctxt t$
                                     in M.format formatter $lid:name$ >>
    | f -> raise (Underivable ("Show cannot be derived for record types with polymorphic fields")) 

  and sum ctxt decl summands = wrap ctxt decl (List.map (case ctxt) summands)

  and record ctxt decl fields = wrap ctxt decl [ <:match_case<
     $record_pattern fields$ -> $in_hovbox
      <:expr<
         Format.pp_print_char formatter '{';
         $List.fold_left1
           (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
           (List.map (field ctxt) fields)$;
         Format.pp_print_char formatter '}';
      >>$ >>]

  and variant ctxt decl (_,tags) = wrap ctxt decl (List.map (polycase ctxt) tags)
end

let _ = Base.register "Show" 
  (fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname
         ~default_module:"ShowDefaults" ())
