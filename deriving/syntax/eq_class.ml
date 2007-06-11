(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Eq"

  let lprefix = "l" and rprefix = "r"

  let wildcard_failure = <:match_case< _ -> false >>

  let tup ctxt ts mexpr exp = 
      match ts with
        | [t] -> 
            <:module_expr< struct type a = $atype_expr ctxt (`Tuple ts)$
                                  let eq l r = let module M = $exp ctxt t$ 
                                   in $mexpr$ l r end >>
        | ts ->
            let _, (lpatt, rpatt), expr = 
              List.fold_right
                (fun t (n, (lpatt, rpatt), expr) ->
                   let lid = Printf.sprintf "l%d" n and rid = Printf.sprintf "r%d" n in
                     (n+1,
                      (Ast.PaCom (loc,<:patt< $lid:lid$ >>, lpatt),
                       Ast.PaCom (loc,<:patt< $lid:rid$ >>, rpatt)),
                      <:expr< let module M = $exp ctxt t$ 
                              in $mexpr$ $lid:lid$ $lid:rid$ && $expr$ >>))
                ts
                (0, (<:patt< >>, <:patt< >>), <:expr< true >>)
            in 
              <:module_expr< struct type a = $atype_expr ctxt (`Tuple ts)$
                                    let eq $Ast.PaTup (loc, lpatt)$ $Ast.PaTup (loc, rpatt)$ = $expr$ end >>


  let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:true

  method tuple ctxt ts = tup ctxt ts <:expr< M.eq >> (self#expr)
    
  method polycase ctxt : Type.tagspec -> Ast.match_case = function
    | Tag (name, None) -> <:match_case< `$name$, `$name$ -> true >>
    | Tag (name, Some e) -> <:match_case< 
        `$name$ l, `$name$ r -> 
           $mproject (self#expr ctxt e) "eq"$ l r >>
    | Extends t -> 
        let lpatt, lguard, lcast = cast_pattern ctxt ~param:"l" t in
        let rpatt, rguard, rcast = cast_pattern ctxt ~param:"r" t in
          <:match_case<
            ($lpatt$, $rpatt$) when $lguard$ && $rguard$ ->
            $mproject (self#expr ctxt t) "eq"$ $lcast$ $rcast$ >>
  
  method case ctxt : Type.summand -> Ast.match_case = 
    fun (name,args) ->
      match args with 
        | [] -> <:match_case< ($uid:name$, $uid:name$) -> true >>
        | _ -> 
            let nargs = List.length args in
            let lpatt, lexpr = tuple ~param:"l" nargs
            and rpatt, rexpr = tuple ~param:"r" nargs in
              <:match_case<
                ($uid:name$ $lpatt$, $uid:name$ $rpatt$) ->
                   $mproject (self#expr ctxt (`Tuple args)) "eq"$ $lexpr$ $rexpr$ >> 
              
  method field ctxt : Type.field -> Ast.expr = function
    | (name, ([], t), `Immutable) -> <:expr<
        $mproject (self#expr ctxt t) "eq"$ $lid:lprefix ^ name$ $lid:rprefix ^ name$ >>
    | (_, _, `Mutable) -> assert false
    | f -> raise (Underivable ("Eq cannot be derived for record types with polymorphic fields")) 

  method sum ?eq ctxt decl summands =
    let wildcard = match summands with [_] -> [] | _ -> [ <:match_case< _ -> false >>] in
  <:module_expr< 
      struct type a = $atype ctxt decl$
             let eq l r = match l, r with 
                          $list:List.map (self#case ctxt) summands @ wildcard$
  end >>

  method record ?eq ctxt decl fields = 
    if List.exists (function (_,_,`Mutable) -> true | _ -> false) fields then
       <:module_expr< struct type a = $atype ctxt decl$ let eq = (==) end >>
    else
    let lpatt = record_pattern ~prefix:"l" fields
    and rpatt = record_pattern ~prefix:"r" fields 
    and expr = 
      List.fold_right
        (fun f e -> <:expr< $self#field ctxt f$ && $e$ >>)
        fields
        <:expr< true >>
    in <:module_expr< struct type a = $atype ctxt decl$
                             let eq $lpatt$ $rpatt$ = $expr$ end >>

  method variant ctxt decl (spec, tags) = 
    <:module_expr< struct type a = $atype ctxt decl$
                          let eq l r = match l, r with
                                       $list:List.map (self#polycase ctxt) tags$
                                       | _ -> false end >>
  end
end

let _ = Base.register "Eq" 
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
         ~default_module:"Eq_defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))

