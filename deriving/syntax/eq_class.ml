module InContext (C : Base.Context) =
struct
  open C
  open Base
  open Util
  open Types
  open Camlp4.PreCast
  include Base.InContext(C)

  let loc = context.loc
  let classname = "Show"

  let lprefix = "l" and rprefix = "r"

  let wildcard_failure = <:match_case< _ -> false >>

  let rec expr t = (new make_module_expr ~classname ~variant ~record ~sum) # expr t
    
  and polycase : Types.tagspec -> Ast.match_case = function
    | Tag (name, None) -> <:match_case< `$name$, `$name$ -> true >>
    | Tag (name, Some e) -> <:match_case< 
        `$name$ l, `$name$ r -> 
           let module M = $expr e$ in M.eq l r >>
    | Extends t -> 
       let lpatt, lguard, lcast = cast_pattern ~param:"l" t in
        let rpatt, rguard, rcast = cast_pattern ~param:"r" t in
          <:match_case<
            ($lpatt$, $rpatt$) when $lguard$ && $rguard$ ->
            let module M = $expr t$ in 
              M.eq $lcast$ $rcast$ >>

  and case : Types.summand -> Ast.match_case = 
    fun (name,args) ->
      let nargs = List.length args in
      let lpatt, lexpr = tuple ~param:"l" nargs
      and rpatt, rexpr = tuple ~param:"r" nargs in
      <:match_case<
        ($uid:name$ $lpatt$, $uid:name$ $rpatt$) ->
          let module M = $expr (Tuple args)$ in
            M.eq $lexpr$ $rexpr$ >> 

  and field : Types.field -> Ast.expr = function
    | (name, ([], t), _) -> <:expr<
        let module M = $expr t$ in
          M.eq $lid:lprefix ^ name$ $lid:rprefix ^ name$ >>
    | f -> raise (Underivable (classname, context.atype)) (* Can't handle "higher-rank" types *)

  and sum summands = <:module_expr< 
      struct type a = $Untranslate.expr context.atype$
             let eq l r = match l, r with 
                          $list:List.map case summands$ | _ -> false
  end >>

  and record fields = 
    let lpatt = record_pattern ~prefix:"l" fields
    and rpatt = record_pattern ~prefix:"r" fields 
    and expr = 
      List.fold_right
        (fun f e -> <:expr< $field f$ && e >>)
        fields
        <:expr< true >>
    in <:module_expr< struct type a = $Untranslate.expr context.atype$
                             let eq $lpatt$ $rpatt$ = $expr$ end >>

  and variant (spec, tags) = 
    <:module_expr< struct type a = $Untranslate.expr context.atype$
                          let eq l r = match l, r with
                                       $list:List.map polycase tags$
                                       | _ -> false end >>
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:(Some "Eq_defaults")
