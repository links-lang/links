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

  let in_a_box box e =
    <:expr< 
      Format.$lid:box$ formatter 0;
      $e$;
      Format.pp_close_box formatter ()
    >>

  let in_hovbox = in_a_box "pp_open_hovbox"
  let in_box = in_a_box "pp_open_box"

  let rec expr t = (new make_module_expr ~classname ~variant ~record ~sum) # expr t

  and polycase : Types.tagspec -> Ast.match_case = function
    | Tag (name, None) -> 
        <:match_case< `$uid:name$ -> 
                      Format.pp_print_string formatter $str:"`" ^ name ^" "$ >>
    | Tag (name, Some e) ->
        <:match_case< `$uid:name$ x ->
                       $in_hovbox <:expr< 
                          Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                          let module M = $expr e$ in M.format formatter x >>$ >>
    | Extends t -> 
        let patt, guard, cast = cast_pattern t in
          <:match_case<
            $patt$ when $guard$ -> 
            $in_hovbox <:expr< let module M = $expr t$ 
                                in M.format formatter cast >>$ >>

  and case : Types.summand -> Ast.match_case =  (* Does this handle the zero-arg case correctly? *)
    fun (name, args) ->
      let patt, exp = tuple (List.length args) in
      <:match_case<
        $uid:name$ $patt$ ->
         $in_hovbox <:expr<
           Format.pp_print_string formatter $str:name$;
           Format.pp_print_break formatter 1 2;
           let module M = $expr (Tuple args)$ 
            in M.format formatter $exp$
           >>$ >>

  and field : Types.field -> Ast.expr = function
    | (name, ([], t), _) -> <:expr< Format.pp_print_string formatter $str:name ^ " ="$;
                                    let module M = $expr t$
                                     in M.format formatter $lid:name$ >>
    | f -> raise (Underivable (classname, context.atype)) (* Can't handle "higher-rank" types *)

  and sum summands = <:module_expr< struct type a = $Untranslate.expr context.atype$
    let rec format formatter = function $list:List.map case summands$
  end >>

  and record fields = <:module_expr< struct type a = $Untranslate.expr context.atype$
    let rec format formatter $record_pattern fields$ = $in_hovbox
      <:expr<
         Format.pp_print_char formatter '{'
         $List.fold_left1
           (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
           (List.map field fields)$
         Format.pp_print_char formatter '}'
      >>$
  end >>

  and variant (spec, tags) = <:module_expr< struct type a = $Untranslate.expr context.atype$
    let rec format formatter = function $list:List.map polycase tags$
  end >>
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:(Some "Show_defaults")
