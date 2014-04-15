(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Show"

let tuple_functors = [2;3;4;5;6]
let tuple_functors = []

let in_a_box ~loc box e =
  <:expr<  Format.$lid:box$ formatter 0;
           $e$;
           Format.pp_close_box formatter () >>

let in_hovbox = in_a_box "pp_open_hovbox" and in_box = in_a_box "pp_open_box"

let wrap ~loc atype matches = <:expr<  {format = fun formatter -> function $list:matches$ } >>

class show ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true

    val methods = ["format"]

    method private field : field -> Ast.expr =
      fun (name, t, _) -> <:expr< Format.pp_print_string formatter $str:name ^ " ="$;
                                  ($self#atomic t$).format formatter $lid:name$ >>

    method private case : summand -> Ast.match_case = function
      | name, [] -> <:match_case< $uid:name$ -> Format.pp_print_string formatter $str:name$ >>
      | name, args -> 
          let patt, exp = tuple ~loc (List.length args) in
            <:match_case<
              $uid:name$ $patt$ ->
              $in_hovbox ~loc <:expr< Format.pp_print_string formatter $str:name$;
                                      Format.pp_print_break formatter 1 2;
                                      $self#nargs args$ >>$ >>

    method private polycase : tagspec -> Ast.match_case = function
      | `Tag (name, None) -> 
          <:match_case< `$uid:name$ -> 
                        Format.pp_print_string formatter $str:"`" ^ name$ >>
      | `Tag (name, Some e) ->
          <:match_case< `$uid:name$ x ->
                         $in_hovbox ~loc <:expr< 
                            Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                            ($self#atomic e$).format formatter x >>$ >>
      | `Local (c, _ as a) -> 
          let rhs = in_hovbox ~loc <:expr< ($self#local a$).format formatter x >>  in
            <:match_case<(# $lid:c$ as x) -> $ rhs$ >>
      | `Appl (qname, _ as c) ->
          let rhs = in_hovbox ~loc <:expr< ($self#constr c$).format formatter x >>  in
            <:match_case<(# $id:Untranslate.qname ~loc qname$ as x) -> $ rhs$ >>

  method sum ctyp ?eq summands = wrap ~loc (self#atype ctyp) (List.map self#case summands)

  method record ctyp ?eq fields = wrap ~loc (self#atype ctyp) [ <:match_case<
      $record_pattern ~loc fields$ -> $in_hovbox ~loc
       <:expr<
          Format.pp_print_char formatter '{';
          $List.fold_left1
            (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
            (List.map self#field fields)$;
          Format.pp_print_char formatter '}'; >>$ >>]

  method variant ctyp (_,tags) = 
    wrap ~loc (self#atype ctyp) (List.map self#polycase tags @ [ <:match_case< _ -> assert false >> ])

  method private nargs : atomic list -> Ast.expr = function
    | [t] -> <:expr< ($self#atomic t$).format formatter v0 >>
    | args ->
        let exprs = List.mapn (fun t n -> Printf.sprintf "v%d" n, t) args in
        let fmt = 
          "@[<hov 1>("^ String.concat ",@;" (List.map (fun _ -> "%a") exprs) ^")@]" in
          List.fold_left
            (fun f (id, t) ->
               <:expr< $f$ ($self#atomic t$).format $lid:id$ >>)
            <:expr< Format.fprintf formatter $str:fmt$ >>
            exprs

  method tuple ctyp args = 
    let n = List.length args in
      if List.mem n tuple_functors then
        apply_functor ~loc <:expr< $lid:Printf.sprintf "show_%d" n$ >>
          (List.map self#atomic args)
      else
        let tpatt, _ = tuple ~loc n in
        let matcher = <:match_case< $tpatt$ -> $self#nargs args$ >>
        in wrap ~loc (self#atype ctyp) [matcher]
end
  
let _ = Base.register classname (new show)
