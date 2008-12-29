(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Eq"

class eq ~loc = 
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true ~default:<:module_expr< Eq.Defaults >>

  method private nargs : atomic list -> Ast.expr = function
    | [t] -> <:expr< $id:self#atomic t$.eq l0 r0 >>
    | args -> 
        fst
          (List.fold_left
             (fun (e, n) t -> 
                let l = Printf.sprintf "l%d" n and r = Printf.sprintf "r%d" n in
                  (<:expr< $id:self#atomic t$.eq $lid:l$ $lid:r$ && $e$ >>, n+1))
             (<:expr< true >>, 0)
             args)

  method tuple atype args = 
    let nargs = List.length args in
    let lpatt, _ = tuple ~loc ~param:"l" nargs
    and rpatt, _ = tuple ~loc ~param:"r" nargs in
      <:module_expr< struct type a = $self#atype atype$
                            let eq $lpatt$ $rpatt$ = $self#nargs args$ end >> 
  
  method private polycase : Type.tagspec -> Ast.match_case = function
    | `Tag (name, None) -> <:match_case< `$name$, `$name$ -> true >>
    | `Tag (name, Some e) -> <:match_case< `$name$ l, `$name$ r -> $id:self#atomic e$.eq l r >>
    | `Local a -> <:match_case< (# $lid:a$ as l, # $lid:a$ as r) -> $id:self#local a$.eq l r >>
    | `Ctor a -> let h = Untranslate.qname ~loc a in
        <:match_case< ((# $id:h$ as l), (# $id:h$ as r)) -> $id:self#ctor a$.eq l r >>

  method private case : Type.summand -> Ast.match_case = 
    fun (name,args) ->
      match args with 
        | [] -> <:match_case< ($uid:name$, $uid:name$) -> true >>
        | _ -> 
            let nargs = List.length args in
            let lpatt, _ = tuple ~loc ~param:"l" nargs
            and rpatt, _ = tuple ~loc ~param:"r" nargs in
              <:match_case< ($uid:name$ $lpatt$, $uid:name$ $rpatt$) ->
                             $self#nargs args$ >> 
              
  method private field : Type.field -> Ast.expr = function
    | (name, t, `Immutable) -> <:expr< $id:self#atomic t$.eq $lid:"l" ^ name$ $lid:"r" ^ name$ >>
    | (_,    _, `Mutable)   -> assert false

  method sum atype ?eq summands =
    let wildcard = match summands with [_] -> [] | _ -> [ <:match_case< _ -> false >>] in
      <:module_expr< struct type a = $self#atype atype$
                            let eq l r = match l, r with 
                                $list:(List.map self#case summands) @ wildcard$
                      end >>

  method record atype ?eq fields = 
    if List.exists (function (_,_,`Mutable) -> true | _ -> false) fields then
       <:module_expr< struct type a = $self#atype atype$ let eq = (==) end >>
    else
    let lpatt = record_pattern ~loc ~prefix:"l" fields
    and rpatt = record_pattern ~loc ~prefix:"r" fields 
    and expr = 
      List.fold_right
        (fun f e -> <:expr< $self#field f$ && $e$ >>)
        fields
        <:expr< true >>
    in <:module_expr< struct type a = $self#atype atype$
                             let eq $lpatt$ $rpatt$ = $expr$ end >>

  method variant atype (spec, tags) = 
    <:module_expr< struct type a = $self#atype atype$
                          let eq l r = match l, r with
                                       $list:List.map self#polycase tags$
                                       | _ -> false end >>
end

let _ = Base.register classname (new eq)
