(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Dump"

let wrap ~loc ~atype ~dumpers ~undump =
  <:module_expr< struct type a = $atype$
                        let to_buffer buffer = function $list:dumpers$
                        let from_stream stream = $undump$ end >>

class dump ~loc =
object (self)
  inherit Base.deriver ~loc ~classname ~allow_private:false ~default:<:module_expr< Dump.Defaults >>

    method private nargs (exprs : (name * Type.atomic) list) : Ast.expr * Ast.expr =
      List.fold_right
        (fun (id,t) (p,u) -> 
           <:expr< $id:self#atomic t$.to_buffer buffer $lid:id$; $p$ >>,
           <:expr< let $lid:id$ = $id:self#atomic t$.from_stream stream in $u$ >>)
        exprs (<:expr<>>, <:expr< $tuple_expr ~loc (List.map (fun (id,_) -> <:expr< $lid:id$ >>) exprs)$>>)

    method tuple atype ts = 
      let atype = self#atype atype in
      let dumpers, undump = 
        let n = List.length ts in 
        let pinner, undump = self#nargs (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) ts) in
        let patt, expr = tuple ~loc n in
          [ <:match_case< $patt$ -> $pinner$ >> ], undump in
        wrap ~loc ~atype ~dumpers ~undump

    method private polycase (tagspec : tagspec) n : Ast.match_case * Ast.match_case = 
      let dumpn = <:expr< Dump_int.to_buffer buffer $`int:n$ >> in
        match tagspec with
          | `Tag (name,None) -> <:match_case< `$name$ -> $dumpn$ >>,
                               <:match_case< $`int:n$ -> `$name$ >>
          | `Tag (name, Some e) -> 
                               <:match_case< `$name$ x -> $dumpn$;
                                                           $id:self#atomic e$.to_buffer buffer x >>,
                               <:match_case< $`int:n$ ->  `$name$ ($id:self#atomic e$.from_stream stream) >>
          | `Local a -> 
                <:match_case< (# $lid:a$ as x) ->
                               $dumpn$; $id:self#local a$.to_buffer buffer x >>,
                <:match_case< $`int:n$ -> ($id:self#local a$.from_stream stream :> a) >>

    method private case (ctor,args) n =
      match args with 
        | [] -> (<:match_case< $uid:ctor$ -> Dump_int.to_buffer buffer $`int:n$ >>,
                 <:match_case< $`int:n$ -> $uid:ctor$ >>)
        | _ -> 
        let nargs = List.length args in
        let patt, exp = tuple ~loc nargs in
        let dump, undump = self#nargs (List.mapn (fun t n -> (Printf.sprintf "v%d" n, t)) args) in
        <:match_case< $uid:ctor$ $patt$ -> 
                      Dump_int.to_buffer buffer $`int:n$;
                      $dump$ >>,
        <:match_case< $`int:n$ -> let $patt$ = $undump$ in $uid:ctor$ $exp$  >>
    
    method private field : Type.field -> Ast.expr * Ast.expr = function
      | (name, _, `Mutable) -> 
          raise (Underivable (loc, "Dump cannot be derived for record types with mutable fields ("^name^")"))
      | (name, t, `Immutable) -> 
          <:expr< $id:self#atomic t$.to_buffer buffer $lid:name$ >>,
          <:expr< $id:self#atomic t$.from_stream stream >>

    method sum (name, params) ?eq summands = 
      let atype = self#atype (name, params) in
      let msg = "Dump: unexpected tag %d at character %d when deserialising " ^ name in
      let dumpers, undumpers = List.split (List.mapn self#case summands) in
        wrap ~loc ~atype ~dumpers
          ~undump:<:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                                | n -> raise (Dump_error
                                                (Printf.sprintf $str:msg$ n
                                                   (Stream.count stream))) >>

    method record atype ?eq fields = 
      let atype = self#atype atype in
       let dumpers, undumpers = List.split (List.map self#field fields) in
       let undump = 
         List.fold_right2
           (fun (field,_,_) undumper e -> 
              <:expr< let $lid:field$ = $undumper$ in $e$ >>)
           fields
           undumpers
           (record_expression ~loc fields) in
         wrap ~loc ~atype ~undump
               ~dumpers:[ <:match_case< $record_pattern ~loc fields$ -> $List.fold_left1 (seq ~loc) dumpers$ >>]
   
    method variant atype (_, tags) = 
      let atype = self#atype atype in
      let msg = "Dump: unexpected tag %d at character %d when deserialising polymorphic variant" in
      let dumpers, undumpers = List.split (List.mapn self#polycase tags) in
        wrap ~loc ~atype ~dumpers:(dumpers @ [ <:match_case< _ -> assert false >>])
          ~undump:<:expr< match Dump_int.from_stream stream with $list:undumpers$ 
                                | n -> raise (Dump_error
                                                (Printf.sprintf $str:msg$ n
                                                   (Stream.count stream))) >>

end

let _ = Base.register classname (new dump)
