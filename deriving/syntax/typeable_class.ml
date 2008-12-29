(*pp camlp4of *)

open Type
open Base
open Camlp4.PreCast
  
let classname = "Typeable"

let mkName ~loc : name -> string = 
  let file_name, sl, _, _, _, _, _, _ = Loc.to_tuple loc in
    Printf.sprintf "%s_%d_%f_%s" 
      file_name sl (Unix.gettimeofday ())

let gen ~loc atype (tname,params) = 
  let paramList = 
    List.fold_right 
      (fun (p,_) cdr ->
           <:expr< $uid:tvar_name p$.type_rep::$cdr$ >>)
      params
    <:expr< [] >>
  in <:module_expr< struct type a = $atype$
                           let type_rep = TypeRep.mkFresh $str:mkName ~loc tname$ $paramList$ end >>

class typeable ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true ~default:<:module_expr< Typeable.Defaults >>

  method tuple atype ts = 
    let params = 
      expr_list ~loc
        (List.map (fun t -> <:expr< $id:self#atomic t$.type_rep >>) ts) in
      <:module_expr< struct type a = $self#atype atype$
                            let type_rep = Typeable.TypeRep.mkTuple $params$ end >>
  method sum ctyp ?eq _ = gen ~loc (self#atype ctyp) ctyp  (* TODO: handle equations correctly *)
  method record ctyp ?eq _ = gen ~loc (self#atype ctyp) ctyp  (* TODO: handle equations correctly *)
  method variant atype (_,tags) =
    let tags, extends = 
      List.fold_left 
        (fun (tags, extends) -> function
           | `Tag (l, None)  -> <:expr< ($str:l$, None) :: $tags$ >>, extends
           | `Tag (l,Some t) -> <:expr< ($str:l$, Some $id:self#atomic t$.type_rep) :: $tags$ >>, extends
           | `Local t        -> tags, <:expr< $id:self#local t$.type_rep ::$extends$ >>
           | `Ctor c         -> tags, <:expr< $id:self#ctor c$.type_rep ::$extends$ >>)
        (<:expr< [] >>, <:expr< [] >>) tags in
      <:module_expr< struct type a = $self#atype atype$
                            let type_rep = Typeable.TypeRep.mkPolyv $tags$ $extends$
                     end >>
end

let () = Base.register classname (new typeable)
