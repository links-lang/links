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
           <:expr< $lid:p$.type_rep::$cdr$ >>)
      params
    <:expr< [] >>
  in <:expr< {type_rep = lazy (Lazy.force (TypeRep.mkFresh $str:mkName ~loc tname$ $paramList$)) }>>

class typeable ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true

  val methods = ["type_rep"]

  method tuple atype ts = 
    let params = 
      expr_list ~loc
        (List.map (fun t -> <:expr< $self#atomic t$.type_rep >>) ts) in
      <:expr< { type_rep = lazy (Lazy.force (Typeable.TypeRep.mkTuple $params$))  } >>
  method sum ctyp ?eq _ = gen ~loc (self#atype ctyp) ctyp  (* TODO: handle equations correctly *)
  method record ctyp ?eq _ = gen ~loc (self#atype ctyp) ctyp  (* TODO: handle equations correctly *)
  method variant atype (_,tags) =
    let tags, extends = 
      List.fold_left 
        (fun (tags, extends) -> function
           | `Tag (l, None)  -> <:expr< ($str:l$, None) :: $tags$ >>, extends
           | `Tag (l,Some t) -> <:expr< ($str:l$, Some ($self#atomic t$.type_rep)) :: $tags$ >>, extends
           | `Local t        -> tags, <:expr< $self#local t$.type_rep ::$extends$ >>
           | `Appl (_ as c)  -> tags, <:expr< $self#constr c$.type_rep ::$extends$ >>)
        (<:expr< [] >>, <:expr< [] >>) tags in
      <:expr< { type_rep = lazy (Lazy.force (Typeable.TypeRep.mkPolyv $tags$ $extends$)) } >>

    method expand : Ast.expr -> Ast.expr = 
      fun expr -> <:expr< { type_rep = lazy (Lazy.force $expr$.type_rep) } >>
end

let () = Base.register classname (new typeable)
