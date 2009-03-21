(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Hash"

let wrap ~loc ~atype ~eq hash =
  <:expr< { _Eq = $eq$ ; hash = fun _state -> $hash$ } >>

let rec seq ~loc : Ast.expr list -> Ast.expr  = function
  | [] -> <:expr< () >>
  | [x] -> <:expr< $x$ >>
  | x :: xs -> <:expr< $x$ ; $seq ~loc xs$ >>
  
class hash ~loc = 
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true

  val superclasses = ["Eq"]
  val methods = ["hash"]

  method private nargs : atomic list -> Ast.expr = 
    fun args ->
      seq ~loc 
        (List.mapn (fun at n -> <:expr< $self#atomic at$.hash _state $lid:Printf.sprintf "v%d" n$ >>) args)

  method tuple atype args = 
    let nargs = List.length args in
    let patt, _ = tuple ~loc ~param:"v" nargs in
      wrap ~loc ~atype ~eq:(self#eq_instance atype) <:expr< fun $patt$ -> $self#nargs args$ >> 
  
  method private polycase : Type.tagspec -> int -> Ast.match_case = 
    fun spec n -> match spec with
    | `Tag (name, None)   -> <:match_case< `$name$ -> _state <!> $`int:tag_hash name$ >>
    | `Tag (name, Some e) -> <:match_case< `$name$ l -> $self#atomic e$.hash _state l >>
    | `Local (c, _ as a) -> <:match_case< (# $lid:c$ as l) -> $self#local a$.hash _state l >>
    | `Appl (qname, _ as a) -> let cname = Untranslate.qname ~loc qname in
        <:match_case< (# $id:cname$ as l) -> $self#constr a$.hash _state l >>

  method private case : Type.summand -> int -> Ast.match_case =
    fun (name,args) n ->
      match args with 
        | [] -> <:match_case< $uid:name$ -> _state <!> $`int:n$ >>
        | _ -> let patt, _ = tuple ~loc ~param:"v" (List.length args) in
                  <:match_case< $uid:name$ $patt$ -> _state <!> $`int:n$; $self#nargs args$ >> 
              
  method private field : Type.field -> Ast.expr =
    (fun (name, typ, _) -> <:expr< $self#atomic typ$.hash _state _v.$lid:name$ >>)

  method sum atype ?eq summands =
    wrap ~loc ~atype ~eq:(self#eq_instance atype)
    <:expr< function $list:List.mapn self#case summands$ >>

  method record atype ?eq fields = 
  let projs = 
    seq ~loc
      (List.map self#field fields) in
        wrap ~loc ~atype ~eq:(self#eq_instance atype)
        (<:expr< fun _v -> $projs$ >>)
           
  method variant atype (spec, tags) = 
    wrap ~loc ~atype ~eq:(self#eq_instance atype)
    <:expr< function $list:List.mapn self#polycase tags$ >>

  method private eq_instance (name, params) = 
    let args = List.map (fun (a, _) -> `Tyvar a) params in
      apply_functor ~loc
      <:expr< $lid:"eq_"^ name$ >>  
        (List.map (fun a -> <:expr< $self#atomic a$._Eq >>) args)
end

let _ = Base.register classname (new hash)
