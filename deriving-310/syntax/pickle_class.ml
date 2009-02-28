(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

(*let tuple_functors = [2;3;4;5;6]*)
let tuple_functors = []

let classname = "Pickle"

let wbind ~loc = let bindop = ">>=" in <:expr< Write.$lid:bindop$ >>
let rbind ~loc = let bindop = ">>=" in <:expr< Read.$lid:bindop$ >>
let wseq  ~loc = let seqop  = ">>"  in <:expr< Write.$lid:seqop$  >>
let rseq  ~loc = let seqop  = ">>"  in <:expr< Read.$lid:seqop$  >>

let find_nonclashing_tag : (Type.name * _) list -> int =
  fun tags ->
    let hashes = List.map (fun (name, _) -> tag_hash name) tags in
    let rec aux candidate = 
      if List.mem candidate hashes then
        aux (candidate + 1)
      else candidate
    in aux 0

let wrap ~loc ~atype ~tymod ~eqmod ~picklers ~unpickler =
  <:expr< { _Typeable = $tymod$ ;
            _Eq = $eqmod$ ;
            pickle = (function $list:picklers$) ; 
            unpickle = $unpickler$ } >>

class pickle ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:false

  val superclasses = ["Eq"; "Typeable"]
  val methods = ["pickle"; "unpickle"]

  method private extension atype tname ts : Ast.match_case =
    (* Try each extension in turn.  If we get an UnknownTag failure,
       try the next one.  This is

       * safe because any two extensions that define the same tag
       must be compatible at that point

       * fast because we can tell on the first integer comparison
       whether we've picked the right path or not.
    *)
      let inner = List.fold_right 
        (fun spec exp -> 
           match spec with
             | `Local t -> 
                 (<:expr< try $exp$
                          with UnknownTag _ -> ($self#local t$.unpickle x :> $atype$ Read.m) >>)
             | `Appl (qname, _ as c) ->
                 (<:expr< try $exp$
                          with UnknownTag _ -> ($self#constr c$.unpickle x :> $atype$ Read.m) >>))
        ts
        <:expr< raise (UnknownTag (n, ($str:"Unexpected tag encountered during unpickling of "
                                       ^tname$))) >>
    in <:match_case< n,[x] -> $inner$ >>

    method private polycase atype extension_tag tagspec : Ast.match_case = 
      let typeable = self#typeable_instance atype and eq = self#eq_instance atype in
      match tagspec with
    | `Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
          allocate $typeable$ $eq$ obj
              (fun thisid -> 
                 store_repr thisid
                    (Repr.make ~constructor:$`int:(tag_hash name)$ [])) >>
    | `Tag (name, Some t) -> <:match_case<
        (`$name$ v1 as obj) ->
           allocate $typeable$ $eq$ obj
            (fun thisid ->
             $wbind ~loc$ ($self#atomic t$.pickle v1)
                    (fun mid -> 
                    (store_repr thisid
                        (Repr.make ~constructor:$`int:(tag_hash name)$ [mid])))) >>
    | `Local (c, _ as t) -> 
        (* TODO: Find a tag number for each type that is guaranteed
           not to clash.  *)
          <:match_case<
            (# $lid:c$) as obj ->
            allocate $typeable$ $eq$ obj
            (fun thisid ->
               $wbind ~loc$ ($self#local t$.pickle obj)
                 (fun mid -> 
                    (store_repr thisid
                       (Repr.make ~constructor:$`int:extension_tag$ [mid])))) >>
    | `Appl (qname, _ as t) -> 
          <:match_case<
            (# $id:Untranslate.qname ~loc qname$) as obj ->
            allocate $typeable$ $eq$ obj
            (fun thisid ->
               $wbind ~loc$ ($self#constr t$.pickle obj)
                 (fun mid -> 
                    (store_repr thisid
                       (Repr.make ~constructor:$`int:extension_tag$ [mid])))) >>

        (*
          <:match_case< (# $lid:t$) as obj -> $self#local t$.pickle obj >>
        *)

    method private polycase_un tagspec : Ast.match_case = match tagspec with
    | (name, None)   -> <:match_case< $`int:(tag_hash name)$, [] -> Read.return `$name$ >>
    | (name, Some t) -> <:match_case< $`int:(tag_hash name)$, [x] -> 
      $rbind ~loc$ ($self#atomic t$.unpickle x) (fun o -> Read.return (`$name$ o)) >>

    method variant (tname, params as atype) (_, tagspec) : Ast.expr =
      let unpickler, extension_tag = 
        let tags, extensions = either_partition
          (function (`Tag (name,t)) -> Left (name,t) | (`Local _) as t -> Right t | `Appl _ as t -> Right t) tagspec in
        let tag_cases : Ast.match_case list = List.map self#polycase_un tags in
        let extension_case : Ast.match_case = self#extension (self#atype atype) tname extensions in
          (<:expr< fun id -> sum $self#typeable_instance atype$ (function $list:tag_cases @ [extension_case]$) id >>,
           find_nonclashing_tag tags)
      in
        wrap
          ~loc
          ~atype:(self#atype atype)
          ~tymod:(self#typeable_instance atype)
          ~eqmod:(self#eq_instance atype)
          ~picklers:(List.map (self#polycase atype extension_tag) tagspec)
          ~unpickler


    method tuple (name, params as atype) ts : Ast.expr = 
      let nts = List.length ts in
        if List.mem nts tuple_functors then
          apply_functor ~loc <:expr< $lid:Printf.sprintf "pickle_%d" nts$ >> 
            (List.map self#atomic ts)
        else
          let ids = (List.mapn (fun t n -> (Printf.sprintf "id%d" n, t)) ts) in
          let eidlist = expr_list ~loc (List.map (fun (id,_) -> <:expr< $lid:id$ >>) ids) in
          let pidlist = patt_list ~loc (List.map (fun (id,_) -> <:patt< $lid:id$ >>) ids) in
          let tpatt,texpr = tuple ~loc ~param:"id" nts in
          let tymod = self#typeable_instance (name, params)
          and eqmod = self#eq_instance (name, params)
          and picklers =
            let inner = 
              List.fold_right
                (fun (id,t) expr -> 
                   <:expr< $wbind ~loc$ ($self#atomic t$.pickle $lid:id$) 
                              (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< store_repr this (Repr.make $eidlist$) >> in
              [ <:match_case< ($tpatt$ as obj) -> 
                               allocate $self#typeable_instance atype$ $self#eq_instance atype$ obj (fun this -> $inner$) >>]
                
          and unpickler = 
            let msg = "unexpected object encountered unpickling "^string_of_int nts^"-tuple" in
            let inner = 
              List.fold_right 
                (fun (id,t) expr ->
                   <:expr< $rbind ~loc$ ($self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< Read.return $texpr$ >> in
              <:expr< fun c -> (tuple $self#typeable_instance atype$
                                  (function 
                                     | $pidlist$ -> $inner$
                                     | _ -> raise (UnpicklingError $str:msg$))) c >>
          and atype = self#atype (name, params) in
            wrap ~loc ~atype ~tymod ~eqmod ~picklers ~unpickler

    method private case atype (name, params' : summand) (n : int) : Ast.match_case * Ast.match_case = 
      let nparams = List.length params' in
      let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
      let exp = 
        List.fold_right2
          (fun p n tail -> 
             <:expr< $wbind ~loc$ ($self#atomic p$.pickle $lid:Printf.sprintf "v%d" n$)
                            (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
          params'
          (List.range 0 nparams)
          <:expr< store_repr thisid (Repr.make ~constructor:$`int:n$ $expr_list ~loc ids$) >> in
        match params' with
          | [] -> <:match_case< $uid:name$ as obj -> 
                                allocate $self#typeable_instance atype$ $self#eq_instance atype$ obj (fun thisid -> $exp$) >>,
                  <:match_case< $`int:n$, [] -> Read.return $uid:name$ >>
          | _  -> <:match_case< $uid:name$ $fst (tuple ~loc ~param:"v" nparams)$ as obj -> 
                                allocate $self#typeable_instance atype$ $self#eq_instance atype$ obj (fun thisid -> $exp$) >>,
      let _, tuple = tuple ~loc ~param:"id" nparams in
      let patt, exp = 
        List.fold_right2 
          (fun n t (pat, exp) ->
             let id = Printf.sprintf "id%d" n in
             <:patt< $lid:id$ :: $pat$ >>,
             <:expr< $rbind ~loc$ ($self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $exp$) >>)
          (List.range 0 nparams)
          params'
        (<:patt< [] >>, <:expr< Read.return ($uid:name$ $tuple$) >>) in
        <:match_case< $`int:n$, $patt$ -> $exp$ >>


    method sum (tname, params as atype) ?eq (summands : summand list) : Ast.expr =
      let picklers, unpicklers = List.split (List.mapn (self#case atype) summands) in
      wrap
        ~loc
        ~atype:(self#atype (tname, params))
        ~tymod:(self#typeable_instance (tname, params))
        ~eqmod:(self#eq_instance (tname, params))
        ~picklers
        ~unpickler:<:expr< fun id -> 
                           let f = function
                                    $list:unpicklers$ 
                                   | n,_ -> raise (UnpicklingError ($str:"Unexpected tag when unpickling "
                                                                    ^tname^": "$^ string_of_int n))
                           in sum $self#typeable_instance atype$ f id >>

    method record (tname, params as atype) ?eq (fields : Type.field list) : Ast.expr =
      let picklers =
        let inner =
          List.fold_right 
            (fun (id,t,_) e ->
               <:expr< $wbind ~loc$ ($self#atomic t$.pickle $lid:id$) (fun $lid:id$ -> $e$) >>)
            fields
          <:expr< (store_repr this
                     (Repr.make
                        $expr_list ~loc (List.map (fun (id,_,_) -> <:expr< $lid:id$ >>) fields)$)) >>
        in
          [ <:match_case< ($record_pattern ~loc fields$ as obj) ->
            allocate $self#typeable_instance atype$ $self#eq_instance atype$ obj (fun this -> $inner$) >> ] in

      let unpickle_record_bindings e = 
        (* TODO: 

           This is a small problem: we need to instantiate
           type variables in the field types with functor arguments.

           However, the field types are constrained to atomics, which
           doesn't include qualified names.

           Probably the proper thing to do is to have some alternative 
           translation mechanism.
        *)
 
        let cparams = List.map (fun (p, _) -> <:ctyp< '$lid:p$ >>) params in
             
        let mutables = List.map (fun (name, rhs, _) -> (name, rhs, `Mutable)) fields in
        let mutrec = Untranslate.repr ~loc (`Record mutables) in
        let tdecl = Ast.TyDcl (loc, "t", cparams, mutrec, []) in
          <:expr< fun arg -> (let module Mutable = struct type $tdecl$ end in $e$) arg >> in
                 
        let unpickler = 
          let msg = "unexpected object encountered unpickling "^tname in
          let assignments = 
            List.fold_right
              (fun (id,_,_) exp ->
                 <:expr< this.Mutable.$lid:id$ <- $lid:id$; $exp$ >>)
              fields
            <:expr< Read.return self >> in
          let inner = 
            List.fold_right
              (fun (id,t,_) exp ->
                 <:expr< $rbind ~loc$ ($self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $exp$) >>)
              fields
              assignments in
          let idpat = patt_list ~loc (List.map (fun (id,_,_) -> <:patt< $lid:id$ >>) fields) in
          let ttype = List.fold_left (fun t (p, _) -> <:ctyp< '$p$ $t$ >>) <:ctyp< Mutable.t >> params in
            unpickle_record_bindings
              (<:expr< record $self#typeable_instance atype$
                 (fun self -> function
                    | $idpat$ -> let this = (Obj.magic self : $ttype$) in $inner$
                    | _ -> raise (UnpicklingError $str:msg$)) $`int:List.length fields$ >>)
        in 
          wrap ~loc ~picklers ~unpickler
            ~atype:(self#atype (tname, params))
            ~tymod:(self#typeable_instance (tname, params))
            ~eqmod:(self#eq_instance (tname, params))

    method private typeable_instance (name, params) =
      let args = List.map (fun (a, _) -> `Tyvar a) params in
        apply_functor ~loc
        <:expr< $lid:"typeable_"^ name$ >>  
          (List.map (fun a -> <:expr< $self#atomic a$._Typeable >>) args)

    method private eq_instance (name, params) =
      let args = List.map (fun (a, _) -> `Tyvar a) params in
        apply_functor ~loc
        <:expr< $lid:"eq_"^ name$ >>  
          (List.map (fun a -> <:expr< $self#atomic a$._Eq >>) args)
end

let () = Base.register classname (new pickle)

