(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)
  module UT = Type.Untranslate(L)

  let typeable_defaults t = <:module_expr< Typeable.Typeable_defaults($t$) >>

  module Typeable = Typeable_class.InContext(L)
  module Eq       = Eq_class.InContext(L)

  let classname = "Shelve"
  let bind, seq = 
    let bindop = ">>=" and seqop = ">>" in
      <:expr< $lid:bindop$ >>, <:expr< $lid:seqop$ >>

  let unshelve_record_bindings ctxt (tname,params,rhs,cs,_) (fields : field list) e = <:expr<
      let num_fields = $`int:List.length fields$ in
      let module Mutable = struct
        type t = $UT.repr 
            (instantiate_modargs_repr ctxt 
               (Record (List.map (fun (n,p,_) -> (n,p,`Mutable)) fields)))$
      end in $e$ >>

  let unshelve_record ctxt (tname,_,_,_,_ as decl) fields expr = 
    let msg = "unexpected object encountered unshelving "^tname in
    let assignments = 
      List.fold_right
        (fun (id,_,_) exp ->
           <:expr< this.Mutable.$lid:id$ <- $lid:id$; $exp$ >>)
        fields
      <:expr< return this_input >> in
    let inner = 
      List.fold_right
        (fun (id,([],t),_) exp ->
           <:expr< $bind$ ($mproject (expr ctxt t) "unshelve"$ $lid:id$)
             (fun $lid:id$ -> $exp$) >>)
        fields
        assignments in
    let idpat = patt_list (List.map (fun (id,_,_) -> <:patt< $lid:id$ >>) fields) in
      unshelve_record_bindings ctxt decl fields
        (<:expr< fun id -> W.record
           (fun this_input subvalues -> 
              let this = (Obj.magic this_input : Mutable.t) in
                match subvalues with
                  | $idpat$ -> $inner$
                  | _ -> raise (UnshelvingError $str:msg$)) num_fields id >>)

  let shelve_record ctxt decl fields expr =
    let inner =
      List.fold_right 
        (fun (id,([],t),_) e ->
           <:expr< $bind$ ($mproject (expr ctxt t) "shelve"$ $lid:id$) 
                          (fun $lid:id$ -> $e$) >>)
        fields
        <:expr< (W.store_repr this
                   (Repr.make
                      $expr_list (List.map (fun (id,_,_) -> <:expr< $lid:id$ >>) fields)$)) >>
    in
      [ <:match_case< ($record_pattern fields$ as obj) ->
                       W.allocate obj (fun this -> $inner$) >> ]


  let typeable_instance ctxt tname =
    <:module_expr< Typeable.Typeable_defaults(
    $apply_functor <:module_expr< $uid:"Typeable_" ^ tname$ >> 
      (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p ctxt.argmap$.T >>)
         ctxt.params)$) >>

  let eq_instance ctxt tname =
    apply_functor <:module_expr< $uid:"Eq_" ^ tname$ >> 
      (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p ctxt.argmap$.E >>)
         ctxt.params)

  let rebind_params ctxt name : Ast.str_item = 
    NameMap.fold
      (fun _ param s -> <:str_item< $s$ module $uid:param$ = $uid:param$.$uid:name$ >>)
      ctxt.argmap
      <:str_item< >>

  let wrap ~ctxt ~atype ~tymod ~eqmod ~shelvers ~unshelver =
    <:module_expr< struct open Eq open Typeable
                          module T = $tymod$
                          module E = $eqmod$
                          type a = $atype$
                          open Write
                          let shelve = let module W = Utils(T)(E) in function $list:shelvers$
                          open Read
                          let unshelve = let module W = Utils(T) in $unshelver$
    end >>

    let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:false

    method tuple ctxt ts = 
      let nts = List.length ts in
      let ids = (List.mapn (fun t n -> (Printf.sprintf "id%d" n, t)) ts) in
      let eidlist = expr_list (List.map (fun (id,_) -> <:expr< $lid:id$ >>) ids) in
      let pidlist = patt_list (List.map (fun (id,_) -> <:patt< $lid:id$ >>) ids) in
      let tpatt,texpr = tuple ~param:"id" nts in
      let tymod = Typeable.tup ctxt ts <:expr< M.T.typeRep >> (self#expr)
      and eqmod = Eq.tup ctxt ts <:expr< M.E.eq >> (self#expr)
      and shelvers =
        let inner = 
          List.fold_right
            (fun (id,t) expr -> 
               <:expr< $bind$ ($mproject (self#expr ctxt t) "shelve"$ $lid:id$) 
                            (fun $lid:id$ -> $expr$) >>)
            ids
            <:expr< W.store_repr this (Repr.make $eidlist$) >> in
          [ <:match_case< ($tpatt$ as obj) -> 
                  W.allocate obj (fun this -> $inner$) >>]

      and unshelver = 
        let msg = "unexpected object encountered unshelving "^string_of_int nts^"-tuple" in
        let inner = 
          List.fold_right 
            (fun (id,t) expr ->
               <:expr< $bind$ ($mproject (self#expr ctxt t) "unshelve"$ $lid:id$) (fun $lid:id$ -> $expr$) >>)
            ids
            <:expr< return $texpr$ >> in
          <:expr< W.tuple
            (function
               | $pidlist$ -> $inner$
               | _ -> raise (UnshelvingError $str:msg$)) >>
      and atype = atype_expr ctxt (`Tuple ts) in
        wrap ~ctxt ~atype ~tymod ~eqmod ~shelvers ~unshelver

    method polycase ctxt tagspec : Ast.match_case = match tagspec with
    | Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
          W.allocate obj
              (fun thisid -> 
                 W.store_repr thisid
                    (Repr.make ~constructor:$`int:(tag_hash name)$ [])) >>
    | Tag (name, Some t) -> <:match_case< 
        (`$name$ v1 as obj) ->
           W.allocate obj
            (fun thisid ->
             $bind$ ($mproject (self#expr ctxt t) "shelve"$ v1)
                    (fun mid -> 
                    (W.store_repr thisid
                        (Repr.make ~constructor:$`int:(tag_hash name)$ [mid])))) >>
    | Extends t -> 
        let patt, guard, cast = cast_pattern ctxt t in <:match_case<
         ($patt$ as obj) when $guard$ ->
            ($mproject (self#expr ctxt t) "shelve"$ $cast$) >>

    method polycase_un ctxt tagspec : Ast.match_case = match tagspec with
    | (name, None)   -> <:match_case< $`int:(tag_hash name)$, [] -> return `$name$ >>
    | (name, Some t) -> <:match_case< $`int:(tag_hash name)$, [x] -> 
      $bind$ ($mproject (self#expr ctxt t) "unshelve"$ x) (fun o -> return (`$name$ o)) >>

    method extension ctxt tname ts : Ast.match_case =
      (* Try each extension in turn.  If we get an UnknownTag failure,
         try the next one.  This is

         * safe because any two extensions that define the same tag
           must be compatible at that point

         * fast because we can tell on the first integer comparison
           whether we've picked the right path or not.
      *)
      let inner = List.fold_right 
        (fun t exp -> <:expr<
           let module M = $(self#expr ctxt t)$ in
             try $exp$
             with UnknownTag (n,_) -> (M.unshelve id : M.a Read.m :> a Read.m)
               >>)
        ts
        <:expr< raise (UnknownTag (n, ($str:"Unexpected tag encountered during unshelving of "
                                       ^tname$))) >>
    in <:match_case< n,_ -> $inner$ >>

    method variant ctxt (tname,_,_,_,_ as decl) (_, tags) = 
      let unshelver = 
        let tags, extensions = either_partition
          (function Tag (name,t) -> Left (name,t) | Extends t -> Right t) tags in
        let tag_cases = List.map (self#polycase_un ctxt) tags in
        let extension_case = self#extension ctxt tname extensions in
          <:expr< fun id ->
            let f = function $list:tag_cases @ [extension_case]$ in W.sum f id >>
      in
        wrap ~ctxt ~atype:(atype ctxt decl) ~tymod:(typeable_instance ctxt tname)
          ~eqmod:(eq_instance ctxt tname)
          ~shelvers:(List.map (self#polycase ctxt) tags) ~unshelver

    method case ctors ctxt (name, params') n : Ast.match_case * Ast.match_case = 
    let nparams = List.length params' in
    let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
    let exp = 
      List.fold_right2
        (fun p n tail -> 
           <:expr< $bind$ ($mproject (self#expr ctxt p) "shelve"$ $lid:Printf.sprintf "v%d" n$)
                          (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
        params'
        (List.range 0 nparams)
        <:expr< W.store_repr thisid (Repr.make ~constructor:$`int:n$ $expr_list ids$) >> in
      match params' with
        | [] -> <:match_case< $uid:name$ as obj -> 
                              W.allocate obj (fun thisid -> $exp$) >>,
                <:match_case< $`int:n$, [] -> return $uid:name$ >>
        | _  -> <:match_case< $uid:name$ $fst (tuple ~param:"v" nparams)$ as obj -> 
                              W.allocate obj (fun thisid -> $exp$) >>,
    let _, tuple = tuple ~param:"id" nparams in
    let patt, exp = 
      List.fold_right2 
        (fun n t (pat, exp) ->
           let m = Printf.sprintf "M%d" n and id = Printf.sprintf "id%d" n in
           <:patt< $lid:id$ :: $pat$ >>,
           <:expr< let module $uid:m$ = $self#expr ctxt t$
                    in $bind$ ($uid:m$.unshelve $lid:id$) (fun $lid:id$ -> $exp$) >>)
        (List.range 0 nparams)
        params'
      (<:patt< [] >>, <:expr< return ($uid:name$ $tuple$) >>) in
      <:match_case< $`int:n$, $patt$ -> $exp$ >>

  method sum ?eq ctxt (tname,_,_,_,_ as decl) summands =
    let nctors = List.length summands in
    let shelvers, unshelvers = List.split (List.mapn (self#case nctors ctxt) summands) in
    wrap ~ctxt ~atype:(atype ctxt decl)
      ~tymod:(typeable_instance ctxt tname)
      ~eqmod:(eq_instance ctxt tname)
      ~shelvers
      ~unshelver:<:expr< fun id -> 
        let f = function $list:unshelvers$ 
                 | n,_ -> raise (UnshelvingError ($str:"Unexpected tag when unshelving "
                                                  ^tname^": "$^ string_of_int n))
        in W.sum f id >>

  method record ?eq ctxt (tname,_,_,_,_ as decl) (fields : Type.field list) = 
      wrap ~ctxt ~atype:(atype ctxt decl) 
        ~shelvers:(shelve_record ctxt decl fields (self#expr))
        ~unshelver:(unshelve_record ctxt decl fields (self#expr))
        ~tymod:(typeable_instance ctxt tname)
        ~eqmod:(eq_instance ctxt tname)
  end
end

let _ = Base.register "Shelve"
  ((fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
          ~default_module:"Shelve_defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
