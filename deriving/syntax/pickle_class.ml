(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)
  module UT = Type.Untranslate(L)

  let tuple_functors = [2;3;4;5;6]

  let typeable_defaults t = <:module_expr< Typeable.Defaults($t$) >>

  module Typeable = Typeable_class.InContext(L)
  module Eq       = Eq_class.InContext(L)

  let classname = "Pickle"
  let bind, seq = 
    let bindop = ">>=" and seqop = ">>" in
      <:expr< $lid:bindop$ >>, <:expr< $lid:seqop$ >>

  let unpickle_record_bindings ctxt (tname,params,rhs,cs,_) (fields : field list) e = <:expr<
      let module Mutable = struct
        type t = $UT.repr 
            (instantiate_modargs_repr ctxt 
               (Record (List.map (fun (n,p,_) -> (n,p,`Mutable)) fields)))$
      end in $e$ >>

  let unpickle_record ctxt (tname,_,_,_,_ as decl) fields expr = 
    let msg = "unexpected object encountered unpickling "^tname in
    let assignments = 
      List.fold_right
        (fun (id,_,_) exp ->
           <:expr< this.Mutable.$lid:id$ <- $lid:id$; $exp$ >>)
        fields
      <:expr< return self >> in
    let inner = 
      List.fold_right
        (fun (id,([],t),_) exp ->
           <:expr< $bind$ ($mproject (expr ctxt t) "unpickle"$ $lid:id$)
             (fun $lid:id$ -> $exp$) >>)
        fields
        assignments in
    let idpat = patt_list (List.map (fun (id,_,_) -> <:patt< $lid:id$ >>) fields) in
      unpickle_record_bindings ctxt decl fields
        (<:expr< W.record
           (fun self -> function
                  | $idpat$ -> let this = (Obj.magic self : Mutable.t) in $inner$
                  | _ -> raise (UnpicklingError $str:msg$)) $`int:List.length fields$ >>)

  let pickle_record ctxt decl fields expr =
    let inner =
      List.fold_right 
        (fun (id,([],t),_) e ->
           <:expr< $bind$ ($mproject (expr ctxt t) "pickle"$ $lid:id$) 
                          (fun $lid:id$ -> $e$) >>)
        fields
        <:expr< (W.store_repr this
                   (Repr.make
                      $expr_list (List.map (fun (id,_,_) -> <:expr< $lid:id$ >>) fields)$)) >>
    in
      [ <:match_case< ($record_pattern fields$ as obj) ->
                       W.allocate obj (fun this -> $inner$) >> ]


  let typeable_instance ctxt tname =
    <:module_expr< Typeable.Defaults(
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

  let wrap ~ctxt ~atype ~tymod ~eqmod ~picklers ~unpickler =
    <:module_expr< struct open Eq open Typeable
                          module T = $tymod$
                          module E = $eqmod$
                          type a = $atype$
                          open Write
                          let pickle = let module W = Utils(T)(E) in function $list:picklers$
                          open Read
                          let unpickle = let module W = Utils(T) in $unpickler$
    end >>

    let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:false

    method tuple ctxt ts = 
      let nts = List.length ts in
        if List.mem nts tuple_functors then
          apply_functor <:module_expr< $uid:Printf.sprintf "Pickle_%d" nts$ >> 
            (List.map (self#expr ctxt) ts)
        else
          let ids = (List.mapn (fun t n -> (Printf.sprintf "id%d" n, t)) ts) in
          let eidlist = expr_list (List.map (fun (id,_) -> <:expr< $lid:id$ >>) ids) in
          let pidlist = patt_list (List.map (fun (id,_) -> <:patt< $lid:id$ >>) ids) in
          let tpatt,texpr = tuple ~param:"id" nts in
          let tymod = Typeable.tup ctxt ts <:expr< M.T.type_rep >> (self#expr)
          and eqmod = Eq.tup ctxt ts <:expr< M.E.eq >> (self#expr)
          and picklers =
            let inner = 
              List.fold_right
                (fun (id,t) expr -> 
                   <:expr< $bind$ ($mproject (self#expr ctxt t) "pickle"$ $lid:id$) 
                              (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< W.store_repr this (Repr.make $eidlist$) >> in
              [ <:match_case< ($tpatt$ as obj) -> 
                               W.allocate obj (fun this -> $inner$) >>]
                
          and unpickler = 
            let msg = "unexpected object encountered unpickling "^string_of_int nts^"-tuple" in
            let inner = 
              List.fold_right 
                (fun (id,t) expr ->
                   <:expr< $bind$ ($mproject (self#expr ctxt t) "unpickle"$ $lid:id$) (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< return $texpr$ >> in
              <:expr< W.tuple
                      (function 
                         | $pidlist$ -> $inner$
                         | _ -> raise (UnpicklingError $str:msg$)) >>
          and atype = atype_expr ctxt (`Tuple ts) in
            <:module_expr< Pickle.Defaults($wrap ~ctxt ~atype ~tymod ~eqmod ~picklers ~unpickler$) >>
              
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
             $bind$ ($mproject (self#expr ctxt t) "pickle"$ v1)
                    (fun mid -> 
                    (W.store_repr thisid
                        (Repr.make ~constructor:$`int:(tag_hash name)$ [mid])))) >>
    | Extends t -> 
        let patt, guard, cast = cast_pattern ctxt t in <:match_case<
         ($patt$ as obj) when $guard$ ->
            ($mproject (self#expr ctxt t) "pickle"$ $cast$) >>

    method polycase_un ctxt tagspec : Ast.match_case = match tagspec with
    | (name, None)   -> <:match_case< $`int:(tag_hash name)$, [] -> return `$name$ >>
    | (name, Some t) -> <:match_case< $`int:(tag_hash name)$, [x] -> 
      $bind$ ($mproject (self#expr ctxt t) "unpickle"$ x) (fun o -> return (`$name$ o)) >>

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
             with UnknownTag (n,_) -> (M.unpickle id :> a Read.m) >>)
        ts
        <:expr< raise (UnknownTag (n, ($str:"Unexpected tag encountered during unpickling of "
                                       ^tname$))) >>
    in <:match_case< n,_ -> $inner$ >>

    method variant ctxt (tname,_,_,_,_ as decl) (_, tags) = 
      let unpickler = 
        let tags, extensions = either_partition
          (function Tag (name,t) -> Left (name,t) | Extends t -> Right t) tags in
        let tag_cases = List.map (self#polycase_un ctxt) tags in
        let extension_case = self#extension ctxt tname extensions in
          <:expr< fun id -> W.sum (function $list:tag_cases @ [extension_case]$) id >>
      in
        wrap
          ~ctxt
          ~atype:(atype ctxt decl)
          ~tymod:(typeable_instance ctxt tname)
          ~eqmod:(eq_instance ctxt tname)
          ~picklers:(List.map (self#polycase ctxt) tags)
          ~unpickler

    method case ctors ctxt (name, params') n : Ast.match_case * Ast.match_case = 
    let nparams = List.length params' in
    let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
    let exp = 
      List.fold_right2
        (fun p n tail -> 
           <:expr< $bind$ ($mproject (self#expr ctxt p) "pickle"$ $lid:Printf.sprintf "v%d" n$)
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
                    in $bind$ ($uid:m$.unpickle $lid:id$) (fun $lid:id$ -> $exp$) >>)
        (List.range 0 nparams)
        params'
      (<:patt< [] >>, <:expr< return ($uid:name$ $tuple$) >>) in
      <:match_case< $`int:n$, $patt$ -> $exp$ >>

  method sum ?eq ctxt (tname,_,_,_,_ as decl) summands =
    let nctors = List.length summands in
    let picklers, unpicklers = List.split (List.mapn (self#case nctors ctxt) summands) in
    wrap
      ~ctxt
      ~atype:(atype ctxt decl)
      ~tymod:(typeable_instance ctxt tname)
      ~eqmod:(eq_instance ctxt tname)
      ~picklers
      ~unpickler:<:expr< fun id -> 
                         let f = function
                                  $list:unpicklers$ 
                                 | n,_ -> raise (UnpicklingError ($str:"Unexpected tag when unpickling "
                                                                  ^tname^": "$^ string_of_int n))
                         in W.sum f id >>

  method record ?eq ctxt (tname,_,_,_,_ as decl) (fields : Type.field list) = 
      wrap
        ~ctxt
        ~atype:(atype ctxt decl) 
        ~picklers:(pickle_record ctxt decl fields (self#expr))
        ~unpickler:(unpickle_record ctxt decl fields (self#expr))
        ~tymod:(typeable_instance ctxt tname)
        ~eqmod:(eq_instance ctxt tname)
  end
end

let _ = Base.register "Pickle"
  ((fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
          ~default_module:"Defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
