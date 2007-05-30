open Camlp4.PreCast

module InContext (C : Base.Context) =
struct
  open C
  open Types
  open Util
  open Base
  include Base.InContext(C)

  let classname = "Functor"

  let param_map : string NameMap.t = 
    List.fold_right
      (fun name map -> NameMap.add name ("f_" ^ name) map)
      (snd context.ltype)
      NameMap.empty

  let tdec, sigdec = 
    let (name, params, rhs, _) = context.tdec in 
    let dec = ("f", params, `Alias (Constr ([name], List.map (fun p -> Param p) params)), []) in
      Untranslate.decl dec, Untranslate.sigdecl dec

  let wrapper expr = 
    let (name, params, rhs, _) = context.tdec in 
    let patts :Ast.ident list = 
      List.map 
        (fun (name,_) -> <:ident< $lid:NameMap.find name param_map$ >>)
        params in
      <:module_expr< struct
        $tdec$ 
        let map $list:patts$ = $expr$
      end >>
(*
   prototype: [[t]] : t -> t[b_i/a_i]


   [[a_i]]   = f_i

   [[C1|...CN]] = function [[C1]] ... [[CN]]               sum
   [[`C1|...`CN]] = function [[`C1]] ... [[`CN]]           variant
 
   [[{t1,...tn}]] = fun (t1,tn) -> ([[t1]],[[tn]])         tuple
   [[{l1:t1; ... ln:tn}]] = 
         fun {l1=t1;...ln=tn} -> {l1=[[t1]];...ln=[[tn]]}  record

   [[(t1,...tn) c]] = c_map [[t1]]...[[tn]]                constructor

   [[a -> b]] = f . [[a]] (where a_i \notin fv(b))         function

   [[C0]]    = C0->C0                                      nullary constructors
   [[C1 (t1...tn)]]  = C1 t -> C0 ([[t1]] t1...[[tn]] tn)  unary constructor
   [[`C0]]   = `C0->`C0                                    nullary tag
   [[`C1 t]] = `C1 t->`C0 [[t]] t                          unary tag
*)
  let rec polycase = function
    | Tag (name, None) -> <:match_case< `$name$ -> `$name$ >>
    | Tag (name, Some e) -> <:match_case< `$name$ x -> `$name$ ($expr e$ x) >>
    | Extends t -> 
        let patt, guard, exp = cast_pattern t in
          <:match_case< $patt$ when $guard$ -> $expr t$ $exp$ >>

  and expr = function
    | Param (p,_) -> <:expr< $lid:NameMap.find p param_map$ >>
    | Function (f,t) when not (contains_tvars t) -> 
        <:expr< fun f x -> f ($expr f$ x) >>
    | Constr (qname, ts) -> 
        List.fold_left 
          (fun fn arg -> <:expr< $fn$ $expr arg$ >>)
          <:expr< $id:modname_from_qname ~qname ~classname$.map >>
          ts
    | Tuple ts -> tup ts
    | Variant (_, tags) -> 
        <:expr< function $list:List.map polycase tags$ >>
    | _ -> raise (Underivable (classname, context.atype))

  and tup ts = 
    let args, exps = 
      List.split
        (List.map2
           (fun t n -> 
              let v = Printf.sprintf "t%d" n in
                <:ident< $lid:v$ >>, <:expr< $expr t$ $lid:v$ >>)
           ts
           (List.range 0 (List.length ts))) in
      <:expr< fun ($list:args$) -> ($tuple_expr exps$) >>

  and case = function
    | (name, []) -> <:match_case< $uid:name$ -> $uid:name$ >>
    | (name, args) -> 
        let f = tup args 
        and tpatt, texp = tuple (List.length args) in
          <:match_case< $uid:name$ $tpatt$ -> $uid:name$ ($f$ $texp$) >>

  and field (name, (_,t), _) : Ast.expr =
    <:expr< $expr t$ $lid:name$ >>

  let rhs = function
    |`Fresh (_, Sum summands)  -> 
       <:expr<  function $list:List.map case summands$ >>
    |`Fresh (_, Record fields) -> 
       <:expr< fun $record_pattern fields$ -> 
                   $record_expr (List.map (fun ((l,_,_) as f) -> (l,field f)) fields)$ >>
    |`Alias e                  -> expr e

  let maptype = 
    let (name, params, r, constraints) = context.tdec in
    let ctor_in = Constr ([name], List.map (fun p -> Param p) params) in
    let ctor_out = substitute param_map ctor_in  (* c[f_i/a_i] *) in
      List.fold_right (* (a_i -> f_i) -> ... -> c[a_i] -> c[f_i/a_i] *)
        (fun (p,_) out -> 
           (<:ctyp< ('$lid:p$ -> '$lid:NameMap.find p param_map$) -> $out$>>))
        params
        (Untranslate.expr (Function (ctor_in, ctor_out)))

   let signature : Ast.sig_item list =  
     [ <:sig_item< $sigdec$ >>; 
       <:sig_item< val map : $maptype$ >> ] 

  let decl : Camlp4.PreCast.Ast.module_binding =
    let (name, params, r, constraints) = context.tdec in
      <:module_binding<
         $uid:classname ^ "_" ^ name$
       : sig $list:signature$ end
       = $wrapper (rhs r)$ >>

end

let generate ({Base.loc=loc} as context) csts = 
  let decl (context,_) =
    let module M = InContext(struct let context = context end) in
      M.decl in
  <:str_item< module rec $list:List.map decl csts$ >>
