(**
   Generate Pickle instances
**)
(** / Pickle *)

(* FIXME:
   optimization: shouldn't pickle a tag for single-constructor types *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
open Deriving;

(* Generate names for type parameters (type variables) *)
value param_names (params : list (string * (bool*bool))) : list (string * (string * string)) =
    (List.map2
       (fun (p,_) n -> (p, (Printf.sprintf "v%d" n, Printf.sprintf "V%d" n)))
       params
       (range 0 (List.length params - 1)));


(* Generate a printer for each constructor parameter *)
value rec gen_printer ({tname=self;loc=loc}as ti) = fun [
  c when ltype_of_ctyp c = Some ti.ltype -> <:module_expr< This >>
| <:ctyp< $lid:id$ >>                    -> <:module_expr< $uid:"Pickle_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>                   -> <:module_expr< $gen_printer ti t1$ $gen_printer ti t2$ >>
| <:ctyp< ( $list:params$ ) >>           -> (List.fold_left 
                                               (fun s param -> <:module_expr< $s$ $param$ >>)
                                               <:module_expr< $uid:Printf.sprintf "Pickle_%d" (List.length params)$ >>
                                               (List.map (gen_printer ti) params))
| <:ctyp< '$a$ >>                        -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| s                                      -> failwith ("Cannot generate pickle instance for " ^ self)
];

value gen_printers ({tname=self;loc=loc} as ti) tuple params = 
 let m = (List.fold_left
            (fun s param -> <:module_expr< $s$ $param$ >>)
            <:module_expr< $uid:Printf.sprintf "Pickle_%d" (List.length params)$ >>
            (List.map (gen_printer ti) params)) in
   <:expr< let module S = $m$ in S.pickle buffer $tuple$ >>;

(* Generate an individual case clause for the pickle function. 
case:
     | $(Ctor-name) $(params) -> 
         Buffer.add_string buffer $(Ctor-name + space)
         let module S = Pickle_$(|params|) $(modularized_param_types) in
             S.pickle $(params) buffer
*)
value gen_pickle_case ({tname=self} as ti) (loc, name, params') (pos : int) =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
  match params' with [
    [] -> 
      (patt, None, <:expr< Pickle_int.pickle buffer $int:string_of_int pos$ >>)
  | _ -> 
      let tuple = 
        match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with [ (* 0-tuples and 1-tuples are invalid *)
          []     -> <:expr< () >>
        | [x]    -> x
        | params -> <:expr< ( $list:params$ )>> 
        ]
      in (patt, None, <:expr< do { Pickle_int.pickle buffer $int:string_of_int pos$; $gen_printers ti tuple params'$ } >>)
];
value gen_unpickle_case ({tname=self} as ti) (loc, name, params') (pos : int) =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let expr = 
    List.fold_left
      (fun acc (_,v) -> <:expr< $acc$ $lid:v$ >>) 
      <:expr< $uid:name$ >>
      params
  in
  let patt = <:patt< $int:string_of_int pos$ >> in
    (patt, None, 
     List.fold_right 
       (fun (ptype, var) expr ->
          <:expr<  let module S = $gen_printer ti ptype$ in
          let $lid:var$ = S.unpickle stream in $expr$ >>)
       params
       expr)
;



(* Generate the pickle and unpickle functions. *)
value gen_sum ({tname=self;loc=loc} as ti) thismod ctors = <:str_item< 
   value rec pickle buffer = 
         let module This = $thismod$ in 
             fun [ $list:List.map2 (gen_pickle_case ti) ctors (range 0 (List.length ctors - 1))$ ]
   and unpickle stream =
         let module This = $thismod$ in
           match Pickle_int.unpickle stream with [ $list:List.map2 (gen_unpickle_case ti) ctors (range 0 (List.length ctors - 1))$ ]
>>;

(* Generate a `this' module given the type *)
value gen_this_module loc atype = <:module_expr< 
   Pickle_defaults (struct
                   type a = $atype$; 
                   value pickle = (pickle : Buffer.t -> $atype$ -> unit)
                   and unpickle = (unpickle : Stream.t char -> $atype$);
                 end) >>;


value gen_funs_record ({loc=loc}as ti) thismod fields = 
   let projections = List.map
     (fun (loc,k,_,v) ->
	<:expr< let module S = $gen_printer ti v$ in
                S.pickle buffer (obj . $lid:k$) >>
     ) fields in
   let subexprs = List.map (fun (loc, k,_,_) -> (<:patt< $lid:k$ >>, <:expr< $lid:k$ >> )) fields in
   let constructions = List.fold_right
     (fun (loc, k, _, v) expr ->
       <:expr< let module S = $gen_printer ti v$ in 
               let $lid:k$ = S.unpickle stream in
                 $expr$ >>
     ) fields <:expr< { $list:subexprs$  } >>
in
<:str_item<
   value rec pickle buffer obj = 
        do { $list:projections$ }
   and unpickle stream =
      let module This = $thismod$ in
          $constructions$
>>
;

value gen_polycase ({loc=loc} as ti) = fun [
  (MLast.RfTag name _ params', n) -> 
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< `$uid:name$>> params) in (* the "uid" isn't really safe here *)
      (let tuple = match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with [ (* 0-tuples and 1-tuples are invalid *)
         []     -> <:expr< () >>
       | [x]    -> x
       | params -> <:expr< ( $list:params$ )>> 
       ] 
       in (patt, None, <:expr< do { Pickle_int.pickle buffer $int:string_of_int n$ ; 
				    $gen_printers ti tuple params'$ }
	     >>))
      
| (MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp), n) -> 
    (<:patt< (# $[tname]$ as $lid:tname$) >>, 
     None, 
     <:expr< let module S = $gen_printer ti ctyp$ in do {
       Pickle_int.pickle buffer $int:string_of_int n$ ;
       S.pickle buffer $lid:tname$
     } >>)
| (MLast.RfInh _, _) ->
    failwith ("Cannot generate pickle instance for " ^ ti.tname)
];



value gen_unpickle_polycase ({loc=loc; tname=self} as ti) = fun [
  (MLast.RfTag name _ params', n) -> 
    let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                    params' (range 0 (List.length params' - 1))) in
    let expr = 
      List.fold_left
        (fun acc (_,v) -> <:expr< $acc$ $lid:v$ >>) 
      <:expr< `$uid:name$ >>
        params
    in
      (<:patt< $int:string_of_int n$ >>, None, 
       List.fold_right 
         (fun (ptype, var) expr ->
            <:expr<  let module S = $gen_printer ti ptype$ in
            let $lid:var$ = S.unpickle stream in $expr$ >>)
         params
         expr)
| (MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp), n) -> 
    (<:patt< $int:string_of_int n$ >>, None,
            <:expr<  let module S = $gen_printer ti ctyp$ in
                       (S.unpickle stream :> a) >>)
| (MLast.RfInh _, _) ->
    failwith ("Cannot generate pickle instance for " ^ ti.tname)
];

value gen_funs_poly ({loc=loc} as ti) thismod row =
<:str_item<
  value rec pickle buffer =
     let module This = $thismod$ in
         fun [ $list:List.map2 (curry (gen_polycase ti)) row (range 0 (List.length row - 1))$]
     and unpickle stream =
         let module This = $thismod$ in
           match Pickle_int.unpickle stream with
             [ $list:List.map2 (curry (gen_unpickle_polycase ti)) row  (range 0 (List.length row - 1))$ ] 
>>;


(* TODO: merge with gen_printer *)
value gen_module_expr ({loc=loc; tname=tname; atype=atype; rtype=rtype} as ti) = 
 let rec gen = fun [
  <:ctyp< [ $list:ctors$ ] >>  -> <:module_expr< (Pickle_defaults (struct
                                                                  type a = $atype$;
                                                                  $gen_sum ti (gen_this_module loc atype) ctors$;
                                                                end) : Pickle with type a = $atype$) >>
| <:ctyp< $lid:id$ >>          -> <:module_expr< $uid:"Pickle_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>         -> <:module_expr< $gen t1$ $gen t2$ >>
| <:ctyp< $uid:m$ . $t2$ >>    -> <:module_expr< $uid:m$ . $gen t2$ >>
| <:ctyp< ( $list:params$ ) >> -> (List.fold_left 
                                     (fun s param -> <:module_expr< $s$ $param$ >>)
                                     <:module_expr< $uid:Printf.sprintf "Pickle_%d" (List.length params)$ >>
                                     (List.map gen params))
| <:ctyp< { $list:fields$ } >> -> <:module_expr< (Pickle_defaults (struct
                                                                     type a = $atype$;
                                                                     $gen_funs_record ti (gen_this_module loc atype) fields$;
                                                                   end) : Pickle with type a = $atype$) >>
| <:ctyp< [= $list:row$ ] >>  -> <:module_expr< (Pickle_defaults (struct
                                                                  type a = $atype$;
                                                                  $gen_funs_poly ti (gen_this_module loc atype) row$;
                                                                   end) : Pickle with type a = $atype$) >>
| <:ctyp< '$a$ >>              -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| _                            -> error loc ("Cannot currently generate pickle instances for "^ tname)
] in gen rtype
;

(* Generate a single instance, possibly a functor *)
value gen_instance ((loc, tname), params, ctyp, constraints) =
  let params = param_names params in
  let atype = gen_type_a loc <:ctyp< $lid:tname$ >> params in
  let ltype = gen_type_l tname params in
  let struct_expr = gen_module_expr {loc=loc; tname=tname; ltype=ltype; atype=atype; rtype=ctyp; argmap=params} in
    <:str_item< declare
        open Pickle; 
        open Primitives; 
        module $uid:"Pickle_"^ tname$ = $gen_functor loc "Pickle" params struct_expr$; 
     end >> ;

(* Generate n mutually-recursive instances (which are /not/ functors) *)
value gen_instances loc tdl = 
  let modules = 
    let exprs = (List.map
                   (fun ((loc,tname),_,ctype,_) -> 
                      ("Pickle_" ^  tname, 
                       <:module_type< Pickle with type a = $lid:tname$ >>, 
                       gen_module_expr {loc=loc; argmap=[]; tname=tname; atype= <:ctyp< $lid:tname$ >>; ltype= ([],tname); rtype=ctype})) tdl) in
      <:str_item< module rec $list:exprs$ >>
    in
 <:str_item< declare
   open Pickle;
   $modules$;
 end >>;

value gen_instances loc : instantiator = fun [
  [td] -> gen_instance td
| tdl when List.exists is_polymorphic tdl -> error loc "Cannot (currently!) generate Pickle instances for polymoprhic mutually recursive types"
| tdl  -> gen_instances loc tdl
];

instantiators.val :=   [("Pickle"    , gen_instances):: instantiators.val];
