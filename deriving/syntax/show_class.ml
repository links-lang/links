#load "pa_extend.cmo";
#load "q_MLast.cmo";
open Deriving;

(**
   Generate Show instances
**)
(** / Show *)


(* Generate names for type parameters (type variables) *)
value param_names (params : list (string * (bool*bool))) : list (string * (string * string)) =
    (List.map2
       (fun (p,_) n -> (p, (Printf.sprintf "v%d" n, Printf.sprintf "V%d" n)))
       params
       (range 0 (List.length params - 1)));


(* Generate a printer for each constructor parameter *)
value rec gen_printer ({tname=self;loc=loc}as ti) = fun [
  c when ltype_of_ctyp c = Some ti.ltype -> <:module_expr< This >>
| <:ctyp< $lid:id$ >>                    -> <:module_expr< $uid:"Show_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>                   -> <:module_expr< $gen_printer ti t1$ $gen_printer ti t2$ >>
| <:ctyp< ( $list:params$ ) >>           -> (List.fold_left 
                                               (fun s param -> <:module_expr< $s$ $param$ >>)
                                               <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
                                               (List.map (gen_printer ti) params))
| <:ctyp< $_$ -> $_$ >> as a             -> <:module_expr< Show_unprintable(struct type a = $a$; end) >>
| <:ctyp< '$a$ >>                        -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| s                                      -> <:module_expr< (Show_unprintable
                                                              (struct type a = V0.a; end)
                                                                 : Show with type a = V0.a) >>
];

value gen_printers ({tname=self;loc=loc} as ti) tuple params = 
 let m = (List.fold_left
            (fun s param -> <:module_expr< $s$ $param$ >>)
            <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
            (List.map (gen_printer ti) params)) in
   <:expr< let module S = $m$ in S.showBuf $tuple$ buffer >>;

(* Generate an individual case clause for the showBuf function. 
case:
     | $(Ctor-name) $(params) -> 
         Buffer.add_string buffer $(Ctor-name + space)
         let module S = Show_$(|params|) $(modularized_param_types) in
             S.showBuf $(params) buffer
*)
value gen_case ({tname=self} as ti) (loc, name, params') =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
  match params' with [
    [] -> 
      (patt, None, <:expr< Buffer.add_string buffer $str:name$ >>)
  | _ -> 
      let tuple = 
        match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with [ (* 0-tuples and 1-tuples are invalid *)
          []     -> <:expr< () >>
        | [x]    -> x
        | params -> <:expr< ( $list:params$ )>> 
        ]
      in (patt, None, <:expr< do { Buffer.add_string buffer $str:name ^" "$; $gen_printers ti tuple params'$ } >>)
];

(* Generate the showBuf function, the meat of the thing. *)
value gen_showBuf_sum ({tname=self;loc=loc} as ti) thismod ctors = <:str_item< 
   value rec showBuf obj buffer = 
         let module This = $thismod$ in 
             match obj with [ $list:List.map (gen_case ti) ctors$ ]
>>;

value gen_showBuf_record ({loc=loc;tname=self}as ti) fields = 
  let showfields = List.map2 (fun (loc,k,_(* what is this? *),v) endp ->
                                let sep = if endp then <:expr< () >>
                                else <:expr< Buffer.add_string buffer "; " >> in
                                <:expr< let module S = $gen_printer ti v$ in
                                            do { Buffer.add_string buffer $str:k ^ "="$; 
                                                 S.showBuf obj.$lid:k$ buffer ; 
                                                 $sep$
                                               } >>) 
    fields (endmarker fields)
    in
<:str_item<
   value showBuf obj buffer = do {
      Buffer.add_char buffer '{';
      do { $list:showfields$ };
      Buffer.add_char buffer '}';
   }
>>;

value gen_polycase ({loc=loc; tname=tname} as ti) = fun [
  MLast.RfTag name _ (* what is this? *) params' ->
    let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                    params' (range 0 (List.length params' - 1))) in
    let patt = (List.fold_left 
                  (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< `$name$>> params) in
      match params' with [
        [] -> (patt, None, <:expr< Buffer.add_string buffer $str:"`" ^ name$ >>)
      | _ -> (let tuple = match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with [ (* 0-tuples and 1-tuples are invalid *)
                []     -> <:expr< () >>
              | [x]    -> x
              | params -> <:expr< ( $list:params$ )>> 
              ] 
              in (patt, None, <:expr< do { Buffer.add_string buffer $str:"`" ^ name ^" "$; 
					   $gen_printers ti tuple params'$ }
		              >>))
      ]
| MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp) -> 
                                                (<:patt< (# $[tname]$ as $lid:tname$) >>, None, 
   <:expr< let module S = $gen_printer ti ctyp$ in S.showBuf $lid:tname$ buffer >>)

| MLast.RfInh _ -> error loc ("Cannot generate show instance for " ^ tname)
]
;

value gen_showBuf_polyv ({loc=loc;tname=tname} as ti) thismod (row : list MLast.row_field) =
<:str_item< 
   value rec showBuf obj buffer = 
         let module This = $thismod$ in 
             match obj with [ $list:List.map (gen_polycase ti) row$ ]
>>;

(* Generate a `this' module given the type *)
value gen_this_module loc atype = <:module_expr< 
   ShowDefaults (struct
                   type a = $atype$; 
                   value showBuf = (showBuf : $atype$ -> Buffer.t -> unit); 
                 end) >>;

(* TODO: merge with gen_printer *)
value gen_module_expr ({loc=loc; tname=tname; atype=atype; rtype=rtype} as ti) = 
 let rec gen = fun [
  <:ctyp< [ $list:ctors$ ] >>  -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_showBuf_sum ti (gen_this_module loc atype) ctors$;
                                                                end) : Show with type a = $atype$) >>
| <:ctyp< $lid:id$ >>          -> <:module_expr< $uid:"Show_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>         -> <:module_expr< $gen t1$ $gen t2$ >>
| <:ctyp< $uid:m$ . $t2$ >>    -> <:module_expr< $uid:m$ . $gen t2$ >>
| <:ctyp< { $list:fields$ } >> -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_showBuf_record ti fields$;
                                                                end) : Show with type a = $atype$) >>
| <:ctyp< ( $list:params$ ) >> -> (List.fold_left 
                                     (fun s param -> <:module_expr< $s$ $param$ >>)
                                     <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
                                     (List.map gen params))
| <:ctyp< '$a$ >>              -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| <:ctyp< [| $list:row$ |] >>  -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_showBuf_polyv ti (gen_this_module loc atype) row$;
                                                                end) : Show with type a = $atype$) >>
| _                            -> error loc ("Cannot currently generate show instances for "^ tname)
] in gen rtype
;

(* Generate a single instance, possibly a functor *)
value gen_instance ((loc, tname), params, ctyp, constraints) =
  let params = param_names params in
  let atype = gen_type_a loc <:ctyp< $lid:tname$ >> params in
  let ltype = gen_type_l tname params in
  let struct_expr = gen_module_expr {loc=loc; tname=tname; ltype=ltype; atype=atype; rtype=ctyp; argmap=params} in
    <:str_item< declare
        open Show; 
        open Primitives; 
        module $uid:"Show_"^ tname$ = $gen_functor loc "Show" params struct_expr$; 
     end >> ;

(* Generate n mutually-recursive instances (which are /not/ functors) *)
value gen_instances loc tdl = 
  let modules = 
    let exprs = (List.map
                   (fun ((loc,tname),_,ctype,_) -> 
                      ("Show_" ^  tname, 
                       <:module_type< Show with type a = $lid:tname$ >>, 
                       gen_module_expr {loc=loc; argmap=[]; tname=tname; atype= <:ctyp< $lid:tname$ >>; ltype= ([],tname); rtype=ctype})) tdl) in
      <:str_item< module rec $list:exprs$ >>
    in
 <:str_item< declare
   open Show;
   open Primitives;
   $modules$;
 end >>;

value gen_instances loc : instantiator = fun [
  [td] -> gen_instance td
| tdl when List.exists is_polymorphic tdl -> error loc "Cannot (currently!) generate Show instances for polymoprhic mutually recursive types"
| tdl  -> gen_instances loc tdl
];

instantiators.val :=   [("Show"    , gen_instances):: instantiators.val];
