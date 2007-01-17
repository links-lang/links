#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Deriving
include Deriving.Struct_utils(struct let classname="Show" end)

let currentp currents = function
| None -> false
| Some (_, s) -> List.mem_assoc s currents


let module_name currents = function
| None -> assert false
| Some (_, s) -> List.assoc s currents



(* Generate a printer for each constructor parameter *)
let rec gen_printer ({tname=self;loc=loc;currents=currents}as ti) = function
| c when currentp currents (ltype_of_ctyp c) -> <:module_expr< $uid:(module_name currents (ltype_of_ctyp c))$ >>
| <:ctyp< $lid:id$ >>                    -> <:module_expr< $uid:"Show_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>                   -> <:module_expr< $gen_printer ti t1$ $gen_printer ti t2$ >>
| <:ctyp< $uid:t1$ . $t2$ >>             -> <:module_expr< $uid:t1$ . $gen_printer ti t2$ >>
| <:ctyp< ( $list:params$ ) >>           -> (List.fold_left 
                                               (fun s param -> <:module_expr< $s$ $param$ >>)
                                               <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
                                               (List.map (gen_printer ti) params))
| <:ctyp< $_$ -> $_$ >> as a             -> <:module_expr< Show_unprintable(struct type a = $a$; end) >>
| <:ctyp< '$a$ >>                        -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| s                                      -> <:module_expr< (Show_unprintable
                                                              (struct type a = V0.a; end)
                                                                 : Show with type a = V0.a) >>


let gen_printers ({tname=self;loc=loc} as ti) tuple params = 
 let m = (List.fold_left
            (fun s param -> <:module_expr< $s$ $param$ >>)
            <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
            (List.map (gen_printer ti) params)) in
   <:expr< let module S = $m$ in S.format formatter $tuple$ >>

(* Generate an individual case clause for the format function. 
case:
     | $(Ctor-name) $(params) -> 
         Format.pp_print_string formatter $(Ctor-name + space)
         let module S = Show_$(|params|) $(modularized_param_types) in
             S.format formatter $(params)
*)
let gen_case ({tname=self} as ti) (loc, name, params') =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
  match params' with
  | [] -> 
      (patt, None, <:expr< Format.pp_print_string formatter $str:name$ >>)
  | _ -> 
      let tuple = 
        match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with (* 0-tuples and 1-tuples are invalid *)
        | []     -> <:expr< () >>
        | [x]    -> x
        | params -> <:expr< ( $list:params$ )>> 
      in (patt, None, <:expr< do { Format.pp_open_hovbox formatter 0;
                                   Format.pp_print_string formatter $str:name$;
                                   Format.pp_print_break formatter 1 2;
                                   $gen_printers ti tuple params'$;
                                   Format.pp_close_box formatter () } >>)


(* Generate the format function, the meat of the thing. *)
let gen_format_sum ({tname=self;loc=loc} as ti) ctors = <:str_item< 
   value rec format formatter = 
             fun [ $list:List.map (gen_case ti) ctors$ ]
>>

let gen_format_record ({loc=loc;tname=self}as ti) fields = 
  let showfields = List.map2 (fun (loc,k,_(* what is this? *),v) endp ->
                                let sep = if endp then <:expr< () >>
                                else <:expr< Format.pp_print_string formatter "; " >> in
                                <:expr< let module S = $gen_printer ti v$ in
                                            do { Format.pp_print_string formatter $str:k ^ "="$; 
                                                 S.format formatter obj.$lid:k$ ; 
                                                 $sep$
                                               } >>) 
    fields (endmarker fields)
    in
<:str_item<
   value format formatter obj = do {
      Format.pp_print_char formatter '{';
      do { $list:showfields$ };
      Format.pp_print_char formatter '}';
   }
>>

let gen_polycase ({loc=loc; tname=tname} as ti) = function
| MLast.RfTag (name, _ (* what is this? *), params') ->
    let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                    params' (range 0 (List.length params' - 1))) in
    let patt = (List.fold_left 
                  (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< `$name$>> params) in
      (match params' with
        | [] -> (patt, None, <:expr< Format.pp_print_string formatter $str:"`" ^ name$ >>)
        | _ -> (let tuple = (match List.map (fun (_,p) -> <:expr< $lid:p$ >>) params with (* 0-tuples and 1-tuples are invalid *)
                               | []     -> <:expr< () >>
                               | [x]    -> x
                               | params -> <:expr< ( $list:params$ )>>)
          in (patt, None, <:expr< do { Format.pp_open_hovbox formatter 0;
                                       Format.pp_print_string formatter $str:"`" ^ name ^" "$; 
				       $gen_printers ti tuple params'$;
                                       Format.pp_close_box formatter () }
		              >>)))
| MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp) -> 
    (<:patt< (# $[tname]$ as $lid:tname$) >>, None, 
    <:expr< let module S = $gen_printer ti ctyp$ in S.format formatter $lid:tname$ >>)
| MLast.RfInh _ -> error loc ("Cannot generate show instance for " ^ tname)



let gen_format_polyv ({loc=loc;tname=tname} as ti) (row : MLast.row_field list) =
<:str_item< 
   value rec format formatter = 
             fun [ $list:List.map (gen_polycase ti) row$ ]
>>

(* Generate a `this' module given the type *)
let gen_this_module loc atype = <:module_expr< 
   ShowDefaults (struct
                   type a = $atype$; 
                   value showBuf = (showBuf : Format.formatter -> $atype$ -> unit); 
                 end) >>

(* TODO: merge with gen_printer *)
let gen_module_expr ({loc=loc; tname=tname; atype=atype; rtype=rtype} as ti) = 
 let rec gen = function
| <:ctyp< [ $list:ctors$ ] >>  -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_format_sum ti ctors$;
                                                                end) : Show with type a = $atype$) >>
| <:ctyp< $lid:id$ >>          -> <:module_expr< $uid:"Show_"^ id$ >>
| <:ctyp< $t1$ $t2$ >>         -> <:module_expr< $gen t1$ $gen t2$ >>
| <:ctyp< $uid:m$ . $t2$ >>    -> <:module_expr< $uid:m$ . $gen t2$ >>
| <:ctyp< { $list:fields$ } >> -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_format_record ti fields$;
                                                                end) : Show with type a = $atype$) >>
| <:ctyp< ( $list:params$ ) >> -> (List.fold_left 
                                     (fun s param -> <:module_expr< $s$ $param$ >>)
                                     <:module_expr< $uid:Printf.sprintf "Show_%d" (List.length params)$ >>
                                     (List.map gen params))
| <:ctyp< '$a$ >>              -> <:module_expr< $uid:snd (List.assoc a ti.argmap)$ >>
| <:ctyp< [= $list:row$ ] >>  -> <:module_expr< (ShowDefaults (struct
                                                                  type a = $atype$;
                                                                  $gen_format_polyv ti row$;
                                                                end) : Show with type a = $atype$) >>
| _                            -> error loc ("Cannot currently generate show instances for "^ tname)
 in gen rtype


let gen_instances loc tdl = gen_finstances ~gen_module_expr:gen_module_expr loc ~tdl:tdl

let _ = 
  begin
    instantiators := ("Show", gen_instances):: !instantiators;
    sig_instantiators := ("Show", Sig_utils.gen_sigs "Show"):: !sig_instantiators;
  end
