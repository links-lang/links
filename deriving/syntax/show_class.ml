#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Deriving
include Deriving.Struct_utils(struct let classname="Show" let defaults = "ShowDefaults" end)

let currentp currents = function
| None -> false
| Some (_, s) -> List.mem_assoc s currents


let module_name currents = function
| None -> assert false
| Some (_, s) -> List.assoc s currents



(* Generate a printer for each constructor parameter *)
let gen_printer =
  let current default (t:MLast.ctyp) gen ({loc=loc;currents=currents} as ti) c = 
    let lt = ltype_of_ctyp t in
    if currentp currents lt then 
      <:module_expr< $uid:(module_name currents lt)$ >>
    else default t gen ti c
  in 
  gen_module_expr
    ~tyapp:(current gen_app)
    ~tylid:(current gen_lid)
    ~tyrec:gen_other 
    ~tysum:gen_other
    ~tyvrn:gen_other

let gen_printers ({loc=loc} as ti) tuple params = 
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
let gen_case ti (loc, name, params') =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
  let tuple = tuple_expr loc (List.map (fun (_,p) -> <:expr< $lid:p$ >>) params)
  in (patt, None, <:expr< do { Format.pp_open_hovbox formatter 0;
                               Format.pp_print_string formatter $str:name$;
                               Format.pp_print_break formatter 1 2;
                               $gen_printers ti tuple params'$;
                               Format.pp_close_box formatter () } >>)


(* Generate the format function, the meat of the thing. *)
let gen_format_sum ctors ({loc=loc} as ti) = <:str_item< 
   value rec format formatter = 
             fun [ $list:List.map (gen_case ti) ctors$ ]
>>

let gen_format_record fields ({loc=loc}as ti) = 
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
| MLast.RfInh ctyp -> let var, guard, expr = cast_pattern loc ctyp ~param:"x" in
    (var, guard, 
     <:expr< let module S = $gen_printer ti ctyp$ in 
                S.format formatter $expr$ >>)

let gen_format_polyv (row,_) ({loc=loc} as ti) =
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
let gen_module_expr ti = 
  let wrapper f ti data = apply_defaults ti (f data ti) in
  gen_module_expr ti
    ~tyrec:(fun _ _ -> wrapper gen_format_record)
    ~tysum:(fun _ _ -> wrapper gen_format_sum)
    ~tyvrn:(fun _ _ -> wrapper gen_format_polyv) ti.rtype

let gen_instances loc tdl = gen_finstances ~gen_module_expr:gen_module_expr loc ~tdl:tdl

let _ = 
  begin
    instantiators := ("Show", gen_instances):: !instantiators;
    sig_instantiators := ("Show", Sig_utils.gen_sigs "Show"):: !sig_instantiators;
  end
