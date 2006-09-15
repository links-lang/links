(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Auto_pat
open Ident

let queue = ref []
let printed = Hashtbl.create 1024

let rec_state ppf d =
  Format.fprintf ppf "disp_%i" d.uid;
  queue := d :: !queue

let rec print_source lhs ppf = function
  | Catch  -> Format.fprintf ppf "v"
  | Const c -> Types.Print.print_const ppf c
  | Nil -> Format.fprintf ppf "`nil"
  | Left -> Format.fprintf ppf "v1"
  | Right -> Format.fprintf ppf "v2"
  | Stack i -> Format.fprintf ppf "%s" (List.nth lhs (i-1))
  | Recompose (i,j) -> 
      Format.fprintf ppf "(%s,%s)" 
	(match i with (-1) -> "v1" | (-2) -> "nil" 
	   | i -> List.nth lhs (i-1))
	(match j with (-1) -> "v2" | (-2) -> "nil" 
	   | j -> List.nth lhs (j-1))
	
let print_result lhs ppf =
  Array.iteri 
    (fun i s ->
       if i > 0 then Format.fprintf ppf ",";
       print_source lhs ppf s; 
    )
    
let print_ret lhs ppf (code,ret,ar) = 
  Format.fprintf ppf "$%i" code;
  if Array.length ret <> 0 then 
    Format.fprintf ppf "(%a)" (print_result lhs) ret
      
let print_ret_opt ppf = function
  | None -> Format.fprintf ppf "*"
  | Some r -> print_ret [] ppf r
      
let gen_lhs prefix d code =
  let arity = d.arity.(code) in
  let r = ref [] in
  for i = 0 to arity - 1 do r := Format.sprintf "%s%i" prefix i :: !r done;
  !r
    
let print_kind ppf actions =
  let print_lhs ppf (code,lhs) =
    Format.fprintf ppf "$%i(" code;
    let rec aux = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "%s" x
      | x::r -> Format.fprintf ppf "%s,x" x; aux r
    in aux lhs;
    Format.fprintf ppf ")" in
  let print_basic (t,ret) =
    Format.fprintf ppf " | %a -> %a@\n"
      Types.Print.print t
      (print_ret []) ret
  in
  let print_prod2 lhs = function
    | Impossible -> assert false
    | Ignore r ->
	Format.fprintf ppf "%a\n" 
	  (print_ret lhs) r
    | TailCall d ->
	Format.fprintf ppf "%a v2@\n" rec_state d
    | Dispatch (d, branches) ->
	Format.fprintf ppf "@\n        match %a v2 with@\n" rec_state d;
	Array.iteri 
	  (fun code r ->
	     let rhs = gen_lhs "r" d code in
	     Format.fprintf ppf "        | %a -> %a@\n" 
	       print_lhs (code,rhs)
	       (print_ret (rhs@lhs)) r;
   	  )
	  branches
  in
  let print_prod prefix ppf = function
    | Impossible -> ()
    | Ignore d2 ->
	Format.fprintf ppf " | %s(v1,v2) -> " prefix;
	print_prod2 [] d2
    | TailCall d ->
	Format.fprintf ppf " | %s(v1,v2) -> %a v1@\n" prefix rec_state d
    | Dispatch (d,branches) ->
	Format.fprintf ppf " | %s(v1,v2) -> @\n" prefix;
	Format.fprintf ppf "      match %a v1 with@\n" rec_state d;
	Array.iteri 
	  (fun code d2 ->
	     let lhs = gen_lhs "l" d code in
             Format.fprintf ppf "      | %a -> " print_lhs (code,lhs);
	     print_prod2 lhs d2;
   	  )
	  branches
  in
  let rec print_record_opt ppf = function
    | None -> ()
    | Some (RecLabel (l,d)) ->
	print_prod ("record:"^(Label.string_of_attr l)) ppf d
    | Some (RecNolabel (r1,r2)) ->
	Format.fprintf ppf " | Record -> @\n";
	Format.fprintf ppf "     SomeField:%a;NoField:%a@\n" 
          print_ret_opt r1 print_ret_opt r2
  in
  
  List.iter print_basic actions.basic;
  print_prod "" ppf actions.prod;
  print_prod "XML" ppf actions.xml;
  print_record_opt ppf actions.record
    
let print_actions ppf = function
  | AKind k -> print_kind ppf k
  | AIgnore r -> Format.fprintf ppf "v -> %a@\n" (print_ret []) r

let print_state_opt ppf d =
  if Hashtbl.mem printed d.uid then () 
  else (
    Hashtbl.add printed d.uid ();
    Format.fprintf ppf "State %i = function@\n" d.uid;
    print_actions ppf d.actions;
    Format.fprintf ppf "====================================@."
  )

let print_state ppf d =
  Hashtbl.clear printed;
  queue := [ d ];
  while !queue <> [] do
    let d = List.hd !queue in
    queue := List.tl !queue;
    print_state_opt ppf d
  done;
  Hashtbl.clear printed
 
