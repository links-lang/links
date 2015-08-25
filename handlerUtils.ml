open Utility
open TypeUtils

type raw_operation = string * (Types.datatype list) * Types.datatype (* name, args, continuation *)
type operation = string * Types.datatype

(* Extracts operations from an effect row. 
 * The output is a list of triples, where each triple 
 * is on the form (opname, op parameters, continuation parameter)
 *)	   
let extract_operations : Types.row -> raw_operation list
  = fun (fields,_,_) -> 
  let extract_signatures p =
    match p with
      `Record (fields,_,_) 
    | `Variant (fields,_,_) ->
      let last_element = (string_of_int (StringMap.size fields)) in
      let (k,fields) = StringMap.pop last_element fields in
      let k = match k with
	  `Present k -> k
	| _ -> assert false
      in
      let ps = StringMap.fold (fun _ t ps -> match t with
					       `Present t -> (Types.concrete_type t) :: ps
					     | _ -> assert false
			      ) fields []
      in
      (ps,k)
    | _ -> ([],p)
  in
  let fields = StringMap.to_list (fun name t -> (name,t)) fields in
  List.map (fun (name,t) -> match t with
			      `Present p -> let (ps, k) = extract_signatures p in (name, ps, k)
			    | _          -> assert false (* this should never occur *)
		      ) fields  
  

let wrap_in_record t =
  Types.make_record_type (StringMap.add "1" t StringMap.empty)

(* Simplifies the operation signatures, e.g. {Get:((), (a) -> (b) -> c)} becomes {Get:(() -> (a))} 
 *)
let simplify_operations  : raw_operation list -> operation list
  = fun ops ->
  let simplify_operation : raw_operation -> operation
    = fun (name, ps, k) ->
    let rec curry ps r =
      match ps with
	[p] -> Types.make_pure_function_type p r
      | p :: ps -> Types.make_pure_function_type p (curry ps r)
    in
    let r = List.hd (TypeUtils.arg_types (Types.concrete_type k)) in
    match ps with
      [] -> (name, r)
    | _  ->  let ps = Types.make_tuple_type ps in
	     let ps = Types.make_pure_function_type ps r in
	     (name,ps)
  in
  List.map simplify_operation ops


let effectrow_of_oplist : operation list -> Sugartypes.handler_spec -> Types.row
  = fun ops spec ->
  let fields = List.fold_left (fun fields (name, optype) -> StringMap.add name optype fields) StringMap.empty ops in  
  match spec with
    `Closed -> Types.make_closed_row fields
  | `Open -> let row = Types.make_empty_open_row (`Unl, `Any) in
	     Types.extend_row fields row
  | _ -> failwith "handlerUtils.ml: Handler specialisation not yet support."
    

let oplist_of_effectrow : Types.row -> operation list
  = fun (fields,_,_) ->
  let ops = List.rev (StringMap.fold (fun name t ops -> (name, t) :: ops) fields []) in
  List.map (fun (name,t) -> match t with
			      `Present p -> (name,p)
			    | _ -> assert false
	   ) ops
			  

let unwrap_from_record r =
  match r with
  | `Record (smap,_,_) ->
     begin
       let (p,_) = StringMap.pop "1" smap in
       match p with
       | `Present p -> p
       | _ -> failwith "Error: Attempt to unwrap non-present type"
     end
  | _ -> r
	   
let fix_operation_arity : Types.row -> Types.row
  = fun ((fields,row_var,dual) as row) -> row
(*  let fix_arity : operation -> operation
    = fun (name, opsig) ->
    match opsig with
      `Function (dt,eff,rt) as t -> 
      begin
	match dt with
	`Record (fields,_,_) as r when StringMap.size fields > 1 -> (name, `Function (unwrap_from_record r, eff, rt)) (* Ensures that Op() is interpreted as Op(()), and Op(x1,...,xN) is interpreted as Op((x1,...,xN)) *)
	  | _ -> (name, t)
      end
    | _ as t -> (name, t)
  in
  let oplist = oplist_of_effectrow row in
  let oplist = List.map fix_arity oplist in
  let (fields,_,_) = effectrow_of_oplist oplist `Closed in
  (fields,row_var,dual)*)

let extract_continuation_tails : raw_operation list -> Types.datatype list
  = fun ops ->
  List.map (function
	     | (_, _, (`Function (_,_,t))) -> t
	     | (name,ps,t) -> failwith (name ^ ": Param size: " ^ (string_of_int (List.length ps)) ^ ", Type h: " ^ (Types.string_of_datatype t))
	   ) ops

let return_case = "Return"	   

let handles_operation : Types.row -> string -> bool
  = fun (fields,_,_) opname -> StringMap.mem opname fields		   

let is_closed spec =
  match spec with
    `Pure
  | `Closed -> true
  | _ -> false

let allow_wild : Types.row -> Types.row
  = fun row ->
  let fields = StringMap.add "wild" Types.unit_type StringMap.empty in
  Types.extend_row fields row

let make_operations_presence_polymorphic : Types.row -> Types.row
  = fun (signatures,row_var,dual) ->
  let has_wild = StringMap.exists (fun name _ -> String.compare name "wild" == 0) signatures in
  let signatures = StringMap.filter (fun name _ -> String.compare name "wild" <> 0) signatures in
  let signatures = StringMap.map
		(function
		    `Present _ -> Types.fresh_presence_variable (`Unl, `Any)
		  | _ -> assert false					
		) signatures
  in
  let row = (signatures, row_var, dual) in
  if has_wild then
    allow_wild row
  else
    row
  
