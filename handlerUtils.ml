open Utility
open TypeUtils

type operation           = string * Types.datatype

type operation_raw       = RawOperation of string * Types.datatype * Types.datatype
			 (*| RawSpecial   of string * Types.datatype*)
			 | RawFailure   of string

let lookup_present_safe : string -> Types.field_spec_map -> Types.datatype option
  = fun name fields ->
  match StringMap.lookup name fields with
    Some (`Present p) -> Some p
  | _ -> None

let get_operation_arg_type : string -> Types.field_spec_map -> Types.datatype option 
  = fun index fields ->
  match lookup_present_safe index fields with
    Some p -> Some (TypeUtils.concrete_type p)
    (*Some ((`MetaTypeVar point) as p) -> 
    begin
      match Unionfind.find point with
        `Body p -> Some p
      | _       -> Some p
    end
  | Some p -> Some p*)
  | _ -> None

let get_operation : string -> Types.row -> operation option
  = fun name (fields,_,_) ->
  match lookup_present_safe name fields with
    Some p -> 
    begin
      match p with
	`Record (fields,_,_) ->
	begin 
	  match get_operation_arg_type "1" fields with
	    Some p -> Some (name, p)
	  | _ -> None
	end
      | _ -> assert false
    end
  | _ -> None

(* Extracts and normalises operations from an effect row. 
 * Definition of "normalised operation signature":
 *   If an operation signature is on the form:
 *    Op:(x,k) where k has a function type.
 *  then its said to be normalised.
 *  In other words, the continuation parameter is guaranteed 
 *  to have a function type.
 *)	   
let extract_operations : Types.row -> operation_raw list
  = fun (fields,_,_) -> 
  let normalise_operation_signature name optype =
    match optype with
      `Record (types,_,_) ->
      let num_params = StringMap.size types in
      if num_params = 2 then
	let (Some p) = get_operation_arg_type "1" types in
	let (Some k) = get_operation_arg_type "2" types in
	match TypeUtils.concrete_type k with
	  ((`Function _) as k) -> RawOperation (name, p, k) (* Is already normalised *)
	| _ as inp_t -> (* Needs to be normalised: Construct new function type. *)
	   let out_t = Types.fresh_type_variable (`Unl, `Any) in
	   let k     = Types.make_pure_function_type inp_t out_t in (* or inp_t instead of p? *)
	   RawOperation (name, p, k)	 
(*	| _ -> RawFailure ("expected last parameter in operation " ^ name ^ " to have a function type.") (* Continuation must be a function type. *)*)
      else
	RawFailure ("Operation " ^ name ^ " accepts " ^ (string_of_int num_params) ^ " argument(s), but operations must 2 arguments.")
    | _ -> RawFailure (name ^ " is not an operation") (* The signature is not a valid operation signature *)
  in
  let fields = StringMap.filter (fun _ t -> match t with
					  `Present _ -> true
					 | _         -> false
				) fields (* Note: This step should not be necessary. *)
  in
  let fields = StringMap.to_list (fun name t -> (name,t)) fields in
  List.map (fun (name,t) -> match t with
			   `Present p -> normalise_operation_signature name p
			 | _          -> assert false (* cannot occur *)
	   ) fields
  

let wrap_in_record t =
  Types.make_record_type (StringMap.add "1" t StringMap.empty)

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

(* Simplifies the operation signatures, e.g. {Get:((), (a) -> (b) -> c)} becomes {Get:(() -> (a))} 
 *)
let simplify_operations  : operation_raw list -> operation list
  = fun ops ->
  let simplify_operation : operation_raw -> operation
    = fun op ->
    match op with
      RawOperation (name, p, k) -> let p  = wrap_in_record p in
				   let r  = List.hd (TypeUtils.arg_types k) in
				   let p  = Types.make_pure_function_type p r in
				   (name, p)
    | RawFailure msg    -> failwith msg
  in
  List.map simplify_operation ops


let effectrow_of_oplist : operation list -> bool -> Types.row
  = fun ops isclosed ->
  let fields = List.fold_left (fun fields (name, optype) -> StringMap.add name optype fields) StringMap.empty ops in  
  if isclosed then
    Types.make_closed_row fields
  else
    let row = Types.make_empty_open_row (`Unl, `Any) in
    Types.extend_row fields row
    

let oplist_of_effectrow : Types.row -> operation list
  = fun (fields,_,_) ->
  let ops = List.rev (StringMap.fold (fun name t ops -> (name, t) :: ops) fields []) in
  List.map (fun (name,t) -> match t with
			      `Present p -> (name,p)
			    | _ -> assert false
	   ) ops
			  

let fix_operation_arity : Types.row -> Types.row
  = fun ((fields,row_var,dual) as row) ->
  let fix_arity : operation -> operation
    = fun (name, opsig) ->
    match opsig with
      `Function (dt,eff,rt) as t -> 
      begin
	match dt with
	  `Record (fields,_,_) as r when StringMap.size fields = 0 || StringMap.size fields > 1 -> (name, `Function (wrap_in_record r, eff, rt)) (* Ensures that Op() is interpreted as Op(()), and Op(x1,...,xN) is interpreted as Op((x1,...,xN)) *)
	  | _ -> (name, t)
      end
    | _ as t -> (name, t)
  in
  let oplist = oplist_of_effectrow row in
  let oplist = List.map fix_arity oplist in
  let (fields,_,_) = effectrow_of_oplist oplist false in
  (fields,row_var,dual)

let is_operation_invalid : operation_raw -> bool
  = function
    RawFailure _  -> true
   | _             -> false

let extract_continuation_tails : operation_raw list -> Types.datatype list
  = fun ops ->
  List.map (function
	     | RawOperation (_, _, (`Function (_,_,t) as f)) -> t
	     | _ -> assert false
	   ) ops

let return_case = "Return"	   

let handles_operation : Types.row -> string -> bool
  = fun (fields,_,_) opname -> StringMap.mem opname fields		   

let is_closed spec =
  match spec with
    `Pure
  | `Closed -> true
  | _ -> false

let make_operation_row_polymorphic : Types.row -> Types.row
  = fun (types,row_var,dual) ->
  let types = StringMap.map
		(function
		    `Present _ -> Types.fresh_presence_variable (`Unl, `Any)
		  | _ -> assert false					
		) types
  in
  (types,row_var,dual)

let allow_wild : Types.row -> Types.row
  = fun row ->
  let fields = StringMap.add "wild" (Types.unit_type) StringMap.empty in
  Types.extend_row fields row
  
