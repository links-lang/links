(*pp deriving *)
open Utility
(*module Env = Env_links
open Env	       *)

type name_map = (int * string) list
		deriving (Show)
       
module TraverseIr = struct
  (** TODO: Remove binders_map after the bug in the class below has been fixed. **)
  let binders_map prog =    
    let rec computation : (Ir.var * string) list -> Ir.computation -> (Ir.var * string) list =
      fun map (bs, tc) ->
      tail_computation (bindings map bs) tc
    and tail_computation : (Ir.var * string) list -> Ir.tail_computation -> (Ir.var * string) list =
      fun map ->
      function
      | `Case (v, clauses, default_clause) ->
	 let map = Utility.StringMap.fold (fun _ (b,c) map -> computation (binder b :: map) c) clauses map in
	 map @ (Utility.from_option [] (Utility.opt_map (fun (b,c) -> computation (binder b :: map) c) default_clause))
      | `If (v, c1, c2) -> (computation map c2) @ (computation map c2)
      | `Special s -> special map s
      | _ -> map
    and special : (Ir.var * string) list -> Ir.special -> (Ir.var * string) list =
      fun map ->
      function
      | `Handle (v, clauses, _) ->
	 StringMap.fold (fun _ (b,c) map -> binder b :: map @ (computation map c)) clauses map
      | _ -> map
    and bindings : (Ir.var * string) list -> Ir.binding list -> (Ir.var * string) list  =
      fun map -> 
      function
      | b :: bs ->
	 begin
	   match b with
	   | `Let (b, (_,tc)) -> bindings ((binder b)::map @ tail_computation map tc) bs
           | `Alien (b,_) -> assert false
           | `Fun (b, (_, args, comp), _, _) ->
	      let map = computation ((binder b :: (List.map binder args)) @ map) comp in
	      bindings map bs
           | `Rec ((b, (_, args, comp), _, _) :: rest) ->	      
	      let funs = List.map (fun f -> `Fun f) rest in
	      let map = computation map comp in
	      let map = bindings (binder b :: (List.map binder args) @ map) funs in
	      bindings map bs
	   | _ -> assert false
	 end
      | [] -> map
    and binder : Ir.binder -> (Ir.var * string) =     
      fun b ->
      match Var.name_of_binder b with
      | "" -> (Var.var_of_binder b, "_v")
      | name -> (Var.var_of_binder b, name)
    in
    let map = computation [] prog in
    (*    print_endline (Show_name_map.show map); *)
    IntMap.from_alist map
  
  class gatherer =
  object ((o : 'self_type))

    val name_map : (int * string) list = []
    method with_name_map : (int * string) list -> 'self_type =
      fun env ->
      {< name_map = env >}

    method get_name_map : string IntMap.t =
      print_endline (Show_name_map.show name_map);
      IntMap.from_alist name_map
					 	
    val operations = StringSet.empty
		       
    method with_operations : StringSet.t -> 'self_type =
      fun ops ->
      {< operations = ops  >}

    method get_operation_env : int StringMap.t  =
      let operations = StringSet.fold (fun s acc -> (s, Var.fresh_raw_var ()) :: acc) operations [] in
      StringMap.from_alist operations
	   
    method computation : Ir.computation -> 'self_type =
      fun (bs,tc) ->
      let o = o#bindings bs in
      o#tail_computation tc
					     
    method tail_computation : Ir.tail_computation -> 'self_type =
      function
      | `Case (v, clauses, default_clause) ->
	 let o' = o#value v in
	 (* Collect binders *)
	 let collect_from = fun (b,c) -> (o#with_name_map (o#binder b :: name_map))#computation c in
	 let o'' = StringMap.fold (fun _ clause o -> collect_from clause) clauses o' in
	 from_option o'' (opt_map collect_from default_clause)
      | `If (v, c1, c2) ->
	 let o' = o#computation c1 in
	 o'#computation c2
      | `Special s -> o#special s
      | _ -> o

    method special : Ir.special -> 'self_type =
      function
      | `Handle (v, clauses,_) ->
	 let o = o#value v in
	 (* Collect binders *)
	 StringMap.fold (fun _ (b,c) o -> (o#with_name_map (o#binder b :: name_map))#computation c) clauses o
      | `DoOperation (name, args, _) ->
	 o#with_operations (StringSet.add name operations)
      | _ -> o
		    
    method value : Ir.value -> 'self_type =
      function
      | _ -> o

    method bindings : Ir.binding list -> 'self_type =
      function
      | b :: bs -> (o#binding b)#bindings bs
      | [] -> o


    method binding : Ir.binding -> 'self_type =      
      function
      | `Let (b, (_, tc)) -> (o#with_name_map (o#binder b :: name_map))#tail_computation tc
      | `Alien (b,_) -> o#with_name_map (o#binder b :: name_map)
      | `Fun (b, (_, args, comp), _, _) ->
	 (o#with_name_map (o#binder b :: (List.map o#binder args) @ name_map))#computation comp
      | `Rec ((b, (_, args, comp), _, _) :: rest) ->	      
	 let funs = List.map (fun f -> `Fun f) rest in
	 let o = o#computation comp in
	 (o#with_name_map (o#binder b :: (List.map o#binder args) @ name_map))#bindings funs
      | _ -> assert false

    method binder : Ir.binder -> (int * string) =
      fun b ->
      (Var.var_of_binder b, match Var.name_of_binder b with
			    | "" -> "_v"
			    | n  -> n)

    method funbinder : Ir.binder -> (int * string) =
      fun b ->
      (Var.var_of_binder b, match Var.name_of_binder b with
			    | "" -> "_fun"
			    | n  -> n)

    method program : Ir.program -> 'self_type =
      fun comp ->
      o#computation comp     
  end
    
  let gather ir =
    new gatherer#program ir
end
