(*pp deriving*)
open Utility

module TraverseIR = struct        
  class gatherer =
  object ((o : 'self_type))

    val name_env : (int * string) list = []
    method with_name_env : (int * string) list -> 'self_type =
      fun env ->
      {< name_env = env >}

    method get_name_env : (int * string) list = name_env
					 	
    val operations = StringSet.empty
    val operation_id_map = None		       
		       
    method with_operations : StringSet.t -> 'self_type =
      fun ops ->
      {< operations = ops  >}

(*    method get_operations : StringMap.t =
      match unique_operations with
      | Some map -> map
      | None -> *)
	   
    method computation : Ir.computation -> 'self_type =
      fun (bs,tc) ->
      let o = o#bindings bs in
      o#tail_computation tc
					     
    method tail_computation : Ir.tail_computation -> 'self_type =
      function
      | `Case (v, clauses, default_clause) ->
	 let o = o#value v in
	 (* Collect binders *)
	 let o = StringMap.fold (fun _ (b,c) o -> (o#with_name_env (o#binder b :: name_env))#computation c) clauses o in
	 (from_option o (opt_map (fun (b,c) -> (o#with_name_env (o#binder b :: name_env))#computation c) default_clause))
      | `If (v, c1, c2) ->
	 let o = o#computation c1 in
	 o#computation c2
      | _ -> o

    method special : Ir.special -> 'self_type =
      function
      | `Handle (v, clauses,_) ->
	 let o = o#value v in
	 (* Collect binders *)
	 StringMap.fold (fun _ (b,c) o -> (o#with_name_env (o#binder b :: name_env))#computation c) clauses o
      | `DoOperation (name, args, _) -> o#with_operations (StringSet.add name operations)
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
      | `Let (b, _)
      | `Alien (b,_) -> o#with_name_env (o#binder b :: name_env)
      | `Fun (b, (_, args, comp), _, _) ->
	 (o#with_name_env (o#binder b :: (List.map o#binder args) @ name_env))#computation comp
      | `Rec ((b, (_, args, comp), _, _) :: rest) ->	      
	 let funs = List.map (fun f -> `Fun f) rest in
	 let o = o#computation comp in
	 (o#with_name_env (o#binder b :: (List.map o#binder args) @ name_env))#bindings funs
      | _ -> assert false

    method binder : Ir.binder -> (int * string) =
      fun b -> (Var.var_of_binder b, Var.name_of_binder b)

    method program : Ir.program -> 'self_type =
      fun comp ->
      o#computation comp     
  end
    
  let gather ir =
    new gatherer#program ir
end
