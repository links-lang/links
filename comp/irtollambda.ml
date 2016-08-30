(*let primeir_of_ir : Ir.program -> PrimeIr.program =
  fun program ->
  let rec value : Ir.value -> PrimeIr.value = function
    | _ -> assert false
  and computation : Ir.computation -> PrimeIr.computation =
    fun (bs,tc) -> (bindings bs, tail_computation tc)
  and tail_computation : Ir.tail_computation -> PrimeIr.tail_computation = function
    | _ -> assert false
  and binding : Ir.binding -> PrimeIr.binding = function
    | _ -> assert false
  and bindings : Ir.binding list -> PrimeIr.binding list =
    fun bs -> List.map binding bs
  and special : Ir.special -> PrimeIr.special = function
    | _ -> assert false
  in
  computation program*)

type name_env = string Utility.intmap
type globals  = int Utility.intmap                       

let binop_of_string : string -> LLambda.binary_operation option =
  fun binop_name ->
  try 
    Some
      (match binop_name with
       | "+" -> `Plus, Some `Int
       | "-" -> `Minus, Some `Int
       | "*" -> `Mult, Some `Int
       | "/" -> `Div, Some `Int
       | "mod" -> `Mod, Some `Int
       | "+." -> `Plus, Some `Float
       | "-." -> `Minus, Some `Float
       | "*." -> `Mult, Some `Float
       | "/." -> `Div, Some `Float
       | "==" -> `Eq, None
       | "<>" -> `Neq, None
       | "<"  -> `Lt, None
       | ">"  -> `Gt, None
       | "<=" -> `Le, None
       | ">=" -> `Ge, None
       | "&&" -> `And, Some `Bool
       | "||" -> `Or, Some `Bool
       | _ -> raise Not_found)
  with
  | Not_found -> None

                       
let is_primitive   : Var.var -> bool = Lib.is_primitive_var
let primitive_name : Var.var -> string option =
  fun var ->
  try
    Some (Lib.primitive_name var)
  with
  | Not_found -> None               
                   
let llambda_of_ir : string -> globals -> name_env -> Ir.program -> LLambda.program =
  fun module_name globals nenv prog ->
  let open LLambda in
  let open Utility in
  let error msg = failwith (Printf.sprintf "Translation error: %s" msg) in
  let ident_of_var var =
    try
      let vname = IntMap.find var nenv in
      { name = vname ; uid = var }
      with
    | _ -> error ("Cannot find name for var " ^ (string_of_int var))
  in
  let ident_of_binder b =
    let var   = Var.var_of_binder b in
      match Var.name_of_binder b with
      | ""    -> ident_of_var var
      | bname -> { name = bname ; uid = var }
  in
  let global_counter = ref 0 in
  let is_global ident =
    try
      let _ = IntMap.find ident.uid globals in
      true
    with | _ -> false
  in
  let make_global ident get_set =
    try
      let num = IntMap.find ident.uid globals in
      `Primitive (`Global (ident, num, String.capitalize_ascii module_name, get_set))
    with | _ -> error ("Cannot find global position for identifier " ^ (string_of_identifier ident))
  in
  let get_global ident = make_global ident `Get in
  let set_global ident = make_global ident `Set in
  let rec is_primitive_function : Ir.value -> Ir.var option = function
  | `Variable var -> if is_primitive var then Some var else None
  | `TAbs (_,v) 
  | `TApp (v,_) -> is_primitive_function v
  | v -> error ("Unknown, possibly primitive, node:\n " ^ (Ir.Show_value.show v))
  in
  let rec value : Ir.value -> LLambda.program = function
    | `TAbs (_, v)
    | `TApp (v, _) -> value v
    | `Constant const -> `Constant const
    | `ApplyPure (f, args) -> tail_computation (`Apply (f, args))
    | `Variable var ->
       if is_primitive var then
	 `Primitive
          (match primitive_name var with
	   | Some "Nil" -> `Nil
	   | Some fun_name -> `Builtin fun_name
           | None -> error ("Cannot find primitive name for var " ^ (string_of_int var)))
       else
         let ident = ident_of_var var in
         if is_global ident then
           get_global ident
         else
           `Variable ident
    | `Extend (fields, record) ->
       if StringMap.size fields > 0 then
         let fields = StringMap.map (fun v -> value v) fields in
         let record = opt_map (fun v -> value v) record in
         `Extend (fields, record)
       else
         `Unit
    | `Project (label, record) -> `Project (label, value record)
    | `Inject (label, arg, t)  -> `Inject (label, value arg, t)
    | `Coerce (v,_)            -> value v
    | v -> error ("Unimplemented feature:\n" ^ (Ir.Show_value.show v))
  and computation : Ir.computation -> LLambda.program =
    fun (bs,tc) -> bindings bs (tail_computation tc)         
  and tail_computation : Ir.tail_computation -> LLambda.program = function
    | `Return v -> value v
    | `Special s -> special s
    | `If (cond, tbranch, fbranch) -> `If (value cond, computation tbranch, computation fbranch)
    | `Case (v, clauses, default_clause) ->
       let translate (b,c) = (ident_of_binder b, computation c) in
       let clauses = StringMap.map translate clauses in
       let default_clause = opt_map translate default_clause in
       `Case (value v, clauses, default_clause)
    | `Apply (f, args) ->
       let args = List.map value args in
       begin
	 match is_primitive_function f with
	 | None     -> `Apply (value f, args)
	 | Some uid ->
            `Primitive (
	    (* Next, figure out which type of primitive f is *)
	       let fun_name =
	         match primitive_name uid with
	         | Some name -> name
	         | None -> error ("Cannot find primitive name for var " ^ (string_of_int uid))
	       in
               begin
                 match binop_of_string fun_name with
                 | Some binop -> `BinOp (binop, args)
	         | _ -> `FnApply (fun_name, args)
	       end)
       end
    | tc -> error ("Unimplemented feature:\n" ^ (Ir.Show_tail_computation.show tc))
  and binding : Ir.binding -> LLambda.program -> LLambda.program =
    fun b k ->
    match b with
    | `Let (b, (_, tc)) -> `Let (ident_of_binder b, tail_computation tc, k)
    | `Fun (b, (_, bs, comp), _, _) ->
       let b = ident_of_binder b in
       let bs = List.map ident_of_binder bs in
       let comp = computation comp in
      `Let (b, `Fun (bs, comp), k)
    | `Rec funs ->
       let funs =
         List.fold_right
           (fun (b, (_, bs, comp),_,_) funs ->
             let b = ident_of_binder b in
             let bs = List.map ident_of_binder bs in
             let comp = computation comp in
             (b, `Fun (bs, comp)) :: funs)
           funs []
       in
       `Rec (funs, k)
    | b -> error ("Unimplemented feature:\n" ^ (Ir.Show_binding.show b))
  and bindings : Ir.binding list -> LLambda.program -> LLambda.program =
    fun bs k ->
    match bs with
    | []  -> k
    | [b] -> binding b k
    | b :: bs  ->
       let bs =
         List.fold_left
           (fun program b ->
             fun k -> program (binding b k))
           (binding b) bs
       in
       bs k      
  and toplevel_binding : Ir.binding -> LLambda.program =
    fun b ->
    match b with
    | `Let (b, (_, tc)) ->
       let b = ident_of_binder b in
       `Let (b, tail_computation tc, set_global b)
    | `Fun (b, (_, bs, comp), _, _) ->
       let b = ident_of_binder b in
       let bs = List.map ident_of_binder bs in
       let comp = computation comp in
       let global = set_global b in
      `Let (b, `Fun (bs, comp), global)
    | `Rec funs ->
       let funs =
         List.fold_right
           (fun (b, (_, bs, comp),_,_) funs ->
             let b = ident_of_binder b in
             let bs = List.map ident_of_binder bs in
             let comp = computation comp in
             (b, `Fun (bs, comp)) :: funs)
           funs []
       in
       let bs = List.map fst funs in
       let globals =
         match bs with
         | [] -> assert false
         | [b] -> set_global b
         | b :: bs ->            
            let seed = set_global b in
            List.fold_left (fun globals b -> `Sequence (globals, set_global b)) seed bs
       in
       `Rec (funs, globals)
    | b -> error ("Unimplemented feature:\n" ^ (Ir.Show_binding.show b))
  and toplevel_bindings : Ir.binding list -> LLambda.program ->  LLambda.program =
    fun bs k ->
    let k = `Sequence (k, `Unit) (* Ensure that the program ends in exit code 0 *)
    in
    match bs with
    | []  -> k
    | [b] -> `Sequence (toplevel_binding b, k)
    | b :: bs  ->
       let bs =
         List.fold_left
           (fun program b ->
             fun k -> program (`Sequence (toplevel_binding b, k)))
           (fun k -> `Sequence (toplevel_binding b, k)) bs
       in
       bs k       
  and special : Ir.special -> LLambda.program = function
    | `DoOperation (label, args, _) -> `DoOperation (label, List.map value args)
    | `Handle (v, clauses, spec) ->
       let v = value v in
       let clauses = StringMap.map
                       (fun (cc,b,comp) ->
                         let cc =
                           match cc with
                           | `Effect b  -> `Effect (ident_of_binder b)
                           | `Exception -> `Exception
                           | `Regular   -> `Regular
                         in
                         let b = ident_of_binder b in
                         let comp = computation comp in
                         (cc,b,comp)) clauses
       in
       let (return_clause, clauses) = StringMap.pop "Return" clauses in
       `Handle (v, clauses, return_clause, spec)
    | `Wrong _ -> `Wrong
    | s -> error ("Unimplemented feature:\n" ^ (Ir.Show_special.show s))
  and program : Ir.program -> LLambda.program =
    fun (bs,tc) -> toplevel_bindings bs (tail_computation tc)
  in  
  program prog
