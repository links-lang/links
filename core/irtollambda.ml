type name_env = string Utility.intmap
type globals  = int Utility.intmap
type effenv   = int Utility.stringmap

open LLambda
open Ir
open Utility



module Llambda = LLambda

let is_primitive   : Var.var -> bool = Lib.is_primitive_var
let primitive_name : Var.var -> string option =
  fun var ->
  try
    Some (Lib.primitive_name var)
  with
  | Not_found -> None  

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



let error msg = failwith (Printf.sprintf "Translation error: %s" msg)

let ir_llambda : string -> globals -> name_env -> effenv -> Ir.program -> Llambda.program =
  fun filename globals nenv unused prog ->
  (********************Auxiliary functions from llambda, refactor into separate file******************)
  let fresh_identifier identname = {name = identname ; uid = (Var.fresh_raw_var ()) } in
  let rec is_primitive_function : Ir.value -> Ir.var option = function
  | `Variable var -> if is_primitive var then Some var else None
  | `TAbs (_,v) 
  | `TApp (v,_) -> is_primitive_function v
  | v -> error ("Unknown, possibly primitive, node:\n " )
  in  
  let ident_of_var var =
    try
      let vname = IntMap.find var nenv in
      { name = vname ; uid = var }
      with
      | Notfound.NotFound _ -> error ("Cannot find name for var " ^ (string_of_int var))
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
    with | Notfound.NotFound _ -> false         
  in
  let make_global ident get_set =
    try
      let num = IntMap.find ident.uid globals in
      `Primitive (`Global (ident, num, String.capitalize_ascii filename, get_set))
    with
    | Not_found -> error ("Cannot find global position for identifier " ^ (string_of_identifier ident))
  in
  let get_global ident = make_global ident `Get in
  let set_global ident = make_global ident `Set in
(******************************************************************************************************)   


  let rec value : Ir.value -> LLambda.program = function
    |  `Constant constant -> `Constant constant 
    |  `Variable var -> if is_primitive var then `Primitive (match primitive_name var with
    	                                                        | Some "Nil" -> `Nil
    	                                                        | Some f -> `Builtin f
    	                                                        | None -> error("Cannot evaluate variable name")
    	                                                                 )
    	                                                else let ident = ident_of_var var in
    	                                                      if is_global ident then get_global ident
    	                                                      else `Variable ident

    |  `Extend (fieldlist, record) -> 
         if Utility.StringMap.size fieldlist > 0 then
           `Extend (Utility.StringMap.map (fun v -> value v) fieldlist, opt_map (fun v -> value v) record )
         else 
            `Unit
    |  `Project (nameField, valueRecord) -> `Project (nameField, value valueRecord)
    (*-|  'Erase (fields, record) ->-*)
    |  `Inject (label, v , typ) -> `Inject (label, value v, typ)
    |  `TAbs (vars, v) -> value v
    |  `TApp (v, typs) -> value v
    (*|  'XmlNode          ->*)
    |  `ApplyPure (f, args)  -> tail_computation (`Apply (f, args))
    |  `Coerce (v, typs) -> value v
    (*|  `Closure (vr, tyargs, vl) *)
    |  value -> error ("Unimplemented case :\n")
  and tail_computation : Ir.tail_computation -> Llambda.program = function
    |  `Return v -> value v
    |  `Apply (f,args) -> let vals =
                            if List.length args > 0 then
                              List.map value args
                            else
                             [`Unit]
                           in begin
                         	  match is_primitive_function f with
                         	  | None -> `Apply (value f, vals)
                         	  | Some prim -> `Primitive (let fp = match primitive_name prim with
                                          | Some "print" -> "print"
                         	                | Some name -> name
                         	                | None -> error ("Cannot match function name")
                         	                in
                         	                  begin
                         	              	    match binop_of_string fp with
                         	              	    | Some binop -> `BinOp (binop, vals)
                         	              	    | None -> if fp = "print" then `Print vals else error("No such binary operation " ^ fp)
                         	                | _ -> `FnApply (fp, vals)   
                         	                  end)
                              end
    |  `Special s -> error("Special Not implemented ")
    |  `Case (v, cases, def) -> let translate (b,c) = (ident_of_binder b, computation c) in
                                    `Case (value v, StringMap.map translate cases, opt_map translate def)
    |  `If (cond, t, f) -> `If (value cond, computation t, computation f)
  and computation : Ir.computation -> Llambda.program = fun (binds,tailcomp) -> bindings binds (tail_computation tailcomp)    
  and bindings : Ir.binding list -> LLambda.program -> LLambda.program = fun bindings body -> match bindings with
    | []  -> body
    | [b] -> binding b body
    | b :: bs  ->
       let b_ret = List.fold_left (fun program b -> fun k -> program (binding b k)) (binding b) bs (*chain bindings together*) in
       b_ret body       
  and binding : Ir.binding -> LLambda.program -> LLambda.program = fun binding body -> match binding with
    |  `Let (binder, (tyvars, tailcomp) ) -> `Let (ident_of_binder binder, tail_computation tailcomp, body)
    |  `Fun (binder, (tyvars, binderlist, comp), bindop, loc) -> let binder = ident_of_binder binder in 
                                                                 let binderlist = 
                                                                    if List.length binderlist > 0 then
                                                                      List.map ident_of_binder binderlist
                                                                    else
                                                                      [fresh_identifier "_unit"]
                                                                 in `Let(binder, `Fun(binderlist, computation comp), set_global binder)
    | `Rec funs ->  
       let funs =
         List.fold_right
           (fun (b, (_, bs, comp),_,_) funs ->
             let b = ident_of_binder b in
             let bs =
               if List.length bs > 0 then
                 List.map ident_of_binder bs
               else
                 [fresh_identifier "_unit"]
             in
             let comp = computation comp in
             (b, `Fun (bs, comp)) :: funs)
           funs []
       in
       `Rec (funs, body)
    (*| `Module (name, binderlist) *)
    (*| `Alien (binder, name, lang)  (**Ask Sam about**) *)
    | _ -> error ("Unimplemented feature of type binding:\n")
  (****************************************************************************)    
  and toplevel_binding : Ir.binding -> LLambda.program =
    fun b ->
    match b with
    | `Let (b, (_, tc)) ->
       let b = ident_of_binder b in
       `Let (b, tail_computation tc, set_global b)
    | `Fun (b, (_, bs, comp), _, _) ->
       let b = ident_of_binder b in
       let bs =
         if List.length bs > 0 then
           List.map ident_of_binder bs
         else
           [fresh_identifier "_unit"]
       in
       let comp = computation comp in
       let global = set_global b in
      `Let (b, `Fun (bs, comp), global)
    | `Rec funs ->
       let funs =
         List.fold_right
           (fun (b, (_, bs, comp),_,_) funs ->
             let b = ident_of_binder b in
             let bs =
               if List.length bs > 0 then
                 List.map ident_of_binder bs
               else
                 [fresh_identifier "_unit"]
             in
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
    | b -> error ("Unimplemented feature:\n" )
  and toplevel_bindings : Ir.binding list -> LLambda.program ->  LLambda.program =
    fun bs k ->
    let program_end = `Sequence (k, `Unit) in (* Ensure that the program ends in exit code 0 *)
    let bind_effects = (* splice in effect labels *)
      StringMap.fold
        (fun _ uid program ->
          let ident = ident_of_var uid in
          let set_ooid = `Primitive (`SetOOId (filename, ident.name)) in
          let set_global = set_global ident in
          let register_effect = `Let (ident, set_ooid, set_global) in
          (fun k -> program (`Sequence (register_effect, k))))
        unused (fun k -> k)
    in
    match bs with
    | []  -> bind_effects k
    | [b] -> `Sequence (bind_effects (toplevel_binding b), k)
    | b :: bs  ->
       let bs =
         List.fold_left
           (fun program b ->
             fun k -> program (`Sequence (toplevel_binding b, k)))
           (fun k -> `Sequence (toplevel_binding b, k)) bs
       in
       bind_effects (bs k)
  and program : Ir.program -> LLambda.program =
    fun (bs,tc) -> toplevel_bindings bs (tail_computation tc)
  in  
  program prog

