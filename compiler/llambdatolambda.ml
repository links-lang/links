(** Translate Links-Lambda to Lambda **)

open Links_core

type globalenv = int Utility.intmap
type effenv    = int Utility.stringmap

let lambda_of_llambda : string -> globalenv * effenv -> LLambda.program -> Lambda.lambda =
  fun module_name (globals,effenv) prog ->
  let open LLambda in
  let open Lambda in
  let open LambdaDSL in
  let open Utility in
  let error s = failwith (Printf.sprintf "LLambda-to-Lambda translation error: %s" s) in
  let get_global_effect label =
    try
      let uid = StringMap.find label effenv in
      let pos = IntMap.find uid globals in
      lprim (field pos) [ lgetglobal (String.capitalize_ascii module_name) ]
    with
    | Not_found | Notfound.NotFound _ -> error (Printf.sprintf "%s is not registered as a global" label)
  in
  let builtin fun_name =
    try
      LambdaDSL.lookup "Builtins" fun_name
    with | Not_found -> error (Printf.sprintf "Cannot locate built-in %s" fun_name)
  in
  let identifier : LLambda.identifier -> Ident.t =
    fun {name = name' ; uid = uid' } -> create_ident name' uid'
  in
  let fresh_identifier name = create_ident name (Var.fresh_raw_var ()) in
  let make_pair k v =
    let key = linteger (hash_label k) in
    lprim box [ key ; v ]
  in
  let make_assoc_list base xs =
    List.fold_left
      (fun xs x -> lprim box [ x ; xs ])
      base xs
  in
  let rec translate : LLambda.program -> Lambda.lambda = function
    (* values *)
    | `Constant c -> lconstant constant c
    | `Unit -> lconstant (fun x -> x) unit
    | `Variable ident -> lvar (identifier ident)
    | `Inject (label, v, _) -> lpolyvariant label (Some [translate v])
    | `Project (label, row) ->
       let label = linteger (hash_label label) in
       let row   = translate row in
       let project = builtin "project" in
       lapply project [ label ; row ]
    | `Extend (fields, record) ->
       let fields = StringMap.to_alist fields in
       let pairs  = List.map (fun (k,v) -> make_pair k (translate v)) fields in
       let record =
         match record with
         | Some r -> translate r
         | None   -> lconstant (fun x -> x) unit
       in
       make_assoc_list record pairs
    | `Primitive prim -> primitive prim
    (* tail_computations *)
    | `Apply (f, args) -> lapply (translate f) (List.map translate args)
    | `If (cond, tbranch, fbranch) ->
       lif (translate cond)
           (translate tbranch)
           (translate fbranch)
    | `Case (v, clauses, default_clause) ->
       let v = translate v in
       let default_clause =
	 match default_clause with
	 | None -> lapply (pervasives "failwith") [lstring "Fatal error: Pattern-matching failed"]
	 | Some (b,c) -> llet (identifier b) v (translate c)
       in
       let label = fresh_identifier "_switch" in
       let switch_expr k =
         llet label (lproject 0 v) k
       in
       let vlabel = lvar label in
       switch_expr
	 (StringMap.fold
	    (fun label (b,c) matchfail ->
	      let label = linteger (hash_label label) in
	      lif (neq vlabel label)
		matchfail
                (llet (identifier b)
                      (lproject 1 v)
                      (translate c)
                )
	    ) clauses default_clause
	 )
    (* bindings *)
    | `Sequence (lam, next) -> lseq (translate lam) (translate next)
    | `Let (ident, expr, body) ->
       llet (identifier ident)
            (translate expr)
            (translate body)
    | `Fun (args, body) ->
       let args = List.map identifier args in
       lfun args (translate body)
    | `Rec (funs, body) ->
       let funs =
         List.fold_right
           (fun (ident,body) funs ->
             let ident = identifier ident in
             let body  = translate body in
             (ident,body) :: funs)
           funs []
       in
       lletrec funs (translate body)
    | `Wrong -> lapply (pervasives "failwith") [lstring "Unknown fatal error"]
(*    | `DoOperation (label, args) ->
       let pairs = List.mapi (fun i e -> make_pair (string_of_int (i + 1)) (translate e)) args in
       let assoc_list = make_assoc_list (lconst unit) pairs in
       lperform (translate label) [assoc_list]*)
    (*| `Handle (comp, clauses, return_clause, spec) ->
       let comp = translate comp in
       let value_handler = (* Translate return clause *)
         let (_,ident,comp) = return_clause in
         let ident = identifier ident in
         let comp  = translate comp in
         lfun [ident] comp
       in
       let exn_handler =
         let exn = fresh_identifier "_exn" in
	 lfun [exn] (lraise Lambda.Raise_reraise (lvar exn))
       in
       let eff_handler =
         let eff     = fresh_identifier "_eff" in (* Pointer to the invoked operation *)
         let cont    = fresh_identifier "_cont" in (* Pointer to the captured continuation *)
         let k_ident = fresh_identifier "_k" in
         let multi_shot_k k_ident =
	   let param = fresh_identifier "_param" in
           let k_ident'  = fresh_identifier "_k'" in
	   lfun [param]
                (llet k_ident'
                      (lapply (obj "clone") [lvar k_ident])
		      (lapply (pervasives "continue") [lvar k_ident' ; lvar param])) (* Multi-shot continuation *)
         in
         let one_shot_k k_ident =
           let param = fresh_identifier "_param" in
	   lfun [param]
		(lapply (pervasives "continue") [lvar k_ident ; lvar param]) (* One-shot continuation *)
         in
         let bind_k scope =
           llet ~kind:Alias
                k_ident
                (lprim (makeblock 0 Mutable) [lvar cont])
                scope
         in
         let forward_effect = ldelegate eff cont in (* Forwards the handling of the invoked operation to another handler *)
         let compile_clauses clauses default_clause =
           let efflabel = lproject 0 (lvar eff) in
           StringMap.fold
             (fun label (cc,ident,comp) program ->
               let comp = translate comp in
               let clause_label = get_global_effect label in
               begin
                 let (operation_arity, bind_continuation) =
                   match cc with
                   | `Effect (k_ident'', arity) ->
                      let k_ident'' = identifier k_ident'' in
                      (arity, fun body ->           (* Bind operation arguments and the continuation *)
                              (llet k_ident''
                                    (multi_shot_k k_ident)
                                    body))
                   | `Exception arity
                   | `Regular arity -> (arity, fun x -> x)
                 in
                 let clause body =
                   let bind_args_in body = (* project operation arguments from the outer box, if any *)
                     if operation_arity > 0 then
                       let ident = identifier ident in
                       let args = lproject 1 (lvar eff) in
	               llet ident
		            args
		            body
	             else
	               body
                   in
                   bind_continuation (bind_args_in body)
                 in
                 lif (eq efflabel clause_label)
                     (clause comp)
                     program
               end)
             clauses default_clause
         in
         let clauses = compile_clauses clauses forward_effect in
         lfun [ eff ; cont ] (bind_k clauses)
       in
       let alloc_stack =
	 lalloc_stack
	   value_handler
	   exn_handler
	   eff_handler
       in
       let thunk =
	 let unitb = fresh_identifier "_unit" in
	 lfun [unitb] (lapply comp [lconst unit])
       in
       lresume [ alloc_stack
	       ; thunk
	       ; lconst unit]*)
    | v -> error ("Unimplemented feature:\n")
  and primitive : LLambda.primitive -> Lambda.lambda = function
    | `Nil -> lnil
    | `Builtin name -> builtin name
    | `FnApply (name, args) ->
       let args = List.map translate args in
       begin
         match name with
         | "Cons"    -> lcons (List.hd args) (List.tl args)
         | "Concat"  -> let concat = pervasives "@" in
                        lapply concat args
         | "not"     -> lprim Pnot args
         | "negate"  -> lprim Pnegint args
         | "negatef" -> lprim Pnegfloat args
         | _         -> let f = builtin name in
                        lapply f args
       end
    | `Print args ->
        let args = List.map translate args in
        lapply lprint args
    | `BinOp (binop, args) ->
       let args = List.map translate args in
       let prim =
         match binop with
         | `Plus, Some `Int  -> Paddint
         | `Minus, Some `Int -> Psubint
         | `Mult, Some `Int  -> Pmulint
         | `Div, Some `Int   -> Pdivint Unsafe
         | `Mod, _           -> Pmodint Unsafe
         | `Plus, Some `Float  -> Paddfloat
         | `Minus, Some `Float -> Psubfloat
         | `Mult, Some `Float  -> Pmulfloat
         | `Div, Some `Float  -> Pdivfloat
         | `Eq, None          -> Pccall (prim_binary_op "caml_equal")
         | `Neq, None          -> Pccall (prim_binary_op "caml_notequal")
         | `Lt, None          -> Pccall (prim_binary_op "caml_lessthan")
         | `Gt, None          -> Pccall (prim_binary_op "caml_greaterthan")
         | `Le, None          -> Pccall (prim_binary_op "caml_lessequal")
         | `Ge, None          -> Pccall (prim_binary_op "caml_greaterequal")
         | `And, _            -> Psequand
         | `Or,  _            -> Psequor
         | _ -> assert false
       in
       lprim prim args
    | `Global (ident, num, module_name, get_set) ->
       let ident = identifier ident in
       let module_name = lgetglobal module_name in
       begin
         match get_set with
         | `Set -> lprim (set_field_imm num) [module_name ; lvar ident]
         | `Get -> lprim (field num) [ module_name ]
       end
    (*| `SetOOId (module_name, effect_name) -> leffect (Printf.sprintf "%s.%s" module_name effect_name)*)
    | _ -> assert false
  and constant : LLambda.constant -> structured_constant =  function
    | `Int i      -> const_base int i
    | `Float f    -> const_base float f
    | `String s   -> string s
    | `Bool true  -> tt
    | `Bool false -> ff
    | `Char c     -> const_base char c
    | c -> error ("Unimplemented feature:\n" )
  in
  let exit_success = lconst ff in (* in this context "lconst ff" represents the exit code 0 *)
  translate prog
