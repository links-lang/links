(*pp deriving *)
module Types = Types_links
module Env = Env_links
open Utility
open Ir
open PP

(* Core lambda calculus representation *)       
type clambda =
  | Apply of clambda * clambda list
  | Fun of string list * clambda
  | Rec of (string * string list * clambda) list * clambda
  | Primitive of string
  | Let of string * clambda * clambda
  | Var of string
  | String of string
  deriving (Show)		


(*module type IRTRANSLATOR = sig
  type t
  type env = int Env.String.t
  val computation :  env -> Ir.computation -> t
  val tail_computation : env -> Ir.tail_computation -> t
  val value : env -> Ir.value -> t
end
       
module OCaml_of : (IRTRANSLATOR with type t = string) = struct
  type t = string
	     
  let rec tail_computation env = function
    | `Return v -> failwith ""
    | `Apply (v, vs) -> failwith ""
    | `Special _ -> failwith ""
    | `Case (v, clauses, default) -> failwith ""
    | `If  (v, t1, t2) -> failwith ""
  and computation env (b,tc) =
    tail_computation env tc
  and value env = function
    | `Constant c -> failwith ""                (* constant: c *)
    | `Variable var -> failwith ""              (* variable use: x *)
    | `Extend (r, rho) -> failwith ""           (* record extension: (l1=v1, ..., lk=vk|r) or (l1=v1, ..., lk=vk) *)
    | `Project (l, r) -> failwith ""            (* record projection: r.l *)
    | `Erase (ls,r)   -> failwith ""                (* erase fields from a record: r\{ls} *)
    | `Inject (lof name * value * Types.datatype  (* variant injection: L(v) *)

    | `TAbs of tyvar list * value       (* type abstraction: /\xs.v *)
    | `TApp of value * tyarg list       (* type application: v ts *)

    | `XmlNode of name * value name_map * value list
    (* XML node construction: <tag attributes>body</tag> *)
    | `ApplyPure of value * value list  (* non-side-effecting application: v ws *)

    | `Closure of var * value           (* closure creation: f env *)

    | `Coerce of value * Types.datatype (* type coercion: v:A *)
end*)

(*module type READER = sig
  type env

  val pure : 'a -> env -> 'a
  val bind : (env -> 'a) -> ('a -> env -> 'b) -> env -> 'b
  val ask  : env -> env
  val local : (env -> env) -> (env -> 'a) -> env -> 'a
end
  
module Reader : (READER with type env = int list) = struct
  type env = int list
  type 'a m = env -> 'a

  let pure t _ = t
  let bind r f e = f (r e) e
  let ask e = e
  let local f r e = r (f e)
end *)

(*module type IRTRANSLATOR = sig
  type t
  type env 

  val computation : env -> Ir.computation -> t
  val tail_computation : env -> Ir.tail_computation -> t
  val value       : env -> Ir.value -> t
  val binding     : env -> Ir.binding -> t
  val special     : env -> Ir.special -> t
  val fun_def     : env -> Ir.fun_def -> t
  val constant    : constant -> t
end *)

(*module IrTranslator : IRTRANSLATOR with type t = clambda and type env = string Env.Int.t = struct
  type t = clambda
  type env = string Env.Int.t
		 
  let rec computation env (bs, tc) =
    (* TODO: process binders (bs) *)
    tail_computation env tc
  and tail_computation env = function
    | `Apply (v, vs) -> Apply (value env v, List.map (value env) vs)
    | _ -> failwith "tail_computation: Case not yet implemented."
  and value env = function
    | `TApp (v, _) -> value env v
    | `TAbs (_, v) -> value env v
    | `Variable (var : Var.var) ->
       let module Show_IntStringEnv = Env.Int.Show_t(Deriving_Show.Show_string) in
       let name =
	 match Env.Int.find env var with
	 | None -> failwith "Could not find variable name!"
	 | Some n -> n	
       in
       if Lib.is_primitive_var var
       then Primitive name
       else Var name
    | `Constant c -> constant c
    | _ -> failwith "value: Case not yet implemented"
  and binding env = function
    | _ -> failwith "binding: Case not yet implemented"
  and special env = function
    | _ -> failwith "special: Case not yet implemented"
  and fun_def env = function
    | _ -> failwith "fun_def: Case not yet implemented"
  and constant = function
    | `String s -> String s
    | _ -> failwith "constant: Case not yet implemented"
end*)  

module type IRTRANSLATOR = sig
  type t 
  type env
	 
  class translator : env ->
		     object ('self)
		       val env : env
		       method computation : Ir.computation -> t
		       method tail_computation : Ir.tail_computation -> t
		       method value : Ir.value -> t
		       method binding : Ir.binding -> t
		       method special : Ir.special -> t
		       method fun_def : Ir.fun_def -> t
		       method constant : Ir.constant -> t
		     end
end

let make_var_name v n = 
  let name = 
    (if n = "" 
    then "v"
    else "_"^n) ^ "_" ^ (string_of_int v)
  in 
  name
   			     
(*module IrTranslator : IRTRANSLATOR with type t = clambda and type env = string Env.Int.t = struct*)
module IrTranslator = struct
  type t = clambda
  type env = string Env.Int.t

  class translator (env : env) =
  object (o : 'self)

    val env = env

    method lookup : Var.var -> string option
      = fun var -> Env.Int.find env var

    method add_bindings : binder list -> 'self_type = fun bs ->
	  let env = List.fold_left 
              (fun e (v, (_, n, _)) -> Env.Int.bind e (v, (make_var_name v n))) env bs in
          {< env=env >}
		    
    method computation : Ir.computation -> t
      = fun (bs,tc) ->
      List.map o#binding bs;
      o#tail_computation tc
			 
    method tail_computation : Ir.tail_computation -> t = function
      | `Apply (v, vs) -> Apply (o#value v, List.map (o#value) vs)
      | _ -> failwith "tail_computation: Case not yet implemented."
		      
    method value : Ir.value -> t = function
	 | `TApp (v, _) -> o#value v
	 | `TAbs (_, v) -> o#value v
	 | `Variable (var : Var.var) ->
	    let module Show_IntStringEnv = Env.Int.Show_t(Deriving_Show.Show_string) in
	    let name =
	      match o#lookup var with
	      | None -> failwith "Could not find variable name!"
	      | Some name -> name
	    in	      
	    if Lib.is_primitive_var var
	    then Primitive name
	    else Var name
	 | `Constant c -> o#constant c
	 | _ -> failwith "value: Case not yet implemented"

    method bindings : Ir.binding list -> ('self -> t) -> t
      = fun bs f ->
       match bs with
       |  [] -> f o
       | (b :: bs) -> o#binding b (fun o' -> o'#bindings bs f)

    method binder : Ir.binder -> string
      = fun (v, (_, name, _)) -> make_var_name v name
		    
    method binding : Ir.binding -> ('self -> t) -> t
      =	fun b f ->
	  match b with
          | `Let (x, (_, tc)) -> 
              let o' = o#add_bindings [x] in
              Let (o#binder x, o#tail_computation tc, f o')
          | `Fun g -> o#binding (`Rec [g]) f
	  | `Rec funs ->
	     let fst4 (x,_,_,_) = x in
	     let names = List.map fst4 funs in
	     let o' = o#add_bindings (List.map fst4 funs) in
	     Rec (
		 List.map (
		     fun (binder, (_, f_binders, comp), _, _) ->
		     let args = List.map o'#binder f_binders in
		     let o'' = o'#add_bindings f_binders in
		     (o''#binder binder, 
		      args, 
		      o''#computation comp)) funs,
		 f o')
      | _ -> failwith "binding: Case not yet implemented"
		      
    method special : Ir.special -> t = function
      | _ -> failwith "special: Case not yet implemented"
		      
    method fun_def : Ir.fun_def -> t = function
      | _ -> failwith "fun_def: Case not yet implemented"
		      
    method constant : Ir.constant -> t = function
      | `String s -> String s
      | _ -> failwith "constant: Case not yet implemented"
  end

  let translate env prog =
    let t = new translator env in
    t#computation prog    
end
			     
(*
([`Fun ((1790, ((String) ~> (), "myprint", `Global )),
         ([(6429, (`Unl , `Any ), `Row ...)],
           [(1789, (String, "", `Local ))],
           ([`Let ((1788, (String, "s", `Local )),
                    ([], `Return `Variable 1789))],
             `Apply (`TApp (`TAbs ([(6427, (`Unl , `Any ), `Row ...)],
                                    `TApp (`Variable 61,
                                            [`Row ({}, ..., false)])),
                             [`Row ({}, ..., false)]), [`Variable 1788]))),
         None, `Unknown )],
  `Apply (`TApp (`TAbs ([(6431, (`Unl , `Any ), `Row ...)],
                         `TApp (`Variable 1790, [`Row ({}, ..., false)])),
                  [`Row ({}, ..., false)]),
           [`Constant `String "Hello World!"]))

 *)			     

let string_of_environment env =
  let module Show_IntStringEnv = Env.Int.Show_t(Deriving_Show.Show_string) in
  Show_IntStringEnv.show env			   

let rec ocaml_of_clambda = function
  | Apply (f, args) ->
     let args =
       if args = []
       then text "l_unit"
       else doc_join (fun arg -> ocaml_of_clambda arg) args
     in
     parens (group (
		  nest 2 ((ocaml_of_clambda f) ^| args)))
  | Primitive "print" -> text "print_endline"
  | Primitive p -> failwith ("ocaml_of_clambda: Primitive function " ^ p ^ " is not yet supported!")
  | Var name -> text name
  | String s -> text ("\"" ^ (String.escaped s) ^ "\"")
  | _ -> failwith "ocaml_of_clambda: Case not yet implemented"
											     
let clambda_of_ir e c = IrTranslator.translate e c

let invert env =
  Env.String.fold
    (fun name var env ->
       if Env.Int.has env var then
         failwith ("(invert_env) duplicate variable in environment")
       else
         Env.Int.bind env (var, name))
    env Env.Int.empty

		
let ocaml_of_ir env bindings prog =
  print_endline "Dumping IR:";
  print_endline (Ir.Show_program.show prog);
  print_endline "=====================================================================";
  let lambda = clambda_of_ir (invert env) prog in
  print_endline (Show_clambda.show lambda);
  print_endline "=====================================================================";
  let ocaml = (pretty 100 (ocaml_of_clambda lambda)) in
  print_endline ocaml;
  print_endline "=====================================================================";
  print_endline "Writing to file out/a.ml";
  let () =
    let oc = open_out "out/a.ml" in
    Printf.fprintf oc "%s" ocaml;
    close_out oc;
  in
  print_endline "=====================================================================";
  print_endline "Invoking ocamlc on out/a.ml";
  Unix.system "/home/dhil/projects/ocaml-effects/local/bin/ocamlc /home/dhil/projects/links/out/a.ml -o /home/dhil/projects/links/out/a.out";
  print_endline "=====================================================================";		
  let v = Lambda.Pidentity in
  "Hello World"
