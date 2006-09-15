(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Location
type type_fun = Types.t -> bool -> Types.t

let register op arity typ eval =
  Typer.register_op op arity typ;
  Eval.register_op op eval

let register_unary op typ eval =
  register op 1
    (function
       | [ tf ] -> 
	   typ tf
       | _ ->
	   raise (Typer.Error (
	     ("Built-in operator " ^ op ^ " needs exactly one argument")))
    )
    (function
       | [ v ] -> eval v
       | _ -> assert false
    )

let register_binary op typ eval =
  register op 2
    (function
       | [ tf1; tf2 ] -> 
	   typ tf1 tf2
       | _ ->
	   raise (Typer.Error (
	     ("Built-in operator " ^ op ^ " needs exactly two arguments")))
    )
    (function
       | [ v1; v2 ] -> eval v1 v2
       | _ -> assert false
    )

let register_cst op t v =
  register op 0
    (function
       | [ ] -> fun _ _  -> t
       | _ -> assert false)
    (function
       | [ ] -> v
       | _ -> assert false
    )

let register_fun op dom codom eval =
  register_cst op
    (Types.arrow (Types.cons dom) (Types.cons codom))
    (Value.Abstraction (Some [(dom,codom)],eval))

let register_fun2 op dom1 dom2 codom eval =
  let t2 = Types.arrow (Types.cons dom2) (Types.cons codom) in
  let iface2 = Some [(dom2,codom)] in
  register_cst op
    (Types.arrow (Types.cons dom1) (Types.cons t2))
    (Value.Abstraction (Some [(dom1,t2)],(fun v1 ->
					    Value.Abstraction (iface2,
							       eval v1))))
let register_op op ?(expect=Types.any) typ eval =
  register_unary op 
    (fun tf _ _ -> let t = tf expect true in typ t)
    eval

let register_op2 op t1 t2 s eval =
  register_binary op
    (fun tf1 tf2 _ _ -> ignore (tf1 t1 false); ignore (tf2 t2 false); s)
    eval
