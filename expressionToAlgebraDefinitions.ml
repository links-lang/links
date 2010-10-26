module A = Algebra
module ADag = Algebra_dag

module AEnv = Env.Int

module rec Ts : (Components.S with type key = int and type value = Ti.tblinfo) =
struct
  include Components.Make(struct 
			    include Components.Default 
			    type key = int
			    type value = Ti.tblinfo
			  end)
end

and Vs : (Components.S with type key = (int * string) and type value = Ti.tblinfo * Cs.implementation_type) =
struct
  include Components.Make(struct
			    type key = int * string
			    type value = Ti.tblinfo * Cs.implementation_type
			    let incr = fun (col, tag) i -> ((col + i), tag)
			    let decr = fun (col, tag) i -> ((col - i), tag)
			    let col = fst
			  end)
end

and Fs : (Components.S with type key = int and type value = Ti.fundev list) =
struct
  include Components.Make(struct
			    include Components.Default
			    type key = int
			    type value = Ti.fundev list
			  end)
end

and Ti : 
sig
  type tblinfo = { q : ADag.t; cs : Cs.t; ts : Ts.t; vs : Vs.t ; fs : Fs.t }
  type error_plan = ADag.t option

  type aenv = tblinfo AEnv.t
  type fundev = (aenv * ADag.t * (Var.var list * Query2.Annotate.typed_t))

end
= 
struct

  type tblinfo = { q : ADag.t; cs : Cs.t; ts : Ts.t; vs : Vs.t; fs : Fs.t}
  type error_plan = ADag.t option

  type aenv = tblinfo AEnv.t
  type fundev = (aenv * ADag.t * (Var.var list * Query2.Annotate.typed_t))
end

include Ti

(* Naming conventions for Iter and Pos columns. Use only these names! *)
let iter = A.Iter 0 
let iter' = A.Iter 1
let inner = A.Iter 2
let outer = A.Iter 3 
let pos = A.Pos 0 
let pos' = A.Pos 1
let ord = A.Pos 2
let ord' = A.Pos 3
let item' = A.Pos 4
let item'' = A.Pos 5
let grp_key = A.Pos 6
let c' = A.Pos 7
let res = A.Pos 8
let res' = A.Pos 9
let res'' = A.Pos 10
let pos'' = A.Pos 11
