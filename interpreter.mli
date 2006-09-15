(* Evaluation of programs and expressions *)

val run_program : Result.environment -> Syntax.expression list -> (Result.environment * Result.result)



(* Temporary, I hope *)
val apply_cont : Result.environment -> Result.continuation -> Result.result -> Result.result 

val apply_cont_safe : Result.environment -> Result.continuation -> Result.result -> Result.result 

