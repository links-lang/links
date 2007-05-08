(* Evaluation of programs and expressions *)

val run_program : Result.environment -> Result.environment -> Syntax.program -> (Result.environment * Result.result)
val run_defs : Result.environment -> Result.environment -> Syntax.definition list -> Result.environment


(* Temporary, I hope *)
val apply_cont : Result.environment -> Result.continuation -> Result.result -> Result.result 

val apply_cont_safe : Result.environment -> Result.continuation -> Result.result -> Result.result 

val has_client_context : bool ref
val program_source : Syntax.program ref
