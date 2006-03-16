(* Evaluation of programs and expressions *)

val run_program : Sl_result.environment -> Sl_syntax.expression list -> (Sl_result.environment * Sl_result.result)



(* Temporary, I hope *)
val apply_cont : Sl_result.environment -> Sl_result.continuation -> Sl_result.result -> Sl_result.result 

val apply_cont_safe : Sl_result.environment -> Sl_result.continuation -> Sl_result.result -> Sl_result.result 

