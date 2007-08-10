(** Evaluation of programs and expressions *)

(** {2 Running programs} *)
val run_program : Result.environment -> Result.environment -> Syntax.program -> (Result.environment * Result.result)
val run_defs : Result.environment -> Result.environment -> Syntax.definition list -> Result.environment


(** {1 Continuation functions} 
    Exposing the continuation functions is temporary, I hope. *)

val apply_cont : Result.environment -> Result.continuation -> Result.result -> Result.result 

val apply_cont_safe : Result.environment -> Result.continuation -> Result.result -> Result.result 

(** {1 Some hacks for inter-module communication.} *)

val has_client_context : bool ref
val program_source : Syntax.program ref
