(** Temporary interface that wires the front end together *)

type 'a t = string -> 'a

(* Grammar for interactive shell *)
val interactive : TypeSugar.Typed.sentence t

(* Grammar for programs stored in files etc. *)
val file : (TypeSugar.Typed.binding list * TypeSugar.Typed.phrase option) t

(* Print the inferred type of an expression *)
val show_expr_type : string -> unit
