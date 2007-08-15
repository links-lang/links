(*pp deriving *)
(** This is quite undesirable; but this datatype was shared between
    Syntax and SqlQuery so it had to be refactored. 
*)
(* Perhaps other AST (IR?) definitions could be brought down to this
   level to keep this company. *)
type comparison = [`Less | `LessEq | `Equal | `NotEq]
    deriving (Eq, Typeable, Show, Pickle, Shelve)

