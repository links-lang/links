open Sugartypes

type ppos = SourceCode.lexpos * SourceCode.lexpos
val dummy_ppos : ppos

val pos : ppos -> position
val with_pos : ppos -> 'a -> 'a with_pos

val make_record : ppos -> (name * phrase) list -> phrase
(* JSTOLAREK: ugh *)
val annotate : ppos -> (name with_pos * datatype') -> ppos ->
               [< `Handler of (name * Types.datatype option) with_pos * handlerlit * 'a
               | `Var of name with_pos * phrase * location ] ->  binding

val make_fun : (ppos * (string with_pos * datatype')) option -> ppos
            -> (string with_pos * declared_linearity * funlit * location)
            -> binding
val make_db_insert : ppos -> phrase -> name list -> phrase -> ppos
                  -> string option -> phrase
val make_db_exps : ppos -> (name * phrase) list -> phrase
