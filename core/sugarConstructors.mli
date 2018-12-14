open Sugartypes

type ppos = SourceCode.lexpos * SourceCode.lexpos
val dummy_ppos : ppos

val pos : ppos -> position
val with_pos : ppos -> 'a -> 'a with_pos

val make_record : ppos -> (name * phrase) list -> phrase

(* Make bindings *)
type name_or_pat = Name of name with_pos | Pat of pattern
type signature_opt = Sig of (name with_pos * datatype') with_pos | NoSig

val make_fun_binding : signature_opt -> ppos
                    -> (declared_linearity * name with_pos * pattern list list *
                        location * (binding list * phrase))
                    -> binding
val make_handler_binding : signature_opt -> ppos
                        -> (binder * handlerlit)
                        -> binding
val make_val_binding : signature_opt -> ppos
                    -> (name_or_pat * phrase * location)
                    -> binding

val make_db_insert : ppos -> phrase -> name list -> phrase -> ppos
                  -> string option -> phrase
val make_db_exps : ppos -> (name * phrase) list -> phrase
