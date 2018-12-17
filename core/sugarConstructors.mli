open Sugartypes

type ppos = SourceCode.lexpos * SourceCode.lexpos
val dummy_ppos : ppos

val pos : ppos -> position
val with_pos : ppos -> 'a -> 'a with_pos

val make_record : ppos -> (name * phrase) list -> phrase

(* Make bindings *)
type name_or_pat = Name of name with_pos | Pat of pattern
type signature = Sig of (name with_pos * datatype') with_pos | NoSig
val sig_of_opt : (name with_pos * datatype') with_pos option -> signature

val make_fun_binding : signature -> ppos
                    -> (declared_linearity * name with_pos * pattern list list *
                        location * (binding list * phrase))
                    -> binding
val make_unl_fun_binding : signature -> ppos
                        -> (name with_pos * pattern list list *
                            (binding list * phrase))
                        -> binding
val make_lin_fun_binding : signature -> ppos
                        -> (name with_pos * pattern list list *
                            (binding list * phrase))
                        -> binding
val make_handler_binding : signature -> ppos
                        -> (name with_pos * handlerlit)
                        -> binding
val make_val_binding : signature -> ppos
                    -> (name_or_pat * phrase * location)
                    -> binding

val make_hnlit : [`Deep | `Shallow ] -> pattern
              -> clause list * pattern list list option
              -> handlerlit

val make_db_insert : ppos -> phrase -> name list -> phrase
                  -> string with_pos option -> phrase
val make_db_exps : ppos -> (name * phrase) list -> phrase

val make_spawn : ppos -> spawn_kind -> given_spawn_location
              -> binding list * phrase
              -> phrase
