(*pp deriving *)
open Utility
open Sugartypes

type var_env =
    Types.meta_type_var StringMap.t *
      Types.meta_row_var StringMap.t 
      deriving (Show)

module Env = Env.String

let var_of_quantifier =
  function
    | `RigidTypeVar var
    | `TypeVar var
    | `RowVar var
    | `RigidRowVar var -> var

module Utils : sig
  val unify : Types.alias_environment -> Types.datatype * Types.datatype -> unit
  val instantiate : Types.environment -> string -> Types.datatype
  val generalise : Types.environment -> Types.datatype -> Types.datatype
  val register_alias : name * int list * Types.datatype -> Types.alias_environment -> Types.alias_environment

  val is_generalisable : phrase -> bool
end =
struct
  let unify = Unify.datatypes
  let instantiate = Instantiate.var
  let generalise = Generalise.generalise
  let register_alias = Types.register_alias

  let rec opt_generalisable o = opt_app is_generalisable true o
  and is_generalisable (p, _) = match p with
    | `Constant _
    | `Var _
    | `FunLit _
    | `TextNode _
    | `Section _ -> true

    | `ListLit ps
    | `TupleLit ps -> List.for_all is_generalisable ps
    | `Projection (p, _)
    | `TypeAnnotation (p, _)
    | `Upcast (p, _, _)
    | `Escape (_, p) -> is_generalisable p
    | `ConstructorLit (_, p) -> opt_generalisable p
    | `RecordLit (fields, p) ->
        List.for_all (snd ->- is_generalisable) fields && opt_generalisable p
    | `With (p, fields) ->
        List.for_all (snd ->- is_generalisable) fields && is_generalisable p
    | `Block (bindings, e) -> 
        List.for_all is_generalisable_binding bindings && is_generalisable e
    | `Conditional (p1, p2, p3) ->
        is_generalisable p1 
     && is_generalisable p2
     && is_generalisable p3 
    | `Xml (_, attrs, attrexp, children) -> 
        List.for_all (snd ->- List.for_all is_generalisable) attrs
     && opt_generalisable attrexp
     && List.for_all (is_generalisable) children
    | `Formlet (p1, p2) ->
        is_generalisable p1 && is_generalisable p2
    | `Regex r -> is_generalisable_regex r
    | `Iteration _ (* could do a little better in some of these cases *)
    | `Page _
    | `FormletPlacement _
    | `PagePlacement _
    | `UnaryAppl _
    | `FormBinding _
    | `InfixAppl _
    | `Spawn _
    | `SpawnWait _
    | `FnAppl _
    | `Switch _
    | `Receive _
    | `DatabaseLit _
    | `TableLit _
    | `DBDelete _
    | `DBInsert _
    | `DBUpdate _ -> false
  and is_generalisable_binding (bind, _ : binding) = match bind with
      (* need to check that pattern matching cannot fail *) 
    | `Fun _
    | `Funs _
    | `Infix
    | `Type _
    | `Include _
    | `Foreign _ -> true
    | `Exp p -> is_generalisable p
    | `Val (pat, rhs, _, _) ->
        is_safe_pattern pat && is_generalisable rhs
  and is_safe_pattern (pat, _) = match pat with
      (* safe patterns cannot fail *)
    | `Nil 
    | `Cons _
    | `List _ 
    | `Constant _ -> false
    (* NOTE: variant assigment is typed such that it must always succeed *)
    | `Variant (_, None) -> true
    | `Variant (_, Some p) -> is_safe_pattern p
    | `Negative _ -> true
    | `Any
    | `Variable _ -> true
    | `Record (ps, None) -> List.for_all (snd ->- is_safe_pattern) ps
    | `Record (ps, Some p) -> List.for_all (snd ->- is_safe_pattern) ps && is_safe_pattern p
    | `Tuple ps -> List.for_all is_safe_pattern ps
    | `HasType (p, _)
    | `As (_, p) -> is_safe_pattern p
  and is_generalisable_regex = function 
      (* don't check whether it can fail; just check whether it
         contains non-generilisable sub-expressions *)
    | `Range _
    | `Simply _
    | `Any
    | `StartAnchor
    | `EndAnchor -> true
    | `Group r
    | `Repeat (_, r)
    | `Quote r -> is_generalisable_regex r
    | `Seq rs -> List.for_all is_generalisable_regex rs
    | `Alternate (r1, r2) -> is_generalisable_regex r1 && is_generalisable_regex r2
    | `Splice p -> is_generalisable p
    | `Replace (r, `Literal _) -> is_generalisable_regex r
    | `Replace (r, `Splice p) -> is_generalisable_regex r && is_generalisable p
end

module Errors :
sig
  type griper = 
      pos:Syntax.position ->
  t1:(string * Types.datatype) ->
  t2:(string * Types.datatype) ->
  aliases:Types.alias_environment ->
  error:Unify.error ->
  unit

  val die : Syntax.position -> string -> 'a

  val if_condition : griper
  val if_branches  : griper

  val switch_pattern : griper
  val switch_patterns : griper
  val switch_branches : griper

  val extend_record : griper
  val record_with : griper

  val list_lit : griper

  val table_name : griper
  val table_db : griper

  val delete_table : griper
  val delete_pattern : griper
  val delete_where : griper
  val insert_table : griper
  val insert_values : griper
  val update_table : griper
  val update_pattern : griper
  val update_where : griper
  val update_write : griper

  val spawn_process : griper
  val spawn_wait_process : griper
  val spawn_wait_return : griper

  val receive_mailbox : griper
  val receive_patterns : griper

  val unary_apply : griper
  val infix_apply : griper
  val fun_apply : griper

  val xml_attribute : griper
  val xml_attributes : griper
  val xml_child : griper

  val formlet_body : griper
  val page_body : griper

  val render_formlet : griper
  val render_handler : griper
  val render_attributes : griper

  val page_placement : griper

  val form_binding_body : griper
  val form_binding_pattern : griper

  val iteration_list_body : griper
  val iteration_list_pattern : griper
  val iteration_table_body : griper
  val iteration_table_pattern : griper
  val iteration_body : griper
  val iteration_where : griper

  val escape : griper

  val projection : griper

  val upcast_source : griper
  val upcast_subtype : Syntax.position -> Types.datatype -> Types.datatype -> 'a

  val type_annotation : griper

  val bind_val : griper
  val bind_val_annotation : griper
  val bind_fun_annotation : griper
  val bind_rec_annotation : griper
  val bind_rec_rec : griper
  val bind_exp : griper

  val list_pattern : griper
  val cons_pattern : griper
  val record_pattern : griper
  val pattern_annotation : griper
end
  = struct
    type griper = 
        pos:Syntax.position ->
      t1:(string * Types.datatype) ->
      t2:(string * Types.datatype) ->
      aliases:Types.alias_environment ->
      error:Unify.error ->
      unit

    let wm () = Settings.get_value Basicsettings.web_mode

    let code s =
      if wm() then
        "<code>" ^ s ^ "</code>"
      else
        match getenv "TERM" with
          | Some ("rxvt"|"aterm"|"gnome-terminal"|"xterm") ->
              Printf.sprintf  "`\x1b[1;5;%dm" (30 + Random.int 8) ^ s ^ "\x1b[0m'"
          | _ -> ("`"^ s ^ "'" )
      
    let nl () =
      if wm() then
        "<br />\n"
      else
        "\n"

    let tab () =
      if wm() then
        "&nbsp;&nbsp;&nbsp;&nbsp;"
      else
        "    "

    let show_type = Types.string_of_datatype

    let die pos msg = raise (Errors.Type_error (pos, msg))

    let but (expr, t) =
", but the expression" ^ nl() ^
tab() ^ code expr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type t) ^ "."

    let but2things (lthing, (lexpr, lt)) (rthing, (rexpr, rt)) =
", but the " ^ lthing ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the " ^ rthing ^ nl() ^
tab() ^ code rexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ (show_type rt) ^ "."

    let but2 l r = but2things ("expression", l) ("expression", r)

    let with_but pos s et =
      die pos (s ^ but et)

    let with_but2 pos s l r =
      die pos (s ^ but2 l r)

    let with_but2things pos s l r =
      die pos (s ^ but2things l r)

    let fixed_type pos thing t l =
      with_but pos (thing ^ " must have type " ^ code (show_type t)) l

    let if_condition ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ = 
      fixed_type pos ("The condition of an " ^ code "if (...) ... else ..." ^ " expression") t l

    let if_branches ~pos ~t1:l ~t2:r ~aliases:_ ~error:_ =
      with_but2 pos ("Both branches of an " ^ code "if (...) ... else ..." ^
                       " expression should have the same type") l r

    let switch_pattern ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The type of an input to a switch should match the type of its patterns, but
the expression" ^ nl () ^
tab () ^ code lexpr ^ nl () ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the patterns have type" ^ nl () ^
tab () ^ code (show_type rt))

    let switch_patterns ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
All the cases of a switch should have compatible patterns, but
the pattern" ^ nl () ^
tab () ^ code lexpr ^ nl () ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the subsequent patterns have type" ^ nl () ^
tab () ^ code (show_type rt))

    let switch_branches ~pos ~t1:(lexpr, lt) ~t2:(_, rt) ~aliases:_ ~error:_ =
      die pos ("\
All the cases of a switch should have the same type, but
the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the subsequent expressions have type" ^ nl() ^
tab() ^ (show_type rt))

    (* [BUG] This griper is a bit rubbish because it doesn't distinguish
    between two different errors. *)
    let extend_record ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~aliases:_ ~error:_ =
      die pos ("\
Only a record can be extended, and it must be extended with different fields, but
the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the extension fields have type" ^ code (show_type t) ^ ".")

    let record_with ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~aliases:_ ~error:_ =
      die pos ("\
A record can only be updated with compatible fields, but
the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the update fields have type" ^ code (show_type t) ^ ".")

    let list_lit ~pos ~t1:l ~t2:r ~aliases:_ ~error:_ =
      with_but2 pos "All elements of a list literal must have the same type" l r

    let table_name ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Table names" t l

    let table_db ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Databases" t l

    let delete_table ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Tables" t l

    let delete_where ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Where clauses" t l

    let delete_pattern ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The binder must match the table in a delete generator, \
but the pattern" ^ nl() ^
tab () ^ code lexpr ^ nl () ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the read row has type" ^ nl () ^
tab () ^ code (show_type rt))

    let insert_table ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Tables" t l

    let insert_values ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The values must match the table in an insert expression,
but the values" ^ nl () ^
tab () ^ code lexpr ^ nl () ^
"have type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the write row has type" ^ nl () ^
tab () ^ code (show_type rt))

    let update_table ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Tables" t l

    let update_pattern ~pos ~t1:l ~t2:r ~aliases:_ ~error:_ =
      with_but2things pos
        "The binding must match the table in an update expression" ("pattern", l) ("row", r)

    let update_write ~pos ~t1:(_, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The fields must match the table in an update expression,
but the fields have type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the write row has type" ^ nl () ^
tab () ^ code (show_type rt))

    let update_where ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Where clauses" t l

    let spawn_process ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Processes" t l

    let spawn_wait_process ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Processes" t l

    let spawn_wait_return ~pos ~t1:_ ~t2:_ ~aliases:_ ~error:_ =
      (* this should never happen as the first argument is a fresh
         type variable *)
      assert false

    let receive_mailbox ~pos ~t1:(_, lt) ~t2:(_, rt) ~aliases:_ ~error:_ =
      die pos ("\
The current mailbox must always have a mailbox type" ^ nl() ^
code (show_type rt) ^ nl() ^
"but the current mailbox type is" ^ nl() ^
code (show_type lt) ^ ".")

    let receive_patterns ~pos ~t1:(_, lt) ~t2:(_, rt) ~aliases:_ ~error:_ =
      die pos ("\
The current mailbox type should match the type of the patterns in a receive, but
the current mailbox takes messages of type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the patterns have type" ^ nl () ^
tab () ^ code (show_type rt))

    let unary_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The unary operator" ^ nl() ^ 
tab () ^ code lexpr ^ nl() ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the argument passed to it has type" ^ nl() ^
tab () ^ code (show_type (List.hd (Types.arg_types rt))) ^ ".")

    let infix_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The infix operator" ^ nl() ^ 
tab () ^ code lexpr ^ nl() ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the arguments passed to it have types" ^ nl() ^
tab () ^ code (show_type (List.hd (Types.arg_types rt))) ^ nl() ^
"and" ^ nl() ^
tab () ^ code (show_type (List.hd (List.tl (Types.arg_types rt)))) ^ ".")

    let fun_apply ~pos ~t1:(lexpr, lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The function" ^ nl() ^ 
tab () ^ code lexpr ^ nl () ^
"has type" ^ nl () ^
tab () ^ code (show_type lt) ^ nl () ^
"while the arguments passed to it have types" ^ nl() ^
String.concat
(nl() ^ "and" ^ nl())
(List.map (fun t ->
             tab() ^ code (show_type t)) (Types.arg_types rt)) ^ ".")

    let xml_attribute ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "XML attributes" t l

    let xml_attributes ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "A list of XML attributes" t l

    let xml_child ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "XML child nodes" t l

    let formlet_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Formlet bodies" t l

    let page_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Page bodies" t l

    let render_formlet ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Formlets" t l

    let render_handler ~pos ~t1:l ~t2:r ~aliases:_ ~error:_ =
      with_but2 pos
        "The formlet must match its handler in a formlet placement" l r

    let render_attributes ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "A list of XML attributes" t l

    let page_placement ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Page antiquotes" t l

    let form_binding_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Formlets" t l

    let form_binding_pattern ~pos ~t1:l ~t2:(rexpr, rt) ~aliases:_ ~error:_ =
      let rt = Types.make_formlet_type rt in
        with_but2things pos
          ("The binding must match the formlet in a formlet binding") ("pattern", l) ("expression", (rexpr, rt))

    let iteration_list_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "The body of a list generator" t l
      
    let iteration_list_pattern ~pos ~t1:l ~t2:(rexpr,rt) ~aliases:_ ~error:_ =
      let rt = Types.make_list_type rt in
        with_but2things pos
          ("The binding must match the list in a list generator") ("pattern", l) ("expression", (rexpr, rt))

    let iteration_table_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "The body of a table generator" t l

    let iteration_table_pattern ~pos ~t1:l ~t2:(rexpr,rt) ~aliases:_ ~error:_ =
      let rt = Types.make_table_type (rt, Types.fresh_type_variable ()) in
        with_but2things pos
          ("The binding must match the table in a table generator") ("pattern", l) ("expression", (rexpr, rt))

    let iteration_body ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "The body of a for comprehension" t l

    let iteration_where ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Where clauses" t l

    let escape ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "The argument to escape" t l

    let projection ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~aliases:_ ~error:_ =
      die pos ("\
Only a field that is present in a record can be projected, but \
the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the projection has type" ^ code (show_type t) ^ ".")

    let upcast_source ~pos ~t1:(lexpr, lt) ~t2:(_,t) ~aliases:_ ~error:_ =
      die pos ("\
The source expression must match the source type of an upcast, but\
the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the source type is" ^ code (show_type t) ^ ".")

    let upcast_subtype pos t1 t2 =
      die pos ("\
An upcast must cast must be of the form" ^ code ("e : t2 <- t1") ^
"where " ^ code "t1" ^ " is a subtype of" ^ code "t2" ^ nl() ^
"but" ^ nl() ^
code (show_type t1) ^ nl() ^
"is not a subtype of" ^ nl() ^
code (show_type t2) ^ ".")

    let type_annotation ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The inferred type of the expression" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"is" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but it is annotated with type" ^ nl() ^
tab() ^ code (show_type rt))

    let bind_val ~pos ~t1:l ~t2:r ~aliases:_ ~error:_ =
      with_but2things pos
        ("The binder must match the body of a value binding") ("pattern", l) ("expression", r)

    let bind_val_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The value has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but it is annotated with type" ^ nl() ^
tab() ^ code (show_type rt))

    let bind_fun_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The non-recursive function definition has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but it is annotated with type" ^ nl() ^
tab() ^ code (show_type rt))

    let bind_rec_annotation ~pos ~t1:(_,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The recursive function definition has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but it is annotated with type" ^ nl() ^
tab() ^ code (show_type rt))

    let bind_rec_rec ~pos ~t1:(_,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
      die pos ("\
The recursive function definition has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but its previously inferred type is" ^ nl() ^
tab() ^ code (show_type rt))

    let bind_exp ~pos ~t1:l ~t2:(_,t) ~aliases:_ ~error:_ =
      fixed_type pos "Side-effect expressions" t l

    (* patterns *)
    let list_pattern ~pos ~t1:(lexpr,lt) ~t2:(rexpr,rt) ~aliases:_ ~error:_ =
      die pos ("\
All elements in a list pattern must have the same type, but the pattern" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"while the pattern" ^ nl() ^
tab() ^ code rexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ (show_type rt))

    let cons_pattern ~pos ~t1:(lexpr,lt) ~t2:(rexpr,rt) ~aliases:_ ~error:_ =
      die pos ("\
The two subpatterns of a cons pattern " ^ code "p1::p2" ^ " must have compatible types:\
if " ^ code "p1" ^ " has type " ^ code "t'" ^ " then " ^ code "p2" ^ " must have type " ^ code "[t]" ^ ".\
However, the pattern" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"whereas the pattern" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"has type" ^ nl() ^
tab() ^ code (show_type rt))

    let record_pattern ~pos:(_,_,expr as pos) ~t1:(_lexpr,_lt) ~t2:(_rexpr,_rt) ~aliases:_ ~error =
      match error with
        | `PresentAbsentClash (label, _, _) ->
            (* NB: is it certain that this is what's happened? *)
          die pos ("\
Duplicate labels are not allowed in record patterns.  However, the pattern" ^ nl() ^
tab() ^ code expr ^ nl() ^
"contains more than one binding for the label " ^ nl() ^
tab() ^ code label)
      | `Msg msg -> raise (Errors.Type_error (pos, msg))

  let pattern_annotation ~pos ~t1:(lexpr,lt) ~t2:(_,rt) ~aliases:_ ~error:_ =
    die pos ("\
The inferred type of the pattern" ^ nl() ^
tab() ^ code lexpr ^ nl() ^
"is" ^ nl() ^
tab() ^ code (show_type lt) ^ nl() ^
"but it is annotated with type" ^ nl() ^
tab() ^ code (show_type rt))

end
let mailbox = "_MAILBOX_"
let mailbox_type env = Utils.instantiate env mailbox

let constant_type = function
  | `Float _  -> `Primitive `Float
  | `Int _    -> `Primitive `Int
  | `Bool _   -> `Primitive `Bool
  | `Char _   -> `Primitive `Char
  | `String _ ->  Types.string_type

let type_section env (`Section s as s') = s', match s with
    `Minus         -> Utils.instantiate env "-"
  | `FloatMinus    -> Utils.instantiate env "-."
  | `Project label ->
      let f = Types.fresh_type_variable () in
      let r = `Record (Types.make_singleton_open_row (label, `Present f)) in
        `Function (Types.make_tuple_type [r], mailbox_type env, f)
  | `Name var      -> Utils.instantiate env var

let datatype = DesugarDatatype.read_datatype

let type_unary_op env = function
  | `Minus      -> datatype "(Int) -> Int"
  | `FloatMinus -> datatype "(Float) -> Float"
  | `Name n     -> Utils.instantiate env n
  | `Abs        -> 
      let mb = Types.fresh_type_variable ()
      and mb2 = Types.fresh_type_variable ()
      and rv = Types.fresh_type_variable ()
      and arg = Types.fresh_type_variable () in
        `Function (Types.make_tuple_type [
                     `Function (Types.make_tuple_type [arg], mb, rv)
                   ], mb2,
                   `Function (arg, mb, rv))

let type_binary_op env = function
  | `Minus        -> datatype "(Int,Int) -> Int"
  | `FloatMinus   -> datatype "(Float,Float) -> Float"
  | `RegexMatch flags -> 
      let nativep  = List.exists ((=) `RegexNative)  flags
      and listp    = List.exists ((=) `RegexList)    flags 
      and replacep = List.exists ((=) `RegexReplace) flags in
        (match replacep, listp, nativep with
           | true,   _   , true  -> (* sntilde *) datatype "(NativeString, Regex) -> NativeString"
           | true,   _   , false -> (* stilde  *) datatype "(String, Regex) -> String"
           | false, true , true  -> (* lntilde *) datatype "(NativeString, Regex) -> [String]"
           | false, true , false -> (* ltilde *)  datatype "(String, Regex) -> [String]"
           | false, false, true  -> (* ntilde *)  datatype "(NativeString, Regex) -> Bool"
           | false, false, false -> (* tilde *)   datatype "(String, Regex) -> Bool")
  | `And
  | `Or           -> datatype "(Bool,Bool) -> Bool"
  | `Cons         -> Utils.instantiate env "Cons"
  | `Name "++"    -> Utils.instantiate env "Concat"
  | `Name ">"
  | `Name ">="
  | `Name "=="
  | `Name "<"
  | `Name "<="
  | `Name "<>"    ->
      let a = Types.fresh_type_variable ()
      and mb = Types.fresh_type_variable () in
        `Function (Types.make_tuple_type [a; a], mb, `Primitive `Bool);
  | `Name "!"     -> Utils.instantiate env "send"
  | `Name n       -> Utils.instantiate env n
  | `App          -> 
      let tup = `Record (Types.make_empty_open_row ())
      and mb = Types.fresh_type_variable ()
      and mb2 = Types.fresh_type_variable ()
      and rv = Types.fresh_type_variable () in
        `Function (Types.make_tuple_type [
                     `Function (tup, mb, rv);
                     tup],
                   mb2, rv)

(** close a pattern type relative to a list of patterns

   If there are no _ or variable patterns at a variant type, then that
   variant will be closed.
*)
let rec close_pattern_type : pattern list -> Types.datatype -> Types.datatype = fun pats t ->
  let cpt : pattern list -> Types.datatype -> Types.datatype = close_pattern_type in
    match t with
      | `Record row when Types.is_tuple row->
          let fields, row_var = fst (Types.unwrap_row row) in
          let rec unwrap_at i p =
            match fst p with
              | `Variable _ | `Any | `Constant _ -> p
              | `As (_, p) | `HasType (p, _) -> unwrap_at i p
              | `Tuple ps ->
                  List.nth ps i
              | `Nil | `Cons _ | `List _ | `Record _ | `Variant _ | `Negative _ -> assert false in
          let fields =
            StringMap.fold
              (fun name ->
                 function
                   | `Present t ->
                       let pats = List.map (unwrap_at ((int_of_string name) - 1)) pats in
                         StringMap.add name (`Present (cpt pats t))
                   | `Absent ->
                       assert false) fields StringMap.empty in
            `Record (fields, row_var)
      | `Record row ->
          let fields, row_var = fst (Types.unwrap_row row) in
          let rec unwrap_at name p =
            match fst p with
              | `Variable _ | `Any | `Constant _ -> p
              | `As (_, p) | `HasType (p, _) -> unwrap_at name p
              | `Record (ps, default) ->
                  if List.mem_assoc name ps then
                    List.assoc name ps
                  else
                    begin
                      match default with
                        | None -> assert false
                        | Some p -> unwrap_at name p
                    end
              | `Nil | `Cons _ | `List _ | `Tuple _ | `Variant _ | `Negative _ -> assert false in
          let fields =
            StringMap.fold
              (fun name ->
                 function
                   | `Present t ->
                       let pats = List.map (unwrap_at name) pats in
                         StringMap.add name (`Present (cpt pats t))
                   |  `Absent ->
                        assert false) fields StringMap.empty in
            `Record (fields, row_var)
      | `Variant row ->
          let fields, row_var = fst (Types.unwrap_row row) in
          let end_pos p =
            let _, (_, end_pos, buf) = p in
              (*
                QUESTION:
                
                This indicates the position immediately after the pattern.
                How can we indicate a 0-length position in an error message?
              *)
              (end_pos, end_pos, buf) in
            
          let rec unwrap_at : string -> pattern -> pattern list = fun name p ->
            match fst p with
              | `Variable _ | `Any -> [ `Any, end_pos p ]
              | `As (_, p) | `HasType (p, _) -> unwrap_at name p
              | `Variant (name', None) when name=name' ->
                    [(`Record ([], None), end_pos p)]
              | `Variant (name', Some p) when name=name' -> [p]
              | `Variant _ -> []
              | `Negative names when List.mem name names -> []
              | `Negative _ -> [ `Any, end_pos p ]
              | `Nil | `Cons _ | `List _ | `Tuple _ | `Record _ | `Constant _ -> assert false in
          let rec are_open : pattern list -> bool =
            function
              | [] -> false
              | ((`Variable _ | `Any | `Negative _), _) :: _ -> true
              | ((`As (_, p) | `HasType (p, _)), _) :: ps -> are_open (p :: ps)
              | ((`Variant _), _) :: ps -> are_open ps
              | ((`Nil | `Cons _ | `List _ | `Tuple _ | `Record _ | `Constant _), _) :: _ -> assert false in
          let fields =
            StringMap.fold
              (fun name field_spec env ->
                 match field_spec with
                   | `Present t ->
                       let pats = concat_map (unwrap_at name) pats in
                       let t = cpt pats t in
                         (StringMap.add name (`Present t)) env
                   | `Absent ->
                       assert false) fields StringMap.empty
          in
            if are_open pats then
              begin
                let row = (fields, row_var) in
                  (* NOTE: type annotations can lead to a closed type even though the patterns are open *)
(*
                  if Types.is_closed_row row then
                    failwith ("Open row pattern with closed type: "^Types.string_of_row row)
                  else
*)
                    `Variant row
              end
            else
              begin
                match Unionfind.find row_var with
                  | `Flexible _ | `Rigid _ -> `Variant (fields, Unionfind.fresh `Closed)
                  | `Recursive _ | `Body _ | `Closed -> assert false
              end
      | `Application ("List", [t]) ->
          let rec unwrap p : pattern list =
            match fst p with
              | `Variable _ | `Any -> [p]
              | `Constant _ | `Nil -> []
              | `Cons (p1, p2) -> p1 :: unwrap p2 
              | `List ps -> ps
              | `As (_, p) | `HasType (p, _) -> unwrap p
              | `Variant _ | `Negative _ | `Record _ | `Tuple _ -> assert false in
          let pats = concat_map unwrap pats in
            `Application ("List", [cpt pats t])
      | `ForAll (qs, t) -> `ForAll (qs, cpt pats t)
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t -> cpt pats t
              | `Flexible _ | `Rigid _ -> t
              | `Recursive _ -> assert false
          end
      | `Not_typed
      | `Primitive _
      | `Function _
      | `Table _
       (* TODO: expand applications? *)
      | `Application _ -> t

let unify aliases ~pos ~(handle:Errors.griper) ((_,ltype as t1), (_,rtype as t2)) =
  try
    Utils.unify aliases (ltype, rtype)
  with Unify.Failure error -> handle ~pos ~t1 ~t2 ~aliases ~error

type context = {
  (* mapping from type variable names to type variables *)
  tvars : Types.meta_type_var StringMap.t;
  (* mapping from row variable names to row variables *)
  rvars : Types.meta_row_var StringMap.t; 
  (* mapping from variables to type schemes
     and from typenames to types *)
  tenv : Types.typing_environment;
}

let lookup_pos =
  function
    | (start, finish, Some source_code) -> source_code#lookup(start, finish)
    | _ -> Syntax.dummy_position

let type_pattern closed alias_env tenvs : pattern -> pattern * Types.environment * Types.datatype =
  let make_singleton_row =
    match closed with
      | `Closed -> Types.make_singleton_closed_row
      | `Open -> Types.make_singleton_open_row in

  let make_negative_row names =
    (List.fold_right
       (fun name fields ->
          StringMap.add name `Absent fields) names StringMap.empty,
     Types.fresh_row_variable()) in

  (* type_pattern p types the pattern p returning a typed pattern, a
     type environment for the variables bound by the pattern and two
     types. The first type is the type of the pattern 'viewed from the
     outside' - in the case of variant patterns it must be open in
     order to allow cases to unify. The second type is the type of the
     pattern 'viewed from the inside'. Type annotations are only
     applied to the inner pattern. The type environment is constructed
     using types from the inner type.

  *)
  let rec type_pattern (pattern, pos' : pattern) : pattern * Types.environment * (Types.datatype * Types.datatype) =
    let _UNKNOWN_POS_ = "<unknown>" in
    let tp = type_pattern in
    let unify (l, r) = unify alias_env ~pos:(lookup_pos pos') (l, r)
    and erase (p,_, _) = p
    and ot (_,_,(t,_)) = t
    and it (_,_,(_,t)) = t
    and env (_,e,_) = e
    and pos ((_,p),_,_) = let (_,_,p) = lookup_pos p in p
                                                          (* TODO: check for duplicate bindings *)
    and (++) = Env.extend in
    let (p, env, (outer_type, inner_type)) :
           patternnode * Types.environment * (Types.datatype * Types.datatype) =
      match pattern with
        | `Any ->
            let t = Types.fresh_type_variable () in
              `Any, Env.empty, (t, t)
        | `Nil ->
            let t = Types.make_list_type (Types.fresh_type_variable ()) in
              `Nil, Env.empty, (t, t)
        | `Constant c as c' ->
            let t = constant_type c in
              c', Env.empty, (t, t)
        | `Variable (x,_,pos) -> 
            let xtype = Types.fresh_type_variable () in
              (`Variable (x, Some xtype, pos),
               Env.bind Env.empty (x, xtype),
               (xtype, xtype))
        | `Cons (p1, p2) -> 
            let p1 = tp p1
            and p2 = tp p2 in
            let () = unify ~handle:Errors.cons_pattern ((pos p1, Types.make_list_type (ot p1)), 
                                                        (pos p2, ot p2)) in
            let () = unify ~handle:Errors.cons_pattern ((pos p1, Types.make_list_type (it p1)), 
                                                        (pos p2, it p2)) in
              `Cons (erase p1, erase p2), env p1 ++ env p2, (ot p2, it p2)
        | `List ps -> 
            let ps' = List.map tp ps in
            let env' = List.fold_right (env ->- (++)) ps' Env.empty in
            let list_type p ps typ =
              let _ = List.iter (fun p' -> unify ~handle:Errors.list_pattern ((pos p, typ p), 
                                                                              (pos p', typ p'))) ps
              in
                Types.make_list_type (typ p) in
            let ts =
              match ps' with
                | [] -> let t = Types.fresh_type_variable () in t, t
                | p::ps ->
                    list_type p ps ot, list_type p ps it
            in                            
              `List (List.map erase ps'), env', ts
        | `Variant (name, None) -> 
            let vtype () = `Variant (make_singleton_row (name, `Present Types.unit_type)) in
              `Variant (name, None), Env.empty, (vtype (), vtype ())
        | `Variant (name, Some p) -> 
            let p = tp p in
            let vtype typ = `Variant (make_singleton_row (name, `Present (typ p))) in
              `Variant (name, Some (erase p)), env p, (vtype ot, vtype it)
        | `Negative names ->
            let outer_type = `Variant (Types.make_empty_open_row ()) in
            let inner_type = `Variant (make_negative_row names) in
              `Negative names, Env.empty, (outer_type, inner_type)
        | `Record (ps, default) -> 
            let ps = alistmap tp ps
            and default = opt_map tp default in
            let initial_outer, initial_inner, denv =
              match default with
                | None ->
                    let row = Types.make_empty_closed_row () in
                      row, row, Env.empty
                | Some r ->
                    let make_row typ =
                      let row = 
                        List.fold_right
                          (fun (label, _) -> Types.row_with (label, `Absent))
                          ps (Types.make_empty_open_row ()) in
                      let () = unify ~handle:Errors.record_pattern (("", `Record row),
                                                                    (pos r, typ r))
                      in
                        row
                    in                      
                      make_row ot, make_row it, env r in
            let rtype typ initial =
              `Record (List.fold_right
                         (fun (l, f) -> Types.row_with (l, `Present (typ f)))
                         ps initial)
            and penv = 
              List.fold_right (snd ->- env ->- (++)) ps Env.empty
            in
              (`Record (alistmap erase ps, opt_map erase default),
               penv ++ denv,
               (rtype ot initial_outer, rtype it initial_outer))
        | `Tuple ps -> 
            let ps' = List.map tp ps in
            let env' = List.fold_right (env ->- (++)) ps' Env.empty in
            let make_tuple typ = Types.make_tuple_type (List.map typ ps') in
              `Tuple (List.map erase ps'), env', (make_tuple ot, make_tuple it)
        | `As ((x, _, pos), p) ->
            let p = tp p in
            let env' = Env.bind (env p) (x, it p) in
              `As ((x, Some (it p), pos), erase p), env', (ot p, it p)
        | `HasType (p, t) ->
            let p = tp p in
            let () = unify ~handle:Errors.pattern_annotation ((pos p, it p), 
                                                              (_UNKNOWN_POS_, DesugarDatatype.desugar_datatype' tenvs t))
            in
              `HasType (erase p, t), env p, (ot p, it p) in
      (p, pos'), env, (outer_type, inner_type)
  in
    fun pattern ->
      let pos, env, (outer_type, _) = type_pattern pattern in
        pos, env, outer_type


let rec extract_row : Types.alias_environment -> Types.datatype -> Types.row
  = fun alias_env t ->
  match t with
    | `Record row -> row
    | `Variant row -> row
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Body t -> extract_row alias_env t
            | _ -> failwith
                ("Internal error: attempt to extract a row from a datatype that is not a record or variant: " 
                 ^ Types.string_of_datatype t)
        end
    | `Application (s, ts) ->
        extract_row alias_env (Instantiate.alias (Types.lookup_alias (s, ts) alias_env) ts)
    | _ -> failwith
        ("Internal error: attempt to extract a row from a datatype that is not a record or variant: " 
         ^ Types.string_of_datatype t)

let rec pattern_env : pattern -> Types.datatype Env.t = 
  fun (p, _) -> match p with
    | `Any
    | `Nil
    | `Constant _ -> Env.empty

    | `HasType (p,_) -> pattern_env p
    | `Variant (_, Some p) -> pattern_env p
    | `Variant (_, None) -> Env.empty
    | `Negative _ -> Env.empty
    | `Record (ps, Some p) ->
        List.fold_right (snd ->- pattern_env ->- Env.extend) ps (pattern_env p)
    | `Record (ps, None) ->
        List.fold_right (snd ->- pattern_env ->- Env.extend) ps Env.empty
    | `Cons (h,t) -> Env.extend (pattern_env h) (pattern_env t)
    | `List ps
    | `Tuple ps -> List.fold_right (pattern_env ->- Env.extend) ps Env.empty
    | `Variable (v, Some t, _) -> Env.bind Env.empty (v, t)
    | `Variable (_, None, _) -> assert false
    | `As       ((v, Some t, _), p) -> Env.bind (pattern_env p) (v, t)
    | `As       ((_, None, _), _) -> assert false


let rec extract_formlet_bindings : phrase -> Types.datatype Env.t = function
  | `FormBinding (_, pattern), _ -> pattern_env pattern
  | `Xml (_, _, _, children), _ ->
      List.fold_right
        (fun child env ->
           Env.extend env (extract_formlet_bindings child))
        children Env.empty
  | _ -> Env.empty
          
let rec type_check : context -> phrase -> phrase * Types.datatype = 
  fun ({tenv = (env, alias_env)} as context) (expr, pos) ->
    let _UNKNOWN_POS_ = "<unknown>" in
    let no_pos t = (_UNKNOWN_POS_, t) in
    let unify (l, r) = unify alias_env ~pos:(lookup_pos pos) (l, r)
    and (++) (env, alias_env) env' = (Env.extend env env', alias_env) in
    let typ (_,t) : Types.datatype = t 
    and erase (p, _) = p
    and erase_pat (p, _, _) = p
    and pattern_typ (_, _, t) = t
    and pattern_env (_, e, _) = e in
    let pattern_pos ((_,p),_,_) = let (_,_,p) = lookup_pos p in p in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos (_,p) = let (_,_,p) = lookup_pos p in p in   
    let exp_pos (p,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in
    let tpc = type_pattern `Closed alias_env (context.tvars, context.rvars)
    and tpo = type_pattern `Open alias_env (context.tvars, context.rvars)
    and tc : phrase -> phrase * Types.datatype = type_check context
    and expr_string (_,pos : Sugartypes.phrase) : string =
      let (_,_,e) = lookup_pos pos in e 
    and erase_cases = List.map (fun ((p, _, _), (e, _)) -> p, e) in
    let type_cases binders =
      let pt = Types.fresh_type_variable () in
      let bt = Types.fresh_type_variable () in
      let binders, pats = 
        List.fold_right
          (fun (pat, body) (binders, pats) ->
             let pat = tpo pat in
             let () =
               unify ~handle:Errors.switch_patterns
                 (ppos_and_typ pat, no_pos pt)
             in
               (pat, body)::binders, pat :: pats)
          binders ([], []) in
      let pt = close_pattern_type (List.map fst3 pats) pt in

      (* NOTE: it is important to type the patterns in isolation first in order
         to allow them to be closed before typing the bodies *)

      let binders = 
        List.fold_right
          (fun (pat, body) binders ->
             let body = type_check {context with tenv = context.tenv ++ pattern_env pat} body in
             let () = unify ~handle:Errors.switch_branches
               (pos_and_typ body, no_pos bt)
             in
               (pat, body)::binders)
          binders []
      in
        binders, pt, bt in

    let e, t =
      match (expr : phrasenode) with
        | `Var v            -> `Var v, Utils.instantiate env v
        | `Section _ as s   -> type_section env s

        (* literals *)
        | `Constant c as c' -> c', constant_type c
        | `TupleLit [p] -> 
            let p = tc p in
              `TupleLit [erase p], typ p
        | `TupleLit ps ->
            let ps = List.map tc ps in
              `TupleLit (List.map erase ps), Types.make_tuple_type (List.map typ ps)
        | `RecordLit (fields, rest) ->
            let fields, field_env, absent_field_env = 
              List.fold_right
                (fun (label, e) (fields, field_env, absent_field_env) ->
                   let e = tc e in
                   let t = typ e in
                     ((label, e)::fields,
                      StringMap.add label (`Present t) field_env,
                      StringMap.add label `Absent absent_field_env))
                fields ([], StringMap.empty, StringMap.empty) in
              begin match rest with
                | None ->
                    `RecordLit (alistmap erase fields, None), `Record (field_env, Unionfind.fresh `Closed)
                | Some r ->
                    let r : phrase * Types.datatype = tc r in
                    let rtype = typ r in
                    (* make sure rtype is a record type that doesn't match any of the existing fields *)
                    let () = unify ~handle:Errors.extend_record
                      (pos_and_typ r, no_pos (`Record (absent_field_env, Types.fresh_row_variable ()))) in
                    let (rfield_env, rrow_var), _ = Types.unwrap_row (extract_row alias_env rtype) in 
                    (* attempt to extend field_env with the labels from rfield_env
                       i.e. all the labels belonging to the record r
                    *)
                    let field_env' =
                      StringMap.fold (fun label t field_env' ->
                                        match t with
                                          | `Absent ->
                                              if StringMap.mem label field_env then
                                                field_env'
                                              else
                                                StringMap.add label `Absent field_env'
                                          | `Present _ ->
                                              if StringMap.mem label field_env then
                                                failwith ("Could not extend record "^ expr_string (fst r)^" (of type "^
                                                            Types.string_of_datatype rtype^") with the label "^
                                                            label^
                                                            " (of type"^Types.string_of_datatype (`Record (field_env, Unionfind.fresh `Closed))^
                                                            ") because the labels overlap")
                                              else
                                                StringMap.add label t field_env') rfield_env field_env in
                      `RecordLit (alistmap erase fields, Some (erase r)), `Record (field_env', rrow_var)
              end
        | `ListLit es ->
            begin match List.map tc es with
              | [] -> `ListLit [], `Application ("List", [Types.fresh_type_variable ()])
              | e :: es -> 
                  List.iter (fun e' -> unify ~handle:Errors.list_lit (pos_and_typ e, pos_and_typ e')) es;
                  `ListLit (List.map erase (e::es)), `Application ("List", [typ e])
            end
        | `FunLit (pats, body) ->
            let pats = List.map (List.map tpc) pats in
            let fold_in_envs = List.fold_left (fun env pat' -> env ++ pattern_env pat') in
            let env', aliases = List.fold_left fold_in_envs context.tenv pats in
            let body = type_check {context with tenv = (Env.bind env' (mailbox, Types.fresh_type_variable ()), aliases)} body in
            let ftype = 
              List.fold_right
                (fun pat rtype ->
                   let args = Types.make_tuple_type (List.map pattern_typ pat) in
                     `Function (args, Types.fresh_type_variable (), rtype))
                pats (typ body) in
              `FunLit (List.map (List.map erase_pat) pats, erase body), ftype

        | `ConstructorLit (c, None) ->
            let type' = `Variant (Types.make_singleton_open_row 
                                    (c, `Present Types.unit_type)) in
              `ConstructorLit (c, None), type'

        | `ConstructorLit (c, Some v) ->
            let v = tc v in
            let type' = `Variant (Types.make_singleton_open_row
                                    (c, `Present (typ v))) in
              `ConstructorLit (c, Some (erase v)), type'

        (* database *)
        | `DatabaseLit (name, (driver, args)) ->
            let driver = opt_map tc driver
            and args   = opt_map tc args
            and name   = tc name in
              `DatabaseLit (erase name, (opt_map erase driver, opt_map erase args)), `Primitive `DB

        | `TableLit (tname, dtype, constraints, db) ->
            let tname = tc tname 
            and db = tc db in
            let () = unify ~handle:Errors.table_name (pos_and_typ tname, no_pos Types.string_type)
            and () = unify ~handle:Errors.table_db (pos_and_typ db, no_pos Types.database_type) in
            let read_row = match dtype with
              | RecordType row ->
                  row
              | UnitType ->
                  raise (Syntax.ASTSyntaxError(lookup_pos pos, "Tables must have at least one field"))
              | _ ->
                  raise (Syntax.ASTSyntaxError(lookup_pos pos, "Tables must take a non-empty record type")) in
            let write_row = DesugarDatatype.make_write_row read_row constraints  in
              `TableLit (erase tname, dtype, constraints, erase db), 
               `Table (DesugarDatatype.desugar_datatype (RecordType read_row),
                       DesugarDatatype.desugar_datatype (RecordType write_row))
                 
        | `DBDelete (pat, from, where) ->
            let pat  = tpc pat 
            and from = tc from
            and read  = `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let () = unify ~handle:Errors.delete_table
              (pos_and_typ from, no_pos (`Table (read, write))) in
            let () = unify ~handle:Errors.delete_pattern (ppos_and_typ pat, no_pos read) in
            let where = opt_map (type_check {context with tenv = (context.tenv ++ pattern_env pat)}) where in
            let () =
              opt_iter
                (fun e -> unify ~handle:Errors.delete_where (pos_and_typ e, no_pos Types.bool_type)) where
            in
              `DBDelete (erase_pat pat, erase from, opt_map erase where), Types.unit_type
        | `DBInsert (into, values) ->
            let into   = tc into
            and values = tc values
            and read  = `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let () = unify ~handle:Errors.insert_table
              (pos_and_typ into, no_pos (`Table (read, write))) in
            let () = unify ~handle:Errors.insert_values (pos_and_typ values, no_pos (Types.make_list_type write)) in
              `DBInsert (erase into, erase values), Types.unit_type
        | `DBUpdate (pat, from, where, set) ->
            let pat  = tpc pat
            and from = tc from
            and read =  `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let () = unify ~handle:Errors.update_table
              (pos_and_typ from, no_pos (`Table (read, write))) in
            let () = unify ~handle:Errors.update_pattern (ppos_and_typ pat, no_pos read) in
            let context' = {context with tenv = context.tenv ++ pattern_env pat} in
            let where = opt_map (type_check context') where in
            let () =
              opt_iter
                (fun e -> unify ~handle:Errors.update_where (pos_and_typ e, no_pos Types.bool_type)) where in

            let set, field_env =
              List.fold_right
                (fun (name, exp) (set, field_env) ->
                   let exp = type_check context' exp in
                     if StringMap.mem name field_env then
                       Errors.die (lookup_pos pos) "Duplicate fields in update expression."
                     else
                       (name, exp)::set, StringMap.add name (`Present (typ exp)) field_env)
                set ([], StringMap.empty) in

            let () = unify ~handle:Errors.update_write
              (no_pos write, no_pos (`Record (field_env, Types.fresh_row_variable ())))
            in
              `DBUpdate (erase_pat pat, erase from, opt_map erase where, 
                         List.map (fun (n,(p,_)) -> n, p) set), Types.unit_type

        (* concurrency *)
        | `Spawn p ->
            (* (() -{b}-> d) -> Mailbox (b) *)
            let pid_type = Types.fresh_type_variable () in
            let () = unify ~handle:Errors.spawn_process
              ((uexp_pos p, pid_type), no_pos (`Application ("Mailbox", [Types.fresh_type_variable()]))) in
            let context' = {context with tenv = Env.bind env (mailbox, pid_type), alias_env} in
            let p = type_check context' p in
              `Spawn (erase p), pid_type
        | `SpawnWait p ->
            (* (() -{b}-> d) -> d *)
            let return_type = Types.fresh_type_variable () in
            let pid_type = Types.fresh_type_variable () in
            let () = unify ~handle:Errors.spawn_wait_process
              ((uexp_pos p, pid_type), no_pos (`Application ("Mailbox", [Types.fresh_type_variable()]))) in
            let context' = {context with tenv = Env.bind env (mailbox, pid_type), alias_env} in
            let p = type_check context' p in
              unify ~handle:Errors.spawn_wait_return (no_pos return_type, no_pos (typ p));
              `SpawnWait (erase p), return_type
        | `Receive (binders, _) ->
            let mbtype = Types.fresh_type_variable () in
            let boxed_mbtype = mailbox_type env in
            let () = unify ~handle:Errors.receive_mailbox
              (no_pos boxed_mbtype, no_pos (`Application ("Mailbox", [mbtype]))) in
            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Errors.receive_patterns
              (no_pos mbtype, no_pos pattern_type)
            in
              `Receive (erase_cases binders, Some pattern_type), body_type

        (* applications of various sorts *)
        | `UnaryAppl (op, p) -> 
            let op = op, type_unary_op env op
            and p = tc p
            and rettyp = Types.fresh_type_variable () in
              unify ~handle:Errors.unary_apply
                ((Sugartypes.string_of_unary_op (fst op), typ op), no_pos (`Function (Types.make_tuple_type [typ p], mailbox_type env, rettyp)));
              `UnaryAppl (fst op, erase p), rettyp
        | `InfixAppl (op, l, r) ->
            let opt = type_binary_op env op in
            let l = tc l
            and r = tc r 
            and rettyp = Types.fresh_type_variable () in
              unify ~handle:Errors.infix_apply
                ((Sugartypes.string_of_binop op, opt), no_pos (`Function (Types.make_tuple_type [typ l; typ r], 
                                                     mailbox_type env, rettyp)));
              `InfixAppl (op, erase l, erase r), rettyp
        | `FnAppl (f, ps) ->
            let f = tc f
            and ps = List.map (tc) ps
            and rettyp = Types.fresh_type_variable () in
              unify ~handle:Errors.fun_apply
                (pos_and_typ f, no_pos (`Function (Types.make_tuple_type (List.map typ ps), 
                                                   mailbox_type env, rettyp)));
              `FnAppl (erase f, List.map erase ps), rettyp

        (* xml *)
        | `Xml (tag, attrs, attrexp, children) ->
            let attrs = alistmap (List.map (tc)) attrs
            and attrexp = opt_map tc attrexp
            and children = List.map (tc) children in
            let () = List.iter
              (snd ->-
                 List.iter (fun attr -> unify ~handle:Errors.xml_attribute
                              (pos_and_typ attr, no_pos Types.string_type))) attrs
            and () =
              opt_iter
                (fun e ->
                   unify ~handle:Errors.xml_attributes
                     (pos_and_typ e, no_pos (`Application ("Attributes", [])))) attrexp
            and () =
              List.iter (fun child ->
                           unify ~handle:Errors.xml_child (pos_and_typ child, no_pos Types.xml_type)) children in
              `Xml (tag, 
                    List.map (fun (x,p) -> (x, List.map erase p)) attrs,
                    opt_map erase attrexp,
                    List.map erase children), Types.xml_type
        | `TextNode _ as t -> t, Types.xml_type
        | `Formlet (body, yields) ->
            let body = tc body in
            let context' = {context with tenv = context.tenv ++ extract_formlet_bindings (erase body)} in
            let yields = type_check context' yields in
              unify ~handle:Errors.formlet_body (pos_and_typ body, no_pos Types.xml_type);
              `Formlet (erase body, erase yields), Types.make_formlet_type (typ yields)
        | `Page e ->
            let e = tc e in
              unify ~handle:Errors.page_body (pos_and_typ e, no_pos Types.xml_type);
              `Page (erase e), Types.page_type
        | `FormletPlacement (f, h, attributes) ->
            let t = Types.fresh_type_variable () in

            let f = tc f
            and h = tc h
            and attributes = tc attributes in

            let () = unify ~handle:Errors.render_formlet
              (pos_and_typ f, no_pos (`Application ("Formlet", [t]))) in
            let () = unify ~handle:Errors.render_handler
              (pos_and_typ h, (exp_pos f, `Application ("Handler", [t]))) in
            let () = unify ~handle:Errors.render_attributes
              (pos_and_typ attributes, no_pos (`Application ("Attributes", [])))
            in
              `FormletPlacement (erase f, erase h, erase attributes), Types.xml_type
        | `PagePlacement e ->
            let e = tc e in
              unify ~handle:Errors.page_placement (pos_and_typ e, no_pos Types.page_type);
              `PagePlacement (erase e), Types.xml_type
        | `FormBinding (e, pattern) ->
            let e = tc e
            and pattern = tpc pattern in
            let a = Types.fresh_type_variable () in
            let ft = Types.make_formlet_type a in
              unify ~handle:Errors.form_binding_body (pos_and_typ e, no_pos ft);
              unify ~handle:Errors.form_binding_pattern (ppos_and_typ pattern, (exp_pos e, a));
              `FormBinding (erase e, erase_pat pattern), Types.xml_type

        (* various expressions *)
        | `Iteration (generators, body, where, orderby) ->            
            let generators, context =
              List.fold_left
                (fun (generators, context) ->
                   function
                     | `List (pattern, e) ->
                         let a = Types.fresh_type_variable () in
                         let lt = Types.make_list_type a in
                         let pattern = tpc pattern
                         and e = tc e in
                         let () = unify ~handle:Errors.iteration_list_body (pos_and_typ e, no_pos lt) in
                         let () = unify ~handle:Errors.iteration_list_pattern (ppos_and_typ pattern, (exp_pos e, a))
                         in
                           (`List (erase_pat pattern, erase e) :: generators,
                            {context with tenv = context.tenv ++ pattern_env pattern})
                     | `Table (pattern, e) ->
                         let a = Types.fresh_type_variable () in
                         let tt = Types.make_table_type (a, Types.fresh_type_variable ()) in
                         let pattern = tpc pattern
                         and e = tc e in
                         let () = unify ~handle:Errors.iteration_table_body (pos_and_typ e, no_pos tt) in
                         let () = unify ~handle:Errors.iteration_table_pattern (ppos_and_typ pattern, (exp_pos e, a)) in
                           (`Table (erase_pat pattern, erase e) :: generators,
                            {context with tenv = context.tenv ++ pattern_env pattern}))
                ([], context) generators in
            let generators = List.rev generators in              
            let tc = type_check context in
            let body = tc body in
            let where = opt_map tc where in
            let orderby = opt_map tc orderby in
              unify ~handle:Errors.iteration_body
                (pos_and_typ body, no_pos (Types.make_list_type (Types.fresh_type_variable ())));
              opt_iter (fun where -> unify ~handle:Errors.iteration_where
                          (pos_and_typ where, no_pos Types.bool_type)) where;
              `Iteration (generators, erase body, opt_map erase where, opt_map erase orderby), (typ body)

        | `Escape ((name,_,pos), e) ->
            (* There's a question here whether to generalise the
               return type of continuations.  With `escape'
               continuations are let-bound, so generalising the return
               type is sound.  With `call/cc' continuations are
               lambda-bound so the return type cannot be generalised.
               If we do generalise here then we can accept more valid
               programs, since the continuation can then be used in
               any context, e.g.:
               
                 escape y in {
                   var _ = y(1) == "";
                   var _ = y(1) == true;
                   2
                 }

               However, currently we desugar escape to call/cc, so
               generalising will mean accepting programs that have an
               invalid type in the IR (although they're guaranteed not
               to "go wrong".)

               (Also, should the mailbox type be generalised?)
            *)
            let f = Types.fresh_type_variable ()
            and t = Types.fresh_type_variable ()
            and m = Types.fresh_type_variable () in
            let cont_type = `Function (Types.make_tuple_type [f], m, t) in
            let context' = {context with tenv = Env.bind env (name, cont_type), alias_env} in
            let e = type_check context' e in
            let () = unify ~handle:Errors.escape (pos_and_typ e, no_pos f) in
              `Escape ((name, Some cont_type, pos), erase e), typ e
        | `Conditional (i,t,e) ->
            let i = tc i
            and t = tc t
            and e = tc e in
              unify ~handle:Errors.if_condition
                (pos_and_typ i, no_pos (`Primitive `Bool));
              unify ~handle:Errors.if_branches
                (pos_and_typ t, pos_and_typ e);
              `Conditional (erase i, erase t, erase e), (typ t)
        | `Block (bindings, e) ->
            let rec type_bindings context =
              function
                | [] -> [], context.tenv
                | b :: bs ->
                    let b, typing_env' = type_binding context b in
                    let bs, typing_env'' = type_bindings ({context with tenv =
                                                              Types.concat_typing_environment context.tenv typing_env'}) bs in
                      b :: bs, typing_env'' in
            let bindings, typing_env = type_bindings context bindings in
            let e = type_check {context with tenv = typing_env} e in
              `Block (bindings, erase e), typ e
        | `Regex r ->
            `Regex (type_regex context r), `Application ("Regex", [])
        | `Projection (r,l) ->
            let r = tc r in
            let fieldtype = Types.fresh_type_variable () in
              unify ~handle:Errors.projection
                (pos_and_typ r, no_pos (`Record (Types.make_singleton_open_row 
                                                   (l, `Present fieldtype))));
              `Projection (erase r, l), fieldtype
        | `With (r, fields) ->
            let r = tc r
            and fields = alistmap tc fields in
            let rtype = typ r 
            and fields_type =
              `Record (List.fold_right
                         (fun (lab, rhs) row ->
                            Types.row_with (lab, `Present (Types.fresh_type_variable ())) row)
                         fields (Types.make_empty_open_row ())) in
              unify ~handle:Errors.record_with (pos_and_typ r, no_pos fields_type);
              `With (erase r, alistmap erase fields), rtype
        | `TypeAnnotation (e, t) ->
            let e = tc e
            and t' = DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t in
              unify ~handle:Errors.type_annotation (pos_and_typ e, no_pos t');
              `TypeAnnotation (erase e, t), t'
        | `Upcast (e, t1, t2) ->
            let e = tc e
            and t1' = DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t1
            and t2' = DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t2 in
              if Types.is_sub_type (t2', t1') then
                begin
                  unify ~handle:Errors.upcast_source (pos_and_typ e, no_pos t2');
                  `Upcast (erase e, t1, t2), t1'
                end
              else
                Errors.upcast_subtype (lookup_pos pos) t2' t1'
        | `Switch (e, binders, _) ->
            let e = tc e in
            let binders, pattern_type, body_type = type_cases binders in
            let () = unify ~handle:Errors.switch_pattern (pos_and_typ e, no_pos pattern_type) in
              `Switch (erase e, erase_cases binders, Some pattern_type), body_type
    in (e, pos), t
and type_binding : context -> binding -> binding * Types.typing_environment =
  fun ({tenv = (env, alias_env)} as context) (def, pos) ->
    let type_check = type_check in
    let unify (l, r) = unify alias_env ~pos:(lookup_pos pos) (l, r)
    and typ (_,t) = t
    and erase (e, _) = e
    and erase_pat (e, _, _) = e
    and pattern_typ (_, _, t) = t
    and tc = type_check context
    and tpc = type_pattern `Closed alias_env (context.tvars, context.rvars)
    and pattern_env (_, e, _) = e
    and (++) (env, alias_env) env' = (Env.extend env env', alias_env) in
    let _UNKNOWN_POS_ = "<unknown>" in
    let no_pos t = (_UNKNOWN_POS_, t) in
    let pattern_pos ((_,p),_,_) = let (_,_,p) = lookup_pos p in p in
    let ppos_and_typ p = (pattern_pos p, pattern_typ p) in
    let uexp_pos (_,p) = let (_,_,p) = lookup_pos p in p in   
    let exp_pos (p,_) = uexp_pos p in
    let pos_and_typ e = (exp_pos e, typ e) in
    let typed, env = match def with
        | `Include _ -> assert false
        | `Val (pat, body, location, datatype) -> 
            let body = tc body in
            let pat = tpc pat in
            let penv = pattern_env pat in
            let bt = typ body in
            let () = unify ~handle:Errors.bind_val (ppos_and_typ pat, pos_and_typ body) in
            let () = opt_iter (fun t ->
                                 unify ~handle:Errors.bind_val_annotation
                                   (no_pos bt, no_pos (DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t))) datatype in
            let bt, penv =
              if Utils.is_generalisable (erase body) then
                (Utils.generalise env bt, Env.map (Utils.generalise env) penv)
              else
                bt, penv
            in
              `Val (erase_pat pat, erase body, location, datatype), (Env.extend env penv, alias_env)
        | `Fun ((name, _, pos), (pats, body), location, t) ->
            let pats = List.map (List.map tpc) pats in
            let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in
            let body_env, alias_env = List.fold_left fold_in_envs context.tenv pats in
            let mt = Types.fresh_type_variable () in
            let body = type_check {context with tenv = (Env.bind body_env (mailbox, mt), alias_env)} body in
            let ft =
              let rec makeft =
                function
                  | [] -> typ body
                  | [_] -> `Function (Types.fresh_type_variable (), mt, typ body)
                  | _ :: pats -> `Function (Types.fresh_type_variable (),
                                            Types.fresh_type_variable (),
                                            makeft pats)
              in
                makeft pats in
            let () = opt_iter (fun t ->
                                 unify ~handle:Errors.bind_fun_annotation
                                   (no_pos ft, no_pos (DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t))) t in
              (`Fun ((name, Some ft, pos), (List.map (List.map erase_pat) pats, erase body), location, t),
               (Env.bind env (name, Utils.generalise env ft), alias_env))
        | `Funs (defs) ->
            let fbs, patss =
              List.split 
                (List.map
                   (fun ((name,_,_), (pats, body), _, t) ->
                      let pats = List.map (List.map tpc) pats in
                      let ft =
                        List.fold_right
                          (fun pat rtype ->
                             let args = Types.make_tuple_type (List.map pattern_typ pat) in
                               `Function (args, Types.fresh_type_variable (), rtype))
                          pats (Types.fresh_type_variable ()) in
                      let fb =
                        match t with
                          | None -> ft
                          | Some t ->
                              let fb = Utils.generalise env (DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) t) in
                              let () = unify ~handle:Errors.bind_rec_annotation (no_pos ft, no_pos fb) in
                                fb
                      in
                        ((name, fb), pats)) defs) in
            let defs =
              let body_env = List.fold_left (fun env (name, fb) -> Env.bind env (name, fb)) env fbs in
              let fold_in_envs = List.fold_left (fun env pat' -> env ++ (pattern_env pat')) in
                List.rev
                  (List.fold_left2
                     (fun defs ((name, _, pos), (_, body), location, t) pats ->
                        let body_env, alias_env = List.fold_left fold_in_envs (body_env, alias_env) pats in
                        let mt = Types.fresh_type_variable () in
                        let body = type_check {context with tenv = (Env.bind body_env (mailbox, mt), alias_env)} body in
                        let ft =
                          let rec makeft =
                            function
                              | [] -> typ body
                              | [_] -> `Function (Types.fresh_type_variable (), mt, typ body)
                              | _ :: pats -> `Function (Types.fresh_type_variable (),
                                                          Types.fresh_type_variable (),
                                                          makeft pats)
                          in
                            makeft pats in
                        let () = unify ~handle:Errors.bind_rec_rec (no_pos ft, no_pos (Env.lookup body_env name))
                        in
                          ((name, Some ft, pos), (pats, body), location, t) :: defs) [] defs patss) in
            let env =
              List.fold_left (fun env (name, fb) ->
                                Env.bind env (name, Utils.generalise env fb)) env fbs in
            let typing_env = env, alias_env in
              (`Funs (List.map (fun (binder, (ppats, body), location, dtopt) -> 
                                  binder, 
                                  (List.map (List.map erase_pat) ppats, erase body),
                                  location, dtopt) defs), typing_env)
        | `Foreign (language, name, datatype) ->
            let assumption = DesugarDatatype.desugar_datatype datatype in
              (`Foreign (language, name, datatype),
               (Env.bind env (name, assumption), alias_env))
        | `Type (typename, args, datatype) as t ->
            let dtype = DesugarDatatype.desugar_datatype' (context.tvars, context.rvars) datatype in
            let args =
              List.fold_right
                (fun name args ->
                   if StringMap.mem name context.tvars then
                     match Unionfind.find (StringMap.find name context.tvars) with
                       | `Flexible var | `Rigid var -> var :: args
                       | _ -> args
                   else if StringMap.mem name context.rvars then
                     match Unionfind.find (StringMap.find name context.rvars) with
                       | `Flexible var | `Rigid var -> var :: args
                       | _ -> args
                   else
                     assert false)
                args [] in
              t, (env, Utils.register_alias (typename, args, dtype) alias_env)
        | `Infix -> `Infix, context.tenv
        | `Exp e ->
            let e = tc e in
            let () = unify ~handle:Errors.bind_exp
              (pos_and_typ e, no_pos Types.unit_type)
            in
              `Exp (erase e), (Env.empty, Env.empty)
    in (typed, pos), env
and type_regex typing_env : regex -> regex =
  fun m -> 
    let erase (e, _) = e in
    let tr = type_regex typing_env in
      match m with
        | (`Range _ | `Simply _ | `Any  | `StartAnchor | `EndAnchor) as r -> r
        | `Quote r -> `Quote (tr r)
        | `Seq rs -> `Seq (List.map tr rs)
        | `Alternate (r1, r2) -> `Alternate (tr r1, tr r2)
        | `Group r -> `Group (tr r)
        | `Repeat (repeat, r) -> `Repeat (repeat, tr r)
        | `Splice e -> `Splice (erase (type_check typing_env e))
        | `Replace (r, `Literal s) -> `Replace (tr r, `Literal s)
        | `Replace (r, `Splice e) -> `Replace (tr r, `Splice (erase (type_check typing_env e)))
            
let mkContext : Types.typing_environment -> context =
  fun tenv -> {tenv = tenv; tvars = StringMap.empty; rvars = StringMap.empty}

let type_bindings typing_env bindings =
  let tyenv, bindings =
    List.fold_left
      (fun ((tenv : Types.typing_environment), bindings) (binding : binding) ->
         let _, (tvars, rvars) = DesugarDatatype.var_mapping_from_binding binding in
         let ctxt =  {tenv = tenv; tvars = tvars; rvars = rvars} in
         let binding, tenv' = type_binding ctxt binding in
           Types.concat_typing_environment tenv tenv', binding::bindings)
      (typing_env, []) bindings
  in
    tyenv, List.rev bindings
      
let type_sugar = Settings.add_bool("type_sugar", true, `User)
let show_pre_sugar_typing = Settings.add_bool("show_pre_sugar_typing", false, `User)

module Check =
struct
  let program tyenv (bindings, body) =
    if Settings.get_value type_sugar then
      try
        Debug.if_set show_pre_sugar_typing
          (fun () ->
             "before type checking: "^Show_program.show (bindings, body));

        let env', bindings = type_bindings tyenv bindings in 
        let program, t, env =
          match body with
            | None -> (bindings, None), Types.unit_type, env'
            | Some (_,pos as body) ->
                let _, (tvars, rvars) = DesugarDatatype.var_mapping_from_binding (`Exp body, pos) in
                let (body, typ) = type_check {tenv = env'; tvars = tvars; rvars = rvars} body in
                  (bindings, Some body), typ, env'
        in
          program, t, env
      with
          Unify.Failure (`Msg msg) -> failwith msg
    else
      (bindings, body), Types.unit_type, tyenv

  let sentence tyenv =
    if Settings.get_value type_sugar then
      let erase (e, _) = e in
        function
          | `Definitions bindings -> 
              let te, bindings = type_bindings tyenv bindings in
                `Definitions bindings, Types.unit_type, te
          | `Expression (_, pos as body) -> 
              let _, (tvars, rvars) = DesugarDatatype.var_mapping_from_binding (`Exp body, pos) in
              let body, t = (type_check {tenv = tyenv; tvars = tvars; rvars = rvars} body) in
                `Expression body, t, tyenv
          | `Directive d -> `Directive d, Types.unit_type, tyenv
    else
      fun sentence -> sentence, Types.unit_type, tyenv
end



