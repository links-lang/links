open Utility

let module_sep = ":::"

type binding_stack_node = [
  | `OpenStatement of string
  | `LocalVarBinding of string
]

(* Given a name and a prefix, appends the prefix (with separator)
 * as long as the prefix is not the empty string *)
let prefixWith name prefix =
  if prefix = "" then name else prefix ^ module_sep ^ name

let print_stack_node = function
 | `OpenStatement mn -> "module: " ^ mn
 | `LocalVarBinding lvb -> "var: " ^ lvb

let print_module_stack s = print_list (List.map print_stack_node s)

let rec moduleInScopeInner seen_modules binding_stack module_name =
  match binding_stack with
    | [] ->
        if StringSet.mem module_name seen_modules then Some(module_name) else None
    | (`LocalVarBinding _)::xs -> moduleInScopeInner seen_modules xs module_name
    | (`OpenStatement x)::xs ->
        let fully_qual = prefixWith module_name x in
        if StringSet.mem fully_qual seen_modules then
          Some(fully_qual)
        else
          moduleInScopeInner seen_modules xs module_name

let moduleInScope seen_modules binding_stack module_name =
  moduleInScopeInner seen_modules binding_stack module_name

