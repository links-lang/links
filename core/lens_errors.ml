open Lens
open Lens.Utility

let unpack r ~die ~fmt =
  match r with
  | Result.Ok t -> t
  | Result.Error e -> Format.asprintf "%a" fmt e |> die

let format_lens_sort_error f e =
  let open Sort.Lens_sort_error in
  match e with
  | UnboundColumns c ->
      Format.fprintf f "The columns { %a } are not bound." Alias.Set.pp_pretty c

let unpack_lens_sort_result ~die res =
  unpack ~die ~fmt:format_lens_sort_error res

let unpack_type_lens_result ~die res = unpack_lens_sort_result ~die res

let format_fun_dep_tree_form_error f e =
  let open Fun_dep.Tree.Tree_form_error in
  match e with
  | ContainsCycle cycle ->
      Format.fprintf f "The functional dependencies contain a cycle:\n  %a"
        (Format.pp_print_list
           ~pp_sep:(Format.pp_constant " -> ")
           Alias.Set.pp_pretty)
        cycle
  | NotDisjoint cols ->
      Format.fprintf f
        "The columns { %a } were found in two non-disjoint nodes."
        Alias.Set.pp_pretty cols

let format_select_sort_error f e =
  let open Sort.Select_sort_error in
  match e with
  | UnboundColumns c ->
      Format.fprintf f
        "The columns { %a } in the predicate are not bound by the lens."
        Alias.Set.pp_pretty c
  | PredicateDoesntIgnoreOutputs { columns; fds } ->
      Format.fprintf f
        "The lens has restrictions on the columns { %a }, which are defined as \
         outputs of the functional dependencies { %a }."
        Alias.Set.pp_pretty columns Fun_dep.Set.pp_pretty fds
  | TreeFormError { error } ->
      Format.fprintf f "The functional dependencies were not in tree form. %a"
        format_fun_dep_tree_form_error error

let format_drop_sort_error f e =
  let open Sort.Drop_sort_error in
  match e with
  | UnboundColumns c ->
      Format.fprintf f "The columns { %a } are not bound by the lens."
        Alias.Set.pp_pretty c
  | DropNotDefinedByKey ->
      Format.fprintf f
        "The functional dependencies do not specify that the drop columns are \
         defined by the key columns."
  | DefaultDropMismatch ->
      Format.fprintf f
        "The number of default values does not match the number of drop \
         columns."
  | DropTypeError { column; default_type; column_type } ->
      Format.fprintf f
        "The type of column '%s' is %a, but the default value is of type %a."
        column Phrase.Type.pp column_type Phrase.Type.pp default_type
  | DefiningFDNotFound c ->
      Format.fprintf f
        "A functional dependency defining the columns { %a } could not be \
         found."
        Alias.Set.pp_pretty c
  | DefaultDoesntMatchPredicate ->
      Format.fprintf f
        "The default value specified does not satisfy the lens predicate."
  | NotIndependent c ->
      Format.fprintf f "The columns { %a} are not independent in the predicate."
        Alias.Set.pp_pretty c

let unpack_type_drop_lens_result ~die res =
  unpack ~die ~fmt:format_drop_sort_error res

let format_phrase_typesugar_error f { Lens.Phrase.Typesugar.msg; data = _ } =
  Format.fprintf f "%s" msg

let format_type_select_error f e =
  let open Type.Select_lens_error in
  match e with
  | SortError e -> format_select_sort_error f e
  | PredicateTypeError e ->
      Format.fprintf f "%a" format_phrase_typesugar_error e
  | PredicateNotBoolean ty ->
      Format.fprintf f
        "Lens select predicate is of type %a, but should be a boolean \
         expression."
        Phrase.Type.pp_pretty ty

let unpack_type_select_lens_result ~die res =
  unpack ~die ~fmt:format_type_select_error res

let unpack_phrase_typesugar_result ~die res =
  unpack ~die ~fmt:format_phrase_typesugar_error res

let unpack_sort_select_result ~die res =
  unpack ~die ~fmt:format_select_sort_error res

let format_sort_join_error f e =
  let open Sort.Join_sort_error in
  match e with
  | UnboundColumn c ->
      Format.fprintf f "The columns { %a } are not bound." Alias.Set.pp_pretty c
  | AlreadyBound c ->
      Format.fprintf f "The columns { %a } are already bound."
        Alias.Set.pp_pretty c
  | TreeFormError { error } ->
      Format.fprintf f "The functional dependencies were not in tree form. %a"
        format_fun_dep_tree_form_error error

let unpack_sort_join_result ~die res =
  unpack ~die ~fmt:format_sort_join_error res

let format_type_join_error f e =
  let open Type.Join_lens_error in
  let pp_lens f l =
    match l with
    | Left -> Format.fprintf f "left"
    | Right -> Format.fprintf f "right"
  in
  match e with
  | SortError e -> format_sort_join_error f e
  | PredicateNotBoolean (l, t) ->
      Format.fprintf f
        "The delete %a predicate is of type %a, but should be a boolean \
         expression."
        pp_lens l Phrase.Type.pp_pretty t
  | PredicateTypeError (l, e) ->
      Format.fprintf f "The delete %a predicate failed to type check: %a"
        pp_lens l format_phrase_typesugar_error e

let unpack_type_join_lens_result ~die res =
  unpack ~die ~fmt:format_type_join_error res

let format_eval_error f e =
  let open Eval.Error in
  match e with
  | InvalidData ->
      Format.fprintf f
        "Not all records in data satisfy the condition in the lens sort."
  | InvalidDataType -> Format.fprintf f "Data is not a set of records."
  | ViolatesFunDepConstraint fd ->
      Format.fprintf f "Data violates the functional dependency %a."
        Fun_dep.pp_pretty fd

let unpack_eval_error ~die res = unpack ~die ~fmt:format_eval_error res

let format_lens_unchecked_error f e =
  let open Type.Unchecked_lens_error in
  match e with
  | UncheckedLens ->
      Format.fprintf f
        "This lens has some checks which can only be performed at runtime. To \
         ignore this use `lenscheck`."

let unpack_lens_checked_result ~die res =
  unpack ~die ~fmt:format_lens_unchecked_error res
