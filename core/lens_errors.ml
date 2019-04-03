open Lens

let unpack r ~die ~fmt =
  match r with
  | Result.Ok t -> t
  | Result.Error e -> fmt e |> die

(** Unpack the result or format and throw the error using the [die] function. *)
let unpack_type_lens_result ~die res =
  match res with
  | Result.Ok t -> t
  | Result.Error e -> (
      let open Type.Lens_error in
      match e with
      | UnboundColumns c ->
          Format.asprintf "The columns { %a } were not bound."
            Alias.Set.pp_pretty c
          |> die
      | FunDepNotTreeForm c ->
          Format.asprintf
            "The functional dependencies are not in tree form. There seems to \
             be an error with the columns { %a }."
            Alias.Set.pp_pretty c
          |> die
      | ProbablyCycle c ->
          Format.asprintf
            "There is possibly a cycle or non-disjoint references with \
             functional dependencies involving { %a }."
            Alias.Set.pp_pretty c
          |> die )

let format_select_sort_error e =
  let open Sort.Select_sort_error in
  match e with
  | UnboundColumns c ->
      Format.asprintf
        "The columns { %a } in the predicate are not bound by the lens."
        Alias.Set.pp_pretty c
  | PredicateDoesntIgnoreOutputs {columns; fds} ->
      Format.asprintf
        "The lens has restrictions on the columns { %a }, which are defined \
         as outputs of the functional dependencies { %a }."
        Alias.Set.pp_pretty columns Fun_dep.Set.pp_pretty fds

let format_drop_sort_error e =
  let open Sort.Drop_sort_error in
  match e with
  | UnboundColumns c ->
      Format.asprintf "The columns { %a } are not bound by the lens."
        Alias.Set.pp_pretty c
  | DropNotDefinedByKey ->
      Format.asprintf
        "The functional dependencies do not specify that the drop columns are \
         defined by the key columns."
  | DefaultDropMismatch ->
      Format.asprintf
        "The number of default values does not match the number of drop \
         columns."
  | DropTypeError {column; default_type; column_type} ->
      Format.asprintf
        "The type of column '%s' is %a, but the default value is of type %a."
        column Phrase.Type.pp column_type Phrase.Type.pp default_type
  | DefiningFDNotFound c ->
      Format.asprintf
        "A functional dependency defining the columns { %a } could not be \
         found."
        Alias.Set.pp_pretty c

let unpack_type_drop_lens_result ~die res =
  match res with
  | Result.Ok t -> t
  | Result.Error e -> format_drop_sort_error e |> die

let format_phrase_typesugar_error f {Lens.Phrase.Typesugar.msg; data= _} =
  Format.fprintf f "%s" msg

let format_type_select_error e =
  let open Type.Select_lens_error in
  match e with
  | SortError e -> format_select_sort_error e
  | PredicateTypeError e ->
      Format.asprintf "%a" format_phrase_typesugar_error e
  | PredicateNotBoolean ty ->
      Format.asprintf
        "Lens select predicate is of type %a, but should be a boolean \
         expression."
        Phrase.Type.pp_pretty ty

let unpack_type_select_lens_result ~die res =
  unpack ~die ~fmt:format_type_select_error res

let unpack_phrase_typesugar_result ~die res =
  unpack ~die ~fmt:format_phrase_typesugar_error res

let unpack_sort_select_result ~die res =
  unpack ~die ~fmt:format_select_sort_error res

let format_sort_join_error e =
  let open Sort.Join_sort_error in
  match e with
  | UnboundColumn c ->
      Format.asprintf "The columns { %a } are not bound." Alias.Set.pp_pretty c
  | AlreadyBound c ->
      Format.asprintf "The columns { %a } are already bound."
        Alias.Set.pp_pretty c

let unpack_sort_join_result ~die res =
  unpack ~die ~fmt:format_sort_join_error res

let format_type_join_error e =
  let open Type.Join_lens_error in
  let pp_lens f l =
    match l with
    | Left -> Format.fprintf f "left"
    | Right -> Format.fprintf f "right"
  in
  match e with
  | SortError e -> format_sort_join_error e
  | PredicateNotBoolean (l, t) ->
      Format.asprintf
        "The delete %a predicate is of type %a, but should be a boolean \
         expression."
        pp_lens l Phrase.Type.pp_pretty t
  | PredicateTypeError (l, e) ->
      Format.asprintf "The delete %a predicate failed to type check: %a"
        pp_lens l format_phrase_typesugar_error e

let unpack_type_join_lens_result ~die res =
  unpack ~die ~fmt:format_type_join_error res

let format_eval_error e =
  let open Eval.Error in
  match e with
  | InvalidData ->
      "Not all records in data satisfy the condition in the lens sort."
  | InvalidDataType -> "Data is not a set of records."
  | ViolatesFunDepConstraint fd ->
      Format.asprintf "Data violates the functional dependency %a."
        Fun_dep.pp_pretty fd

let unpack_eval_error ~die res = unpack ~die ~fmt:format_eval_error res
