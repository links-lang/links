(* Some tests *)

open Utility

 let attempt : ('a -> 'b) -> 'a -> (string, 'b) either
  = fun f p ->
    try
      Right (f p)
    with e ->
      Left (Errors.format_exception e)


let failsAt : string -> ('a -> 'b) -> 'a -> (string, string) either
  = fun name f p ->
    match f p with
      | Left msg -> Right ("Failed at "^ name ^" with error "^msg)
      | Right _ -> Left ("Failed to fail at " ^ name)

let equals : ('a -> string) -> 'a -> 'a -> (string, 'a) either
  = fun show expected r ->
    if expected = r then Right r
    else Left ("Expected " ^ show expected ^ "; but got " ^ show r)


open Utility.EitherMonad

let checkTypes = attempt (Inference.type_program Library.typing_env ->- snd)

let parse_thingy s =
  let sugar, pos_context = (Parse.parse_string Parse.program) s in
  let (program, _, _), _ = Frontend.Pipeline.program Library.typing_env pos_context sugar in
    Sugar.desugar_program program

let parse = attempt parse_thingy


let optimise = attempt (fun program -> Optimiser.optimise_program (Library.typing_env, program))
let run tests = attempt (let _, prelude = Oldloader.load_file Library.typing_env (Settings.get_value Basicsettings.prelude_file) in
                         let prelude, _ = Interpreter.run_program [] [] prelude in
                           Interpreter.run_program prelude [] ->- snd) tests
let show = attempt Result.string_of_result

let type_matches ~inferred ~expected =
  let nfreevars =
    Types.TypeVarSet.cardinal -<- Types.free_type_vars
  and inferred =
    Instantiate.datatype (Types.make_fresh_envs inferred) inferred
  and expected =
    Instantiate.datatype (Types.make_rigid_envs expected) expected
  in try
      (* Check for unification without instantiation.  Perhaps we
         could just count freevars on both sides instead of all this
         make_fresh_envs nonsense.
      *)
      let c = nfreevars inferred in
        Unify.datatypes (expected, inferred);
        c = (nfreevars inferred)
    with _ -> false

let type_matches ~inferred ~expected =
  (* We want to check that two types are "equivalent".
     Strategy:
     ...
  *)
  let check_rhs_unchanged l r =
    let l =
      Instantiate.datatype (Types.make_wobbly_envs l) l
    and r =
      Instantiate.datatype (Types.make_rigid_envs r) r
    in try
        let c = Types.free_type_vars r in
          Unify.datatypes (l, r);
          Types.TypeVarSet.equal c (Types.free_type_vars r);
    with _ -> false
  in check_rhs_unchanged inferred expected
  && check_rhs_unchanged expected inferred

(* Check that the body of the program has a type equivalent to `t' *)
let has_type (t : Types.datatype) (Syntax.Program (_, body) as program) =
  let body_type = Syntax.node_datatype body in
    if type_matches ~expected:t ~inferred:body_type then Right program
    else Left (Printf.sprintf
                 "Types not equivalent : expected %s\n%s\ngot %s\n%s"
                 (Types.string_of_datatype t)
                 (Types.Show_datatype.show t)
                 (Types.string_of_datatype body_type)
                 (Types.Show_datatype.show body_type)
)

let datatype = DesugarDatatypes.read ~aliases:Env.String.empty

let functionp : Result.result -> Result.result m = function
  | `RecFunction _ as f -> Right f
  | r -> Left ("Expected a function but got "^ Result.Show_result.show r)

let failedp : 'a m -> string option =
  function
      Left l -> Some l
    | Right _ -> None

let is_function ?with_type s =
  parse s >>= checkTypes >>=
      (match with_type with
        | Some t -> has_type (datatype t)
        | _ -> inRight) >>= optimise >>= run >>= functionp

let result ~with_type is =
  fun s -> parse  s >>= checkTypes >>= has_type (datatype with_type) >>= optimise >>= run >>= show >>= equals identity is

let fails_at_runtime t =
  fun s -> parse s >>= checkTypes >>= has_type (datatype t) >>= optimise >>= failsAt "runtime" run

let has_syntaxerror =
  failsAt "parsing" parse

let has_typeerror =
  fun s -> parse s >>= failsAt "typechecking" checkTypes

let is_function ?with_type s =
  failedp (is_function ?with_type:with_type s)

let result ~with_type is s =
  failedp (result ~with_type:with_type is s)

let has_typeerror s =
  failedp (has_typeerror s)

let fails_at_runtime t s =
  failedp (fails_at_runtime t s)

let has_syntaxerror s =
  failedp (has_syntaxerror s)

let run tests =
  ListLabels.iter tests
    ~f:
    (fun (name, test, run) ->
       Printf.printf "%s %s\n"
         (match run test with
            | None -> "SUCCESS:"
            | Some msg -> "FAILURE (" ^ msg ^ "):\n   ") name;
    flush stdout)

