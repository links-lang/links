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
let parse = attempt (Parse.parse_string Parse.program)
let optimise = attempt (fun program -> Optimiser.optimise_program (Library.typing_env, program))
let run = attempt (Interpreter.run_program [] [] ->- snd)
let show = attempt Result.string_of_result

(* For now, two types are considered equivalent if they print the
   same.  We'll do better later, perhaps when we have type
   minimisation. *)
let equivalent_types l r = Inferencetypes.string_of_datatype l = Inferencetypes.string_of_datatype r

(* Check that the last expression of `es' has a type equivalent to `t' *)
let has_type ((_, t) : Inferencetypes.assumption) (es : Syntax.expression list) = 
  let e = Utility.last es in
  let etype = Syntax.node_datatype e in
    if equivalent_types t etype then Right es
    else Left (Printf.sprintf
                 "Types not equivalent : expected %s, got %s" 
                 (Inferencetypes.string_of_datatype t)
                 (Inferencetypes.string_of_datatype etype))

let datatype = Parse.parse_string Parse.datatype

let result ~with_type is =
  fun s -> parse  s >>= checkTypes >>= has_type (datatype with_type) >>= optimise >>= run >>= show >>= equals identity is

let fails_at_runtime t = 
  fun s -> parse s >>= checkTypes >>= has_type (datatype t) >>= optimise >>= failsAt "runtime" run

let tests = [
  "Iteration",
  "for (var i <- [1,2,3,4,5]) if (i == 3) [] else [i, i]",
  result "[1, 1, 2, 2, 4, 4, 5, 5]"
    ~with_type: "[Int]";
  
  "Where clause",
  "for (var i <- [1,2,3,4,5]) where (i <> 3) [i, i]",
  result "[1, 1, 2, 2, 4, 4, 5, 5]"
    ~with_type: "[Int]";
  
  "Concatenation/union",
  "[1,2,3] ++ [4,5]",
  result "[1, 2, 3, 4, 5]"
    ~with_type: "[Int]";
  
  "Head and tail",
  "hd(['a','b','c']) == 'a'",
  result "true"
    ~with_type: "Bool";
  
  "Equality",
  "[1,2,3] == [1,2,3] && [1,2,3] <> [2,1,3]",
  result "true"
    ~with_type: "Bool";
  
  "Cons",
  "1 :: 2 :: 3 :: 4 :: []",
  result "[1, 2, 3, 4]"
    ~with_type: "[Int]";
  
  "Let-patterns [1]",
  "{var x :: xs = [1,2,3] ; x}",
  result "1"
    ~with_type: "Int";
  
  "Let-patterns [2]",
  "{var (x :: y :: xs, z) = ([1,2,3],4) ; (x,y)}",
  result "(1, 2)"
    ~with_type: "(Int, Int)";

  "Let-patterns [3]",
  "{var ((v,w) :: (x,y) :: z) = [(1,2),(3,4)] ; (w,x)}",
  result "(2, 3)"
    ~with_type: "(Int, Int)";
  
  "Let-patterns [4]",
  "{var (x,y) :: [] = [(1,2),(3,4),(4,5)] ; (x,y)}",
  fails_at_runtime "(Int, Int)";
  
  "Let-patterns [5]",
  "{var [(x,y)] = [(1,2),(3,4),(4,5)] ; (x,y)}",
  fails_at_runtime ("(Int, Int)");

  "Let-patterns [6]",
  "{var [] = [1,2,3] ; ()}",
  fails_at_runtime "()";

  "Let-patterns [7]",
  "{var [x] = [] ; ()}",
  fails_at_runtime "()";
  
  "Let-patterns [8]",
  "{var x::y = [] ; ()}",
  fails_at_runtime "()";

  "Case-patterns [1]",
  "switch ([]) { case [] -> 1 }",
  result "1"
    ~with_type: "Int";

  "Case-patterns [2]",
  "switch ([]) { case x::xs -> 1 }",
  fails_at_runtime "Int";

  "Case-patterns [3]",
  "switch ([]) { case [] -> 2 case x -> 1 }",
  result "2"
    ~with_type: "Int";
  
  "Case-patterns [4]",
  "switch ([]) { case x::xs -> 1 case x -> 2 }",
  result "2"
    ~with_type: "Int";
  
  "Case-patterns [5]",
  "switch ([1]) { case x::xs -> 1  case x -> 2 }",
  result "1"
    ~with_type: "Int";

  "Case-patterns [6]",
  "switch ([1,3]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }",
  result "3"
    ~with_type: "Int";
  
  "Case-patterns [7]",
  "switch ([1]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }",
  result "2"
    ~with_type: "Int";
  
  "Case-patterns [8]",
  "switch ([1,3]) { case x::y::[] -> 3 case x::y -> 2 case x -> 1 }",
  result "3"
    ~with_type: "Int";

  "Case-patterns [9]",
  "switch ([1]) { case x::[] -> 2 case x -> 1 }",
  result "2"
    ~with_type: "Int";

  "Case-patterns (redundancy) [10]",
  "fun (x) { switch (x) { case ([], []) -> 1 case (x::xs, y::ys) -> 2 case ([], _) -> 3 case (_, []) -> 4 }}",
  result "fun"
    ~with_type: "([a], [b]) -> Int";

  "Case-patterns (singleton list pattern)",
  "switch ([1]) { case [x] -> 2 case x -> 1 }",
  result "2"
    ~with_type: "Int";

  "Case patterns (with redefined hd)",
  "{ fun hd(_) { 1 } switch (\"a\") { case [y] -> y }}",
  result "'a'"
    ~with_type: "Char";

  "With parentheses:",
  "switch ([1]) { case (x::xs) -> 1  case x -> 2 }",
  result "1"
    ~with_type: "Int";

  "Length function",
  "fun len (l) { switch (l) { case [] -> 0 case x::xs -> 1 + len(xs) } } len ([1,2,3])",
  result "3"
    ~with_type: "Int";

  "Map function",
  "fun map (f,l) { switch (l) { case [] -> [] case x::xs -> f(x) :: map(f,xs) } } map((+)(1), [1,2,3])",
  result "[2, 3, 4]"
    ~with_type: "[Int]";

  "Sorting:",
  "for (var i <- [2,1,3]) orderby (i) [i]",
  result "[1, 2, 3]"
    ~with_type: "[Int]";

  "Empty-list comparison (1)",
  "[] < []",
  result "false"
    ~with_type: "Bool";

  "Empty-list comparison (2)",
  "[] > []",
  result "false"
    ~with_type: "Bool";

  "List comparison (1)",
  "[1] < []",
  result "false"
    ~with_type: "Bool";

  "List comparison (2)",
  "[1] > []",
  result "true"
    ~with_type: "Bool";
]

let run tests = 
  ListLabels.iter tests
    ~f:
    (fun (name, test, run) ->
       Printf.printf "Test %s%s\n" name
         (match run test with
            | Right _ -> " SUCCEEDED"
            | Left msg -> "\nFAILED with error :\n" ^ msg))
    
