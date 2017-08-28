open Value
open Types
open Utility
open Proc
open Lib

let _ = 
	print_endline "adding Ocaml Random module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
    "random_init",
    (p1 (fun seed -> 
      (Random.init (unbox_int seed); seed)),
    datatype "(Int) ~> Int",
    IMPURE);

    "random_full_init",
    (p1 (fun lst ->
      (Random.full_init (Array.of_list (List.map unbox_int (unbox_list lst))));
      lst),
    datatype "([Int]) ~> [Int]",
    IMPURE);

    "random_self_init",
    (`PFun (fun _ _ -> (Random.self_init (); `Record []) ), 
    datatype "() -> ()",
    IMPURE); 

    "random_bits",
    (`PFun (fun _ _ -> (box_int (Random.bits ()))),
    datatype "() -> Int",
    IMPURE);

    "random_int",
    (p1 (unbox_int ->- Random.int ->- box_int),
    datatype "(Int) -> Int",
    IMPURE);

    "random_float",
    (p1 (unbox_float ->- Random.float ->- box_float),
    datatype "(Float) -> Float",
    IMPURE);

    "random_bool",
    (`PFun (fun _ _ -> (box_bool (Random.bool ()))),
    datatype "() -> Bool",
    IMPURE)

  	] in
	env := List.append (!env) new_env ;;