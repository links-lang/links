open Utility

(* Representation for a very limited subset of (OCaml) regular
   expressions. *)

type repeat = Star | Plus | Question
and  regex = | Range of (char * char)
             | Simply of string
             | Quote of regex
             | Any
             | StartAnchor
             | EndAnchor
             | Seq of regex list
             | Alternate of (regex * regex)
         | Group of regex
             | Repeat of (repeat * regex)
         | Replace of (regex * string)
               [@@deriving show]

let string_of_regex : regex -> string = fun s ->
  (* Using points-free style here (i.e. omitting the s) triggers a bug in
     versions of OCaml before about 3.09.0, so don't do that. *)
  let group s = Printf.sprintf "\\(%s\\)" s  in
  let compile_repeat = function
    | Star -> "*"
    | Plus -> "+"
    | Question -> "?" in
  let rec compile = function (* got rid of gratuituous grouping. this matters for replace *)
    | Range (f, t) -> Printf.sprintf "[%s-%s]" (Str.quote (String.make 1 f)) (Str.quote (String.make 1 t))
    | Simply s ->  s
    | Quote s ->  Str.quote (compile s)
    | Any -> "."
    | StartAnchor -> "^"
    | EndAnchor -> "$"
    | Seq rs -> List.fold_right (fun input output -> (compile input) ^ output) rs ""
    | Alternate (r1,r2) -> (compile r1) ^ "\\|" ^ (compile r2)
    | Group s -> group (compile s)
    | Repeat (s, r) ->  (compile r ^ compile_repeat s)
    | Replace _ -> assert false
  in
    compile s

let compile_ocaml : regex -> Str.regexp = Str.regexp -<- string_of_regex

let tests : (string * regex * string * bool) list =
  [
    (let s = "some .*string$\" ++?" in
       "splicing", Simply s, s, true);

    "range 0", Range ('0', '9'), "3", true;
    "range 1", Range ('0', '9'), "0", true;
    "range 2", Range ('0', '9'), "9", true;
    "range 3", Range ('0', '9'), ".", false;
    "range 4", Range ('a', 'z'), "p", true;
    "range 5", Range ('A', 'Z'), "p", false;

    "star 0", Repeat (Star, Any), "23r2r3", true;
    "star 1", Repeat (Star, Any), "", true;
    "star 2", Repeat (Star, (Simply "abc")), "abc", true;
    "star 3", Repeat (Star, (Simply "abc")), "abcabc", true;
    "star 4", Repeat (Star, (Simply "abc")), "", true;
    "star 5", Repeat (Star, (Simply "abc")), "a", false;
    "star 6", Repeat (Star, (Simply "abc")), "abca", false;

    "plus 0", Repeat (Plus, Any), "23r2r3", true;
    "plus 1", Repeat (Plus, Any), "", false;
    "plus 2", Repeat (Plus, (Simply "abc")), "abc", true;
    "plus 3", Repeat (Plus, (Simply "abc")), "abcabc", true;
    "plus 4", Repeat (Plus, (Simply "abc")), "", false;
    "plus 5", Repeat (Plus, (Simply "abc")), "a", false;
    "plus 6", Repeat (Plus, (Simply "abc")), "abca", false;

    "nesting 0", Seq [Simply "A";
                      Repeat (Plus, Simply "B")], "ABBB", true;

    "nesting 1", Seq [Simply "A";
                      Repeat (Plus, Simply "B")], "ABAB", false;

    "nesting 2", Repeat (Plus, Seq [Simply "A";
                                    Simply "B"]), "ABAB", true;

    "nesting 3", Repeat (Plus, Seq [Simply "A";
                                    Simply "B"]), "ABBB", false;
  ]

let run_tests = List.iter
  (fun (n, r, s, b) ->
     if Str.string_match (compile_ocaml r) s 0 = b
     then prerr_endline ("PASS: " ^ n)
     else prerr_endline ("FAIL: " ^ n))

