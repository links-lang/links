(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type verbosity = Quiet | Summary | Details
let verbosity = ref Quiet
let set_verbosity = (:=) verbosity

let gettimeofday = ref (fun _ -> 0.)

let todo = ref []

let register level f = todo := (level,f) :: !todo

let dump ppf =
  List.iter (function
	       | (level,f) when level <= !verbosity -> f ppf
	       | _ -> ()) !todo

module Timer = struct
  type t = {
    name: string;
    mutable count : int;
    mutable total : float;
    mutable last  : float;
    mutable is_in : bool;
  }

  let print ppf c =
    Format.fprintf ppf "Timer %s. Total time: %f. Count: %i@."
      c.name c.total c.count
      
  let create s = 
    let c = { name = s; count = 0; total = 0.; last = 0.; is_in = false } in
    register Summary (fun ppf -> print ppf c);
    c
      
  let start c =
    assert(not c.is_in);
    c.is_in <- true;
    c.last <- !gettimeofday();
    c.count <- c.count + 1
      
  let stop c x =
    assert(c.is_in);
    c.is_in <- false;
    c.total <- c.total +. (!gettimeofday() -. c.last);
    x
end

module Counter = struct
  type t = {
    name: string;
    mutable count : int;
  }

  let print ppf c =
    Format.fprintf ppf "Counter %s: %i@."
      c.name c.count
      
  let create s = 
    let c = { name = s; count = 0 } in
    register Summary (fun ppf -> print ppf c);
    c
      
  let incr c =
    c.count <- c.count + 1

  let add c n =
    c.count <- c.count + n
end


module InOut = struct
  let h = Hashtbl.create 17

  let enter s =
    let i = try Hashtbl.find h s with Not_found ->
      let r = ref 0 in
      Hashtbl.add h s r;
      r in
    incr i;
    Printf.printf "+%s[%i] " s !i;
    flush stdout

  let leave s =
    let i = try Hashtbl.find h s with Not_found -> assert false in
    decr i;
    Printf.printf "-%s[%i] " s !i;
    flush stdout

  let wrap s f x =
    enter s;
    try
      let r = f x in
      leave s;
      r
    with exn ->
      leave s;
      raise exn
    
end
