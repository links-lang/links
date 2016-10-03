(* Module [Getopt]: parsing of command line arguments *)
(* Alain Frisch *)

let noshort = '\000'
let nolong  = ""

type opt = char * string * ((unit -> unit) option) * ((string -> unit) option)

exception Error of string

let index_option s c =
  try Some (String.index s c)
  with Not_found -> None

let extract_arg_handle opt = function
  | (_,_,_,Some handle) -> handle
  | _ -> raise (Error (Printf.sprintf 
			 "Option %s does not accept argument" opt))

let extract_handle opt = function
  | (_,_,Some handle,_) -> handle
  | _ -> raise (Error (Printf.sprintf 
			 "Option %s must have an argument" opt))

let parse opts others args first last =
  let find_long opt =
    try List.find (fun (_,l,_,_) -> opt = l) opts 
    with Not_found ->
      raise (Error (Printf.sprintf "Unknown option --%s" opt))
  in
  let find_short opt =
    try List.find (fun (l,_,_,_) -> opt = l) opts 
    with Not_found ->
      raise (Error (Printf.sprintf "Unknown option -%c" opt))
  in

  (* Anonymous arguments after -- *)
  let rec skip no =
    if (no <= last) then (others args.(no); skip (succ no)) in

  let rec aux no =
    if (no <= last) then
      let s = args.(no) in
      let l = String.length s in
      if (l=0) then (others s; aux (succ no))
      else if  (s.[0] = '-') then
	if (l >= 2) && (s.[1] = '-') then
	  if (l = 2) then skip (succ no) (* -- *)
	  else match index_option s '=' with
	    | Some i -> (* long option with argument *)
		let opt = String.sub s 2 (i-2) in
		let arg = String.sub s (i+1) (l-i-1) in
		let handle = extract_arg_handle ("--"^opt) (find_long opt) in
		handle arg;
		aux (succ no)
	    | None ->  (* long option with no argument *)
		let opt = String.sub s 2 (l-2) in
		let handle = extract_handle s (find_long opt) in
		handle ();
		aux (succ no)
	else if (l = 1) then (others s; aux (succ no))  (* - *)
	else (* short option *)
	  let opt = s.[1] in
	  match find_short opt with
	    | (_,_,Some handle,None) ->
		(* no argument allowed; next chars are options *)
		handle ();
		for i = 2 to (l - 1) do
		  match find_short s.[i] with
		    | (_,_,Some handle,None) -> handle ()
		    | _ -> raise (Error (Printf.sprintf 
					   "Only non-argument short-options can be concatenated (error with option %c in %s)"  s.[i] s))
		done;
		aux (succ no)
	    | (_,_,_,Some handle) as o ->
		(* argument allowed or mandatory *)
		if (l>2) then (* immediate argument *)
		  (handle (String.sub s 2 (l-2)); aux (succ no))
		else if (no+1 <= last) && (args.(no+1).[0] <> '-') then
		  (* non-immediate argument *)
		  (handle args.(no+1); aux (no+2))
		else 
		  (* no argument *)
		  let handle = extract_handle s o in
		  (handle (); aux (succ no))
	    | _ -> failwith "Getopt.parse"
      else
	(others s; aux (succ no))
  in
  aux first



let parse_cmdline opts others =
  parse opts others Sys.argv 1 (Array.length Sys.argv - 1)

(* useful actions and handlers *)

let set var value = Some (fun () -> var := value)

let append lst = Some (fun x  -> lst := !lst@[x])

let incr var = Some (fun () -> Pervasives.incr var)

let atmost_once var exc = Some (fun x -> if !var="" then var := x else raise exc)

