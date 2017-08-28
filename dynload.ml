
(* dynamically load plugins.*)

module StrM = Map.Make(String)
module StrS = Set.Make(String) 
module StrMM = MoreLabels.Map.Make(String)
module StrMS  = MoreLabels.Set.Make(String)

(* reference all Ocaml standard libraries, otherwise Dynlink.loadfile may encounter errors.
   -linkall fails because some modules(not in standard libraries) define same names.
   Better update this part if Ocaml standard libraries are modified. *)
let _ = 
	let _ = Arg.parse in
	let _ = Array.get in
	let _ = Buffer.create in
	let _ = Complex.zero in
	let _ = Callback.register in
	let _ = Char.chr in
	let _ = Digest.compare in
	let _ = Ephemeron.K1.create in
	let _ = Filename.concat in
	let _ = Format.open_box in
	let _ = Gc.set in
	let _ = Genlex.make_lexer in
	let _ = Hashtbl.create in
	let _ = Int32.add in
	let _ = Int64.add in
	let _ = Lazy.force in
	let _ = Lexing.dummy_pos in
	let _ = List.cons in
	let _ = Marshal.to_channel in
	let _ = MoreLabels.Hashtbl.create in
	let _ = Nativeint.add in
	let _ = Oo.id in
	let _ = Parsing.symbol_end in
	let _ = Printexc.to_string in
	let _ = Printf.fprintf in
	let _ = Queue.create in
	let _ = Random.full_init in
	let _ = Scanf.bscanf in
	let _ = Spacetime.save_event_for_automatic_snapshots in
	let _ = Stack.create in
	let _ = StdLabels.Array.get in
	let _ = StdLabels.Bytes.get in
	let _ = StdLabels.List.rev in
	let _ = StdLabels.String.get in
	let _ = Stream.of_list in
	let _ = String.get in
	let _ = Sys.argv in
	let _ = Uchar.max in
	let _ = Weak.create in
	()

let load name = 
	try (
		Dynlink.loadfile name)
	with Dynlink.Error e -> 
   		Printf.eprintf "Dynlink error: %s\n%!" (Dynlink.error_message e)
