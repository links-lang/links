let path = Settings.get_value Basicsettings.Ferry.pf_path
let pfopt = path ^ "pfopt"
let pfsql = path ^ "pfsql"

exception PF_error of string

let exists cmd = 
  try
  let stats = Unix.stat cmd in
    match stats.Unix.st_kind with
      | Unix.S_REG -> true
      | _ -> false
  with Unix.Unix_error _ -> false

let pipe_through cmd input =
  assert (exists cmd);
  try
    let (ic, oc) = Unix.open_process cmd in
      output_string oc input;
      close_out oc;
      let inbuf = Buffer.create 10240 in
      let s = String.create 1024 in
	let rec loop () =
	  let read = Pervasives.input ic s 0 1024 in
	    if read <> 0 then
	      begin
		(* Printf.printf "\nread %d\n%s\n" read (String.sub s 0 read); *)
		Buffer.add_substring inbuf s 0 read;
		loop ()
	      end
	    else
	      ()
	in
	  loop ();
	  close_in ic;
	  Buffer.contents inbuf
  with _ -> raise (PF_error cmd)

let pipe_pfopt = pipe_through pfopt

let pipe_pfsql = pipe_through pfsql


	


