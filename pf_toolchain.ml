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

let pipe_pfopt = Debug.print ">>> pfopt"; pipe_through pfopt

let pipe_pfsql = Debug.print ">>> pfsql"; pipe_through pfsql

let buf = ref (Buffer.create 1024)
let c = ref 0

let output_plan plan fname =
  let o = open_out fname in
    output_string o plan;
    close_out o

let optimize_sql q = 
  Algebra_export.export_plan_bundle (`Buffer !buf) q;
  Debug.f ">>> pfopt %d" !c;
  let optimized = pipe_pfopt (Buffer.contents !buf) in
  let sql = pipe_pfsql optimized in

    let i = string_of_int !c in
      output_plan (Buffer.contents !buf) ("plan_" ^ i ^ ".xml");
      output_plan optimized ("plan_opt_" ^ i ^ ".xml");
      output_plan sql ("plan_opt_sql_" ^ i ^ ".xml");

      Buffer.clear !buf;
      incr c;

      sql
