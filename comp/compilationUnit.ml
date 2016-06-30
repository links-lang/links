open FileInfo
  
type native_compilation_unit =
  { srcfile     : fileinfo
  ; ifacefile   : fileinfo option (* maybe useful in the future when Links gets a module system *)
  ; objfile     : fileinfo
  ; cmxfile     : fileinfo
  ; cmifile     : fileinfo option
  ; modulename  : string
  }

type comp_unit = native_compilation_unit
  
(* Getters *)
let source_file cu = cu.srcfile
let object_file cu = cu.objfile
let cmx_file    cu = cu.cmxfile
let cmi_file    cu = cu.cmifile
let module_name cu = cu.modulename  


let temporary_files cu =
  let files =
    (match cmi_file cu with
    | Some cmi -> [cmi]
    | None -> [])
    @ [ object_file cu ; cmx_file cu ]
  in
  List.map FileInfo.filename files  

(* Creation *)
let make_compilation_unit srcfile =
  let src = make_fileinfo srcfile in
  let caml_module_name = Compenv.module_of_filename (Format.std_formatter) (filename src) (fileroot src) in
  let caml_compatible_file = with_fileroot (String.uncapitalize_ascii caml_module_name) src in
  { srcfile     = src
  ; ifacefile   = None
  ; objfile     = with_extension "o"   caml_compatible_file
  ; cmxfile     = with_extension "cmx" caml_compatible_file
  ; cmifile     = None
  ; modulename  = caml_module_name
  }
    
let with_cmi_file fi cu = { cu with cmifile = Some fi }    
