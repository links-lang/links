open FileInfo

type source_desc =
  { srcfile : fileinfo
  ; ifacefile : fileinfo option (* maybe useful in the future when Links gets a module system *)
  }

let make_source_desc ?(interface_file=None) source_file =
  { srcfile   = make_fileinfo source_file
  ; ifacefile = None
  }
    
let source_file desc = desc.srcfile
let interface_file desc = desc.ifacefile
  
module type BASIC_COMPILATION_UNIT =
sig
  type 'a t
    
  val source_file : 'a t -> fileinfo
  val module_name : 'a t -> string
  val ir          : 'a t -> 'a
  val make        : source_desc -> string -> 'a -> 'a t
  val string_of_basic_unit : 'a t -> string
end

module Basic_Compilation_Unit : BASIC_COMPILATION_UNIT =
struct
  type 'a t =
    { source      : source_desc    
    ; module_name : string
    ; ir          : 'a
    }

  let make source_desc caml_module_name ir' =
    let srcfile = source_file source_desc in
    { source      = source_desc
    ; module_name = caml_module_name
    ; ir          = ir'
    }

  let source_file bcu = bcu.source.srcfile
  let module_name bcu = bcu.module_name
  let ir          bcu = bcu.ir
  let string_of_basic_unit bcu =
    Printf.sprintf
      ("{ source      = %s\n, module_name = %s\n, ir          = <abstr> }")
      (source_file bcu |> filename) (module_name bcu)
end

type 'a basic_comp_unit = 'a Basic_Compilation_Unit.t
  
let make_basic_compilation_unit = Basic_Compilation_Unit.make

module type NATIVE_COMPILATION_UNIT =
sig
  type 'a t

  val make : 'a basic_comp_unit -> 'a t
    
  val source_file : 'a t -> fileinfo
  val module_name : 'a t -> string
  val ir          : 'a t -> 'a
  val object_file : 'a t -> fileinfo
  val cmx_file    : 'a t -> fileinfo
  val cmi_file    : 'a t -> fileinfo option
  val with_cmi_file : fileinfo -> 'a t -> 'a t
  val temporary_files : 'a t -> fileinfo list
  val string_of_native_unit : 'a t -> string
end
  
module Native_Compilation_Unit : NATIVE_COMPILATION_UNIT =  
struct
  module BC = Basic_Compilation_Unit
    
  type 'a t =
      { basic_unit  : 'a BC.t
      ; objfile     : fileinfo
      ; cmxfile     : fileinfo
      ; cmifile     : fileinfo option
      }

  let make bcu =
    let src = BC.source_file bcu in
    { basic_unit = bcu
    ; objfile    = with_extension "o" src
    ; cmxfile    = with_extension "cmx" src
    ; cmifile    = None
    }

  let basic_unit ncu  = ncu.basic_unit
  let source_file ncu = BC.source_file (basic_unit ncu)
  let module_name ncu = BC.module_name (basic_unit ncu)
  let ir          ncu = BC.ir (basic_unit ncu)
  let object_file ncu = ncu.objfile
  let cmx_file    ncu = ncu.cmxfile
  let cmi_file    ncu = ncu.cmifile
  let with_cmi_file fi cu = { cu with cmifile = Some fi } 
  let temporary_files ncu =
    (match cmi_file ncu with
    | Some cmi -> [cmi]
    | None -> [])
    @ [ object_file ncu ; cmx_file ncu ]

  let string_of_native_unit cu =
    let string_of_option f = function
      | Some x -> f x
      | None   -> "<None>"
    in
    Printf.sprintf
      ("{ basic_unit = \n%s\n, objfile    = %s\n, cmxfile    = %s\n, cmifile    = %s }")
      (BC.string_of_basic_unit (basic_unit cu)) (object_file cu |> filename) (cmx_file cu |> filename) (cmi_file cu |> string_of_option filename)
end

type 'a native_comp_unit = 'a Native_Compilation_Unit.t
  
let make_native_compilation_unit = Native_Compilation_Unit.make

(*type native_compilation_unit =
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


let string_of_compilation_unit cu =
  let string_of_option f = function
    | Some x -> f x
    | None   -> "<None>"
  in
  Printf.sprintf
    ("{ module name = %s\n, source      = %s\n, interface   = %s\n, object file = %s\n, cmx file    = %s\n, cmi file    = %s }")
  (module_name cu) (source_file cu |> filename) (string_of_option filename None) (object_file cu |> filename) (cmx_file cu |> filename) (cmi_file cu |> string_of_option filename)*)
