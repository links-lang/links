open FileInfo

(* Source descriptor *)  
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

(* Compilation units *)  
module type COMPILATION_UNIT =
sig
  type 'a t

  val source_file : 'a t -> fileinfo
  val module_name : 'a t -> string
  val ir          : 'a t -> 'a
end

let obj_ext = "o"
let compiled_byteobj_ext = "cmo"
let compiled_bytemod_ext = "cma"
let compiled_natobj_ext  = "cmx"
let compiled_natmod_ext  = "cmxa"
  
module type BASIC_COMPILATION_UNIT =
sig
  type 'a t

  include COMPILATION_UNIT with type 'a t := 'a t
    
  val make        : source_desc -> string -> int * 'a -> 'a t
  val string_of_basic_unit : 'a t -> string
  val count_globals : 'a t -> int                                       
end

module Basic_Compilation_Unit : BASIC_COMPILATION_UNIT =
struct
  type 'a t =
    { source      : source_desc    
    ; module_name : string
    ; ir          : 'a
    ; globals_count : int
    }

  let make source_desc caml_module_name (num_globals,ir') =
    let srcfile = source_file source_desc in
    { source      = source_desc
    ; module_name = caml_module_name
    ; ir          = ir'
    ; globals_count = num_globals
    }

  let source_file bcu = bcu.source.srcfile
  let module_name bcu = bcu.module_name
  let ir          bcu = bcu.ir
  let count_globals bcu = bcu.globals_count                          
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

  include COMPILATION_UNIT with type 'a t := 'a t
    
  val make : 'a basic_comp_unit -> 'a t
    
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
    ; objfile    = with_extension obj_ext src
    ; cmxfile    = with_extension compiled_natobj_ext src
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

(* For linking *)  
module type LINKABLE_UNIT =
sig
  type t

  val make      : string -> t
  val locate    : t -> string
end

module Linkable_Unit : LINKABLE_UNIT =
struct
  type t = fileinfo
  let make f = FileInfo.make_fileinfo f

  let locate = FileInfo.filename
end

type linkable_unit = Linkable_Unit.t

let make_linkable_unit = Linkable_Unit.make
