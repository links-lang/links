open FileInfo
  
type native_compilation_unit
type comp_unit = native_compilation_unit

(* Creation *)
val make_compilation_unit : string -> comp_unit
val with_cmi_file : fileinfo -> comp_unit -> comp_unit
    
(* Getters *)
val source_file : comp_unit -> fileinfo
val object_file : comp_unit -> fileinfo
val cmx_file    : comp_unit -> fileinfo
val cmi_file    : comp_unit -> fileinfo option
val module_name : comp_unit -> string  
val temporary_files : comp_unit -> string list

