open FileInfo
  
(*type native_compilation_unit
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

val string_of_compilation_unit : comp_unit -> string*)


type source_desc

val make_source_desc : ?interface_file:string option -> string -> source_desc
val source_file : source_desc -> fileinfo
val interface_file : source_desc -> fileinfo option

module type BASIC_COMPILATION_UNIT =
sig
  type 'a t
    
  val source_file : 'a t -> fileinfo
  val module_name : 'a t -> string
  val ir          : 'a t -> 'a
  val make        : source_desc -> string -> 'a -> 'a t
  val string_of_basic_unit : 'a t -> string
end

module Basic_Compilation_Unit : BASIC_COMPILATION_UNIT
  
type 'a basic_comp_unit = 'a Basic_Compilation_Unit.t

(*type basic_comp_unit = 'a. 'a BASIC_COMPILATION_UNIT.t*)
  
val make_basic_compilation_unit : source_desc -> string -> 'a -> 'a basic_comp_unit

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

module Native_Compilation_Unit : NATIVE_COMPILATION_UNIT

type 'a native_comp_unit = 'a Native_Compilation_Unit.t

val make_native_compilation_unit : 'a basic_comp_unit -> 'a native_comp_unit
