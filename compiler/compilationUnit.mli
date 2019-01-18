open FileInfo

(* Source descriptor *)
type source_desc

val make_source_desc : ?interface_file:string option -> string -> source_desc
val source_file : source_desc -> fileinfo
val interface_file : source_desc -> fileinfo option


(* Compilation units *)  
module type COMPILATION_UNIT =
sig
  type 'a t

  val source_file : 'a t -> fileinfo
  val module_name : 'a t -> string
  val ir          : 'a t -> 'a
end

(* Basic compilation unit -- before byte or native compilation *)
module type BASIC_COMPILATION_UNIT =
sig
  type 'a t

  include COMPILATION_UNIT with type 'a t := 'a t
    
  val make        : source_desc -> string -> int * 'a -> 'a t
  val string_of_basic_unit : 'a t -> string
  val count_globals : 'a t -> int                                       
end

module Basic_Compilation_Unit : BASIC_COMPILATION_UNIT
type 'a basic_comp_unit = 'a Basic_Compilation_Unit.t
  
val make_basic_compilation_unit : source_desc -> string -> int * 'a -> 'a basic_comp_unit


(* Native compilation unit -- during native compilation *)  
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

module Native_Compilation_Unit : NATIVE_COMPILATION_UNIT
type 'a native_comp_unit = 'a Native_Compilation_Unit.t

val make_native_compilation_unit : 'a basic_comp_unit -> 'a native_comp_unit

(** Linking **)
module type LINKABLE_UNIT =
sig
  type t

  val make      : string -> t
  val locate    : t -> string
end

module Linkable_Unit : LINKABLE_UNIT
  
type linkable_unit = Linkable_Unit.t  

val make_linkable_unit : string -> linkable_unit
