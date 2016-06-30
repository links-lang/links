type fileinfo
  
(* Operations on fileinfo objects *)
val fileroot  : fileinfo -> string
val basename  : fileinfo -> string
val extension : fileinfo -> string option
val dirname   : fileinfo -> string
val filename  : fileinfo -> string

(* Creation *)
val make_fileinfo : string -> fileinfo  
(* Copies a given fileinfo object except its extension *)
val with_extension : string -> fileinfo -> fileinfo
val with_fileroot  : string -> fileinfo -> fileinfo
