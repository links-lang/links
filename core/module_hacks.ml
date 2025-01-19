(* A collection of hacks specific for the (improper) module system. *)
module Name = struct
  let module_name_hack_pat = Str.regexp "_[0-9]+\\$"

  let prettify : string -> string
  = fun name ->
    Str.global_replace module_name_hack_pat "." name
end
