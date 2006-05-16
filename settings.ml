(*
  Compiler settings
*)
module SettingsMap = Utility.StringMap

type 'a setting = 'a ref * string

let bool_settings = ref (SettingsMap.empty)
let int_settings = ref (SettingsMap.empty)
let string_settings = ref (SettingsMap.empty)

let add settings v name =
  if SettingsMap.mem name (!settings) then
    failwith ("Setting: "^name^" already present")
  else
    let setting = ref v, name
    in
      (settings := SettingsMap.add name setting (!settings); setting)

let add_bool = add bool_settings
let add_int = add int_settings
let add_string = add string_settings

let lookup settings name =
  (assert(SettingsMap.mem name (!settings));
   SettingsMap.find name (!settings))

let lookup_bool = lookup bool_settings
let lookup_int = lookup int_settings
let lookup_string = lookup string_settings

let get_value ({contents=v}, _) = v
let get_name (_, name) = name

let set_value (r, _) v = r := v
