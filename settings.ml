(*
  Compiler settings
*)
module SettingsMap = Utility.StringMap

type 'a setting = 'a ref * string

let get_value ({contents=v}, _) = v
let get_name (_, name) = name

let set_value (r, _) v = r := v

type universal = [
| `Bool of bool setting
| `Int of int setting
| `String  of string setting
]

let settings = ref (SettingsMap.empty)

let parse_and_set (name, value) =
  if SettingsMap.mem name (!settings) then
    match SettingsMap.find name (!settings) with
      | `Bool setting ->
	  begin
	    try
	      set_value setting (bool_of_string value)
	    with (Invalid_argument _) ->
	      output_string stderr ("Setting '" ^ name ^ "' expects a boolean\n"); flush stderr
	  end
      | `Int setting ->
	  begin
	    try
	      set_value setting (int_of_string value)
	    with Invalid_argument _ ->
	      output_string stderr ("Setting '" ^ name ^ "' expects a boolean\n"); flush stderr
	  end
      | `String setting ->
	  set_value setting value
  else
     output_string stderr ("Unknown setting: " ^ name ^ "\n"); flush stderr

let add universal_setting name =
  if SettingsMap.mem name (!settings) then
    failwith ("Setting: "^name^" already present")
  else
    (settings := SettingsMap.add name universal_setting (!settings))

let add_bool value name =
  let setting = (ref value, name) in
    add (`Bool setting) name;
    setting
let add_int value name =
  let setting = (ref value, name) in
    add (`Int setting) name;
    setting
let add_string value name =
  let setting = (ref value, name) in
    add (`String setting) name;
    setting

let lookup name =
  (assert(SettingsMap.mem name (!settings));
   SettingsMap.find name (!settings))

let lookup_bool name =
  match lookup name with
    | `Bool setting -> setting
    | _ -> assert(false)
let lookup_int name =
  match lookup name with
    | `Int setting -> setting
    | _ -> assert(false)
let lookup_string name =
  match lookup name with
    | `String setting -> setting
    | _ -> assert(false)

