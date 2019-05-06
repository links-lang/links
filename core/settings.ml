(**
  Compiler settings
*)

open Utility
module SettingsMap = Utility.StringMap

type mode = [`User | `System]
type 'a setting = {mutable value: 'a; name: string; mode: mode; hook: 'a -> unit }
(*
[SUGGESTION]
  add an optional function to each setting
  that is called whenever the setting is updated
*)
(* ('a * 'a -> unit) option*)

let get_value setting = setting.value
let get_name setting = setting.name

let set_value setting v = setting.value <- v; setting.hook v

type universal = [
| `Bool of bool setting
| `Int of int setting
| `String of string setting
]

let parse_bool = function
  | "true"
  | "yes"
  | "oui"
  | "ja"
  | "tak"
  | "on"
  | "sure"
  | "yep"
  | "totally" -> true
  | "false"
  | "no"
  | "non"
  | "nein"
  | "nie"
  | "off"
  | "nope"
  | "negative"
  | "uncool" -> false
  | _ -> raise (Invalid_argument "parse_bool")

let is_user = function
  | `User -> true
  | `System -> false

let is_user = function
  | `Bool setting -> is_user setting.mode
  | `Int setting -> is_user setting.mode
  | `String setting -> is_user setting.mode

let settings : ((universal SettingsMap.t) ref) = ref (SettingsMap.empty)

(* parse and set *)
let parse_and_set' : [`Any | `OnlyUser] -> (string * string) -> bool -> unit = fun kind (name, value) quiet ->
  let output_string msg =
    if not quiet then
      output_string stderr msg; flush stderr
  in
  if SettingsMap.mem name (!settings) then
    let universal_setting = SettingsMap.find name (!settings) in
      match kind, is_user(universal_setting) with
        | `Any, _
        | `OnlyUser, true ->
            begin
          match universal_setting with
            | `Bool setting ->
                begin
              try
                set_value setting (parse_bool value)
              with (Invalid_argument _) ->
                output_string ("Setting '" ^ name ^ "' expects a boolean\n")
                end
            | `Int setting ->
                begin
              try
                set_value setting (int_of_string value)
              with Invalid_argument _ ->
                output_string ("Setting '" ^ name ^ "' expects a boolean\n")
                end
            | `String setting ->
                   let expanded_value =
                     try Utility.Sys.expand value
                     with Utility.Sys.Unknown_environment_variable v ->
                       failwith (Printf.sprintf "failed to expand environment variable '%s' in value '%s' while setting '%s'.\n" v value setting.name)
                   in
               set_value setting expanded_value
            end
        | _ ->
        output_string ("Cannot change system setting '" ^ name ^ "'\n")
  else
    output_string ("Unknown setting: " ^ name ^ "\n")

let parse_and_set = parse_and_set' `Any     (* any setting can be set *)
let parse_and_set_user = parse_and_set' `OnlyUser (* only allow user settings to be set *)

let add : string -> universal -> unit = fun name universal_setting ->
  if SettingsMap.mem name (!settings) then
    failwith ("Setting: "^name^" already present")
  else
    (settings := SettingsMap.add name universal_setting (!settings))

let add_bool ?(hook=ignore) (name, value, mode) =
  let setting = {value; name; mode; hook} in
    add name (`Bool setting);
    setting
let add_int ?(hook=ignore) (name, value, mode) =
  let setting : int setting = {value; name; mode; hook} in
    add name (`Int setting);
    setting
let add_string ?(hook=ignore) (name, value, mode) =
  let setting = {value; name; mode; hook} in
    add name (`String setting);
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


let format_universal formatter : universal -> string = fun universal_setting ->
  match universal_setting with
    | `Bool setting ->
    formatter setting.name (string_of_bool setting.value)
    | `Int setting ->
    formatter setting.name (string_of_int setting.value)
    | `String setting ->
    formatter setting.name setting.value

let print_settings () =
  let get_settings mode =
    List.rev
      (SettingsMap.fold (fun _ setting p ->
                           match mode, is_user setting with
                             | `User, true
                             | `System, false ->
                         (format_universal
                            (Printf.sprintf " %-25s %-7s") setting)::p
                             | _ -> p) !settings []) in
  let user_settings =
    ("User settings" :: get_settings `User) in
  let system_settings =
    ("System settings" :: get_settings `System)
  in
    [""] @ user_settings @ [""] @ system_settings

let from_argv : string array -> string list =
  let set name value =
    parse_and_set_user (name, value) false
  and is_opt = start_of ~is:"--"
  and de_opt s = StringLabels.sub ~pos:2 ~len:(String.length s - 2) s in
  let rec process nonopts = function
    | [] -> nonopts
    | option::rest when is_opt option && String.contains option '=' ->
        let idx = String.index option '=' in
          set
            (de_opt (StringLabels.sub ~pos:0 ~len:idx option))
            (StringLabels.sub ~pos:idx ~len:(String.length option - idx) option);
        process nonopts rest
    | option::value::rest when is_opt option ->
        set (de_opt option) value ;
        process nonopts rest
    | nonopt::rest -> process (nonopt::nonopts) rest
  in
    process [] -<- Array.to_list


let load_file quiet filename =
  let file = open_in filename in

  let strip_comment s =
    if String.contains s '#' then
      let i = String.index s '#' in
    String.sub s 0 i
    else
      s in

  let is_empty = not -<- StringUtils.contains Char.isAlpha in

  let parse_line n s =
    let s = strip_comment s in
      if not (is_empty s) then
    (* ignore 'empty' lines *)
    begin
      if String.contains s '=' then
        begin
          let i = String.index s '=' in
          let name = String.sub s 0 i in
          let value = String.sub s (i+1) ((String.length s) - (i+1))
          in
        parse_and_set (name, value) quiet
        end
      else
        failwith ("Error in configuration file (line "^string_of_int n^"): '"^s^"'\n"^
            "Configuration options must be of the form <name>=<value>")
    end in

  let rec parse_lines n =
    try
      parse_line n (input_line file);
      parse_lines (n+1)
    with
    End_of_file -> close_in file
  in
    parse_lines 1
