open Utility
open Scanner

(* NB : For now, positions are resolved eagerly.  It might be better
   to resolve them lazily, i.e. keep the source around and only find
   the text corresponding to an expression when an error actually
   occurs.
*)

module LinksLexer : (LexerSig with type token         = Parser.token and
                                   type lexer_context = Lexer.lexer_context) =
  struct
    type token         = Parser.token
    type lexer_context = Lexer.lexer_context
    type 'a grammar    = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
    let lexer          = Lexer.lexer
    let fresh_context  = Lexer.fresh_context
end

module LinksParser = Scanner (LinksLexer)

(* Given an input channel, return a function suitable for input to
   Lexing.from_function that reads characters from the channel.
*)
let reader_of_channel channel buffer = input channel buffer 0

(* Given a string, return a function suitable for input to
   Lexing.from_function that reads characters from the string.
*)
let reader_of_string ?pp string =
  let string = match pp with
    | None -> string
    | Some command -> Utility.filter_through ~command string in
  let current_pos = ref 0 in
    fun buffer nchars ->
        let nchars = min nchars (String.length string - !current_pos) in
        Bytes.blit (Bytes.of_string string) !current_pos buffer 0 nchars;
        current_pos := !current_pos + nchars;
        nchars

let interactive : Sugartypes.sentence LinksLexer.grammar =
  Parser.interactive
let program : (Sugartypes.binding list * Sugartypes.phrase option) LinksLexer.grammar =
  Parser.file
let datatype : Sugartypes.Datatype.with_pos LinksLexer.grammar =
  Parser.just_datatype

let normalize_pp = function
  | "" -> None
  | pp -> Some pp

(* We parse in a "context", which at the moment means the set of
   operator precedences, but more generally is an environment with
   respect to which any parse-time resolution takes place.
*)

(** Installed preprocessor *)
let pp = Settings.(option "preprocessor"
                   |> privilege `System
                   |> to_string from_string_option
                   |> convert Utility.some
                   |> sync)

let default_preprocessor () = from_option "" (Settings.get pp)

(** Public functions: parse some data source containing Links source
    code and return a list of ASTs.

    We use Lexing.from_function in every case rather than
    Lexing.from_channel, Lexing.from_string etc. so that we can
    intercept and retain the code that has been read (in order to give
    better error messages).
**)
let parse_string ?(pp=default_preprocessor ()) ?in_context:context grammar string =
  let pp = normalize_pp pp
  and context = LinksParser.normalize_context context in
    LinksParser.read ?nlhook:None ~parse:grammar
      ~infun:(reader_of_string ?pp string) ~name:"<string>" ~context ()

let parse_channel ?interactive ?in_context:context grammar (channel, name) =
  let context = LinksParser.normalize_context context in
    LinksParser.read ?nlhook:interactive ~parse:grammar
      ~infun:(reader_of_channel channel) ~name:name ~context ()

let parse_file ?(pp=default_preprocessor ()) ?in_context:context grammar filename =
  match normalize_pp pp with
    | None -> parse_channel ?in_context:context grammar (open_in filename, filename)
    | Some pp ->
        Utility.call_with_open_infile filename
          (fun channel ->
             let context = LinksParser.normalize_context context in
             LinksParser.read ~nlhook:ignore
               ~parse:grammar
               ~infun:(reader_of_string ~pp (String.concat "\n" (lines channel)))
               ~name:filename
               ~context ())

module Readline = struct
  (** Readline settings. **)
  (* Path for readline history file *)
  let readline_history_path
    = Settings.(option "readline_history_path"
                |> to_string from_string_option
                |> convert Utility.(Sys.expand ->- some)
                |> sync)

  let readline_history_path () =
    match Settings.get readline_history_path with
    | None -> Filename.concat (Unix.getenv "HOME") ".links_history"
    | Some settings_path -> settings_path

  (* Enable native readline? *)
  let history_loaded = ref false
  let load_history on =
    if on then
      (* Ensure we retain history *)
      let _ = LNoise.history_load ~filename:(readline_history_path ()) in
      let _ = LNoise.history_set ~max_length:100 in
      history_loaded := true
    else history_loaded := false

  let native_readline
    = Settings.(flag ~default:true "native_readline"
                |> synopsis "Selects whether to use the native readline support in REPL mode"
                |> convert parse_bool
                |> action load_history
                |> CLI.(add (short 'r' <&> long "rlwrap"))
                |> sync)


  let reader_of_readline ps1 =
    let current_pos = ref 0 in
    let buf = Buffer.create 30 in
    let dots = String.make (String.length ps1 - 1) '.' ^ " " in
    (* Gets an input from the command line, with a newline at the end, *)
    let get_input prompt =
      match LNoise.linenoise prompt with
      | None -> ""
      | Some inp ->
         ignore (LNoise.history_add inp);
         ignore (LNoise.history_save ~filename:(readline_history_path ())); (* FIXME todo: probably sub-optimal to save the history on every time. *)
         inp ^ "\n" in

    let initial_string = get_input ps1 in
    Buffer.add_string buf initial_string;
    (* Function to access the buffer (passed to Lexing.from_function) *)
    let accessor_fun =
      fun dst_buf nchars ->
      let nchars = min nchars (Buffer.length buf - !current_pos) in
      Buffer.blit buf !current_pos dst_buf 0 nchars;
      current_pos := !current_pos + nchars;
      nchars in
    (* Function to populate the buffer (passed as newline callback) *)
    let populate_fun =
      fun _ ->
      let input_str = get_input dots in
      Buffer.add_string buf input_str in
    (accessor_fun, populate_fun)

  (* Reads lines in and parses them, given an initial prompt *)
  let parse_readline ps1 ?in_context:context grammar =
    let context = LinksParser.normalize_context context in
    let (accessor_fun, populate_fun) = reader_of_readline ps1 in
    LinksParser.read ?nlhook:(Some populate_fun) ~parse:grammar
      ~infun:accessor_fun ~name:"<stdin>" ~context ()

  let prepare_prompt : string -> unit
    = fun ps1 ->
    if not (Settings.get native_readline)
    then (print_string ps1; flush stdout)

  let parse : string -> (Sugartypes.sentence * position_context)
    = fun ps1 ->
    if Settings.get native_readline
    then let () =
           if not !history_loaded
           then load_history true
         in parse_readline ps1 interactive
    else let make_dotter ps1 =
           let dots = String.make (String.length ps1 - 1) '.' ^ " " in
           fun _ -> print_string dots; flush stdout
         in
         parse_channel ~interactive:(make_dotter ps1) interactive (stdin, "<stdin>")
end
