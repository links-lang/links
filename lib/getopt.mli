(** Module [Getopt]: parsing of command line arguments.

Copyright (C) 2000-2004 Alain Frisch. Distributed under the terms of the MIT
license.

email: {{:Alain.Frisch\@ens.fr}Alain Frisch\@ens.fr}

web:   {{:http://www.eleves.ens.fr/home/frisch}http://www.eleves.ens.fr/home/frisch}

   This module provides a general mechanism for extracting options and
   arguments from the command line to the program. It is an alternative
   to the module [Arg] from the standard OCaml distribution.

   The syntax is close to GNU getopt and getop_long ([man 3 getopt]).
*)

(**
{1 Layout of the command line}
   There are two types of argument on the command line: options and
   anonymous arguments. Options may have two forms: a short one introduced 
   by a single dash character (-) and a long one introduced by a double
   dash (--).

   Options may have an argument attached. For the long form, the syntax
   is "--option=argument". For the short form, there are two possible syntaxes:
   "-o argument" (argument doesn't start with a dash) and "-oargument"

   Short options that refuse arguments may be concatenated, as in
   "-opq".

   The special argument -- interrupts the parsing of options: all the
   remaining arguments are arguments even they start with a dash.

{1 Command line specification}
   A specification lists the possible options and describe what to do
   when they are found; it also gives the action for anonymous arguments
   and for the special option - (a single dash alone).
*)
   
type opt = char * string * ((unit -> unit) option) * ((string -> unit) option)

(**
   The specification for a single option is a tuple
   [(short_form, long_form, action, handler)]
   where:
   - [short_form] is the character for the short form of the option
     without the leading -
     (or [noshort='\000'] if the option does not have a short form)

   - [long_form] is the string for the long form of the option
     without the leading --
     (or [nolong=""] if no long form)

   - [(action : (unit -> unit) option)] gives the action to be executed
     when the option is found without an argument

   - [(handler : (string -> unit) option)] specifies how to handle the
     argument when the option is found with the argument


   According to the pair [(action, handler)], the corresponding option
   may, must or mustn't have an argument :

   - [(Some _, Some _)] : the option may have an argument; the short form can't be
     concatenated with other options (even if the user does not want to provide
     an argument). The behaviour (handler/action) is determined by the 
     presence of the argument.

   - [(Some _, None)] : the option must not have an argument; the short form, if
     it exists, may be concatenated

   - [(None, Some _)] : the option must have an argument; the short form can't
     be concatenated

   - [(None, None)] : not allowed

*)

(** [noshort='\000'] can be used when an option has no short form *)
val noshort : char

(** [nolong=""] can be used when an option has no long form *)
val nolong  : string

(** Signals error on the command line *)
exception Error of string

(** {1 Parsing the command line} *)

val parse : opt list -> (string -> unit) -> string array -> int -> int -> unit
(**
    [parse opts others args first last] parse the arguments
    [args.(first)], [arg.(first+1)] ... [args.(last)].
    [others] is called on anonymous arguments (and the special - argument);
    [opts] is a list of option specifications (there must be no ambiguities).

  @raise Error : Unknown options, unexpected argument, ...
*)

val parse_cmdline : opt list -> (string -> unit) -> unit
(**
    Parse the command line in [Sys.argv] using [parse].
*)


(** {1 Useful actions and handlers} *)

val set : 'a ref -> 'a -> ((unit -> unit) option)
(** @return an action that gives a reference a given value *)

val incr : int ref -> ((unit -> unit) option)
(** @return an action that increments an [int] reference *)

val append : string list ref -> ((string -> unit) option)
(** @return an handler that appends the argument to the end of a [string list]
   reference *)

val atmost_once : string ref -> exn -> ((string -> unit) option)
(** @return an handler that stores the argument in a [string] reference if
    it is empty, raises an exception otherwise *)
