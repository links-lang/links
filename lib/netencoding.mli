(* $Id: netencoding.mli,v 2.9 2005/04/14 12:44:40 stolpmann Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* Extracted part of the base64 encoder for use in the Links project.
   Jeremy Yallop, 2006-05-11 *)

(** Base64, Quoted Printable, URL encoding, HTML escaping *)

(* *********************************************************************)
(* Several encodings important for the net                             *)
(* *********************************************************************)


(* *********************************************************************)
(* Base 64 encoding                                                    *)
(* *********************************************************************)

(* See RFC 2045 for a description of Base 64 encoding. *)

(* THREAD-SAFETY: 
 * All Base64 functions are reentrant and thus thread-safe.
 *)

module Base64 : sig

  (** Base64 encoding as described in RFC 2045 *)

  val encode : ?pos:int -> ?len:int -> ?linelength:int -> ?crlf:bool ->
               string -> string
      (** Compute the "base 64" encoding of the given string argument.
       * Note that the result is a string that only contains the characters
       * a-z, A-Z, 0-9, +, /, =, and optionally spaces, CR and LF characters.
       *
       * If [pos] and/or [len] are passed, only the substring starting at
       * [pos] (default: 0) with length [len] (default: rest of the string)
       * is encoded.
       *
       * The result is divided up into lines not longer than [linelength]
       * (without counting the line separator); default: do not divide lines.
       * If [linelength] is smaller than 4, no line division is performed.
       * If [linelength] is not divisible by 4, the produced lines are a 
       * bit shorter than [linelength].
       *
       * If [crlf] (default: false) the lines are ended by CRLF; otherwise 
       * they are only ended by LF.
       * (You need the crlf option to produce correct MIME messages.)
       * 
       *)

  val decode : ?pos:int -> ?len:int -> ?url_variant:bool -> 
               ?accept_spaces:bool -> string -> string
      (** Decodes the given string argument. 
       *
       * If [pos] and/or [len] are passed, only the substring starting at
       * [pos] (default: 0) with length [len] (default: rest of the string)
       * is decoded.
       * 
       * If [url_variant] (default: [true]) is set, the functions also
       * accepts the characters '-' and '.' as produced by [url_encode].
       *
       * If [accept_spaces] (default: [false]) is set, the function ignores
       * white space contained in the string to decode (otherwise the
       * function fails if it finds white space). Furthermore, the character
       * '>' is considered as "space", too (so you don't have trouble with
       * mbox mailboxes that accidentally quote "From").
       *)


end

(* ======================================================================
 * History:
 * 
 * $Log: netencoding.mli,v $
 * Revision 2.9  2005/04/14 12:44:40  stolpmann
 * One can now select LF as line terminator for all message writing
 * functions. Netsendmail.sendmail defaults to LF for Unix compatiblity.
 *
 * Revision 2.8  2004/07/10 23:43:06  stolpmann
 * 	ocamldoc
 *
 * Revision 2.7  2004/07/04 21:02:46  stolpmann
 * 	Bug fix: QuotedPrintable takes care of adding soft line breaks.
 *
 * Revision 2.6  2002/07/03 01:16:38  stolpmann
 * 	New: Html.encode and Html.decode
 *
 * Revision 2.5  2002/02/02 23:54:06  stolpmann
 * 	Improvements for mbox mailboxes.
 *
 * Revision 2.4  2002/01/14 01:04:58  stolpmann
 * 	Netencoding.Url.mk/dest_url_encoded_parameters: These functions
 * now reside here and not in Cgi.
 *
 * Revision 2.3  2002/01/04 22:46:08  stolpmann
 * 	New: QuotedPrintable.encoding_pipe, and decoding_pipe.
 * 	The deprecated functions of [QuotedPrintable] and [Q] have been
 * moved into modules [Deprecated].
 *
 * Revision 2.2  2002/01/04 21:59:35  stolpmann
 * 	New: Base64.encoding_pipe, and Base64.decoding_pipe.
 * 	The deprecated Base64 functions have been moved into a
 * module [Deprecated].
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.5  2001/08/30 19:45:43  gerd
 * 	Module Url: added the option ~plus
 *
 * Revision 1.4  2000/06/25 22:34:43  gerd
 * 	Added labels to arguments.
 *
 * Revision 1.3  2000/06/25 21:15:48  gerd
 * 	Checked thread-safety.
 *
 * Revision 1.2  2000/03/03 01:08:29  gerd
 * 	Added Netencoding.Html functions.
 *
 * Revision 1.1  2000/03/02 01:14:48  gerd
 * 	Initial revision.
 *
 * 
 *)
