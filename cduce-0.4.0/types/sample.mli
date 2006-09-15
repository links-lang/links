(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t = Types.descr

val get : Types.descr -> t
  (** 
    Extract when possible a simpler type which is ``trivially'' non-empty.
    This subtype is built from scalar and intersection of simple arrow types
    using products, XML elements and records, without recursion nor
    boolean combination.

    The simpler type is not a subtype because of arrows and records.

    Interpretation of this subtype, to extract sample values:
    - basic type: pick a value
    - open record type:  add some extra field not listed
    - intersection of arrow types: any abstraction with this interface
    
    Raises Not_found for an empty type
  **)

val print : Format.formatter -> t -> unit


val single : Types.descr -> Types.const
  (**
     Raises Not_found for an empty type.
     Raises Exit if at least two values in the type.
  **)
val single_opt : Types.descr -> Types.const option
