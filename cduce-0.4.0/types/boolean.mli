(* Implementation of boolean combination in disjunctive normal form (dnf).
   A dnf is a disjunction of lines, each line consisting of a 
   conjunction of atoms and negations of atoms.

   A line is coded as a pair (p,n) where p collects atoms and n
   the negated atoms. p and n are disjoint and sorted lists without duplicates
   (w.r.t Pervasives.compare).

   A dnf is coded as a sorted list of lines without duplicated
   (w.r.t Pervasives.compare).
*)

module Make(X : Custom.T) :
sig
  include Custom.T
  type elem = X.t

  external get: t -> (X.t list * X.t list) list = "%identity"

  val empty : t
  val full  : t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
  val diff  : t -> t -> t
  val atom  : X.t -> t

  val map : (X.t -> X.t) -> t -> t
  val iter: (X.t -> unit) -> t -> unit
  val compute: empty:'d -> full:'c -> cup:('d -> 'c -> 'd) 
    -> cap:('c -> 'b -> 'c) -> diff:('c -> 'b -> 'c) ->
    atom:(X.t -> 'b) -> t -> 'd
  val compute_bool: (X.t -> t) -> t -> t
    
  val print: string -> (Format.formatter -> X.t -> unit) -> t ->
    (Format.formatter -> unit) list
    
    
  val trivially_disjoint : t -> t -> bool    
end
