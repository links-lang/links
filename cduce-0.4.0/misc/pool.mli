(*
module type T = 
sig
  include Custom.T
    (* Hashtbl.hash'able and Pervasives.compare'able type;
       typically, t is an integer *)
  type value
    
  val clear: unit -> unit  
    (* Previously allocated symbols are no longer valid; no check
       is provided. Registered values can be released by the GC only after
       a call to clear. *)

  val mk: value -> t
  val dummy_min: t
  val dummy_max: t
    (* Two dummy symbols, not associated with any registered value;
       resp. smallest and largest than any other symbol *)

  val value: t -> value
end

module Make(H : Custom.T) : T with type value = H.t and type t = int

module NoHash(H : Custom.T) : T with type value = H.t and type t = int


module Weak(H : Custom.T) : sig
  include Custom.T
  type value = H.t
  val mk: value -> t
  val value: t -> value
end

      
*)
