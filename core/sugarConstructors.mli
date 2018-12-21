(* Import module signatures. *)
module type Pos                  = SugarConstructorsIntf.Pos
module type SugarConstructorsSig = SugarConstructorsIntf.SugarConstructorsSig

(* Actual implementation of smart constructors as a functor on a Pos module. *)
module SugarConstructors (Position : Pos)
       : (SugarConstructorsSig with type t := Position.t)

(* Module for making nodes using Sugartypes positions. *)
module Make : (SugarConstructorsSig with type t := unit)
