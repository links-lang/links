(* Import module signatures. *)
module type Pos                  = SugarConstructorsIntf.Pos
module type SugarConstructorsSig = SugarConstructorsIntf.SugarConstructorsSig

(* Actual implementation of smart constructors as a functor on a Pos module. *)
module SugarConstructors (Position : Pos)
       : (SugarConstructorsSig with type t := Position.t)

(* Modules for making nodes using various types of positions. *)
module Make : (SugarConstructorsSig
               with type t := (SourceCode.lexpos * SourceCode.lexpos *
                               SourceCode.source_code option))
module DummyMake : (SugarConstructorsSig with type t := unit)
