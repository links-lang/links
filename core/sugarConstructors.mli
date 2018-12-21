(* Import module signatures *)
module type Pos                  = SmartConstructorsIntf.Pos
module type SmartConstructorsSig = SmartConstructorsIntf.SmartConstructorsSig

(* Actual implementation of smart constructors as a functor on a Pos module *)
module SmartConstructors (Position : Pos)
       : (SmartConstructorsSig with type t := Position.t)

(* Modules for making nodes using various types of positions *)
module Make : (SmartConstructorsSig
               with type t := (SourceCode.lexpos * SourceCode.lexpos *
                               SourceCode.source_code option))
module DummyMake : (SmartConstructorsSig with type t := unit)
