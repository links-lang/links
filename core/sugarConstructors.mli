module type Pos = sig
  type t
  val pos      : t -> Sugartypes.position
  val with_pos : t -> 'a -> 'a Sugartypes.with_pos
end

module type SmartConstructorsSig = SmartConstructorsIntf.SmartConstructorsSig

module SmartConstructors (Position : Pos)
       : (SmartConstructorsSig with type t := Position.t)

module Make : (SmartConstructorsSig
               with type t := (SourceCode.lexpos * SourceCode.lexpos *
                               SourceCode.source_code option))

module DummyMake : (SmartConstructorsSig with type t := unit)
