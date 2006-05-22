(* A built-in, first-order type *)
type primitive = 
    Bool | Char | Int | Float | String | Unit

(* A type variable *)
type tvar = string

(* A type name, perhaps a dot-qualified one. *)    
type name = 
  | Name of string
  | Qname of string list
      
(* A "type expression"; something that can appear "inline". *)
type typeexp = [
| `Primitive of primitive
| `Name of name
| `TVar of tvar
| `Application of typeexp * name (* e.g. string ref list *)
| `Tuple of typeexp list
]

type constructor = {
  name : string;
  args : typeexp list;
}

(* A toplevel type declaration *)
type typedecl = string * tvar list * [
| `Sum of constructor list
|  typeexp
]
    
(* also:

   various higher-order builtins:
     lists,
     arrays,
     references,
     lazy_t,
     option,
     streams,

   various first-order builtins:
     native ints,
     channels,

   various other kinds:
     records,
     polymorphic variants,
     classes,
     exceptions,

   anything else? *)
