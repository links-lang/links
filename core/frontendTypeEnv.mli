type qual_var_environment =
  (QualifiedName.t option * Types.datatype) Env.String.t
[@@deriving show]

type qual_module_environment =
  (QualifiedName.t option * Types.module_t) Env.String.t
[@@deriving show]

type tycon_environment = Types.tycon_spec Env.String.t [@@deriving show]

type t =
  { var_env: qual_var_environment
  ; module_env: qual_module_environment
  ; tycon_env: tycon_environment
  ; effect_row: Types.row }
[@@deriving show]

val empty_typing_environment : t

val open_module : QualifiedName.t -> t -> t

val normalise_typing_environment : t -> t

val extend_typing_environment : ?effects:Types.row -> t -> t -> t

val string_of_environment : qual_var_environment -> string

val string_of_typing_environment : t -> string

exception TyConsNotFound of QualifiedName.t

exception VariableNotFound of QualifiedName.t

exception ModuleNotFound of QualifiedName.t

val lookup_variable : t -> QualifiedName.t -> Types.datatype

val find_variable : t -> QualifiedName.t -> Types.datatype option

val lookup_tycons : t -> QualifiedName.t -> Types.tycon_spec

val find_tycons : t -> QualifiedName.t -> Types.tycon_spec option

val lookup_module : t -> QualifiedName.t -> Types.module_t

val find_module : t -> QualifiedName.t -> Types.module_t option

val bind_var : t -> Env.String.name * Types.datatype -> t

val bind_tycons : t -> Env.String.name * Types.tycon_spec -> t

val bind_module : t -> Env.String.name * Types.module_t -> t

val var_env_bind_var :
     qual_var_environment
  -> Env.String.name * Types.datatype
  -> (QualifiedName.t option * Types.datatype) Env.String.t
