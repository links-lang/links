module type S = sig
  val db : Lens.Database.t

  (** Create a table with the provided table name and primary key and fields.
     All of the fields are created as integers. *)
  val create :
    table:string ->
    primary_key:string list ->
    fields:string list ->
    (module Table_S.S)

  (** Drop table and then create it again with the provided table name and
     primary key and fields. All of the fields are created as integers. *)
  val drop_create :
    table:string ->
    primary_key:string list ->
    fields:string list ->
    (module Table_S.S)

  (** Create a table with the provided table name and single functional
     dependency in string form. *)
  val create_easy : table:string -> string -> (module Table_S.S)

  (** Drop table and then create it again with the provided table name and single functional
      dependency in string form. *)
  val drop_create_easy : table:string -> string -> (module Table_S.S)

  type col_gen_type = [ `Seq | `Constant of int | `RandTo of int | `Rand ]

  (** Generate n rows of data. The col_gen_type specifies how each column should
     be generated, and can either be a sequential integer, a constant integer, a
     bounded random integer or a random integer. *)
  val generate_data : n:int -> col_gen_type list -> int list list

  (** Drop table and then create it again with the provided table name and single functional
      dependency in string form. *)
  val drop_create_easy_populate :
    table:string ->
    n:int ->
    fd:string ->
    col_gen_type list ->
    (module Table_S.S)

  val print_and_execute : string -> unit
end
